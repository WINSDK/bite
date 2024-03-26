mod fmt;

use decoder::{Decodable, Decoded};
use object::{Object, ObjectSegment, ObjectSection};
use object::{Architecture, BinaryFormat, SectionKind};
use object::read::File as ObjectFile;
use processor_shared::{PhysAddr, Section, Segment};
use symbols::Index;
use tokenizing::Token;

use memmap2::Mmap;
use x86_64::long_mode as x64;
use x86_64::protected_mode as x86;
use arm::armv7 as armv7;
use arm::armv8::a64 as aarch64;

use std::fs::File;
use std::borrow::Cow;
use std::mem::ManuallyDrop;

pub enum Error {
    IO(std::io::Error),
    Object(object::Error),
    Symbol(symbols::Error),
    NotAnExecutable,
    DecompressionFailed(object::Error),
    UnknownArchitecture(object::Architecture),
}

pub union Instruction {
    x86: ManuallyDrop<x86_64::protected_mode::Instruction>,
    x64: ManuallyDrop<x86_64::long_mode::Instruction>,
    riscv: ManuallyDrop<riscv::Instruction>,
    mips: ManuallyDrop<mips::Instruction>,
    armv7: ManuallyDrop<armv7::Instruction>,
    aarch64: ManuallyDrop<aarch64::Instruction>,
}

macro_rules! impl_recursion {
    ($symbols:expr, $errors:expr, $instructions:expr, $sections:expr,
     $max_instruction_width:expr, $decoder:expr, $arch:ident) => {{
        $max_instruction_width = $decoder.max_width();

        let width_guess = if $max_instruction_width == 4 {
            4
        } else {
            5
        };

        for section in $sections.iter().filter(|s| s.kind == SectionKind::Text ||
                                                   s.kind == SectionKind::Unknown) {
            let mut reader = decoder::Reader::new(section.bytes());
            let mut ip = section.start;

            log::complex!(
                w "[processor::recurse] analyzing section ",
                b &*section.name,
                w " <",
                g format!("{:x}", section.start),
                w "..",
                g format!("{:x}", section.end),
                w ">.",
            );

            // guessing an average of 5 byte long instructions
            log::PROGRESS.set("Decoding instructions", section.bytes().len() / width_guess);

            loop {
                // prefetch next cache line line
                #[cfg(target_arch = "x86")]
                unsafe {
                    core::arch::x86::_mm_prefetch(
                        reader.as_ptr() as *const i8,
                        core::arch::x86::_MM_HINT_NTA
                    );
                }

                #[cfg(target_arch = "x86_64")]
                unsafe {
                    core::arch::x86_64::_mm_prefetch(
                        reader.as_ptr() as *const i8,
                        core::arch::x86_64::_MM_HINT_NTA
                    );
                }

                match $decoder.decode(&mut reader) {
                    Ok(mut instruction) => {
                        instruction.update_rel_addrs(ip);

                        let width = instruction.width();
                        $instructions.push((ip, Instruction {
                            $arch: std::mem::ManuallyDrop::new(instruction)
                        }));

                        ip += width;
                    }
                    Err(error) => {
                        if error.kind == decoder::ErrorKind::ExhaustedInput {
                            break;
                        }

                        let width = error.size();
                        $errors.push((ip, error));
                        ip += width;
                    }
                }

                log::PROGRESS.step();
            }
        }
    }};
}

/// Architecture agnostic analysis of a module.
pub struct Processor {
    /// Where execution start. Might be zero in case of libraries.
    pub entrypoint: PhysAddr,

    /// Where the binary is located.
    pub path: std::path::PathBuf,

    /// Symbol lookup by physical address.
    pub index: Index,

    /// File handle to binary,
    _file: File,

    /// A memory map of the binary.
    _mmap: Mmap,

    /// Object's sections sorted by address.
    sections: Vec<Section>,

    /// Object's segments sorted by address.
    segments: Vec<Segment>,

    /// Errors occurred in decoding instructions.
    /// Sorted by address.
    errors: Vec<(PhysAddr, decoder::Error)>,

    /// Successfully decoded instructions.
    /// Sorted by address.
    instructions: Vec<(PhysAddr, Instruction)>,

    /// How many bytes an instruction given the architecture.
    max_instruction_width: usize,

    /// Function pointer to an [`Instruction`]'s implementation of [`Decoded::tokens`].
    instruction_tokens: fn(&Instruction, &Index) -> Vec<Token>,

    /// Function pointer to an [`Instruction`]'s implementation of [`Decoded::width`].
    instruction_width: fn(&Instruction) -> usize,

    /// Target's instruction set.
    arch: Architecture,
}

impl Processor {
    pub fn parse<P: AsRef<std::path::Path>>(path: P) -> Result<Self, Error> {
        let file = std::fs::File::open(path.as_ref()).map_err(Error::IO)?;
        let mmap = unsafe { Mmap::map(&file).map_err(Error::IO)? };
        let binary: &'static [u8] = unsafe { std::mem::transmute(&mmap[..]) };

        let obj = ObjectFile::parse(binary).map_err(Error::Object)?;
        let entrypoint = obj.entry() as PhysAddr;

        if entrypoint != 0 {
            log::complex!(
                w "[processor::parse] entrypoint ",
                g format!("{entrypoint:#X}"),
                w ".",
            );
        }

        let path = path.as_ref().to_path_buf();
        let now = std::time::Instant::now();

        let mut sections = Vec::new();
        let mut segments = Vec::new();

        let arch = obj.architecture();

        for segment in obj.segments() {
            let name = match segment.name().map_err(Error::Object)? {
                Some(name) => Cow::Owned(name.to_string()),
                _ => Cow::Borrowed("unnamed"),
            };

            let start = segment.address() as PhysAddr;
            let end = start + segment.size() as PhysAddr;

            segments.push(Segment { name, start, end });
        }

        for section in obj.sections() {
            let name = match section.name() {
                Ok(name) => Cow::Owned(name.to_string()),
                _ => Cow::Borrowed("unnamed"),
            };

            let section_bytes = match section.data() {
                Ok(data) => data,
                Err(..) => {
                    log::complex!(
                        w "[processor::new] ",
                        y format!("failed to read section {name}.")
                    );

                    continue;
                }
            };

            const DWARF_SECTIONS: [&str; 22] = [
                ".debug_abbrev",
                ".debug_addr",
                ".debug_aranges",
                ".debug_cu_index",
                ".debug_frame",
                ".eh_frame",
                ".eh_frame_hdr",
                ".debug_info",
                ".debug_line",
                ".debug_line_str",
                ".debug_loc",
                ".debug_loclists",
                ".debug_macinfo",
                ".debug_macro",
                ".debug_pubnames",
                ".debug_pubtypes",
                ".debug_ranges",
                ".debug_rnglists",
                ".debug_str",
                ".debug_str_offsets",
                ".debug_tu_index",
                ".debug_types",
            ];

            let start = section.address() as PhysAddr;
            let end = section_bytes.len() + start;
            let loaded = !DWARF_SECTIONS.contains(&&*name) && section.address() != 0;
            let section = Section::new(
                name,
                section.kind(),
                loaded,
                section_bytes,
                section.address() as PhysAddr,
                start,
                end
            );

            sections.push(section);
        }

        if sections.is_empty() {
            let addr = if obj.format() == BinaryFormat::Pe {
                0x1000
            } else {
                0
            };

            let rva = entrypoint - obj.relative_address_base() as PhysAddr;
            let start = obj.relative_address_base() as PhysAddr + rva;
            let end = start + binary.len() - rva;
            let section = Section::new(
                Cow::Borrowed("flat (generated)"),
                SectionKind::Text,
                true,
                &binary[rva..],
                addr,
                start,
                end,
            );

            sections.push(section);
        }

        if segments.is_empty() {
            let start = obj.relative_address_base() as PhysAddr;
            let end = start + binary.len();
            let segment = Segment {
                name: Cow::Borrowed("flat (generated)"),
                start,
                end,
            };

            segments.push(segment);
        }

        // sort segments/sections by their physical address
        segments.sort_unstable_by_key(|s| s.start);
        sections.sort_unstable_by_key(|s| s.start);

        let (instruction_tokens, instruction_width) = unsafe {
            match arch {
                Architecture::Riscv32 | Architecture::Riscv64 => (
                    std::mem::transmute(<riscv::Instruction as Decoded>::tokens as usize),
                    std::mem::transmute(<riscv::Instruction as Decoded>::width as usize),
                ),
                Architecture::Mips | Architecture::Mips64 => (
                    std::mem::transmute(<mips::Instruction as Decoded>::tokens as usize),
                    std::mem::transmute(<mips::Instruction as Decoded>::width as usize),
                ),
                Architecture::X86_64_X32 | Architecture::I386 => (
                    std::mem::transmute(<x86::Instruction as Decoded>::tokens as usize),
                    std::mem::transmute(<x86::Instruction as Decoded>::width as usize),
                ),
                Architecture::X86_64 => (
                    std::mem::transmute(<x64::Instruction as Decoded>::tokens as usize),
                    std::mem::transmute(<x64::Instruction as Decoded>::width as usize),
                ),
                Architecture::Arm => (
                    std::mem::transmute(<armv7::Instruction as Decoded>::tokens as usize),
                    std::mem::transmute(<armv7::Instruction as Decoded>::width as usize),
                ),
                Architecture::Aarch64 | Architecture::Aarch64_Ilp32 => (
                    std::mem::transmute(<aarch64::Instruction as Decoded>::tokens as usize),
                    std::mem::transmute(<aarch64::Instruction as Decoded>::width as usize),
                ),
                arch => return Err(Error::UnknownArchitecture(arch)),
            }
        };

        let mut index = Index::default();

        index.parse_dwarf(&obj, &sections).map_err(Error::Symbol)?;
        index.parse_pdb(&obj).map_err(Error::Symbol)?;
        index.parse_symbols(&obj).map_err(Error::Symbol)?;
        index.parse_imports(&obj).map_err(Error::Symbol)?;
        index.complete();

        let mut instructions = Vec::new();
        let mut errors = Vec::new();
        let max_instruction_width;

        match arch {
            Architecture::Riscv32 => {
                impl_recursion!(
                    &index,
                    &mut errors,
                    &mut instructions,
                    &mut sections,
                    max_instruction_width,
                    riscv::Decoder { is_64: false },
                    riscv
                )
            }
            Architecture::Riscv64 => {
                impl_recursion!(
                    &index,
                    &mut errors,
                    &mut instructions,
                    &mut sections,
                    max_instruction_width,
                    riscv::Decoder { is_64: true },
                    riscv
                )
            }
            Architecture::Mips | Architecture::Mips64 => {
                impl_recursion!(
                    &index,
                    &mut errors,
                    &mut instructions,
                    &mut sections,
                    max_instruction_width,
                    mips::Decoder::default(),
                    mips
                )
            }
            Architecture::X86_64_X32 | Architecture::I386 => {
                impl_recursion!(
                    &index,
                    &mut errors,
                    &mut instructions,
                    &mut sections,
                    max_instruction_width,
                    x86::Decoder::default(),
                    x86
                )
            }
            Architecture::X86_64 => {
                impl_recursion!(
                    &index,
                    &mut errors,
                    &mut instructions,
                    &mut sections,
                    max_instruction_width,
                    x64::Decoder::default(),
                    x64
                )
            }
            Architecture::Arm => {
                impl_recursion!(
                    &index,
                    &mut errors,
                    &mut instructions,
                    &mut sections,
                    max_instruction_width,
                    armv7::Decoder::default(),
                    armv7
                )
            },
            Architecture::Aarch64 | Architecture::Aarch64_Ilp32 => {
                impl_recursion!(
                    &index,
                    &mut errors,
                    &mut instructions,
                    &mut sections,
                    max_instruction_width,
                    aarch64::Decoder::default(),
                    aarch64
                )
            }
            _ => unreachable!(),
        };

        instructions.sort_unstable_by_key(|k| k.0);
        errors.sort_unstable_by_key(|k| k.0);

        log::complex!(
            w "[processor::parse] took ",
            y format!("{:#?}", now.elapsed()),
            w " to parse ",
            w format!("{path:?}.")
        );

        Ok(Self {
            entrypoint,
            path,
            sections,
            segments,
            errors,
            instructions,
            index,
            _file: file,
            _mmap: mmap,
            max_instruction_width,
            instruction_tokens,
            instruction_width,
            arch,
        })
    }

    /// Format address relative to a given section.
    #[inline]
    pub fn format_bytes(
        &self,
        addr: PhysAddr,
        len: usize,
        section: &Section,
        is_padded: bool,
    ) -> Option<String> {
        section.format_bytes(addr, len, self.max_instruction_width * 3 + 1, is_padded)
    }

    /// Relatively slow tokenization of an [`Instruction`].
    /// Xref's get resolved which requires some extra computation.
    pub fn instruction_tokens(&self, instruction: &Instruction, symbols: &Index) -> Vec<Token> {
        (self.instruction_tokens)(instruction, symbols)
    }

    pub fn instruction_width(&self, instruction: &Instruction) -> usize {
        (self.instruction_width)(instruction)
    }

    pub fn error_by_addr(&self, addr: PhysAddr) -> Option<&decoder::Error> {
        match self.errors.binary_search_by(|k| k.0.cmp(&addr)) {
            Ok(idx) => Some(&self.errors[idx].1),
            Err(..) => None,
        }
    }

    pub fn instruction_by_addr(&self, addr: PhysAddr) -> Option<&Instruction> {
        match self.instructions.binary_search_by(|k| k.0.cmp(&addr)) {
            Ok(idx) => Some(&self.instructions[idx].1),
            Err(..) => None,
        }
    }

    pub fn segments(&self) -> impl DoubleEndedIterator<Item = &Segment> {
        self.segments.iter()
    }

    /// Iterate through all non-debug sections.
    pub fn sections(&self) -> impl DoubleEndedIterator<Item = &Section> {
        self.sections.iter().filter(|section| section.loaded)
    }

    pub fn section_name(&self, addr: PhysAddr) -> Option<&str> {
        self.sections
            .iter()
            .find(|s| (s.start..=s.end).contains(&addr))
            .map(|s| &s.name as &str)
    }
}

impl Drop for Processor {
    /// Required `Drop` impl as [`Instruction`]'s a non-copy union.
    fn drop(&mut self) {
        for (_, instruction) in self.instructions.iter_mut() {
            match self.arch {
                Architecture::X86_64 => unsafe { ManuallyDrop::drop(&mut instruction.x64) },
                Architecture::X86_64_X32 | Architecture::I386 => unsafe {
                    ManuallyDrop::drop(&mut instruction.x86)
                },
                Architecture::Riscv64 | Architecture::Riscv32 => unsafe {
                    ManuallyDrop::drop(&mut instruction.riscv)
                },
                Architecture::Mips | Architecture::Mips64 => unsafe {
                    ManuallyDrop::drop(&mut instruction.mips)
                },
                _ => {}
            }
        }
    }
}
