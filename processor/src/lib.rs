mod fmt;
mod blocks;

use decoder::{Decodable, Decoded};
use object::{Endianness, Object, ObjectSegment};
use object::{Architecture, BinaryFormat};
use object::read::File as ObjectFile;
use processor_shared::{AddressMap, Addressed, PhysAddr, Section, SectionKind, Segment};
use debugvault::Index;
use tokenizing::Token;
use binformat::{elf, macho, pe, RawSymbol};

use memmap2::Mmap;
use x86_64::long_mode as x64;
use x86_64::protected_mode as x86;
use arm::armv7 as armv7;
use arm::armv8::a64 as aarch64;

use std::fs::File;
use std::mem::ManuallyDrop;

pub use blocks::{BlockContent, Block};

/// FIXME: This is way too large and way too broad.
///        Especially since these are being started for any address with a faulty decoding.
pub enum Error {
    IO(std::io::Error),
    Object(object::Error),
    Debug(debugvault::Error),
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

        for section in $sections.iter().filter(|s| s.kind == SectionKind::Code) {
            let mut prev_inst = None;
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
                        instruction.update_rel_addrs(ip, prev_inst);

                        let width = instruction.width();
                        $instructions.push(Addressed {
                            addr: ip,
                            item: Instruction {
                                $arch: std::mem::ManuallyDrop::new(instruction)
                            }
                        });

                        prev_inst = $instructions.last().map(|inst| {
                            unsafe { &*inst.item.$arch }
                        });
                        ip += width;
                    }
                    Err(error) => {
                        if error.kind == decoder::ErrorKind::ExhaustedInput {
                            break;
                        }

                        let width = error.size();
                        $errors.push(Addressed {
                            addr: ip,
                            item: error
                        });
                        prev_inst = None;
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
    errors: AddressMap<decoder::Error>,

    /// Successfully decoded instructions.
    /// Sorted by address.
    instructions: AddressMap<Instruction>,

    /// How many bytes an instruction given the architecture.
    max_instruction_width: usize,

    /// Function pointer to an [`Instruction`]'s implementation of [`Decoded::tokens`].
    instruction_tokens: fn(&Instruction, &Index) -> Vec<Token>,

    /// Function pointer to an [`Instruction`]'s implementation of [`Decoded::width`].
    instruction_width: fn(&Instruction) -> usize,

    /// Target's instruction set.
    arch: Architecture,

    /// Target's endianness.
    endianness: Endianness,
}

impl Processor {
    pub fn parse<P: AsRef<std::path::Path>>(path: P) -> Result<Self, Error> {
        let file = std::fs::File::open(path.as_ref()).map_err(Error::IO)?;
        let mmap = unsafe { Mmap::map(&file).map_err(Error::IO)? };
        let binary: &'static [u8] = unsafe { std::mem::transmute(&mmap[..]) };
        let obj = ObjectFile::parse(binary)?;

        let path = path.as_ref().to_path_buf();
        let now = std::time::Instant::now();

        let mut syms = AddressMap::default();
        let mut sections = Vec::new();
        match &obj {
            object::File::MachO32(macho) => {
                let debug_info = macho::MachoDebugInfo::parse(macho)?;
                sections.extend(debug_info.sections);
                syms.extend(debug_info.syms);
            }
            object::File::MachO64(macho) => {
                let debug_info = macho::MachoDebugInfo::parse(macho)?;
                sections.extend(debug_info.sections);
                syms.extend(debug_info.syms);
            }
            object::File::Elf32(elf) => {
                let debug_info = elf::ElfDebugInfo::parse(elf)?;
                sections.extend(debug_info.sections);
                syms.extend(debug_info.syms);
            }
            object::File::Elf64(elf) => {
                let debug_info = elf::ElfDebugInfo::parse(elf)?;
                sections.extend(debug_info.sections);
                syms.extend(debug_info.syms);
            }
            object::File::Pe32(pe) => {
                let debug_info = pe::PeDebugInfo::parse(pe)?;
                // sections.extend(debug_info.sections);
                syms.extend(debug_info.syms);
                todo!()
            }
            object::File::Pe64(pe) => {
                let debug_info = pe::PeDebugInfo::parse(pe)?;
                // sections.extend(debug_info.sections);
                syms.extend(debug_info.syms);
                todo!()
            }
            _ => {}
        }

        for section in sections.iter() {
            syms.push(Addressed {
                addr: section.start,
                item: RawSymbol { name: &section.name, module: None }
            });
        }

        let index = Index::parse(&obj, &path, syms).map_err(Error::Debug)?;
        let entrypoint = index.get_func_by_name("entry").unwrap_or(0);

        if entrypoint != 0 {
            log::complex!(
                w "[processor::parse] entrypoint ",
                g format!("{entrypoint:#X}"),
                w ".",
            );
        }

        let mut segments = Vec::new();
        for segment in obj.segments() {
            let name = segment.name()?.unwrap_or("unknown").to_string();
            let start = segment.address() as PhysAddr;
            let end = start + segment.size() as PhysAddr;

            segments.push(Segment { name, start, end });
        }

        segments.sort_unstable_by_key(|s| s.start);
        sections.sort_unstable_by_key(|s| s.start);

        if sections.is_empty() {
            let base = if obj.format() == BinaryFormat::Pe {
                0x1000
            } else {
                0
            };

            let rva = entrypoint - obj.relative_address_base() as PhysAddr;
            let start = obj.relative_address_base() as PhysAddr + rva;
            let end = start + binary.len() - rva;
            let section = Section::new(
                "flat".to_string(),
                "GENERATED",
                SectionKind::Code,
                &binary[rva..],
                base + start,
                end,
            );

            sections.push(section);
        }

        if segments.is_empty() {
            let start = obj.relative_address_base() as PhysAddr;
            let end = start + binary.len();
            let segment = Segment {
                name: "flat (generated)".to_string(),
                start,
                end,
            };

            segments.push(segment);
        }

        let arch = obj.architecture();
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

        let mut instructions = AddressMap::default();
        let mut errors = AddressMap::default();
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

        instructions.sort_unstable();
        errors.sort_unstable();

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
            endianness: obj.endianness(),
        })
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
        match self.errors.search(addr) {
            Ok(idx) => Some(&self.errors[idx].item),
            Err(..) => None,
        }
    }

    pub fn instruction_by_addr(&self, addr: PhysAddr) -> Option<&Instruction> {
        match self.instructions.search(addr) {
            Ok(idx) => Some(&self.instructions[idx].item),
            Err(..) => None,
        }
    }

    pub fn segments(&self) -> impl DoubleEndedIterator<Item = &Segment> {
        self.segments.iter()
    }

    /// Iterate through all non-debug sections.
    pub fn sections(&self) -> impl DoubleEndedIterator<Item = &Section> {
        self.sections.iter()
    }

    /// First try to find a section that matches, then if it exists, try to find a
    /// section that matches better.
    ///
    /// E.g. `addr` might be the end of one section but the start of another, it will find the
    /// later section.
    pub fn section_by_addr(&self, addr: PhysAddr) -> Option<&Section> {
        let mut found = None;
        for section in self.sections() {
            if addr >= section.start && addr <= section.end {
                found = Some(section);
            }
        }

        found
    }

    pub fn section_name(&self, addr: PhysAddr) -> Option<&str> {
        self.sections
            .iter()
            .filter(|sec| !matches!(sec.kind, SectionKind::Unloaded | SectionKind::Debug))
            .find(|s| (s.start..=s.end).contains(&addr))
            .map(|s| &s.name as &str)
    }
}

impl Drop for Processor {
    /// Required `Drop` impl as [`Instruction`]'s a non-copy union.
    fn drop(&mut self) {
        for Addressed { item: inst, .. } in self.instructions.iter_mut() {
            match self.arch {
                Architecture::X86_64 => unsafe { ManuallyDrop::drop(&mut inst.x64) },
                Architecture::X86_64_X32 | Architecture::I386 => unsafe {
                    ManuallyDrop::drop(&mut inst.x86)
                },
                Architecture::Riscv64 | Architecture::Riscv32 => unsafe {
                    ManuallyDrop::drop(&mut inst.riscv)
                },
                Architecture::Mips | Architecture::Mips64 => unsafe {
                    ManuallyDrop::drop(&mut inst.mips)
                },
                _ => {}
            }
        }
    }
}

impl From<object::Error> for Error {
    fn from(err: object::Error) -> Self {
        Error::Object(err)
    }
}
