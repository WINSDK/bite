use crate::{Error, PhysAddr, Section, Segment, VirtAddr};
use decoder::{Decodable, Decoded};
use object::{Object, ObjectSegment};
use tokenizing::{colors, Token};

use object::{Architecture, BinaryFormat, ObjectSection, SectionKind};
use x86_64::long_mode as x64;
use x86_64::protected_mode as x86;

use std::borrow::Cow;
use std::mem::ManuallyDrop;

pub union Instruction {
    x86: ManuallyDrop<x86_64::protected_mode::Instruction>,
    x64: ManuallyDrop<x86_64::long_mode::Instruction>,
    riscv: ManuallyDrop<riscv::Instruction>,
    mips: ManuallyDrop<mips::Instruction>,
}

macro_rules! impl_recursion {
    ($symbols:expr, $errors:expr, $instructions:expr, $sections:expr,
     $max_instruction_width:expr, $decoder:expr, $arch:ident) => {{
        $max_instruction_width = $decoder.max_width();

        let width_guess = if $max_instruction_width == 4 {
            2
        } else {
            5
        };

        for section in $sections.iter().filter(|s| s.kind == SectionKind::Text) {
            let mut reader = decoder::Reader::new(&section.bytes);
            let mut addr = section.start;

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
            log::PROGRESS.set("Decoding instructions", section.bytes.len() / width_guess);

            loop {
                match $decoder.decode(&mut reader) {
                    Ok(mut instruction) => {
                        instruction.find_xrefs(addr, $symbols);

                        let width = instruction.width();
                        $instructions.push((addr, Instruction {
                            $arch: std::mem::ManuallyDrop::new(instruction)
                        }));

                        addr += width;
                    }
                    Err(error) => {
                        if error.kind == decoder::ErrorKind::ExhaustedInput {
                            break;
                        }

                        let width = error.size();
                        $errors.push((addr, error));
                        addr += width;
                    }
                }

                log::PROGRESS.step();
            }
        }
    }};
}

/// Architecture agnostic analysis of a module.
pub struct Processor {
    /// Where the binary is located.
    pub path: std::path::PathBuf,

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

    /// Symbol lookup by physical address.
    symbols: symbols::Index,

    /// How many bytes an instruction given the architecture.
    max_instruction_width: usize,

    /// Function pointer to an [`Instruction`]'s implementation of [`Decoded::tokens`].
    instruction_tokens: fn(&Instruction) -> Vec<Token>,

    /// Function pointer to an [`Instruction`]'s implementation of [`Decoded::width`].
    instruction_width: fn(&Instruction) -> usize,

    /// Target's instruction set.
    arch: Architecture,
}

impl Processor {
    /// Parse module that doesn't have an associated [`object::File`] yet.
    pub fn parse_unknown<P: AsRef<std::path::Path>>(path: P) -> Result<Self, Error> {
        let binary = std::fs::read(&path).map_err(Error::IO)?;
        let obj = object::File::parse(&binary[..]).map_err(Error::IncompleteObject)?;
        Self::parse(path, &binary[..], &obj)
    }

    pub fn parse<P: AsRef<std::path::Path>>(
        path: P,
        binary: &[u8],
        obj: &object::File<'_>,
    ) -> Result<Self, Error> {
        let now = std::time::Instant::now();

        let mut sections = Vec::new();
        let mut segments = Vec::new();

        let arch = obj.architecture();

        for segment in obj.segments() {
            let name = match segment.name() {
                Ok(Some(name)) => Cow::Owned(name.to_string()),
                _ => Cow::Borrowed("unnamed"),
            };

            let phys_start = segment.file_range().0 as PhysAddr;
            let phys_end = phys_start + segment.size() as PhysAddr;

            let segment = Segment {
                name,
                addr: segment.address() as VirtAddr,
                start: phys_start,
                end: phys_end,
            };

            segments.push(segment);
        }

        for section in obj.sections() {
            // there appear to be sections that aren't meant to be conventionally loaded
            // they appear to be sections meant for the debugger to load
            if section.address() == 0 {
                continue;
            }

            let name = match section.name() {
                Ok(name) => Cow::Owned(name.to_string()),
                _ => Cow::Borrowed("unnamed"),
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

            if DWARF_SECTIONS.contains(&&*name) {
                continue;
            }

            let bytes = match section.uncompressed_data() {
                Ok(uncompressed) => uncompressed,
                Err(..) => {
                    log::complex!(
                        w "[processor::new] ",
                        y format!("failed to decompress section {name}.")
                    );

                    continue;
                }
            };

            let start = section.address() as PhysAddr;
            let end = bytes.len() + start;
            let section = Section {
                name,
                kind: section.kind(),
                bytes: bytes.into_owned(),
                addr: section.address() as VirtAddr,
                start,
                end,
            };

            sections.push(section);
        }

        if sections.is_empty() {
            let addr = if obj.format() == BinaryFormat::Pe {
                0x1000
            } else {
                0
            };

            let rva = obj.entry() as VirtAddr - obj.relative_address_base() as VirtAddr;
            let start = obj.relative_address_base() as VirtAddr + rva;
            let end = start + binary.len() - rva;
            let section = Section {
                name: Cow::Borrowed("flat (generated)"),
                kind: SectionKind::Text,
                bytes: binary[rva..].to_vec(),
                addr,
                start,
                end,
            };

            sections.push(section);
        }

        if segments.is_empty() {
            let addr = if obj.format() == BinaryFormat::Pe {
                0x1000
            } else {
                0
            };

            let start = obj.relative_address_base() as VirtAddr;
            let end = start + binary.len();
            let segment = Segment {
                name: Cow::Borrowed("flat (generated)"),
                addr,
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
                arch => return Err(Error::UnknownArchitecture(arch)),
            }
        };

        let mut symbols = symbols::Index::new();

        symbols.parse_debug(&obj).map_err(Error::IncompleteSymbolTable)?;
        symbols.parse_imports(&binary[..], &obj).map_err(Error::IncompleteImportTable)?;

        let mut instructions = Vec::new();
        let mut errors = Vec::new();
        let max_instruction_width;

        match arch {
            Architecture::Riscv32 => {
                impl_recursion!(
                    &symbols,
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
                    &symbols,
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
                    &symbols,
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
                    &symbols,
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
                    &symbols,
                    &mut errors,
                    &mut instructions,
                    &mut sections,
                    max_instruction_width,
                    x64::Decoder::default(),
                    x64
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
            w format!("{:?}.", path.as_ref())
        );

        Ok(Self {
            path: path.as_ref().to_path_buf(),
            sections,
            segments,
            errors,
            instructions,
            symbols,
            max_instruction_width,
            instruction_tokens,
            instruction_width,
            arch,
        })
    }

    /// Format address relative to a given section.
    pub fn format_bytes(&self, addr: PhysAddr, len: usize, section: &Section) -> Option<String> {
        section.format_bytes(addr, len, self.max_instruction_width * 3 + 1)
    }

    pub fn instruction_tokens(&self, instruction: &Instruction) -> Vec<Token> {
        (self.instruction_tokens)(&instruction)
    }

    pub fn instruction_width(&self, instruction: &Instruction) -> usize {
        (self.instruction_width)(&instruction)
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

    pub fn sections(&self) -> impl DoubleEndedIterator<Item = &Section> {
        self.sections.iter()
    }

    pub fn symbols(&self) -> &symbols::Index {
        &self.symbols
    }

    pub fn functions(&self, range: std::ops::Range<usize>) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        let lines_to_read = range.end - range.start;
        let lines = self
            .symbols()
            .iter()
            .filter(|(_, func)| !func.intrinsic())
            .skip(range.start)
            .take(lines_to_read + 10);

        // for each instruction
        for (addr, symbol) in lines {
            tokens.push(Token::from_string(format!("{addr:0>10X}"), colors::WHITE));
            tokens.push(Token::from_str(" | ", colors::WHITE));

            if let Some(module) = symbol.module() {
                tokens.push(Token::from_string(module.to_string(), colors::MAGENTA));
                tokens.push(Token::from_str("!", colors::GRAY60));
            }

            for token in symbol.name() {
                tokens.push(token.clone());
            }

            tokens.push(Token::from_str("\n", colors::WHITE));
        }

        tokens
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
