use crate::{Addr, Section};
use decoder::{Decodable, Decoded};
use tokenizing::Token;

use object::{Architecture, ObjectSection, SectionIterator, SectionKind};
use x86_64::long_mode as x64;
use x86_64::protected_mode as x86;

use std::mem::ManuallyDrop;

pub union Instruction {
    x86: ManuallyDrop<x86_64::protected_mode::Instruction>,
    x64: ManuallyDrop<x86_64::long_mode::Instruction>,
    riscv: ManuallyDrop<riscv::Instruction>,
    mips: ManuallyDrop<mips::Instruction>,
}

macro_rules! impl_recursion {
    ($this:expr, $index:expr, $arch:ident, $decoder:expr) => {{
        let decoder = $decoder;
        $this.max_instruction_width = decoder.max_width();
        for section in $this.sections.iter().filter(|s| s.kind == SectionKind::Text) {
            let mut reader = decoder::Reader::new(&section.bytes);
            let mut addr = section.start;

            log::complex!(
                w "[processor::recurse] analyzing section ",
                b &section.name,
                w " <",
                g format!("{:x}", section.start),
                w "..",
                g format!("{:x}", section.end),
                w ">.",
            );

            loop {
                match decoder.decode(&mut reader) {
                    Ok(mut instruction) => {
                        instruction.find_xrefs(addr, $index);

                        let width = instruction.width();
                        $this.instructions.push((addr, Instruction {
                            $arch: std::mem::ManuallyDrop::new(instruction)
                        }));

                        addr += width;
                    }
                    Err(error) => {
                        if error.kind == decoder::ErrorKind::ExhaustedInput {
                            break;
                        }

                        let width = error.size();
                        $this.errors.push((addr, error));
                        addr += width;
                    }
                }
            }
        }
    }};
}

/// Architecture agnostic code analysis.
pub struct Processor {
    /// Object's sections sorted by address.
    sections: Vec<Section>,

    /// Errors occurred in decoding instructions.
    /// Sorted by address.
    pub errors: Vec<(Addr, decoder::Error)>,

    /// Successfully decoded instructions.
    /// Sorted by address.
    pub instructions: Vec<(Addr, Instruction)>,

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
    pub fn new(sections: SectionIterator, arch: Architecture) -> Result<Self, Architecture> {
        let mut found_sections: Vec<Section> = Vec::new();
        let mut has_text_section = false;

        for section in sections {
            // there appear to be sections that aren't meant to be conventionally loaded
            // they appear to be sections meant for the debugger to load
            if section.address() == 0 {
                continue;
            }

            let name = section.name().unwrap_or("unknown").to_string();

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

            let start = section.address() as Addr;
            let end = bytes.len() + start;
            let section = Section {
                name,
                kind: section.kind(),
                bytes: bytes.into_owned(),
                start,
                end,
            };

            if section.kind == SectionKind::Text {
                has_text_section = true;
            }

            found_sections.push(section);
        }

        if !has_text_section {
            // FIXME
            unimplemented!("We don't handle objects that don't have an code sections.");
        }

        // sort sections by their address
        found_sections.sort_unstable_by_key(|s| s.start);

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
                arch => return Err(arch),
            }
        };

        Ok(Self {
            sections: found_sections,
            errors: Vec::new(),
            instructions: Vec::new(),
            max_instruction_width: 0,
            instruction_tokens,
            instruction_width,
            arch,
        })
    }

    pub fn recurse(&mut self, index: &symbols::Index) {
        match self.arch {
            Architecture::Riscv32 => {
                impl_recursion!(self, index, riscv, riscv::Decoder { is_64: false })
            }
            Architecture::Riscv64 => {
                impl_recursion!(self, index, riscv, riscv::Decoder { is_64: true })
            }
            Architecture::Mips | Architecture::Mips64 => {
                impl_recursion!(self, index, mips, mips::Decoder::default())
            }
            Architecture::X86_64_X32 | Architecture::I386 => {
                impl_recursion!(self, index, x86, x86::Decoder::default())
            }
            Architecture::X86_64 => {
                impl_recursion!(self, index, x64, x64::Decoder::default())
            }
            _ => {}
        };

        self.instructions.sort_unstable_by_key(|k| k.0);
        self.errors.sort_unstable_by_key(|k| k.0);
    }

    /// Format address relative to a given section.
    pub fn format_bytes(&self, addr: Addr, len: usize, section: &Section) -> Option<String> {
        section.format_bytes(addr, len, self.max_instruction_width * 3 + 1)
    }

    pub fn instruction_tokens(&self, instruction: &Instruction) -> Vec<Token> {
        (self.instruction_tokens)(&instruction)
    }

    pub fn instruction_width(&self, instruction: &Instruction) -> usize {
        (self.instruction_width)(&instruction)
    }

    pub fn error_by_addr(&self, addr: Addr) -> Option<&decoder::Error> {
        match self.errors.binary_search_by(|k| k.0.cmp(&addr)) {
            Ok(idx) => Some(&self.errors[idx].1),
            Err(..) => None,
        }
    }

    pub fn instruction_by_addr(&self, addr: Addr) -> Option<&Instruction> {
        match self.instructions.binary_search_by(|k| k.0.cmp(&addr)) {
            Ok(idx) => Some(&self.instructions[idx].1),
            Err(..) => None,
        }
    }

    /// Find the previous item given an address.
    pub fn prev_item(&self, addr: Addr) -> Option<Addr> {
        let i1 = self.instructions.binary_search_by(|(a, _)| a.cmp(&addr));
        let i2 = self.errors.binary_search_by(|(a, _)| a.cmp(&addr));

        match (i1, i2) {
            // there is an item at this addr
            (Ok(_), _) | (_, Ok(_)) => None,
            (Err(i1), Err(i2)) => {
                if i1 == 0 && i2 == 0 {
                    return None;
                }

                if i1 == 0 {
                    return Some(self.errors[i2 - 1].0);
                }

                if i2 == 0 {
                    return Some(self.instructions[i1 - 1].0);
                }

                let a1 = self.instructions[i1 - 1].0;
                let a2 = self.errors[i2 - 1].0;

                Some(std::cmp::max(a1, a2))
            }
        }
    }

    pub fn next_item(&self, addr: Addr) -> Option<Addr> {
        let i1 = self.instructions.binary_search_by(|(a, _)| a.cmp(&addr));
        let i2 = self.errors.binary_search_by(|(a, _)| a.cmp(&addr));

        match (i1, i2) {
            // there is an item at this addr
            (Ok(_), _) | (_, Ok(_)) => None,
            (Err(i1), Err(i2)) => {
                if i1 == self.instructions.len() && i2 == self.errors.len() {
                    return None;
                }

                if i1 == self.instructions.len() {
                    return Some(self.errors[i2].0);
                }

                if i2 == self.errors.len() {
                    return Some(self.instructions[i1].0);
                }

                let a1 = self.instructions[i1].0;
                let a2 = self.errors[i2].0;

                Some(std::cmp::min(a1, a2))
            }
        }
    }

    pub fn sections(&self) -> impl DoubleEndedIterator<Item = &Section> {
        self.sections.iter()
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
