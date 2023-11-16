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
    pub sections: Vec<Section>,
    errors: Vec<(Addr, decoder::Error)>,
    instructions: Vec<(Addr, Instruction)>,
    max_instruction_width: usize,
    instruction_tokens: fn(&Instruction) -> Vec<Token>,
    instruction_width: fn(&Instruction) -> usize,
    arch: Architecture,
}

impl Processor {
    pub fn new(sections: SectionIterator, arch: Architecture) -> Result<Self, Architecture> {
        let mut found_sections: Vec<Section> = Vec::new();
        let mut has_text_section = false;

        for section in sections {
            let name = section.name().unwrap_or("unknown").to_string();

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

            let section = Section {
                name,
                kind: section.kind(),
                bytes: bytes.into_owned(),
                start: section.address() as Addr,
                end: (section.address() + section.size()) as Addr,
            };

            if section.kind == SectionKind::Text {
                has_text_section = true;
            }

            found_sections.push(section);
        }

        if !has_text_section {
            // FIXME
            std::process::exit(1);
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

    pub fn bytes_by_addr(&self, addr: usize) -> Option<&[u8]> {
        for section in self.sections.iter() {
            if (section.start..=section.end).contains(&addr) {
                return addr.checked_sub(section.start).and_then(|addr| section.bytes.get(addr..));
            }
        }

        None
    }

    pub fn format_bytes(&self, addr: Addr, len: usize) -> String {
        let bytes = self.bytes_by_addr(addr).unwrap();
        let bytes = &bytes[..std::cmp::min(bytes.len(), len as usize)];

        decoder::encode_hex_bytes_truncated(bytes, self.max_instruction_width * 3 + 1)
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
