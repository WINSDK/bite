//! Consumes decoder crates and provides an interface to interact with the decoders.

use std::collections::BTreeMap;
use std::collections::VecDeque;

use decoder::Decoded;

pub use decoder::{Complete, Decodable, ToTokens, TokenStream, encode_hex_bytes_truncated};

pub mod x86 {
    pub use x86_64::protected_mode::Decoder;
}

pub mod x64 {
    pub use x86_64::long_mode::Decoder;
}

pub mod riscv {
    pub use riscv::Decoder;
}

pub mod mips {
    pub use mips::Decoder;
}

/// Recursive decent disassembler that inspect one given section.
/// It currently has the limitation of only being able to inspect the section
/// where a given binaries entrypoint is.
#[derive(Debug)]
pub struct Processor<'data, D: Decodable> {
    section: &'data [u8],
    pub base_addr: usize,
    entrypoint: usize,
    addresses: VecDeque<usize>,
    pub decoder: D,
    pub instructions: BTreeMap<usize, Result<D::Instruction, D::Error>>,
}

impl<'data, D: Decodable> Processor<'data, D> {
    pub fn new(section: &'data [u8], base_addr: usize, entrypoint: usize, decoder: D) -> Self {
        Self {
            section,
            base_addr,
            entrypoint,
            addresses: VecDeque::with_capacity(1024),
            decoder,
            instructions: BTreeMap::new(),
        }
    }

    pub fn recurse(&mut self) {
        self.addresses.push_back(self.entrypoint);

        match self.entrypoint.checked_sub(self.base_addr) {
            Some(entrypoint) => self.addresses.push_back(entrypoint),
            None => {
                eprintln!("failed to calculate entrypoint, defaulting to 0x1000");
                self.addresses.push_back(self.base_addr + 0x1000);
            }
        }

        while let Some(addr) = self.addresses.pop_front() {
            // don't visit addresses that are already decoded
            if self.instructions.contains_key(&addr) {
                continue;
            }

            // don't visit addresses that are outside of the section
            let bytes = match self.bytes_by_addr(addr) {
                Some(bytes) => bytes,
                None => continue,
            };

            let mut reader = decoder::Reader::new(bytes);
            let instruction = self.decoder.decode(&mut reader);
            let width = match &instruction {
                Ok(inst) => {
                    if inst.is_jump() {}
                    inst.width()
                }
                Err(err) if !err.is_complete() => continue,
                Err(err) => err.incomplete_width(),
            };

            self.instructions.insert(addr, instruction);
            self.addresses.push_back(addr + width);
        }
    }

    fn bytes_by_addr(&self, addr: usize) -> Option<&'data [u8]> {
        addr.checked_sub(self.base_addr)
            .and_then(|addr| self.section.get(addr..))
    }

    pub fn get_instruction_bytes(
        &self,
        rva: usize,
        instruction: &Result<D::Instruction, D::Error>,
    ) -> &'data [u8] {
        match instruction {
            Ok(instruction) => &self.section[rva..][..instruction.width()],
            Err(err) => &self.section[rva..][..err.incomplete_width()],
        }
    }
}
