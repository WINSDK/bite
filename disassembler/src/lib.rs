//! Consumes decoder crates and provides an interface to interact with the decoders.

use std::collections::BTreeMap;
use std::collections::VecDeque;

use decoder::{encode_hex_bytes_truncated, Decodable, Failed, Decoded};
pub use decoder::{ToTokens, TokenStream};

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
pub struct Processor<D: Decodable> {
    pub section: Vec<u8>,
    pub entrypoint: usize,
    pub base_addr: usize,
    pub decoder: D,
    pub instructions: BTreeMap<usize, Result<D::Instruction, D::Error>>,
}

pub type MaybeInstruction<'a> = Result<&'a dyn Decoded, &'a dyn Failed>;

pub trait InspectProcessor {
    fn iter(&self) -> Box<dyn Iterator<Item = (usize, MaybeInstruction)> + '_>;
    fn base_addr(&self) -> usize;
    fn section(&self) -> &[u8];
    fn bytes(&self, instruction: MaybeInstruction, addr: usize) -> String;
}

impl<D: Decodable> InspectProcessor for Processor<D> {
    fn iter(&self) -> Box<dyn Iterator<Item = (usize, MaybeInstruction)> + '_> {
        Box::new(self.instructions.iter().map(|(addr, inst)| {
            (
                *addr,
                match inst {
                    Ok(ref val) => Ok(val as &dyn Decoded),
                    Err(ref err) => Err(err as &dyn Failed),
                },
            )
        }))
    }

    fn base_addr(&self) -> usize {
        self.base_addr
    }

    fn section(&self) -> &[u8] {
        &self.section[..]
    }

    fn bytes(&self, instruction: MaybeInstruction, addr: usize) -> String {
        let rva = addr - self.base_addr;
        let bytes = match instruction {
            Ok(instruction) => &self.section[rva..][..instruction.width()],
            Err(err) => &self.section[rva..][..err.incomplete_width()],
        };

        encode_hex_bytes_truncated(bytes, self.decoder.max_width() * 3 + 1)
    }
}

impl<D: Decodable> Processor<D> {
    pub fn new(section: Vec<u8>, base_addr: usize, entrypoint: usize, decoder: D) -> Self {
        Self {
            section,
            entrypoint,
            base_addr,
            decoder,
            instructions: BTreeMap::new(),
        }
    }

    pub fn recurse(&mut self) {
        let mut addresses = VecDeque::with_capacity(1024);
        addresses.push_back(self.entrypoint);

        match self.entrypoint.checked_sub(self.base_addr) {
            Some(entrypoint) => addresses.push_back(entrypoint),
            None => {
                eprintln!("failed to calculate entrypoint, defaulting to 0x1000");
                addresses.push_back(self.base_addr + 0x1000);
            }
        }

        while let Some(addr) = addresses.pop_front() {
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
            addresses.push_back(addr + width);
        }
    }

    fn bytes_by_addr<'a>(&'a self, addr: usize) -> Option<&'a [u8]> {
        addr.checked_sub(self.base_addr).and_then(|addr| self.section.get(addr..))
    }
}
