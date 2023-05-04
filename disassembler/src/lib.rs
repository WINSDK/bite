//! Consumes decoder crates and provides an interface to interact with the decoders.

use std::collections::BTreeMap;
use std::collections::VecDeque;

use decoder::Decoded;

use decoder::{encode_hex_bytes_truncated, Complete, Decodable};
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

pub trait InspectProcessor {
    fn iter(&self) -> Box<dyn Iterator<Item = (usize, &dyn InspectInstruction)> + '_>;
    fn base_addr(&self) -> usize;
    fn section(&self) -> &[u8];
    fn max_width(&self) -> usize;
}

pub trait InspectInstruction {
    fn tokens(&self) -> TokenStream;
    fn bytes(&self, proc: &dyn InspectProcessor, addr: usize) -> String;
}

impl<D: Decoded + ToTokens> InspectInstruction for D {
    fn tokens(&self) -> TokenStream {
        let mut stream = TokenStream::new();
        self.tokenize(&mut stream);
        stream
    }

    fn bytes(&self, proc: &dyn InspectProcessor, addr: usize) -> String {
        let rva = addr - proc.base_addr();
        encode_hex_bytes_truncated(
            &proc.section()[rva..][..self.width()],
            proc.max_width() * 3 + 1,
        )
    }
}

impl<D: Decodable> InspectProcessor for Processor<D> {
    fn iter(&self) -> Box<dyn Iterator<Item = (usize, &dyn InspectInstruction)> + '_> {
        Box::new(
            self.instructions
                .iter()
                .map(|(addr, inst)| (*addr, inst.as_ref().unwrap() as &dyn InspectInstruction)),
        )
    }

    fn base_addr(&self) -> usize {
        self.base_addr
    }

    fn section(&self) -> &[u8] {
        &self.section[..]
    }

    fn max_width(&self) -> usize {
        self.decoder.max_width()
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
        addr.checked_sub(self.base_addr)
            .and_then(|addr| self.section.get(addr..))
    }
}
