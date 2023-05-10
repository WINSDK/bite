#![allow(dead_code)]
//! Consumes decoder crates and provides an interface to interact with the decoders.

use std::collections::BTreeMap;
use std::collections::VecDeque;

use tokenizing::Token;
use decoder::{encode_hex_bytes_truncated, Decodable, Decoded, Failed};
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

const MAX_OPERANDS: usize = 4;

#[derive(Debug)]
enum Xref {
    Data(Vec<Token<'static>>),
    Label(Vec<Token<'static>>),
}

#[derive(Debug, Default)]
struct XrefOperand {
    addr: usize,
    xref: Vec<Token<'static>>,
}

#[derive(Debug)]
pub struct Metadata<D: Decoded> {
    instruction: D,
    shadowing: [Option<XrefOperand>; MAX_OPERANDS]
}

impl<D: Decoded> Metadata<D> {
    fn new(instruction: D) -> Self {
        Self {
            instruction,
            shadowing: Default::default(),
        }
    }

    pub fn tokens(&self) -> TokenStream {
        let mut stream = TokenStream::new();
        self.instruction.tokenize(&mut stream);

        stream
    }
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
    pub parsed: BTreeMap<usize, Result<Metadata<D::Instruction>, D::Error>>,
}

pub type MaybeInstruction<'a> = Result<&'a dyn Decoded, &'a dyn Failed>;

pub trait InspectProcessor {
    fn iter(&self) -> Box<dyn DoubleEndedIterator<Item = (usize, MaybeInstruction)> + '_>;
    fn instruction_count(&self) -> usize;
    fn base_addr(&self) -> usize;
    fn section(&self) -> &[u8];
    fn bytes(&self, instruction: MaybeInstruction, addr: usize) -> String;
}

impl<D: Decodable> InspectProcessor for Processor<D> {
    fn iter(&self) -> Box<dyn DoubleEndedIterator<Item = (usize, MaybeInstruction)> + '_> {
        Box::new(self.parsed.iter().map(|(addr, inst)| {
            (
                *addr,
                match inst {
                    Ok(ref val) => Ok(&val.instruction as &dyn Decoded),
                    Err(ref err) => Err(err as &dyn Failed),
                },
            )
        }))
    }

    fn instruction_count(&self) -> usize {
        self.parsed.len()
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
            parsed: BTreeMap::new(),
        }
    }

    pub fn recurse(&mut self) {
        let mut unexplored_data = VecDeque::with_capacity(1024);
        let mut raw_instructions = VecDeque::with_capacity(1024);

        // TODO: recurse starting from entrypoint, following jumps
        // unexplored_data.push_back(self.entrypoint);
        unexplored_data.push_back(self.base_addr);

        match self.entrypoint.checked_sub(self.base_addr) {
            Some(entrypoint) => unexplored_data.push_back(entrypoint),
            None => {
                eprintln!("failed to calculate entrypoint, defaulting to 0x1000");
                unexplored_data.push_back(self.base_addr + 0x1000);
            }
        }

        while let Some(addr) = unexplored_data.pop_front() {
            // don't visit addresses that are already decoded
            if self.parsed.contains_key(&addr) {
                continue;
            }

            // don't visit addresses that are outside of the section
            let bytes = match self.bytes_by_addr(addr) {
                Some(bytes) => bytes,
                None => continue,
            };

            let mut reader = decoder::Reader::new(bytes);
            let instruction = self.decoder.decode(&mut reader);
            let width = match instruction {
                Ok(inst) => {
                    let width = inst.width();
                    raw_instructions.push_back((addr, inst));
                    width
                }
                Err(err) if !err.is_complete() => continue,
                Err(err) => {
                    let width = err.incomplete_width();
                    self.parsed.insert(addr, Err(err));
                    width
                }
            };

            unexplored_data.push_back(addr + width);
        }

        while let Some((addr, instruction)) = raw_instructions.pop_front() {
            let instruction: D::Instruction = instruction;

            self.parsed.insert(addr, Ok(Metadata::new(instruction)));
        }
    }

    fn bytes_by_addr<'a>(&'a self, addr: usize) -> Option<&'a [u8]> {
        addr.checked_sub(self.base_addr).and_then(|addr| self.section.get(addr..))
    }
}
