/// ```text
/// <block>    = <section> <labelled>
/// <section>  = <section-end> <section-start>
///            | <section-start>
///            | <section-end>
/// <labelled> = <label> <real>
/// <real>     = <instruction> | <error> | <bytes>
/// ```

use std::sync::Arc;

use debugvault::Symbol;
use processor_shared::{encode_hex_bytes_truncated, Section};
use tokenizing::{colors, Token, TokenStream};

use crate::Processor;

#[derive(Debug)]
pub enum Block {
    SectionStart {
        section: Section,
    },
    SectionEnd {
        section: Section,
    },
    Label {
        symbol: Arc<Symbol>,
    },
    Instruction {
        addr: usize,
        inst: Vec<Token>,
        bytes: String,
    },
    Error {
        addr: usize,
        err: decoder::ErrorKind,
        bytes: String,
    },
    Bytes {
        addr: usize,
        bytes: Vec<u8>,
    },
}

impl Block {
    /// Length of block when tokenized.
    pub fn len(&self) -> usize {
        match self {
            Self::SectionStart { .. } => 2,
            Self::SectionEnd { .. } => 2,
            Self::Label { .. } => 2,
            Self::Instruction { .. } => 1,
            Self::Error { .. } => 1,
            Self::Bytes { bytes, .. } => (bytes.len() / 32) + 1,
        }
    }

    pub fn tokenize(&self, stream: &mut TokenStream) {
        match self {
            Self::Label { symbol } => {
                stream.push("\n<", colors::BLUE);
                stream.inner.extend_from_slice(symbol.name());
                stream.push(">", colors::BLUE);
            }
            Self::SectionStart { section } => {
                stream.push("section started", colors::WHITE);
                stream.push_owned(format!(" {} ", section.name), colors::BLUE);
                stream.push("{", colors::GRAY60);
                stream.push_owned(format!("{:?}", section.kind), colors::MAGENTA);
                stream.push("} ", colors::GRAY60);
                stream.push_owned(format!("{:x}", section.start), colors::GREEN);
                stream.push("-", colors::GRAY60);
                stream.push_owned(format!("{:x}", section.end), colors::GREEN);
            }
            Self::SectionEnd { section } => {
                stream.push("section ended", colors::WHITE);
                stream.push_owned(format!(" {} ", section.name), colors::BLUE);
                stream.push("{", colors::GRAY60);
                stream.push_owned(format!("{:?}", section.kind), colors::MAGENTA);
                stream.push("} ", colors::GRAY60);
                stream.push_owned(format!("{:x}", section.start), colors::GREEN);
                stream.push("-", colors::GRAY60);
                stream.push_owned(format!("{:x}", section.end), colors::GREEN);
            }
            Self::Instruction { addr, inst, bytes } => {
                stream.push_owned(format!("{addr:0>10X}  "), colors::GRAY40);
                stream.push_owned(bytes.clone(), colors::GREEN);
                stream.inner.extend_from_slice(&inst);
            }
            Self::Error { addr, err, bytes } => {
                stream.push_owned(format!("{addr:0>10X}  "), colors::GRAY40);
                stream.push_owned(bytes.clone(), colors::GREEN);
                stream.push("<", colors::GRAY40);
                stream.push_owned(format!("{err:?}"), colors::RED);
                stream.push(">", colors::GRAY40);
            }
            Self::Bytes { addr, bytes } => {
                let mut off = 0;
                // Never print more than 100 lines, this is a little scuffed.
                for chunk in bytes.chunks(32).take(100) {
                    stream.push_owned(format!("{:0>10X}  ", addr + off), colors::GRAY40);
                    let s = processor_shared::encode_hex_bytes_truncated(chunk, chunk.len(), false);
                    stream.push_owned(s, colors::GREEN);
                    stream.push("\n", colors::WHITE);
                    off += chunk.len();
                }
                // Pop last newline
                stream.inner.pop();
            }
        }
    }
}

impl Processor {
    fn bytes_by_addr(&self, addr: usize, len: usize) -> &[u8] {
        let section = self.sections().find(|sec| sec.contains(addr)).expect("Invalid address.");
        section.bytes_by_addr(addr, len)
    }

    fn parse_data_or_code(&self, addr: usize) -> Option<Block> {
        if let Some(inst) = self.instruction_by_addr(addr) {
            let width = self.instruction_width(&inst);
            let inst = self.instruction_tokens(&inst, &self.index);
            let bytes = self.bytes_by_addr(addr, width);
            let bytes =
                encode_hex_bytes_truncated(&bytes, self.max_instruction_width * 3 + 1, true);
            return Some(Block::Instruction { addr, inst, bytes });
        }

        if let Some(err) = self.error_by_addr(addr) {
            let bytes = self.bytes_by_addr(addr, err.size());
            let bytes =
                encode_hex_bytes_truncated(&bytes, self.max_instruction_width * 3 + 1, true);
            return Some(Block::Error {
                addr,
                err: err.kind,
                bytes,
            });
        }

        let section = self.sections().find(|sec| sec.contains(addr)).expect("Invalid address.");
        let mut bytes_len = 0;
        while self.error_by_addr(addr).is_none()
            && self.instruction_by_addr(addr).is_none()
            && self.index.get_func_by_addr(addr).is_none()
            && addr < section.end
        {
            bytes_len += 1;
        }

        if bytes_len > 0 {
            let bytes = self.bytes_by_addr(addr, bytes_len).to_vec();
            return Some(Block::Bytes { addr, bytes });
        }

        None
    }

    /// Pars blocks given an address boundary.
    pub fn parse_blocks(&self, addr: usize) -> Vec<Block> {
        let mut blocks = Vec::new();

        let section_start = self.sections().find(|sec| sec.start == addr);
        let section_end = self.sections().find(|sec| sec.end == addr);

        match (section_start, section_end) {
            (Some(start), Some(end)) => {
                blocks.push(Block::SectionEnd {
                    section: end.clone(),
                });
                blocks.push(Block::SectionStart {
                    section: start.clone(),
                });
            }
            (Some(section), None) => {
                blocks.push(Block::SectionStart {
                    section: section.clone(),
                });
            }
            (None, Some(section)) => {
                blocks.push(Block::SectionEnd {
                    section: section.clone(),
                });
            }
            (None, None) => {}
        }

        if let Some(real_block) = self.parse_data_or_code(addr) {
            if let Some(symbol) = self.index.get_func_by_addr(addr) {
                blocks.push(Block::Label { symbol });
            }

            blocks.push(real_block);
        }

        blocks
    }

    /// Only need to compute the start's of blocks.
    pub fn compute_block_boundaries(&self) -> Vec<usize> {
        let mut boundaries = Vec::new();
        std::thread::scope(|s| {
            let threads: Vec<_> = self
                .sections()
                .map(|section| s.spawn(|| self.compute_section_boundaries(section)))
                .collect();

            for thread in threads {
                boundaries.extend(thread.join().unwrap());
            }
        });

        boundaries.dedup();
        boundaries.sort_unstable();
        boundaries
    }

    fn compute_section_boundaries(&self, section: &Section) -> Vec<usize> {
        let mut boundaries = Vec::new();
        let mut addr = section.addr;

        boundaries.push(section.start);

        loop {
            if addr == section.end {
                break;
            }

            if self.index.get_func_by_addr(addr).is_some() {
                boundaries.push(addr);
            }

            if let Some(inst) = self.instruction_by_addr(addr) {
                boundaries.push(addr);
                addr += self.instruction_width(inst);
                continue;
            }

            if let Some(err) = self.error_by_addr(addr) {
                boundaries.push(addr);
                addr += err.size();
                continue;
            }

            let mut bytes_len = 0;
            loop {
                if addr >= section.end {
                    break;
                }

                if self.instruction_by_addr(addr).is_some() {
                    break;
                }

                if self.error_by_addr(addr).is_some() {
                    break;
                }

                // We found some labelled bytes, so those would have to be in a different block.
                if bytes_len >= 1 && self.index.get_func_by_addr(addr).is_some() {
                    break;
                }

                bytes_len += 1;
                addr += 1;
            }

            if bytes_len > 0 {
                boundaries.push(addr);
            }
        }

        boundaries.push(section.end);
        boundaries
    }
}
