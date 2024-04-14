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
use object::Endian;
use processor_shared::{encode_hex_bytes_truncated, Section, SectionKind};
use tokenizing::{colors, Token, TokenStream};
use crate::Processor;

const BYTES_BLOCK_SIZE: usize = 256;

#[derive(Debug)]
pub enum BlockContent {
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
        inst: Vec<Token>,
        bytes: String,
    },
    Error {
        err: decoder::ErrorKind,
        bytes: String,
    },
    CString {
        bytes: Vec<u8>,
    },
    Got {
        size: usize,
        symbol: Arc<Symbol>,
    },
    Pointer {
        size: usize,
        value: u64,
        symbol: Option<Arc<Symbol>>,
    },
    Bytes {
        bytes: Vec<u8>,
    },
}

#[derive(Debug)]
pub struct Block {
    pub addr: usize,
    pub content: BlockContent,
}

impl Block {
    /// Length of block when tokenized.
    pub fn len(&self) -> usize {
        match &self.content {
            BlockContent::SectionStart { .. } => 2,
            BlockContent::SectionEnd { .. } => 2,
            BlockContent::Label { .. } => 2,
            BlockContent::Instruction { .. } => 1,
            BlockContent::Error { .. } => 1,
            BlockContent::CString { bytes } => bytes.len() + 1,
            BlockContent::Pointer { size, .. } => *size,
            BlockContent::Got { size, .. } => *size,
            BlockContent::Bytes { bytes } => (bytes.len() / 32) + 1,
        }
    }

    pub fn tokenize(&self, stream: &mut TokenStream) {
        match &self.content {
            BlockContent::Label { symbol } => {
                stream.push("\n<", colors::BLUE);
                stream.inner.extend_from_slice(symbol.name());
                stream.push(">", colors::BLUE);
            }
            BlockContent::SectionStart { section } => {
                stream.push("section started", colors::WHITE);
                stream.push_owned(format!(" {} ", section.name), colors::BLUE);
                stream.push("{", colors::GRAY60);
                stream.push_owned(format!("{:?}", section.kind), colors::MAGENTA);
                stream.push("} ", colors::GRAY60);
                stream.push_owned(format!("{:x}", section.start), colors::GREEN);
                stream.push("-", colors::GRAY60);
                stream.push_owned(format!("{:x}", section.end), colors::GREEN);
            }
            BlockContent::SectionEnd { section } => {
                stream.push("section ended", colors::WHITE);
                stream.push_owned(format!(" {} ", section.name), colors::BLUE);
                stream.push("{", colors::GRAY60);
                stream.push_owned(format!("{:?}", section.kind), colors::MAGENTA);
                stream.push("} ", colors::GRAY60);
                stream.push_owned(format!("{:x}", section.start), colors::GREEN);
                stream.push("-", colors::GRAY60);
                stream.push_owned(format!("{:x}", section.end), colors::GREEN);
            }
            BlockContent::Instruction { inst, bytes } => {
                stream.push_owned(format!("{:0>10X}  ", self.addr), colors::GRAY40);
                stream.push_owned(bytes.clone(), colors::GREEN);
                stream.inner.extend_from_slice(&inst);
            }
            BlockContent::Error { err, bytes } => {
                stream.push_owned(format!("{:0>10X}  ", self.addr), colors::GRAY40);
                stream.push_owned(bytes.clone(), colors::GREEN);
                stream.push("<", colors::GRAY40);
                stream.push_owned(format!("{err:?}"), colors::RED);
                stream.push(">", colors::GRAY40);
            }
            BlockContent::CString { bytes } => {
                stream.push_owned(format!("{:0>10X}  ", self.addr), colors::GRAY40);
                let lossy_string = String::from_utf8_lossy(&bytes);
                let escaped = format!("\"{}\"", lossy_string.escape_debug());
                stream.push_owned(escaped, colors::ORANGE);
            }
            BlockContent::Got { symbol, .. } => {
                stream.push_owned(format!("{:0>10X}  ", self.addr), colors::GRAY40);
                stream.push("<", colors::BLUE);
                stream.inner.extend_from_slice(symbol.name());
                stream.push(">", colors::BLUE);
            }
            BlockContent::Pointer { value, symbol, .. } => {
                stream.push_owned(format!("{:0>10X}  ", self.addr), colors::GRAY40);
                stream.push_owned(format!("{:#x}", value), colors::GREEN);
                if let Some(symbol) = symbol {
                    stream.push(" <", colors::BLUE);
                    stream.inner.extend_from_slice(symbol.name());
                    stream.push(">", colors::BLUE);
                }
            }
            BlockContent::Bytes { bytes } => {
                let mut off = 0;
                for chunk in bytes.chunks(32) {
                    stream.push_owned(format!("{:0>10X}  ", self.addr + off), colors::GRAY40);
                    let s = processor_shared::encode_hex_bytes_truncated(chunk, usize::MAX, false);
                    stream.push_owned(s, colors::GREEN);
                    stream.push("\n", colors::WHITE);
                    off += chunk.len();
                }
                // Pop last newline.
                stream.inner.pop();
            }
        }
    }
}

impl Processor {
    /// Pars blocks given an address boundary.
    pub fn parse_blocks(&self, addr: usize) -> Vec<Block> {
        let mut blocks = Vec::new();

        let section_start = self.sections().find(|sec| sec.start == addr);
        let section_end = self.sections().find(|sec| sec.end == addr);

        match (section_start, section_end) {
            (Some(start), Some(end)) => {
                blocks.push(Block {
                    addr,
                    content: BlockContent::SectionEnd {
                        section: end.clone(),
                    },
                });
                blocks.push(Block {
                    addr,
                    content: BlockContent::SectionStart {
                        section: start.clone(),
                    },
                });

                // Empty sections end at the same address but won't be accounted for otherwise.
                if start.bytes().is_empty() {
                    blocks.push(Block {
                        addr,
                        content: BlockContent::SectionEnd {
                            section: start.clone(),
                        },
                    });
                }
            }
            (Some(section), None) => blocks.push(Block {
                addr,
                content: BlockContent::SectionStart {
                    section: section.clone(),
                },
            }),
            (None, Some(section)) => blocks.push(Block {
                addr,
                content: BlockContent::SectionEnd {
                    section: section.clone(),
                },
            }),
            (None, None) => {}
        }

        let section = self.section_by_addr(addr).unwrap();

        if addr == section.end {
            return blocks;
        }

        match section.kind {
            SectionKind::Code => self.parse_code(addr, section, &mut blocks),
            SectionKind::CString => self.parse_cstring(addr, section, &mut blocks),
            SectionKind::Ptr32 => self.parse_pointer(addr, section, 4, &mut blocks),
            SectionKind::Ptr64 => self.parse_pointer(addr, section, 8, &mut blocks),
            SectionKind::Got32 => self.parse_got(addr, 4, &mut blocks),
            SectionKind::Got64 => self.parse_got(addr, 4, &mut blocks),
            // For any other section kinds just assume they're made of bytes.
            // As a note, we calculate the byte boundaries in blocks of [`BYTES_BLOCK_SIZE`],
            // so this block can be up to [`BYTES_BLOCK_SIZE`] bytes.
            _ => {
                let bytes = section.bytes_by_addr(addr, BYTES_BLOCK_SIZE).to_vec();
                blocks.push(Block {
                    addr,
                    content: BlockContent::Bytes { bytes }
                });
            }
        }

        blocks
    }

    fn parse_got(&self, addr: usize, size: usize ,blocks: &mut Vec<Block>) {
        let symbol = self.index.get_func_by_addr(addr).unwrap_or_default();
        blocks.push(Block {
            addr,
            content: BlockContent::Got { size, symbol }
        });
    }

    fn parse_pointer(&self, addr: usize, section: &Section, size: usize ,blocks: &mut Vec<Block>) {
        let bytes = section.bytes_by_addr(addr, size);
        let value = if size == 4 {
            self.endianness.read_u32_bytes(bytes.try_into().unwrap()) as u64
        } else {
            self.endianness.read_u64_bytes(bytes.try_into().unwrap())
        };

        let symbol = self.index.get_func_by_addr(addr);

        blocks.push(Block {
            addr,
            content: BlockContent::Pointer { size, value, symbol }
        });
    }

    fn parse_cstring(&self, addr: usize, section: &Section, blocks: &mut Vec<Block>) {
        let bytes = section.bytes_by_addr(addr, usize::MAX);
        let end = bytes.iter().position(|&b| b == b'\0').unwrap_or(bytes.len());
        blocks.push(Block {
            addr,
            content: BlockContent::CString { bytes: bytes[..end].to_vec() }
        });
    }

    fn parse_code(&self, addr: usize, section: &Section, blocks: &mut Vec<Block>) {
        let opt_inst = self.instruction_by_addr(addr);
        let opt_err = self.error_by_addr(addr);

        if opt_inst.is_some() || opt_err.is_some() {
            if let Some(symbol) = self.index.get_func_by_addr(addr) {
                blocks.push(Block {
                    addr,
                    content: BlockContent::Label { symbol },
                })
            }
        }

        if let Some(inst) = opt_inst {
            let width = self.instruction_width(&inst);
            let inst = self.instruction_tokens(&inst, &self.index);
            let bytes = section.bytes_by_addr(addr, width);
            let bytes =
                encode_hex_bytes_truncated(&bytes, self.max_instruction_width * 3 + 1, true);
            blocks.push(Block {
                addr,
                content: BlockContent::Instruction { inst, bytes },
            });
            return;
        }

        if let Some(err) = opt_err {
            let bytes = section.bytes_by_addr(addr, err.size());
            let bytes =
                encode_hex_bytes_truncated(&bytes, self.max_instruction_width * 3 + 1, true);

            blocks.push(Block {
                addr,
                content: BlockContent::Error {
                    err: err.kind,
                    bytes,
                },
            });
            return;
        }

        // If we don't find any code, find bytes at the boundary.
        self.parse_bytes(addr, section, blocks);
    }

    fn parse_bytes(&self, addr: usize, section: &Section, blocks: &mut Vec<Block>) {
        let mut baddr = addr;
        loop {
            if baddr == section.end {
                break;
            }

            if self.instruction_by_addr(baddr).is_some() {
                break;
            }

            if self.error_by_addr(baddr).is_some() {
                break;
            }

            if addr != baddr && self.index.get_func_by_addr(baddr).is_some() {
                break;
            }

            baddr += 1;
        }

        let bytes_len = baddr - addr;
        if bytes_len > 0 {
            let bytes = section.bytes_by_addr(addr, bytes_len).to_vec();
            blocks.push(Block {
                addr,
                content: BlockContent::Bytes { bytes },
            });
        }
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

        boundaries.sort_unstable();
        boundaries.dedup();
        boundaries
    }

    fn compute_section_boundaries(&self, section: &Section) -> Vec<usize> {
        let mut boundaries = Vec::new();
        boundaries.push(section.start);
        match section.kind {
            SectionKind::Code => self.compute_code_boundaries(section, &mut boundaries),
            SectionKind::CString => self.compute_cstring_boundaries(section, &mut boundaries),
            SectionKind::Ptr32 | SectionKind::Got32 => {
                let mut addr = section.addr;
                while addr < section.end {
                    boundaries.push(addr);
                    addr += 4;
                }
            },
            SectionKind::Ptr64 | SectionKind::Got64 => {
                let mut addr = section.addr;
                while addr < section.end {
                    boundaries.push(addr);
                    addr += 8;
                }
            },
            // For any kind of DWARF debugging info, don't show any of the section.
            SectionKind::Debug => {},
            // For any other section kinds just assume they evenly
            // split in blocks of [`BYTES_BLOCK_SIZE`].
            _ => {
                let mut addr = section.addr;
                while addr < section.end {
                    boundaries.push(addr);
                    addr += BYTES_BLOCK_SIZE;
                }
            },
        }
        boundaries.push(section.end);
        boundaries
    }

    fn compute_code_boundaries(&self, section: &Section, boundaries: &mut Vec<usize>) {
        let mut addr = section.addr;

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

            let mut baddr = addr;
            loop {
                if baddr == section.end {
                    break;
                }

                if self.instruction_by_addr(baddr).is_some() {
                    break;
                }

                if self.error_by_addr(baddr).is_some() {
                    break;
                }

                // We found some labelled bytes, so those would have to be in a different block.
                if addr != baddr && self.index.get_func_by_addr(baddr).is_some() {
                    break;
                }

                baddr += 1;
            }

            let bytes_len = baddr - addr;
            if bytes_len > 0 {
                boundaries.push(addr);
                addr = baddr;
            }
        }
    }

    fn compute_cstring_boundaries(&self, section: &Section, boundaries: &mut Vec<usize>) {
        let mut start_off = 0;
        for (idx, &byte) in section.bytes().iter().enumerate() {
            if byte == b'\0' {
                // Check if there isn't two consecutive null bytes.
                if idx != start_off {
                    boundaries.push(section.start + start_off);
                }
                // Update the start to the byte after the null byte.
                start_off = idx + 1;
            }
        }
    }
}
