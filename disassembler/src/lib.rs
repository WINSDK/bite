//! Consumes decoder crates and provides an interface to interact with the decoders.

use object::Architecture;

use decoder::Streamable;
use decoder::ToTokens;

#[derive(Debug)]
pub enum Error {
    /// Either the target architecture isn't supported yet or won't be.
    UnsupportedArchitecture,
}

pub struct Line {
    pub section_base: usize,
    pub offset: usize,
    pub tokens: decoder::TokenStream,
    pub address: String,
    pub bytes: String,
}

impl ToString for Line {
    fn to_string(&self) -> String {
        let mut fmt = String::with_capacity(30);
        let tokens = &self.tokens;
        let operands = &tokens[1..];

        fmt += &tokens[0].text;

        if operands.is_empty() {
            return fmt;
        }

        fmt += " ";

        if operands.len() > 1 {
            for operand in &operands[..operands.len() - 1] {
                fmt += &operand.text;
                fmt += ", ";
            }
        }

        fmt += &operands[operands.len() - 1].text;
        fmt
    }
}

pub struct InstructionStream<'data> {
    inner: InternalInstructionStream<'data>,
}

enum InternalInstructionStream<'data> {
    Riscv(riscv::Stream<'data>),
    Mips(mips::Stream<'data>),
    X86_64(x86_64::Stream<'data>),
}

impl<'data> InstructionStream<'data> {
    pub fn new(bytes: &'data [u8], arch: Architecture, section_base: usize) -> Result<Self, Error> {
        let inner = match arch {
            Architecture::Mips | Architecture::Mips64 => {
                InternalInstructionStream::Mips(mips::Stream {
                    bytes,
                    offset: 0,
                    width: 0,
                    section_base,
                })
            }
            Architecture::Riscv32 => InternalInstructionStream::Riscv(riscv::Stream {
                bytes,
                offset: 0,
                width: 0,
                is_64: false,
                section_base,
            }),
            Architecture::Riscv64 => InternalInstructionStream::Riscv(riscv::Stream {
                bytes,
                offset: 0,
                width: 0,
                is_64: true,
                section_base,
            }),
            Architecture::X86_64_X32 => InternalInstructionStream::X86_64(x86_64::Stream {
                reader: x86_64::Reader::new(bytes),
                bytes,
                decoder: x86_64::DecoderKind::x86(),
                offset: 0,
                width: 0,
                section_base,
            }),
            Architecture::X86_64 => InternalInstructionStream::X86_64(x86_64::Stream {
                reader: x86_64::Reader::new(bytes),
                bytes,
                decoder: x86_64::DecoderKind::x64(),
                offset: 0,
                width: 0,
                section_base,
            }),
            _ => return Err(Error::UnsupportedArchitecture),
        };

        Ok(Self { inner })
    }
}

impl Iterator for InstructionStream<'_> {
    type Item = Line;

    fn next(&mut self) -> Option<Self::Item> {
        let mut tokens = decoder::TokenStream::new();

        match self.inner {
            InternalInstructionStream::Riscv(ref mut stream) => {
                let inst = stream.next()?;
                let width = stream.width;
                let offset = stream.offset - width;

                match inst {
                    Ok(inst) => inst.tokenize(&mut tokens),
                    Err(err) => tokens.push_owned(format!("{err:?}"), tokenizing::colors::RED),
                }

                return Some(Line {
                    section_base: stream.section_base,
                    offset,
                    tokens,
                    address: format!("0x{:0>10X}  ", stream.section_base + offset),
                    bytes: decoder::encode_hex_bytes_truncated(
                        &stream.bytes[offset..std::cmp::min(offset + width, stream.bytes.len())],
                        13, // 4 bytes shown (4 bytes * (2 hex + 1 space) + 1 pad)
                    ),
                })
            }
            InternalInstructionStream::Mips(ref mut stream) => {
                let inst = stream.next()?;
                let width = stream.width;
                let offset = stream.offset - width;

                match inst {
                    Ok(inst) => inst.tokenize(&mut tokens),
                    Err(err) => tokens.push_owned(format!("{err:?}"), tokenizing::colors::RED),
                }

                Some(Line {
                    section_base: stream.section_base,
                    offset,
                    tokens,
                    address: format!("0x{:0>10X}  ", stream.section_base + offset),
                    bytes: decoder::encode_hex_bytes_truncated(
                        &stream.bytes[offset..std::cmp::min(offset + width, stream.bytes.len())],
                        13, // 4 bytes shown (4 bytes * (2 hex + 1 space) + 1 pad)
                    ),
                })
            }
            InternalInstructionStream::X86_64(ref mut stream) => {
                let inst = stream.next()?;
                let width = stream.width;
                let offset = stream.offset - width;

                match inst {
                    Ok(inst) => inst.tokenize(&mut tokens),
                    Err(err) => tokens.push_owned(format!("{err:?}"), tokenizing::colors::RED),
                }

                // TODO: if byte array overflows, put it on a newline
                Some(Line {
                    section_base: stream.section_base,
                    offset,
                    tokens,
                    address: format!("0x{:0>10X}  ", stream.section_base + offset),
                    bytes: decoder::encode_hex_bytes_truncated(
                        &stream.bytes[offset..std::cmp::min(offset + width, stream.bytes.len())],
                        25, // 8 bytes shown (8 bytes * (2 hex + 1 space) + 1 pad)
                    ),
                })
            }
        }
    }
}
