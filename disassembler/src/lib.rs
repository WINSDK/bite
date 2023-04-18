//! Consumes decoder crates and provides an interface to interact with the decoders.

use object::Architecture;

use decoder::Streamable;
use decoder::DecodableInstruction;

#[derive(Debug)]
pub enum Error {
    /// Either the target architecture isn't supported yet or won't be.
    UnsupportedArchitecture,

    /// Riscv specific instruction decoding error.
    Riscv(riscv::Error),

    /// MIPS-V specific instruction decoding error.
    Mips(mips::Error),
}

pub struct Line {
    pub section_base: usize,
    pub offset: usize,
    pub stream: decoder::TokenStream,
    pub address: String,
}

impl ToString for Line {
    fn to_string(&self) -> String {
        let mut fmt = String::with_capacity(30);
        let tokens = self.stream.tokens();
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
}

impl<'data> InstructionStream<'data> {
    pub fn new(bytes: &'data [u8], arch: Architecture, section_base: usize) -> Result<Self, Error> {
        let inner = match arch {
            Architecture::Mips => {
                InternalInstructionStream::Mips(mips::Stream { bytes, offset: 0 })
            }
            Architecture::Mips64 => {
                InternalInstructionStream::Mips(mips::Stream { bytes, offset: 0 })
            }
            Architecture::Riscv32 => InternalInstructionStream::Riscv(riscv::Stream {
                bytes,
                offset: 0,
                width: 4,
                is_64: false,
                section_base,
            }),
            Architecture::Riscv64 => InternalInstructionStream::Riscv(riscv::Stream {
                bytes,
                offset: 0,
                width: 4,
                is_64: true,
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
        let mut section_base = 0;
        let mut offset = 0;

        let tokens = match self.inner {
            InternalInstructionStream::Riscv(ref mut stream) => {
                section_base = stream.section_base;
                offset += stream.offset;
                offset += offset.saturating_sub(stream.width);

                match stream.next()? {
                    Ok(inst) => Ok(inst.tokenize()),
                    Err(err) => Err(format!("{err:?}")),
                }
            }
            InternalInstructionStream::Mips(ref mut stream) => {
                offset += stream.offset;
                offset += offset.saturating_sub(4);

                match stream.next()? {
                    Ok(inst) => Ok(inst.tokenize()),
                    Err(err) => Err(format!("{err:?}")),
                }
            }
        };

        let tokens = tokens.unwrap_or_else(|err| {
            let mut tokens = [tokenizing::EMPTY_TOKEN; 5];

            tokens[0] = tokenizing::Token {
                text: std::borrow::Cow::Owned(err),
                color: tokenizing::colors::RED,
            };

            decoder::TokenStream::new(tokens, 1)
        });

        Some(Line {
            section_base,
            offset,
            stream: tokens,
            address: String::new(),
        })
    }
}
