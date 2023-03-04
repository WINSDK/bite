mod arm;
mod mips;
mod riscv;

mod lookup;
#[allow(dead_code, unused_variables, unused_assignments)]
mod x86_64;

use crate::symbols::Index;
use object::Architecture;
use object::{Object, ObjectSection, SectionKind};

use std::borrow::Cow;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub enum Error {
    /// Either the target architecture isn't supported yet or won't be.
    UnsupportedArchitecture,

    /// Instruction stream is empty.
    NoBytesLeft,

    /// Register in instruction is impossible/unknown.
    UnknownRegister,

    /// Opcode in instruction is impossible/unknown.
    UnknownOpcode,

    /// Instruction has a valid register and opcode yet is still invalid.
    InvalidInstruction,
}

pub struct Dissasembly {
    pub lines: Mutex<Vec<Line>>,
    pub symbols: Mutex<Index>,
}

impl Dissasembly {
    pub fn new() -> Self {
        Self {
            lines: Mutex::new(Vec::new()),
            symbols: Mutex::new(Index::new()),
        }
    }

    pub fn load<P>(self: Arc<Self>, path: P, show_donut: Arc<AtomicBool>)
    where
        P: AsRef<std::path::Path> + Sync + Send + 'static,
    {
        show_donut.store(true, Ordering::Relaxed);

        let task = tokio::spawn(async move {
            let now = tokio::time::Instant::now();
            let binary = tokio::fs::read(&path)
                .await
                .map_err(|_| "Unexpected read of binary failed.")?;

            let obj = object::File::parse(&*binary).map_err(|_| "Not a valid object.")?;

            *self.symbols.lock().unwrap() = Index::parse(&obj)
                .await
                .map_err(|_| "Failed to parse symbols table.")?;

            let section = obj
                .sections()
                .filter(|s| s.kind() == SectionKind::Text)
                .find(|t| t.name() == Ok(".text"))
                .ok_or("Failed to find `.text` section.")?;

            let raw = section
                .uncompressed_data()
                .map_err(|_| "Failed to decompress .text section.")?
                .into_owned()
                .leak();

            let base_offset = section.address() as usize;
            let stream = match InstructionStream::new(raw, obj.architecture(), base_offset) {
                Err(..) => return Err("Failed to disassemble: UnsupportedArchitecture."),
                Ok(stream) => stream,
            };

            // TODO: optimize for lazy chunk loading
            let mut lines = self.lines.lock().unwrap();
            for inst in stream {
                lines.push(inst);
            }

            println!("took {:#?} to parse {:?}", now.elapsed(), path.as_ref());

            Ok(())
        });

        tokio::spawn(async move {
            let err = match task.await {
                Ok(Err(err)) => format!("{err:?}"),
                Err(err) => format!("{err:?}"),
                _ => {
                    show_donut.store(false, Ordering::Relaxed);
                    return;
                },
            };

            show_donut.store(false, Ordering::Relaxed);

            if let Some(window) = crate::gui::WINDOW.get() {
                let window = std::sync::Arc::clone(window);

                rfd::AsyncMessageDialog::new()
                    .set_title("Bite failed at runtime")
                    .set_description(&err)
                    .set_parent(&*window)
                    .show()
                    .await;
            }

            eprintln!("{err}");
        });
    }
}

trait DecodableInstruction {
    fn tokenize(self) -> TokenStream;
}

trait Streamable {
    type Item: DecodableInstruction;
    type Error;

    fn next(&mut self) -> Result<Self::Item, Error>;
}

pub struct InstructionToken {
    pub text: Cow<'static, str>,
    pub color: crate::colors::Color,
}

impl InstructionToken {
    pub fn text(&self, scale: f32) -> wgpu_glyph::Text {
        wgpu_glyph::Text::new(&self.text)
            .with_color(self.color)
            .with_scale(scale)
    }
}

pub struct TokenStream {
    inner: [InstructionToken; 5],
    token_count: usize,
}

pub struct Line {
    pub section_base: usize,
    pub offset: usize,
    tokens: TokenStream,
}

impl ToString for Line {
    fn to_string(&self) -> String {
        let mut fmt = String::with_capacity(30);
        let tokens = self.tokens();
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

impl Line {
    pub fn tokens(&self) -> &[InstructionToken] {
        &self.tokens.inner[..self.tokens.token_count]
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

                stream.next().map(DecodableInstruction::tokenize)
            }
            InternalInstructionStream::Mips(ref mut stream) => {
                offset += stream.offset;
                offset += offset.saturating_sub(4);

                stream.next().map(DecodableInstruction::tokenize)
            }
        };

        let tokens = match tokens {
            Ok(tokens) => tokens,
            Err(Error::NoBytesLeft) => return None,
            Err(err) => {
                let mut tokens = [EMPTY_TOKEN; 5];

                tokens[0] = InstructionToken {
                    text: Cow::Owned(format!("<{err:?}>")),
                    color: crate::colors::RED,
                };

                TokenStream {
                    inner: tokens,
                    token_count: 1,
                }
            }
        };

        Some(Line {
            section_base,
            offset,
            tokens,
        })
    }
}

pub struct Reader<'a, T> {
    pub buf: &'a [T],
    pub pos: usize,
}

impl<'a, T> Reader<'a, T> {
    pub fn new(buf: &'a [T]) -> Self {
        Self { buf, pos: 0 }
    }

    pub fn inner(&self) -> &'a [T] {
        &self.buf[self.pos..]
    }

    pub fn take(&mut self, value: T) -> bool
    where
        T: PartialEq,
    {
        let eq = self.buf.get(self.pos) == Some(&value);
        self.pos += eq as usize;
        eq
    }

    pub fn seek(&self) -> Option<T>
    where
        T: Copy,
    {
        self.buf.get(self.pos).copied()
    }

    pub fn consume(&mut self) -> Option<T>
    where
        T: Copy,
    {
        self.buf.get(self.pos).map(|&val| {
            self.pos += 1;
            val
        })
    }

    pub fn consume_exact(&mut self, num_bytes: usize) -> Option<&[T]> {
        self.inner().get(..num_bytes)
    }

    /// Returns `None` if either the reader is at the end of a byte stream or the conditional
    /// fails, on success will increment internal position.
    pub fn consume_eq<F: FnOnce(T) -> bool>(&mut self, f: F) -> Option<T>
    where
        T: Copy,
    {
        self.buf.get(self.pos).filter(|&&x| f(x)).map(|&val| {
            self.pos += 1;
            val
        })
    }
}

#[rustfmt::skip]
const ENCODED_NUGGETS: [u8; 16] = [
    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9',
    b'a', b'b', b'c', b'd', b'e', b'f',
];

#[inline(always)]
#[rustfmt::skip]
fn reverse_hex_nuggets(mut imm: usize) -> usize {
    imm = (imm & 0x00000000ffffffff) << 32 | (imm & 0xffffffff00000000) >> 32;
    imm = (imm & 0x0000ffff0000ffff) << 16 | (imm & 0xffff0000ffff0000) >> 16;
    imm = (imm & 0x00ff00ff00ff00ff) << 8  | (imm & 0xff00ff00ff00ff00) >> 8;
    imm = (imm & 0x0f0f0f0f0f0f0f0f) << 4  | (imm & 0xf0f0f0f0f0f0f0f0) >> 4;
    imm
}

fn encode_hex(mut imm: i64) -> String {
    use crate::push_unsafe;

    let mut hex = String::with_capacity(20); // max length of an i64
    let raw = unsafe { hex.as_mut_vec() };
    let mut off = 0;

    if imm < 0 {
        push_unsafe!(raw, off, b'-');
        imm = -imm;
    }

    push_unsafe!(raw, off, b'0');
    push_unsafe!(raw, off, b'x');

    let num_len = imm.checked_ilog(16).unwrap_or(0) as usize + 1;
    let leading_zeros = (16 - num_len) * 4;
    let mut imm = reverse_hex_nuggets(imm as usize);

    imm >>= leading_zeros;
    for _ in 0..num_len {
        push_unsafe!(raw, off, ENCODED_NUGGETS[imm & 0b1111]);
        imm >>= 4;
    }

    unsafe { raw.set_len(off) }
    hex
}

const EMPTY_OPERAND: std::borrow::Cow<'static, str> = std::borrow::Cow::Borrowed("");

const EMPTY_TOKEN: InstructionToken = InstructionToken {
    color: crate::colors::WHITE,
    text: std::borrow::Cow::Borrowed(""),
};
