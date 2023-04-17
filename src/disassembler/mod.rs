mod arm;
mod mips;
mod riscv;

mod lookup;
#[allow(dead_code, unused_variables, unused_assignments)]
mod x86_64;

use crate::colors::{LineKind, Token, EMPTY_TOKEN};
use crate::symbols::Index;
use crate::warning;
use object::Architecture;
use object::{Object, ObjectSection, SectionKind};

use std::borrow::Cow;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
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
    /// Address of last instruction in the object.
    pub address_space: AtomicUsize,

    /// Lines of disassembly e.i. labels and instructions.
    lines: Mutex<Vec<LineKind>>,

    /// Symbol lookup by RVA.
    pub symbols: Mutex<Index>,
}

impl Dissasembly {
    pub fn new() -> Self {
        Self {
            address_space: AtomicUsize::new(0),
            lines: Mutex::new(Vec::new()),
            symbols: Mutex::new(Index::new()),
        }
    }

    pub async fn load<P>(self: Arc<Self>, path: P, show_donut: Arc<AtomicBool>)
    where
        P: AsRef<std::path::Path> + Sync + Send + 'static,
    {
        let now = tokio::time::Instant::now();
        let path_fmt = format!("{:?}", path.as_ref());

        let task = tokio::spawn(async move {
            show_donut.store(true, Ordering::Relaxed);

            let binary = tokio::fs::read(&path)
                .await
                .map_err(|_| "Unexpected read of binary failed.")?;

            // SAFETY: tokio::spawn's in this scope require a &'static
            let binary: &'static [u8] = unsafe { std::mem::transmute(&binary[..]) };

            let obj = object::File::parse(binary).map_err(|_| "Not a valid object.")?;
            let arch = obj.architecture();

            let section = obj
                .sections()
                .filter(|s| s.kind() == SectionKind::Text)
                .find(|t| t.name() == Ok(".text"))
                .ok_or("Failed to find `.text` section.")?;

            self.address_space.store(
                (obj.relative_address_base() + section.size()) as usize,
                Ordering::Relaxed,
            );

            // SAFETY: tokio::spawn's in this scope require a &'static
            let section: object::Section<'static, '_> = unsafe { std::mem::transmute(section) };

            let raw = section
                .uncompressed_data()
                .map_err(|_| "Failed to decompress .text section.")?;

            let symbols = Index::parse(&obj)
                .await
                .map_err(|_| "Failed to parse symbols table.")?;

            // TODO: optimize for lazy chunk loading
            let base_offset = section.address() as usize;
            let stream = InstructionStream::new(&raw[..], arch, base_offset)
                .map_err(|_| "Failed to disassemble: UnsupportedArchitecture.")?;

            let mut lines = Vec::with_capacity(1024);

            for line in stream {
                if let Some(label) = symbols.get_by_line(&line) {
                    lines.push(LineKind::Newline);
                    lines.push(LineKind::Label(label));
                }

                lines.push(LineKind::Instruction(line));
            }

            *self.symbols.lock().unwrap() = symbols;
            *self.lines.lock().unwrap() = lines;

            Ok::<(), &str>(())
        });

        match task.await.unwrap() {
            Err(err) => warning!("{err:?}"),
            _ => println!("took {:#?} to parse {}", now.elapsed(), path_fmt),
        };
    }

    pub fn lines(&self) -> Option<std::sync::MutexGuard<Vec<LineKind>>> {
        if let Ok(lines) = self.lines.try_lock() {
            if !lines.is_empty() {
                return Some(lines);
            }
        }

        None
    }

    pub fn clear(&self) {
        self.lines.lock().unwrap().clear();
        self.symbols.lock().unwrap().clear();
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

pub struct TokenStream {
    inner: [Token; 5],
    token_count: usize,
}

impl TokenStream {
    pub fn tokens(&self) -> &[Token] {
        &self.inner[..self.token_count]
    }
}

pub struct Line {
    pub section_base: usize,
    pub offset: usize,
    pub stream: TokenStream,
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
            _ => {
                warning!("{arch:?}");
                return Err(Error::UnsupportedArchitecture);
            }
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

                tokens[0] = Token {
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
            stream: tokens,
            address: String::new(),
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

pub fn encode_hex(mut imm: i64) -> String {
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

    let num_len = (imm).ilog(16) as usize + 1;
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

pub fn encode_hex_padded(mut imm: i64, size: usize) -> String {
    use crate::push_unsafe;

    let mut hex = String::with_capacity(20); // max length of an i64
    let raw = unsafe { hex.as_mut_vec() };
    let mut off = 0;
    let mut is_neg = 0;

    if imm < 0 {
        push_unsafe!(raw, off, b'-');
        imm = -imm;
        is_neg = 1;
    }

    let num_len = (imm + 1).ilog(16) as usize;
    let leading_zeros = (16 - num_len) * 4;
    let mut imm = reverse_hex_nuggets(imm as usize);

    for _ in 0..size.saturating_sub(num_len + is_neg) {
        push_unsafe!(raw, off, b'0');
    }

    imm >>= leading_zeros;
    for _ in 0..num_len {
        push_unsafe!(raw, off, ENCODED_NUGGETS[imm & 0b1111]);
        imm >>= 4;
    }

    unsafe { raw.set_len(off) }
    hex
}

const EMPTY_OPERAND: std::borrow::Cow<'static, str> = std::borrow::Cow::Borrowed("");
