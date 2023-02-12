use object::Architecture;
use std::borrow::Cow;
use std::collections::BTreeMap;

mod arm;
mod mips;
mod riscv;

// mod lookup;
// #[allow(dead_code, unused_variables, unused_assignments)]
// mod x86_64;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// Instruction stream is empty.
    NoBytesLeft,

    InvalidInstruction,
    UnknownRegister,
    UnknownOpcode,
}

trait DecodableInstruction {
    fn operands(&self) -> &[std::borrow::Cow<'static, str>];
    fn decode(&self) -> String;

    fn psuedo_decode(&mut self) -> String {
        self.decode()
    }
}

trait Streamable {
    type Item: DecodableInstruction;
    type Error;

    fn next(&mut self) -> Result<Self::Item, Error>;
    fn format(&self, next: Result<Self::Item, Error>) -> Option<String>;
}

pub struct InstructionStream<'data> {
    inner: InternalInstructionStream<'data>,
    symbols: BTreeMap<usize, Cow<'static, str>>,
}

enum InternalInstructionStream<'data> {
    Riscv(riscv::Stream<'data>),
    Mips(mips::Stream<'data>),
}

impl<'data> InstructionStream<'data> {
    pub fn new(
        bytes: &'data [u8],
        arch: Architecture,
        section_base: usize,
        symbols: BTreeMap<usize, Cow<'static, str>>,
    ) -> Self {
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
            _ => todo!(),
        };

        Self { inner, symbols }
    }
}

impl Iterator for InstructionStream<'_> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let mut addr = 0;

        let fmt = match self.inner {
            InternalInstructionStream::Riscv(ref mut stream) => {
                let next = stream.next();
                addr += stream.section_base;
                addr += stream.offset;
                stream.format(next)
            }
            InternalInstructionStream::Mips(ref mut stream) => {
                let next = stream.next();
                addr += stream.offset;
                stream.format(next)
            }
        };

        fmt.map(|fmt| {
            if let Some(label) = self.symbols.get(&addr) {
                return format!("\n{addr:012} <{label}>:\n\t{fmt}");
            }

            fmt
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

    pub fn offset(&mut self, num_bytes: isize) {
        self.pos = (self.pos as isize + num_bytes) as usize;
    }

    pub fn take(&mut self, value: T) -> bool
    where
        T: PartialEq,
    {
        let eq = self.buf.get(self.pos) == Some(&value);
        self.pos += eq as usize;
        eq
    }

    pub fn take_buf(&mut self, values: &[T]) -> bool
    where
        T: PartialEq,
    {
        let eq = self.buf.get(self.pos..self.pos + values.len()) == Some(values);
        self.buf = &self.buf[(values.len()) * eq as usize..];
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

type Operand = std::borrow::Cow<'static, str>;

const EMPTY_OPERAND: Operand = std::borrow::Cow::Borrowed("");
