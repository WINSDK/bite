//! # `yaxpeax-x86`, a decoder for x86-family instruction sets
//!
//! `yaxpeax-x86` provides x86 decoders, for 64-, and 32-bit modes. `yaxpeax-x86` also
//! implements traits defined by `yaxpeax_arch`, making it suitable for interchangeable use with
//! other `yaxpeax`-family instruction decoders.
//!
//! instructions, operands, registers, and generally all decoding structures, are in their mode's
//! respective submodule:
//! * `x86_64`/`amd64` decoding is under [`long_mode`]
//! * `x86_32`/`x86` decoding is under [`protected_mode`]

pub mod long_mode;
pub mod protected_mode;

mod safer_unchecked;
mod tests;

use yaxpeax_arch::Decoder;
use decoder::ToTokens;
use tokenizing::{Colors, ColorScheme};

pub use yaxpeax_arch::U8Reader as Reader;

const MEM_SIZE_STRINGS: [&'static str; 64] = [
    "byte ", "word ", "BUG ", "dword ", "ptr ", "far ", "BUG ", "qword ", "BUG ", "mword ", "BUG ", "BUG ",
    "BUG ", "BUG ", "BUG ", "xmmword ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ",
    "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "ymmword ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ",
    "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ",
    "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "ptr ", "zmmword "
];

struct Number(i32);

impl ToTokens for Number {
    fn tokenize(self, stream: &mut decoder::TokenStream) {
        if self.0 == core::i32::MIN {
            stream.push(" - ", Colors::expr());
            stream.push("0x7fffffff", Colors::immediate());
        } else if self.0 < 0 {
            stream.push(" - ", Colors::expr());
            stream.push_owned(decoder::encode_hex(-self.0 as i64), Colors::immediate());
        } else {
            stream.push(" + ", Colors::expr());
            stream.push_owned(decoder::encode_hex(self.0 as i64), Colors::immediate());
        }
    }
}

pub struct MemoryAccessSize {
    size: u8,
}

impl MemoryAccessSize {
    /// return the number of bytes referenced by this memory access.
    ///
    /// if the number of bytes cannot be confidently known by the instruction in isolation (as is
    /// the case for `xsave`/`xrstor`-style "operate on all processor state" instructions), this
    /// function will return `None`.
    pub fn bytes_size(&self) -> Option<u8> {
        if self.size == 63 {
            None
        } else {
            Some(self.size)
        }
    }

    /// a human-friendly label for the number of bytes this memory access references.
    ///
    /// there are some differences from size names that may be expected elsewhere; `yaxpeax-x86`
    /// prefers to use consistent names for a width even if the way those bytes are used varies.
    ///
    /// the sizes `yaxpeax-x86` knows are as follows:
    /// | size (bytes) | name       |
    /// |--------------|------------|
    /// | 1            | `byte`     |
    /// | 2            | `word`     |
    /// | 4            | `dword`    |
    /// | 6            | `far`      |
    /// | 8            | `qword`    |
    /// | 10           | `mword`    |
    /// | 16           | `xmmword`  |
    /// | 32           | `ymmword`  |
    /// | 64           | `zmmword`  |
    /// | variable     | `ptr`      |
    ///
    /// "mword" refers to an mmx-sized access - 80 bits, or 10 bytes. `mword` is also used for
    /// 64-bit far calls, because they reference a contiguous ten bytes; two bytes of segment
    /// selector and eight bytes of address.
    ///
    /// "variable" accesses access a number of bytes dependent on the physical processor and its
    /// operating mode. this is particularly relevant for `xsave`/`xrstor`-style instructions.
    pub fn size_name(&self) -> &'static str {
        MEM_SIZE_STRINGS[self.size as usize - 1]
    }
}

impl std::fmt::Display for MemoryAccessSize {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.size_name())
    }
}

impl std::fmt::Debug for MemoryAccessSize {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

pub enum DecoderKind {
    X86(protected_mode::InstDecoder),
    X64(long_mode::InstDecoder),
}

impl DecoderKind {
    pub fn x86() -> Self {
        Self::X86(protected_mode::InstDecoder::default())
    }

    pub fn x64() -> Self {
        Self::X64(long_mode::InstDecoder::default())
    }
}

pub enum Instruction {
    X86(protected_mode::Instruction),
    X64(long_mode::Instruction),
}

pub struct Stream<'data> {
    pub reader: yaxpeax_arch::U8Reader<'data>,
    pub bytes: &'data [u8],
    pub decoder: DecoderKind,
    pub offset: usize,
    pub width: usize,
    pub section_base: usize,
}

impl ToTokens for Instruction {
    fn tokenize(self, stream: &mut decoder::TokenStream) {
        match self {
            Self::X86(inst) => inst.tokenize(stream),
            Self::X64(inst) => inst.tokenize(stream),
        };
    }
}

#[derive(Debug)]
pub enum Error {
    InvalidOpcode,
    InvalidOperand,
    InvalidPrefixes,
    TooLong,
}

impl decoder::Streamable for Stream<'_> {
    type Item = Instruction;
    type Error = Error;

    fn next(&mut self) -> Option<Result<Self::Item, Error>> {
        let result = match self.decoder {
            DecoderKind::X86(decoder) => match decoder.decode(&mut self.reader) {
                Err(protected_mode::DecodeError::InvalidOpcode) => Some(Err(Error::InvalidOpcode)),
                Err(protected_mode::DecodeError::InvalidOperand) => Some(Err(Error::InvalidOperand)),
                Err(protected_mode::DecodeError::InvalidPrefixes) => Some(Err(Error::InvalidPrefixes)),
                Err(protected_mode::DecodeError::TooLong) => Some(Err(Error::TooLong)),
                Err(
                    protected_mode::DecodeError::ExhaustedInput
                    | protected_mode::DecodeError::IncompleteDecoder,
                ) => None,
                Ok(inst) => Some(Ok(Instruction::X86(inst)))
            }
            DecoderKind::X64(decoder) => match decoder.decode(&mut self.reader) {
                Err(long_mode::DecodeError::InvalidOpcode) => Some(Err(Error::InvalidOpcode)),
                Err(long_mode::DecodeError::InvalidOperand) => Some(Err(Error::InvalidOperand)),
                Err(long_mode::DecodeError::InvalidPrefixes) => Some(Err(Error::InvalidPrefixes)),
                Err(long_mode::DecodeError::TooLong) => Some(Err(Error::TooLong)),
                Err(
                    long_mode::DecodeError::ExhaustedInput
                    | long_mode::DecodeError::IncompleteDecoder,
                ) => None,
                Ok(inst) => Some(Ok(Instruction::X64(inst)))
            },
        };

        let width = <Reader as yaxpeax_arch::Reader<u64, u8>>::offset(&mut self.reader) as usize;
        self.offset += width;
        self.width = width;
        return result;
    }
}
