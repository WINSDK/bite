//! # `yaxpeax-x86`, a decoder for x86-family instruction sets
//!
//! `yaxpeax-x86` provides x86 decoders, for 64-bit and 32-bit modes.
//!
//! instructions, operands, registers, and generally all decoding structures, are in their mode's
//! respective submodule:
//! * `x86_64`/`amd64` decoding is under [`long_mode`]
//! * `x86_32`/`x86` decoding is under [`protected_mode`]

pub mod long_mode;
pub mod protected_mode;
mod safer_unchecked;

use tokenizing::{ColorScheme, Colors};

const MEM_SIZE_STRINGS: [&str; 64] = [
    "byte ", "word ", "BUG ", "dword ", "ptr ", "far ", "BUG ", "qword ", "BUG ", "mword ", "BUG ",
    "BUG ", "BUG ", "BUG ", "BUG ", "xmmword ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ",
    "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "ymmword ", "BUG ",
    "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ",
    "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "BUG ",
    "BUG ", "BUG ", "BUG ", "BUG ", "BUG ", "ptr ", "zmmword ",
];

struct Number(i32);

impl decoder::ToTokens for Number {
    fn tokenize(&self, stream: &mut decoder::TokenStream) {
        if self.0 == i32::MIN {
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Error {
    InvalidOpcode,
    InvalidOperand,
    InvalidPrefixes,
    TooLong,
    ExhaustedInput,
    IncompleteDecoder,
}

impl decoder::Failed for Error {
    #[inline]
    fn is_complete(&self) -> bool {
        !matches!(self, Error::ExhaustedInput | Error::IncompleteDecoder)
    }

    #[inline]
    fn incomplete_width(&self) -> usize {
        1
    }
}
