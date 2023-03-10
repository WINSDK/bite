//! Microsoft Visual Studio symbol demangler
//!
//! ```text
//! <mangled-name> = ? <name> <type-encoding>
//!
//! <name> = <unscoped-name> {[<named-scope>]+ | [<nested-name>]}? @
//!
//! <unqualified-name> = <operator-name>
//!                    | <ctor-dtor-name>
//!                    | <source-name>
//!                    | <template-name>
//!
//! <operator-name> = ???
//!                 | ?B // cast, the target type is encoded as the return type.
//!
//! <source-name> = <identifier> @
//!
//! <postfix> = <unqualified-name> [<postfix>]
//!           | <substitution> [<postfix>]
//!
//! <template-name> = <unscoped-template-name> <template-args>
//!                 | <substitution>
//!
//! <unscoped-template-name> = ?$ <unqualified-name>
//!
//! <type-encoding> = <function-class> <function-type>
//!                 | <storage-class> <variable-type>
//!
//! <function-class> = <member-function> E? // E designates a 64-bit 'this'
//!                                         // pointer. in 64-bit mode *all*
//!                                         // 'this' pointers are 64-bit.
//!                  | <global-function>
//!
//! <function-type> = <this-cvr-qualifiers> <calling-convention>
//!                   <return-type> <argument-list> <throw-spec>
//!
//! <member-function> = A // private: near
//!                   | B // private: far
//!                   | C // private: static near
//!                   | D // private: static far
//!                   | E // private: near
//!                   | F // private: far
//!                   | I // near
//!                   | J // far
//!                   | K // static near
//!                   | L // static far
//!                   | M // near
//!                   | N // far
//!                   | Q // near
//!                   | R // far
//!                   | S // static near
//!                   | T // static far
//!                   | U // near
//!                   | V // far
//!
//! <global-function> = Y // global near
//!                   | Z // global far
//!
//! <storage-class> = 0 // private static member
//!                 | 1 // protected static member
//!                 | 2 // public static member
//!                 | 3 // global
//!                 | 4 // static local
//! ```
//!
//! source [MicrosoftMangle.cpp](https://github.com/llvm-mirror/clang/blob/aa231e4be75ac4759c236b755c57876f76e3cf05/lib/AST/MicrosoftMangle.cpp#L1609)

use super::TokenStream;
use crate::colors;

pub fn parse(s: &str) -> Option<TokenStream> {
    None
}

/// State required to parse symbols
struct Parser {
    stream: TokenStream,
    offset: usize,
}

impl Parser {
    /// Create an initialized parser that hasn't started parsing yet.
    fn new(s: &str) -> Self {
        Self {
            stream: TokenStream::new(s),
            offset: 0,
        }
    }

    /// Create a reference to the underlying pinned string that holds the mangled symbol.
    #[inline]
    fn src(&self) -> &'static str {
        &self.stream.inner()[self.offset..]
    }

    /// View the current byte in the mangled symbol without incrementing the offset.
    fn peek(&self) -> Option<u8> {
        self.src().bytes().next()
    }

    /// View a slice in the mangled symbol without incrementing the offset.
    fn peek_slice(&self, range: std::ops::Range<usize>) -> Option<&[u8]> {
        self.src().as_bytes().get(range)
    }

    /// View the current byte in the mangled symbol, incrementing the offset.
    fn take(&mut self) -> Option<u8> {
        self.src().bytes().next().map(|byte| {
            self.offset += 1;
            byte
        })
    }

    /// Increment the offset if the current byte equals the byte given.
    fn consume(&mut self, byte: u8) -> Option<()> {
        if self.src().bytes().next() == Some(byte) {
            self.offset += 1;
            return Some(());
        }

        None
    }

    /// Increment the offset if the slices match.
    fn consume_slice(&mut self, slice: &[u8]) -> Option<()> {
        if self.src().as_bytes().get(..slice.len()) == Some(slice) {
            self.offset += slice.len();
            return Some(());
        }

        None
    }

    fn calling_conv(&mut self) -> Option<()> {
        let conv = match self.peek()? {
            b'A' | b'B' => "__cdecl ",
            b'C' | b'D' => "__pascal ",
            b'E' | b'F' => "__thiscall ",
            b'G' | b'H' => "__stdcall ",
            b'I' | b'J' => "__fastcall ",
            b'M' | b'N' => "__clrcall ",
            b'O' | b'P' => "__eabi ",
            b'L' => "__dll_export ",
            b'Q' => "__vectorcall",
            _ => return None,
        };

        self.offset += 1;
        self.stream.push(conv, colors::RED);
        Some(())
    }

    fn enuhm(&mut self) -> Option<()> {
        let enuhm = match self.peek()? {
            1 => "unsigned char",
            2 => "short",
            3 => "unsigned short",
            4 => "int",
            5 => "unsigned int",
            6 => "long",
            7 => "unsigned long",
            _ => return None,
        };

        self.offset += 1;
        self.stream.push("enum", colors::RED);
        self.stream.push("<", colors::PURPLE);
        self.stream.push(enuhm, colors::PURPLE);
        self.stream.push(">", colors::PURPLE);
        Some(())
    }

    fn tipe(&mut self) -> Option<()> {
        let tipe = if let Some(..) = self.consume(b'_') {
            match self.peek()? {
                b'A' => "&",
                b'B' => "&volatile",
                b'C' => "signed char",
                b'D' => "char",
                b'E' => "unsigned char",
                b'F' => "short",
                b'G' => "unsigned short",
                b'H' => "int",
                b'I' => "unsigned int",
                b'J' => "long",
                b'K' => "unsigned long",
                b'M' => "float",
                b'N' => "double",
                b'O' => "long double",
                b'P' => "*",
                b'Q' => "*const",
                b'R' => "*volatile",
                b'S' => "*const volatile",
                b'@' => "void",
                b'0'..=b'9' => return self.backref(),
                _ => return None,
            }
        } else {
            match self.peek()? {
                b'D' => "i8",
                b'E' => "u8",
                b'F' => "i16",
                b'G' => "u16",
                b'H' => "i32",
                b'I' => "u32",
                b'J' => "i64",
                b'K' => "u64",
                b'L' => "i128",
                b'M' => "u128",
                b'N' => "bool",
                b'O' => "Array",
                b'W' => "wchar",
                _ => return None,
            }
        };

        self.offset += 1;
        self.stream.push(tipe, colors::PURPLE);
        Some(())
    }

    fn function(&mut self) -> Option<()> {
        self.calling_conv();
        self.tipe()?;
        Some(())
    }

    fn tipe_modi(&mut self) -> Option<()> {
        if let Some(..) = self.consume_slice(b"$$C") {
            return Some(());
        }

        let modi = match self.peek()? {
            b'P' => "*",
            b'Q' => "*const",
            b'R' => "*volatile",
            b'S' => "*const volatile",
            b'A' => "&",
            b'B' => "&volatile",
            _ => return None,
        };

        self.stream.push(modi, colors::RED);
        self.consume_slice(b"$A");
        self.consume_slice(b"$B");
        self.class_modi()?;

        if let Some(..) = self.consume(b'Y') {
            todo!();
        }

        Some(())
    }

    fn class_modi(&mut self) -> Option<()> {
        let modi = self.take()?;

        let prefixes = [
            self.consume(b'E').map(|_| "__ptr64 "),
            self.consume(b'F').map(|_| "unaligned "),
            self.consume(b'I').map(|_| "restrict "),
        ];

        for prefix in prefixes.iter().filter_map(|&x| x) {
            self.stream.push(prefix, colors::RED);
        }

        let modi = match modi {
            b'B' | b'J' | b'N' | b'R' | b'V' | b'Z' | b'3' => "const ",
            b'C' | b'G' | b'K' | b'O' | b'S' | b'W' | b'0' | b'4' => "volatile ",
            b'D' | b'H' | b'L' | b'P' | b'T' | b'X' | b'1' | b'5' => "const volatile ",
            b'6' | b'7' | b'8' | b'9' => return self.function(),
            _ => match self.peek_slice(0..2) {
                Some(b"_A") | Some(b"_B") | Some(b"_C") | Some(b"_D") => return self.function(),
                _ => return None,
            },
        };

        self.stream.push(modi, colors::RED);
        Some(())
    }

    fn backref(&mut self) -> Option<()> {
        todo!()
    }
}
