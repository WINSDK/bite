//! Rust v0 symbol demangler
//!
//! ```text
//! <symbol-name> =
//!     "_R" [<decimal-number>] <path> [<instantiating-crate>] [<vendor-specific-suffix>]
//!
//! <path> = "C" <identifier>                    // crate root
//!        | "M" <impl-path> <type>              // <T> (inherent impl)
//!        | "X" <impl-path> <type> <path>       // <T as Trait> (trait impl)
//!        | "Y" <type> <path>                   // <T as Trait> (trait definition)
//!        | "N" <namespace> <path> <identifier> // ...::ident (nested path)
//!        | "I" <path> {<generic-arg>} "E"      // ...<T, U> (generic args)
//!        | <backref>
//!
//! <impl-path> = [<disambiguator>] <path>
//!
//! <identifier> = [<disambiguator>] <undisambiguated-identifier>
//! <disambiguator> = "s" <base-62-number>
//! <undisambiguated-identifier> = ["u"] <decimal-number> ["_"] <bytes>
//!
//! <namespace> = "C"   // closure
//!             | "S"   // shim
//!             | <A-Z> // other special namespaces
//!             | <a-z> // internal namespaces
//!
//! <generic-arg> = <lifetime>
//!               | <type>
//!               | "K" <const> // forward-compat for const generics
//!
//! <lifetime> = "L" <base-62-number>
//! <binder> = "G" <base-62-number>
//!
//! <type> = <basic-type>
//!        | <path>                      // named type
//!        | "A" <type> <const>          // [T; N]
//!        | "S" <type>                  // [T]
//!        | "T" {<type>} "E"            // (T1, T2, T3, ...)
//!        | "R" [<lifetime>] <type>     // &T
//!        | "Q" [<lifetime>] <type>     // &mut T
//!        | "P" <type>                  // *const T
//!        | "O" <type>                  // *mut T
//!        | "F" <fn-sig>                // fn(...) -> ...
//!        | "D" <dyn-bounds> <lifetime> // dyn Trait<Assoc = X> + Send + 'a
//!        | <backref>
//!
//! <basic-type> = "a"      // i8
//!              | "b"      // bool
//!              | "c"      // char
//!              | "d"      // f64
//!              | "e"      // str
//!              | "f"      // f32
//!              | "h"      // u8
//!              | "i"      // isize
//!              | "j"      // usize
//!              | "l"      // i32
//!              | "m"      // u32
//!              | "n"      // i128
//!              | "o"      // u128
//!              | "s"      // i16
//!              | "t"      // u16
//!              | "u"      // ()
//!              | "v"      // ...
//!              | "x"      // i64
//!              | "y"      // u64
//!              | "z"      // !
//!              | "p"      // placeholder (e.g. for generic params), shown as _
//!
//! <fn-sig> = [<binder>] ["U"] ["K" <abi>] {<type>} "E" <type>
//!
//! <abi> = "C"
//!       | <undisambiguated-identifier>
//!
//! <dyn-bounds> = [<binder>] {<dyn-trait>} "E"
//! <dyn-trait> = <path> {<dyn-trait-assoc-binding>}
//! <dyn-trait-assoc-binding> = "p" <undisambiguated-identifier> <type>
//! <const> = <type> <const-data>
//!         | "p" // placeholder, shown as _
//!         | <backref>
//!
//! <const-data> = ["n"] {<hex-digit>} "_"
//! <base-62-number> = {<0-9a-zA-Z>} "_"
//! <backref> = "B" <base-62-number>
//!
//! <instantiating-crate> = <path>
//! <vendor-specific-suffix> = ("." | "$") <suffix>
//! ```
//!
//! source [2603-rust-symbol-name-mangling-v0](https://rust-lang.github.io/rfcs/2603-rust-symbol-name-mangling-v0.html)
mod tests;

use crate::TokenStream;
use config::CONFIG;
use tokenizing::{colors, Color32};

/// Max recursion depth.
const MAX_DEPTH: usize = 256;

/// Try to parse a rust v0 symbol.
pub fn parse(s: &str) -> Option<TokenStream> {
    // macOS prefixes symbols with an extra underscore therefore '__R' is allowed
    let s = s.strip_prefix('R').or(s.strip_prefix("_R")).or(s.strip_prefix("__R"))?;

    // paths have to be ascii
    if !s.bytes().all(|c| c.is_ascii()) {
        return None;
    }

    let mut parser = Parser::new(s);
    parser.path()?;

    Some(parser.stream)
}

/// State required to parse symbols.
struct Parser {
    stream: TokenStream,
    offset: usize,
    depth: usize,
    printing: bool,
}

/// Differentiator for nested path's.
#[derive(PartialEq)]
enum NameSpace {
    Closure,
    Shim,
    Special,
    Internal,
    Type,
    Value,
}

impl<'src> Parser {
    /// Create an initialized parser that hasn't started parsing yet.
    fn new(s: &str) -> Self {
        Self {
            stream: TokenStream::new(s),
            offset: 0,
            depth: 0,
            printing: true,
        }
    }

    /// Create a reference to the underlying pinned string that holds the mangled symbol.
    #[inline]
    fn src(&self) -> &'src str {
        &self.stream.inner()[self.offset..]
    }

    #[inline]
    fn push(&mut self, text: &'static str, color: Color32) {
        if self.printing {
            self.stream.push(text, color);
        }
    }

    /// View the current byte in the mangled symbol without incrementing the offset.
    #[inline]
    fn peek(&self) -> Option<u8> {
        self.src().bytes().next()
    }

    /// Increment the offset if the current byte equals the byte given.
    #[inline]
    fn eat(&mut self, byte: u8) -> bool {
        let matches = self.src().bytes().next() == Some(byte);
        self.offset += matches as usize;
        matches
    }

    /// Fails if recursion depth is reached, otherwise increments depth.
    #[inline]
    fn recurse_deeper(&mut self) -> Option<()> {
        self.depth += 1;
        (self.depth < MAX_DEPTH).then_some(())
    }

    /// Run a closure where each function called in it isn't appended to the [TokenStream].
    fn dont_print<F: FnOnce(&mut Self) -> Option<()>>(&mut self, f: F) -> Option<()> {
        self.printing = false;
        f(self)?;
        self.printing = true;

        Some(())
    }

    /// Run a closure that consumes a base64 number, modifies the offset to that backref.
    /// If the backref is ahead of the current offset, the function fails.
    /// If the recursion depth is greater than [MAX_DEPTH], the function fails.
    /// Restores offset after executing the closure.
    fn backref<F: FnOnce(&mut Self) -> Option<()>>(&mut self, f: F) -> Option<()> {
        self.recurse_deeper()?;

        let current = self.offset;
        let backref = self.base62()?;

        if backref >= current {
            return None;
        }

        let current = self.offset;
        self.offset = backref;
        f(self)?;
        self.offset = current;

        self.depth -= 1;
        Some(())
    }

    /// Run a closure repeatedly till the 'E' (end of list) character appears.
    /// Prints the delimiter between closure calls.
    /// Fails on iterating more than 100 items.
    fn delimited<F: Fn(&mut Self) -> Option<()>>(
        &mut self,
        delimiter: &'static str,
        f: F,
    ) -> Option<()> {
        if !self.eat(b'E') {
            f(self)?;
        }

        // iterate through the first 100 items
        for _ in 0..100 {
            if self.eat(b'E') {
                return Some(());
            }

            self.push(delimiter, CONFIG.colors.asm.expr);
            f(self)?;
        }

        // fail on too many iterations
        None
    }

    /// Consumes a series of bytes that are in the range 0 to Z and converts it to base 62.
    /// It also consumes an underscore in the case that the base62 number happens to be zero.
    fn base62(&mut self) -> Option<usize> {
        let mut num = 0usize;

        if self.eat(b'_') {
            return Some(num);
        }

        while let Some(chr) = self.peek() {
            let base_62_chr = match chr {
                b'0'..=b'9' => chr - b'0',
                b'a'..=b'z' => chr - b'a' + 10,
                b'A'..=b'Z' => chr - b'A' + 36,
                b'_' => {
                    num = num.checked_add(1)?;
                    self.offset += 1;

                    return Some(num);
                }
                _ => return None,
            };

            num = num.checked_mul(62)?;
            num = num.checked_add(base_62_chr as usize)?;

            self.offset += 1;
        }

        None
    }

    /// Consumes a series of bytes that are in the range 0 to 9 and converts it to base 10.
    fn base10(&mut self) -> Option<usize> {
        let mut len = 0usize;

        while let Some(digit @ b'0'..=b'9') = self.peek() {
            len = len.checked_mul(10)?;
            len = len.checked_add((digit - b'0') as usize)?;

            self.offset += 1;
        }

        Some(len)
    }

    /// Consumes an ident's disambiguator.
    #[inline]
    fn disambiguator(&mut self) -> Option<usize> {
        if self.eat(b's') {
            return self.base62();
        }

        None
    }

    /// Consumes either a regular unambiguous or a punycode enabled string.
    fn ident(&mut self) -> Option<&'src str> {
        if self.eat(b'u') && cfg!(debug_assertions) {
            // TODO: punycode symbols decoding
        }

        let len = self.base10()?;
        self.eat(b'_');

        self.src().get(..len).map(|slice| {
            self.offset += slice.len();
            slice
        })
    }

    /// Parses a path's namespace.
    fn namespace(&mut self) -> Option<NameSpace> {
        let ns = match self.peek()? {
            b'C' => Some(NameSpace::Closure),
            b'S' => Some(NameSpace::Shim),
            b'A'..=b'Z' => Some(NameSpace::Special),
            b't' => Some(NameSpace::Type),
            b'v' => Some(NameSpace::Value),
            b'a'..=b'z' => Some(NameSpace::Internal),
            _ => return None,
        };

        self.offset += 1;
        ns
    }

    /// Appends a generic (which can be a lifetime, type or constant) out of a list of generics.
    fn generic(&mut self) -> Option<()> {
        if let Some(lifetime) = self.lifetime() {
            self.push(lifetime, CONFIG.colors.asm.annotation);
            self.push(" ", colors::WHITE);
            return Some(());
        }

        if self.tipe().is_some() {
            return Some(());
        }

        if self.eat(b'K') {
            return self.constant();
        }

        None
    }

    /// Parses a series of ascii hex numbers, ending in a '_'.
    fn hex_nibbles(&mut self) -> Option<&[u8]> {
        let mut len = 0;

        loop {
            match self.peek()? {
                b'0'..=b'9' | b'a'..=b'f' => {}
                b'_' => {
                    self.offset += 1;
                    break;
                }
                _ => return None,
            }

            len += 1;
            self.offset += 1;
        }

        self.offset -= 1;
        let hex_nibbles = self.src()[..len].as_bytes();
        self.offset += 1;
        Some(hex_nibbles)
    }

    /// Appends a constant which is either a placeholder 'p', backref or a series of hex numbers.
    fn constant(&mut self) -> Option<()> {
        // placeholder
        if self.eat(b'p') {
            self.push("_", CONFIG.colors.brackets);
            return Some(());
        }

        if self.eat(b'B') {
            return self.backref(Self::constant);
        }

        // NOTE: can't implement generic constants using just &'static str's

        self.offset += 1;
        self.hex_nibbles()?;
        self.push("_", CONFIG.colors.brackets);
        Some(())
    }

    /// Parses a lifetime if it's not a '_ or a part of token that
    /// ends up using more than 25 lifetimes.
    fn lifetime(&mut self) -> Option<&'src str> {
        if !self.eat(b'L') {
            return None;
        }

        Some(match self.base62()? {
            1 => "'a",
            2 => "'b",
            3 => "'c",
            4 => "'d",
            5 => "'e",
            6 => "'f",
            7 => "'g",
            8 => "'h",
            9 => "'i",
            10 => "'j",
            11 => "'k",
            12 => "'l",
            13 => "'m",
            14 => "'n",
            15 => "'o",
            16 => "'p",
            17 => "'q",
            18 => "'s",
            19 => "'t",
            20 => "'u",
            21 => "'v",
            22 => "'w",
            23 => "'x",
            24 => "'y",
            25 => "'z",
            _ => return None,
        })
    }

    /// Parses a index into the lifetimes being used.
    fn binder(&mut self) -> Option<usize> {
        if !self.eat(b'G') {
            return None;
        }

        self.base62()
    }

    /// Parses a type that can be represented using just a single character.
    fn basic_tipe(&mut self) -> Option<&'static str> {
        let basic = match self.peek()? {
            b'b' => "bool",
            b'c' => "char",
            b'e' => "str",
            b'u' => "()",
            b'a' => "i8",
            b's' => "i16",
            b'l' => "i32",
            b'x' => "i64",
            b'n' => "i128",
            b'i' => "isize",
            b'h' => "u8",
            b't' => "u16",
            b'm' => "u32",
            b'y' => "u64",
            b'o' => "u128",
            b'j' => "usize",
            b'f' => "f32",
            b'd' => "f64",
            b'z' => "!",
            b'p' => "_",
            b'v' => "...",
            _ => return None,
        };

        self.offset += 1;
        Some(basic)
    }

    /// Appends some generic path, failing if the recursion depth is greater than [MAX_DEPTH].
    fn path(&mut self) -> Option<()> {
        self.recurse_deeper()?;

        match self.peek()? {
            // crate root
            b'C' => {
                self.offset += 1;

                self.disambiguator();
                let ident = self.ident()?;
                self.push(ident, CONFIG.colors.asm.component);
            }
            // <T> (inherited impl)
            b'M' => {
                self.offset += 1;

                self.disambiguator();
                self.dont_print(Self::path)?;
                self.push("<", CONFIG.colors.asm.annotation);
                self.tipe()?;
                self.push(">", CONFIG.colors.asm.annotation);
            }
            // <T as Trait> (trait impl)
            b'X' => {
                self.offset += 1;

                self.disambiguator();
                self.dont_print(Self::path)?;
                self.push("<", CONFIG.colors.asm.annotation);
                self.tipe()?;
                self.push(" as ", CONFIG.colors.asm.annotation);
                self.path()?;
                self.push(">", CONFIG.colors.asm.annotation);
            }
            // <T as Trait> (trait definition)
            b'Y' => {
                self.offset += 1;

                self.push("<", CONFIG.colors.asm.annotation);
                self.tipe()?;
                self.push(" as ", CONFIG.colors.asm.annotation);
                self.path()?;
                self.push(">", CONFIG.colors.asm.annotation);
            }
            // ...::ident (nested path)
            b'N' => {
                self.offset += 1;

                let ns = self.namespace()?;
                self.path()?;

                let disambiguator = self.disambiguator();
                let ident = self.ident()?;

                self.push("::", CONFIG.colors.delimiter);

                match ns {
                    NameSpace::Closure => {
                        self.push("{", CONFIG.colors.brackets);
                        self.push("closure", CONFIG.colors.asm.primitive);

                        if !ident.is_empty() {
                            self.push(":", CONFIG.colors.delimiter);
                            self.push(ident, CONFIG.colors.asm.component);
                        }

                        match disambiguator {
                            Some(0) => self.push("#0", CONFIG.colors.brackets),
                            Some(1) => self.push("#1", CONFIG.colors.brackets),
                            Some(2) => self.push("#2", CONFIG.colors.brackets),
                            Some(3) => self.push("#3", CONFIG.colors.brackets),
                            Some(4) => self.push("#4", CONFIG.colors.brackets),
                            Some(5) => self.push("#5", CONFIG.colors.brackets),
                            Some(6) => self.push("#6", CONFIG.colors.brackets),
                            Some(7) => self.push("#7", CONFIG.colors.brackets),
                            Some(8) => self.push("#8", CONFIG.colors.brackets),
                            Some(9) => self.push("#9", CONFIG.colors.brackets),
                            _ => {}
                        }

                        self.push("}", CONFIG.colors.brackets);
                    }
                    _ => self.push(ident, CONFIG.colors.asm.component),
                }
            }
            // ...<T, U, ..> (generic args)
            b'I' => {
                self.offset += 1;

                let next_is_type = self.src().get(..2) == Some("Nt");

                self.path()?;

                // generics on types shouldn't print a '::'
                if !next_is_type {
                    self.push("::", CONFIG.colors.delimiter);
                }

                self.push("<", CONFIG.colors.asm.annotation);
                self.delimited(", ", Self::generic)?;
                self.push(">", CONFIG.colors.asm.annotation);
            }
            b'B' => {
                self.offset += 1;
                self.backref(Self::path)?;
            }
            _ => return None,
        }

        self.depth -= 1;
        Some(())
    }

    /// Appends some generic type, failing if the recursion depth is greater than [MAX_DEPTH].
    fn tipe(&mut self) -> Option<()> {
        self.recurse_deeper()?;

        // basic types
        if let Some(tipe) = self.basic_tipe() {
            self.push(tipe, CONFIG.colors.asm.primitive);

            self.depth -= 1;
            return Some(());
        }

        // named type
        if self.path().is_some() {
            self.depth -= 1;
            return Some(());
        }

        match self.peek()? {
            // [T; N]
            b'A' => {
                self.offset += 1;

                self.push("[", CONFIG.colors.brackets);
                self.tipe()?;
                self.push("; ", CONFIG.colors.brackets);
                self.constant()?;
                self.push("]", CONFIG.colors.brackets);
            }
            // [T]
            b'S' => {
                self.offset += 1;

                self.push("[", CONFIG.colors.brackets);
                self.tipe()?;
                self.push("]", CONFIG.colors.brackets);
            }
            // (T1, T2, T3, ..)
            b'T' => {
                self.offset += 1;

                self.push("(", CONFIG.colors.brackets);
                self.delimited(", ", Self::tipe)?;
                self.push(")", CONFIG.colors.brackets);
            }
            // &T
            b'R' => {
                self.offset += 1;

                self.push("&", CONFIG.colors.asm.pointer);
                if let Some(lifetime) = self.lifetime() {
                    self.push(lifetime, CONFIG.colors.asm.annotation);
                    self.push(" ", colors::WHITE);
                }

                self.tipe()?;
            }
            // &mut T
            b'Q' => {
                self.offset += 1;

                self.push("&", CONFIG.colors.asm.pointer);
                if let Some(lifetime) = self.lifetime() {
                    self.push(lifetime, CONFIG.colors.asm.annotation);
                    self.push(" ", colors::WHITE);
                }

                self.push("mut ", CONFIG.colors.asm.annotation);
                self.tipe()?;
            }
            // *const T
            b'P' => {
                self.offset += 1;

                self.push("*", CONFIG.colors.asm.pointer);
                self.push("const ", CONFIG.colors.asm.annotation);
                self.tipe()?;
            }
            // *mut T
            b'O' => {
                self.offset += 1;

                self.push("*", CONFIG.colors.asm.pointer);
                self.push("mut ", CONFIG.colors.asm.annotation);
                self.tipe()?;
            }
            // fn(..) -> ..
            b'F' => {
                self.offset += 1;
                self.binder();

                if self.eat(b'U') {
                    self.push("unsafe ", CONFIG.colors.asm.pointer);
                }

                if self.eat(b'K') {
                    self.push("extern ", CONFIG.colors.asm.pointer);

                    if self.eat(b'C') {
                        self.push("\"", CONFIG.colors.brackets);
                        self.push("C", CONFIG.colors.asm.component);
                        self.push("\" ", CONFIG.colors.brackets);
                    } else {
                        let ident = self.ident()?;

                        self.push("\"", CONFIG.colors.brackets);
                        self.push(ident, CONFIG.colors.asm.component);
                        self.push("\"", CONFIG.colors.brackets);
                    }
                }

                self.push("fn", CONFIG.colors.asm.primitive);
                self.push("(", CONFIG.colors.asm.primitive);
                self.delimited(", ", Self::tipe)?;
                self.push(")", CONFIG.colors.brackets);
                self.push(" -> ", CONFIG.colors.brackets);
                self.tipe()?;
            }
            // dyn ..
            b'D' => {
                self.offset += 1;
                self.binder();
                self.push("dyn ", CONFIG.colors.asm.pointer);

                // associated traits e.g. Send + Sync + Pin
                self.delimited(" + ", |this| {
                    this.path()?;

                    // associated trait bounds e.g. Trait<Assoc = X>
                    while this.eat(b'p') {
                        this.push("<", CONFIG.colors.asm.annotation);
                        let ident = this.ident()?;
                        this.push(ident, CONFIG.colors.asm.component);
                        this.push(" = ", CONFIG.colors.asm.expr);
                        this.tipe()?;
                        this.push(">", CONFIG.colors.asm.annotation);
                    }

                    Some(())
                })?;

                if let Some(lifetime) = self.lifetime() {
                    self.push(" + ", CONFIG.colors.asm.expr);
                    self.push(lifetime, CONFIG.colors.asm.annotation);
                }
            }
            b'B' => {
                self.offset += 1;
                self.backref(Self::tipe)?;
            }
            _ => return None,
        }

        self.depth -= 1;
        Some(())
    }
}
