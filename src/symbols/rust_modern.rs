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

use super::TokenStream;
use crate::colors::{self, Color};

/// Max recursion depth
const MAX_DEPTH: usize = 256;

/// Try to parse a rust v0 symbol
pub fn parse(s: &str) -> Option<TokenStream> {
    // paths have to be ascii
    if !s.bytes().all(|c| c.is_ascii()) {
        return None;
    }

    let mut parser = Parser::new(s);
    parser.path()?;

    Some(parser.stream)
}

/// State required to parse symbols
struct Parser {
    stream: TokenStream,
    offset: usize,
    depth: usize,
    printing: bool,
}

/// Differentiator for nested path's
enum NameSpace {
    Closure,
    Shim,
    Special(char),
    Internal(char)
}

impl Parser {
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
    fn src(&self) -> &'static str {
        &self.stream.inner()[self.offset..]
    }

    #[inline]
    fn push(&mut self, text: &'static str, color: Color) {
        if self.printing {
            self.stream.push(text, color);
        }
    }

    /// View the current byte in the mangled symbol without incrementing the offset.
    fn peek(&self) -> Option<u8> {
        self.src().bytes().next()
    }

    /// Increment the offset if the current byte equals the byte given.
    fn consume(&mut self, byte: u8) -> Option<()> {
        if self.src().bytes().next() == Some(byte) {
            self.offset += 1;
            return Some(());
        }

        None
    }

    /// Run a closure where each function called in it isn't appended to the [TokenStream].
    #[inline]
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
    #[inline]
    fn backref<F: FnOnce(&mut Self) -> Option<()>>(&mut self, f: F) -> Option<()> {
        self.depth += 1;

        if self.depth > MAX_DEPTH {
            return None;
        }

        let backref = self.base62()?;
        let current = self.offset;

        if backref >= self.offset - 1 {
            return None;
        }

        self.offset = backref;
        f(self)?;

        self.offset = current;
        self.depth += 1;

        Some(())
    }

    /// Consumes a series of bytes that are in the range 0 to Z and converts it to base 62.
    /// It also consumes an underscore in the case that the base62 number happens to be zero.
    fn base62(&mut self) -> Option<usize> {
        let mut num = 0usize;

        if let Some(..) = self.consume(b'_') {
            return Some(num);
        }

        while let Some(chr) = self.peek() {
            let base_62_chr = match chr {
                b'0'..=b'9' => chr - b'0',
                b'a'..=b'z' => chr - b'a' + 10,
                b'A'..=b'Z' => chr - b'A' + 36,
                b'_' => {
                    self.offset += 1;
                    return num.checked_add(1);
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
    fn disambiguator(&mut self) -> Option<usize> {
        if let Some(..) = self.consume(b's') {
            return self.base62();
        }

        None
    }

    /// Consumes either a regular unambiguous or a punycode enabled string.
    fn ident<'src>(&mut self) -> Option<&'src str> {
        if let Some(..) = self.consume(b'u') {
            todo!("punycode symbols decoding");
        }

        let len = self.base10()?;
        let _underscore = self.consume(b'_');

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
            c @ b'A'..=b'Z' => Some(NameSpace::Special(c as char)),
            c @ b'a'..=b'z' => Some(NameSpace::Internal(c as char)),
            _ => return None
        };

        self.offset += 1;
        ns
    }

    /// Appends a generic (which can be a lifetime, type or constant) out of a list of generics.
    fn generic(&mut self) -> Option<()> {
        if let Some(lifetime) = self.lifetime() {
            self.push(lifetime, colors::MAGENTA);
            self.push(" ", colors::MAGENTA);
            return Some(());
        }

        if let Some(..) = self.tipe() {
            return Some(());
        }

        if let Some(..) = self.consume(b'K') {
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
        if let Some(..) = self.consume(b'p') {
            self.push("_", colors::MAGENTA);
            return Some(());
        }

        if let Some(..) = self.consume(b'B') {
            return self.backref(|this| this.constant());
        }

        // can't implement constants using just &'static str's
        self.offset += 1;
        self.hex_nibbles()?;
        self.push("_", colors::MAGENTA);
        Some(())
    }

    /// Parses a lifetime if it's not a '_ or a part of token that
    /// ends up using more than 25 lifetimes.
    fn lifetime<'src>(&mut self) -> Option<&'src str> {
        self.consume(b'L')?;

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
        self.consume(b'G')?;
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
        if self.depth > MAX_DEPTH {
            return None;
        }

        self.depth += 1;

        match self.peek()? {
            // crate root
            b'C' => {
                self.offset += 1;

                let _disambiguator = self.disambiguator();
                let ident = self.ident()?;
                self.push(ident, colors::PURPLE);
            }
            // <T> (inherited impl)
            b'M' => {
                self.offset += 1;

                let _disambiguator = self.disambiguator();

                self.dont_print(|this| this.path())?;
                self.push("<", colors::BLUE);
                self.tipe()?;
                self.push(">", colors::BLUE);
            }
            // <T as Trait> (trait impl)
            b'X' => {
                self.offset += 1;

                let _disambiguator = self.disambiguator();

                self.dont_print(|this| this.path())?;
                self.push("<", colors::BLUE);
                self.tipe()?;
                self.push(" as ", colors::BLUE);
                self.path()?;
                self.push(">", colors::BLUE);
            }
            // <T as Trait> (trait definition)
            b'Y' => {
                self.offset += 1;

                self.push("<", colors::BLUE);
                self.tipe()?;
                self.push(" as ", colors::BLUE);
                self.path()?;
                self.push(">", colors::BLUE);
            }
            // ...::ident (nested path)
            b'N' => {
                self.offset += 1;

                let ns = self.namespace()?;
                self.path()?;
                let disambiguator = self.disambiguator();
                let ident = self.ident()?;

                self.push("::", colors::GRAY);

                match ns {
                    NameSpace::Closure => {
                        self.push("{closure", colors::GRAY);

                        if !ident.is_empty() {
                            self.push(":", colors::GRAY);
                            self.push(ident, colors::GRAY);
                        }

                        match disambiguator {
                            Some(0) => self.push("#0", colors::GRAY),
                            Some(1) => self.push("#1", colors::GRAY),
                            Some(2) => self.push("#2", colors::GRAY),
                            Some(3) => self.push("#3", colors::GRAY),
                            Some(4) => self.push("#4", colors::GRAY),
                            Some(5) => self.push("#5", colors::GRAY),
                            Some(6) => self.push("#6", colors::GRAY),
                            Some(7) => self.push("#7", colors::GRAY),
                            Some(8) => self.push("#8", colors::GRAY),
                            Some(9) => self.push("#9", colors::GRAY),
                            _ => {}
                        }

                        self.push("}", colors::GRAY);

                    }
                    _ => self.push(ident, colors::PURPLE),
                }
            }
            // ...<T, U, ..> (generic args)
            b'I' => {
                self.offset += 1;

                let next_is_type = self.src().get(..2) == Some("Nt");

                self.path()?;

                // generics on types shouldn't print a '::'
                if !next_is_type {
                    self.push("::", colors::GRAY);
                }

                self.push("<", colors::BLUE);

                let mut iters = 0;
                while let None = self.consume(b'E') {
                    if iters != 0 {
                        self.push(", ", colors::BLUE);
                    }

                    self.generic()?;
                    iters += 1;
                }

                self.push(">", colors::BLUE);
            }
            _ => return None,
        }

        self.depth -= 1;
        Some(())
    }

    /// Appends some generic type, failing if the recursion depth is greater than [MAX_DEPTH].
    fn tipe(&mut self) -> Option<()> {
        if self.depth > MAX_DEPTH {
            return None;
        }

        self.depth += 1;

        // basic types
        if let Some(tipe) = self.basic_tipe() {
            self.push(tipe, colors::PURPLE);
            return Some(());
        }

        // named type
        if let Some(..) = self.path() {
            return Some(());
        }

        match self.peek()? {
            // [T; N]
            b'A' => {
                self.offset += 1;

                self.push("[", colors::BLUE);
                self.tipe()?;
                self.push("; ", colors::BLUE);
                self.constant()?;
                self.push("]", colors::BLUE);
            }
            // [T]
            b'S' => {
                self.offset += 1;

                self.push("[", colors::BLUE);
                self.tipe()?;
                self.push("]", colors::BLUE);
            }
            // (T1, T2, T3, ..)
            b'T' => {
                self.offset += 1;

                self.push("(", colors::BLUE);

                let mut iters = 0;
                while let None = self.consume(b'E') {
                    if iters != 0 {
                        self.push(", ", colors::BLUE);
                    }

                    self.tipe()?;
                    iters += 1;
                }

                self.push(")", colors::BLUE);
            }
            // &T
            b'R' => {
                self.offset += 1;

                self.push("&", colors::BLUE);
                if let Some(lifetime) = self.lifetime() {
                    self.push(lifetime, colors::MAGENTA);
                    self.push(" ", colors::MAGENTA);
                }

                self.tipe()?;
            }
            // &mut T
            b'Q' => {
                self.offset += 1;

                self.push("&", colors::BLUE);
                if let Some(lifetime) = self.lifetime() {
                    self.push(lifetime, colors::MAGENTA);
                    self.push(" ", colors::MAGENTA);
                }

                self.push("mut ", colors::BLUE);
                self.tipe()?;
            }
            // *const T
            b'P' => {
                self.offset += 1;

                self.push("*const ", colors::BLUE);
                self.tipe()?;
            }
            // *mut T
            b'O' => {
                self.offset += 1;

                self.push("*mut ", colors::BLUE);
                self.tipe()?;
            }
            // fn(..) -> ..
            b'F' => {
                self.offset += 1;
                self.binder();

                if let Some(..) = self.consume(b'U') {
                    self.push("unsafe ", colors::RED);
                }

                if let Some(..) = self.consume(b'K') {
                    self.push("extern ", colors::RED);

                    if let Some(..) = self.consume(b'C') {
                        self.push("\"C\" ", colors::BLUE);
                    } else {
                        let ident = self.ident()?;

                        self.push("\"", colors::BLUE);
                        self.push(ident, colors::BLUE);
                        self.push("\"", colors::BLUE);
                    }
                }

                self.push("fn", colors::MAGENTA);
                self.push("(", colors::WHITE);

                let mut iters = 0;
                while let None = self.consume(b'E') {
                    if iters != 0 {
                        self.push(", ", colors::BLUE);
                    }

                    self.tipe()?;
                    iters += 1;
                }

                self.push(")", colors::WHITE);
                self.push(" -> ", colors::BLUE);
                self.tipe()?;
            }
            // dyn ..
            b'D' => {
                self.offset += 1;
                self.binder();
                self.push("dyn ", colors::RED);

                // associated traits e.g. Send + Sync + Pin
                let mut iters = 0;
                while let None = self.consume(b'E') {
                    if iters != 0 {
                        self.push(" + ", colors::BLUE);
                    }

                    self.path()?;

                    // associated trait bounds e.g. Trait<Assoc = X>
                    while let Some(..) = self.consume(b'p') {
                        self.push("<", colors::BLUE);
                        let ident = self.ident()?;
                        self.push(ident, colors::PURPLE);
                        self.push(" = ", colors::WHITE);
                        self.tipe()?;
                        self.push(">", colors::BLUE);
                    }

                    iters += 1;
                }

                if let Some(lifetime) = self.lifetime() {
                    self.push(" + ", colors::BLUE);
                    self.push(lifetime, colors::MAGENTA);
                }
            }
            b'B' => {
                self.offset += 1;
                self.backref(|this| this.tipe())?;
            }
            _ => return None,
        }

        self.depth -= 1;
        Some(())
    }
}

#[cfg(test)]
mod tests {
    macro_rules! eq {
        ($mangled:literal => $demangled:literal) => {
            let symbol = $crate::symbols::rust_modern::parse($mangled)
                .expect(&format!("Formatting '{}' failed.", $mangled));

            assert_eq!(
                String::from_iter(symbol.tokens().iter().map(|t| t.text)),
                $demangled
            )
        };
    }

    #[test]
    fn crate_ident() {
        eq!("C8demangle" => "demangle");
    }

    #[test]
    fn generics() {
        eq!("INvNvC3std3mem8align_ofjdE" => "std::mem::align_of::<usize, f64>");
        eq!("INvNtC3std3mem8align_ofINtC3wow6HolderpEE" =>
             "std::mem::align_of::<wow::Holder<_>>");
    }

    #[test]
    fn namespaces() {
        eq!("NvC4bite6decode" => "bite::decode");
        eq!("NvNvC4bite6decode6x86_64" => "bite::decode::x86_64");
        eq!("INvNvC4bite6decode6x86_64NvC3lol4damnE" =>
             "bite::decode::x86_64::<lol::damn>");
    }

    #[test]
    fn methods() {
        eq!("NvNvXs2_C7mycrateINtC7mycrate3FoopEINtNtC3std7convert4FrompE4from3MSG" =>
             "<mycrate::Foo<_> as std::convert::From<_>>::from::MSG");
    }

    #[test]
    fn pointers() {
        eq!("INvC4bite6decodeRL_eE" => "bite::decode::<&str>");
        eq!("INvC4bite6decodeRL0_eE" => "bite::decode::<&'a str>");

        eq!("INvC4bite6decodeQL_eE" => "bite::decode::<&mut str>");
        eq!("INvC4bite6decodeQL0_eE" => "bite::decode::<&'a mut str>");

        eq!("INvC4bite6decodePeE" => "bite::decode::<*const str>");
        eq!("INvC4bite6decodeOeE" => "bite::decode::<*mut str>");
    }

    #[test]
    fn arrays() {
        eq!("INvC4bite6decodeANtNvC3std5array5Arrayjf_E" =>
             "bite::decode::<[std::array::Array; _]>");
    }

    #[test]
    fn tupples() {
        eq!("INvNtC3std3mem8align_ofjTddNvC4core3ptrEE" =>
             "std::mem::align_of::<usize, (f64, f64, core::ptr)>");
    }

    #[test]
    fn backref() {
        eq!("NvMs1_NtNtCs9ltgdHTiPiY_4core3ptr8non_nullINtB5_7NonNullReE6as_ptrCslWKjbRFJPpS_3log" =>
             "<core::ptr::non_null::NonNull<&str>>::as_ptr");
    }

    #[test]
    fn constants() {
        eq!("NvMNtCs9ltgdHTiPiY_4core5sliceSRe4iterCslWKjbRFJPpS_3log" => "<[&str]>::iter");
    }

    #[test]
    fn fn_signature() {
        eq!("INvNtC3std3mem8align_ofFUdddEoE" =>
             "std::mem::align_of::<unsafe fn(f64, f64, f64) -> u128>");

        eq!("INvNtC3std3mem8align_ofFKCdddEoE" =>
             "std::mem::align_of::<extern \"C\" fn(f64, f64, f64) -> u128>");

        eq!("INvNtC3std3mem8align_ofFdddEoE" =>
             "std::mem::align_of::<fn(f64, f64, f64) -> u128>");
    }

    #[test]
    fn dyn_traits() {
        eq!("INvNtC4core4simd3mulDNvNtC4core3mem4Readp4ItemReEL_E" =>
             "core::simd::mul::<dyn core::mem::Read<Item = &str>>");

        eq!("INvNtC4core4simd3mulDNvNtC4core3mem4ReadEL0_E" =>
             "core::simd::mul::<dyn core::mem::Read + 'a>");

        eq!("INvNtC4core4simd3mulDNvNtC4core3mem4ReadEL_E" =>
             "core::simd::mul::<dyn core::mem::Read>");
    }

    #[test]
    fn closures() {
        eq!("NCNvC4bite6decode0" => "bite::decode::{closure}");
        eq!("NCNvC4bite6decodes_0" => "bite::decode::{closure#0}");
        eq!("NCNvC4bite6decodes0_3wow" => "bite::decode::{closure:wow#1}");
    }
}
