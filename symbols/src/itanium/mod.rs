//! This crate can parse a C++ “mangled” linker symbol name into a Rust value
//! describing what the name refers to: a variable, a function, a virtual table,
//! etc. The description type implements `Display`, producing human-readable
//! text describing the mangled name. Debuggers and profilers can use this crate
//! to provide more meaningful output.
//!
//! C++ requires the compiler to choose names for linker symbols consistently
//! across compilation units, so that two compilation units that have seen the
//! same declarations can pair up definitions in one unit with references in
//! another.  Almost all platforms other than Microsoft Windows follow the
//! [Itanium C++ ABI][itanium]'s rules for this.
//!
//! [itanium]: https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling
//!
//! For example, suppose a C++ compilation unit has the definition:
//!
//! ```c++
//! namespace space {
//!   int foo(int x, int y) { return x+y; }
//! }
//! ```
//!
//! The Itanium C++ ABI specifies that the linker symbol for that function must
//! be named `_ZN5space3fooEii`. This crate can parse that name into a Rust
//! value representing its structure. Formatting the value with the `format!`
//! macro or the `std::string::ToString::to_string` trait method yields the
//! string `space::foo(int, int)`, which is more meaningful to the C++
//! developer.
#![allow(rustdoc::invalid_html_tags, rustdoc::broken_intra_doc_links)]

mod ast;
mod error;
mod index_str;
mod subs;
mod tests;

use crate::TokenStream;
use ast::{Demangle, Parse, ParseContext};
use error::{Error, Result};
use index_str::IndexStr;

pub fn parse(s: &str) -> Option<TokenStream> {
    let sym = Symbol::new(s).ok()?;
    Some(sym.demangle())
}

/// A mangled symbol that has been parsed into an AST.
///
/// This is generic over some storage type `T` which can be either owned or
/// borrowed. See the `OwnedSymbol` and `BorrowedSymbol` type aliases.
#[derive(Clone, Debug, PartialEq)]
struct Symbol<'a> {
    raw: &'a str,
    substitutions: subs::SubstitutionTable,
    parsed: ast::MangledName,
}

impl Symbol<'_> {
    /// Given some raw storage, parse the mangled symbol from it with the default
    /// options.
    #[inline]
    fn new(raw: &str) -> Result<Symbol> {
        let mut substitutions = subs::SubstitutionTable::new();

        let parsed = {
            let ctx = ParseContext::new();
            let input = IndexStr::new(raw.as_bytes());

            let (parsed, tail) = ast::MangledName::parse(&ctx, &mut substitutions, input)?;

            if tail.is_empty() {
                parsed
            } else {
                return Err(Error::UnexpectedText);
            }
        };

        Ok(Symbol {
            raw,
            substitutions,
            parsed,
        })
    }

    /// Demangle the symbol and return it as a String.
    ///
    /// Unlike the `ToString` implementation, this function allows options to
    /// be specified.
    #[inline]
    fn demangle(&self) -> TokenStream {
        let mut ctx = ast::DemangleContext::new(&self.substitutions, self.raw);
        self.parsed.demangle(&mut ctx, None);
        ctx.stream
    }
}
