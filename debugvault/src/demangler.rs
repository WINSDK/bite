//! Symbol demangler for common mangling schemes.

use tokenizing::{Token, Color32};
use config::CONFIG;

pub fn parse(s: &str) -> TokenStream {
    // symbols without leading underscores are accepted as
    // dbghelp in windows strips them away

    let s = s.strip_suffix("$got").unwrap_or(s);
    let s = s.strip_suffix("$plt").unwrap_or(s);
    let s = s.strip_suffix("$pltgot").unwrap_or(s);

    // parse rust symbols
    if let Some(s) = crate::rust_legacy::parse(s) {
        return s;
    }

    // parse gnu/llvm/C/C++ symbols
    if let Some(s) = crate::itanium::parse(s) {
        return s;
    }

    // parse rust symbols that match the v0 mangling scheme
    if let Some(s) = crate::rust::parse(s) {
        return s;
    }

    // parse windows msvc C/C++ symbols
    if let Some(s) = crate::msvc::parse(s) {
        return s;
    }

    // return the original mangled symbol on failure
    TokenStream::simple(s)
}

#[derive(Debug)]
pub struct TokenStream {
    /// Unmovable string which the [Token]'s have a pointer to.
    inner: std::pin::Pin<String>,

    /// Internal token representation which is unsafe to access outside of calling [Self::tokens].
    tokens: Vec<Token>,
}

impl TokenStream {
    pub fn new(s: &str) -> Self {
        Self {
            inner: std::pin::Pin::new(s.to_string()),
            tokens: Vec::new(),
        }
    }

    pub fn simple(s: &str) -> Self {
        let mut this = Self {
            inner: std::pin::Pin::new(s.to_string()),
            tokens: Vec::with_capacity(1),
        };

        this.tokens.push(Token::from_string(s.to_string(), CONFIG.colors.asm.component));
        this
    }

    /// SAFETY: must downcast &'static str to a lifetime that matches the lifetime of self.
    #[inline]
    pub fn inner<'a>(&self) -> &'a str {
        unsafe { std::mem::transmute(self.inner.as_ref()) }
    }

    #[inline]
    pub fn push(&mut self, text: &'static str, color: Color32) {
        self.tokens.push(Token::from_str(text, color));
    }

    #[inline]
    pub fn push_string(&mut self, text: String, color: Color32) {
        self.tokens.push(Token::from_string(text, color));
    }

    #[inline]
    pub fn tokens(&self) -> &[Token] {
        self.tokens.as_slice()
    }
}

impl PartialEq for TokenStream {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}
