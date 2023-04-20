//! Symbol demangler for common mangling schemes.

use std::borrow::Cow;
use tokenizing::{Colors, Color, Token, ColorScheme};

pub mod rust;
pub mod msvc;
pub mod itanium;

#[derive(Debug)]
pub struct TokenStream {
    /// Unmovable string which the [Token]'s have a pointer to.
    inner: std::pin::Pin<String>,

    /// Internal token representation which is unsafe to access outside of calling [Self::tokens].
    __tokens: Vec<Token>,
}

impl TokenStream {
    pub fn new(s: &str) -> Self {
        Self {
            inner: std::pin::Pin::new(s.to_string()),
            __tokens: Vec::with_capacity(128),
        }
    }

    pub fn simple(s: &str) -> Self {
        let mut this = Self {
            inner: std::pin::Pin::new(s.to_string()),
            __tokens: Vec::with_capacity(1),
        };

        this.__tokens.push(Token {
            text: Cow::Borrowed(this.inner()),
            color: Colors::item(),
        });

        this
    }

    /// SAFETY: must downcast &'static str to a lifetime that matches the lifetime of self.
    #[inline]
    pub fn inner<'a>(&self) -> &'a str {
        unsafe { std::mem::transmute(self.inner.as_ref()) }
    }

    #[inline]
    pub fn push(&mut self, text: &'static str, color: Color) {
        self.__tokens.push(Token {
            text: Cow::Borrowed(text),
            color,
        })
    }

    #[inline]
    pub fn push_cow(&mut self, text: Cow<'static, str>, color: Color) {
        self.__tokens.push(Token { text, color })
    }

    #[inline]
    pub fn pop(&mut self) {
        self.__tokens.pop();
    }

    #[inline]
    pub fn tokens<'src>(&'src self) -> &'src [Token] {
        self.__tokens.as_slice()
    }
}
