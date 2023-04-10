use super::{Literal, Type, TokenStream, Modifiers, NestedPath};
use crate::colors::Color;

/// Max recursion depth
const MAX_DEPTH: usize = 256;

#[derive(Default, Debug)]
pub(super) struct Backrefs {
    /// Up to 10 idents can be memorized for lookup using backref's: ?0, ?1, ..
    memorized: [Literal; 10],

    /// Number of so far memorized idents.
    memorized_count: usize,

    /// A max of 10 function parameters is supported.
    params: [Type; 10],

    /// Number of so far encountered function parameters.
    param_count: usize,
}

impl Backrefs {
    pub fn try_memorizing_ident<'b>(&'b mut self, ident: &'b Literal) {
        let memorized = &self.memorized[..self.memorized_count];

        if !memorized.contains(ident) && self.memorized_count != 10 {
            self.memorized[self.memorized_count] = *ident;
            self.memorized_count += 1;
        }
    }

    pub fn get_memorized_ident(&mut self, idx: usize) -> Option<Literal> {
        if idx >= self.memorized_count {
            return None;
        }

        Some(Literal::Indexed(idx))
    }

    // TODO: change interface to not be cloning a type
    pub fn try_memorizing_param<'b>(&'b mut self, tipe: &'b Type) {
        let memorized = &self.params[..self.param_count];

        if !memorized.contains(tipe) && self.param_count != 10 {
            self.params[self.param_count] = tipe.clone();
            self.param_count += 1;
        }
    }

    // TODO: change interface to not be cloning a type
    pub fn get_memorized_param(&self, idx: usize) -> Option<Type> {
        if idx >= self.param_count {
            return None;
        }

        Some(self.params[idx].clone())
    }
}

/// State that needs to be shared whilst traversing nodes of the AST.
#[derive(Debug)]
pub(super) struct Context<'a> {
    pub stream: TokenStream,
    pub modifiers_in_use: Modifiers,
    pub offset: usize,
    pub parsing_qualifiers: bool,
    pub memorizing: bool,
    pub scope: &'a [NestedPath],
    depth: usize,
}

impl Context<'_> {
    /// Create an initialized parser that hasn't started parsing yet.
    pub fn new(s: &str) -> Self {
        Self {
            stream: TokenStream::new(s),
            modifiers_in_use: Modifiers::empty(),
            offset: 0,
            memorizing: true,
            parsing_qualifiers: true,
            scope: &[],
            depth: 0,
        }
    }

    /// Pushes a [`Literal`] to the [`TokenStream`], resolving any indexing within a literal.
    pub fn push_literal(&mut self, backrefs: &Backrefs, literal: &Literal, color: Color) {
        let literal = match literal {
            Literal::Borrowed { start, end } => &self.stream.inner()[*start..*end],
            Literal::Indexed(idx) => {
                return self.push_literal(backrefs, &backrefs.memorized[*idx], color);
            }
        };

        let literal: &'static str = unsafe { std::mem::transmute(literal) };

        self.stream.push(literal, color);
    }

    /// Create a reference to the underlying pinned string that holds the mangled symbol.
    #[inline]
    pub fn src<'b>(&self) -> &'b str {
        &self.stream.inner()[self.offset..]
    }

    /// View the current byte in the mangled symbol without incrementing the offset.
    pub fn peek(&self) -> Option<u8> {
        self.src().bytes().next()
    }

    /// View a slice in the mangled symbol without incrementing the offset.
    pub fn peek_slice<'b>(&self, range: std::ops::Range<usize>) -> Option<&'b [u8]> {
        self.src().as_bytes().get(range)
    }

    /// View the current byte in the mangled symbol, incrementing the offset.
    pub fn take(&mut self) -> Option<u8> {
        self.src().bytes().next().map(|byte| {
            self.offset += 1;
            byte
        })
    }

    /// Increment the offset if the current byte equals the byte given.
    pub fn consume(&mut self, byte: u8) -> Option<()> {
        if self.src().bytes().next() == Some(byte) {
            self.offset += 1;
            return Some(());
        }

        None
    }

    /// Increment the offset if the current byte equals the byte given.
    pub fn eat(&mut self, byte: u8) -> bool {
        let matches = self.src().bytes().next() == Some(byte);
        self.offset += matches as usize;
        matches
    }

    /// Increment the offset if the slices match.
    pub fn eat_slice(&mut self, slice: &[u8]) -> bool {
        let matches = self.src().as_bytes().get(..slice.len()) == Some(slice);
        self.offset += slice.len() * (matches as usize);
        matches
    }

    /// Parses a base10 number, incrementing the offset.
    pub fn base10(&mut self) -> Option<usize> {
        let n = match self.peek()? {
            c @ b'0'..=b'9' => (c - b'0') as usize,
            _ => return None,
        };

        self.offset += 1;
        Some(n)
    }

    /// Parses a base16 number that's either in lowercase or upcase, incrementing the offset.
    pub fn base16(&mut self) -> Option<usize> {
        let n = match self.peek()? {
            c @ b'0'..=b'9' => (c - b'0') as usize,
            c @ b'a'..=b'f' => (c - b'a') as usize,
            c @ b'A'..=b'F' => (c - b'A') as usize,
            _ => return None,
        };

        self.offset += 1;
        Some(n)
    }

    /// Parses a generic number (positive, negative, hex or decimal).
    ///
    /// ```text
    /// = <non-negative>
    /// | <hex-digit>
    /// | <number>
    ///
    /// <number>       = '?' <non-negative>
    /// <hex-digit>    = 'A'..='P'
    /// <non-negative> = '1'..='9'
    ///                | {<hex-digit>} '@'
    /// ```
    pub fn number(&mut self) -> Option<isize> {
        let negative = self.eat(b'?');

        if let Some(digit) = self.base10() {
            let mut digit = digit as isize + 1;

            if negative {
                digit = -digit;
            }
            return Some(digit);
        }

        let mut n = 0isize;
        loop {
            match self.take()? {
                chr @ b'A'..=b'P' => {
                    n = n.checked_mul(16)?;
                    n = n.checked_add((chr - b'A') as isize)?;
                }
                b'@' => {
                    if negative {
                        n = -n;
                    }
                    break Some(n);
                }
                _ => break None,
            }
        }
    }

    /// Parses a series of characters up to the character '@', incrementing the offset
    /// by the amount of characters parsed + the terminator.
    pub fn ident(&mut self) -> Option<Literal> {
        let start = self.offset;
        let len = self.src().bytes().position(|c| c == b'@')?;
        self.offset += len + 1;
        Some(Literal::Borrowed {
            start,
            end: start + len,
        })
    }

    /// Increments the depth of the current parser, failing when the depth surpassed [`MAX_DEPTH`].
    #[inline]
    pub fn descent(&mut self) -> Option<()> {
        self.depth += 1;

        if self.depth > MAX_DEPTH {
            return None;
        }

        Some(())
    }

    #[inline]
    /// Decrements the depth of the current parser, panics on negative depths.
    pub fn ascent(&mut self) {
        self.depth -= 1;
    }
}
