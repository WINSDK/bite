//! Grammer for expressions used by debugger.
//!
//! ```text
//! <input> = <ws> <expr> <ws>
//! <expr>  = <compound> | <number> | <symbol>
//!
//! <number> = ['-'] <integer> | ['-'] <hex>
//!
//! <hex>     = '0' ('x' | 'X') {'A'..'F' | 'a'..'f' | '0'..'9'}+
//! <integer> = {'0'..'9'}+
//!
//! <symbol>  = {<characters>}+ # if known in database
//!
//! <compound> = <expr> <ws> <op> <ws> <expr> | '(' <ws> <expr> <ws> ')'
//! <op> = '+' | '-' | '*' | '/' | '%'
//! ```

// TODO: Implement binary presidence (10 + 10 * 10 == 110).
//       This likely requires parsing in two steps where we first generate tokens.

use debugvault::Index;
use std::fmt;

const MAX_DEPTH: usize = 256;

type Span = std::ops::RangeInclusive<usize>;

/// Trait for indicating that any error has to be propagated up.
trait Failing: Sized {
    fn failing(self, ctx: &mut Context) -> Self;
}

impl<T, E> Failing for Result<T, E> {
    fn failing(self, ctx: &mut Context) -> Self {
        ctx.is_failing = self.is_err();
        self
    }
}

impl<T> Failing for Option<T> {
    fn failing(self, ctx: &mut Context) -> Self {
        ctx.is_failing = self.is_none();
        self
    }
}

/// Index into expression arena.
#[derive(Debug, PartialEq, Clone, Copy)]
struct ExprRef(usize);

/// Information required at runtime when parsing expressions.
#[derive(Debug)]
struct Context<'src> {
    /// Reference to input string.
    src: &'src str,

    /// Byte offset into input string.
    offset: usize,

    /// Recursion depth.
    depth: usize,

    /// Indicator used by the [`Failing`] trait.
    is_failing: bool,

    /// Arena of expressions.
    children: Vec<Expr>,
}

/// Error with line number and context.
#[derive(Debug, PartialEq)]
pub struct Error {
    // Byte offset.
    offset: Option<usize>,

    /// Associated message.
    msg: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.msg)?;
        if let Some(offset) = self.offset {
            f.write_str(" at position ")?;
            offset.fmt(f)?;
        }
        f.write_str(".")?;
        Ok(())
    }
}

impl<'src> Context<'src> {
    /// Create's a new [`Context`] required for parsing json
    fn new(src: &'src str) -> Self {
        Self {
            src,
            offset: 0,
            depth: 0,
            is_failing: false,
            children: Vec::with_capacity(16),
        }
    }

    /// Where we are in the string.
    fn src(&self) -> &'src str {
        &self.src[self.offset..]
    }

    /// Adds expression to arena.
    fn store(&mut self, expr: Expr) -> ExprRef {
        let idx = self.children.len();
        self.children.push(expr);
        ExprRef(idx)
    }

    /// Increases the known recursion depth and checks for if we overflow [`MAX_DEPTH`].
    fn descent(&mut self) -> Result<(), Error> {
        self.depth += 1;

        if self.depth == MAX_DEPTH {
            self.error("Reached recursion depth")
        } else {
            Ok(())
        }
    }

    /// Decreases the known recursion depth.
    fn ascent(&mut self) {
        self.depth -= 1;
    }

    /// Formats an [`Error`] by getting the current offset.
    fn error<T>(&self, msg: &str) -> Result<T, Error> {
        Err(Error {
            offset: Some(self.offset),
            msg: msg.to_string(),
        })
    }

    /// Formats an [`Error`] by getting the current offset whilst
    /// notifying that the error has to also be propagated up.
    fn failing<T>(&mut self, msg: &str) -> Result<T, Error> {
        self.is_failing = true;
        self.error(msg)
    }

    /// Return the next character in the stream.
    fn peek(&mut self) -> Option<char> {
        self.src().chars().next()
    }

    /// Conditionally increments the stream if the first character matches `chr`.
    fn consume(&mut self, chr: char) -> Result<(), Error> {
        match self.peek() {
            Some(got) if got == chr => {
                self.offset += got.len_utf8();
                Ok(())
            }
            Some(got) => self.error(&format!("Expected '{chr}' got '{got}'")),
            None => self.error(&format!("Expected '{chr}' got EOF")),
        }
    }

    /// Increments the stream whilst any whitespace is encountered.
    fn consume_whitespace(&mut self) {
        // all forms of accepted whitespace
        while let Some('\u{0020}' | '\u{000a}' | '\u{000d}' | '\u{0009}') = self.peek() {
            self.offset += 1;
        }
    }

    /// Reads a base 10 digit, incrementing the stream past the digit.
    fn base10(&mut self) -> Option<u8> {
        let n = match self.peek()? as u8 {
            chr @ b'0'..=b'9' => chr - b'0',
            _ => return None,
        };

        self.offset += 1;
        Some(n)
    }

    /// Reads a base 16 digit, incrementing the stream past the digit.
    fn base16(&mut self) -> Option<u8> {
        let n = match self.peek()? as u8 {
            chr @ b'0'..=b'9' => chr - b'0',
            chr @ b'a'..=b'f' => chr - b'a' + 10,
            chr @ b'A'..=b'F' => chr - b'A' + 10,
            _ => return None,
        };

        self.offset += 1;
        Some(n)
    }

    /// Reads a base 10 number, both negative or positive.
    /// Increments stream past the integer.
    fn integer(&mut self) -> Result<isize, Error> {
        let mut int = 0isize;

        match self.base10() {
            Some(digit) => {
                int = match int.checked_add(digit as isize) {
                    Some(int) => int,
                    None => return self.failing("Integer expression is too large"),
                };
            }
            None => return self.error("Integer didn't contain any digits"),
        }

        while let Some(digit) = self.base10() {
            int = match int.checked_mul(10) {
                Some(int) => int,
                None => return self.failing("Integer expression is too large"),
            };

            int = match int.checked_add(digit as isize) {
                Some(int) => int,
                None => return self.failing("Integer expression is too large"),
            };
        }

        Ok(int)
    }

    /// Reads a base 16 number, both negative or positive.
    /// Increments stream past the hexadecimal integer.
    fn hex(&mut self) -> Result<isize, Error> {
        let mut int = 0isize;

        self.consume('0')?;

        if let (Err(..), Err(..)) = (self.consume('x'), self.consume('X')) {
            return self.error("unreachable");
        }

        match self.base16() {
            Some(digit) => {
                int = match int.checked_add(digit as isize) {
                    Some(int) => int,
                    None => return self.failing("Integer expression is too large"),
                };
            }
            None => return self.error("Integer didn't contain any digits"),
        }

        while let Some(digit) = self.base16() {
            int = match int.checked_mul(16) {
                Some(int) => int,
                None => return self.failing("Integer expression is too large"),
            };

            int = match int.checked_add(digit as isize) {
                Some(int) => int,
                None => return self.failing("Integer expression is too large"),
            };
        }

        Ok(int)
    }

    /// Reads a base 10 or base 16 number, incrementing the stream past the number.
    fn number(&mut self) -> Result<isize, Error> {
        let is_neg = self.consume('-').is_ok();

        let mut int = if self.src().starts_with("0x") || self.src().starts_with("0X") {
            self.hex()?
        } else {
            self.integer()?
        };

        if is_neg {
            int = match int.checked_mul(-1) {
                Some(int) => int,
                None => return self.failing("Integer expression is too large"),
            };
        }

        return Ok(int);
    }

    /// Reads a serious of characters.
    /// If a pair of <..> is detected, it will allow any series of characters
    /// in-between the generic.
    fn symbol(&mut self) -> Result<&'src str, Error> {
        let start = self.offset;
        let mut depth = 0isize;

        loop {
            match self.peek() {
                // operators are ambiguous
                Some('+' | '-' | '*' | '/' | '%') if depth == 0 => break,
                // brackets are ambiguous
                Some('(' | ')') if depth == 0 => break,
                // whitespace is ambiguous
                Some('\u{0020}' | '\u{000a}' | '\u{000d}' | '\u{0009}') if depth == 0 => break,
                // EOF means there we must be at the end of a symbol
                None => break,
                // entering a generic
                Some('<') => {
                    depth += 1;
                    self.offset += 1;
                }
                // existing a generic
                Some('>') => {
                    depth -= 1;
                    self.offset += 1;
                }
                // any other character should be part of a valid symbol
                Some(chr) => self.offset += chr.len_utf8(),
            }
        }

        Ok(&self.src[start..self.offset])
    }

    fn parse_primary(&mut self) -> Result<Expr, Error> {
        self.consume_whitespace();

        if let Ok(num) = self.number() {
            return Ok(Expr::Number(num));
        }

        let start = self.offset;
        let sym = self.symbol()?;
        if !sym.is_empty() {
            let end = self.offset;
            return Ok(Expr::Symbol {
                val: sym.to_string(),
                span: start..=end,
            });
        }

        if self.consume('(').is_ok() {
            let inner_expr = self.expr_inner()?;
            self.consume(')')?;
            return Ok(inner_expr);
        }

        self.error("Expected a primary expression")
    }

    fn parse_high_precedence(&mut self) -> Result<Expr, Error> {
        let mut lhs = self.parse_primary()?;

        loop {
            self.consume_whitespace();
            match self.peek() {
                Some('*') => {
                    self.consume('*')?;
                    let rhs = self.parse_primary()?;
                    lhs = Expr::Compound {
                        lhs: self.store(lhs),
                        op: Operator::Mul,
                        rhs: self.store(rhs),
                    };
                }
                Some('/') => {
                    self.consume('/')?;
                    let rhs = self.parse_primary()?;
                    lhs = Expr::Compound {
                        lhs: self.store(lhs),
                        op: Operator::Div,
                        rhs: self.store(rhs),
                    };
                }
                Some('%') => {
                    self.consume('%')?;
                    let rhs = self.parse_primary()?;
                    lhs = Expr::Compound {
                        lhs: self.store(lhs),
                        op: Operator::Mod,
                        rhs: self.store(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(lhs)
    }

    fn parse_low_precedence(&mut self) -> Result<Expr, Error> {
        let mut lhs = self.parse_high_precedence()?;

        loop {
            self.consume_whitespace();
            match self.peek() {
                Some('+') => {
                    self.consume('+')?;
                    let rhs = self.parse_high_precedence()?;
                    lhs = Expr::Compound {
                        lhs: self.store(lhs),
                        op: Operator::Add,
                        rhs: self.store(rhs),
                    };
                }
                Some('-') => {
                    self.consume('-')?;
                    let rhs = self.parse_high_precedence()?;
                    lhs = Expr::Compound {
                        lhs: self.store(lhs),
                        op: Operator::Min,
                        rhs: self.store(rhs),
                    };
                }
                _ => break,
            }
        }

        Ok(lhs)
    }

    fn expr_inner(&mut self) -> Result<Expr, Error> {
        self.descent()?;
        let expr = self.parse_low_precedence()?;
        self.ascent();
        Ok(expr)
    }

    /// Parses an [`Expr`] with surrounding whitespace, checking if
    /// any trailing characters are left after parsing.
    fn expr(&mut self) -> Result<Expr, Error> {
        self.consume_whitespace();
        let expr = self.expr_inner()?;
        self.consume_whitespace();

        if !self.src().is_empty() {
            return self.error("Trailing characters in expression");
        }

        Ok(expr)
    }
}

/// Basic mathematical operations.
#[derive(Debug, PartialEq, Eq)]
enum Operator {
    Add,
    Min,
    Mul,
    Div,
    Mod,
}

/// Representation of any given expression.
#[derive(Debug, PartialEq)]
enum Expr {
    Number(isize),
    Symbol {
        val: String,
        span: Span,
    },
    Compound {
        lhs: ExprRef,
        op: Operator,
        rhs: ExprRef,
    },
}

/// Storable [`Expr`] with needed [`ExprRef`]'s.
#[derive(Debug, PartialEq)]
pub struct CompleteExpr {
    /// Arena of expressions.
    children: Vec<Expr>,

    /// Parsed expression.
    root: Expr,
}

impl CompleteExpr {
    /// Retrieves expression from arena.
    fn load(&self, expr_ref: ExprRef) -> &Expr {
        &self.children[expr_ref.0]
    }

    fn eval_recursive(&self, node: &Expr, index: &Index) -> Result<isize, Error> {
        match node {
            Expr::Number(val) => Ok(*val),
            Expr::Symbol { val, .. } => match index.get_func_by_name(val) {
                Some(addr) => Ok(addr as isize),
                None => Err(Error {
                    offset: None,
                    msg: format!("Unknown symbol '{val}'"),
                }),
            },
            Expr::Compound { lhs, op, rhs } => {
                let lhs = self.eval_recursive(self.load(*lhs), index)?;
                let rhs = self.eval_recursive(self.load(*rhs), index)?;
                let err = || Error {
                    offset: None,
                    msg: "Expression overflowed".to_string(),
                };

                match op {
                    Operator::Mul => lhs.checked_mul(rhs).ok_or_else(err),
                    Operator::Div => lhs.checked_div(rhs).ok_or_else(err),
                    Operator::Mod => lhs.checked_rem(rhs).ok_or_else(err),
                    Operator::Add => lhs.checked_add(rhs).ok_or_else(err),
                    Operator::Min => lhs.checked_sub(rhs).ok_or_else(err),
                }
            }
        }
    }

    /// Evaluate the address of a given expression.
    ///
    /// Returns [`None`] if the expression overflows.
    pub fn eval(&self, index: &Index) -> Result<isize, Error> {
        self.eval_recursive(&self.root, index)
    }

    fn find_matching_symbol<'src>(
        &'src self,
        node: &'src Expr,
        cursor: usize,
    ) -> Option<(&'src str, Span)> {
        match node {
            Expr::Symbol { val, span } => span.contains(&cursor).then_some((val, span.clone())),
            Expr::Compound { lhs, rhs, .. } => {
                if let Some(matching) = self.find_matching_symbol(self.load(*lhs), cursor) {
                    return Some(matching);
                }

                if let Some(matching) = self.find_matching_symbol(self.load(*rhs), cursor) {
                    return Some(matching);
                }

                None
            }
            Expr::Number(_) => None,
        }
    }

    pub fn autocomplete<'a>(
        &self,
        index: &'a Index,
        cursor: usize,
    ) -> Option<(debugvault::prefix::Match, Span)> {
        if let Some((prefix, span)) = self.find_matching_symbol(&self.root, cursor) {
            return Some((index.prefixes.find(prefix), span));
        }

        None
    }

    pub fn parse(s: &str) -> Result<Self, Error> {
        if s.is_empty() {
            return Err(Error {
                offset: None,
                msg: "Empty expression".to_string(),
            });
        }

        let mut ctx = Context::new(s);
        ctx.expr().map(|expr| CompleteExpr {
            children: ctx.children,
            root: expr,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(index: &debugvault::Index, s: &str) -> Result<usize, Error> {
        CompleteExpr::parse(s)?.eval(index).map(|addr| addr as usize)
    }

    macro_rules! eval_eq {
        ($expr:expr, $expected:expr) => {{
            let index = debugvault::Index::default();

            match parse(&index, $expr) {
                Err(err) => panic!("failed to parse '{}' with error '{:?}'", $expr, err),
                Ok(parsed) => assert_eq!(parsed, $expected)
            }
        }};

        ([$($function:expr; $addr:expr),*], $expr:expr, $expected:expr) => {{
            #[allow(unused_mut)]
            let mut index = debugvault::Index::default();

            $(
                index.insert_func($addr, $function);
            )*

            match parse(&index, $expr) {
                Err(err) => panic!("failed to parse '{}' with error '{:?}'", $expr, err),
                Ok(parsed) => assert_eq!(parsed, $expected)
            }
        }};
    }

    macro_rules! ast_eq {
        ($expr:expr, $expected:expr) => {{
            match Context::new($expr).expr() {
                Err(err) => panic!("failed to parse '{}' with error '{:?}'", $expr, err),
                Ok(parsed) => assert_eq!(parsed, $expected),
            }
        }};

        ($expr:expr, $expected:expr) => {{
            match Context::new($expr).expr() {
                Err(err) => panic!("failed to parse '{}' with error '{:?}'", $expr, err),
                Ok(parsed) => assert_eq!(parsed, $expected),
            }
        }};
    }

    fn name_span_pair(name: &str) -> Expr {
        Expr::Symbol {
            val: name.to_string(),
            span: 0..=name.len(),
        }
    }

    #[test]
    fn simple() {
        eval_eq!("3 * 32", 96);
        eval_eq!("0x4f2", 0x4f2);
        ast_eq!("0x4f2", Expr::Number(0x4f2));
    }

    #[test]
    fn path() {
        eval_eq!(["abc::f"; 0x1234], "abc::f", 0x1234);
        eval_eq!(
            ["abc::f<std::fmt::Display>"; 0x1234],
            "abc::f<std::fmt::Display>",
            0x1234
        );
    }

    #[test]
    fn compound() {
        eval_eq!(["abc::f"; 0x100], "abc::f * 0x10", 0x1000);
        eval_eq!(
            ["abc::f<std::fmt::Display>"; 0x100],
            "(abc::f<std::fmt::Display> + 10) / 12",
            22
        );
    }

    #[test]
    fn generic() {
        eval_eq!(
            ["abc::f<dyn Debug + Clone>"; 0x100],
            "abc::f<dyn Debug + Clone>",
            0x100
        );
        ast_eq!(
            "abc::f<dyn Debug + Clone>",
            name_span_pair("abc::f<dyn Debug + Clone>")
        );
    }

    #[test]
    fn operation_order() {
        eval_eq!("1 + 10 * 10", 101);
        eval_eq!("1 + (10 + 10)", 21);
        eval_eq!("(10 + 10) * 2", 40);
        eval_eq!("2 * (10 + 10)", 40);
        eval_eq!("10 * 4 / 2", 20);
        eval_eq!("10 * 2 / 4", 5);
    }
}
