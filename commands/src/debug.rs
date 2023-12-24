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

use std::fmt;

const MAX_DEPTH: usize = 256;

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

    /// Symbol lookup table.
    index: &'src symbols::Index,

    /// Byte offset into input string.
    offset: usize,

    /// Recursion depth.
    depth: usize,

    /// Indicator used by the [`Failing`] trait.
    is_failing: bool,

    /// Arena of expressions.
    pool: Vec<Expr>,
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
    fn new(index: &'src symbols::Index, src: &'src str) -> Self {
        Self {
            src,
            index,
            offset: 0,
            depth: 0,
            is_failing: false,
            pool: Vec::with_capacity(16),
        }
    }

    /// Where we are in the string.
    fn src(&self) -> &'src str {
        &self.src[self.offset..]
    }

    /// Adds expression to arena.
    fn store(&mut self, expr: Expr) -> ExprRef {
        let idx = self.pool.len();
        self.pool.push(expr);
        ExprRef(idx)
    }

    /// Retrieves expression from arena.
    fn load(&self, expr_ref: &ExprRef) -> &Expr {
        &self.pool[expr_ref.0]
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

        // generic is missing a opening bracket
        if depth < 0 {
            return Err(Error {
                offset: Some(self.src.find(">").unwrap()),
                msg: "Mismatching brackets".to_string(),
            });
        }

        // generic is missing a closing bracket
        if depth > 0 {
            return Err(Error {
                offset: Some(self.src.rfind("<").unwrap()),
                msg: "Mismatching brackets".to_string(),
            });
        }

        Ok(&self.src[start..self.offset])
    }

    fn parse_primary(&mut self) -> Result<Expr, Error> {
        self.consume_whitespace();

        if let Ok(num) = self.number() {
            return Ok(Expr::Number(num));
        }
        
        let sym = self.symbol()?;
        if !sym.is_empty() {
            return match self.index.get_by_name(sym) {
                Some((addr, function)) => Ok(Expr::Symbol { addr, function }),
                None => self.error(&format!("Unknown symbol '{sym}'")),
            }
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
                },
                Some('/') => {
                    self.consume('/')?;
                    let rhs = self.parse_primary()?;
                    lhs = Expr::Compound {
                        lhs: self.store(lhs),
                        op: Operator::Div,
                        rhs: self.store(rhs),
                    };
                },
                Some('%') => {
                    self.consume('%')?;
                    let rhs = self.parse_primary()?;
                    lhs = Expr::Compound {
                        lhs: self.store(lhs),
                        op: Operator::Mod,
                        rhs: self.store(rhs),
                    };
                },
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
                },
                Some('-') => {
                    self.consume('-')?;
                    let rhs = self.parse_high_precedence()?;
                    lhs = Expr::Compound {
                        lhs: self.store(lhs),
                        op: Operator::Min,
                        rhs: self.store(rhs),
                    };
                },
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

/// Representation of any given expression.
#[derive(Debug, PartialEq)]
enum Expr {
    Number(isize),
    Symbol {
        addr: usize,
        function: symbols::Function,
    },
    Compound {
        lhs: ExprRef,
        op: Operator,
        rhs: ExprRef,
    },
}

impl Expr {
    /// Evaluate the address of a given expression.
    ///
    /// Returns [`None`] if the expression overflows.
    fn eval(&self, ctx: &Context) -> Option<isize> {
        match self {
            Self::Number(n) => Some(*n),
            Self::Symbol { addr, .. } => Some(*addr as isize),
            Self::Compound { lhs, op, rhs } => {
                let lhs = ctx.load(lhs).eval(ctx)?;
                let rhs = ctx.load(rhs).eval(ctx)?;

                match op {
                    Operator::Mul => lhs.checked_mul(rhs),
                    Operator::Div => lhs.checked_div(rhs),
                    Operator::Mod => lhs.checked_rem(rhs),
                    Operator::Add => lhs.checked_add(rhs),
                    Operator::Min => lhs.checked_sub(rhs),
                }
            }
        }
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

/// Parses expression and returns it's corresponding address.
pub fn parse(index: &symbols::Index, s: &str) -> Result<usize, Error> {
    if s.is_empty() {
        return Err(Error {
            offset: None,
            msg: "Empty expression".to_string(),
        });
    }

    let mut ctx = Context::new(index, s);

    match ctx.expr() {
        Err(err) => Err(err),
        Ok(e) => match e.eval(&ctx) {
            Some(val) => Ok(val as usize),
            None => Err(Error {
                offset: None,
                msg: "Expression overflowed".to_string(),
            }),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use symbols::{Function, TokenStream};

    macro_rules! eval_eq {
        ($expr:expr, $expected:expr) => {{
            let index = symbols::Index::new();

            match parse(&index, $expr) {
                Err(err) => panic!("failed to parse '{}' with error '{:?}'", $expr, err),
                Ok(parsed) => assert_eq!(parsed, $expected)
            }
        }};

        ([$($function:expr; $addr:expr),*], $expr:expr, $expected:expr) => {{
            #[allow(unused_mut)]
            let mut index = symbols::Index::new();

            $(
                let f = Function::new(TokenStream::simple($function), None);
                index.insert($addr, f);
            )*

            match parse(&index, $expr) {
                Err(err) => panic!("failed to parse '{}' with error '{:?}'", $expr, err),
                Ok(parsed) => assert_eq!(parsed, $expected)
            }
        }};
    }

    macro_rules! ast_eq {
        ($expr:expr, $expected:expr) => {{
            let index = symbols::Index::new();

            match Context::new(&index, $expr).expr() {
                Err(err) => panic!("failed to parse '{}' with error '{:?}'", $expr, err),
                Ok(parsed) => assert_eq!(parsed, $expected)
            }
        }};

        ([$($function:expr; $addr:expr),*], $expr:expr, $expected:expr) => {{
            #[allow(unused_mut)]
            let mut index = symbols::Index::new();

            $(
                let f = Function::new(TokenStream::simple($function), None);
                index.insert($addr, f);
            )*

            match Context::new(&index, $expr).expr() {
                Err(err) => panic!("failed to parse '{}' with error '{:?}'", $expr, err),
                Ok(parsed) => assert_eq!(parsed, $expected)
            }
        }};
    }

    #[test]
    fn simple() {
        eval_eq!("3 * 32", 96);
        eval_eq!("0x4f2", 0x4f2);
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
            ["abc::f<dyn Debug + Clone>"; 0x100],
            "abc::f<dyn Debug + Clone>",
            Expr::Symbol {
                addr: 0x100,
                function: Function::new(TokenStream::simple("abc::f<dyn Debug + Clone>"), None)
            }
        );
    }

    #[test]
    #[should_panic]
    fn mismatched_generic() {
        ast_eq!(
            ["abc::f<dyn Debug>"; 0x100],
            "abc::f<dyn Debug> + Clone>",
            Expr::Symbol {
                addr: 0x100,
                function: Function::new(TokenStream::simple("abc::f<dyn Debug + Clone>"), None)
            }
        );
        ast_eq!(
            ["abc::f<dyn Debug>"; 0x100],
            "abc::f<dyn Debug< + Clone>",
            Expr::Symbol {
                addr: 0x100,
                function: Function::new(TokenStream::simple("abc::f<dyn Debug + Clone>"), None)
            }
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
