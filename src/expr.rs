//! Grammer for expressions used by debugger.
//!
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

/// Information required at runtime when parsing expressions.
#[derive(Debug)]
struct Context<'src> {
    /// Reference to input string.
    src: &'src str,

    /// Symbol lookup table.
    index: &'src symbols::Index,

    /// Offset into input string.
    offset: usize,

    /// Recursion depth.
    depth: usize,

    /// Indicator used by the [`Failing`] trait
    is_failing: bool,
}

/// Error with line number and context.
#[derive(PartialEq)]
pub struct Error {
    offset: Option<usize>,
    msg: String,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.msg)?;
        if let Some(offset) = self.offset {
            f.write_str(" at position ")?;
            offset.fmt(f)?;
        }
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
        }
    }

    /// Where we are in the string.
    fn src(&self) -> &'src str {
        &self.src[self.offset..]
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
                self.offset += 1;
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

    /// Reads a base 10 digit, incrementing the stream past the integer.
    fn base10(&mut self) -> Option<u8> {
        let n = match self.peek()? as u8 {
            chr @ b'0'..=b'9' => chr - b'0',
            _ => return None,
        };

        self.offset += 1;
        Some(n)
    }

    /// Reads a base 16 digit, incrementing the stream past the integer.
    fn base16(&mut self) -> Option<u8> {
        let n = match self.peek()? as u8 {
            chr @ b'0'..=b'9' => chr - b'0',
            chr @ b'a'..=b'f' => chr - b'a',
            chr @ b'A'..=b'F' => chr - b'A',
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
    /// Increments stream past the integer.
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

    fn operator(&mut self) -> Result<Operator, Error> {
        let op = match self.peek() {
            Some(op) => op,
            None => return self.error("Trailing characters in expression"),
        };

        let op = match op {
            '+' => Operator::Add,
            '-' => Operator::Min,
            '*' => Operator::Mul,
            '/' => Operator::Div,
            '%' => Operator::Mod,
            chr => return self.failing(&format!("Encountered invalid operator '{chr}'")),
        };

        self.offset += 1;
        Ok(op)
    }

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
                Some('<') => depth += 1,
                // existing a generic
                Some('>') => depth -= 1,
                // any other character should be part of a valid symbol
                Some(..) => {}
            }

            self.offset += 1;
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

    fn expr_inner(&mut self) -> Result<Expr, Error> {
        self.descent()?;
        self.consume_whitespace();

        match self.number() {
            Ok(num) => {
                self.consume_whitespace();

                if let Ok(op) = self.operator() {
                    self.consume_whitespace();

                    self.ascent();
                    return Ok(Expr::Compound {
                        lhs: Box::new(Expr::Number(num)),
                        op,
                        rhs: Box::new(self.expr_inner()?),
                    });
                }

                self.ascent();
                return Ok(Expr::Number(num));
            }
            Err(err) if self.is_failing => return Err(err),
            Err(..) => {}
        }

        let sym = self.symbol()?;
        if !sym.is_empty() {
            self.consume_whitespace();

            let (addr, function) = match self.index.get_by_name(sym) {
                Some(got) => got,
                None => {
                    return Err(Error {
                        msg: format!("Function '{sym}' isn't known"),
                        offset: None,
                    })
                }
            };

            if let Ok(op) = self.operator() {
                self.consume_whitespace();

                self.ascent();
                return Ok(Expr::Compound {
                    lhs: Box::new(Expr::Symbol { addr, function }),
                    op,
                    rhs: Box::new(self.expr_inner()?),
                });
            }

            self.ascent();
            return Ok(Expr::Symbol { addr, function });
        }

        if let Ok(()) = self.consume('(') {
            let expr = self.expr_inner()?;
            self.consume(')')?;
            self.consume_whitespace();

            if let Ok(op) = self.operator() {
                self.consume_whitespace();

                self.ascent();
                return Ok(Expr::Compound {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(self.expr_inner()?),
                });
            }

            self.ascent();
            return Ok(expr);
        }

        self.error("Invalid expression")
    }

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

#[derive(Debug, PartialEq)]
enum Expr {
    Number(isize),
    Symbol {
        addr: usize,
        function: symbols::Function,
    },
    Compound {
        lhs: Box<Self>,
        op: Operator,
        rhs: Box<Self>,
    },
}

impl Expr {
    /// Evaluate the address of a given expression.
    ///
    /// Returns [`None`] if the expression overflows.
    fn eval(&self) -> Option<isize> {
        match self {
            Self::Number(n) => Some(*n),
            Self::Symbol { addr, .. } => Some(*addr as isize),
            Self::Compound { lhs, op, rhs } => match op {
                Operator::Add => lhs.eval()?.checked_add(rhs.eval()?),
                Operator::Min => lhs.eval()?.checked_sub(rhs.eval()?),
                Operator::Mul => lhs.eval()?.checked_mul(rhs.eval()?),
                Operator::Div => lhs.eval()?.checked_div(rhs.eval()?),
                Operator::Mod => lhs.eval()?.checked_rem(rhs.eval()?),
            },
        }
    }
}

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
        Ok(e) => match e.eval() {
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
        ast_eq!(
            "3 * 32",
            Expr::Compound {
                lhs: Box::new(Expr::Number(3)),
                op: Operator::Mul,
                rhs: Box::new(Expr::Number(32))
            }
        );
    }

    #[test]
    fn path() {
        eval_eq!(["abc::f"; 0x1234], "abc::f", 0x1234);
        ast_eq!(
            ["abc::f"; 0x1234],
            "abc::f",
            Expr::Symbol {
                addr: 0x1234,
                function: Function::new(TokenStream::simple("abc::f"), None)
            }
        );

        eval_eq!(
            ["abc::f<std::fmt::Display>"; 0x1234],
            "abc::f<std::fmt::Display>",
            0x1234
        );
        ast_eq!(
            ["abc::f<std::fmt::Display>"; 0x1234],
            "abc::f<std::fmt::Display>",
            Expr::Symbol {
                addr: 0x1234,
                function: Function::new(TokenStream::simple("abc::f<std::fmt::Display>"), None)
            }
        )
    }

    #[test]
    fn compound() {
        eval_eq!(["abc::f"; 0x100], "abc::f * 0x10", 0x1000);
        ast_eq!(
            ["abc::f"; 0x100],
            "abc::f * 0x10",
            Expr::Compound {
                lhs: Box::new(Expr::Symbol {
                    addr: 0x100,
                    function: Function::new(TokenStream::simple("abc::f"), None)
                }),
                op: Operator::Mul,
                rhs: Box::new(Expr::Number(0x10))
            }
        );

        eval_eq!(
            ["abc::f<std::fmt::Display>"; 0x100],
            "(abc::f<std::fmt::Display> + 10) / 12",
            22
        );
        ast_eq!(
            ["abc::f<std::fmt::Display>"; 0x100],
            "(abc::f<std::fmt::Display> + 10) / 12",
            Expr::Compound {
                lhs: Box::new(Expr::Compound {
                    lhs: Box::new(Expr::Symbol {
                        addr: 0x100,
                        function: Function::new(
                            TokenStream::simple("abc::f<std::fmt::Display>"),
                            None
                        )
                    }),
                    op: Operator::Add,
                    rhs: Box::new(Expr::Number(10))
                }),
                op: Operator::Div,
                rhs: Box::new(Expr::Number(12))
            }
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
}
