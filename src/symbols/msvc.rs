//! Microsoft Visual Studio symbol demangler
//!
//! ```text
//! <mangled-name> = ? <path> <type-encoding>
//!
//! <path> = <unscoped-path> {[<named-scope>]+ | [<nested-path>]}? @
//!
//! <unqualified-path> = <operator-name>
//!                    | <ctor-dtor-name>
//!                    | <source-name>
//!                    | <template-name>
//!
//! <operator> = ???
//!            | ?B // cast, the target type is encoded as the return type.
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

use std::borrow::Cow;

use super::TokenStream;
use crate::colors::Color;

/// Max recursion depth
const MAX_DEPTH: usize = 256;

pub fn parse(s: &str) -> Option<TokenStream> {
    let mut parser = Ast::new(s);

    // llvm appears to generate a '.' prefix on some symbols
    parser.eat(b'.');

    let sym = dbg!(parser.parse())?;
    Formatter::fmt(&mut parser.stream, sym);

    Some(parser.stream)
}

type Parameters<'a> = Vec<Type<'a>>;

/// Root node of the AST.
#[derive(Default, PartialEq, Debug, Clone)]
enum Type<'src> {
    /// ``` "" ```
    #[default]
    Unit,

    /// ``` {<modifier>} nullptr ```
    Nullptr,

    /// ``` {<modifier>} void ```
    Void(Modifiers),

    /// ``` {<modifier>} bool ```
    Bool(Modifiers),

    /// ``` {<modifier>} char ```
    Char(Modifiers),

    /// ``` {<modifier>} utf8 char ```
    Char8(Modifiers),

    /// ``` {<modifier>} utf16 char ```
    Char16(Modifiers),

    /// ``` {<modifier>} utf32 char ```
    Char32(Modifiers),

    /// ``` {<modifier>} signed char ```
    IChar(Modifiers),

    /// ``` {<modifier>} unsigned char ```
    UChar(Modifiers),

    /// ``` {<modifier>} utf16 char ```
    Wchar(Modifiers),

    /// ``` {<modifier>} i16 ```
    IShort(Modifiers),

    /// ``` {<modifier>} u16 ```
    UShort(Modifiers),

    /// ``` {<modifier>} int ```
    Int(Modifiers),

    /// ``` {<modifier>} unsigned ```
    Uint(Modifiers),

    /// ``` {<modifier>} float ```
    Float(Modifiers),

    /// ``` {<modifier>} double ```
    Double(Modifiers),

    /// ``` {<modifier>} long double ```
    LDouble(Modifiers),

    /// ``` {<modifier>} long ```
    Long(Modifiers),

    /// ``` {<modifier>} unsigned long ```
    Ulong(Modifiers),

    /// ``` {<modifier>} i64 ```
    I64(Modifiers),

    /// ``` {<modifier>} u64 ```
    U64(Modifiers),

    /// ``` {<modifier>} i128 ```
    I128(Modifiers),

    /// ``` {<modifier>} u128 ```
    U128(Modifiers),

    /// ``` {<modifier>} union <path> ```
    Union(Modifiers, Ident<'src>),

    /// ``` {<modifier>} enum <path> ```
    Enum(Modifiers, Ident<'src>),

    /// ``` {<modifier>} struct <path> ```
    Struct(Modifiers, Ident<'src>),

    /// ``` {<modifier>} class <path> ```
    Class(Modifiers, Ident<'src>),

    /// ``` {<modifier>} & <type> ```
    Ref(Modifiers, Box<Type<'src>>),

    /// ``` {<modifier>} && <type> ```
    RValueRef(Modifiers, Box<Type<'src>>),

    /// ``` {<modifier>} * <path> ```
    Ptr(Modifiers, Box<Type<'src>>),

    /// ``` <calling-conv> {<modifier>} <return-type> ({<type>}) ```
    Function(CallingConv, Modifiers, Box<Type<'src>>, Parameters<'src>),

    /// ``` <scope>: <calling-conv> {<qualifier>} <return-type> ({<type>}) ```
    MemberFunction(
        Scope,
        CallingConv,
        Modifiers,
        Box<Type<'src>>,
        Parameters<'src>,
    ),

    /// ``` <scope>: <class> <calling-conv> {<qualifier>} <return-type> ({<type>}) ```
    MemberFunctionPtr(
        Scope,
        Ident<'src>,
        CallingConv,
        Modifiers,
        Box<Type<'src>>,
        Parameters<'src>,
    ),

    /// ``` <number> ```
    Constant(isize),

    /// ``` ??? ```
    TemplateParameterIdx(isize),

    /// ``` {<modifier>} <ident> ```
    Ident(Modifiers, Ident<'src>),
}

/// Either a well known operator of a class or some C++ internal operator implementation.
#[derive(Debug, PartialEq, Clone)]
enum Operator<'src> {
    /// Constructor
    Ctor,

    /// Destructor
    Dtor,

    /// operator new
    New,

    /// operator new[]
    NewArray,

    /// operator delete
    Delete,

    /// operator delete[]
    DeleteArray,

    /// operator<<
    ShiftLeft,

    /// operator<<=
    ShiftLeftEquals,

    /// operator>>
    ShiftRight,

    /// operator>>=
    ShiftRightEquals,

    /// operator[]
    Array,

    /// operator->
    Pointer,

    /// operator*
    Dereference,

    /// operator->*
    MemberDereference,

    /// operator++
    Increment,

    /// operator--
    Decrement,

    /// operator*=
    TimesEquals,

    /// operator-
    Minus,

    /// operator-=
    MinusEquals,

    /// operator+
    Plus,

    /// operator+=
    PlusEquals,

    /// operator/
    Divide,

    /// operator/=
    DivideEquals,

    /// operator%
    Modulus,

    /// operator%=
    ModulusEquals,

    /// operator^
    Xor,

    /// operator^=
    XorEquals,

    /// operator&
    ArithmeticAND,

    /// operator&=
    ANDEquals,

    /// operator|
    ArithmeticOR,

    /// operator|=
    OREquals,

    /// operator~
    ArithmeticNot,

    /// operator&&
    LogicalAND,

    /// operator||
    LogicalOR,

    /// operator!
    LogicalNot,

    /// operator=
    Assign,

    /// operator==
    Equals,

    /// operator!=
    NotEquals,

    /// operator<
    Less,

    /// operator<=
    LessEqual,

    /// operator>
    Greater,

    /// operator>=
    GreaterEqual,

    /// operator,
    Comma,

    /// operator()
    Calling,

    /// operator<=>
    Spaceship,

    /// operator co_await (doesn't match any other operator syntax, C++ at it again) :(
    CoAwait,

    /// vbase destructor
    VBaseDtor,

    /// vector deleting destructor
    VectorDeletingDtor,

    /// default constructor closure
    DefaultCtorClosure,

    /// scalar deleting destructor
    ScalarDelDtor,

    /// vector constructor iterator
    VecCtorIter,

    /// vector destructor iterator
    VecDtorIter,

    /// vector vbase constructor iterator
    VecVbaseCtorIter,

    /// virtual displacement map
    VdispMap,

    /// eh vector constructor iterator
    EHVecCtorIter,

    /// eh vector destructor iterator
    EHVecDtorIter,

    /// eh vector vbase constructor iterator
    EHVecVbaseCtorIter,

    /// copy constructor closure
    CopyCtorClosure,

    /// local vftable constructor closure
    LocalVftableCtorClosure,

    /// Source name???
    SourceName(Cow<'src, str>),
}

/// Calling conventions supported by MSVC
#[derive(Debug, PartialEq, Clone, Copy)]
enum CallingConv {
    Cdecl,
    Pascal,
    Thiscall,
    Stdcall,
    Fastcall,
    Clrcall,
    Eabi,
    Vectorcall,
}

bitflags::bitflags! {
    #[derive(Debug, PartialEq, Clone, Copy)]
    struct Scope: u32 {
        const PUBLIC     = 0b000000001;
        const PRIVATE    = 0b000000010;
        const PROTECTED  = 0b000000100;
        const GLOBAL     = 0b000001000;
        const STATIC     = 0b000010000;
        const VIRTUAL    = 0b000100000;
        const FAR        = 0b001000000;
        const THUNK      = 0b010000000;
        const ADJUST     = 0b100000000;
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    struct Modifiers: u32 {
        /// const ..
        const CONST     = 0b00000001;

        /// volatile ..
        const VOLATILE  = 0b00000010;

        /// __far ..
        const FAR       = 0b00000100;

        /// __ptr64 ..
        const PTR64     = 0b00001000;

        /// __unaligned ..
        const UNALIGNED = 0b00010000;

        /// restrict ..
        const RESTRICT  = 0b00100000;

        /// & ..
        const LVALUE    = 0b01000000;

        /// && ..
        const RVALUE    = 0b10000000;
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Ident<'src> {
    Literal(Cow<'src, str>),
    Interface(Cow<'src, str>),
    Template(Box<Ident<'src>>, Parameters<'src>),
    Operator(Operator<'src>),
    Sequence(Vec<Ident<'src>>),
    Nested(Box<Symbol<'src>>),
    Disambiguator(isize),
}

#[derive(Debug, PartialEq, Clone)]
struct Symbol<'src> {
    root: Ident<'src>,
    tipe: Type<'src>,
}

impl<'src> From<Ident<'src>> for Symbol<'src> {
    #[inline]
    fn from(root: Ident<'src>) -> Symbol<'src> {
        Symbol {
            root,
            tipe: Type::Unit,
        }
    }
}

#[derive(Default)]
struct Backrefs<'src> {
    /// Up to 10 idents can be memorized for lookup using backref's: ?0, ?1, ..
    memorized: [Cow<'src, str>; 10],

    /// Number of so far memorized idents.
    memorized_count: usize,

    /// A max of 10 function parameters is supported.
    params: [Type<'src>; 10],

    /// Number of so far encountered function parameters.
    param_count: usize,
}

struct Ast {
    stream: TokenStream,
    offset: usize,
    depth: usize,
    backrefs: Backrefs<'static>,
}

impl<'src> Ast {
    /// Create an initialized parser that hasn't started parsing yet.
    fn new(s: &str) -> Self {
        Self {
            stream: TokenStream::new(s),
            offset: 0,
            depth: 0,
            backrefs: Backrefs::default(),
        }
    }

    /// Create a reference to the underlying pinned string that holds the mangled symbol.
    #[inline]
    fn src(&self) -> &'src str {
        &self.stream.inner()[self.offset..]
    }

    #[inline]
    fn push(&mut self, text: &'static str, color: Color) {
        self.stream.push(text, color);
    }

    #[inline]
    fn recurse_deeper(&mut self) -> Option<()> {
        self.depth += 1;
        (self.depth < MAX_DEPTH).then_some(())
    }

    fn try_memorizing_ident(&mut self, ident: &Cow<'static, str>) {
        let memorized = &self.backrefs.memorized[..self.backrefs.memorized_count];

        if !memorized.contains(ident) && self.backrefs.memorized_count != 10 {
            self.backrefs.memorized[self.backrefs.memorized_count] = ident.clone();
            self.backrefs.memorized_count += 1;
        }
    }

    fn get_memorized_ident(&mut self, idx: usize) -> Option<Ident<'src>> {
        let memorized = &self.backrefs.memorized[..self.backrefs.memorized_count];
        let memorized = memorized.get(idx).cloned()?;

        Some(Ident::Literal(memorized))
    }

    fn try_memorizing_param(&mut self, tipe: &Type<'static>) {
        let memorized = &self.backrefs.params[..self.backrefs.param_count];

        if !memorized.contains(tipe) && self.backrefs.param_count != 10 {
            self.backrefs.params[self.backrefs.param_count] = tipe.clone();
            self.backrefs.param_count += 1;
        }
    }

    fn get_memorized_param(&self, idx: usize) -> Option<Type<'src>> {
        let memorized = &self.backrefs.params[..self.backrefs.param_count];
        let memorized = memorized.get(idx).cloned()?;

        Some(memorized)
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

    /// Increment the offset if the current byte equals the byte given.
    fn eat(&mut self, byte: u8) -> bool {
        let matches = self.src().bytes().next() == Some(byte);
        self.offset += matches as usize;
        matches
    }

    /// Increment the offset if the slices match.
    fn eat_slice(&mut self, slice: &[u8]) -> bool {
        let matches = self.src().as_bytes().get(..slice.len()) == Some(slice);
        self.offset += slice.len() * (matches as usize);
        matches
    }

    fn base10(&mut self) -> Option<usize> {
        let n = match self.peek()? {
            c @ b'0'..=b'9' => (c - b'0') as usize,
            _ => return None,
        };

        self.offset += 1;
        Some(n)
    }

    fn base16(&mut self) -> Option<usize> {
        let n = match self.peek()? {
            c @ b'0'..=b'9' => (c - b'0') as usize,
            c @ b'a'..=b'f' => (c - b'a') as usize,
            c @ b'A'..=b'F' => (c - b'A') as usize,
            _ => return None,
        };

        self.offset += 1;
        Some(n)
    }

    fn number(&mut self) -> Option<isize> {
        let negative = self.eat(b'?');

        if let (Some(digit), None) | (None, Some(digit)) = (self.base10(), self.base16()) {
            let mut digit = digit as isize;

            if negative {
                digit = -digit;
            }
            return Some(digit);
        }

        let mut n = 0isize;
        loop {
            match self.peek()? {
                chr @ b'A'..=b'P' => {
                    self.offset += 1;

                    n = n.checked_mul(16)?;
                    n = n.checked_add((chr - b'A') as isize)?;
                }
                b'@' => {
                    self.offset += 1;

                    if negative {
                        n = -n;
                    }
                    return Some(n);
                }
                _ => return None,
            }
        }
    }

    fn hex_nibbles(&mut self) -> Option<&'src str> {
        let mut len = 0;

        while let Some(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F') = self.peek() {
            self.offset += 1;
            len += 1;
        }

        Some(&self.src()[..len])
    }

    fn md5(&mut self) -> Option<Ident<'src>> {
        let data = self.hex_nibbles()?;

        // the md5 string must be of length 32
        if data.len() != 32 {
            return None;
        }

        // md5 string must be terminated with a '@'
        if !self.eat(b'@') {
            return None;
        }

        Some(Ident::Literal(Cow::Borrowed(data)))
    }

    fn calling_conv(&mut self) -> Option<CallingConv> {
        let conv = match self.peek()? {
            b'A' | b'B' => CallingConv::Cdecl,
            b'C' | b'D' => CallingConv::Pascal,
            b'E' | b'F' => CallingConv::Fastcall,
            b'G' | b'H' => CallingConv::Stdcall,
            b'I' | b'J' => CallingConv::Thiscall,
            b'M' | b'N' => CallingConv::Clrcall,
            b'O' | b'P' => CallingConv::Eabi,
            b'Q' => CallingConv::Vectorcall,
            _ => return None,
        };

        self.offset += 1;
        Some(conv)
    }

    fn params(&mut self) -> Option<Parameters<'src>> {
        let mut params = Vec::new();

        loop {
            if let Some(b'@') | Some(b'Z') | None = self.peek() {
                break;
            }

            if let Some(digit) = self.base10() {
                params.push(self.get_memorized_param(digit)?);
                continue;
            }

            let start = self.src().len();
            let tipe = self.tipe(Modifiers::empty())?;
            let end = self.src().len();

            // single-letter types are ignored for backref's because
            // memorizing them doesn't save anything.
            if start - end > 1 {
                self.try_memorizing_param(&tipe);
            }

            params.push(tipe);
        }

        if !(self.eat(b'Z') || self.src().is_empty()) {
            self.consume(b'@')?;
        }

        Some(params)
    }

    fn function_qualifiers(&mut self) -> Modifiers {
        let mut qual = Modifiers::empty();

        if self.eat(b'E') {
            qual |= Modifiers::PTR64;
        }

        if self.eat(b'I') {
            qual |= Modifiers::RESTRICT;
        }

        if self.eat(b'F') {
            qual |= Modifiers::UNALIGNED;
        }

        if self.eat(b'G') {
            qual |= Modifiers::LVALUE;
        }

        if self.eat(b'H') {
            qual |= Modifiers::RVALUE;
        }

        let modi = self.modifiers();
        Modifiers::union(modi, qual)
    }

    /// ["X" <void>] <parameters> "Z"
    fn function_params(&mut self) -> Option<Parameters<'src>> {
        if self.eat(b'X') {
            return Some(vec![Type::Void(Modifiers::empty())]);
        }

        let params = self.params()?;
        self.consume(b'Z')?;
        Some(params)
    }

    /// <this-cvr-qualifiers> <calling-convention> <return-type> <argument-list> <throw-spec>
    fn function_tipe(&mut self, qualifiers: bool) -> Option<Type<'src>> {
        let mut quali = Modifiers::empty();
        let mut return_quali = Modifiers::empty();

        if qualifiers {
            quali |= self.function_qualifiers();
        }

        let conv = self.calling_conv()?;

        if self.eat(b'?') {
            return_quali |= self.modifiers();
        }

        let return_type = self.tipe(return_quali)?;
        let params = self.function_params()?;

        Some(Type::Function(conv, quali, Box::new(return_type), params))
    }

    fn function_scope(&mut self) -> Scope {
        let Some(scope) = self.take() else {
            return Scope::empty();
        };

        match scope {
            b'A' => Scope::PRIVATE,
            b'B' => Scope::PRIVATE | Scope::FAR,
            b'C' | b'D' => Scope::PRIVATE | Scope::STATIC,
            b'E' | b'F' => Scope::PRIVATE | Scope::VIRTUAL,
            b'G' => Scope::PRIVATE | Scope::ADJUST,
            b'H' => Scope::PRIVATE | Scope::ADJUST | Scope::FAR,
            b'I' => Scope::PROTECTED,
            b'J' => Scope::PROTECTED | Scope::FAR,
            b'K' => Scope::PROTECTED | Scope::STATIC,
            b'L' => Scope::PROTECTED | Scope::STATIC | Scope::FAR,
            b'M' => Scope::PROTECTED | Scope::VIRTUAL,
            b'N' => Scope::PROTECTED | Scope::ADJUST | Scope::FAR,
            b'O' => Scope::PROTECTED | Scope::ADJUST,
            b'P' => Scope::PROTECTED | Scope::ADJUST | Scope::FAR,
            b'Q' => Scope::PUBLIC,
            b'R' => Scope::PUBLIC | Scope::FAR,
            b'S' => Scope::PUBLIC | Scope::STATIC,
            b'T' => Scope::PUBLIC | Scope::STATIC | Scope::FAR,
            b'U' => Scope::PUBLIC | Scope::VIRTUAL,
            b'V' => Scope::PUBLIC | Scope::VIRTUAL | Scope::FAR,
            b'W' => Scope::PUBLIC | Scope::ADJUST,
            b'X' => Scope::PUBLIC | Scope::ADJUST | Scope::FAR,
            b'Y' => Scope::GLOBAL,
            b'Z' => Scope::GLOBAL | Scope::FAR,
            _ => Scope::empty(),
        }
    }

    /// ```
    /// <return-type> = <type>
    ///               | @      // structors (they have no declared return type)
    /// ```
    fn function_return_type(&mut self) -> Option<Type<'src>> {
        let modi = self.return_modifiers();

        if self.eat(b'@') {
            return Some(Type::Unit);
        }

        self.tipe(modi)
    }

    fn member_function_ptr_type(&mut self, qualifiers: bool) -> Option<Type<'src>> {
        let class = self.name(true)?;
        let mut quali = Modifiers::empty();
        let mut scope = Scope::empty();

        if self.eat(b'E') {
            quali |= Modifiers::PTR64;
        }

        if qualifiers {
            quali |= self.qualifiers();
        } else {
            scope |= self.function_scope();
        }

        let conv = self.calling_conv()?;
        let return_modi = self.return_modifiers();
        let return_type = self.function_return_type()?;
        let params = self.function_params()?;

        Some(Type::MemberFunctionPtr(
            scope,
            class,
            conv,
            return_modi,
            Box::new(return_type),
            params,
        ))
    }

    fn array(&mut self) -> Option<Type<'src>> {
        todo!()
    }

    fn pointee(&mut self) -> Option<Type<'src>> {
        let mut modi = Modifiers::empty();

        if self.eat(b'E') {
            modi |= Modifiers::PTR64;
        }

        modi |= self.modifiers();
        self.tipe(modi)
    }

    /// ```
    /// <variable-type> = <type> <cvr-qualifiers>
    ///                 | <type> <pointee-cvr-qualifiers> // pointers, references
    /// ```
    fn tipe(&mut self, mut modi: Modifiers) -> Option<Type<'src>> {
        self.recurse_deeper()?;

        match self.peek_slice(0..2)? {
            b"W4" => {
                self.offset += 2;
                self.depth -= 1;

                let name = self.name(false)?;
                return Some(Type::Enum(modi, name));
            }
            b"A6" => {
                self.offset += 2;
                self.depth -= 1;

                let fn_type = self.function_tipe(false)?;
                return Some(Type::Ref(modi, Box::new(fn_type)));
            }
            b"P6" => {
                self.offset += 2;
                self.depth -= 1;

                let fn_type = self.function_tipe(false)?;
                return Some(Type::Ptr(modi, Box::new(fn_type)));
            }
            b"P8" => {
                self.offset += 2;
                self.depth -= 1;

                return self.member_function_ptr_type(true);
            }
            _ => {}
        }

        if self.eat(b'$') {
            if self.eat(b'0') {
                self.depth -= 1;
                return self.number().map(Type::Constant);
            }

            if self.eat(b'D') {
                self.depth -= 1;
                return self.number().map(Type::TemplateParameterIdx);
            }

            if self.eat(b'$') {
                if self.eat(b'Y') {
                    self.depth -= 1;
                    let name = self.name(true)?;
                    return Some(Type::Ident(modi, name));
                }

                if self.eat(b'C') {
                    modi = self.qualifiers();
                }

                if self.eat(b'T') {
                    self.depth -= 1;
                    return Some(Type::Nullptr);
                }

                if self.eat(b'Q') {
                    self.depth -= 1;

                    let fn_type = self.function_tipe(false)?;
                    return Some(Type::RValueRef(modi, Box::new(fn_type)));
                }

                if self.eat_slice(b"BY") {
                    self.depth -= 1;
                    return self.array();
                }

                if self.eat_slice(b"A6") {
                    self.depth -= 1;
                    return self.function_tipe(false);
                }

                if self.eat_slice(b"A8@@") {
                    self.depth -= 1;
                    return self.function_tipe(true);
                }
            }

            if self.eat(b'S')
                || self.eat_slice(b"$V")
                || self.eat_slice(b"$Z")
                || self.eat_slice(b"$$V")
            {
                self.depth -= 1;
                return Some(Type::Unit);
            }

            if let Some(b'1' | b'H' | b'I' | b'J') = self.take() {
                self.depth -= 1;
                self.consume(b'?')?;
                return self.member_function_ptr_type(false);
            }
        }

        if self.eat(b'?') {
            self.depth -= 1;
            let idx = self.number()?;
            return Some(Type::TemplateParameterIdx(-idx));
        }

        if let Some(digit) = self.base10() {
            self.depth -= 1;
            return self.get_memorized_param(digit);
        }

        let tipe = match self.take()? {
            b'T' => Type::Union(modi, self.name(false)?),
            b'U' => Type::Struct(modi, self.name(false)?),
            b'V' => Type::Class(modi, self.name(false)?),
            b'A' => Type::Ref(modi, Box::new(self.pointee()?)),
            b'B' => Type::Ref(Modifiers::VOLATILE, Box::new(self.pointee()?)),
            b'P' => Type::Ptr(modi, Box::new(self.pointee()?)),
            b'Q' => Type::Ptr(Modifiers::CONST, Box::new(self.pointee()?)),
            b'R' => Type::Ptr(Modifiers::VOLATILE, Box::new(self.pointee()?)),
            b'S' => Type::Ptr(
                Modifiers::CONST | Modifiers::VOLATILE,
                Box::new(self.pointee()?),
            ),
            b'Y' => self.array()?,
            b'X' => Type::Void(modi),
            b'D' => Type::Char(modi),
            b'C' => Type::IChar(modi),
            b'E' => Type::UChar(modi),
            b'F' => Type::IShort(modi),
            b'G' => Type::UShort(modi),
            b'H' => Type::Int(modi),
            b'I' => Type::Uint(modi),
            b'J' => Type::Long(modi),
            b'K' => Type::Ulong(modi),
            b'M' => Type::Float(modi),
            b'N' => Type::Double(modi),
            b'O' => Type::LDouble(modi),
            b'_' => match self.take()? {
                b'N' => Type::Bool(modi),
                b'J' => Type::I64(modi),
                b'K' => Type::U64(modi),
                b'L' => Type::I128(modi),
                b'M' => Type::U128(modi),
                b'W' => Type::Wchar(modi),
                b'Q' => Type::Char8(modi),
                b'S' => Type::Char16(modi),
                b'U' => Type::Char32(modi),
                _ => return None,
            },
            _ => return None,
        };

        self.depth -= 1;
        Some(tipe)
    }

    /// ```
    /// | <const>
    /// | <volatile>
    /// | <const> <volatile>
    /// | nothing
    /// ```
    fn qualifiers(&mut self) -> Modifiers {
        let quali = match self.peek() {
            Some(b'B' | b'R') => Modifiers::CONST,
            Some(b'C' | b'S') => Modifiers::VOLATILE,
            Some(b'D' | b'T') => Modifiers::CONST | Modifiers::VOLATILE,
            Some(b'A' | b'Q') => Modifiers::empty(),
            _ => return Modifiers::empty(),
        };

        self.offset += 1;
        quali
    }

    /// ```
    /// = <far> <const>
    /// | <far> <volatile>
    /// | <const>
    /// | <volatile>
    /// | <const> <volatile>
    /// | nothing
    /// ```
    fn modifiers(&mut self) -> Modifiers {
        let modi = match self.peek() {
            Some(b'F') => Modifiers::FAR | Modifiers::CONST,
            Some(b'G') => Modifiers::FAR | Modifiers::VOLATILE,
            Some(b'H') => Modifiers::FAR | Modifiers::VOLATILE | Modifiers::CONST,
            Some(b'B' | b'R') => Modifiers::CONST,
            Some(b'C' | b'S') => Modifiers::VOLATILE,
            Some(b'D' | b'T') => Modifiers::CONST | Modifiers::VOLATILE,
            Some(b'A' | b'Q') => Modifiers::empty(),
            _ => return Modifiers::empty(),
        };

        self.offset += 1;
        modi
    }

    /// ```
    /// = <const>
    /// | <volatile>
    /// | <const> <volatile>
    /// | nothing
    /// ```
    fn return_modifiers(&mut self) -> Modifiers {
        if !self.eat(b'?') {
            return Modifiers::empty();
        }

        let modi = match self.peek() {
            Some(b'B') => Modifiers::CONST,
            Some(b'C') => Modifiers::VOLATILE,
            Some(b'D') => Modifiers::CONST | Modifiers::VOLATILE,
            _ => return Modifiers::empty(),
        };

        self.offset += 1;
        modi
    }

    /// ```
    /// = <operator-name>
    /// | <ctor-dtor-name>
    /// | <source-name>
    /// | <template-name>
    /// ```
    fn operator(&mut self) -> Option<Operator<'src>> {
        // TODO: this doesn't handle all cases
        let op = match self.take()? {
            b'0' => Operator::Ctor,
            b'1' => Operator::Dtor,
            b'2' => Operator::New,
            b'3' => Operator::Delete,
            b'4' => Operator::Assign,
            b'5' => Operator::ShiftRight,
            b'6' => Operator::ShiftLeft,
            b'7' => Operator::LogicalNot,
            b'8' => Operator::Equals,
            b'9' => Operator::NotEquals,
            b'A' => Operator::Array,
            b'C' => Operator::Pointer,
            b'D' => Operator::Dereference,
            b'E' => Operator::Increment,
            b'F' => Operator::Decrement,
            b'G' => Operator::Minus,
            b'H' => Operator::Plus,
            b'I' => Operator::ArithmeticAND,
            b'J' => Operator::MemberDereference,
            b'K' => Operator::Divide,
            b'L' => Operator::Modulus,
            b'M' => Operator::Less,
            b'N' => Operator::LessEqual,
            b'O' => Operator::Greater,
            b'P' => Operator::GreaterEqual,
            b'Q' => Operator::Comma,
            b'R' => Operator::Calling,
            b'S' => Operator::ArithmeticNot,
            b'T' => Operator::Xor,
            b'U' => Operator::ArithmeticOR,
            b'V' => Operator::LogicalAND,
            b'W' => Operator::LogicalOR,
            b'X' => Operator::TimesEquals,
            b'Y' => Operator::PlusEquals,
            b'Z' => Operator::MinusEquals,
            b'_' => match self.take()? {
                b'0' => Operator::DivideEquals,
                b'1' => Operator::ModulusEquals,
                b'2' => Operator::ShiftRightEquals,
                b'3' => Operator::ShiftLeftEquals,
                b'4' => Operator::ANDEquals,
                b'5' => Operator::OREquals,
                b'6' => Operator::XorEquals,
                b'D' => Operator::VBaseDtor,
                b'E' => Operator::VectorDeletingDtor,
                b'F' => Operator::DefaultCtorClosure,
                b'G' => Operator::ScalarDelDtor,
                b'H' => Operator::VecCtorIter,
                b'I' => Operator::VecDtorIter,
                b'J' => Operator::VecVbaseCtorIter,
                b'K' => Operator::VdispMap,
                b'L' => Operator::EHVecCtorIter,
                b'M' => Operator::EHVecDtorIter,
                b'N' => Operator::EHVecVbaseCtorIter,
                b'O' => Operator::CopyCtorClosure,
                b'T' => Operator::LocalVftableCtorClosure,
                b'U' => Operator::NewArray,
                b'V' => Operator::DeleteArray,
                b'_' => match self.take()? {
                    b'L' => Operator::CoAwait,
                    b'M' => Operator::Spaceship,
                    b'K' => return self.ident().map(Cow::Borrowed).map(Operator::SourceName),
                    _ => return None,
                },
                _ => return None,
            },
            _ => return None,
        };

        Some(op)
    }

    fn ident(&mut self) -> Option<&'src str> {
        let len = self.src().bytes().position(|c| c == b'@')?;
        let ident = &self.src()[..len];
        self.offset += len + 1;
        Some(ident)
    }

    /// Doesn't return anything as there is no easy way to decode this.
    fn encoded_ident(&mut self) -> Option<Type<'src>> {
        let width = self.base10()?;
        if width > 2 {
            return None;
        }

        let len = std::cmp::min(self.number()? as usize, width * 32);
        self.number()?;

        for _ in 0..len {
            let chr = self.take()?;

            if let b'0'..=b'9' | b'a'..=b'z' | b'_' | b'$' = chr {
                continue;
            }

            if chr == b'?' {
                self.take()?;
                continue;
            }

            return None;
        }

        Some(Type::Unit)
    }

    #[inline]
    fn name(&mut self, memorize: bool) -> Option<Ident<'src>> {
        let ident = Cow::Borrowed(self.ident()?);
        if memorize {
            self.try_memorizing_ident(&ident);
        }
        Some(Ident::Literal(ident))
    }

    fn template(&mut self) -> Option<Ident<'src>> {
        self.recurse_deeper()?;

        let backrefs = std::mem::take(&mut self.backrefs);
        let name = self.unqualified_path()?;
        let params = self.params()?;
        self.backrefs = backrefs;

        self.depth -= 1;
        Some(Ident::Template(Box::new(name), params))
    }

    fn unqualified_path(&mut self) -> Option<Ident<'src>> {
        self.recurse_deeper()?;

        // memorized ident
        if let Some(digit) = self.base10() {
            self.depth -= 1;
            return self.get_memorized_ident(digit);
        }

        if self.eat(b'?') {
            if self.eat(b'$') {
                self.depth -= 1;
                return self.template();
            }

            self.depth -= 1;
            return self.operator().map(Ident::Operator);
        }

        self.depth -= 1;
        self.name(false)
    }

    fn nested_path(&mut self) -> Option<Ident<'src>> {
        self.recurse_deeper()?;

        if let Some(digit) = self.base10() {
            self.depth -= 1;
            return self.get_memorized_ident(digit);
        }

        if self.eat(b'?') {
            if self.eat(b'?') {
                self.depth -= 1;
                return self.parse().map(Box::new).map(Ident::Nested);
            }

            self.depth -= 1;
            return match self.peek()? {
                // templated nested path segment
                b'$' => {
                    self.offset += 1;

                    self.template().and_then(|ident| match ident {
                        Ident::Literal(ref literal) => {
                            self.try_memorizing_ident(literal);
                            Some(ident)
                        }
                        _ => None,
                    })
                }
                // anonymous namespace
                b'A' => {
                    self.offset += 1;

                    if let Some(b"0x") = self.peek_slice(0..2) {
                        let mut len = 0;

                        // skip over anonymous namespace disambiguator
                        while self.peek().filter(|c| c.is_ascii_hexdigit()).is_some() {
                            len += 1;
                            self.offset += 1;
                        }

                        self.try_memorizing_ident(&Cow::Borrowed(&self.src()[..len]));
                    }

                    self.consume(b'@')?;
                    Some(Ident::Literal(Cow::Borrowed("`anonymous namespace'")))
                }
                // interface
                b'Q' => {
                    self.offset += 1;

                    let ident = Cow::Borrowed(self.ident()?);
                    self.consume(b'@')?;
                    self.try_memorizing_ident(&ident);

                    Some(Ident::Interface(ident))
                }
                // disambiguator
                _ => {
                    let disambiguator = self.number()?;
                    Some(Ident::Disambiguator(disambiguator))
                }
            };
        }

        let ident = Cow::Borrowed(self.ident()?);
        self.try_memorizing_ident(&ident);

        self.depth -= 1;
        Some(Ident::Literal(ident))
    }

    fn path(&mut self) -> Option<Ident<'src>> {
        let mut full_path = Vec::new();
        let tail = self.unqualified_path()?;

        while !self.eat(b'@') {
            self.recurse_deeper()?;
            let segment = self.nested_path()?;

            match segment {
                Ident::Sequence(inner) => full_path.extend(inner),
                _ => full_path.push(segment),
            }

            self.depth -= 1;
        }

        full_path.push(tail);

        Some(Ident::Sequence(full_path))
    }

    /// ``` ? <name> <type-encoding> ```
    fn parse(&mut self) -> Option<Symbol<'src>> {
        self.recurse_deeper()?;
        self.consume(b'?')?;

        // unparseable MD5 encoded symbol
        if self.eat_slice(b"?@") {
            self.depth -= 1;
            return self.md5().map(Ident::into);
        }

        // scoped template instantiation?
        if self.eat_slice(b"$TSS") {
            let mut n = 0usize;

            while !self.eat(b'@') {
                let digit = self.base10()?;

                n = n.checked_mul(10)?;
                n = n.checked_add(digit)?;
            }

            self.depth -= 1;
            todo!("TODO: return thread safe static guard")
        }

        // any other template instantiation
        if self.eat(b'$') {
            self.depth -= 1;
            return self.template().map(Ident::into);
        }

        let root = self.path()?;
        let prefix = self.peek();
        self.offset += 1;

        // either no type of a C style type
        if let None | Some(b'9') = prefix {
            self.depth -= 1;
            return Some(root).map(Ident::into);
        }

        let tipe = match prefix? {
            b'0'..=b'4' => {
                let tipe = self.tipe(Modifiers::empty())?;
                self.modifiers();
                tipe
            }
            b'Y' => {
                let conv = self.calling_conv()?;
                let modi = self.modifiers();
                let return_modi = self.return_modifiers();
                let return_tipe = self.tipe(return_modi)?;
                let params = self.function_params()?;

                Type::Function(conv, modi, Box::new(return_tipe), params)
            }
            b'_' => self.encoded_ident()?,
            _ => {
                let mut quali = Modifiers::empty();
                let scope = self.function_scope();

                if !scope.contains(Scope::STATIC) {
                    quali |= self.function_qualifiers();
                }

                let conv = self.calling_conv()?;
                let return_type = self.function_return_type()?;
                let params = self.function_params()?;

                Type::MemberFunction(scope, conv, quali, Box::new(return_type), params)
            }
        };

        self.depth -= 1;
        Some(Symbol { root, tipe })
    }
}

struct Formatter<'src> {
    stream: &'src mut TokenStream,
    sym: Symbol<'src>,
}

impl<'src> Formatter<'src> {
    fn fmt(stream: &'src mut TokenStream, sym: Symbol<'src>) {
        let mut this = Formatter { stream, sym };

        this.path();
    }

    fn path(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_ast() {
        let mut parser = Ast::new("?x@@YAXMH@Z");
        let tree = parser.parse().unwrap();

        assert_eq!(
            tree,
            Symbol {
                root: Ident::Sequence(vec![Ident::Literal(Cow::Borrowed("x"),),],),
                tipe: Type::Function(
                    CallingConv::Cdecl,
                    Modifiers::empty(),
                    Box::new(Type::Void(Modifiers::empty())),
                    vec![
                        Type::Float(Modifiers::empty()),
                        Type::Int(Modifiers::empty()),
                    ],
                ),
            }
        );
    }

    #[test]
    fn instance() {
        dbg!(Ast::new("?x@ns@@3PEAV?$klass@HH@1@EA").parse().unwrap());
    }
}
