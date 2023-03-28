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
mod context;
mod tests;

use std::fmt;

use super::TokenStream;
use crate::colors;
use context::{Backrefs, Context};

use bitflags::bitflags;

pub fn parse(s: &str) -> Option<TokenStream> {
    let mut parser = Context::new(s);
    let mut backrefs = Backrefs::default();

    // llvm appears to generate a '.' prefix on some symbols
    parser.eat(b'.');

    let sym = dbg!(Symbol::parse(&mut parser, &mut backrefs)?);
    sym.demangle(&mut parser, &mut backrefs);

    Some(parser.stream)
}

trait Format<'a> {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs);
}

trait Parse: Sized {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self>;
}

/// Root node of the AST.
#[derive(Default, Debug, Clone, PartialEq)]
enum Type {
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
    WChar(Modifiers),

    /// ``` {<modifier>} i16 ```
    IShort(Modifiers),

    /// ``` {<modifier>} u16 ```
    UShort(Modifiers),

    /// ``` {<modifier>} int ```
    Int(Modifiers),

    /// ``` {<modifier>} unsigned ```
    UInt(Modifiers),

    /// ``` {<modifier>} float ```
    Float(Modifiers),

    /// ``` {<modifier>} double ```
    Double(Modifiers),

    /// ``` {<modifier>} long double ```
    LDouble(Modifiers),

    /// ``` {<modifier>} long ```
    Long(Modifiers),

    /// ``` {<modifier>} unsigned long ```
    ULong(Modifiers),

    /// ``` {<modifier>} int64_t ```
    I64(Modifiers),

    /// ``` {<modifier>} uint64_t ```
    U64(Modifiers),

    /// ``` {<modifier>} int128_t ```
    I128(Modifiers),

    /// ``` {<modifier>} uint128_t ```
    U128(Modifiers),

    /// ``` {<modifier>} union <ident> ```
    Union(Modifiers, Literal),

    /// ``` {<modifier>} enum <ident> ```
    Enum(Modifiers, Literal),

    /// ``` {<modifier>} struct <ident> ```
    Struct(Modifiers, Literal),

    /// ``` {<modifier>} class <ident> ```
    Class(Modifiers, Literal),

    /// ``` {<modifier>} & <type> ```
    Ref(Modifiers, Box<Pointee>),

    /// ``` {<modifier>} && <type> ```
    RValueRef(Modifiers, Box<Pointee>),

    /// ``` {<modifier>} * <type> ```
    Ptr(Modifiers, Box<Pointee>),

    /// ``` <calling-conv> {<modifier>} <return-type> ({<type>}) ```
    Function(Function),

    /// ``` <scope>: <calling-conv> {<qualifier>} <return-type> ({<type>}) ```
    MemberFunction(MemberFunction),

    /// ``` <scope>: <path> <calling-conv> {<qualifier>} <return-type> ({<type>}) ```
    MemberFunctionPtr(MemberFunctionPtr),

    /// ``` <number> ```
    Constant(isize),

    /// ``` ??? ```
    TemplateParameterIdx(isize),

    /// ``` {<modifier>} <path> ```
    Typedef(Modifiers, Literal),

    /// ``` <storage> {<modifier>} ```
    Variable(StorageVariable, Modifiers, Box<Type>),

    /// Index into the parameter substitution table.
    Indexed(usize),

    /// String encoded using a format we don't know.
    Encoded(EncodedIdent),

    /// ???
    Array(Array),
}

impl Parse for Type {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        match ctx.peek_slice(0..2)? {
            b"W4" => {
                ctx.offset += 2;
                ctx.memorizing = false;

                let name = Literal::parse(ctx, backrefs)?;
                return Some(Type::Enum(ctx.modifiers, name));
            }
            b"A6" => {
                ctx.offset += 2;
                ctx.parsing_qualifiers = false;

                let func = Function::parse(ctx, backrefs)
                    .map(Type::Function)
                    .map(Pointee)?;

                return Some(Type::Ref(ctx.modifiers, Box::new(func)));
            }
            b"P6" => {
                ctx.offset += 2;
                ctx.parsing_qualifiers = false;

                let func = Function::parse(ctx, backrefs)
                    .map(Type::Function)
                    .map(Pointee)?;

                return Some(Type::Ptr(ctx.modifiers, Box::new(func)));
            }
            b"P8" => {
                ctx.offset += 2;

                return MemberFunctionPtr::parse(ctx, backrefs).map(Type::MemberFunctionPtr);
            }
            _ => {}
        }

        if ctx.eat(b'$') {
            if ctx.eat(b'0') {
                return ctx.number().map(Type::Constant);
            }

            if ctx.eat(b'D') {
                return ctx.number().map(Type::TemplateParameterIdx);
            }

            if ctx.eat(b'$') {
                if ctx.eat(b'Y') {
                    ctx.memorizing = false;
                    let name = Literal::parse(ctx, backrefs)?;
                    return Some(Type::Typedef(ctx.modifiers, name));
                }

                if ctx.eat(b'T') {
                    return Some(Type::Nullptr);
                }

                if ctx.eat(b'Q') {
                    ctx.parsing_qualifiers = false;

                    let func = Function::parse(ctx, backrefs)
                        .map(Type::Function)
                        .map(Pointee)?;

                    return Some(Type::RValueRef(ctx.modifiers, Box::new(func)));
                }

                if ctx.eat_slice(b"BY") {
                    return Array::parse(ctx, backrefs).map(Type::Array);
                }

                if ctx.eat_slice(b"A6") {
                    ctx.parsing_qualifiers = false;
                    return Function::parse(ctx, backrefs).map(Type::Function);
                }

                if ctx.eat_slice(b"A8@@") {
                    ctx.parsing_qualifiers = true;
                    return Function::parse(ctx, backrefs).map(Type::Function);
                }

                if ctx.eat(b'V') || ctx.eat(b'Z') || ctx.eat_slice(b"$V") {
                    return Some(Type::Unit);
                }

                if ctx.eat(b'C') {
                    ctx.modifiers = Qualifiers::parse(ctx, backrefs)?.0;
                }
            }

            if ctx.eat(b'S') {
                return Some(Type::Unit);
            }

            if let Some(b'1' | b'H' | b'I' | b'J') = ctx.take() {
                ctx.consume(b'?')?;
                return MemberFunctionPtr::parse(ctx, backrefs).map(Type::MemberFunctionPtr);
            }
        }

        if ctx.eat(b'?') {
            let idx = ctx.number()?;
            return Some(Type::TemplateParameterIdx(-idx));
        }

        if let Some(digit) = ctx.base10() {
            return backrefs.get_memorized_param(digit);
        }

        ctx.memorizing = false;
        let tipe = match ctx.take()? {
            b'T' => Type::Union(ctx.modifiers, Literal::parse(ctx, backrefs)?),
            b'U' => Type::Struct(ctx.modifiers, Literal::parse(ctx, backrefs)?),
            b'V' => Type::Class(ctx.modifiers, Literal::parse(ctx, backrefs)?),
            b'A' => Type::Ref(ctx.modifiers, Box::new(Pointee::parse(ctx, backrefs)?)),
            b'B' => Type::Ref(
                Modifiers::VOLATILE,
                Box::new(Pointee::parse(ctx, backrefs)?),
            ),
            b'P' => Type::Ptr(ctx.modifiers, Box::new(Pointee::parse(ctx, backrefs)?)),
            b'Q' => Type::Ptr(Modifiers::CONST, Box::new(Pointee::parse(ctx, backrefs)?)),
            b'R' => Type::Ptr(
                Modifiers::VOLATILE,
                Box::new(Pointee::parse(ctx, backrefs)?),
            ),
            b'S' => Type::Ptr(
                Modifiers::CONST | Modifiers::VOLATILE,
                Box::new(Pointee::parse(ctx, backrefs)?),
            ),
            b'Y' => Type::Array(Array::parse(ctx, backrefs)?),
            b'X' => Type::Void(ctx.modifiers),
            b'D' => Type::Char(ctx.modifiers),
            b'C' => Type::IChar(ctx.modifiers),
            b'E' => Type::UChar(ctx.modifiers),
            b'F' => Type::IShort(ctx.modifiers),
            b'G' => Type::UShort(ctx.modifiers),
            b'H' => Type::Int(ctx.modifiers),
            b'I' => Type::UInt(ctx.modifiers),
            b'J' => Type::Long(ctx.modifiers),
            b'K' => Type::ULong(ctx.modifiers),
            b'M' => Type::Float(ctx.modifiers),
            b'N' => Type::Double(ctx.modifiers),
            b'O' => Type::LDouble(ctx.modifiers),
            b'_' => match ctx.take()? {
                b'N' => Type::Bool(ctx.modifiers),
                b'J' => Type::I64(ctx.modifiers),
                b'K' => Type::U64(ctx.modifiers),
                b'L' => Type::I128(ctx.modifiers),
                b'M' => Type::U128(ctx.modifiers),
                b'W' => Type::WChar(ctx.modifiers),
                b'Q' => Type::Char8(ctx.modifiers),
                b'S' => Type::Char16(ctx.modifiers),
                b'U' => Type::Char32(ctx.modifiers),
                _ => return None,
            },
            _ => return None,
        };

        Some(tipe)
    }
}

impl<'a> Format<'a> for Type {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        self.demangle_pre(ctx, backrefs);
        self.demangle_post(ctx, backrefs);
    }
}

/// Converts an AST to a string.
///
/// Converting an AST representing a C++ type to a string is tricky due
/// to the bad grammar of the C++ declaration inherited from C. You have
/// to construct a string from inside to outside. For example, if a type
/// X is a pointer to a function returning int, the order you create a
/// string becomes something like this:
///
///   (1) X is a pointer: *X
///   (2) (1) is a function returning int: int (*X)()
///
/// So you cannot construct a result just by appending strings to a result.
///
/// To deal with this, we split the function into two. demangle_pre() writes
/// the "first half" of type declaration, and demangle_post() writes the
/// "second half". For example, demangle_pre() writes a return type for a
/// function and demangle_post() writes an parameter list.
impl<'a, 'b> Type {
    fn demangle_pre(&'a self, ctx: &mut Context<'a>, backrefs: &'b mut Backrefs) {
        match self {
            Type::Unit => {}
            Type::Nullptr => ctx.stream.push("nullptr", colors::MAGENTA),
            Type::Void(modi) => {
                ctx.stream.push("void", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::Bool(modi) => {
                ctx.stream.push("bool", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::Char(modi) => {
                ctx.stream.push("char", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::Char8(modi) => {
                ctx.stream.push("char8", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::Char16(modi) => {
                ctx.stream.push("char16", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::Char32(modi) => {
                ctx.stream.push("char32", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::IChar(modi) => {
                ctx.stream.push("signed char", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::UChar(modi) => {
                ctx.stream.push("unsigned char", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::WChar(modi) => {
                ctx.stream.push("wchar", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::IShort(modi) => {
                ctx.stream.push("short", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::UShort(modi) => {
                ctx.stream.push("unsigned short", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::Int(modi) => {
                ctx.stream.push("int", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::UInt(modi) => {
                ctx.stream.push("unsigned int", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::Float(modi) => {
                ctx.stream.push("float", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::Double(modi) => {
                ctx.stream.push("double", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::LDouble(modi) => {
                ctx.stream.push("long double", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::Long(modi) => {
                ctx.stream.push("long", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::ULong(modi) => {
                ctx.stream.push("unsigned long", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::I64(modi) => {
                ctx.stream.push("int64_t", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::U64(modi) => {
                ctx.stream.push("uint64_t", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::I128(modi) => {
                ctx.stream.push("int128_t", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::U128(modi) => {
                ctx.stream.push("uint128_t", colors::MAGENTA);
                modi.demangle(ctx, backrefs);
            }
            Type::Union(modi, name) => {
                ctx.stream.push("union ", colors::MAGENTA);
                ctx.push_literal(backrefs, name, colors::PURPLE);
                modi.demangle(ctx, backrefs);
            }
            Type::Enum(modi, name) => {
                ctx.stream.push("enum ", colors::MAGENTA);
                ctx.push_literal(backrefs, name, colors::PURPLE);
                modi.demangle(ctx, backrefs);
            }
            Type::Struct(modi, name) => {
                ctx.stream.push("struct ", colors::MAGENTA);
                ctx.push_literal(backrefs, name, colors::PURPLE);
                modi.demangle(ctx, backrefs);
            }
            Type::Class(modi, name) => {
                ctx.stream.push("class ", colors::MAGENTA);
                ctx.push_literal(backrefs, name, colors::PURPLE);
                modi.demangle(ctx, backrefs);
            }
            Type::Ptr(modi, tipe) | Type::Ref(modi, tipe) | Type::RValueRef(modi, tipe) => {
                // "[]" and "()" (for function parameters) take precedence over "*",
                // so "int *x(int)" means "x is a function returning int *". We need
                // parentheses to supercede the default precedence. (e.g. we want to
                // emit something like "int (*x)(int)".)

                let tipe = &tipe.0;
                match tipe {
                    Type::Function(func) => {
                        func.return_type.0.demangle_pre(ctx, backrefs);
                        ctx.stream.push(" (", colors::GRAY40);
                        func.calling_conv.demangle(ctx, backrefs);
                    }
                    Type::MemberFunction(func) => {
                        func.return_type.0.demangle_pre(ctx, backrefs);
                        func.calling_conv.demangle(ctx, backrefs);
                    }
                    Type::MemberFunctionPtr(func) => {
                        func.return_type.0.demangle_pre(ctx, backrefs);
                        ctx.stream.push(" (", colors::GRAY40);
                        func.calling_conv.demangle(ctx, backrefs);
                    }
                    Type::Array(..) => {
                        tipe.demangle_pre(ctx, backrefs);
                        ctx.stream.push(" (", colors::GRAY40);
                    }
                    _ => tipe.demangle_pre(ctx, backrefs),
                }

                match self {
                    Type::Ptr(..) => ctx.stream.push(" *", colors::RED),
                    Type::Ref(..) => ctx.stream.push(" &", colors::RED),
                    Type::RValueRef(..) => ctx.stream.push(" &&", colors::RED),
                    _ => {}
                }

                modi.demangle(ctx, backrefs);
            }
            Type::Function(func) => {
                func.return_type.0.demangle_pre(ctx, backrefs);
                ctx.stream.push(" ", colors::WHITE);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push(" ", colors::WHITE);
            }
            Type::MemberFunction(func) => {
                func.storage_scope.demangle(ctx, backrefs);
                func.return_type.0.demangle_pre(ctx, backrefs);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push(" ", colors::WHITE);
            }
            Type::MemberFunctionPtr(func) => {
                func.return_type.0.demangle_pre(ctx, backrefs);
                ctx.stream.push(" (", colors::GRAY40);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push("  ", colors::WHITE);
                func.class_name.demangle(ctx, backrefs);
                ctx.stream.push("::*", colors::GRAY40);
            }
            Type::Constant(val) => {
                let val = std::borrow::Cow::Owned(val.to_string());
                ctx.stream.push_cow(val, colors::GRAY20);
            }
            Type::TemplateParameterIdx(_idx) => todo!(),
            Type::Typedef(modi, name) => {
                ctx.push_literal(backrefs, name, colors::PURPLE);
                modi.demangle(ctx, backrefs);
            }
            Type::Variable(storage, modi, tipe) => {
                storage.demangle(ctx, backrefs);
                tipe.demangle_pre(ctx, backrefs);
                modi.demangle(ctx, backrefs);
                ctx.stream.push(" ", colors::WHITE);
            }
            Type::Indexed(idx) => {
                let literal = backrefs.get_memorized_ident(*idx).unwrap();
                ctx.push_literal(backrefs, &literal, colors::PURPLE);
            }
            Type::Encoded(_) => {}
            Type::Array(_) => todo!(),
        }
    }

    fn demangle_post(&'a self, ctx: &mut Context<'a>, backrefs: &'b mut Backrefs) {
        match self {
            Type::Ptr(_, tipe) | Type::Ref(_, tipe) => {
                let tipe = &tipe.0;

                match tipe {
                    Type::Function(..) => ctx.stream.push(")", colors::GRAY40),
                    Type::MemberFunction(..) => ctx.stream.push(")", colors::GRAY40),
                    Type::MemberFunctionPtr(..) => ctx.stream.push(")", colors::GRAY40),
                    Type::Array(..) => ctx.stream.push(")", colors::GRAY40),
                    _ => {}
                }

                tipe.demangle_post(ctx, backrefs);
            }
            Type::Function(func) => {
                ctx.stream.push("(", colors::GRAY40);
                func.params.0.demangle(ctx, backrefs);
                ctx.stream.push(")", colors::GRAY40);

                func.qualifiers.0.demangle(ctx, backrefs);
                func.return_type.0.demangle_post(ctx, backrefs);
            }
            Type::MemberFunction(func) => {
                ctx.stream.push("(", colors::GRAY40);
                func.params.0.demangle(ctx, backrefs);
                ctx.stream.push(")", colors::GRAY40);

                func.qualifiers.0.demangle(ctx, backrefs);
                func.return_type.0.demangle_post(ctx, backrefs);
            }
            Type::MemberFunctionPtr(func) => {
                ctx.stream.push("(", colors::GRAY40);
                func.params.0.demangle(ctx, backrefs);
                ctx.stream.push(")", colors::GRAY40);

                func.qualifiers.0.demangle(ctx, backrefs);
                func.return_type.0.demangle_post(ctx, backrefs);
            }
            Type::Variable(_, _, tipe) => tipe.demangle_post(ctx, backrefs),
            Type::Array(..) => todo!(),
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Function {
    calling_conv: CallingConv,
    qualifiers: FunctionQualifiers,
    return_type: Box<FunctionReturnType>,
    params: FunctionParameters,
}

impl Parse for Function {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let calling_conv = CallingConv::parse(ctx, backrefs)?;
        let mut qualifiers = FunctionQualifiers(Modifiers::empty());

        if ctx.parsing_qualifiers {
            qualifiers = FunctionQualifiers::parse(ctx, backrefs)?;
        }

        if ctx.eat(b'?') {
            ctx.modifiers = Modifiers::parse(ctx, backrefs)?;
        }

        let return_type = FunctionReturnType::parse(ctx, backrefs)?;
        let params = FunctionParameters::parse(ctx, backrefs)?;

        Some(Function {
            calling_conv,
            qualifiers,
            return_type: Box::new(return_type),
            params,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
struct MemberFunction {
    storage_scope: StorageScope,
    calling_conv: CallingConv,
    qualifiers: FunctionQualifiers,
    return_type: Box<FunctionReturnType>,
    params: FunctionParameters,
}

impl Parse for MemberFunction {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let mut qualifiers = FunctionQualifiers(Modifiers::empty());
        let storage_scope = StorageScope::parse(ctx, backrefs)?;

        if !storage_scope.contains(StorageScope::STATIC) {
            qualifiers = FunctionQualifiers::parse(ctx, backrefs)?;
        }

        let calling_conv = CallingConv::parse(ctx, backrefs)?;
        ctx.modifiers = ReturnModifiers::parse(ctx, backrefs)?.0;
        let return_type = FunctionReturnType::parse(ctx, backrefs)?;
        let params = FunctionParameters::parse(ctx, backrefs)?;

        Some(MemberFunction {
            storage_scope,
            calling_conv,
            qualifiers,
            return_type: Box::new(return_type),
            params,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
struct MemberFunctionPtr {
    storage_scope: StorageScope,
    class_name: Path,
    calling_conv: CallingConv,
    qualifiers: FunctionQualifiers,
    return_type: Box<FunctionReturnType>,
    params: FunctionParameters,
}

impl Parse for MemberFunctionPtr {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let class_name = Path::parse(ctx, backrefs)?;
        let mut qualifiers = FunctionQualifiers(Modifiers::empty());
        let mut storage_scope = StorageScope::empty();

        if ctx.eat(b'E') {
            qualifiers = FunctionQualifiers(Modifiers::PTR64);
        }

        if ctx.parsing_qualifiers {
            qualifiers.0 |= FunctionQualifiers::parse(ctx, backrefs)?.0;
        } else {
            storage_scope = StorageScope::parse(ctx, backrefs)?;
        }

        let calling_conv = CallingConv::parse(ctx, backrefs)?;
        ctx.modifiers = ReturnModifiers::parse(ctx, backrefs)?.0;
        let return_type = FunctionReturnType::parse(ctx, backrefs)?;
        let params = FunctionParameters::parse(ctx, backrefs)?;

        Some(MemberFunctionPtr {
            storage_scope,
            class_name,
            calling_conv,
            qualifiers,
            return_type: Box::new(return_type),
            params,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Array(Vec<Type>);

impl Parse for Array {
    fn parse(_: &mut Context, _: &mut Backrefs) -> Option<Self> {
        todo!()
    }
}

impl<'a> Format<'a> for Array {
    fn demangle(&'a self, _: &mut Context<'a>, _: &mut Backrefs) {
        todo!()
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Pointee(Type);

impl Parse for Pointee {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        if ctx.eat(b'E') {
            ctx.modifiers |= Modifiers::PTR64;
        }

        let pointer_modi = Modifiers::parse(ctx, backrefs)?;

        ctx.modifiers |= pointer_modi;

        Type::parse(ctx, backrefs).map(Pointee)
    }
}

#[derive(Debug, PartialEq, Clone)]
struct FunctionReturnType(Type);

impl Parse for FunctionReturnType {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        ctx.modifiers = ReturnModifiers::parse(ctx, backrefs)?.0;

        if ctx.eat(b'@') {
            return Some(FunctionReturnType(Type::Unit));
        }

        Type::parse(ctx, backrefs).map(FunctionReturnType)
    }
}

/// Either a well known operator of a class or some C++ internal operator implementation.
#[derive(Debug, PartialEq, Clone)]
enum Operator {
    Ctor,
    Dtor,
    New,
    NewArray,
    Delete,
    DeleteArray,
    ShiftLeft,
    ShiftLeftEquals,
    ShiftRight,
    ShiftRightEquals,
    Array,
    Pointer,
    Dereference,
    MemberDereference,
    Increment,
    Decrement,
    TimesEquals,
    Minus,
    MinusEquals,
    Plus,
    PlusEquals,
    Divide,
    DivideEquals,
    Modulus,
    ModulusEquals,
    Xor,
    XorEquals,
    VFTable,
    VBTable,
    VCall,
    TypeOff,
    LocalStaticGuard,
    String,
    ArithmeticAND,
    ANDEquals,
    ArithmeticOR,
    OREquals,
    ArithmeticNot,
    LogicalAND,
    LogicalOR,
    LogicalNot,
    Assign,
    Equals,
    NotEquals,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Comma,
    Calling,
    DynamicInitializer,
    DynamicAtexitDtor,
    LocalStaticThreadGuard,
    Spaceship,
    CoAwait,
    VBaseDtor,
    VectorDeletingDtor,
    DefaultCtorClosure,
    ScalarDeletingDtor,
    VecCtorIter,
    VecDtorIter,
    VecVbaseCtorIter,
    VdispMap,
    EHVecCtorIter,
    EHVecDtorIter,
    EHVecVbaseCtorIter,
    CopyCtorClosure,
    LocalVFTable,
    LocalVftableCtorClosure,
    PlacementDeleteClosure,
    PlacementDeleteArrayClosure,
    SourceName(Literal),
}

impl Parse for Operator {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        let op = match ctx.take()? {
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
            b'_' => match ctx.take()? {
                b'0' => Operator::DivideEquals,
                b'1' => Operator::ModulusEquals,
                b'2' => Operator::ShiftRightEquals,
                b'3' => Operator::ShiftLeftEquals,
                b'4' => Operator::ANDEquals,
                b'5' => Operator::OREquals,
                b'6' => Operator::XorEquals,
                b'7' => Operator::VFTable,
                b'8' => Operator::VBTable,
                b'9' => Operator::VCall,
                b'A' => Operator::TypeOff,
                b'B' => Operator::LocalStaticGuard,
                b'C' => Operator::String,
                b'D' => Operator::VBaseDtor,
                b'E' => Operator::VectorDeletingDtor,
                b'F' => Operator::DefaultCtorClosure,
                b'G' => Operator::ScalarDeletingDtor,
                b'H' => Operator::VecCtorIter,
                b'I' => Operator::VecDtorIter,
                b'J' => Operator::VecVbaseCtorIter,
                b'K' => Operator::VdispMap,
                b'L' => Operator::EHVecCtorIter,
                b'M' => Operator::EHVecDtorIter,
                b'N' => Operator::EHVecVbaseCtorIter,
                b'O' => Operator::CopyCtorClosure,
                b'R' => todo!("RTTI"),
                b'S' => Operator::LocalVFTable,
                b'T' => Operator::LocalVftableCtorClosure,
                b'U' => Operator::NewArray,
                b'V' => Operator::DeleteArray,
                b'X' => Operator::PlacementDeleteClosure,
                b'Y' => Operator::PlacementDeleteArrayClosure,
                b'_' => match ctx.take()? {
                    b'L' => Operator::CoAwait,
                    b'E' => Operator::DynamicInitializer,
                    b'F' => Operator::DynamicAtexitDtor,
                    b'J' => Operator::LocalStaticThreadGuard,
                    b'M' => Operator::Spaceship,
                    b'K' => return ctx.ident().map(Operator::SourceName),
                    _ => return None,
                },
                _ => return None,
            },
            _ => return None,
        };

        Some(op)
    }
}

impl<'a> Format<'a> for Operator {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        let literal = match *self {
            Operator::Ctor => {
                let name = ctx.scope.get(0);

                return match name {
                    Some(NestedPath::Literal(l)) => ctx.push_literal(backrefs, l, colors::BLUE),
                    _ => ctx.stream.push("`unnamed constructor'", colors::GRAY20),
                };
            }
            Operator::Dtor => {
                let name = ctx.scope.get(0);

                ctx.stream.push("~", colors::MAGENTA);

                return match name {
                    Some(NestedPath::Literal(l)) => ctx.push_literal(backrefs, l, colors::BLUE),
                    _ => ctx.stream.push("`unnamed destructor'", colors::GRAY20),
                };
            }
            Operator::VBTable => {
                ctx.stream.push("`vbtable'{{for `", colors::BLUE);
                return;
            }
            Operator::SourceName(src) => {
                ctx.push_literal(backrefs, &src, colors::MAGENTA);
                return;
            }
            Operator::New => "operator new",
            Operator::Delete => "operator delete",
            Operator::Assign => "operator=",
            Operator::ShiftRight => "operator>>",
            Operator::ShiftRightEquals => "operator>>=",
            Operator::ShiftLeft => "operator<<",
            Operator::ShiftLeftEquals => "operator<<=",
            Operator::LogicalNot => "operator!",
            Operator::Equals => "operator=",
            Operator::NotEquals => "operator!=",
            Operator::Array => "operator[]",
            Operator::Pointer => "operator->",
            Operator::Dereference => "operator*",
            Operator::TimesEquals => "operator*=",
            Operator::MemberDereference => "operator->*",
            Operator::Increment => "operator++",
            Operator::Decrement => "operator--",
            Operator::Minus => "operator-",
            Operator::MinusEquals => "operator-=",
            Operator::Plus => "operator+",
            Operator::PlusEquals => "operator+=",
            Operator::ArithmeticAND => "operator&",
            Operator::ANDEquals => "operator&=",
            Operator::LogicalAND => "operator&&",
            Operator::ArithmeticOR => "operator|",
            Operator::OREquals => "operator|=",
            Operator::LogicalOR => "operator||",
            Operator::Divide => "operator/",
            Operator::DivideEquals => "operator/=",
            Operator::Modulus => "operator%",
            Operator::ModulusEquals => "operator%=",
            Operator::Less => "operator<",
            Operator::LessEqual => "operator<=",
            Operator::Greater => "operator>",
            Operator::GreaterEqual => "operator>=",
            Operator::Comma => "operator,",
            Operator::Calling => "operator()",
            Operator::ArithmeticNot => "operator~",
            Operator::Xor => "operator^",
            Operator::XorEquals => "operator^=",
            Operator::VFTable => "`vftable'",
            Operator::LocalVFTable => "`local vftable'",
            Operator::VCall => "`vcall'",
            Operator::TypeOff => "`typeoff'",
            Operator::LocalStaticGuard => "`local static guard'",
            Operator::String => "`string'",
            Operator::VBaseDtor => "`vbase destructor'",
            Operator::VectorDeletingDtor => "`vector deleting destructor'",
            Operator::DefaultCtorClosure => "`default constructor closure'",
            Operator::ScalarDeletingDtor => "`scalar deleting destructor'",
            Operator::VecCtorIter => "`vector constructor iterator'",
            Operator::VecDtorIter => "`vector destructor iterator'",
            Operator::VecVbaseCtorIter => "`vector vbase constructor iterator'",
            Operator::VdispMap => "`virtual displacement map'",
            Operator::EHVecCtorIter => "`eh vector constructor iterator'",
            Operator::EHVecDtorIter => "`eh vector destructor iterator'",
            Operator::EHVecVbaseCtorIter => "`eh vector vbase constructor iterator'",
            Operator::CopyCtorClosure => "`copy constructor closure'",
            Operator::LocalVftableCtorClosure => "`local vftable constructor closure'",
            Operator::DynamicInitializer => "`dynamic intializer'",
            Operator::DynamicAtexitDtor => "`dynamic atexit destructor'",
            Operator::LocalStaticThreadGuard => "`local static thread guard'",
            Operator::PlacementDeleteClosure => "`placement delete closure'",
            Operator::PlacementDeleteArrayClosure => "`placement delete[] closure'",
            Operator::NewArray => "operator new[]",
            Operator::DeleteArray => "operator delete[]",
            Operator::CoAwait => "co_await",
            Operator::Spaceship => "operator<=>",
        };

        ctx.stream.push(literal, colors::MAGENTA);
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Parameters(Vec<Type>);

impl Parse for Parameters {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let mut types = Vec::new();

        loop {
            if let Some(b'@') | Some(b'Z') | None = ctx.peek() {
                break;
            }

            if let Some(digit) = ctx.base10() {
                types.push(backrefs.get_memorized_param(digit)?);
                continue;
            }

            let start = ctx.src().len();
            let tipe = Type::parse(ctx, backrefs)?;
            let end = ctx.src().len();

            // single-letter types are ignored for backref's because
            // memorizing them doesn't save anything.
            if start - end > 1 {
                backrefs.try_memorizing_param(&tipe);
            }

            types.push(tipe);
        }

        if !(ctx.eat(b'Z') || ctx.src().is_empty()) {
            ctx.consume(b'@')?;
        }

        Some(Parameters(types))
    }
}

impl<'a> Format<'a> for Parameters {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        let mut params = self.0.iter();

        if let Some(param) = params.next() {
            param.demangle_pre(ctx, backrefs);
        }

        for param in params {
            ctx.stream.push(", ", colors::GRAY40);
            param.demangle(ctx, backrefs);
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct FunctionParameters(Parameters);

impl Parse for FunctionParameters {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        if ctx.eat(b'X') {
            let tipe = vec![Type::Void(Modifiers::empty())];
            return Some(FunctionParameters(Parameters(tipe)));
        }

        let params = Parameters::parse(ctx, backrefs)?;
        ctx.consume(b'Z')?;
        Some(FunctionParameters(params))
    }
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

impl Parse for CallingConv {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        let conv = match ctx.peek()? {
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

        ctx.offset += 1;
        Some(conv)
    }
}

impl<'a> Format<'a> for CallingConv {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        let literal = match self {
            CallingConv::Cdecl => "__cdecl",
            CallingConv::Pascal => "__pascal",
            CallingConv::Thiscall => "__thiscall",
            CallingConv::Stdcall => "__stdcall",
            CallingConv::Fastcall => "__fastcall",
            CallingConv::Clrcall => "__clrcall",
            CallingConv::Eabi => "__eabicall",
            CallingConv::Vectorcall => "__vectorcall",
        };

        ctx.stream.push(literal, colors::GRAY40);
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum StorageVariable {
    PrivateStatic,
    ProtectedStatic,
    PublicStatic,
    Global,
}

impl<'a> Format<'a> for StorageVariable {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        let literal = match self {
            StorageVariable::PrivateStatic => "private: static ",
            StorageVariable::ProtectedStatic => "protected: static ",
            StorageVariable::PublicStatic => "public: static ",
            StorageVariable::Global => return,
        };

        ctx.stream.push(literal, colors::PURPLE);
    }
}

bitflags! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    struct StorageScope: u32 {
        const PUBLIC    = 1;
        const PRIVATE   = 1 << 1;
        const PROTECTED = 1 << 2;
        const GLOBAL    = 1 << 3;
        const STATIC    = 1 << 4;
        const VIRTUAL   = 1 << 5;
        const FAR       = 1 << 6;
        const THUNK     = 1 << 7;
        const ADJUST    = 1 << 8;
    }
}

impl Parse for StorageScope {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        Some(match ctx.take()? {
            b'A' => StorageScope::PRIVATE,
            b'B' => StorageScope::PRIVATE | StorageScope::FAR,
            b'C' | b'D' => StorageScope::PRIVATE | StorageScope::STATIC,
            b'E' | b'F' => StorageScope::PRIVATE | StorageScope::VIRTUAL,
            b'G' => StorageScope::PRIVATE | StorageScope::ADJUST,
            b'H' => StorageScope::PRIVATE | StorageScope::ADJUST | StorageScope::FAR,
            b'I' => StorageScope::PROTECTED,
            b'J' => StorageScope::PROTECTED | StorageScope::FAR,
            b'K' => StorageScope::PROTECTED | StorageScope::STATIC,
            b'L' => StorageScope::PROTECTED | StorageScope::STATIC | StorageScope::FAR,
            b'M' => StorageScope::PROTECTED | StorageScope::VIRTUAL,
            b'N' => StorageScope::PROTECTED | StorageScope::ADJUST | StorageScope::FAR,
            b'O' => StorageScope::PROTECTED | StorageScope::ADJUST,
            b'P' => StorageScope::PROTECTED | StorageScope::ADJUST | StorageScope::FAR,
            b'Q' => StorageScope::PUBLIC,
            b'R' => StorageScope::PUBLIC | StorageScope::FAR,
            b'S' => StorageScope::PUBLIC | StorageScope::STATIC,
            b'T' => StorageScope::PUBLIC | StorageScope::STATIC | StorageScope::FAR,
            b'U' => StorageScope::PUBLIC | StorageScope::VIRTUAL,
            b'V' => StorageScope::PUBLIC | StorageScope::VIRTUAL | StorageScope::FAR,
            b'W' => StorageScope::PUBLIC | StorageScope::ADJUST,
            b'X' => StorageScope::PUBLIC | StorageScope::ADJUST | StorageScope::FAR,
            b'Y' => StorageScope::GLOBAL,
            b'Z' => StorageScope::GLOBAL | StorageScope::FAR,
            _ => return None,
        })
    }
}

impl<'a> Format<'a> for StorageScope {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        let literal = match *self {
            StorageScope::PUBLIC => "public: ",
            StorageScope::PRIVATE => "private: ",
            StorageScope::PROTECTED => "protected: ",
            StorageScope::GLOBAL => "global: ",
            StorageScope::STATIC => "static: ",
            StorageScope::VIRTUAL => "virtual: ",
            StorageScope::FAR => "[far]: ",
            StorageScope::THUNK => "[thunk]: ",
            _ => return,
        };

        ctx.stream.push(literal, colors::WHITE);
    }
}

bitflags! {
    #[derive(PartialEq, Eq, Clone, Copy)]
    struct Modifiers: u32 {
        /// const
        const CONST     = 1;

        /// volatile
        const VOLATILE  = 1 << 1;

        /// __far
        const FAR       = 1 << 2;

        /// __ptr64
        const PTR64     = 1 << 3;

        /// __unaligned
        const UNALIGNED = 1 << 4;

        /// restrict
        const RESTRICT  = 1 << 5;

        /// &
        const LVALUE    = 1 << 6;

        /// &&
        const RVALUE    = 1 << 7;
    }
}

impl Parse for Modifiers {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        let modi = match ctx.peek() {
            Some(b'F') => Modifiers::FAR | Modifiers::CONST,
            Some(b'G') => Modifiers::FAR | Modifiers::VOLATILE,
            Some(b'H') => Modifiers::FAR | Modifiers::VOLATILE | Modifiers::CONST,
            Some(b'B' | b'R') => Modifiers::CONST,
            Some(b'C' | b'S') => Modifiers::VOLATILE,
            Some(b'D' | b'T') => Modifiers::CONST | Modifiers::VOLATILE,
            _ => return Some(Modifiers::empty()),
        };

        ctx.offset += 1;
        Some(modi)
    }
}

impl<'a> Format<'a> for Modifiers {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        let color = colors::BLUE;

        if self.contains(Modifiers::CONST) {
            ctx.stream.push(" const", color);
        }

        if self.contains(Modifiers::VOLATILE) {
            ctx.stream.push(" volatile", color);
        }

        if self.contains(Modifiers::FAR) {
            ctx.stream.push(" __far", color);
        }

        if self.contains(Modifiers::UNALIGNED) {
            ctx.stream.push(" __unaligned", color);
        }

        if self.contains(Modifiers::RESTRICT) {
            ctx.stream.push(" restrict", color);
        }

        if self.contains(Modifiers::LVALUE) {
            ctx.stream.push(" &", color);
        }

        if self.contains(Modifiers::RVALUE) {
            ctx.stream.push(" &&", color);
        }
    }
}

impl fmt::Debug for Modifiers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let literal = match *self {
            Modifiers::CONST => "Modifiers::CONST",
            Modifiers::VOLATILE => "Modifiers::VOLATILE",
            Modifiers::FAR => "Modifiers::FAR",
            Modifiers::PTR64 => "Modifiers::PTR64",
            Modifiers::UNALIGNED => "Modifiers::UNALIGNED",
            Modifiers::RESTRICT => "Modifiers::RESTRICT",
            Modifiers::LVALUE => "Modifiers::LVALUE",
            Modifiers::RVALUE => "Modifiers::RVALUE",
            _ => "Modifiers::empty()",
        };

        f.write_str(literal)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ReturnModifiers(Modifiers);

impl Parse for ReturnModifiers {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        if !ctx.eat(b'?') {
            return Some(ReturnModifiers(Modifiers::empty()));
        }

        let modi = match ctx.take()? {
            b'A' => Modifiers::empty(),
            b'B' => Modifiers::CONST,
            b'C' => Modifiers::VOLATILE,
            b'D' => Modifiers::CONST | Modifiers::VOLATILE,
            _ => return None,
        };

        Some(ReturnModifiers(modi))
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Qualifiers(Modifiers);

impl Parse for Qualifiers {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        let quali = match ctx.peek() {
            Some(b'B' | b'R') => Modifiers::CONST,
            Some(b'C' | b'S') => Modifiers::VOLATILE,
            Some(b'D' | b'T') => Modifiers::CONST | Modifiers::VOLATILE,
            Some(b'A' | b'Q') => Modifiers::empty(),
            _ => return Some(Qualifiers(Modifiers::empty())),
        };

        ctx.offset += 1;
        Some(Qualifiers(quali))
    }
}

#[derive(Debug, Clone, PartialEq)]
struct FunctionQualifiers(Modifiers);

impl Parse for FunctionQualifiers {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let mut quali = Modifiers::empty();

        if ctx.eat(b'I') {
            quali |= Modifiers::RESTRICT;
        }

        if ctx.eat(b'F') {
            quali |= Modifiers::UNALIGNED;
        }

        if ctx.eat(b'G') {
            quali |= Modifiers::LVALUE;
        }

        if ctx.eat(b'H') {
            quali |= Modifiers::RVALUE;
        }

        let modi = Qualifiers::parse(ctx, backrefs)?.0;
        Some(FunctionQualifiers(Modifiers::union(modi, modi)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Literal {
    /// Index to a memorized string in [`Backrefs`].
    Indexed(usize),

    /// Borrowed string.
    Borrowed { start: usize, end: usize },
}

impl Parse for Literal {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let ident = ctx.ident()?;
        if ctx.memorizing {
            backrefs.try_memorizing_ident(&ident);
        }
        Some(ident)
    }
}

impl Literal {
    fn len(&self) -> usize {
        match self {
            Self::Indexed(..) => unreachable!(),
            Self::Borrowed { start, end } => end - start,
        }
    }
}

impl Default for Literal {
    fn default() -> Self {
        Self::Borrowed { start: 0, end: 0 }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct MD5(Literal);

impl Parse for MD5 {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        let data = ctx.hex_nibbles()?;

        // the md5 string must be of length 32
        if data.len() != 32 {
            return None;
        }

        // md5 string must be terminated with a '@'
        if !ctx.eat(b'@') {
            return None;
        }

        Some(Self(data))
    }
}

impl<'a> Format<'a> for MD5 {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        ctx.stream.push("??@", colors::GRAY20);
        ctx.push_literal(backrefs, &self.0, colors::GRAY20);
        ctx.stream.push("@", colors::GRAY20);
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Scope(Vec<NestedPath>);

impl Parse for Scope {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let mut paths = Vec::new();

        while !ctx.eat(b'@') {
            let segment = NestedPath::parse(ctx, backrefs)?;
            paths.push(segment);
        }

        Some(Scope(paths))
    }
}

impl<'a> Format<'a> for Scope {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        for (idx, part) in self.0.iter().rev().enumerate() {
            part.demangle(ctx, backrefs);

            if idx != self.0.len() - 1 {
                ctx.stream.push("::", colors::GRAY20);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Path {
    name: UnqualifiedPath,
    scope: Scope,
}

impl Parse for Path {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let name = UnqualifiedPath::parse(ctx, backrefs)?;
        let scope = Scope::parse(ctx, backrefs)?;

        Some(Path { name, scope })
    }
}

impl<'a> Format<'a> for Path {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        self.scope.demangle(ctx, backrefs);

        if !self.scope.0.is_empty() {
            ctx.stream.push("::", colors::GRAY20);
        }

        self.name.0.demangle(ctx, backrefs);
    }
}

#[derive(Debug, Clone, PartialEq)]
enum NestedPath {
    Literal(Literal),
    Interface(Literal),
    Template(Template),
    Operator(Operator),
    Symbol(Box<Symbol>),
    Disambiguator(isize),
    MD5(MD5),
    Anonymous,
}

impl Parse for NestedPath {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        ctx.descent()?;

        if let Some(digit) = ctx.base10() {
            ctx.ascent();
            return backrefs.get_memorized_ident(digit).map(NestedPath::Literal);
        }

        if ctx.eat(b'?') {
            ctx.ascent();
            return match ctx.peek()? {
                b'?' => {
                    ctx.offset += 1;

                    Symbol::parse(ctx, backrefs)
                        .map(Box::new)
                        .map(NestedPath::Symbol)
                }
                b'$' => {
                    ctx.offset += 1;

                    Template::parse(ctx, backrefs).and_then(|template| {
                        let name = template.name.0;

                        if let NestedPath::Literal(ref literal) = name {
                            backrefs.try_memorizing_ident(literal);
                            return Some(name);
                        }

                        None
                    })
                }
                b'A' => {
                    ctx.offset += 1;

                    if let Some(b"0x") = ctx.peek_slice(0..2) {
                        let mut len = 0;

                        // skip over anonymous namespace disambiguator
                        while let Some(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F') = ctx.peek() {
                            len += 1;
                            ctx.offset += 1;
                        }

                        backrefs.try_memorizing_ident(&Literal::Borrowed {
                            start: ctx.offset,
                            end: ctx.offset + len,
                        });
                    }

                    ctx.consume(b'@')?;
                    Some(NestedPath::Anonymous)
                }
                b'Q' => {
                    ctx.offset += 1;

                    let ident = ctx.ident()?;
                    ctx.consume(b'@')?;
                    backrefs.try_memorizing_ident(&ident);

                    Some(NestedPath::Interface(ident))
                }
                _ => {
                    let disambiguator = ctx.number()?;
                    Some(NestedPath::Disambiguator(disambiguator))
                }
            };
        }

        let ident = ctx.ident()?;
        backrefs.try_memorizing_ident(&ident);

        ctx.ascent();
        Some(NestedPath::Literal(ident))
    }
}

impl<'a> Format<'a> for NestedPath {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        match self {
            NestedPath::Literal(ident) => {
                ctx.push_literal(backrefs, ident, colors::BLUE);
            }
            NestedPath::Interface(ident) => {
                ctx.stream.push("[", colors::GRAY40);
                ctx.push_literal(backrefs, ident, colors::BLUE);
                ctx.stream.push("]", colors::GRAY40);
            }
            NestedPath::Template(template) => template.demangle(ctx, backrefs),
            NestedPath::Operator(operator) => operator.demangle(ctx, backrefs),
            NestedPath::Symbol(inner) => inner.demangle(ctx, backrefs),
            NestedPath::Disambiguator(val) => {
                let val = std::borrow::Cow::Owned(format!("{val:?}"));

                ctx.stream.push("`", colors::GRAY20);
                ctx.stream.push_cow(val, colors::GRAY20);
                ctx.stream.push("'", colors::GRAY20);
            }
            NestedPath::MD5(md5) => md5.demangle(ctx, backrefs),
            NestedPath::Anonymous => ctx.stream.push("`anonymous namespace'", colors::GRAY40),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct UnqualifiedPath(NestedPath);

impl Parse for UnqualifiedPath {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        ctx.descent()?;

        // memorized ident
        if let Some(digit) = ctx.base10() {
            ctx.ascent();
            return backrefs
                .get_memorized_ident(digit)
                .map(NestedPath::Literal)
                .map(UnqualifiedPath);
        }

        if ctx.eat(b'?') {
            if ctx.eat(b'$') {
                ctx.ascent();
                return Template::parse(ctx, backrefs)
                    .map(NestedPath::Template)
                    .map(UnqualifiedPath);
            }

            ctx.ascent();
            return Operator::parse(ctx, backrefs)
                .map(NestedPath::Operator)
                .map(UnqualifiedPath);
        }

        ctx.ascent();
        ctx.ident().map(NestedPath::Literal).map(UnqualifiedPath)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct EncodedIdent;

impl Parse for EncodedIdent {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        let width = ctx.base10()?;
        if width > 2 {
            return None;
        }

        let len = std::cmp::min(ctx.number()? as usize, width * 32);
        ctx.number()?;

        for _ in 0..len {
            let chr = ctx.take()?;

            if let b'0'..=b'9' | b'a'..=b'z' | b'_' | b'$' = chr {
                continue;
            }

            if chr == b'?' {
                ctx.take()?;
                continue;
            }

            return None;
        }

        Some(EncodedIdent)
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Template {
    name: Box<UnqualifiedPath>,
    params: Parameters,
}

impl Parse for Template {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        let mut temp = Backrefs::default();
        let name = Box::new(UnqualifiedPath::parse(ctx, &mut temp)?);
        let params = Parameters::parse(ctx, &mut temp)?;

        Some(Template { name, params })
    }
}

impl<'a> Format<'a> for Template {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        self.name.0.demangle(ctx, backrefs);
        ctx.stream.push("<", colors::GRAY40);
        self.params.demangle(ctx, backrefs);
        ctx.stream.push(">", colors::GRAY40);
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Symbol {
    path: Path,
    tipe: Type,
}

impl Parse for Symbol {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        ctx.descent()?;
        ctx.consume(b'?')?;

        // unparseable MD5 encoded symbol
        if ctx.eat_slice(b"?@") {
            ctx.ascent();
            return MD5::parse(ctx, backrefs)
                .map(NestedPath::MD5)
                .map(NestedPath::into)
                .map(Path::into);
        }

        // scoped template instantiation?
        if ctx.eat_slice(b"$TSS") {
            let mut n = 0usize;

            while !ctx.eat(b'@') {
                let digit = ctx.base10()?;

                n = n.checked_mul(10)?;
                n = n.checked_add(digit)?;
            }

            ctx.ascent();
            // let name = NestedPath::parse(ctx, backrefs)?;
            // let scope = Scope::parse(ctx, backrefs)?;
            todo!("TODO: return thread safe static guard")
        }

        // any other template instantiation
        if ctx.eat(b'$') {
            ctx.ascent();
            return Template::parse(ctx, backrefs)
                .map(NestedPath::Template)
                .map(NestedPath::into)
                .map(Path::into);
        }

        let path = Path::parse(ctx, backrefs)?;

        // no type
        if ctx.peek().is_none() {
            ctx.ascent();
            return Some(path).map(Path::into);
        }

        ctx.modifiers = Modifiers::empty();

        let tipe = match ctx.take()? {
            // C style type
            b'9' => return Some(path).map(Path::into),
            // <type> <cvr-qualifiers>
            b'0' => {
                let tipe = Type::parse(ctx, backrefs)?;
                let modi = Modifiers::parse(ctx, backrefs)?;
                Type::Variable(StorageVariable::PrivateStatic, modi, Box::new(tipe))
            }
            // <type> <cvr-qualifiers>
            b'1' => {
                let tipe = Type::parse(ctx, backrefs)?;
                let modi = Modifiers::parse(ctx, backrefs)?;
                Type::Variable(StorageVariable::ProtectedStatic, modi, Box::new(tipe))
            }
            // <type> <cvr-qualifiers>
            b'2' => {
                let tipe = Type::parse(ctx, backrefs)?;
                let modi = Modifiers::parse(ctx, backrefs)?;
                Type::Variable(StorageVariable::PublicStatic, modi, Box::new(tipe))
            }
            // <type> <cvr-qualifiers>
            b'3' => {
                let tipe = Type::parse(ctx, backrefs)?;
                let modi = Modifiers::parse(ctx, backrefs)?;
                Type::Variable(StorageVariable::Global, modi, Box::new(tipe))
            }
            b'Y' => Type::Function(Function::parse(ctx, backrefs)?),
            b'_' => EncodedIdent::parse(ctx, backrefs).map(Type::Encoded)?,
            _ => MemberFunction::parse(ctx, backrefs).map(Type::MemberFunction)?,
        };

        ctx.ascent();
        Some(Symbol { path, tipe })
    }
}

impl<'a> Format<'a> for Symbol {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        ctx.scope = &self.path.scope.0[..];

        self.tipe.demangle_pre(ctx, backrefs);
        self.path.demangle(ctx, backrefs);
        self.tipe.demangle_post(ctx, backrefs);

        ctx.scope = &[];
    }
}

impl From<Path> for Symbol {
    #[inline]
    fn from(path: Path) -> Symbol {
        Symbol {
            path,
            tipe: Type::Unit,
        }
    }
}

impl From<NestedPath> for Path {
    #[inline]
    fn from(name: NestedPath) -> Path {
        Path {
            name: UnqualifiedPath(name),
            scope: Scope(Vec::new()),
        }
    }
}
