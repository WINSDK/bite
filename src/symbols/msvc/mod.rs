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

use std::borrow::Cow;
use std::fmt;
use std::mem::MaybeUninit;

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

/// Converts an trivially printable node to a string.
trait Format<'a> {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs);
}

/// Converts an node to a string.
///
/// Converting an node representing a C++ type to a string is tricky due
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
trait PositionalFormat<'a> {
    fn demangle_pre(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs);
    fn demangle_post(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs);
}

/// Parses node potentially modifying the context.
/// Output may depend on child nodes or the parent as they modify the context which will
/// later be used by parent nodes or other unparsed children.
trait Parse: Sized {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self>;
}

#[derive(Default, Debug, Clone, PartialEq)]
enum Type {
    #[default]
    Unit,
    Nullptr,
    Void(Modifiers),
    Bool(Modifiers),
    Char(Modifiers),
    Char8(Modifiers),
    Char16(Modifiers),
    Char32(Modifiers),
    IChar(Modifiers),
    UChar(Modifiers),
    WChar(Modifiers),
    IShort(Modifiers),
    UShort(Modifiers),
    Int(Modifiers),
    UInt(Modifiers),
    Float(Modifiers),
    Double(Modifiers),
    LDouble(Modifiers),
    Long(Modifiers),
    ULong(Modifiers),
    Int64(Modifiers),
    UInt64(Modifiers),
    Int128(Modifiers),
    Uint128(Modifiers),
    Union(Modifiers, Path),
    Enum(Modifiers, Path),
    Struct(Modifiers, Path),
    Class(Modifiers, Path),
    Ref(Modifiers, Box<Pointee>),
    RValueRef(Modifiers, Box<Pointee>),
    Ptr(Modifiers, Box<Pointee>),
    Function(Function),
    MemberFunction(MemberFunction),
    MemberFunctionPtr(MemberFunctionPtr),
    Constant(isize),
    Variable(Variable),

    /// Renamed literal with additional modifiers.
    Typedef(Modifiers, Literal),

    /// String encoded using a format we don't know.
    Encoded(EncodedIdent),

    /// Array of a single type which can be on n dimensions. E.g. int[20][10][5][..]
    Array(Array),

    /// template-parameter-<idx>
    TemplateParameterIdx(isize),

    /// Virtual function table
    VFTable(Qualifiers, Option<Scope>),

    /// Virtual base table
    VBTable(Qualifiers, Option<Scope>),

    /// ???
    VCallThunk(isize, CallingConv),

    /// extern "C"
    Extern(Box<Type>),
}

impl Parse for Type {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        match ctx.peek_slice(0..2)? {
            b"W4" => {
                ctx.offset += 2;
                ctx.memorizing = false;

                let name = Path::parse(ctx, backrefs)?;
                return Some(Type::Enum(ctx.modifiers_in_use, name));
            }
            b"A6" => {
                ctx.offset += 2;
                ctx.parsing_qualifiers = false;

                let func = Function::parse(ctx, backrefs)
                    .map(Type::Function)
                    .map(Pointee)?;

                return Some(Type::Ref(ctx.modifiers_in_use, Box::new(func)));
            }
            b"P6" => {
                ctx.offset += 2;
                ctx.parsing_qualifiers = false;

                let func = Function::parse(ctx, backrefs)
                    .map(Type::Function)
                    .map(Pointee)?;

                return Some(Type::Ptr(ctx.modifiers_in_use, Box::new(func)));
            }
            b"P8" => {
                ctx.offset += 2;
                ctx.parsing_qualifiers = true;

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
                    return Some(Type::Typedef(ctx.modifiers_in_use, name));
                }

                if ctx.eat(b'T') {
                    return Some(Type::Nullptr);
                }

                if ctx.eat(b'Q') {
                    ctx.parsing_qualifiers = false;

                    let func = Function::parse(ctx, backrefs)
                        .map(Type::Function)
                        .map(Pointee)?;

                    return Some(Type::RValueRef(ctx.modifiers_in_use, Box::new(func)));
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
                    ctx.modifiers_in_use = Qualifiers::parse(ctx, backrefs)?.0;
                }
            }

            if ctx.eat(b'S') {
                return Some(Type::Unit);
            }

            if let Some(b'1' | b'H' | b'I' | b'J') = ctx.peek() {
                ctx.offset += 1;
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

        let tipe = match ctx.take()? {
            b'T' => Type::Union(ctx.modifiers_in_use, Path::parse(ctx, backrefs)?),
            b'U' => Type::Struct(ctx.modifiers_in_use, Path::parse(ctx, backrefs)?),
            b'V' => Type::Class(ctx.modifiers_in_use, Path::parse(ctx, backrefs)?),
            b'A' => Type::Ref(
                ctx.modifiers_in_use,
                Box::new(Pointee::parse(ctx, backrefs)?),
            ),
            b'B' => Type::Ref(
                Modifiers::VOLATILE,
                Box::new(Pointee::parse(ctx, backrefs)?),
            ),
            b'P' => Type::Ptr(
                ctx.modifiers_in_use,
                Box::new(Pointee::parse(ctx, backrefs)?),
            ),
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
            b'X' => Type::Void(ctx.modifiers_in_use),
            b'D' => Type::Char(ctx.modifiers_in_use),
            b'C' => Type::IChar(ctx.modifiers_in_use),
            b'E' => Type::UChar(ctx.modifiers_in_use),
            b'F' => Type::IShort(ctx.modifiers_in_use),
            b'G' => Type::UShort(ctx.modifiers_in_use),
            b'H' => Type::Int(ctx.modifiers_in_use),
            b'I' => Type::UInt(ctx.modifiers_in_use),
            b'J' => Type::Long(ctx.modifiers_in_use),
            b'K' => Type::ULong(ctx.modifiers_in_use),
            b'M' => Type::Float(ctx.modifiers_in_use),
            b'N' => Type::Double(ctx.modifiers_in_use),
            b'O' => Type::LDouble(ctx.modifiers_in_use),
            b'_' => match ctx.take()? {
                b'N' => Type::Bool(ctx.modifiers_in_use),
                b'J' => Type::Int64(ctx.modifiers_in_use),
                b'K' => Type::UInt64(ctx.modifiers_in_use),
                b'L' => Type::Int128(ctx.modifiers_in_use),
                b'M' => Type::Uint128(ctx.modifiers_in_use),
                b'W' => Type::WChar(ctx.modifiers_in_use),
                b'Q' => Type::Char8(ctx.modifiers_in_use),
                b'S' => Type::Char16(ctx.modifiers_in_use),
                b'U' => Type::Char32(ctx.modifiers_in_use),
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

impl<'a> PositionalFormat<'a> for Type {
    fn demangle_pre(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        match self {
            Type::Unit => {}
            Type::Nullptr => ctx.stream.push("std::nullptr_t", colors::MAGENTA),
            Type::Void(modi) => {
                ctx.stream.push("void", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Bool(modi) => {
                ctx.stream.push("bool", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Char(modi) => {
                ctx.stream.push("char", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Char8(modi) => {
                ctx.stream.push("char8", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Char16(modi) => {
                ctx.stream.push("char16", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Char32(modi) => {
                ctx.stream.push("char32", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::IChar(modi) => {
                ctx.stream.push("signed char", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::UChar(modi) => {
                ctx.stream.push("unsigned char", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::WChar(modi) => {
                ctx.stream.push("wchar", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::IShort(modi) => {
                ctx.stream.push("short", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::UShort(modi) => {
                ctx.stream.push("unsigned short", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Int(modi) => {
                ctx.stream.push("int", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::UInt(modi) => {
                ctx.stream.push("unsigned int", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Float(modi) => {
                ctx.stream.push("float", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Double(modi) => {
                ctx.stream.push("double", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::LDouble(modi) => {
                ctx.stream.push("long double", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Long(modi) => {
                ctx.stream.push("long", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::ULong(modi) => {
                ctx.stream.push("unsigned long", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Int64(modi) => {
                ctx.stream.push("__int64", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::UInt64(modi) => {
                ctx.stream.push("unsigned __int64", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Int128(modi) => {
                ctx.stream.push("__int64", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Uint128(modi) => {
                ctx.stream.push("unsigned __int128", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Union(modi, name) => {
                ctx.stream.push("union ", colors::MAGENTA);
                name.demangle(ctx, backrefs);
                ctx.stream.push(" ", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Enum(modi, name) => {
                ctx.stream.push("enum ", colors::MAGENTA);
                name.demangle(ctx, backrefs);
                ctx.stream.push(" ", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Struct(modi, name) => {
                ctx.stream.push("struct ", colors::MAGENTA);
                name.demangle(ctx, backrefs);
                ctx.stream.push(" ", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Class(modi, name) => {
                ctx.stream.push("class ", colors::MAGENTA);
                name.demangle(ctx, backrefs);
                ctx.stream.push(" ", colors::MAGENTA);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Ptr(modi, tipe) | Type::Ref(modi, tipe) | Type::RValueRef(modi, tipe) => {
                // "[]" and "()" (for function parameters) take precedence over "*",
                // so "int *x(int)" means "x is a function returning int *". We need
                // parentheses to supercede the default precedence. (e.g. we want to
                // emit something like "int (*x)(int)".)

                let tipe = &tipe.0;
                match tipe {
                    Type::Function(func) => {
                        func.return_type.demangle_pre(ctx, backrefs);
                        ctx.stream.push("(", colors::GRAY40);
                        func.calling_conv.demangle(ctx, backrefs);
                        ctx.stream.push(" ", colors::WHITE);
                    }
                    Type::MemberFunction(func) => {
                        func.storage_scope.demangle(ctx, backrefs);
                        func.return_type.demangle_pre(ctx, backrefs);
                        func.calling_conv.demangle(ctx, backrefs);
                        ctx.stream.push(" ", colors::WHITE);
                    }
                    Type::MemberFunctionPtr(func) => {
                        func.storage_scope.demangle(ctx, backrefs);
                        func.return_type.demangle_pre(ctx, backrefs);
                        ctx.stream.push("(", colors::GRAY40);
                        func.calling_conv.demangle(ctx, backrefs);
                        ctx.stream.push(" ", colors::WHITE);
                    }
                    Type::Array(..) => {
                        tipe.demangle_pre(ctx, backrefs);
                        ctx.stream.push(" (", colors::GRAY40);
                    }
                    _ => tipe.demangle_pre(ctx, backrefs),
                }

                match self {
                    Type::Ptr(..) => ctx.stream.push("*", colors::RED),
                    Type::Ref(..) => ctx.stream.push("&", colors::RED),
                    Type::RValueRef(..) => ctx.stream.push("&&", colors::RED),
                    _ => {}
                }

                modi.demangle_pre(ctx, backrefs);
            }
            Type::Function(func) => {
                func.return_type.demangle_pre(ctx, backrefs);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push(" ", colors::WHITE);
            }
            Type::MemberFunction(func) => {
                func.storage_scope.demangle(ctx, backrefs);
                func.return_type.demangle_pre(ctx, backrefs);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push(" ", colors::WHITE);
            }
            Type::MemberFunctionPtr(func) => {
                func.storage_scope.demangle(ctx, backrefs);
                func.return_type.demangle_pre(ctx, backrefs);
                ctx.stream.push("(", colors::GRAY40);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push("  ", colors::WHITE);
                func.class_name.demangle(ctx, backrefs);
                ctx.stream.push("::*", colors::GRAY40);
            }
            Type::Constant(val) => {
                let val = Cow::Owned(val.to_string());
                ctx.stream.push_cow(val, colors::GRAY20);
            }
            Type::TemplateParameterIdx(_idx) => todo!(),
            Type::Typedef(modi, name) => {
                ctx.push_literal(backrefs, name, colors::PURPLE);
                modi.demangle_pre(ctx, backrefs);
            }
            Type::Variable(Variable {
                storage,
                tipe,
                modi,
            }) => {
                storage.demangle(ctx, backrefs);
                tipe.demangle_pre(ctx, backrefs);
                modi.demangle_pre(ctx, backrefs);
                ctx.stream.push(" ", colors::WHITE);
            }
            Type::Encoded(_) => {}
            Type::Array(array) => {
                array.tipe().demangle_pre(ctx, backrefs);
            }
            Type::VFTable(quali, _) => {
                quali.0.demangle_pre(ctx, backrefs);
            }
            Type::VBTable(quali, _) => {
                quali.0.demangle_pre(ctx, backrefs);
            }
            Type::VCallThunk(_, calling_conv) => {
                ctx.stream.push("[thunk]: ", colors::GRAY40);
                calling_conv.demangle(ctx, backrefs);
            }
            Type::Extern(tipe) => {
                ctx.stream.push("extern \"C\" ", colors::GRAY40);
                tipe.demangle_pre(ctx, backrefs);
            }
        }
    }

    fn demangle_post(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
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

                func.qualifiers.0.demangle_post(ctx, backrefs);
                func.return_type.demangle_post(ctx, backrefs);
            }
            Type::MemberFunction(func) => {
                ctx.stream.push("(", colors::GRAY40);
                func.params.0.demangle(ctx, backrefs);
                ctx.stream.push(")", colors::GRAY40);

                func.qualifiers.0.demangle_post(ctx, backrefs);
                func.return_type.demangle_post(ctx, backrefs);
            }
            Type::MemberFunctionPtr(func) => {
                ctx.stream.push("(", colors::GRAY40);
                func.params.0.demangle(ctx, backrefs);
                ctx.stream.push(")", colors::GRAY40);

                func.qualifiers.0.demangle_post(ctx, backrefs);
                func.return_type.demangle_post(ctx, backrefs);
            }
            Type::Variable(Variable { tipe, .. }) => tipe.demangle_post(ctx, backrefs),
            Type::Array(array) => {
                let len = Cow::Owned(array.len.to_string());
                ctx.stream.push("[", colors::GRAY40);
                ctx.stream.push_cow(len, colors::BLUE);
                ctx.stream.push("]", colors::GRAY40);
                array.tipe().demangle_post(ctx, backrefs);
            }
            Type::VBTable(_, scope) | Type::VFTable(_, scope) => match scope {
                Some(scope) if !scope.0.is_empty() => {
                    ctx.stream.push("{for `", colors::GRAY40);
                    scope.demangle(ctx, backrefs);
                    ctx.stream.push("'}", colors::GRAY40);
                }
                None => {
                    ctx.stream.push("{for ??}", colors::GRAY40);
                }
                _ => {}
            },
            Type::VCallThunk(offset, _) => {
                ctx.stream.push("{{", colors::GRAY40);
                ctx.stream.push_cow(Cow::Owned(offset.to_string()), colors::BLUE);
                ctx.stream.push(", {{flat}}}}", colors::GRAY40);
            }
            Type::Extern(tipe) => {
                tipe.demangle_post(ctx, backrefs);
            }
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct SymbolType(Type);

impl Parse for SymbolType {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let tipe = match ctx.take()? {
            b'0'..=b'4' => {
                ctx.offset -= 1;
                Type::Variable(Variable::parse(ctx, backrefs)?)
            }
            b'5' => {
                todo!()
            }
            // virtual function table
            b'6' => {
                let qualifiers = Qualifiers::parse(ctx, backrefs)?;
                let scope = Scope::parse(ctx, backrefs);
                Type::VFTable(qualifiers, scope)
            }
            // virtual base table
            b'7' => {
                let qualifiers = Qualifiers::parse(ctx, backrefs)?;
                let scope = Scope::parse(ctx, backrefs);
                Type::VBTable(qualifiers, scope)
            }
            // RTTI's don't have a type
            b'8' => Type::Unit,
            // C style type
            b'9' => return None,
            // anonymous function
            b'Y' => Type::Function(Function::parse(ctx, backrefs)?),
            // special cases
            b'$' => match ctx.take()? {
                // extern "C" modifier
                b'$' => {
                    // prefix
                    match ctx.take()? {
                        b'J' | b'N' | b'O' => {}
                        _ => return None,
                    }

                    // skip at least one base10 number.
                    let mut did_a_skip = false;
                    loop {
                        if ctx.base10().is_none() {
                            break;
                        }

                        did_a_skip = true;
                    }

                    if !did_a_skip {
                        return None;
                    }

                    dbg!(ctx.src());
                    let tipe = SymbolType::parse(ctx, backrefs)?.0;
                    Type::Extern(Box::new(tipe))
                }
                b'B' => {
                    let offset = ctx.number()?;
                    ctx.consume(b'A')?;
                    let calling_conv = CallingConv::parse(ctx, backrefs)?;
                    Type::VCallThunk(offset, calling_conv)
                }
                // TODO: there are more cases here
                _ => return None,
            },
            // ident with unknown encoding
            b'_' => EncodedIdent::parse(ctx, backrefs).map(Type::Encoded)?,
            // anything else should be a member function
            _ => {
                ctx.offset -= 1;
                MemberFunction::parse(ctx, backrefs).map(Type::MemberFunction)?
            }
        };

        Some(SymbolType(tipe))
    }
}

/// ```text
/// = '?' <storage> <type> <cvr-qualifiers>
/// |     <storage> <cvr-qualifiers> <type>
///
/// <storage> = PrivateStatic | ProtectedStatic | PublicStatic | Global | FunctionLocalStatic
/// ```
#[derive(Debug, PartialEq, Clone)]
struct Variable {
    storage: StorageVariable,
    modi: Modifiers,
    tipe: Box<Type>,
}

impl Parse for Variable {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let storage = match ctx.take()? {
            b'0' => StorageVariable::PrivateStatic,
            b'1' => StorageVariable::ProtectedStatic,
            b'2' => StorageVariable::PublicStatic,
            b'3' => StorageVariable::Global,
            b'4' => StorageVariable::FunctionLocalStatic,
            _ => unsafe { std::hint::unreachable_unchecked() },
        };

        let (tipe, modi);
        if ctx.eat(b'?') {
            modi = Modifiers::parse(ctx, backrefs)?;
            tipe = Type::parse(ctx, backrefs)?;
        } else {
            tipe = Type::parse(ctx, backrefs)?;
            modi = Modifiers::parse(ctx, backrefs)?;
        }

        Some(Variable {
            storage,
            modi,
            tipe: Box::new(tipe),
        })
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
        let return_type = FunctionReturnType::parse(ctx, backrefs)?;
        let params = FunctionParameters::parse(ctx, backrefs)?;

        Some(Function {
            calling_conv,
            qualifiers: FunctionQualifiers(Modifiers::empty()),
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
        let storage_scope = StorageScope::parse(ctx, backrefs)?;
        let mut qualifiers = FunctionQualifiers(Modifiers::empty());

        if !storage_scope.contains(StorageScope::STATIC) {
            qualifiers = FunctionQualifiers::parse(ctx, backrefs)?;
        }

        let calling_conv = CallingConv::parse(ctx, backrefs)?;
        ctx.modifiers_in_use = ReturnModifiers::parse(ctx, backrefs)?.0;
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
        ctx.modifiers_in_use = ReturnModifiers::parse(ctx, backrefs)?.0;
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

#[derive(Debug)]
struct Array {
    modifiers: Modifiers,
    tipe: MaybeUninit<Box<Type>>,
    len: isize,
}

impl Parse for Array {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let dimensions = ctx.number()?;
        let mut root = Array {
            modifiers: Modifiers::empty(),
            tipe: MaybeUninit::uninit(),
            len: 0,
        };

        let mut node = &mut root;

        if dimensions < 0 {
            return None;
        }

        for _ in 0..dimensions {
            // clear modifiers
            ctx.modifiers_in_use = Modifiers::empty();

            // construct array
            node.tipe = MaybeUninit::new(Box::new(Type::Array(Array {
                modifiers: Modifiers::empty(),
                tipe: MaybeUninit::uninit(),
                len: ctx.number()?,
            })));

            // get reference to array within the boxed type
            match **unsafe { node.tipe.assume_init_mut() } {
                Type::Array(ref mut arr) => node = arr,
                // SAFETY: this can only be an array as we just declared `current` to be an array
                _ => unsafe { std::hint::unreachable_unchecked() },
            };
        }

        if ctx.eat_slice(b"$$C") {
            root.modifiers = match ctx.take()? {
                b'A' => Modifiers::empty(),
                b'B' => Modifiers::CONST,
                b'C' | b'D' => Modifiers::CONST | Modifiers::VOLATILE,
                _ => return None,
            };
        }

        root.len = ctx.number()?;
        root.tipe = Type::parse(ctx, backrefs)
            .map(Box::new)
            .map(MaybeUninit::new)?;

        Some(root)
    }
}

impl Array {
    #[inline]
    fn tipe(&self) -> &Type {
        unsafe { self.tipe.assume_init_ref() }
    }
}

impl PartialEq for Array {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.modifiers == other.modifiers
            && unsafe { self.tipe.assume_init_ref() == other.tipe.assume_init_ref() }
            && self.len == other.len
    }
}

impl Clone for Array {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            modifiers: self.modifiers,
            tipe: MaybeUninit::new(unsafe { self.tipe.assume_init_ref() }.clone()),
            len: self.len,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Pointee(Type);

impl Parse for Pointee {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        if ctx.eat(b'E') {
            ctx.modifiers_in_use |= Modifiers::PTR64;
        }

        let pointer_modi = Modifiers::parse(ctx, backrefs)?;

        ctx.modifiers_in_use |= pointer_modi;

        Type::parse(ctx, backrefs).map(Pointee)
    }
}

#[derive(Debug, PartialEq, Clone)]
struct FunctionReturnType(Type);

impl Parse for FunctionReturnType {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        ctx.modifiers_in_use = ReturnModifiers::parse(ctx, backrefs)?.0;

        if ctx.eat(b'@') {
            return Some(FunctionReturnType(Type::Unit));
        }

        Type::parse(ctx, backrefs).map(FunctionReturnType)
    }
}

impl<'a> PositionalFormat<'a> for FunctionReturnType {
    fn demangle_pre(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        self.0.demangle_pre(ctx, backrefs);
        if self.0 != Type::Unit {
            ctx.stream.push(" ", colors::WHITE);
        }
    }

    fn demangle_post(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        self.0.demangle_post(ctx, backrefs);
    }
}

/// Either a well known operator of a class or some C++ internal operator implementation.
#[derive(Debug, PartialEq, Clone)]
enum Intrinsics {
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
    DynamicInitializer(Box<Symbol>),
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
    RTTITypeDescriptor(Modifiers, Box<Type>),
    RTTIBaseClassDescriptor {
        nv_off: isize,
        ptr_off: isize,
        vbtable_off: isize,
        flags: isize,
    },
    RTTIBaseClassArray,
    RTTIClassHierarchyDescriptor,
    RTTIClassCompleteObjectLocator,
    LocalVFTable,
    LocalVftableCtorClosure,
    PlacementDeleteClosure,
    PlacementDeleteArrayClosure,
    SourceName(Literal),
}

impl Parse for Intrinsics {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let op = match ctx.take()? {
            b'0' => Intrinsics::Ctor,
            b'1' => Intrinsics::Dtor,
            b'2' => Intrinsics::New,
            b'3' => Intrinsics::Delete,
            b'4' => Intrinsics::Assign,
            b'5' => Intrinsics::ShiftRight,
            b'6' => Intrinsics::ShiftLeft,
            b'7' => Intrinsics::LogicalNot,
            b'8' => Intrinsics::Equals,
            b'9' => Intrinsics::NotEquals,
            b'A' => Intrinsics::Array,
            b'C' => Intrinsics::Pointer,
            b'D' => Intrinsics::Dereference,
            b'E' => Intrinsics::Increment,
            b'F' => Intrinsics::Decrement,
            b'G' => Intrinsics::Minus,
            b'H' => Intrinsics::Plus,
            b'I' => Intrinsics::ArithmeticAND,
            b'J' => Intrinsics::MemberDereference,
            b'K' => Intrinsics::Divide,
            b'L' => Intrinsics::Modulus,
            b'M' => Intrinsics::Less,
            b'N' => Intrinsics::LessEqual,
            b'O' => Intrinsics::Greater,
            b'P' => Intrinsics::GreaterEqual,
            b'Q' => Intrinsics::Comma,
            b'R' => Intrinsics::Calling,
            b'S' => Intrinsics::ArithmeticNot,
            b'T' => Intrinsics::Xor,
            b'U' => Intrinsics::ArithmeticOR,
            b'V' => Intrinsics::LogicalAND,
            b'W' => Intrinsics::LogicalOR,
            b'X' => Intrinsics::TimesEquals,
            b'Y' => Intrinsics::PlusEquals,
            b'Z' => Intrinsics::MinusEquals,
            b'_' => match ctx.take()? {
                b'0' => Intrinsics::DivideEquals,
                b'1' => Intrinsics::ModulusEquals,
                b'2' => Intrinsics::ShiftRightEquals,
                b'3' => Intrinsics::ShiftLeftEquals,
                b'4' => Intrinsics::ANDEquals,
                b'5' => Intrinsics::OREquals,
                b'6' => Intrinsics::XorEquals,
                b'7' => Intrinsics::VFTable,
                b'8' => Intrinsics::VBTable,
                b'9' => Intrinsics::VCall,
                b'A' => Intrinsics::TypeOff,
                b'B' => Intrinsics::LocalStaticGuard,
                b'C' => Intrinsics::String,
                b'D' => Intrinsics::VBaseDtor,
                b'E' => Intrinsics::VectorDeletingDtor,
                b'F' => Intrinsics::DefaultCtorClosure,
                b'G' => Intrinsics::ScalarDeletingDtor,
                b'H' => Intrinsics::VecCtorIter,
                b'I' => Intrinsics::VecDtorIter,
                b'J' => Intrinsics::VecVbaseCtorIter,
                b'K' => Intrinsics::VdispMap,
                b'L' => Intrinsics::EHVecCtorIter,
                b'M' => Intrinsics::EHVecDtorIter,
                b'N' => Intrinsics::EHVecVbaseCtorIter,
                b'O' => Intrinsics::CopyCtorClosure,
                b'R' => match ctx.take()? {
                    b'0' => {
                        ctx.consume(b'?');
                        let modi = Modifiers::parse(ctx, backrefs)?;
                        let tipe = Type::parse(ctx, backrefs)?;
                        Intrinsics::RTTITypeDescriptor(modi, Box::new(tipe))
                    }
                    b'1' => {
                        let nv_off = ctx.number()?;
                        let ptr_off = ctx.number()?;
                        let vbtable_off = ctx.number()?;
                        let flags = ctx.number()?;

                        Intrinsics::RTTIBaseClassDescriptor {
                            nv_off,
                            ptr_off,
                            vbtable_off,
                            flags,
                        }
                    }
                    b'2' => Intrinsics::RTTIBaseClassArray,
                    b'3' => Intrinsics::RTTIClassHierarchyDescriptor,
                    b'4' => Intrinsics::RTTIClassCompleteObjectLocator,
                    _ => return None,
                },
                b'S' => Intrinsics::LocalVFTable,
                b'T' => Intrinsics::LocalVftableCtorClosure,
                b'U' => Intrinsics::NewArray,
                b'V' => Intrinsics::DeleteArray,
                b'X' => Intrinsics::PlacementDeleteClosure,
                b'Y' => Intrinsics::PlacementDeleteArrayClosure,
                b'_' => match ctx.take()? {
                    b'L' => Intrinsics::CoAwait,
                    b'E' => {
                        let sym = Symbol::parse(ctx, backrefs).or_else(|| {
                            // not sure what this is.
                            // it appears to be a literal from what the ghidra tests indicate
                            Literal::parse(ctx, backrefs)
                                .map(NestedPath::Literal)
                                .map(NestedPath::into)
                                .map(Path::into)
                        })?;

                        Intrinsics::DynamicInitializer(Box::new(sym))
                    }
                    b'F' => Intrinsics::DynamicAtexitDtor,
                    b'J' => Intrinsics::LocalStaticThreadGuard,
                    b'M' => Intrinsics::Spaceship,
                    b'K' => return ctx.ident().map(Intrinsics::SourceName),
                    _ => return None,
                },
                _ => return None,
            },
            _ => return None,
        };

        Some(op)
    }
}

impl<'a> Format<'a> for Intrinsics {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        let literal = match *self {
            Intrinsics::Ctor => {
                let name = ctx.scope.get(0);

                return match name {
                    Some(NestedPath::Literal(l)) => ctx.push_literal(backrefs, l, colors::BLUE),
                    _ => ctx.stream.push("`unnamed constructor'", colors::GRAY20),
                };
            }
            Intrinsics::Dtor => {
                let name = ctx.scope.get(0);

                ctx.stream.push("~", colors::MAGENTA);

                return match name {
                    Some(NestedPath::Literal(l)) => ctx.push_literal(backrefs, l, colors::BLUE),
                    _ => ctx.stream.push("`unnamed destructor'", colors::GRAY20),
                };
            }
            Intrinsics::DynamicInitializer(ref tipe) => {
                ctx.stream
                    .push("`dynamic initializer for '", colors::GRAY20);
                tipe.demangle(ctx, backrefs);
                ctx.stream.push("''", colors::GRAY40);
                return;
            }
            Intrinsics::SourceName(src) => {
                ctx.push_literal(backrefs, &src, colors::MAGENTA);
                return;
            }
            Intrinsics::RTTITypeDescriptor(_, ref tipe) => {
                tipe.demangle_pre(ctx, backrefs);
                ctx.stream.push("`RTTI Type Descriptor'", colors::GRAY40);
                return;
            }
            Intrinsics::RTTIBaseClassDescriptor {
                nv_off,
                ptr_off,
                vbtable_off,
                flags,
            } => {
                let str = format!(
                    "`RTTI Base Class Descriptor at ({nv_off}, {ptr_off}, {vbtable_off}, {flags})'",
                );
                ctx.stream.push_cow(Cow::Owned(str), colors::GRAY40);
                return;
            }
            Intrinsics::RTTIBaseClassArray => {
                ctx.stream.push("`RTTI Base Class Array'", colors::GRAY40);
                return;
            }
            Intrinsics::RTTIClassHierarchyDescriptor => {
                ctx.stream
                    .push("`RTTI Class Hierarchy Descriptor'", colors::GRAY40);
                return;
            }
            Intrinsics::RTTIClassCompleteObjectLocator => {
                ctx.stream
                    .push("`RTTI Complete Object Locator'", colors::GRAY40);
                return;
            }
            Intrinsics::New => "operator new",
            Intrinsics::Delete => "operator delete",
            Intrinsics::Assign => "operator=",
            Intrinsics::ShiftRight => "operator>>",
            Intrinsics::ShiftRightEquals => "operator>>=",
            Intrinsics::ShiftLeft => "operator<<",
            Intrinsics::ShiftLeftEquals => "operator<<=",
            Intrinsics::LogicalNot => "operator!",
            Intrinsics::Equals => "operator=",
            Intrinsics::NotEquals => "operator!=",
            Intrinsics::Array => "operator[]",
            Intrinsics::Pointer => "operator->",
            Intrinsics::Dereference => "operator*",
            Intrinsics::TimesEquals => "operator*=",
            Intrinsics::MemberDereference => "operator->*",
            Intrinsics::Increment => "operator++",
            Intrinsics::Decrement => "operator--",
            Intrinsics::Minus => "operator-",
            Intrinsics::MinusEquals => "operator-=",
            Intrinsics::Plus => "operator+",
            Intrinsics::PlusEquals => "operator+=",
            Intrinsics::ArithmeticAND => "operator&",
            Intrinsics::ANDEquals => "operator&=",
            Intrinsics::LogicalAND => "operator&&",
            Intrinsics::ArithmeticOR => "operator|",
            Intrinsics::OREquals => "operator|=",
            Intrinsics::LogicalOR => "operator||",
            Intrinsics::Divide => "operator/",
            Intrinsics::DivideEquals => "operator/=",
            Intrinsics::Modulus => "operator%",
            Intrinsics::ModulusEquals => "operator%=",
            Intrinsics::Less => "operator<",
            Intrinsics::LessEqual => "operator<=",
            Intrinsics::Greater => "operator>",
            Intrinsics::GreaterEqual => "operator>=",
            Intrinsics::Comma => "operator,",
            Intrinsics::Calling => "operator()",
            Intrinsics::ArithmeticNot => "operator~",
            Intrinsics::Xor => "operator^",
            Intrinsics::XorEquals => "operator^=",
            Intrinsics::VFTable => "`vftable'",
            Intrinsics::VBTable => "`vbtable'",
            Intrinsics::LocalVFTable => "`local vftable'",
            Intrinsics::VCall => "`vcall'",
            Intrinsics::TypeOff => "`typeoff'",
            Intrinsics::LocalStaticGuard => "`local static guard'",
            Intrinsics::String => "`string'",
            Intrinsics::VBaseDtor => "`vbase destructor'",
            Intrinsics::VectorDeletingDtor => "`vector deleting destructor'",
            Intrinsics::DefaultCtorClosure => "`default constructor closure'",
            Intrinsics::ScalarDeletingDtor => "`scalar deleting destructor'",
            Intrinsics::VecCtorIter => "`vector constructor iterator'",
            Intrinsics::VecDtorIter => "`vector destructor iterator'",
            Intrinsics::VecVbaseCtorIter => "`vector vbase constructor iterator'",
            Intrinsics::VdispMap => "`virtual displacement map'",
            Intrinsics::EHVecCtorIter => "`eh vector constructor iterator'",
            Intrinsics::EHVecDtorIter => "`eh vector destructor iterator'",
            Intrinsics::EHVecVbaseCtorIter => "`eh vector vbase constructor iterator'",
            Intrinsics::CopyCtorClosure => "`copy constructor closure'",
            Intrinsics::LocalVftableCtorClosure => "`local vftable constructor closure'",
            Intrinsics::DynamicAtexitDtor => "`dynamic atexit destructor'",
            Intrinsics::LocalStaticThreadGuard => "`local static thread guard'",
            Intrinsics::PlacementDeleteClosure => "`placement delete closure'",
            Intrinsics::PlacementDeleteArrayClosure => "`placement delete[] closure'",
            Intrinsics::NewArray => "operator new[]",
            Intrinsics::DeleteArray => "operator delete[]",
            Intrinsics::CoAwait => "co_await",
            Intrinsics::Spaceship => "operator<=>",
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
            // either the stream is consumed or an list ending is encountered.
            if let Some(b'@') | Some(b'Z') | None = ctx.peek() {
                ctx.offset += 1;
                return Some(Parameters(types));
            }

            if let Some(digit) = ctx.base10() {
                types.push(backrefs.get_memorized_param(digit)?);
                continue;
            }

            ctx.modifiers_in_use = Modifiers::empty();

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
    }
}

impl<'a> Format<'a> for Parameters {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        let mut params = self.0.iter();

        if let Some(param) = params.next() {
            param.demangle(ctx, backrefs);
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
        let params = if ctx.eat(b'X') {
            Parameters(vec![Type::Void(Modifiers::empty())])
        } else {
            Parameters::parse(ctx, backrefs)?
        };

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
        let conv = match ctx.take()? {
            b'A' | b'B' => CallingConv::Cdecl,
            b'C' | b'D' => CallingConv::Pascal,
            b'E' | b'F' => CallingConv::Thiscall,
            b'G' | b'H' => CallingConv::Stdcall,
            b'I' | b'J' => CallingConv::Fastcall,
            b'M' | b'N' => CallingConv::Clrcall,
            b'O' | b'P' => CallingConv::Eabi,
            b'Q' => CallingConv::Vectorcall,
            _ => return None,
        };

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
    FunctionLocalStatic,
}

impl<'a> Format<'a> for StorageVariable {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        let literal = match self {
            StorageVariable::PrivateStatic => "private: static ",
            StorageVariable::ProtectedStatic => "protected: static ",
            StorageVariable::PublicStatic => "public: static ",
            StorageVariable::Global | StorageVariable::FunctionLocalStatic => return,
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
        let color = colors::MAGENTA;

        if self.contains(StorageScope::PUBLIC) {
            ctx.stream.push("public: ", color);
        }

        if self.contains(StorageScope::PRIVATE) {
            ctx.stream.push("private: ", color);
        }

        if self.contains(StorageScope::PROTECTED) {
            ctx.stream.push("protected: ", color);
        }

        if self.contains(StorageScope::STATIC) {
            ctx.stream.push("static ", color);
        }

        if self.contains(StorageScope::VIRTUAL) {
            ctx.stream.push("virtual ", color);
        }
    }
}

bitflags! {
    #[derive(PartialEq, Eq, Clone, Copy)]
    struct Modifiers: u32 {
        const CONST     = 0b0000000000001;
        const VOLATILE  = 0b0000000000010;
        const FAR       = 0b0000000000100;
        const PTR64     = 0b0000000001000;
        const UNALIGNED = 0b0000000010000;
        const RESTRICT  = 0b0000000100000;
        const LVALUE    = 0b0000001000000;
        const RVALUE    = 0b0000010000000;
    }
}

impl Parse for Modifiers {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        let modi = match ctx.peek() {
            Some(b'E') => Modifiers::FAR,
            Some(b'F') => Modifiers::FAR | Modifiers::CONST,
            Some(b'G') => Modifiers::FAR | Modifiers::VOLATILE,
            Some(b'H') => Modifiers::FAR | Modifiers::VOLATILE | Modifiers::CONST,
            Some(b'A' | b'Q') => Modifiers::empty(),
            Some(b'B' | b'R') => Modifiers::CONST,
            Some(b'C' | b'S') => Modifiers::VOLATILE,
            Some(b'D' | b'T') => Modifiers::CONST | Modifiers::VOLATILE,
            _ => return Some(Modifiers::empty()),
        };

        ctx.offset += 1;
        Some(modi)
    }
}

impl<'a> PositionalFormat<'a> for Modifiers {
    fn demangle_pre(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        let color = colors::BLUE;

        if self.contains(Modifiers::CONST) {
            ctx.stream.push("const ", color);
        }

        if self.contains(Modifiers::VOLATILE) {
            ctx.stream.push("volatile ", color);
        }
    }

    fn demangle_post(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
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
            ctx.stream.push(" __restrict", color);
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
        let mut quali = [Modifiers::empty(); 4];

        for idx in 0..4 {
            let addi = match ctx.peek() {
                Some(b'E') => Modifiers::PTR64,
                Some(b'I') => Modifiers::RESTRICT,
                Some(b'F') => Modifiers::UNALIGNED,
                Some(b'G') => Modifiers::LVALUE,
                Some(b'H') => Modifiers::RVALUE,
                _ => break,
            };

            let lvalue_mismatch = addi == Modifiers::LVALUE && quali.contains(&Modifiers::RVALUE);
            let rvalue_mismatch = addi == Modifiers::RVALUE && quali.contains(&Modifiers::LVALUE);

            if lvalue_mismatch || rvalue_mismatch {
                break;
            }

            quali[idx] = addi;
            ctx.offset += 1;
        }

        let modi = quali[0] | quali[1] | quali[2] | quali[3] | Qualifiers::parse(ctx, backrefs)?.0;
        Some(FunctionQualifiers(modi))
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
        let data = {
            let mut len = 0;
            let start = ctx.offset;

            while ctx.base16().is_some() {
                len += 1;
            }

            Literal::Borrowed {
                start,
                end: start + len,
            }
        };

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

#[derive(Debug, Default, PartialEq, Clone)]
struct Scope(Vec<NestedPath>);

impl Parse for Scope {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let mut paths = Vec::new();

        while !ctx.eat(b'@') {
            let segment = NestedPath::parse(ctx, backrefs)?;

            // ignore sisambiguators
            if let NestedPath::Disambiguator(..) = segment {
                continue;
            }

            paths.push(segment);
        }

        // for empty scope's a trailing '@' will appear
        ctx.consume(b'@');

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
    Intrinsics(Intrinsics),
    Symbol(Box<Symbol>),
    Disambiguator(isize),
    MD5(MD5),
    Anonymous,
}

impl Parse for NestedPath {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        ctx.descent()?;

        // return memorized ident
        if let Some(digit) = ctx.base10() {
            ctx.ascent();
            return backrefs.get_memorized_ident(digit).map(NestedPath::Literal);
        }

        if ctx.eat(b'?') {
            ctx.ascent();
            return match ctx.peek()? {
                b'?' => Symbol::parse(ctx, backrefs)
                    .map(Box::new)
                    .map(NestedPath::Symbol),
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

                    if ctx.eat_slice(b"0x") {
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
            NestedPath::Intrinsics(int) => int.demangle(ctx, backrefs),
            NestedPath::Symbol(inner) => inner.demangle(ctx, backrefs),
            NestedPath::Disambiguator(val) => {
                let val = std::borrow::Cow::Owned(format!("`{val}'"));
                ctx.stream.push_cow(val, colors::GRAY20);
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

        // return memorized ident
        if let Some(digit) = ctx.base10() {
            ctx.ascent();
            return backrefs
                .get_memorized_ident(digit)
                .map(NestedPath::Literal)
                .map(UnqualifiedPath);
        }

        // special intrinsic
        if ctx.eat(b'?') {
            if ctx.eat(b'$') {
                ctx.ascent();
                return Template::parse(ctx, backrefs)
                    .map(NestedPath::Template)
                    .map(UnqualifiedPath);
            }

            ctx.ascent();
            return Intrinsics::parse(ctx, backrefs)
                .map(NestedPath::Intrinsics)
                .map(UnqualifiedPath);
        }

        let name = ctx.ident()?;
        backrefs.try_memorizing_ident(&name);

        ctx.ascent();
        Some(UnqualifiedPath(NestedPath::Literal(name)))
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

/// Root node of the AST.
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

        ctx.modifiers_in_use = Modifiers::empty();
        ctx.parsing_qualifiers = false;

        let tipe = SymbolType::parse(ctx, backrefs)?.0;

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
