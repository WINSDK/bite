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
//!                 | <storage-class> <nested-path>
//!
//! <function-class> = <member-function> E? // E designates a 64-bit 'this'
//!                                         // pointer. in 64-bit mode *all*
//!                                         // 'this' pointers are 64-bit.
//!                  | <global-function>
//!
//! <function-type> = <this-cvr-qualifier> <calling-convention>
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

use bitflags::bitflags;
use context::{Backrefs, Context};
use tokenizing::ColorScheme;

use crate::Colors;

#[cfg(test)]
const PRINTING_SCOPE: bool = true;

#[cfg(not(test))]
const PRINTING_SCOPE: bool = false;

pub fn parse(s: &str) -> Option<crate::TokenStream> {
    let mut ctx = Context::new(s);
    let mut backrefs = Backrefs::new();

    // llvm appears to generate a '.' prefix on some symbols
    ctx.eat(b'.');

    let sym = Symbol::parse(&mut ctx, &mut backrefs)?;
    sym.demangle(&mut ctx, &mut backrefs);

    #[cfg(test)]
    {
        dbg!(&sym);
        if !ctx.src().is_empty() {
            // panic!("not empty '{}'", ctx.src());
        }
    }

    Some(ctx.stream)
}

/// Converts an trivially printable node to a string.
trait Demangle<'a> {
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
trait PositionalDemangle<'a> {
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
struct TypeRef(u32);

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
    W64(Modifiers, Box<Type>),
    Int8(Modifiers),
    UInt8(Modifiers),
    Int16(Modifiers),
    UInt16(Modifiers),
    Int32(Modifiers),
    UInt32(Modifiers),
    Int64(Modifiers),
    UInt64(Modifiers),
    Int128(Modifiers),
    Uint128(Modifiers),
    Union(Modifiers, Path),
    Enum(Modifiers, Path),
    Struct(Modifiers, Path),
    Class(Modifiers, Path),
    Ref(Modifiers, Box<Type>),
    RValueRef(Modifiers, Box<Type>),
    Ptr(Modifiers, Box<Type>),
    Function(Function),
    MemberFunction(MemberFunction),
    MemberFunctionPtr(MemberFunctionPtr),
    Constant(isize),
    Variable(Variable),

    /// Renamed literal with additional modifiers.
    Typedef(Modifiers, Literal),

    /// String encoded using a format we don't know.
    Encoded(EncodedIdent),

    /// Array of a single type which can be on n dimensions.
    ///
    /// ```text
    /// int[20][10][5][..]
    /// ```
    Array(Arrays),

    /// ```text
    /// template-parameter-<idx>
    /// ```
    TemplateParameterIdx(isize),

    /// Virtual function table.
    VFTable(Qualifiers, Option<Scope>),

    /// Virtual base table.
    VBTable(Qualifiers, Option<Scope>),

    /// ```text
    /// ???
    /// ```
    VCallThunk(isize, CallingConv),

    /// ```text
    /// extern "C"
    /// ```
    Extern(Box<Type>),

    /// ```text
    /// fn(a, b, c, ...)
    /// ```
    Variadic,

    /// ```text
    /// & <member-function-ptr>
    /// ```
    Inherited(MemberFunctionPtr),
}

impl Parse for Type {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        match ctx.peek_slice(..2) {
            Some(b"W4") => {
                ctx.offset += 2;
                ctx.memorizing = false;

                let name = Path::parse(ctx, backrefs)?;
                return Some(Type::Enum(ctx.pop_modifiers(), name));
            }
            Some(b"A6") => {
                ctx.offset += 2;
                ctx.parsing_qualifiers = false;

                let func = Function::parse(ctx, backrefs).map(Type::Function)?;
                return Some(Type::Ref(ctx.pop_modifiers(), Box::new(func)));
            }
            Some(b"P6") => {
                ctx.offset += 2;
                ctx.parsing_qualifiers = false;

                let func = Function::parse(ctx, backrefs).map(Type::Function)?;
                return Some(Type::Ptr(ctx.pop_modifiers(), Box::new(func)));
            }
            Some(b"P8") => {
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
                    return Some(Type::Typedef(ctx.pop_modifiers(), name));
                }

                if ctx.eat(b'T') {
                    return Some(Type::Nullptr);
                }

                if ctx.eat(b'Q') {
                    ctx.parsing_qualifiers = false;

                    let func = Function::parse(ctx, backrefs).map(Type::Function)?;
                    return Some(Type::RValueRef(ctx.pop_modifiers(), Box::new(func)));
                }

                if ctx.eat_slice(b"BY") {
                    return Arrays::parse(ctx, backrefs).map(Type::Array);
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
                    let quali = Qualifiers::parse(ctx, backrefs)?;
                    ctx.push_modifiers(quali.0);
                }
            }

            if ctx.eat(b'S') {
                return Some(Type::Unit);
            }

            // single inheritance member function
            if let Some(b'1') = ctx.peek() {
                ctx.offset += 1;
                ctx.consume(b'?')?;

                ctx.parsing_qualifiers = false;

                // `Inherited` is just an alias for a reference
                // but since wrapping a function in a pointer type changes the formatting
                // to that of a function pointer, a new type has to be made
                return Some(Type::Inherited(MemberFunctionPtr::parse(ctx, backrefs)?));
            }

            // multiple inheritance member function
            if let Some(b'H' | b'I' | b'J') = ctx.peek() {
                ctx.offset += 1;
                ctx.consume(b'?')?;

                ctx.parsing_qualifiers = false;

                // `Inherited` is just an alias for a reference
                // but since wrapping a function in a pointer type changes the formatting
                // to that of a function pointer, a new type has to be made
                let tipe = MemberFunctionPtr::parse(ctx, backrefs).map(Type::Inherited);

                let _thunk_offset = ctx.number()?;
                return tipe;
            }
        }

        if ctx.eat(b'?') {
            let idx = ctx.number()?;
            return Some(Type::TemplateParameterIdx(idx));
        }

        if let Some(digit) = ctx.base10() {
            return backrefs.get_memorized_param(digit);
        }

        let modi = ctx.pop_modifiers();
        let tipe = match ctx.take()? {
            b'T' => Type::Union(modi, Path::parse(ctx, backrefs)?),
            b'U' => Type::Struct(modi, Path::parse(ctx, backrefs)?),
            b'V' => Type::Class(modi, Path::parse(ctx, backrefs)?),
            b'A' => Type::Ref(modi, Box::new(Pointee::parse(ctx, backrefs)?.0)),
            b'B' => Type::Ref(
                Modifiers::VOLATILE,
                Box::new(Pointee::parse(ctx, backrefs)?.0),
            ),
            b'P' => Type::Ptr(modi, Box::new(Pointee::parse(ctx, backrefs)?.0)),
            b'Q' => Type::Ptr(Modifiers::CONST, Box::new(Pointee::parse(ctx, backrefs)?.0)),
            b'R' => Type::Ptr(
                Modifiers::VOLATILE,
                Box::new(Pointee::parse(ctx, backrefs)?.0),
            ),
            b'S' => Type::Ptr(
                Modifiers::CONST | Modifiers::VOLATILE,
                Box::new(Pointee::parse(ctx, backrefs)?.0),
            ),
            b'Y' => Type::Array(Arrays::parse(ctx, backrefs)?),
            b'X' => Type::Void(modi),
            b'D' => Type::Char(modi),
            b'C' => Type::IChar(modi),
            b'E' => Type::UChar(modi),
            b'F' => Type::IShort(modi),
            b'G' => Type::UShort(modi),
            b'H' => Type::Int(modi),
            b'I' => Type::UInt(modi),
            b'J' => Type::Long(modi),
            b'K' => Type::ULong(modi),
            b'M' => Type::Float(modi),
            b'N' => Type::Double(modi),
            b'O' => Type::LDouble(modi),
            b'_' => match ctx.take()? {
                b'$' => Type::W64(modi, Box::new(Type::parse(ctx, backrefs)?)),
                b'D' => Type::Int8(modi),
                b'E' => Type::UInt8(modi),
                b'F' => Type::Int16(modi),
                b'G' => Type::UInt16(modi),
                b'H' => Type::Int32(modi),
                b'I' => Type::UInt32(modi),
                b'J' => Type::Int64(modi),
                b'K' => Type::UInt64(modi),
                b'L' => Type::Int128(modi),
                b'M' => Type::Uint128(modi),
                b'N' => Type::Bool(modi),
                b'W' => Type::WChar(modi),
                b'Q' => Type::Char8(modi),
                b'S' => Type::Char16(modi),
                b'U' => Type::Char32(modi),
                _ => return None,
            },
            _ => return None,
        };

        Some(tipe)
    }
}

impl<'a> Demangle<'a> for Type {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        self.demangle_pre(ctx, backrefs);
        self.demangle_post(ctx, backrefs);
    }
}

impl<'a> PositionalDemangle<'a> for Type {
    fn demangle_pre(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        match self {
            Type::Unit => {}
            Type::Nullptr => ctx.stream.push("std::nullptr_t", Colors::known()),
            Type::Void(modi) => {
                ctx.stream.push("void", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Char(modi) => {
                ctx.stream.push("char", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Char8(modi) => {
                ctx.stream.push("char8_t", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Char16(modi) => {
                ctx.stream.push("char16_t", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Char32(modi) => {
                ctx.stream.push("char32_t", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::IChar(modi) => {
                ctx.stream.push("signed char", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::UChar(modi) => {
                ctx.stream.push("unsigned char", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::WChar(modi) => {
                ctx.stream.push("wchar_t", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::IShort(modi) => {
                ctx.stream.push("short", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::UShort(modi) => {
                ctx.stream.push("unsigned short", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Int(modi) => {
                ctx.stream.push("int", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::UInt(modi) => {
                ctx.stream.push("unsigned int", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Float(modi) => {
                ctx.stream.push("float", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Double(modi) => {
                ctx.stream.push("double", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::LDouble(modi) => {
                ctx.stream.push("long double", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Long(modi) => {
                ctx.stream.push("long", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::ULong(modi) => {
                ctx.stream.push("unsigned long", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::W64(modi, tipe) => {
                ctx.stream.push("__w64 ", Colors::known());
                tipe.demangle(ctx, backrefs);
                modi.demangle(ctx, backrefs);
            }
            Type::Int8(modi) => {
                ctx.stream.push("__int8", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::UInt8(modi) => {
                ctx.stream.push("unsigned __int8", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Int16(modi) => {
                ctx.stream.push("__int16", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::UInt16(modi) => {
                ctx.stream.push("unsigned __int16", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Int32(modi) => {
                ctx.stream.push("__int32", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::UInt32(modi) => {
                ctx.stream.push("unsigned __int32", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Int64(modi) => {
                ctx.stream.push("__int64", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::UInt64(modi) => {
                ctx.stream.push("unsigned __int64", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Int128(modi) => {
                ctx.stream.push("__int128", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Uint128(modi) => {
                ctx.stream.push("unsigned __int128", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Bool(modi) => {
                ctx.stream.push("bool", Colors::known());
                modi.demangle(ctx, backrefs);
            }
            Type::Union(modi, name) => {
                ctx.stream.push("union ", Colors::known());
                name.demangle(ctx, backrefs);
                modi.demangle(ctx, backrefs);
            }
            Type::Enum(modi, name) => {
                ctx.stream.push("enum ", Colors::known());
                name.demangle(ctx, backrefs);
                modi.demangle(ctx, backrefs);
            }
            Type::Struct(modi, name) => {
                ctx.stream.push("struct ", Colors::known());
                name.demangle(ctx, backrefs);
                modi.demangle(ctx, backrefs);
            }
            Type::Class(modi, name) => {
                ctx.stream.push("class ", Colors::known());
                name.demangle(ctx, backrefs);
                modi.demangle(ctx, backrefs);
            }
            Type::Ptr(modi, tipe) | Type::Ref(modi, tipe) | Type::RValueRef(modi, tipe) => {
                // "[]" and "()" (for function parameters) take precedence over "*",
                // so "int *x(int)" means "x is a function returning int *". We need
                // parentheses to supercede the default precedence. (e.g. we want to
                // emit something like "int (*x)(int)".)

                match &**tipe {
                    Type::Function(func) => {
                        func.return_type.demangle_pre(ctx, backrefs);
                        ctx.stream.push("(", Colors::brackets());
                        func.calling_conv.demangle(ctx, backrefs);
                    }
                    Type::MemberFunction(func) => {
                        func.storage_scope.demangle(ctx, backrefs);
                        func.return_type.demangle_pre(ctx, backrefs);
                        func.calling_conv.demangle(ctx, backrefs);
                    }
                    Type::MemberFunctionPtr(func) => {
                        func.storage_scope.demangle(ctx, backrefs);
                        func.return_type.demangle_pre(ctx, backrefs);
                        ctx.stream.push("(", Colors::brackets());
                        func.calling_conv.demangle(ctx, backrefs);
                    }
                    Type::Array(..) => {
                        tipe.demangle_pre(ctx, backrefs);
                        ctx.stream.push(" (", Colors::brackets());
                    }
                    _ => tipe.demangle_pre(ctx, backrefs),
                }

                match self {
                    Type::Ptr(..) => ctx.stream.push(" *", Colors::special()),
                    Type::Ref(..) => ctx.stream.push(" &", Colors::special()),
                    Type::RValueRef(..) => ctx.stream.push(" &&", Colors::special()),
                    _ => {}
                }

                modi.demangle(ctx, backrefs);
            }
            Type::Function(func) => {
                func.return_type.demangle_pre(ctx, backrefs);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push(" ", Colors::spacing());
            }
            Type::MemberFunction(func) => {
                func.storage_scope.demangle(ctx, backrefs);
                func.return_type.demangle_pre(ctx, backrefs);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push(" ", Colors::spacing());
            }
            Type::MemberFunctionPtr(func) => {
                func.storage_scope.demangle(ctx, backrefs);
                func.return_type.demangle_pre(ctx, backrefs);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push(" ", Colors::spacing());
                func.class_name.demangle(ctx, backrefs);
            }
            Type::Inherited(func) => {
                ctx.stream.push("&", Colors::special());
                func.storage_scope.demangle(ctx, backrefs);
                func.return_type.demangle_pre(ctx, backrefs);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push(" ", Colors::spacing());
                func.class_name.demangle(ctx, backrefs);
                func.params.demangle(ctx, backrefs);
            }
            Type::Constant(val) => {
                ctx.stream.push_string(val.to_string(), Colors::item());
            }
            Type::TemplateParameterIdx(idx) => {
                ctx.stream.push("`", Colors::brackets());
                ctx.stream.push("template-parameter", Colors::known());
                ctx.stream.push("-", Colors::delimiter());
                ctx.stream.push_string(idx.to_string(), Colors::item());
                ctx.stream.push("'", Colors::brackets());
            }
            Type::Typedef(modi, name) => {
                ctx.push_literal(name, Colors::item());
                modi.demangle(ctx, backrefs);
            }
            Type::Variable(Variable {
                storage,
                tipe,
                quali: modi,
            }) => {
                storage.demangle(ctx, backrefs);
                tipe.demangle_pre(ctx, backrefs);
                modi.demangle(ctx, backrefs);
                ctx.stream.push(" ", Colors::spacing());
            }
            Type::Encoded(_) => {}
            Type::Array(array) => {
                array.tipe.demangle_pre(ctx, backrefs);
            }
            Type::VFTable(quali, _) => {
                quali.demangle(ctx, backrefs);
            }
            Type::VBTable(quali, _) => {
                quali.demangle(ctx, backrefs);
            }
            Type::VCallThunk(_, calling_conv) => {
                ctx.stream.push("[: ", Colors::brackets());
                ctx.stream.push("thunk", Colors::known());
                ctx.stream.push("]: ", Colors::brackets());

                calling_conv.demangle(ctx, backrefs);
            }
            Type::Extern(tipe) => {
                ctx.stream.push("extern ", Colors::special());
                ctx.stream.push("\"", Colors::brackets());
                ctx.stream.push("C", Colors::item());
                ctx.stream.push("\" ", Colors::brackets());

                tipe.demangle_pre(ctx, backrefs);
            }
            Type::Variadic => {
                ctx.stream.push("...", Colors::item());
            }
        }
    }

    fn demangle_post(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        match self {
            Type::Ptr(_, tipe) | Type::Ref(_, tipe) => {
                match **tipe {
                    Type::Function(..) => ctx.stream.push(")", Colors::brackets()),
                    Type::MemberFunction(..) => ctx.stream.push(")", Colors::brackets()),
                    Type::MemberFunctionPtr(..) => ctx.stream.push(")", Colors::brackets()),
                    Type::Array(..) => ctx.stream.push(")", Colors::brackets()),
                    _ => {}
                }

                tipe.demangle_post(ctx, backrefs);
            }
            Type::Function(func) => {
                func.params.demangle(ctx, backrefs);
                func.quali.0.demangle(ctx, backrefs);
                func.return_type.demangle_post(ctx, backrefs);
            }
            Type::MemberFunction(func) => {
                func.params.demangle(ctx, backrefs);
                func.qualifiers.0.demangle(ctx, backrefs);
                func.return_type.demangle_post(ctx, backrefs);
            }
            Type::MemberFunctionPtr(func) => {
                func.params.demangle(ctx, backrefs);
                func.qualifiers.0.demangle(ctx, backrefs);
                func.return_type.demangle_post(ctx, backrefs);
            }
            Type::Variable(Variable { tipe, .. }) => tipe.demangle_post(ctx, backrefs),
            Type::Array(array) => {
                for len in array.lens.iter() {
                    ctx.stream.push("[", Colors::brackets());
                    ctx.stream.push_string(len.to_string(), Colors::annotation());
                    ctx.stream.push("]", Colors::brackets());
                }
            }
            Type::VBTable(_, scope) | Type::VFTable(_, scope) => match scope {
                Some(scope) if !scope.0.is_empty() => {
                    ctx.stream.push("{for `", Colors::brackets());
                    scope.demangle(ctx, backrefs);
                    ctx.stream.push("'}", Colors::brackets());
                }
                None => {
                    ctx.stream.push("{for ??}", Colors::brackets());
                }
                _ => {}
            },
            Type::VCallThunk(offset, _) => {
                ctx.stream.push("{{", Colors::brackets());
                ctx.stream.push_string(offset.to_string(), Colors::item());
                ctx.stream.push(", {{flat}}}}", Colors::brackets());
            }
            Type::Extern(tipe) => tipe.demangle_post(ctx, backrefs),
            Type::W64(_, tipe) => tipe.demangle_post(ctx, backrefs),
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
                let quali = Qualifiers::parse(ctx, backrefs)?;
                let scope = Scope::parse(ctx, backrefs);
                Type::VFTable(quali, scope)
            }
            // virtual base table
            b'7' => {
                let quali = Qualifiers::parse(ctx, backrefs)?;
                let scope = Scope::parse(ctx, backrefs);
                Type::VBTable(quali, scope)
            }
            // RTTI's don't have a type
            b'8' => Type::Unit,
            // C style function don't have a type
            b'9' => Type::Unit,
            // anonymous function
            b'Y' => {
                ctx.parsing_qualifiers = false;
                Function::parse(ctx, backrefs).map(Type::Function)?
            }
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

                    let tipe = SymbolType::parse(ctx, backrefs)?.0;
                    Type::Extern(Box::new(tipe))
                }
                b'B' => {
                    let offset = ctx.number()?;
                    ctx.consume(b'A')?;
                    let calling_conv = CallingConv::parse(ctx, backrefs)?;
                    Type::VCallThunk(offset, calling_conv)
                }
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
/// <type-encoding> = <storage-class> <cvr-qualifier> <nested-path>
/// <storage-class> = 0 // private static member
///                 | 1 // protected static member
///                 | 2 // public static member
///                 | 3 // global
///                 | 4 // static local
/// ```
#[derive(Debug, PartialEq, Clone)]
struct Variable {
    storage: StorageVariable,
    quali: PointeeQualifiers,
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
            _ => return None,
        };

        let mut quali = Modifiers::empty();

        // don't fully understand this one
        if ctx.eat(b'?') {
            quali |= Modifiers::parse(ctx, backrefs)?;
        }

        let tipe = Type::parse(ctx, backrefs)?;
        quali |= PointeeQualifiers::parse(ctx, backrefs)?.0;

        Some(Variable {
            storage,
            quali: PointeeQualifiers(quali),
            tipe: Box::new(tipe),
        })
    }
}

/// ```text
/// <function-type> = <pointee-cvr-qualifier> <calling-convention>
///                   <return-type> <parameters> <throw-spec>
/// ```
#[derive(Debug, PartialEq, Clone)]
struct Function {
    calling_conv: CallingConv,
    quali: PointeeQualifiers,
    return_type: Box<FunctionReturnType>,
    params: FunctionParameters,
}

impl Parse for Function {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let mut quali = Modifiers::empty();
        if ctx.parsing_qualifiers {
            quali = PointeeQualifiers::parse(ctx, backrefs)?.0;
        }

        let calling_conv = CallingConv::parse(ctx, backrefs)?;
        let return_type = FunctionReturnType::parse(ctx, backrefs)?;
        let params = FunctionParameters::parse(ctx, backrefs)?;

        Some(Function {
            calling_conv,
            quali: PointeeQualifiers(quali),
            return_type: Box::new(return_type),
            params,
        })
    }
}

/// ```text
/// <member-function> = static <calling-convention> <return-type> <function-parameters>
///                   | <storage-scope> <pointee-cvr-qualifier> <calling-convention>
///                     <return-type> <function-parameters>
/// ```
#[derive(Debug, PartialEq, Clone)]
struct MemberFunction {
    storage_scope: StorageScope,
    calling_conv: CallingConv,
    qualifiers: PointeeQualifiers,
    return_type: Box<FunctionReturnType>,
    params: FunctionParameters,
}

impl Parse for MemberFunction {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let storage_scope = StorageScope::parse(ctx, backrefs)?;
        let mut qualifiers = PointeeQualifiers(Modifiers::empty());

        if !storage_scope.contains(StorageScope::STATIC) {
            qualifiers = PointeeQualifiers::parse(ctx, backrefs)?;
        }

        let calling_conv = CallingConv::parse(ctx, backrefs)?;
        let modi = MemberReturnQualifiers::parse(ctx, backrefs)?.0;

        ctx.push_modifiers(modi);

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

/// ```text
/// <member-function-pointer> = <storage-scope> [<cvr-qualifier>] [<storage-scope>]
///                             <calling-convention> <member-return-modifiers>
///                             <return-type> <function-parameters>
///
/// <cvr-qualifier> = E <cvr-qualifier>       // ptr64
///                  | <pointee-cvr-qualifier> // ptr64 + pointee qualifiers
/// ```
#[derive(Debug, PartialEq, Clone)]
struct MemberFunctionPtr {
    storage_scope: StorageScope,
    class_name: Path,
    calling_conv: CallingConv,
    qualifiers: PointeeQualifiers,
    return_type: Box<FunctionReturnType>,
    params: FunctionParameters,
}

impl Parse for MemberFunctionPtr {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let class_name = Path::parse(ctx, backrefs)?;
        let mut quali = Modifiers::empty();
        let mut storage_scope = StorageScope::empty();

        if ctx.eat(b'E') {
            quali |= Modifiers::PTR64;
        }

        if ctx.parsing_qualifiers {
            quali |= PointeeQualifiers::parse(ctx, backrefs)?.0;
            quali |= Modifiers::PTR64;
        } else {
            storage_scope = StorageScope::parse(ctx, backrefs)?;
        }

        let calling_conv = CallingConv::parse(ctx, backrefs)?;
        let modi = MemberReturnQualifiers::parse(ctx, backrefs)?.0;

        ctx.push_modifiers(modi);

        let return_type = FunctionReturnType::parse(ctx, backrefs)?;
        let params = FunctionParameters::parse(ctx, backrefs)?;

        Some(MemberFunctionPtr {
            storage_scope,
            class_name,
            calling_conv,
            qualifiers: PointeeQualifiers(quali),
            return_type: Box::new(return_type),
            params,
        })
    }
}

/// ```text
/// <array> = <pointee-cvr-qualifier> <number>{dimensions} <type>
/// <dimensions> = <number>
/// ```
#[derive(Debug, Clone, PartialEq)]
struct Arrays {
    modifiers: Modifiers,
    tipe: Box<Type>,
    lens: Vec<usize>,
}

impl Parse for Arrays {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let dimensions = ctx.number()?;
        if dimensions < 0 {
            return None;
        }

        let modifiers = ctx.pop_modifiers();
        let mut lens = Vec::with_capacity(1);
        for _ in 0..dimensions {
            let len = ctx.number()?;
            lens.push(len as usize);
        }

        let tipe = Type::parse(ctx, backrefs)?;
        Some(Arrays {
            modifiers,
            tipe: Box::new(tipe),
            lens,
        })
    }
}

/// ```text
/// <pointee> = <cvr-qualifier> <type>
///           | E <cvr-qualifier> <type> // ptr64 + pointee qualifiers
/// ```
#[derive(Debug, PartialEq, Clone)]
struct Pointee(Type);

impl Parse for Pointee {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let mut modi = Modifiers::empty();

        if ctx.eat(b'E') {
            modi = Modifiers::PTR64;
        }

        modi |= Modifiers::parse(ctx, backrefs)?;
        ctx.push_modifiers(modi);

        Type::parse(ctx, backrefs).map(Pointee)
    }
}

#[derive(Debug, PartialEq, Clone)]
struct FunctionReturnType(Type);

impl Parse for FunctionReturnType {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        ctx.pop_modifiers();

        if ctx.eat(b'?') {
            let modi = match ctx.take()? {
                b'A' => Modifiers::empty(),
                b'B' => Modifiers::CONST,
                b'C' => Modifiers::VOLATILE,
                b'D' => Modifiers::CONST | Modifiers::VOLATILE,
                _ => return None,
            };

            ctx.push_modifiers(modi);
        }

        if ctx.eat(b'@') {
            return Some(FunctionReturnType(Type::Unit));
        }

        Type::parse(ctx, backrefs).map(FunctionReturnType)
    }
}

impl<'a> PositionalDemangle<'a> for FunctionReturnType {
    fn demangle_pre(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        self.0.demangle_pre(ctx, backrefs);
        if self.0 != Type::Unit {
            ctx.stream.push(" ", Colors::spacing());
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
    TypeCast,
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
    DynamicAtExitDtor(Box<Symbol>),
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
            b'B' => Intrinsics::TypeCast,
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
                        let tipe = Type::parse(ctx, backrefs)?;
                        let modi = Modifiers::parse(ctx, backrefs)?;
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
                        let sym = match Symbol::parse(ctx, backrefs) {
                            Some(sym) => {
                                // consume optional suffix
                                ctx.consume(b'@');
                                sym
                            }
                            None => {
                                // not sure what this is.
                                // it appears to be a literal from what the ghidra tests indicate
                                Literal::parse(ctx, backrefs)
                                    .map(NestedPath::Literal)
                                    .map(NestedPath::into)
                                    .map(Path::into)?
                            }
                        };

                        Intrinsics::DynamicInitializer(Box::new(sym))
                    }
                    b'F' => {
                        let sym = match Symbol::parse(ctx, backrefs) {
                            Some(sym) => {
                                // consume optional suffix
                                ctx.consume(b'@');
                                sym
                            }
                            None => {
                                // not sure what this is.
                                // it appears to be a literal from what the ghidra tests indicate
                                Literal::parse(ctx, backrefs)
                                    .map(NestedPath::Literal)
                                    .map(NestedPath::into)
                                    .map(Path::into)?
                            }
                        };

                        Intrinsics::DynamicAtExitDtor(Box::new(sym))
                    }
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

impl<'a> Demangle<'a> for Intrinsics {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        let literal = match *self {
            Intrinsics::Ctor => {
                match ctx.scope.0.first() {
                    Some(path) => path.demangle(ctx, backrefs),
                    _ => {
                        ctx.stream.push("`", Colors::brackets());
                        ctx.stream.push("unnamed constructor", Colors::known());
                        ctx.stream.push("'", Colors::brackets());
                    }
                };
                return;
            }
            Intrinsics::Dtor => {
                ctx.stream.push("~", Colors::item());

                match ctx.scope.0.first() {
                    Some(path) => path.demangle(ctx, backrefs),
                    _ => {
                        ctx.stream.push("`", Colors::brackets());
                        ctx.stream.push("unnamed destructor", Colors::known());
                        ctx.stream.push("'", Colors::brackets());
                    }
                };
                return;
            }
            Intrinsics::DynamicInitializer(ref tipe) => {
                ctx.stream.push("`", Colors::brackets());
                ctx.stream.push("dynamic initializer for ", Colors::known());
                ctx.stream.push("'", Colors::brackets());

                tipe.demangle(ctx, backrefs);
                ctx.stream.push("''", Colors::brackets());
                return;
            }
            Intrinsics::DynamicAtExitDtor(ref tipe) => {
                ctx.stream.push("`", Colors::brackets());
                ctx.stream.push("dynamic atexit destructor for ", Colors::known());
                ctx.stream.push("'", Colors::brackets());

                tipe.demangle(ctx, backrefs);
                ctx.stream.push("''", Colors::brackets());
                return;
            }
            Intrinsics::SourceName(ref src) => {
                ctx.push_literal(src, Colors::item());
                return;
            }
            Intrinsics::RTTITypeDescriptor(_, ref tipe) => {
                tipe.demangle(ctx, backrefs);
                ctx.stream.push(" `", Colors::brackets());
                ctx.stream.push("RTTI Type Descriptor", Colors::known());
                ctx.stream.push("'", Colors::brackets());
                return;
            }
            Intrinsics::RTTIBaseClassDescriptor {
                nv_off,
                ptr_off,
                vbtable_off,
                flags,
            } => {
                ctx.stream.push("`", Colors::brackets());
                ctx.stream.push("RTTI Base Class Descriptor at ", Colors::known());
                ctx.stream.push("(", Colors::brackets());

                ctx.stream.push_string(nv_off.to_string(), Colors::annotation());
                ctx.stream.push(", ", Colors::brackets());

                ctx.stream.push_string(ptr_off.to_string(), Colors::annotation());
                ctx.stream.push(", ", Colors::brackets());

                ctx.stream.push_string(vbtable_off.to_string(), Colors::annotation());
                ctx.stream.push(", ", Colors::brackets());

                ctx.stream.push_string(flags.to_string(), Colors::annotation());
                ctx.stream.push(")'", Colors::brackets());
                return;
            }
            Intrinsics::RTTIBaseClassArray => {
                ctx.stream.push("`", Colors::brackets());
                ctx.stream.push("RTTI Base Class Array", Colors::known());
                ctx.stream.push("'", Colors::brackets());
                return;
            }
            Intrinsics::RTTIClassHierarchyDescriptor => {
                ctx.stream.push("`", Colors::brackets());
                ctx.stream.push("RTTI Class Hierarchy Descriptor", Colors::known());
                ctx.stream.push("'", Colors::brackets());
                return;
            }
            Intrinsics::RTTIClassCompleteObjectLocator => {
                ctx.stream.push("`", Colors::brackets());
                ctx.stream.push("RTTI Complete Object Locator", Colors::known());
                ctx.stream.push("'", Colors::brackets());
                return;
            }
            Intrinsics::TypeCast => "operatorcast",
            Intrinsics::New => "operator new",
            Intrinsics::Delete => "operator delete",
            Intrinsics::Assign => "operator=",
            Intrinsics::ShiftRight => "operator>>",
            Intrinsics::ShiftRightEquals => "operator>>=",
            Intrinsics::ShiftLeft => "operator<<",
            Intrinsics::ShiftLeftEquals => "operator<<=",
            Intrinsics::LogicalNot => "operator!",
            Intrinsics::Equals => "operator==",
            Intrinsics::NotEquals => "operator!=",
            Intrinsics::Array => "operatorcast",
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
            Intrinsics::LocalStaticThreadGuard => "`local static thread guard'",
            Intrinsics::PlacementDeleteClosure => "`placement delete closure'",
            Intrinsics::PlacementDeleteArrayClosure => "`placement delete[] closure'",
            Intrinsics::NewArray => "operator new[]",
            Intrinsics::DeleteArray => "operator delete[]",
            Intrinsics::CoAwait => "co_await",
            Intrinsics::Spaceship => "operator<=>",
        };

        // TODO: handle each cases colors individually
        ctx.stream.push(literal, Colors::known());
    }
}

/// ```text
/// <parameters> = X // void
///              | <type>+ @
///              | <type>* Z // variable args
/// ```
#[derive(Debug, Clone, PartialEq)]
struct Parameters(Vec<Type>);

impl Parse for Parameters {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let mut types = Vec::new();

        loop {
            // list ending is encountered or a variadic type.
            if ctx.eat(b'Z') {
                // parameter lists can only have a variadic as it's last argument
                // therefore two Z's means a variadic type at the end which doesn't get memorized
                if ctx.eat(b'Z') {
                    types.push(Type::Variadic);
                }

                break;
            }

            // either the stream is consumed or an list ending is encountered.
            if ctx.eat(b'@') || ctx.src().is_empty() {
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
                backrefs.memorize_param(&tipe);
            }

            types.push(tipe);
        }

        if types.is_empty() {
            return None;
        }

        Some(Parameters(types))
    }
}

impl<'a> Demangle<'a> for Parameters {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        let mut params = self.0.iter();

        if let Some(param) = params.next() {
            param.demangle(ctx, backrefs);
        }

        for param in params {
            ctx.stream.push(", ", Colors::delimiter());
            param.demangle(ctx, backrefs);
        }
    }
}

/// ```text
/// <function-parameters> = <argument-list> <throw-spec>
/// ```
#[derive(Debug, PartialEq, Clone)]
struct FunctionParameters(Parameters);

impl Parse for FunctionParameters {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let params = Parameters::parse(ctx, backrefs)?;

        if !ctx.eat(b'Z') || !ctx.eat_slice(b"_E") {
            // TODO: handle throw attribute
        }

        Some(FunctionParameters(params))
    }
}

impl<'a> Demangle<'a> for FunctionParameters {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        ctx.stream.push("(", Colors::brackets());
        self.0.demangle(ctx, backrefs);
        ctx.stream.push(")", Colors::brackets());
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
    Anonymous,
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
            b'K' => CallingConv::Anonymous,
            _ => return None,
        };

        Some(conv)
    }
}

impl<'a> Demangle<'a> for CallingConv {
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
            CallingConv::Anonymous => return,
        };

        ctx.stream.push(literal, Colors::root());
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

impl<'a> Demangle<'a> for StorageVariable {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        if PRINTING_SCOPE {
            let literal = match self {
                StorageVariable::PrivateStatic => "private: static ",
                StorageVariable::ProtectedStatic => "protected: static ",
                StorageVariable::PublicStatic => "public: static ",
                StorageVariable::Global | StorageVariable::FunctionLocalStatic => return,
            };

            ctx.stream.push(literal, Colors::root());
        }
    }
}

bitflags! {
    /// ```text
    /// <member-function> = A // private: near
    ///                   | B // private: far
    ///                   | C // private: static near
    ///                   | D // private: static far
    ///                   | E // private: virtual near
    ///                   | F // private: virtual far
    ///                   | I // protected: near
    ///                   | J // protected: far
    ///                   | K // protected: static near
    ///                   | L // protected: static far
    ///                   | M // protected: virtual near
    ///                   | N // protected: virtual far
    ///                   | Q // public: near
    ///                   | R // public: far
    ///                   | S // public: static near
    ///                   | T // public: static far
    ///                   | U // public: virtual near
    ///                   | V // public: virtual far
    ///
    /// <global-function> = Y // global near
    ///                   | Z // global far
    /// ```
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

impl<'a> Demangle<'a> for StorageScope {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        let color = Colors::root();

        if PRINTING_SCOPE {
            if self.contains(StorageScope::PUBLIC) {
                ctx.stream.push("public: ", color);
            }

            if self.contains(StorageScope::PRIVATE) {
                ctx.stream.push("private: ", color);
            }

            if self.contains(StorageScope::PROTECTED) {
                ctx.stream.push("protected: ", color);
            }
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
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

impl<'a> Demangle<'a> for Modifiers {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        let color = Colors::annotation();

        if self.contains(Modifiers::CONST) {
            ctx.stream.push(" const", color);
        }

        if self.contains(Modifiers::VOLATILE) {
            ctx.stream.push(" volatile", color);
        }
    }
}

/// ```text
/// <cvr-qualifier> = B // const
///                 = C // volatile
///                 = D // const volatile
///                 = A // no qualifier
/// ```
#[derive(Debug, Clone, PartialEq)]
struct MemberReturnQualifiers(Modifiers);

impl Parse for MemberReturnQualifiers {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        if !ctx.eat(b'?') {
            return Some(MemberReturnQualifiers(Modifiers::empty()));
        }

        let modi = match ctx.take()? {
            b'A' => Modifiers::empty(),
            b'B' => Modifiers::CONST,
            b'C' => Modifiers::VOLATILE,
            b'D' => Modifiers::CONST | Modifiers::VOLATILE,
            _ => return None,
        };

        Some(MemberReturnQualifiers(modi))
    }
}

/// ```text
/// <cvr-qualifier> = B // const
///                 = R // const
///                 = C // volatile
///                 = S // volatile
///                 = B // volatile
///                 = D // const volatile
///                 = T // const volatile
///                 = A // no qualifier
///                 = Q // no qualifier
/// ```
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

impl<'a> Demangle<'a> for Qualifiers {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        let color = Colors::annotation();

        if self.0.contains(Modifiers::CONST) {
            ctx.stream.push("const ", color);
        }

        if self.0.contains(Modifiers::VOLATILE) {
            ctx.stream.push("volatile ", color);
        }

        if self.0.contains(Modifiers::FAR) {
            ctx.stream.push("__far ", color);
        }

        if self.0.contains(Modifiers::UNALIGNED) {
            ctx.stream.push("__unaligned ", color);
        }

        if self.0.contains(Modifiers::RESTRICT) {
            ctx.stream.push("__restrict ", color);
        }

        if self.0.contains(Modifiers::LVALUE) {
            ctx.stream.push("& ", Colors::special());
        }

        if self.0.contains(Modifiers::RVALUE) {
            ctx.stream.push("&& ", Colors::special());
        }
    }
}

/// ```text
/// <pointee-cvr-qualifier> = <pointee-cvr-qualifier>{0,4} <cvr-qualifier>
///
/// <pointee-cvr-qualifier> = E // ptr64
///                         = I // __restrict
///                         = F // __unaligned
///                         = G // &
///                         = H // &&
/// ```
#[derive(Debug, Clone, PartialEq)]
struct PointeeQualifiers(Modifiers);

impl Parse for PointeeQualifiers {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let mut quali = Modifiers::empty();

        // there can be up to 4 pointer qualifiers
        for _ in 0..4 {
            let addi = match ctx.peek() {
                Some(b'E') => Modifiers::PTR64,
                Some(b'I') => Modifiers::RESTRICT,
                Some(b'F') => Modifiers::UNALIGNED,
                Some(b'G') => Modifiers::LVALUE,
                Some(b'H') => Modifiers::RVALUE,
                _ => break,
            };

            // a pointer cannot be qualified as both a lvalue and rvalue reference
            if (quali | addi).contains(Modifiers::LVALUE | Modifiers::RVALUE) {
                break;
            }

            quali |= addi;
            ctx.offset += 1;
        }

        // concat generic qualifiers
        quali |= Qualifiers::parse(ctx, backrefs)?.0;

        Some(PointeeQualifiers(quali))
    }
}

impl<'a> Demangle<'a> for PointeeQualifiers {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        let color = Colors::annotation();

        if self.0.contains(Modifiers::CONST) {
            ctx.stream.push(" const", color);
        }

        if self.0.contains(Modifiers::VOLATILE) {
            ctx.stream.push(" volatile", color);
        }

        if self.0.contains(Modifiers::FAR) {
            ctx.stream.push(" __far", color);
        }

        if self.0.contains(Modifiers::UNALIGNED) {
            ctx.stream.push(" __unaligned", color);
        }

        if self.0.contains(Modifiers::RESTRICT) {
            ctx.stream.push(" __restrict", color);
        }

        if self.0.contains(Modifiers::LVALUE) {
            ctx.stream.push(" &", Colors::special());
        }

        if self.0.contains(Modifiers::RVALUE) {
            ctx.stream.push(" &&", Colors::special());
        }
    }
}

/// Literals are indices into a mangled string.
/// Unlike calling [`Context::ident`], a literal memorizes a
/// parsed item as a [`NestedPath::Literal`].
#[derive(Default, Debug, Clone, Copy, PartialEq)]
struct Literal {
    start: usize,
    end: usize,
}

impl Parse for Literal {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let ident = ctx.ident()?;
        if ctx.memorizing {
            backrefs.memorize_path(&NestedPath::Literal(ident));
        }
        Some(ident)
    }
}

impl Literal {
    fn len(&self) -> usize {
        self.end - self.start
    }
}

/// ```text
/// <md5> = <base-16-digit>{32} @
/// ```
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

            Literal {
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

impl<'a> Demangle<'a> for MD5 {
    fn demangle(&'a self, ctx: &mut Context<'a>, _: &mut Backrefs) {
        ctx.stream.push("??@", Colors::brackets());
        ctx.push_literal(&self.0, Colors::item());
        ctx.stream.push("@", Colors::brackets());
    }
}

/// ```text
/// <scope> = {<nested-path>}* @
/// ```
#[derive(Debug, Default, PartialEq, Clone)]
struct Scope(Vec<NestedPath>);

impl Parse for Scope {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        let mut paths = Vec::new();

        while !ctx.eat(b'@') {
            let segment = NestedPath::parse(ctx, backrefs)?;

            // ignore disambiguators
            if let NestedPath::Disambiguator(..) = segment {
                continue;
            }

            paths.push(segment);
        }

        Some(Scope(paths))
    }
}

impl<'a> Demangle<'a> for Scope {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        for (idx, part) in self.0.iter().rev().enumerate() {
            part.demangle(ctx, backrefs);

            if idx != self.0.len() - 1 {
                ctx.stream.push("::", Colors::delimiter());
            }
        }
    }
}

/// Parses a symbol name in the form of A@B@C@@ which represents C::B::A.
/// Where C is the name and B::A is the scope.
///
/// ```text
/// <path> = <unqualified-path> <scope>
/// ```
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

impl<'a> Demangle<'a> for Path {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        self.scope.demangle(ctx, backrefs);

        if !self.scope.0.is_empty() {
            ctx.stream.push("::", Colors::delimiter());
        }

        self.name.0.demangle(ctx, backrefs);
    }
}

/// ```text
///  <nested-path> = <substitution>
///                | ?? <symbol>
///                | ?$ <template>
///                | ?A [<disambiguator>] @ <anonymous-namespace>
///                | Q <nested-path> // interface
///                | I <nested-path> // interface
///                | <disambiguator>
/// ```
#[derive(Debug, Clone, PartialEq)]
enum NestedPath {
    Literal(Literal),
    Interface(Box<NestedPath>),
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
            return backrefs.get_memorized_path(digit);
        }

        if ctx.eat(b'?') {
            ctx.ascent();
            return match ctx.peek()? {
                b'?' => Symbol::parse(ctx, backrefs).map(Box::new).map(NestedPath::Symbol),
                b'$' => {
                    ctx.offset += 1;

                    let template = Template::parse(ctx, backrefs).map(NestedPath::Template)?;
                    backrefs.memorize_path(&template);
                    Some(template)
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

                        backrefs.memorize_path(&NestedPath::Literal(Literal {
                            start: ctx.offset,
                            end: ctx.offset + len,
                        }));
                    }

                    ctx.consume(b'@')?;
                    Some(NestedPath::Anonymous)
                }
                b'Q' | b'I' => {
                    ctx.offset += 1;

                    let path = NestedPath::parse(ctx, backrefs)?;
                    Some(NestedPath::Interface(Box::new(path)))
                }
                _ => {
                    let disambiguator = ctx.number()?;
                    Some(NestedPath::Disambiguator(disambiguator))
                }
            };
        }

        let ident = ctx.ident()?;
        backrefs.memorize_path(&NestedPath::Literal(ident));

        ctx.ascent();
        Some(NestedPath::Literal(ident))
    }
}

impl<'a> Demangle<'a> for NestedPath {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        match self {
            NestedPath::Literal(ident) => ctx.push_literal(ident, Colors::item()),
            NestedPath::Interface(ident) => {
                ctx.stream.push("[", Colors::brackets());
                ident.demangle(ctx, backrefs);
                ctx.stream.push("]", Colors::brackets());
            }
            NestedPath::Template(template) => template.demangle(ctx, backrefs),
            NestedPath::Intrinsics(int) => int.demangle(ctx, backrefs),
            NestedPath::Symbol(inner) => inner.demangle(ctx, backrefs),
            NestedPath::Disambiguator(val) => {
                ctx.stream.push("`", Colors::brackets());
                ctx.stream.push_string(val.to_string(), Colors::item());
                ctx.stream.push("'", Colors::brackets());
            }
            NestedPath::MD5(md5) => md5.demangle(ctx, backrefs),
            NestedPath::Anonymous => {
                ctx.stream.push("`", Colors::brackets());
                ctx.stream.push("anonymous namespace", Colors::item());
                ctx.stream.push("'", Colors::brackets());
            }
        }
    }
}

/// ```text
///  <unqualified-path> = <substitution>
///                     | <intrinsic>
///                     | <source-name>
/// ```
#[derive(Debug, PartialEq, Clone)]
struct UnqualifiedPath(NestedPath);

impl Parse for UnqualifiedPath {
    fn parse(ctx: &mut Context, backrefs: &mut Backrefs) -> Option<Self> {
        ctx.descent()?;

        // return memorized ident
        if let Some(digit) = ctx.base10() {
            ctx.ascent();
            return backrefs.get_memorized_path(digit).map(UnqualifiedPath);
        }

        // special intrinsic
        if ctx.eat(b'?') {
            if ctx.eat(b'$') {
                ctx.ascent();
                let template = Template::parse(ctx, backrefs).map(NestedPath::Template)?;
                backrefs.memorize_path(&template);
                return Some(UnqualifiedPath(template));
            }

            ctx.ascent();
            return Intrinsics::parse(ctx, backrefs)
                .map(NestedPath::Intrinsics)
                .map(UnqualifiedPath);
        }

        // parse source name as a literal
        let name = ctx.ident()?;
        backrefs.memorize_path(&NestedPath::Literal(name));

        ctx.ascent();
        Some(UnqualifiedPath(NestedPath::Literal(name)))
    }
}

/// ```text
/// <encoded-ident> = <width> <length> <character>{0, max(length, width * 32)} [?]
///
/// <width> = <base-10-digit>
/// <length> = <number>
/// <character> = 0..=9
///             | a..=z
///             | _
///             | $
/// ```
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

/// ```text
/// <template-name> = <unqualified-path> <template-arg>+
///
/// <template-arg> = <type>
///                | <integer>
///                | <member-data-pointer>
///                | <member-function-pointer>
///                | $ <constant-value>
///                | <template-arg>+
/// ```
#[derive(Debug, PartialEq, Clone)]
struct Template {
    name: Box<UnqualifiedPath>,
    params: Parameters,
}

impl Parse for Template {
    fn parse(ctx: &mut Context, _: &mut Backrefs) -> Option<Self> {
        let mut temp = Backrefs::new();
        let name = UnqualifiedPath::parse(ctx, &mut temp)?;
        let params = Parameters::parse(ctx, &mut temp)?;

        Some(Template {
            name: Box::new(name),
            params,
        })
    }
}

impl<'a> Demangle<'a> for Template {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        self.name.0.demangle(ctx, backrefs);
        ctx.stream.push("<", Colors::annotation());
        self.params.demangle(ctx, backrefs);
        ctx.stream.push(">", Colors::annotation());
    }
}

/// Root node of the AST.
///
/// ```text
/// <symbol> = ??@ <md5>
///          | ?$TSS <static-guard>
///          | ?$ <template>
///          | <path> [<symbol-type>]
/// ```
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
            return Some(Path::into(path));
        }

        ctx.parsing_qualifiers = false;
        let tipe = SymbolType::parse(ctx, backrefs)?.0;

        ctx.ascent();
        Some(Symbol { path, tipe })
    }
}

impl<'a> Demangle<'a> for Symbol {
    fn demangle(&'a self, ctx: &mut Context<'a>, backrefs: &mut Backrefs) {
        ctx.scope = &self.path.scope;

        // weird typecasting of class member
        if let NestedPath::Intrinsics(Intrinsics::TypeCast) = self.path.name.0 {
            if let Type::MemberFunction(ref func) = self.tipe {
                func.storage_scope.demangle(ctx, backrefs);
                func.calling_conv.demangle(ctx, backrefs);
                ctx.stream.push(" ", Colors::spacing());
                self.path.scope.demangle(ctx, backrefs);
                ctx.stream.push("::", Colors::delimiter());
                ctx.stream.push("operator ", Colors::known());
                func.return_type.0.demangle(ctx, backrefs);
                func.params.demangle(ctx, backrefs);
                return;
            }
        }

        // weird typecasting of class member with templates
        if let NestedPath::Template(ref template) = self.path.name.0 {
            if let NestedPath::Intrinsics(Intrinsics::TypeCast) = template.name.0 {
                if let Type::MemberFunction(ref func) = self.tipe {
                    func.storage_scope.demangle(ctx, backrefs);
                    func.calling_conv.demangle(ctx, backrefs);
                    ctx.stream.push(" ", Colors::spacing());
                    self.path.scope.demangle(ctx, backrefs);
                    ctx.stream.push("::", Colors::delimiter());
                    ctx.stream.push("operator", Colors::known());
                    ctx.stream.push("<", Colors::annotation());
                    template.params.demangle(ctx, backrefs);
                    ctx.stream.push("> ", Colors::annotation());

                    func.return_type.0.demangle(ctx, backrefs);
                    func.params.demangle(ctx, backrefs);
                    return;
                }
            }
        }

        self.tipe.demangle_pre(ctx, backrefs);
        self.path.demangle(ctx, backrefs);
        self.tipe.demangle_post(ctx, backrefs);
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
