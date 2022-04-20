#![cfg(test)]

use crate::decode::Reader;
use std::fmt;
use std::mem::MaybeUninit;
use std::sync::atomic::Ordering;

#[derive(Debug, PartialEq, Eq)]
enum Error {
    UnknownPrefix,
    TooComplex,
    PathLengthNotNumber,
    ArgDelimiterNotFound,
    DecodingBase62Num,
    SymbolTooSmall,
    NotAscii,
    Invalid,
}

type ManglingResult<T> = Result<T, Error>;

// `MAX_COMPLEXITY` must be less than u16::MAX - 2 as Path::Generic's arguement lists
// addresses generic's in u16's.
const MAX_COMPLEXITY: usize = 256;
const MAX_DEPTH: usize = 100;
const MAX_GENERIC_ARGUEMENTS: usize = 16;

struct Symbol<'p> {
    ast: Stack<'p>,
    source: Reader<'p>,
    depth: usize,
}

impl<'p> Symbol<'p> {
    /// Demangle's a symbol.
    pub fn parse(s: &'p str) -> ManglingResult<Self> {
        let s = if s.starts_with("_R") {
            &s[2..]
        } else if s.starts_with('R') {
            // On Windows, dbghelp strips leading underscores, so we accept "R..."
            // form too.
            &s[1..]
        } else if s.starts_with("__R") {
            // On OSX, symbols are prefixed with an extra _
            &s[3..]
        } else {
            return Err(Error::UnknownPrefix);
        };

        if s.is_empty() {
            return Err(Error::Invalid);
        }

        // Only work with ascii text
        if s.bytes().any(|c| c & 0x80 != 0) {
            return Err(Error::NotAscii);
        }

        let mut res = Self { source: Reader::new(s.as_bytes()), ast: Stack::default(), depth: 0 };
        res.consume_path()?;

        Ok(res)
    }

    pub fn display(&self) -> String {
        let mut name = String::new();
        self.fmt(&mut name, &self.ast.stack[0]);
        name
    }

    fn fmt(&self, repr: &mut String, ty: &Type<'p>) {
        match ty {
            Type::Empty => unreachable!("{:#?}", &self.ast),
            Type::Basic(s) => repr.push_str(s),
            Type::Path(path) => match path {
                Path::Crate(_, ident) => {
                    repr.push_str(ident);
                }
                Path::Nested(_, path_idx, _disambiguator, ident) => {
                    self.fmt(repr, &self.ast.stack[*path_idx]);

                    repr.push_str("::");
                    repr.push_str(ident);
                }
                Path::Generic(path_idx, generics) => {
                    self.fmt(repr, &self.ast.stack[*path_idx]);

                    repr.push_str("::<");

                    for idx in 0..generics.len() {
                        const MAX_ARG_POS: u16 = u16::MAX - 1;
                        const EMPTY: u16 = u16::MAX;

                        match generics[idx] {
                            EMPTY => break,
                            MAX_ARG_POS => {
                                repr.push_str("...");
                                break;
                            }
                            generic_idx => {
                                self.fmt(repr, &self.ast.stack[generic_idx as usize]);

                                if idx != generics.len() - 1 && generics[idx + 1] != EMPTY {
                                    repr.push_str(", ");
                                }
                            }
                        }
                    }

                    repr.push('>');
                }
                Path::InherentImpl(_, impl_path_idx, type_idx) => {
                    repr.push('<');

                    self.fmt(repr, &self.ast.stack[*type_idx]);

                    repr.push_str(">::");
                    self.fmt(repr, &self.ast.stack[*impl_path_idx]);
                }
                Path::TraitImpl(_, impl_path_idx, type_idx, path_idx) => {
                    repr.push('<');

                    self.fmt(repr, &self.ast.stack[*type_idx]);
                    repr.push_str(" as ");
                    self.fmt(repr, &self.ast.stack[*path_idx]);

                    repr.push_str(">::");
                    self.fmt(repr, &self.ast.stack[*impl_path_idx]);
                }
                _ => todo!("{:?}", path),
            },
            Type::Array(type_idx, constant) => {
                repr.push('[');
                self.fmt(repr, &self.ast.stack[*type_idx]);
                repr.push_str("; ");

                match constant {
                    Const::Boolean(cond) => {
                        if *cond {
                            repr.push_str("true");
                        } else {
                            repr.push_str("false");
                        }
                    }
                    Const::Integar(num) => format_num(repr, *num),
                }

                repr.push(']');
            }
            Type::Slice(type_idx) => {
                repr.push('[');
                self.fmt(repr, &self.ast.stack[*type_idx]);
                repr.push(']');
            }
            Type::Tuple(type_indices) => {
                repr.push('(');

                for idx in 0..type_indices.len() {
                    self.fmt(repr, &self.ast.stack[type_indices[idx]]);

                    if idx != type_indices.len() - 1 {
                        repr.push_str(", ");
                    }
                }

                repr.push(')');
            }
            Type::Ref(opt_lifetime, type_idx) => {
                match opt_lifetime {
                    Some(Lifetime(0)) | None => repr.push('&'),
                    Some(lifetime) => {
                        repr.push_str("&'");

                        if let Some(formatted) = lifetime.fmt() {
                            repr.push(formatted);
                        } else {
                            format_num(repr, lifetime.0 as isize);
                        }

                        repr.push(' ');
                    }
                }

                self.fmt(repr, &self.ast.stack[*type_idx]);
            }
            Type::RefMut(opt_lifetime, type_idx) => {
                match opt_lifetime {
                    Some(Lifetime(0)) | None => repr.push_str("&mut "),
                    Some(lifetime) => {
                        repr.push_str("&'");

                        if let Some(formatted) = lifetime.fmt() {
                            repr.push(formatted);
                        } else {
                            format_num(repr, lifetime.0 as isize);
                        }

                        repr.push_str(" mut ");
                    }
                }

                self.fmt(repr, &self.ast.stack[*type_idx]);
            }
            Type::Pointer(type_idx) => {
                repr.push_str("*const ");
                self.fmt(repr, &self.ast.stack[*type_idx]);
            }
            Type::PointerMut(type_idx) => {
                repr.push_str("*mut ");
                self.fmt(repr, &self.ast.stack[*type_idx]);
            }
            Type::FnSig(_, is_unsafe, opt_ident, arg_indices, opt_return_idx) => {
                if *is_unsafe {
                    repr.push_str("unsafe ");
                }

                if let Some(ident) = opt_ident {
                    repr.push_str("fn ");
                    repr.push_str(ident);
                    repr.push('(');
                } else {
                    repr.push_str("fn(");
                }

                for idx in 0..arg_indices.len() {
                    self.fmt(repr, &self.ast.stack[arg_indices[idx]]);

                    if idx != arg_indices.len() - 1 {
                        repr.push_str(", ");
                    }
                }

                repr.push(')');

                if let Some(return_idx) = opt_return_idx {
                    repr.push_str(" -> ");
                    self.fmt(repr, &self.ast.stack[*return_idx]);
                }
            }
            Type::DynTrait(_, dyn_trait_indices, lifetime) => {
                repr.push_str("dyn ");

                for (trait_idx, assoc_binding_indices) in dyn_trait_indices {
                    self.fmt(repr, &self.ast.stack[*trait_idx]);

                    if !assoc_binding_indices.is_empty() {
                        repr.push('<');
                        for idx in 0..assoc_binding_indices.len() {
                            let (ident, type_idx) = &assoc_binding_indices[idx];

                            repr.push_str(ident);
                            repr.push_str(" = ");
                            self.fmt(repr, &self.ast.stack[*type_idx]);

                            if idx != assoc_binding_indices.len() - 1 {
                                repr.push_str(", ");
                            }
                        }
                        repr.push('>');
                    }
                }

                if let Some(formatted) = lifetime.fmt() {
                    repr.push_str(" + '");
                    repr.push(formatted);
                }
            }
        }
    }

    fn take_spot(&mut self) -> usize {
        let old = self.ast.ptr;
        self.ast.ptr += 1;
        old
    }

    fn consume_lifetime(&mut self) -> ManglingResult<Lifetime> {
        self.consume_base62().map(|v| Lifetime(v))
    }

    fn consume_base62(&mut self) -> ManglingResult<u64> {
        let mut num = 0u64;

        if self.source.take(b'_') {
            return Ok(num);
        }

        while let Some(chr) = self.source.consume() {
            if chr == b'_' {
                return num.checked_add(1).ok_or(Error::DecodingBase62Num);
            }

            let base_62_chr = match chr {
                b'0'..=b'9' => chr - b'0',
                b'a'..=b'z' => chr - b'a' + 10,
                b'A'..=b'Z' => chr - b'A' + 36,
                _ => return Err(Error::DecodingBase62Num),
            };

            num = num.checked_mul(62).ok_or(Error::DecodingBase62Num)?;
            num = num.checked_add(base_62_chr as u64).ok_or(Error::DecodingBase62Num)?;
        }

        Err(Error::DecodingBase62Num)
    }

    fn try_consume_disambiguator(&mut self) -> ManglingResult<Option<u64>> {
        if self.source.take(b's') {
            return Ok(Some(self.consume_base62()?));
        }

        Ok(None)
    }

    fn consume_ident(&mut self) -> ManglingResult<&'p str> {
        let s = unsafe { std::str::from_utf8_unchecked(self.source.inner()) };

        for (width, chr) in s.bytes().enumerate() {
            if !chr.is_ascii_digit() {
                return match usize::from_str_radix(&s[..width], 10) {
                    Err(_) => Err(Error::PathLengthNotNumber),
                    Ok(len) => {
                        self.source.offset((width + len) as isize);
                        Ok(&s[width..][..len])
                    }
                };
            }
        }

        Err(Error::Invalid)
    }

    fn consume_const(&mut self) -> ManglingResult<Const> {
        let prefix = self.source.seek_exact(2);
        let true_prefix = prefix == Some(b"1_");
        let false_prefix = prefix == Some(b"0_");

        let constant = if true_prefix | false_prefix {
            Const::Boolean(true_prefix)
        } else if self.source.take(b'n') {
            Const::Integar(-(self.consume_base62()? as isize))
        } else {
            Const::Integar(self.consume_base62()? as isize)
        };

        Ok(constant)
    }

    fn consume_path(&mut self) -> ManglingResult<()> {
        if self.depth == MAX_DEPTH {
            return Err(Error::TooComplex);
        }

        self.depth += 1;
        match self.source.consume().ok_or(Error::Invalid)? {
            b'C' => {
                // <identifier>

                let id = self.try_consume_disambiguator()?;
                let ident = self.consume_ident()?;

                self.ast.stack[self.ast.ptr] = Type::Path(Path::Crate(id, ident));
                self.ast.ptr += 1;
            }
            c @ (b'M' | b'X') => {
                // "M" <impl-path> <type>
                // "X" <impl-path> <type> <path>

                let id = self.try_consume_disambiguator()?;
                let spot = self.take_spot();

                let impl_path_spot = self.ast.ptr;
                self.consume_path()?;

                let type_spot = self.ast.ptr;
                self.consume_type()?;

                self.ast.stack[spot] = if c == b'X' {
                    let path_spot = self.ast.ptr;
                    self.consume_path()?;

                    Type::Path(Path::TraitImpl(id, impl_path_spot, type_spot, path_spot))
                } else {
                    Type::Path(Path::InherentImpl(id, impl_path_spot, type_spot))
                }
            }
            b'Y' => {
                // "Y" <type> <path>

                let spot = self.take_spot();

                let type_spot = self.ast.ptr;
                self.consume_type()?;

                let path_spot = self.ast.ptr;
                self.consume_path()?;

                self.ast.stack[spot] = Type::Path(Path::TraitDef(type_spot, path_spot));
            }
            b'N' => {
                // <namespace> <path> <identifier>

                let namespace = match self.source.consume() {
                    Some(b'v') => Namespace::Value,
                    Some(b't') => Namespace::Type,
                    _ => Namespace::Unknown,
                };

                let spot = self.take_spot();

                self.consume_path()?;

                let id = self.try_consume_disambiguator()?;
                let ident = self.consume_ident()?;

                self.ast.stack[spot] = Type::Path(Path::Nested(namespace, spot + 1, id, ident));
            }
            b'I' => {
                // <path> {<lifetime> <type>} "E"

                let mut generic_spots = [u16::MAX; MAX_GENERIC_ARGUEMENTS];
                let spot = self.take_spot();

                self.consume_path()?;

                for idx in 0.. {
                    if self.source.take(b'E') {
                        break;
                    }

                    if idx == MAX_GENERIC_ARGUEMENTS - 1 {
                        // Indicate that there are too many arguements to display.
                        generic_spots[MAX_GENERIC_ARGUEMENTS - 1] = u16::MAX - 1;
                        break;
                    }

                    let type_spot = self.ast.ptr;

                    self.consume_type()?;

                    generic_spots[idx] = type_spot as u16;
                }

                self.ast.stack[spot] = Type::Path(Path::Generic(spot + 1, generic_spots));

                // Consume and ignore optional unique id suffix.
                if self.source.take(b'c') {
                    self.consume_ident()?;
                }
            }
            b'B' => {
                // <base-62-number>

                let backref = self.consume_base62()? as usize - 2;
                let current = self.source.pos.load(Ordering::Acquire);

                self.source.pos.store(backref, Ordering::Relaxed);
                self.consume_type()?;
                self.source.pos.store(current, Ordering::Release);
            }
            b'E' => {}
            #[cfg(debug_assertions)]
            _ => panic!("{:#?}", &self.ast),
            _ => return Err(Error::Invalid),
        }

        Ok(())
    }

    fn consume_types(&mut self) -> ManglingResult<Vec<usize>> {
        let mut spots = Vec::new();
        while !self.source.take(b'E') {
            spots.push(self.ast.ptr);
            self.consume_type()?;
        }

        Ok(spots)
    }

    fn consume_type(&mut self) -> ManglingResult<()> {
        if self.depth == MAX_DEPTH {
            return Err(Error::TooComplex);
        }

        self.depth += 1;
        match self.source.consume().ok_or(Error::Invalid)? {
            b'A' => {
                // <type> <const>

                let spot = self.take_spot();
                self.consume_type()?;
                self.ast.stack[spot] = Type::Array(spot + 1, self.consume_const()?);
            }
            b'S' => {
                // <type>

                let spot = self.take_spot();
                self.consume_type()?;
                self.ast.stack[spot] = Type::Slice(spot + 1);
            }
            b'T' => {
                // {<type>} "E"

                let spot = self.take_spot();
                self.ast.stack[spot] = Type::Tuple(self.consume_types()?);
            }
            b'R' => {
                // [lifetime] <type>

                let mut lifetime = None;
                if self.source.take(b'L') {
                    lifetime = Some(self.consume_lifetime()?);
                }

                let spot = self.take_spot();
                self.consume_type()?;
                self.ast.stack[spot] = Type::Ref(lifetime, spot + 1);
            }
            b'Q' => {
                // [lifetime] <type>

                let mut lifetime = None;
                if self.source.take(b'L') {
                    lifetime = Some(self.consume_lifetime()?);
                }

                let spot = self.take_spot();
                self.consume_type()?;
                self.ast.stack[spot] = Type::RefMut(lifetime, spot + 1);
            }
            b'P' => {
                // <type>

                let spot = self.take_spot();
                self.consume_type()?;
                self.ast.stack[spot] = Type::Pointer(spot + 1);
            }
            b'O' => {
                // <type>

                let spot = self.take_spot();
                self.consume_type()?;
                self.ast.stack[spot] = Type::PointerMut(spot + 1);
            }
            b'F' => {
                // [<binder>] ["U"] ["K" <abi>] {<type>} "E" <type>

                let mut binder = None;
                if self.source.take(b'G') {
                    binder = Some(todo!("bind in fn signature"));
                }

                let is_unsafe = self.source.take(b'U');
                let mut ident = None;

                if self.source.take(b'K') && self.source.take(b'C') {
                    ident = Some(self.consume_ident()?);
                }

                let spot = self.take_spot();
                let args = self.consume_types()?;

                let mut return_ty = None;
                if !self.source.take(b'u') {
                    return_ty = Some(self.ast.ptr);
                }

                self.consume_type()?;

                self.ast.stack[spot] = Type::FnSig(binder, is_unsafe, ident, args, return_ty);
            }
            b'D' => {
                // [binder] {path {"p" ident type}} "E" <lifetime>

                let mut binder = None;
                if self.source.take(b'G') {
                    binder = Some(todo!("bind in dyn trait"));
                }

                let spot = self.take_spot();
                let mut dyn_trait_spots = Vec::new();
                while !self.source.take(b'E') {
                    let path_spot = self.ast.ptr;

                    self.consume_path()?;

                    let mut dyn_trait_assoc_binding_spots = Vec::new();
                    while self.source.take(b'p') {
                        let ident = self.consume_ident()?;
                        let ty_spot = self.ast.ptr;

                        self.consume_type()?;

                        dyn_trait_assoc_binding_spots.push((ident, ty_spot));
                    }

                    dyn_trait_spots.push((path_spot, dyn_trait_assoc_binding_spots));
                }

                if !self.source.take(b'L') {
                    return Err(Error::DecodingBase62Num);
                }

                let lifetime = self.consume_lifetime()?;

                self.ast.stack[spot] = Type::DynTrait(binder, dyn_trait_spots, lifetime);
            }
            b'B' => {
                // <base-62-number>

                let backref = self.consume_base62()? as usize - 2;
                let current = self.source.pos.load(Ordering::Acquire);

                self.source.pos.store(backref, Ordering::Relaxed);
                self.consume_type()?;
                self.source.pos.store(current, Ordering::Release);
            }
            c @ _ => {
                // <basic-type | path>

                if let Some(ty) = basic_types(c) {
                    self.ast.stack[self.take_spot()] = Type::Basic(ty);
                    return Ok(());
                }

                self.source.offset(-1);
                self.consume_path()?;
            }
        }

        Ok(())
    }
}

struct Stack<'p> {
    stack: [Type<'p>; MAX_COMPLEXITY],
    ptr: usize,
}

impl<'p> Default for Stack<'p> {
    fn default() -> Self {
        let mut bytes: MaybeUninit<[Type<'p>; MAX_COMPLEXITY]> = MaybeUninit::uninit();
        let mut ptr = bytes.as_mut_ptr() as *mut Type<'p>;

        for _ in 0..MAX_COMPLEXITY {
            unsafe {
                ptr.write(Type::Empty);
                ptr = ptr.offset(1);
            }
        }

        Self { stack: unsafe { bytes.assume_init() }, ptr: 0 }
    }
}

impl fmt::Debug for Stack<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:#?}", &self.stack[..self.ptr]))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Lifetime(u64);

impl Lifetime {
    fn fmt(&self) -> Option<char> {
        #[rustfmt::skip]
        const CHARS: [char; 52] = [
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
            'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
            'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        ];

        if self.0 != 0 {
            return CHARS.get(self.0 as usize - 1).copied();
        }

        None
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Namespace {
    Unknown,
    Opaque,
    Type,
    Value,
}

// <type> ["n"] {hex-digit} "_" "p"
#[derive(Debug, PartialEq, Clone, Copy)]
enum Const {
    Boolean(bool),
    Integar(isize),
}

// Macros can generate an item with the same name as another item. We can differentiate between
// these using an optional `"s" [base-62-num] "_"` prefix.

#[derive(Debug, PartialEq, Clone, Copy)]
enum Path<'p> {
    /// [disambiguator] <ident>
    ///
    /// crate root.
    Crate(Option<u64>, &'p str),

    /// [disambiguator] <path> <type>
    ///
    /// <T>
    InherentImpl(Option<u64>, usize, usize),

    /// [disambiguator] <path> <type> <path>
    ///
    /// <T as Trait>
    TraitImpl(Option<u64>, usize, usize, usize),

    /// <type> <path>
    ///
    /// <T as Trait>
    TraitDef(usize, usize),

    /// <namespace> <path> [disambiguator] <ident>
    ///
    /// ...::ident
    Nested(Namespace, usize, Option<u64>, &'p str),

    /// <path> {generic-arg} "E"
    ///
    /// for now only allow one generic :(
    ///
    /// ...<T, U>
    Generic(usize, [u16; MAX_GENERIC_ARGUEMENTS]),
}

#[derive(Debug, PartialEq, Clone)]
enum Type<'p> {
    Empty,

    Basic(&'static str),

    /// <path>: named type
    Path(Path<'p>),

    /// <type> <const>: [T; N]
    Array(usize, Const),

    /// <type>: [T]
    Slice(usize),

    /// "T" {type} "E": (T, T, T, ...)
    Tuple(Vec<usize>),

    /// "R" [lifetime] <type>: &T
    Ref(Option<Lifetime>, usize),

    /// "Q" [lifetime] <type>: &mut T
    RefMut(Option<Lifetime>, usize),

    /// "P" <type>: *const T
    Pointer(usize),

    /// "O" <type>: *mut T
    PointerMut(usize),

    /// <abi> = "C" <undisambiguated-identifier>
    /// <undisambiguated-identifier> = ["u"] <decimal-number> ["_"] <byte str>
    ///
    /// If a "U" is present then the byte string is Punycode-encoded.
    ///
    /// "F" ["G" <base-62-number>] ["U"] ["K" abi] {type} "E" <type>: fn(...) -> ...
    ///
    /// If the "U" is present then the function is `unsafe`.
    /// "K" Indicates an abi is present.
    FnSig(Option<u64>, bool, Option<&'p str>, Vec<usize>, Option<usize>),

    /// [binder] {path {"p" ident type}} "E" <lifetime>
    ///
    /// dyn Trait<ident = type> + Read<ident = type> + Sync + ... + 'lifetime
    DynTrait(Option<u64>, Vec<(usize, Vec<(&'p str, usize)>)>, Lifetime),
}

fn format_num(repr: &mut String, mut num: isize) {
    // TODO: increase the performance of integar to string conversion.

    if num.is_negative() {
        num = -num;
        repr.push('-');
    }

    let mut len = (num as f64 + 1.).log10().ceil() as u32;
    while len != 0 {
        let pow = 10isize.pow(len - 1);
        repr.push(((num / pow) as u8 + b'0') as char);

        num %= pow;
        len -= 1;
    }
}

fn basic_types(tag: u8) -> Option<&'static str> {
    Some(match tag {
        b'b' => "bool",
        b'c' => "char",
        b'e' => "str",
        b'u' => "()",
        b'a' => "i8",
        b's' => "i16",
        b'l' => "i32",
        b'x' => "i64",
        b'n' => "i128",
        b'i' => "isize",
        b'h' => "u8",
        b't' => "u16",
        b'm' => "u32",
        b'y' => "u64",
        b'o' => "u128",
        b'j' => "usize",
        b'f' => "f32",
        b'd' => "f64",
        b'z' => "!",
        b'p' => "_",
        b'v' => "...",

        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::{Path, Type};

    macro_rules! fmt {
        ($mangled:literal => $demangled:literal) => {
            assert_eq!(
                $crate::demangler::Symbol::parse($mangled)
                    .map(|sym| dbg!(sym.display()))
                    .as_deref(),
                Ok($demangled)
            );
        };
    }

    #[test]
    fn crate_ident() {
        fmt!("_RC8demangle" => "demangle");
    }

    #[test]
    fn generics() {
        fmt!("_RINvNtC3std3mem8align_ofjdE" => "std::mem::align_of::<usize, f64>");
        fmt!("_RINvNtC3std3mem8align_ofINtC3wow4lmaopEE" => "std::mem::align_of::<wow::lmao::<_>>");
    }

    #[test]
    fn namespaces() {
        fmt!("_RNtC8rustdump6decode" => "rustdump::decode");
        fmt!("_RNvNtC8rustdump6decode6x86_64" => "rustdump::decode::x86_64");
        fmt!("_RINvNtC8rustdump6decode6x86_64NvC3lol4damnE" => "rustdump::decode::x86_64::<lol::damn>");
    }

    #[test]
    fn methods() {
        fmt!("_RNvNvXs2_C7mycrateINtC7mycrate3FoopEINtNtC3std7convert4FrompE4from3MSG" =>
             "<mycrate::Foo::<_> as std::convert::From::<_>>::mycrate::from::MSG");
    }

    #[test]
    fn pointers() {
        fmt!("_RINtC8rustdump6decodeRL_eE" => "rustdump::decode::<&str>");
        fmt!("_RINtC8rustdump6decodeRL0_eE" => "rustdump::decode::<&'a str>");

        fmt!("_RINtC8rustdump6decodeQL_eE" => "rustdump::decode::<&mut str>");
        fmt!("_RINtC8rustdump6decodeQL0_eE" => "rustdump::decode::<&'a mut str>");

        fmt!("_RINtC8rustdump6decodePeE" => "rustdump::decode::<*const str>");
        fmt!("_RINtC8rustdump6decodeOeE" => "rustdump::decode::<*mut str>");
    }

    #[test]
    fn arrays() {
        fmt!("_RINtC8rustdump6decodeANtNvC3std5array5Arrayf_E" => "rustdump::decode::<[std::array::Array; 16]>");
    }

    #[test]
    fn tupples() {
        fmt!("_RINvNtC3std3mem8align_ofjTddNvC4core3ptrEE" => "std::mem::align_of::<usize, (f64, f64, core::ptr)>");
    }

    #[test]
    fn fn_signature() {
        fmt!("_RINvNtC3std3mem8align_ofFUKC3rundddEoE" => "std::mem::align_of::<unsafe fn run(f64, f64, f64) -> u128>");

        fmt!("_RINvNtC3std3mem8align_ofFKC3rundddEoE" => "std::mem::align_of::<fn run(f64, f64, f64) -> u128>");

        fmt!("_RINvNtC3std3mem8align_ofFdddEoE" => "std::mem::align_of::<fn(f64, f64, f64) -> u128>");
    }

    #[test]
    fn dyn_traits() {
        fmt!("_RINvNtC4core4simd3mulDNvNtC4core3mem4Readp4ItemReEL_E" => "core::simd::mul::<dyn core::mem::Read<Item = &str>>");

        fmt!("_RINvNtC4core4simd3mulDNvNtC4core3mem4ReadEL0_E" => "core::simd::mul::<dyn core::mem::Read + 'a>");

        fmt!("_RINvNtC4core4simd3mulDNvNtC4core3mem4ReadEL_E" => "core::simd::mul::<dyn core::mem::Read>");
    }

    #[test]
    fn type_compression() {
        fmt!("_RINxC3std3fooTNyB4_3BarBe_EBd_E" => "std::foo::<(std::Bar, std::Bar), (std::Bar, std::Bar)>");

        fmt!("_RINvCs1234_7mycrate3fooNvB4_3barNvBn_3bazE" => "mycrate::foo::<mycrate::bar, mycrate::bar::baz>");
    }

    // TODO: decrease size of enums
    #[should_panic]
    #[test]
    fn cache_lines() {
        assert!(dbg!(std::mem::size_of::<Path>()) <= 64);
        assert!(dbg!(std::mem::size_of::<Type>()) <= 64);
    }
}
