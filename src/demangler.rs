#![cfg(test)]

use crate::decode::Reader;
use std::fmt;
use std::mem::MaybeUninit;
use std::ops::Range;

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
const MAX_DEPTH: usize = 32;
const MAX_GENERIC_ARGUEMENTS: usize = 16;

/// Demangle's a symbol.
fn demangle(s: &str) -> ManglingResult<Symbol> {
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

    Symbol::parse(s)
}

struct Symbol<'p> {
    ast: Stack<'p>,
    source: Reader<'p>,
    depth: usize,
}

impl<'p> Symbol<'p> {
    pub fn parse(s: &'p str) -> ManglingResult<Self> {
        let mut res = Self { source: Reader::new(s.as_bytes()), ast: Stack::default(), depth: 0 };
        res.consume_path()?;

        Ok(res)
    }

    pub fn display(&self) -> String {
        let mut name = String::new();
        let mut this_path = String::new();

        self.fmt(&mut name, &mut this_path, &self.ast.stack[0]);

        if name.is_empty() {
            this_path
        } else {
            name
        }
    }

    // `this_path` can't work for recursively storing generics within generics.
    fn fmt(&self, name: &mut String, this_path: &mut String, ty: &Type<'p>) {
        match ty {
            Type::Empty => unreachable!(),
            Type::Basic(s) => {
                this_path.clear();
                this_path.push_str(s);
            }
            Type::Path(path) => match path {
                Path::Crate(_, ident) => {
                    this_path.clear();
                    this_path.push_str(ident);
                }
                Path::Nested(_, path_idx, _disambiguator, ident) => {
                    self.fmt(name, this_path, &self.ast.stack[*path_idx]);

                    this_path.push_str("::");
                    this_path.push_str(ident);
                }
                Path::Generic(path_idx, generics) => {
                    self.fmt(name, this_path, &self.ast.stack[*path_idx]);
                    name.push_str(this_path);

                    name.push_str("::<");

                    let mut generics = generics.iter().peekable();
                    while let Some(generic) = generics.next() {
                        if *generic == u16::MAX {
                            break;
                        }

                        if *generic == u16::MAX - 1 {
                            name.push_str("...");
                            break;
                        }

                        self.fmt(name, this_path, &self.ast.stack[*generic as usize]);
                        name.push_str(this_path);

                        match generics.peek() {
                            Some(next) if **next != u16::MAX => name.push_str(", "),
                            _ => {}
                        }
                    }

                    name.push_str(">");
                }
                _ => todo!("{:?}", path),
            },
            _ => todo!("{:?}", ty),
        }
    }

    fn consume_namespace(&mut self) -> ManglingResult<()> {
        Ok(())
    }

    fn consume_base62(&mut self) -> ManglingResult<Base62Num> {
        let mut num = 0u64;

        if self.source.consume_eq(|x| x == b'_').is_some() {
            return Ok(Base62Num(num));
        }

        while let Some(chr) = self.source.consume() {
            if chr == b'_' {
                return num
                    .checked_add(1)
                    .map(|val| Base62Num(val))
                    .ok_or(Error::DecodingBase62Num);
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

    fn try_consume_disambiguator(&mut self) -> ManglingResult<Option<Base62Num>> {
        if self.source.consume_eq(|x| x == b's').is_some() {
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
                        self.source.inc(width + len);
                        Ok(&s[width..][..len])
                    }
                };
            }
        }

        Err(Error::Invalid)
    }

    fn consume_path(&mut self) -> ManglingResult<()> {
        if self.depth == MAX_DEPTH {
            return Err(Error::TooComplex);
        }

        match self.source.consume().ok_or(Error::Invalid)? {
            b'C' => {
                // <identifier>

                let id = self.try_consume_disambiguator()?;
                let ident = self.consume_ident()?;

                self.ast.stack[self.ast.ptr] = Type::Path(Path::Crate(id, ident));
                self.ast.ptr += 1;

                Ok(())
            }
            b'N' => {
                // <namespace> <path> <identifier>

                let namespace = match self.source.consume() {
                    Some(b'v') => Namespace::Value,
                    Some(b't') => Namespace::Type,
                    _ => Namespace::Unknown,
                };

                let spot = self.ast.ptr;

                self.ast.ptr += 1;
                self.consume_path()?;

                let id = self.try_consume_disambiguator()?;
                let ident = self.consume_ident()?;

                self.ast.stack[spot] = Type::Path(Path::Nested(namespace, spot + 1, id, ident));

                Ok(())
            }
            b'I' => {
                // <path> {<lifetime> <type>} "E"

                let mut generic_spots = [u16::MAX; MAX_GENERIC_ARGUEMENTS];
                let spot = self.ast.ptr;

                self.ast.ptr += 1;
                self.consume_path()?;

                for idx in 0.. {
                    if self.source.consume_eq(|x| x == b'E').is_some() {
                        break;
                    }

                    if idx == MAX_GENERIC_ARGUEMENTS - 1 {
                        // Indicate that there are too many arguements to display.
                        generic_spots[MAX_GENERIC_ARGUEMENTS - 1] = u16::MAX - 1;
                        break;
                    }

                    let spot = self.ast.ptr;

                    self.consume_type()?;

                    generic_spots[idx] = spot as u16;
                }

                self.ast.stack[spot] = Type::Path(Path::Generic(spot + 1, generic_spots));

                // Consume and ignore optional unique id suffix.
                if self.source.consume_eq(|x| x == b'c').is_some() {
                    self.consume_ident()?;
                }

                Ok(())
            }
            b'E' => Ok(()),
            _ => Err(Error::Invalid),
        }
    }

    fn consume_type(&mut self) -> ManglingResult<()> {
        if self.depth == MAX_DEPTH {
            return Err(Error::TooComplex);
        }

        match self.source.seek().ok_or(Error::Invalid)? {
            b'A' => {
                // <type> <const>

                todo!()
            }
            b'S' => {
                // <type>

                self.source.inc(1);
                self.ast.stack[self.ast.ptr] = Type::Slice(self.ast.ptr + 1);
                self.ast.ptr += 1;

                self.consume_type()
            }
            b'T' => {
                // {<type>} "E"

                todo!()
            }
            b'R' => {
                // [lifetime] <type>

                todo!()
            }
            b'Q' => {
                // [lifetime] <type>

                todo!()
            }
            b'P' => {
                // <type>

                self.source.inc(1);
                self.ast.stack[self.ast.ptr] = Type::Pointer(self.ast.ptr + 1);
                self.ast.ptr += 1;

                self.consume_type()
            }
            b'O' => {
                // <type>

                self.ast.stack[self.ast.ptr] = Type::PointerMut(self.ast.ptr + 1);
                self.ast.ptr += 1;

                self.consume_type()
            }
            b'F' => {
                // <fn-sig>

                todo!()
            }
            b'D' => {
                // <dyn-bounds> <lifetime>

                todo!()
            }
            b'B' => {
                // <base-62-number>

                todo!()
            }
            c @ _ => {
                // <basic-type | path>

                if let Some(ty) = basic_types(c) {
                    self.source.inc(1);
                    self.ast.stack[self.ast.ptr] = Type::Basic(ty);
                    self.ast.ptr += 1;
                    return Ok(());
                }

                self.consume_path()
            }
        }
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
struct Base62Num(u64);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Lifetime(Base62Num);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Namespace {
    Unknown,
    Opaque,
    Type,
    Value,
}

// <type> ["n"] {hex-digit} "_" "p"
#[derive(Debug, PartialEq, Clone, Copy)]
struct Const<'p> {
    ty: &'p Type<'p>,
    data: &'p [usize],
}

// Macros can generate an item with the same name as another item. We can differentiate between
// these using an optional `"s" [base-62-num] "_"` prefix.

#[derive(Debug, PartialEq, Clone, Copy)]
enum Path<'p> {
    /// [disambiguator] <ident>
    ///
    /// crate root.
    Crate(Option<Base62Num>, &'p str),

    /// [disambiguator] <type>
    ///
    /// <T>
    InherentImpl(Option<Base62Num>, usize),

    /// [disambiguator] <type> <path>
    ///
    /// <T as Trait>
    TraitImpl(Option<Base62Num>, usize, usize),

    /// <type> <path>
    ///
    /// <T as Trait>
    TraitDef(usize, usize),

    /// <namespace> <path> [disambiguator] <ident>
    ///
    /// ...::ident
    Nested(Namespace, usize, Option<Base62Num>, &'p str),

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
    Array(usize, Const<'p>),

    /// <type>: [T]
    Slice(usize),

    /// "T" {type} "E": (T, T, T, ...)
    Tuple(Range<usize>),

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
    FnSig(Base62Num, usize, usize, Range<usize>, usize),

    /// "D" ["G" <base-62-number>] {path {"p" undisambiguated-identifier type}} lifetime <life "E": dyn Trait<Item = X> + Send + 'a
    DynTrait(Option<Base62Num>, &'p [(Path<'p>, &'p [(&'p str, &'p Type<'p>)])], Lifetime),
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
    use super::{demangle, Error, Path, Type};

    #[test]
    fn simple_arg() {
        let symbol = demangle("_RINvNtC3std3mem8align_ofjEC3foo").unwrap();
        dbg!(&symbol.ast);
    }

    #[test]
    fn complex_arg() {
        // _RINvNtC3std3mem8align_ofINtC4what4helldddEE
        let symbol = demangle("_RINvNtC3std3mem8align_ofINtC3wow4lmaoEE").unwrap();
        dbg!(&symbol.ast);
    }

    #[test]
    fn namespaces() {
        demangle("_RNvNtC8rustdump6decode6x86_64").unwrap();
    }

    #[test]
    fn complex_namespace() {
        let symbol = demangle("_RINvNtC8rustdump6decode6x86_64NtC3lol4damnE").unwrap();
        dbg!(&symbol.ast);
    }

    #[test]
    fn simple_namespace() {
        let symbol_a = demangle("_RNtC8rustdump6decode").unwrap();
        dbg!(&symbol_a.ast);

        let symbol_b = demangle("_RNvNtCs1234_7mycrate3foo3bar").unwrap();
        dbg!(&symbol_b.ast);
    }

    #[test]
    fn simple_crate() {
        let symbol = demangle("_RC8demangle").unwrap();

        match symbol.ast.stack[0] {
            Type::Path(Path::Crate(_, repr)) => assert_eq!(repr, "demangle"),
            _ => unreachable!(),
        }
    }

    #[test]
    fn cache_lines() {
        assert!(dbg!(std::mem::size_of::<Path>()) <= 64);
        assert!(dbg!(std::mem::size_of::<Type>()) <= 64);
    }

    #[test]
    fn fmt() {
        let symbol = demangle("_RC8demangle").unwrap();
        assert_eq!(dbg!(symbol.display()), "demangle");

        let symbol = demangle("_RNtCs100_8rustdump6decode").unwrap();
        assert_eq!(dbg!(symbol.display()), "rustdump::decode");

        let symbol = demangle("_RINvNtC8rustdump6decode6x86_64NvC3lol4damnE").unwrap();
        assert_eq!(dbg!(symbol.display()), "rustdump::decode::x86_64::<lol::damn>");

        let symbol = demangle("_RINvNtC3std3mem8align_ofjdE").unwrap();
        assert_eq!(dbg!(symbol.display()), "std::mem::align_of::<usize, f64>");

        let symbol = demangle("_RINvNtC3std3mem8align_ofhhhhhhhhhhhhhhhhhhhhE").unwrap();
        assert_eq!(
            dbg!(symbol.display()),
            "std::mem::align_of::<u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, ...>"
        );

        let symbol = demangle("_RINvNtC3std3mem8align_ofINtC4what4helldddEE").unwrap();
        assert_eq!(dbg!(symbol.display()), "std::mem::align_of::<what::hell::<f64, f64, f64>>");
    }
}
