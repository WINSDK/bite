#![cfg(test)]

use std::fmt;
use std::mem::MaybeUninit;
use std::ops::Range;

#[derive(Debug, PartialEq, Eq)]
enum Error {
    UnknownPrefix,
    RecursionLimit,
    PathLengthNotNumber,
    ArgDelimiterNotFound,
    DecodingBase62Num,
    SymbolTooSmall,
    NotAscii,
    Invalid,
}

const MAX_COMPLEXITY: usize = 256;
const MAX_ARGUEMENT_DEPTH: usize = 16;

struct Symbol<'p> {
    source: &'p str,
    holder: Stack<'p>,
}

impl<'p> Symbol<'p> {
    pub fn parse(s: &'p str) -> Result<Self, Error> {
        let mut holder = Stack::default();
        parse_path(s, &mut holder, 0)?;
        Ok(Self { source: s, holder })
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

/// Demangle's a symbol.
fn demangle(s: &str) -> Result<Symbol, Error> {
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

/// Consume length of path and convert to a str returning the ident len as well.
fn consume_ident_name<'s>(s: &mut &'s str) -> Result<(&'s str, usize), Error> {
    for (width, chr) in s.bytes().enumerate() {
        if !chr.is_ascii_digit() {
            match usize::from_str_radix(&s[..width], 10) {
                Err(_) => return Err(Error::PathLengthNotNumber),
                Ok(len) => {
                    let ident = &s[width..][..len];
                    *s = &s[width + len..];
                    return Ok((ident, width));
                }
            }
        }
    }

    Err(Error::Invalid)
}

/// (base-62 number, length of original number)
#[derive(Default, Debug, PartialEq, Eq, Copy, Clone)]
struct Base62Num(u64);

impl Base62Num {
    /// Consume a str and tries to convert a `Base62Num`.
    pub fn try_consume_into(s: &mut &str) -> Result<(Self, usize), Error> {
        let mut num = 0u64;

        if &s[..] == "_" {
            *s = &s[1..];

            return Ok((Base62Num(0), 1));
        }

        for (idx, c) in s.bytes().enumerate() {
            if c == b'_' {
                *s = &s[idx + 1..];

                return match num.checked_add(1) {
                    Some(num) => Ok((Base62Num(num), idx + 1)),
                    None => Err(Error::DecodingBase62Num),
                };
            }

            let base_62_c = match c {
                b'0'..=b'9' => c as u8 - b'0',
                b'a'..=b'z' => c as u8 - b'a' + 10,
                b'A'..=b'Z' => c as u8 - b'A' + 36,
                _ => return Err(Error::DecodingBase62Num),
            };

            num = num.checked_mul(62).ok_or(Error::DecodingBase62Num)?;
            num = num.checked_add(base_62_c as u64).ok_or(Error::DecodingBase62Num)?;
        }

        Err(Error::DecodingBase62Num)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Lifetime(Base62Num);

// <type> ["n"] {hex-digit} "_" "p"
#[derive(Debug, PartialEq, Clone, Copy)]
struct Const<'p> {
    ty: &'p Type<'p>,
    data: &'p [usize],
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

// Returns number of bytes consumed.
fn parse_path<'p>(mut s: &'p str, holder: &mut Stack<'p>, depth: usize) -> Result<usize, Error> {
    if depth == MAX_ARGUEMENT_DEPTH {
        return Err(Error::RecursionLimit);
    }

    match s.as_bytes()[0] {
        b'C' => {
            // <identifier>

            s = &s[1..];
            let mut disambiguator_len = 0;
            let mut id = None;

            // Try to consume a disambiguator.
            if s.as_bytes()[0] == b's' {
                let (disambiguator, bytes_consumed) = Base62Num::try_consume_into(&mut s)?;

                disambiguator_len = bytes_consumed;
                id = Some(disambiguator);
            }

            let (repr, ident_len) = consume_ident_name(&mut s)?;

            holder.stack[holder.ptr] =
                Type::Path(Path { id, repr, namespace: Namespace::Crate, ..Default::default() });

            holder.ptr += 1;

            // Consume the 'C', optional disambiguator and identifier.
            return Ok(1 + disambiguator_len + ident_len + repr.len());
        }
        b'M' => {
            // [disambiguator] <path> <type>

            todo!()
        }
        b'X' => {
            // [disambiguator] <path> <type> <identifier>
            //
            // <crate::Foo<_> *as* Readable> for example

            todo!()
        }
        b'Y' => {
            // <type> <path>

            todo!()
        }
        b'N' => {
            // <namespace> <path> <identifier>

            let namespace = match s.as_bytes()[1] {
                b'v' => Namespace::Value,
                b't' => Namespace::Type,
                _ => Namespace::Unknown,
            };

            s = &s[2..];
            let mut disambiguator_len = 0;
            let mut id = None;

            // Try to consume a disambiguator.
            if s.as_bytes()[0] == b's' {
                let (disambiguator, bytes_consumed) = Base62Num::try_consume_into(&mut s)?;

                disambiguator_len = bytes_consumed;
                id = Some(disambiguator);
            }

            let jump = parse_path(s, holder, depth + 1)?;
            s = &s[jump..];

            let (repr, ident_len) = consume_ident_name(&mut s)?;

            holder.stack[holder.ptr] =
                Type::Path(Path { id, repr, namespace, ..Default::default() });

            // Mention to the last type that it has a `next` if it's a path.
            match holder.stack[holder.ptr - 1] {
                Type::Path(ref mut path) => {
                    path.next = Some(holder.ptr);
                }
                _ => {}
            }

            holder.ptr += 1;

            // Jump the 'N*', optional disambiguator, identifier and all the characters of
            // the previous part of the path.
            return Ok(2 + disambiguator_len + ident_len + repr.len() + jump);
        }
        b'I' => {
            // <path> {<lifetime> <type>} "E"

            s = &s[1..];
            let jump = parse_path(s, holder, depth + 1)?;
            s = &s[jump..];

            match s.find('E') {
                Some(end) => {
                    s = &s[..end];

                    match holder.stack[holder.ptr - 1] {
                        // Specify that the last path has an arguement.
                        Type::Path(ref mut path) => {
                            path.arguement = Some(holder.ptr);
                        }
                        _ => {}
                    }

                    let jump = parse_type(s, holder, depth + 1)?;
                    s = &s[jump..];

                    holder.ptr += 1;

                    if s.as_bytes().get(end + 1) == Some(&b'C') {
                        let (_, crate_id_len) = consume_ident_name(&mut s)?;
                        return Ok(2 + crate_id_len + jump);
                    }

                    // Jump the 'I', 'E' and the type.
                    return Ok(2 + jump);
                }
                None => return Err(Error::ArgDelimiterNotFound),
            }
        }
        _ => unreachable!("Could be a backref that isn't being accounted for?"),
    }
}

fn parse_type<'p>(mut s: &'p str, holder: &mut Stack<'p>, depth: usize) -> Result<usize, Error> {
    if depth == MAX_ARGUEMENT_DEPTH {
        return Err(Error::RecursionLimit);
    }

    if s.is_empty() {
        holder.ptr -= 1;
        return Ok(0);
    }

    match s.as_bytes()[0] {
        b'A' => {
            // <type> <const>

            todo!()
        }
        b'S' => {
            // <type>

            holder.stack[holder.ptr] = Type::Slice(holder.ptr + 1);
            holder.ptr += 1;

            Ok(1 + parse_type(&s[1..], holder, depth + 1)?)
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

            holder.stack[holder.ptr] = Type::Pointer(holder.ptr + 1);
            holder.ptr += 1;

            Ok(1 + parse_type(&s[1..], holder, depth + 1)?)
        }
        b'O' => {
            // <type>

            holder.stack[holder.ptr] = Type::PointerMut(holder.ptr + 1);
            holder.ptr += 1;

            Ok(1 + parse_type(&s[1..], holder, depth + 1)?)
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
            // <basic-type> | <path>

            if let Some(ty) = basic_types(c) {
                holder.stack[holder.ptr] = Type::Basic(ty);
                return Ok(1);
            }

            parse_path(s, holder, depth + 1)
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
struct Path<'p> {
    repr: &'p str,
    namespace: Namespace,

    /// Macros can generate an item with the same name as another item in the same scope. we
    /// identify these using an `s[optional idx]_` prefix before the length of a path's component.
    /// I'm also assuming an identical item isn't generated more then u16::MAX times.
    id: Option<Base62Num>,

    /// Index into the symbol's arguement table.
    arguement: Option<usize>,

    /// Next substring of the path.
    next: Option<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Namespace {
    Unknown,
    Opaque,
    Type,
    Value,
    Crate,
}

impl Default for Namespace {
    fn default() -> Self {
        Self::Unknown
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
    use super::{demangle, Error, Type};

    #[test]
    fn simple_arg() {
        let symbol = demangle("_RINvNtC3std3mem8align_ofjEC3foo").unwrap();
        dbg!(&symbol.holder);
    }

    #[test]
    fn namespaces() {
        demangle("_RNvNtC8rustdump6decode6x86_64").unwrap();
    }

    #[test]
    fn simple_namespace() {
        let symbol_a = demangle("_RNts9_Cs1234_8rustdump6decode").unwrap();
        dbg!(&symbol_a.holder);

        let symbol_b = demangle("_RNvNtCs1234_7mycrate3foo3bar").unwrap();
        dbg!(&symbol_b.holder);
    }

    #[test]
    fn simple_crate() {
        let symbol = demangle("_RC8demangle").unwrap();

        match symbol.holder.stack[0] {
            Type::Path(path) => assert_eq!(path.repr, "demangle"),
            _ => unreachable!(),
        }
    }

    #[test]
    fn base62() {
        use super::Base62Num;

        let mut s = "10_";
        assert_eq!(Ok((Base62Num(63), 3)), Base62Num::try_consume_into(&mut s));
        assert_eq!(s.len(), 0);

        let mut s = "Z_";
        assert_eq!(Ok((Base62Num(62), 2)), Base62Num::try_consume_into(&mut s));
        assert_eq!(s.len(), 0);

        let mut s = "_";
        assert_eq!(Ok((Base62Num(0), 1)), Base62Num::try_consume_into(&mut s));
        assert_eq!(s.len(), 0);

        let mut s = "";
        assert_eq!(Err(Error::DecodingBase62Num), Base62Num::try_consume_into(&mut s));

        let mut s = "abc";
        assert_eq!(Err(Error::DecodingBase62Num), Base62Num::try_consume_into(&mut s));
    }
}
