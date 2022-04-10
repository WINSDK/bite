#![cfg(test)]

#[derive(Debug, PartialEq, Eq)]
enum Error {
    UnknownPrefix,
    RecursionLimit,
    NamespaceTooManyParts,
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

struct Stack<'p> {
    stack: [Path<'p>; MAX_COMPLEXITY],
    ptr: usize,
}

impl<'p> Symbol<'p> {
    pub fn parse(s: &'p str) -> Result<Self, Error> {
        let mut sym = Symbol {
            source: s,
            holder: Stack { stack: [Path::default(); MAX_COMPLEXITY], ptr: 0 },
        };

        sym.holder.stack[0].parse_path(sym.source, &mut sym.holder, 0)?;

        Ok(sym)
    }

    // <path> = C <identifier>                      // crate-id root
    //        | M <type>                            // inherent impl root
    //        | X <type> <path>                     // trait impl root
    //        | N <namespace> <path> <identifier>   // nested path
    //        | I <path> {<generic-arg>} E          // generic arguments
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

struct Lifetime(Base62Num);

// <type> ["n"] {hex-digit} "_" "p"
struct Const<'a, 'b> {
    ty: &'a Type<'a, 'b>,
    data: &'a [usize],
}

enum Type<'a, 'b> {
    Basic(&'static str),

    /// <path>: named type
    Path(Path<'b>),

    /// <type> <const>: [T; N]
    Array(&'a Type<'a, 'b>, Const<'a, 'b>),

    /// <type>: [T]
    Slice(&'a Type<'a, 'b>),

    /// "T" {type} "E": (T, T, T, ...)
    Tuple(&'a [Type<'a, 'b>]),

    /// "R" [lifetime] <type>: &T
    Ref(Option<Lifetime>, &'a Type<'a, 'b>),

    /// "Q" [lifetime] <type>: &mut T
    RefMut(Option<Lifetime>, &'a Type<'a, 'b>),

    /// "P" <type>: *const T
    Pointer(&'a Type<'a, 'b>),

    /// "O" <type>: *mut T
    PointerMut(&'a Type<'a, 'b>),

    /// <abi> = "C" <undisambiguated-identifier>
    /// <undisambiguated-identifier> = ["u"] <decimal-number> ["_"] <byte str>
    ///
    /// If a "U" is present then the byte string is Punycode-encoded.
    ///
    /// "F" ["G" <base-62-number>] ["U"] ["K" abi] {type} "E" <type>: fn(...) -> ...
    ///
    /// If the "U" is present then the function is `unsafe`.
    /// "K" Indicates an abi is present.
    FnSig(Base62Num, usize, &'b str, &'a [Type<'a, 'b>], &'a Type<'a, 'b>),

    /// "D" ["G" <base-62-number>] {path {"p" undisambiguated-identifier type}} lifetime <life "E": dyn Trait<Item = X> + Send + 'a
    DynTrait(Option<Base62Num>, &'a [(Path<'b>, &'a [(&'a str, &'a Type<'a, 'b>)])], Lifetime),
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

impl<'p> Path<'p> {
    // Returns number of bytes consumed.
    pub fn parse_path<'a>(
        mut self,
        mut s: &'p str,
        holder: &mut Stack<'p>,
        mut depth: usize,
    ) -> Result<usize, Error> {
        if depth == MAX_ARGUEMENT_DEPTH {
            return Err(Error::RecursionLimit);
        }

        let stack_start = holder.ptr;

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

                let path = &mut holder.stack[holder.ptr];
                let (repr, ident_len) = consume_ident_name(&mut s)?;

                holder.ptr += 1;
                path.namespace = Namespace::Crate;
                path.next = (!s.is_empty()).then_some(holder.ptr);
                path.repr = repr;
                path.id = id;

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

                let jump = self.parse_path(s, holder, depth + 1)?;
                s = &s[jump..];

                let path = &mut holder.stack[holder.ptr];
                let (repr, ident_len) = consume_ident_name(&mut s)?;

                holder.ptr += 1;
                path.namespace = namespace;
                path.next = (!s.is_empty()).then_some(holder.ptr);
                path.repr = repr;
                path.id = id;

                // Jump the 'N*', optional disambiguator, identifier and all the characters of
                // the previous part of the path.
                return Ok(2 + disambiguator_len + ident_len + repr.len() + jump);
            }
            b'I' => {
                s = &s[1..];

                match s.find('E') {
                    Some(end) => {
                        holder.ptr += 1;

                        self.arguement = Some(holder.ptr);
                        self.parse_path(&s[..end][1..], holder, depth + 1)?;

                        s = &s[end + 1..];
                    }
                    None => return Err(Error::ArgDelimiterNotFound),
                }
            }
            // we actually parse one of the available paths.
            _ => {},
        }

        if s.is_empty() {
            // Path must have a crate.
            debug_assert!(
                holder.stack[..holder.ptr].iter().any(|path| path.namespace == Namespace::Crate)
            );

            return Ok(0);
        }

        for part in &mut holder.stack[stack_start..=holder.ptr] {}

        Err(Error::RecursionLimit)
    }
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

fn common_type(tag: u8) -> Option<&'static str> {
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
    use super::{Error, Path, demangle};

    #[test]
    fn idents() {
        demangle("_RINvNtCdemangle6decode6x86_64dE").unwrap();
    }

    #[test]
    fn namespaces() {
        demangle("_RNvNtC8rustdump6decode6x86_64").unwrap();
    }

    #[test]
    fn simple_namespace() {
        let symbol = demangle("_RNts9_Cs1234_8rustdump6decode").unwrap();
        dbg!(&symbol.holder.stack[..symbol.holder.ptr]);

        let symbol = demangle("_RNvNtCs1234_7mycrate3foo3bar").unwrap();
        dbg!(&symbol.holder.stack[..symbol.holder.ptr]);
    }

    #[test]
    fn simple_crate() {
        let symbol = demangle("_RC8demangle").unwrap();
        assert_eq!(symbol.holder.stack[0].repr, "demangle");
        assert_eq!(symbol.holder.stack[1], Path::default());
    }

    #[test]
    fn empty_type() {
        demangle("_R").unwrap();
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
