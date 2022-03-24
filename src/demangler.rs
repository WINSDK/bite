use std::fmt;

#[derive(Debug, PartialEq, Eq)]
enum Error {
    UnknownPrefix,
    RecursionLimit,
    NamespaceTooManyParts,
    PathLengthNotNumber,
    ArgDelimiterNotFound,
    SymbolTooSmall,
    NotAscii,
    Invalid,
}

const MAX_IDENT_PARTS: usize = 128;
const MAX_ARGUEMENT_DEPTH: usize = 100;

struct Symbol<'ident> {
    source: &'ident str,
    ident: Ident<'ident>,
}

impl<'ident> Symbol<'ident> {
    pub fn parse(s: &'ident str, depth: usize) -> Result<Ident, Error> {
        if depth == MAX_ARGUEMENT_DEPTH {
            return Err(Error::RecursionLimit);
        }

        let mut sym = Symbol { source: s, ident: unsafe { Ident::new() } };

        let ident_has_arguement = sym.has_arguement();
        sym.parse_namespaces()?;
        sym.parse_path_names()?;

        for part in sym.parts() {
            println!("{:?}", part);
        }

        if ident_has_arguement {
            dbg!(&sym.source[..sym.source.find('E').unwrap()]);
            match sym.source.find('E') {
                Some(spot) => {
                    let inner_ident = &sym.source[..spot];
                    match Self::parse(inner_ident, depth) {
                        Ok(arg) => sym.ident.arguement = Some(Box::new(arg)),
                        Err(e) => return Err(e),
                    }
                }
                None => return Err(Error::ArgDelimiterNotFound),
            }
        }

        Ok(sym.ident)
    }

    // <path> = C <identifier>                      // crate-id root
    //        | M <type>                            // inherent impl root
    //        | X <type> <path>                     // trait impl root
    //        | N <namespace> <path> <identifier>   // nested path
    //        | I <path> {<generic-arg>} E          // generic arguments

    fn parse_namespaces(&mut self) -> Result<(), Error> {
        unsafe { self.ident.set_part_count(MAX_IDENT_PARTS) };

        for idx in 0.. {
            if idx == MAX_ARGUEMENT_DEPTH {
                return Err(Error::NamespaceTooManyParts);
            }

            match &self.source[idx * 2..].get(..2) {
                Some("Nt") => self.ident[idx + 1].namespace = NameSpace::Type,
                Some("Nv") => self.ident[idx + 1].namespace = NameSpace::Value,

                _ => match self.source.as_bytes().get(idx * 2) {
                    None => return Err(Error::SymbolTooSmall),
                    Some(chr) => {
                        if chr == &b'C' {
                            self.ident[0].namespace = NameSpace::Crate;
                            self.source = &self.source[1..];

                            unsafe { self.ident.set_part_count(idx + 1) };
                        } else {
                            unsafe { self.ident.set_part_count(idx) };
                        }

                        self.source = &self.source[idx * 2..];
                        break;
                    }
                },
            }
        }

        Ok(())
    }

    fn parse_path_names(&mut self) -> Result<(), Error> {
        let mut s = self.source;
        let mut length_extension = 0;

        let names_ident_count = self.ident.len;
        for (ident_count, part) in self.ident.iter_mut_all().enumerate() {
            // Break if we reach end of arguement or there is no characters left to parse.
            // We're past all the paths we found from `Self::parse_namespaces`.
            if ident_count >= names_ident_count {
                break;
            }

            // Are they unique or do they have multiple copies.
            if s.as_bytes()[0] == b's' {
                // Length of unique id and convert to a u16.
                for (width, chr) in s[1..].bytes().enumerate() {
                    if chr == b'_' {
                        part.id = Some(u16::from_str_radix(&s[1..][..width], 10).unwrap());
                        s = &s[width + 2..];
                    }

                    if !chr.is_ascii_digit() {
                        break;
                    }
                }
            }

            if let Some(ty) = common_type(s.as_bytes()[0]) {
                length_extension += 1;
                s = &s[1..];

                part.path = ty;
            } else {
                part.path = Self::consume_ident_name(&mut s)?;
            }
        }

        unsafe { self.ident.set_part_count(self.ident.len + length_extension) };
        self.source = s;

        Ok(())
    }

    /// Consume length of path and convert to a str.
    fn consume_ident_name<'s>(s: &mut &'s str) -> Result<&'s str, Error> {
        for (width, chr) in s.bytes().enumerate() {
            if !chr.is_ascii_digit() {
                match usize::from_str_radix(&s[..width], 10) {
                    Err(_) => return Err(Error::PathLengthNotNumber),
                    Ok(len) => {
                        let ident = &s[width..][..len];
                        *s = &s[width + len..];
                        return Ok(ident);
                    }
                }
            }
        }

        unreachable!("No end to identifier found")
    }

    fn has_arguement(&mut self) -> bool {
        let ident_has_arguement = self.source.as_bytes().first() == Some(&b'I');
        self.source = &self.source[ident_has_arguement as usize..];
        ident_has_arguement
    }

    fn parts(&self) -> impl Iterator<Item = &IdentPart> {
        self.ident.iter()
    }
}

#[derive(PartialEq, Eq, Clone)]
struct Ident<'part> {
    idents: [IdentPart<'part>; MAX_IDENT_PARTS],
    arguement: Option<Box<Ident<'part>>>,

    len: usize,
}

impl<'part> Ident<'part> {
    pub unsafe fn new() -> Self {
        Self {
            idents: [IdentPart { path: "", namespace: NameSpace::Type, id: None }; MAX_IDENT_PARTS],
            arguement: None,
            len: 0,
        }
    }

    pub unsafe fn set_part_count(&mut self, count: usize) {
        self.len = count;
    }

    pub fn iter_all(&self) -> impl Iterator<Item = &IdentPart<'_>> {
        self.idents[..].iter()
    }

    pub fn iter_mut_all<'ident>(
        &'ident mut self,
    ) -> impl Iterator<Item = &'ident mut IdentPart<'part>> {
        self.idents[..].iter_mut()
    }
}

impl<'part> std::ops::Deref for Ident<'part> {
    type Target = [IdentPart<'part>];

    fn deref(&self) -> &Self::Target {
        &self.idents[..self.len]
    }
}

impl std::ops::DerefMut for Ident<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.idents[..self.len]
    }
}

impl fmt::Debug for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(" {:?}", &self.idents[..self.len]))?;

        if let Some(arg) = &self.arguement {
            f.write_fmt(format_args!("arg:{arg:?},"))?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct IdentPart<'p> {
    path: &'p str,
    namespace: NameSpace,

    /// Macros can generate an item with the same name as another item in the same scope. we
    /// identify these using an `s[optional idx]_` prefix before the length of a path's component.
    /// I'm also assuming an identical item isn't generated more then u16::MAX times.
    id: Option<u16>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum NameSpace {
    Type,
    Value,
    Crate,
}

fn demangle(s: &str) -> Result<Ident, Error> {
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

    let sym = Symbol { source: s, ident: Symbol::parse(s, 0)? };

    Ok(sym.ident)
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
    #[test]
    fn idents() {
        // _RNvNtCs1234_7mycrate3foo3bar
        panic!("{:?}", super::demangle("_RINvNtCs1234_8demangle6decode6x86_64dE").unwrap());
    }

    #[test]
    fn complex() {
        panic!("{:?}", super::demangle("__RNvNtNtC8rustdump6decode6x86_643asm").unwrap());
    }

    #[test]
    fn simple_type() {
        panic!("{:?}", super::demangle("_Rd").unwrap());
    }

    #[test]
    fn empty_type() {
        assert_eq!(super::demangle("_R"), Err(super::Error::SymbolTooSmall));
    }
}
