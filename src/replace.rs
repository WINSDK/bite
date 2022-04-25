use std::borrow::Cow;
use std::fmt;

// 1. sort the statements
// 2. do a binary search on the first section of a path
// 3. return a range of path's that match the subpath
// 4. if any path's match the subset exactly and the superset either
//    doesn't exist or has no matches to any of the statements return
// 5. repeat from 2 starting at the next section

enum Statement {
    /// use crate::namespace::Type
    ///
    /// Replace full path with last section of path.
    Path(*const str),

    /// use crate::namespace::*
    ///
    /// Remove everything in path up to `*`.
    Include(*const str),

    /// use crate::namespace::Type as OtherType
    ///
    /// Replace path with other path.
    Rename(*const str, *const str),
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            match self {
                Self::Path(p) => {
                    f.write_str("Path(")?;
                    f.write_str(std::mem::transmute(*p))?;
                    f.write_str(")")
                }
                Self::Include(p) => {
                    f.write_str("Include(")?;
                    f.write_str(std::mem::transmute(*p))?;
                    f.write_str(")")
                }
                Self::Rename(p1, p2) => {
                    f.write_str("Rename(")?;
                    f.write_str(std::mem::transmute(*p1))?;
                    f.write_str(", ")?;
                    f.write_str(std::mem::transmute(*p2))?;
                    f.write_str(")")
                }
            }
        }
    }
}

impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        let lhs = match self {
            Self::Path(p) | Self::Include(p) | Self::Rename(p, _) => p,
        };

        let rhs = match other {
            Self::Path(p) | Self::Include(p) | Self::Rename(p, _) => p,
        };

        unsafe { &**lhs == &**rhs }
    }
}

impl Eq for Statement {}

impl PartialOrd for Statement {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Statement {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let lhs = match self {
            Self::Path(p) | Self::Include(p) | Self::Rename(p, _) => p,
        };

        let rhs = match other {
            Self::Path(p) | Self::Include(p) | Self::Rename(p, _) => p,
        };

        unsafe { str::cmp(&**lhs, &**rhs) }
    }
}

// NOTE: Using *const str's to refer to parts of inner is correct for as long as `inner` doesn't
// reallocate. Moving inner shouldn't change the pointer to it's internal Vec<u8>.

pub struct Config {
    _inner: String,
    includes: Vec<Statement>,
}

impl Config {
    pub fn from_env(args: &crate::args::Cli) -> Self {
        if let Some(ref conf_path) = args.config {
            if let Ok(data) = std::fs::read(conf_path) {
                if let Ok(s) = String::from_utf8(data) {
                    return Self::from_string(s);
                }
            }
        }

        if let Ok(data) = std::fs::read(".dumpfmt").or(std::fs::read("~/.dumpfmt")) {
            if let Ok(s) = String::from_utf8(data) {
                return Self::from_string(s);
            }
        }

        Self { includes: Vec::new(), _inner: String::new() }
    }

    pub fn from_string(s: String) -> Self {
        let mut includes = Vec::new();

        'lines: for mut line in s.lines() {
            skip_whitespace(&mut line);

            if !line.starts_with("use") {
                continue;
            }

            line = &line[3..];
            if !skip_whitespace(&mut line) {
                continue;
            }

            let mut path = line;
            for (idx, c) in line.bytes().enumerate() {
                if c == b' ' {
                    path = &line[..idx];
                    line = &line[idx..];
                    break;
                }

                if c == b'*' {
                    if let Some(new_idx) = idx.checked_sub(2) {
                        includes.push(Statement::Include(&line[..new_idx]));
                    }

                    continue 'lines;
                }
            }

            if !skip_whitespace(&mut line) {
                if !line.starts_with("as") {
                    includes.push(Statement::Path(path));
                }

                continue;
            }

            line = &line[2..];
            if !skip_whitespace(&mut line) {
                continue;
            }

            for (idx, c) in line.bytes().enumerate() {
                if c == b' ' {
                    line = &line[..idx];
                    break;
                }
            }

            includes.push(Statement::Rename(path, line));
        }

        includes.sort_unstable();

        dbg!(&includes);
        Self { includes, _inner: s }
    }
}

/// Skips whitespace and returns if it skipped any whitespace.
fn skip_whitespace(line: &mut &str) -> bool {
    let mut skipped = false;
    for (idx, c) in line.bytes().enumerate() {
        if c != b' ' {
            *line = &line[idx..];
            return skipped;
        }

        skipped = true;
    }

    skipped
}

/// Returns all entries in slice that start with `target`.
fn binary_search_range<'a, 'b>(slice: &'a [&'b str], target: &str) -> &'a [&'b str] {
    if slice.is_empty() {
        return &[];
    }

    let (mut start, mut end) = (0, slice.len() - 1);
    while start <= end {
        let midpoint = (start + end) / 2;
        let middle = &slice[midpoint];
        let middle = &middle[..std::cmp::min(middle.len(), target.len())];

        match str::cmp(middle, target) {
            std::cmp::Ordering::Greater => end = midpoint - 1,
            std::cmp::Ordering::Less => start = midpoint + 1,
            _ => {
                let (left, right) = slice.split_at(midpoint);

                // Go to the left and check if there are more matches.
                let mut start = midpoint;
                for elem in left.iter().rev() {
                    if elem.get(..target.len()) == Some(target) {
                        start -= 1;
                    }
                }

                // Go to the right and check if there are more matches.
                let mut end = midpoint;
                for elem in right {
                    if elem.get(..target.len()) == Some(target) {
                        end += 1;
                    }
                }

                return &slice[start..end];
            }
        }
    }

    &[]
}

const DEFAULT_TOKENS: phf::Map<&'static str, &'static str> = phf::phf_map! {
    // Generic shortenings
    "std::io" => "io",
    "core::fmt" => "fmt",

    "core::panicking::panic" => "panic",
    "core::panicking::panic_fmt" => "panic_fmt",
    "core::sync::atomic" => "atomic",
    "alloc::sync::Arc" => "sync::Arc",
    "std::sync::mutex::Mutex" => "sync::Mutex",
    "std::sync::rwlock::RwLock" => "sync::RwLock",
    "std::sync::rwlock::RwLockReadGuard" => "sync::RwLockReadGuard",
    "std::sync::poison" => "poison",

    "std::ffi::os_str::OsString" => "OsString",
    "std::ffi::os_str::OsStr" =>  "OsStr",

    "core::ptr::non_null::NonNull" => "ptr::NonNull",
    "core::ptr::unique::Unique" => "UniquePtr",
    "core::ptr::drop_in_place" => "drop_in_place",

    "core::ops::index::Index" => "Index",
    "core::ops::index::IndexMut" => "IndexMut",
    "core::slice::index::SliceIndex" => "SliceIndex",

    "core::ptr" => "ptr",
    "core::slice" => "slice",

    // Str (TODO: doesn't convert for some reason) and so does panic
    "core::str::converts::from_utf8_unchecked_mut" => "str::from_utf8_unchecked_mut",
    "core::str::converts::from_utf8_unchecked" => "str::from_utf8_unchecked",

    // Alloc
    "alloc::string::String" => "String",
    "alloc::string::ToString" => "ToString",
    "alloc::slice::hack::ConvertVec" => "slice::ToVec",
    "alloc::slice::hack::to_vec" => "ToVec",
    "alloc::raw_vec::RawVec" => "RawVec",
    "alloc::vec::Vec" => "Vec",
    "alloc::collections::btree" => "btree",
    "alloc::alloc" => "alloc",
    "core::alloc" => "alloc",

    // Iterators
    "core::str::iter::Chars" => "iter::Chars",
    "core::slice::iter::Iter" => "slice::Iter",
    "core::slice::iter::IterMut" => "slice::IterMut",

    "core::iter::adapters::map::Map" => "Map",
    "core::iter::adapters::zip::Zip" => "Zip",
    "core::iter::adapters::chain::Chain" => "Chain",
    "core::iter::adapters::take::Take" => "Take",
    "core::iter::adapters::take_while::TakeWhile" => "TakeWhile",
    "core::iter::adapters::zip::ZipImpl" => "ZipImpl",
    "core::iter::adapters::peekable::Peekable" => "Peekable",
    "core::iter::adapters::enumerate::Enumerate" => "Enumerate",
    "core::iter::adapters::filter_map::FilterMap" => "FilterMap",
    "core::iter::adapters::filter::Filter" => "Filter",
    "core::iter::adapters::copied::Copied" => "iter::Copied",
    "core::iter::adapters::skip::Skip" => "iter::Skip",
    "core::iter::adapters::rev::Rev" => "iter::Rev",
    "core::iter::range::Step" => "iter::Step",
    "core::str::iter::Bytes" => "iter::Bytes",

    "core::option::Option" => "Option",
    "core::result::Result" => "Result",

    // https://doc.rust-lang.org/std/prelude/v1/index.html
    // Basic traits
    "core::convert::Into" => "Into",
    "core::convert::From" => "From",
    "core::convert::AsMut" => "AsMut",
    "core::convert::AsRef" => "AsRef",
    "core::convert::TryFrom" => "TryFrom",
    "core::convert::TryInto" => "TryInto",

    "core::cmp::Eq" => "Eq",
    "core::cmp::Ord" => "Ord",
    "core::cmp::PartialEq" => "PartialEq",
    "core::cmp::PartiaOrd" => "PartiaOrd",

    "alloc::boxed::Box" => "Box",
    "alloc::borrow::ToOwned" => "ToOwned",
    "core::clone::Clone" => "Clone",
    "core::default::Default" => "Default",
    "core::ops::drop::Drop" => "Drop",

    "core::ops::function::Fn" => "Fn",
    "core::ops::function::FnOnce" => "FnOnce",
    "core::ops::function::FnMut" => "FnMut",

    "core::iter::traits::double_ended::DoubleEndedIterator" => "DoubleEndedIterator",
    "core::iter::traits::exact_size::ExactSizeIterator" => "ExactSizeIterator",
    "core::iter::traits::collect::IntoIterator" => "collect::IntoIterator",
    "core::iter::traits::collect::Extend" => "Extend",
    "core::iter::traits::iterator::Iterator" => "Iterator",

    "core::marker::Send" => "Send",
    "core::marker::Sized" => "Sized",
    "core::marker::Sync" => "Sync",
    "core::marker::Unpin" => "Unpin",

    // Ranges
    "core::ops::range::RangeToInclusive" => "RangeToInclusive",
    "core::ops::range::RangeFrom" => "RangeFrom",
    "core::ops::range::RangeFull" => "RangeFull",
    "core::ops::range::RangeTo" => "RangeTo",
    "core::ops::range::Range" => "Range",

    // Complex traits
    "alloc::vec::spec_extend::SpecExtend" => "Vec::Extend",
    "core::mem::maybe_uninit::MaybeUninit" => "mem::MaybeUninit",
    "core::ops::try_trait::Try" => "Try",
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Type<'a>(&'a str, usize);

impl<'a> Type<'a> {
    #[inline]
    pub fn new(s: &'a str) -> Self {
        assert!(s.len() > 0);

        #[cfg(debug_assertions)]
        for chr in s.chars() {
            match chr {
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' | ':' => {}
                _ => panic!("type: `{s}` includes invalid characters"),
            }
        }

        Self(s, 0)
    }
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.0)
    }
}

impl<'a> fmt::Debug for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.0)
    }
}

impl<'a> Iterator for Type<'a> {
    type Item = (&'a str, &'a str);

    // Splits path of type.
    // e.g. ("core::ptr::NonNull", "") => ("core::ptr", "::NonNull") => ("core", "::ptr::NonNull"),
    //      ("std::option::Option", "") => ("std::option", "::Option") => ("std", "::option::Option"),
    fn next(&mut self) -> Option<Self::Item> {
        let (s, split) = (&self.0, &mut self.1);

        if *split == 0 {
            *split = s.len();
            return Some((s, ""));
        }

        for idx in (0..*split).rev() {
            if s.as_bytes()[idx] == b':' {
                *split = idx - 1;
                return Some(s.split_at(*split));
            }
        }

        None
    }
}

pub fn simplify_type<'a>(s: &'a str) -> Cow<'a, str> {
    let (mut idx, mut last_end) = (0, 0);
    let mut concat = String::new();

    while idx != s.len() {
        if s[idx..].starts_with("::") {
            // Move to the left until a seperator is reached or the start of the str.
            let mut left = 0;
            for jdx in (0..idx).rev() {
                match s.as_bytes()[jdx] {
                    b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' => {}
                    _ => {
                        left = jdx + 1;
                        break;
                    }
                }
            }

            // Move to the right until a seperator is reached or the end of the str.
            let mut right = idx;
            for jdx in idx + 2..s.len() {
                match s.as_bytes()[jdx] {
                    b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'_' | b':' => {}
                    _ => {
                        right = jdx;
                        break;
                    }
                }
            }

            // The type is an extension of a path e.g. Iterator<Item = u8>::next.
            if s[left..].starts_with("::") {
                idx += 2;
                continue;
            }

            // Walk the type hierarchy.
            let mut ty = Type::new(&s[left..right]);
            while let Some((type_path_left, type_path_right)) = ty.next() {
                // Check whether a shorter representation of a type exists.
                if let Some(simple_type_path_left) = DEFAULT_TOKENS.get(type_path_left) {
                    // Reserve space in for String since the a new type will have to be allocated.
                    concat.reserve(s.len());

                    concat.push_str(&s[last_end..left]);
                    concat.push_str(simple_type_path_left);
                    concat.push_str(type_path_right);
                    last_end = right;
                    break;
                }
            }

            idx = right;
        }

        idx += 1;
    }

    if concat.len() == 0 {
        Cow::Borrowed(s)
    } else {
        concat.push_str(&s[last_end..]);
        Cow::Owned(concat)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simplify() {
        let sample = "<std::path::PathBuf as core::convert::From<&str>>::from>";
        assert_eq!(
            Cow::Borrowed("<std::path::PathBuf as From<&str>>::from>"),
            simplify_type(sample)
        );

        let sample = "core::ptr::unique::Unique<dyn core::ops::function::FnMut<(), Output = core::result::Result<(), std::io::error::Error>> + core::marker::Sync + core::marker::Send>";
        assert_eq!(
            Cow::Borrowed(
                "UniquePtr<dyn FnMut<(), Output = Result<(), io::error::Error>> + Sync + Send>"
            ),
            simplify_type(sample)
        );

        let sample = "core::unicode::unicode_data::n::lookup::he820b1879a01d5c6";
        assert_eq!(
            Cow::Borrowed("core::unicode::unicode_data::n::lookup::he820b1879a01d5c6"),
            simplify_type(sample)
        );
    }

    #[test]
    fn slice_type() {
        let mut sample = Type::new("core::option::Option");

        assert_eq!(sample.next(), Some(("core::option::Option", "")));
        assert_eq!(sample.next(), Some(("core::option", "::Option")));
        assert_eq!(sample.next(), Some(("core", "::option::Option")));
        assert_eq!(sample.next(), None);
    }

    #[test]
    fn cfg_generate() {
        let example = r#"
            use        std::path::Path
                                                use std::mem::*
            use     std::mem::transmute          as       rename D:
        "#;

        let mut conf1 = Config::from_string(example.to_string());
        dbg!(&conf1.includes);

        let example = r#"
            use        std::troll
                                                use std::compare
            use     std::mem::transmute
        "#;

        let mut conf2 = Config::from_string(example.to_string());
        dbg!(&conf2.includes);

        let (fmt1, fmt2) = (format!("{:?}", &conf1.includes), format!("{:?}", &conf2.includes));
        std::mem::swap(&mut conf1, &mut conf2);
        let (fmt3, fmt4) = (format!("{:?}", &conf1.includes), format!("{:?}", &conf2.includes));

        // Assert that the swap correctly references to the internal `Statement`s.
        assert_eq!(fmt1, fmt4);
        assert_eq!(fmt2, fmt3);
    }
}
