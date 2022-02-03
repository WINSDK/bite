use std::borrow::Cow;
use std::fmt;

// The v0 mangling scheme transforms unicode characters into some
// ascii representation that looks similar.
//
// https://github.com/rust-lang/rfcs/blob/master/text/2603-rust-symbol-name-mangling-v0.md#unicode-identifiers

#[allow(dead_code)]
const TOKENS: phf::Map<&'static str, &'static str> = phf::phf_map! {
    // Generic shortenings
    "std::io" => "io",
    "std::sync::mutex::Mutex" => "Mutex",
    "std::ffi::os_str::OsString" => "OsString",
    "std::ffi::os_str::OsStr" =>  "OsStr",
    "core::ptr::non_null::NonNull" => "ptr::NonNull",
    "core::ptr::unique::Unique" => "UniquePtr",
    "core::ptr::drop_in_place" => "Drop",
    "core::ops::index::Index" => "Index",
    "core::ptr" => "ptr",
    "core::slice" => "slice",

    // Str (TODO: doesn't convert for some reason)
    "core::str::converts::from_utf8_unchecked_mut" => "str::from_utf8_unchecked_mut",
    "core::str::converts::from_utf8_unchecked" => "str::from_utf8_unchecked",

    // Alloc
    "alloc::string::String" => "String",
    "alloc::vec::Vec" => "Vec",
    "alloc::raw_vec::RawVec" => "RawVec",
    "alloc::alloc" => "alloc",
    "core::alloc" => "alloc",

    // Iterators
    "core::slice::iter::Iter" => "slice::Iter",
    "core::iter::adapters" => "iter",
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

    // Complex traits
    "alloc::vec::spec_extend::SpecExtend" => "Vec::Extend",
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
        let (s, split) = (&mut self.0, &mut self.1);

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
                if let Some(simple_type_path_left) = TOKENS.get(type_path_left) {
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
mod test {
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
}
