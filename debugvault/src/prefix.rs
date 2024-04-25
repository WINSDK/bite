use std::ops::Range;
use std::{cmp::Ordering, sync::Arc};

use crate::Symbol;

fn cmp(a: &str, b: &str) -> Ordering {
    for (x, y) in a.chars().zip(b.chars()) {
        if x < y {
            return Ordering::Less;
        } else if x > y {
            return Ordering::Greater;
        }
    }

    Ordering::Equal
}

/// Datastructure for efficient string match searching.
#[derive(Default, Debug)]
pub struct PrefixMatcher {
    items: Vec<Arc<Symbol>>,
}

impl PrefixMatcher {
    /// Insert an item, doesn't ensure the items are sorted.
    pub fn insert(&mut self, s: &Arc<Symbol>) {
        self.items.push(s.clone());
    }

    /// Sorts elements to allow for searching.
    pub fn reorder(&mut self) {
        self.items.sort_unstable_by(|a, b| cmp(a.as_str(), b.as_str()));
        self.items.shrink_to_fit();
    }

    /// Find some given prefix and return a range back into the items.
    /// Must call [`PrefixMatch::reorder`] before calling this.
    pub fn find(&self, prefix: &str) -> Match {
        // This works as cmp() will return Ordering::Equal if the prefix matches.
        let mid = match self.items.binary_search_by(|item| cmp(item.as_str(), prefix)) {
            Ok(idx) => idx,
            Err(_) => return Match { range: 0..0 },
        };

        // Look left and try to find more matching prefixes
        let mut start = mid;
        while start != 0 && self.items[start - 1].as_str().starts_with(prefix) {
            start -= 1;
        }

        // Look right and try to find more matching prefixes
        let mut end = mid;
        while end != self.items.len() && self.items[end + 1].as_str().starts_with(prefix) {
            end += 1;
        }

        Match {
            range: start..end + 1,
        }
    }
}

/// Storage mechanism for [`PrefixMatch::find`].
#[derive(Debug)]
pub struct Match {
    range: Range<usize>,
}

impl Match {
    /// Iterate through all items that match.
    pub fn iter<'s>(&self, tree: &'s PrefixMatcher) -> impl Iterator<Item = &'s Arc<Symbol>> {
        tree.items[self.range.clone()].iter()
    }
}

#[cfg(test)]
mod test {
    use crate::demangler::TokenStream;
    use super::*;

    fn symbol(s: &str) -> Arc<Symbol> {
        Arc::new(Symbol {
            name_as_str: Arc::from(s),
            name: TokenStream::simple(s),
            module: None,
            is_intrinsics: false
        })
    }

    #[test]
    fn insert() {
        let mut tree = PrefixMatcher::default();
        tree.insert(&symbol("file"));
        tree.insert(&symbol("file_name"));
        tree.insert(&symbol("file::name"));
        tree.insert(&symbol("file::no"));
        tree.reorder();
        let expected = [
            "file",
            "file::name",
            "file::no",
            "file_name",
        ];
        assert_eq!(tree.items.len(), expected.len(), "Mismatched length");
        for (x, y) in tree.items.iter().zip(expected.iter()) {
            assert_eq!(&x.as_str(), y, "Mismatch");
        }
    }

    #[test]
    fn find() {
        let mut tree = PrefixMatcher::default();
        tree.insert(&symbol("file"));
        tree.insert(&symbol("file_name"));
        tree.insert(&symbol("file::name"));
        tree.insert(&symbol("file::no"));
        tree.reorder();
        let expected = [
            "file::name",
            "file::no",
        ];
        assert_eq!(tree.find("file::").range.len(), expected.len(), "Mismatched length");
        for (x, y) in tree.find("file::").iter(&tree).zip(expected.iter()) {
            assert_eq!(&x.as_str(), y, "Mismatch");
        }
    }
}
