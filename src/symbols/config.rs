// 1. sort the statements
// 2. do a binary search on the first section of a path
// 3. return a range of path's that match the subpath
// 4. if any path's match the subset exactly and the superset either
//    doesn't exist or has no matches to any of the statements return
// 5. repeat from 2 starting at the next section

#[derive(Debug, Clone)]
pub enum Statement<'p> {
    /// use crate::namespace::Type
    ///
    /// Replace full path with last section of path.
    Path(Vec<&'p str>),

    /// use crate::namespace::*
    ///
    /// Remove everything in path up to `*`.
    Include(Vec<&'p str>),

    /// use crate::namespace::Type as OtherType
    ///
    /// Replace path with other path.
    Rename(Vec<&'p str>, &'p str),
}

impl Statement<'_> {
    #[inline]
    pub fn path(&self) -> &[&str] {
        match self {
            Self::Path(p) | Self::Include(p) | Self::Rename(p, _) => p,
        }
    }
}

impl PartialEq for Statement<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.path() == other.path()
    }
}

impl PartialOrd for Statement<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Statement<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.path().cmp(other.path())
    }
}

impl Eq for Statement<'_> {}

// NOTE: Using *const str's to refer to parts of inner is correct for as long as `inner` doesn't
// reallocate. Moving inner shouldn't change the pointer to it's internal Vec<u8>.

#[derive(Debug)]
pub struct Config {
    includes: Vec<Statement<'static>>,
}

impl Config {
    pub fn from_env() -> Self {
        if let Some(ref conf_path) = crate::ARGS.config {
            if let Ok(data) = std::fs::read(conf_path) {
                if let Ok(s) = String::from_utf8(data) {
                    return Self::from_string(s);
                }
            }
        }

        if let Ok(data) = std::fs::read(".dumpfmt").or_else(|_| std::fs::read("~/.dumpfmt")) {
            if let Ok(s) = String::from_utf8(data) {
                return Self::from_string(s);
            }
        }

        Self {
            includes: Vec::new(),
        }
    }

    pub fn from_string(s: String) -> Self {
        let mut includes = Vec::new();

        let s: &'static str = Box::leak(s.into_boxed_str());
        for mut line in s.lines() {
            skip_whitespace(&mut line);

            if !line.starts_with("use") {
                continue;
            }

            line = &line[3..];
            if !skip_whitespace(&mut line) {
                continue;
            }

            let mut rename = "";
            let mut is_rename_statement = false;
            if let Some((statement, renamed)) = line.split_once(" as ") {
                line = statement.trim();

                rename = renamed.trim_start();
                rename = &rename[..rename.find(' ').unwrap_or(rename.len())];
                is_rename_statement = true;
            }

            let mut path = Vec::new();

            while let Some(end_of_path_part) = line.find("::") {
                path.push(&line[..end_of_path_part]);
                line = &line[end_of_path_part + 2..];
            }

            let mut is_include_statement = false;
            if let Some(include_spot) = line.find('*') {
                line = &line[..include_spot];
                is_include_statement = true;
            }

            if !line.is_empty() {
                path.push(line);
            }

            let statement = if is_include_statement {
                Statement::Include(path)
            } else if is_rename_statement {
                Statement::Rename(path, rename)
            } else {
                Statement::Path(path)
            };

            includes.push(statement);
        }

        includes.sort_unstable();

        Self { includes }
    }

    /// Returns all entries in slice that start with `target`.
    pub fn search(&self) -> Search {
        Search {
            matches: self.includes.as_slice(),
            part_amount_match: 0,
            best_match: None,
        }
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

#[derive(Debug, Default)]
pub struct Search<'a> {
    matches: &'a [Statement<'static>],
    part_amount_match: usize,
    pub best_match: Option<&'a Statement<'static>>,
}

impl<'a> Search<'a> {
    /// Returns all entries in slice that start with `target` at `offset`.
    pub fn binary_search_range(&mut self, target: &str) {
        if self.matches.is_empty() {
            return;
        }

        let midpoint = self
            .matches
            .iter()
            .position(|statement| statement.path().get(self.part_amount_match) == Some(&target));

        let ((mut start, mut end), (left, right)) = match midpoint {
            Some(midpoint) => ((midpoint, midpoint), self.matches.split_at(midpoint)),
            None => {
                self.matches = &[];
                return;
            }
        };

        // Go to the left and check if there are more matches.
        for matched in left.iter().rev() {
            if matched.path().get(self.part_amount_match) == Some(&target) {
                start -= 1;
            }
        }

        // Go to the right and check if there are more matches.
        for matched in right {
            if matched.path().get(self.part_amount_match) == Some(&target) {
                end += 1;
            }
        }

        self.matches = &self.matches[start..end];
        self.part_amount_match += 1;
    }

    pub fn calculate_matches(&mut self) {
        for matched in self.matches {
            if matched.path().len() == self.part_amount_match {
                self.best_match = Some(matched);
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cfg_generate() {
        let text = r#"
            use        std::path::Path
                                                use std::mem::*
            use     std::mem::transmute          as       rename D:
        "#;

        let conf = Config::from_string(text.to_string());
        let conf = conf.includes;

        assert!(conf.contains(&Statement::Path(vec!["std", "path", "Path"])));
        assert!(conf.contains(&Statement::Include(vec!["std", "mem"])));
        assert!(conf.contains(&Statement::Rename(
            vec!["std", "mem", "transmute"],
            "rename"
        )));
    }
}
