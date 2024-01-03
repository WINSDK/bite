use std::fmt;
use std::ops::Range;
use std::path::{Path, PathBuf};

use crate::debug::CompleteExpr;

#[derive(Debug, PartialEq)]
pub enum Command {
    Load(PathBuf),
    PrintPath,
    ChangeDir(PathBuf),
    Quit,
    Run(Vec<String>),
    Goto(usize),
    Break(usize),
    BreakDelete(usize),
    SetEnv(String),
    Stop,
    Continue,
    Clear,
    Trace,
    FollowChildren,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Missing(&'static str),
    UnknownName(String),
    PathDoesntExist(PathBuf),
    PathIsntFile(PathBuf),
    PathIsntDir(PathBuf),
    InvalidEnv,
    Debugger(crate::debug::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Missing(part) => f.write_fmt(format_args!("Expected '{part}'.")),
            Self::UnknownName(cmd) => match possible_command(&cmd) {
                Some(guess) => f.write_fmt(format_args!(
                    "Command '{cmd}' is unknown, did you mean '{guess}'?"
                )),
                None => f.write_fmt(format_args!("Command '{cmd}' is unknown.")),
            },
            Self::PathDoesntExist(path) => {
                f.write_fmt(format_args!("Path {path:?} doesn't exist."))
            }
            Self::PathIsntFile(path) => f.write_fmt(format_args!("Path {path:?} isn't a file.")),
            Self::PathIsntDir(path) => f.write_fmt(format_args!("Path {path:?} isn't a directory.")),
            Self::InvalidEnv => f.write_str("Invalid environmental variable pair."),
            Self::Debugger(err) => err.fmt(f),
        }
    }
}

fn expand_homedir(path: PathBuf) -> PathBuf {
    let home_dir = match dirs::home_dir() {
        Some(dir) => dir,
        None => return path,
    };

    match path.strip_prefix("~") {
        Ok(relative_path) => home_dir.join(relative_path),
        Err(_) => path
    }
}

fn collapse_homedir(path: PathBuf) -> PathBuf {
    let home_dir = match dirs::home_dir() {
        Some(dir) => dir,
        None => return path,
    };

    match path.strip_prefix(&home_dir) {
        Ok(relative_path) => Path::new("~").join(relative_path),
        Err(_) => path
    }
}

fn possible_command(unknown: &str) -> Option<&str> {
    const CMDS: &[&str] = &[
        "exec",
        "pwd",
        "cd",
        "quit",
        "run",
        "goto",
        "set",
        "break",
        "delete",
        "stop",
        "continue",
        "clear",
        "trace",
        "follow-children",
    ];

    let mut distance = u32::MAX;
    let mut best_guess = "";
    for cmd in CMDS {
        let d = triple_accel::levenshtein_exp(unknown.as_bytes(), cmd.as_bytes());
        if d < distance {
            distance = d;
            best_guess = cmd;
        }
    }

    // A guess that's less than 2 `steps` away from a correct arg.
    (distance <= 2).then_some(best_guess)
}

#[derive(Debug)]
struct Context<'src> {
    /// Reference to input string.
    src: &'src str,

    /// Symbol lookup table.
    index: &'src symbols::Index,

    /// Offset into input string.
    offset: usize,

    /// Cursor position in bytes.
    cursor: usize,

    /// Command suggestions on failure.
    suggestions: Vec<String>,
}

impl<'src> Context<'src> {
    /// Create's a new [`Context`].
    pub fn new(index: &'src symbols::Index, src: &'src str, cursor: usize) -> Self {
        Self {
            src,
            index,
            offset: 0,
            cursor,
            suggestions: Vec::new(),
        }
    }

    /// Where we are in the string.
    fn src(&self) -> &'src str {
        &self.src[self.offset..]
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.src().chars().next() {
                Some(' ') => self.offset += 1,
                Some(_) | None => break,
            }
        }
    }

    fn parse_till_whitespace(&mut self) -> &'src str {
        let start = self.offset;
        loop {
            match self.src().chars().next() {
                Some(' ') | None => break,
                Some(chr) => self.offset += chr.len_utf8(),
            }
        }

        &self.src[start..self.offset]
    }

    fn parse_next(&mut self, expected: &'static str) -> Result<&'src str, Error> {
        self.skip_whitespace();

        let s = self.parse_till_whitespace();
        if s.is_empty() {
            return Err(Error::Missing(expected));
        }

        self.skip_whitespace();
        Ok(s)
    }

    fn parse_arg(&mut self, expected: &'static str) -> Result<&'src str, Error> {
        let s = self.src().trim();

        // mark all remaining characters as read
        self.offset = self.src.len() - 1;

        if s.is_empty() {
            return Err(Error::Missing(expected));
        }

        Ok(s)
    }

    fn parse_file_path(&mut self) -> Result<PathBuf, Error> {
        let start = self.offset;
        let s = self.parse_arg("path").unwrap_or_default();
        let path = expand_homedir(PathBuf::from(s));

        if path.is_file() {
            return Ok(path);
        }

        if path.exists() {
            return Err(Error::PathIsntFile(path));
        }

        self.autocomplete_path(start, s);
        Err(Error::PathDoesntExist(path))
    }

    fn parse_dir_path(&mut self) -> Result<PathBuf, Error> {
        let start = self.offset;
        let s = self.parse_arg("path").unwrap_or_default();
        let path = expand_homedir(PathBuf::from(s));

        if path.is_dir() {
            return Ok(path);
        }

        if path.exists() {
            return Err(Error::PathIsntDir(path));
        }

        self.autocomplete_path(start, s);
        Err(Error::PathDoesntExist(path))
    }

    fn autocomplete_path(&mut self, start: usize, s: &str) {
        let path = expand_homedir(PathBuf::from(s));
        let subpath = path.components().fold(PathBuf::from("."), |mut valid_path, comp| {
            valid_path.push(comp);
            if !valid_path.exists() {
                valid_path.pop();
            }
            valid_path
        });

        fn last_path_component(path: &Path) -> &str {
            path.components()
                .last()
                .and_then(|comp| comp.as_os_str().to_str())
                .unwrap_or_default()
        }

        let last_comp = if s == "./" {
            s
        } else {
            last_path_component(&path)
        };

        if let Ok(dir) = subpath.read_dir() {
            let mut entries: Vec<PathBuf> = dir
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.path())
                .collect();

            entries.sort_unstable();

            for entry in entries {
                let entry = last_path_component(&entry).to_string();

                if !entry.starts_with(&last_comp) {
                    continue;
                }

                let mut path = path.clone();
                path.pop();
                path.push(&entry);

                let is_dir = path.is_dir();
                let mut path = collapse_homedir(path);

                // append '/' to suggestion to allow easier navigation through directories
                if is_dir {
                    path.pop();
                    path.push(entry.to_string() + "/");
                }

                if let Some(path) = path.to_str() {
                    self.suggestions.push(self.src[..start].to_string() + path);
                }
            }
        }
    }

    fn parse_env(&mut self) -> Result<String, Error> {
        let s = self.parse_arg("environmental variable")?;
        let (var, val) = s.split_once("=").ok_or(Error::InvalidEnv)?;
        Ok(format!("{var}={val}"))
    }

    fn parse_debug_expr(&mut self) -> Result<usize, Error> {
        let offset = self.offset;
        let s = self.parse_arg("expr")?;
        let expr = CompleteExpr::parse(s).map_err(Error::Debugger)?;

        let err = match expr.eval(self.index) {
            Ok(val) => return Ok(val as usize),
            Err(err) => err,
        };

        if let Some(relative_cursor) = self.cursor.checked_sub(offset) {
            if let Some((suggestion, span)) = expr.autocomplete(self.index, relative_cursor) {
                let mut src = self.src.to_string();
                let span = Range {
                    start: span.start + offset,
                    end: span.end + offset,
                };
                src.replace_range(span, &suggestion);
                self.suggestions.push(src);
            }
        }

        Err(Error::Debugger(err))
    }

    fn parse(&mut self) -> Result<Command, Error> {
        let name = match self.parse_next("command")? {
            "exec" | "e" => Command::Load(self.parse_file_path()?),
            "pwd" => Command::PrintPath,
            "cd" => Command::ChangeDir(self.parse_dir_path()?),
            "quit" | "q" => Command::Quit,
            "run" | "r" => {
                let mut args = Vec::new();
                if let Ok("--") = self.parse_next("split") {
                    while let Ok(arg) = self.parse_next("arg") {
                        args.push(arg.to_string());
                    }
                }
                Command::Run(args)
            }
            "set" => Command::SetEnv(self.parse_env()?),
            "goto" | "g" => Command::Goto(self.parse_debug_expr()?),
            "break" | "b" => Command::Break(self.parse_debug_expr()?),
            "delete" | "bd" => Command::BreakDelete(self.parse_debug_expr()?),
            "stop" | "s" => Command::Stop,
            "continue" | "c" => Command::Continue,
            "clear" => Command::Clear,
            "trace" => Command::Trace,
            "follow-children" => Command::FollowChildren,
            name => return Err(Error::UnknownName(name.to_string())),
        };

        Ok(name)
    }
}

impl Command {
    pub fn parse(
        index: &symbols::Index,
        s: &str,
        cursor: usize,
    ) -> Result<Self, (Error, Vec<String>)> {
        let mut ctx = Context::new(index, s, cursor);
        ctx.parse().map_err(|err| (err, ctx.suggestions))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use symbols::{Function, TokenStream};

    macro_rules! eval_eq {
        ($expr:expr, $expected:expr) => {{
            let index = symbols::Index::new();

            match Command::parse(&index, $expr, 0) {
                Err(err) => panic!("failed to parse '{}' with error '{:?}'", $expr, err),
                Ok(parsed) => assert_eq!(parsed, $expected)
            }
        }};

        ([$($function:expr; $addr:expr),*], $expr:expr, $expected:expr) => {{
            #[allow(unused_mut)]
            let mut index = symbols::Index::new();

            $(
                let f = Function::new(TokenStream::simple($function), None);
                index.insert($addr, f);
            )*

            match Command::parse(&index, $expr, 0) {
                Err(err) => panic!("failed to parse '{}' with error '{:?}'", $expr, err),
                Ok(parsed) => assert_eq!(parsed, $expected)
            }
        }};
    }

    #[test]
    #[should_panic]
    fn empty() {
        eval_eq!("", Command::Continue);
    }

    #[test]
    fn breaking() {
        eval_eq!("break 3 * 32", Command::Break(96));
    }

    #[test]
    #[should_panic]
    fn breaking_invalid() {
        eval_eq!(["f"; 0x1234], "break main", Command::Break(96));
    }

    #[test]
    fn goto() {
        eval_eq!(["abc::f"; 0x1234], "goto abc::f", Command::Goto(0x1234));
        eval_eq!(
            ["abc::f"; 0x1234],
            "      goto   2 * abc::f ",
            Command::Goto(2 * 0x1234)
        );
    }

    #[test]
    fn run() {
        eval_eq!("r", Command::Run(Vec::new()));
        eval_eq!("run", Command::Run(Vec::new()));
        eval_eq!("run --", Command::Run(Vec::new()));
        eval_eq!("run -- abc", Command::Run(vec!["abc".to_string()]));
        eval_eq!("r -- abc", Command::Run(vec!["abc".to_string()]));
        eval_eq!(
            "r -- abc dba",
            Command::Run(vec!["abc".to_string(), "dba".to_string()])
        );
        eval_eq!(
            "r --   abc       dba    ",
            Command::Run(vec!["abc".to_string(), "dba".to_string()])
        );
    }

    #[test]
    fn set_env() {
        eval_eq!("set a=10", Command::SetEnv("a=10".to_string()));
    }

    #[test]
    #[should_panic]
    fn set_env_invalid() {
        eval_eq!("set a10", Command::SetEnv("a=10".to_string()));
    }

    #[test]
    #[should_panic]
    fn set_env_expected() {
        eval_eq!("set", Command::SetEnv("a=10".to_string()));
    }

    #[test]
    fn change_dir() {
        let home = expand_homedir(PathBuf::from("~"));
        eval_eq!("cd ~", Command::ChangeDir(home));
        eval_eq!("cd   / ", Command::ChangeDir(PathBuf::from("/")));
        eval_eq!("cd . ", Command::ChangeDir(PathBuf::from(".")));
    }

    #[test]
    #[should_panic]
    fn change_dir_invalid() {
        eval_eq!("cd ???", Command::ChangeDir(PathBuf::from("???")));
    }
}
