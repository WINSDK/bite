use std::fmt;
use std::path::{Path, PathBuf};

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
            Self::InvalidEnv => f.write_str("Invalid environmental variable pair."),
            Self::Debugger(err) => err.fmt(f),
        }
    }
}

fn expand_homedir(path: PathBuf) -> std::path::PathBuf {
    if !path.starts_with("~") {
        return path.to_path_buf();
    }

    let mut home_dir = match dirs::home_dir() {
        Some(dir) => dir,
        None => return path.to_path_buf(),
    };

    if path == Path::new("~") {
        return home_dir;
    }

    if home_dir == Path::new("/") {
        // Corner case: `home_dir` root directory;
        // don't prepend extra `/`, just drop the tilde.
        path.strip_prefix("~").unwrap().to_path_buf()
    } else {
        home_dir.push(path.strip_prefix("~/").unwrap());
        home_dir
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
}

impl<'src> Context<'src> {
    /// Create's a new [`Context`].
    pub fn new(index: &'src symbols::Index, src: &'src str) -> Self {
        Self {
            src,
            index,
            offset: 0,
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

    fn parse_path(&mut self) -> Result<PathBuf, Error> {
        let s = self.parse_arg("path").unwrap_or("~");
        let path = expand_homedir(PathBuf::from(s));
        if !path.exists() {
            return Err(Error::PathDoesntExist(path));
        }

        Ok(path)
    }

    fn parse_env(&mut self) -> Result<String, Error> {
        let s = self.parse_arg("environmental variable")?;
        let (var, val) = s.split_once("=").ok_or(Error::InvalidEnv)?;
        Ok(format!("{var}={val}"))
    }

    fn parse_debug_expr(&mut self) -> Result<usize, Error> {
        let s = self.parse_arg("expr")?;
        crate::debug::parse(&self.index, s).map_err(Error::Debugger)
    }

    fn parse(&mut self) -> Result<Command, Error> {
        let name = match self.parse_next("command")? {
            "exec" | "e" => Command::Load(self.parse_path()?),
            "pwd" => Command::PrintPath,
            "cd" => Command::ChangeDir(self.parse_path()?),
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
    pub fn parse(index: &symbols::Index, s: &str) -> Result<Self, Error> {
        Context::new(index, s).parse()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use symbols::{Function, TokenStream};

    macro_rules! eval_eq {
        ($expr:expr, $expected:expr) => {{
            let index = symbols::Index::new();

            match Command::parse(&index, $expr) {
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

            match Command::parse(&index, $expr) {
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
