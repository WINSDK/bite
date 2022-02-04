use std::path::PathBuf;

use crate::{assert_exit, exit};

const HELP: &'static str = "OVERVIEW: rust object dumper

USAGE: rustdump [options] <OBJECT>

OPTIONS:
  -H, --help          Print usage information
  -L, --libs          Print linked shared libraries 
  -N, --names         Print all symbols exposed by object
  -D, --dissasembly   Path to object you're disassembling
  -S, --simplify      Replace common types with shortened paths";

#[derive(Debug, Clone)]
pub struct Cli {
    /// Print shared libraries the object is linked against.
    pub libs: bool,

    /// Print all symbols exposed by object.
    pub names: bool,

    /// Strip symbols into a simpler format.
    pub simplify: bool,

    /// Disassemble object into `readable` assembly,
    pub disassemble: bool,

    /// Path to symbol being disassembled.
    pub path: PathBuf,
}

impl Cli {
    pub fn parse() -> Self {
        let mut cli = Cli {
            libs: false,
            names: false,
            simplify: false,
            disassemble: false,
            path: PathBuf::new(),
        };

        let mut args = std::env::args().skip(1).peekable();
        while let Some(arg) = args.next() {
            if args.peek().is_none() {
                cli.path = PathBuf::from(arg);
                break;
            }

            match arg.as_str() {
                "-H" | "--help" => exit!("{HELP}"),
                "-S" | "--simplify" => cli.simplify = true,
                "-N" | "--names" => cli.names = true,
                "-D" | "--disassemble" => cli.disassemble = true,
                "-L" | "--libs" => cli.libs = true,
                unknown => exit!("Unknown cmd arg '{unknown}' was entered"),
            }
        }

        cli.validate_args();
        cli
    }

    fn validate_args(&self) {
        assert_exit!(self.path.is_file(), "{HELP}");
        assert_exit!(
            self.disassemble as u8 + self.libs as u8 + self.names as u8 == 1,
            "Invalid combination of required arguements"
        );
    }
}
