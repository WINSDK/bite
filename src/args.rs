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

const NAMES: &[&'static str] = &["--help", "--libs", "--names", "--dissasembly", "--simplify"];
const SHORT: &[&'static str] = &["-H", "-L", "-N", "-D", "-S"];

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
                "-L" | "--libs" => cli.libs = true,
                "-D" | "--disassemble" => {
                    cli.disassemble = true;

                    if let Some(path) = args.next().as_deref() {
                        if NAMES.contains(&path) || SHORT.contains(&path) {
                            exit!("Must specify path to object");
                        }

                        cli.path = PathBuf::from(path);
                    } else {
                        exit!("Must specify path to object");
                    }
                }
                unknown => {
                    let mut distance = u32::MAX;
                    let mut best_guess = "";
                    for name in NAMES {
                        let d = triple_accel::levenshtein_exp(unknown.as_bytes(), name.as_bytes());
                        if d < distance {
                            distance = d;
                            best_guess = name;
                        }
                    }

                    // A guess that's less than 3 `steps` away from a correct arg.
                    if distance < 4 {
                        exit!("Unknown cmd arg '{unknown}' did you mean '{best_guess}'?");
                    } else {
                        exit!("Unknown cmd arg '{unknown}' was entered.");
                    }
                }
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
