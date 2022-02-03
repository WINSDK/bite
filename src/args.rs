use std::path::PathBuf;

const HELP: &'static str = "OVERVIEW: rust object dumper

USAGE: rustdump [options] <OBJECTS>

OPTIONS:
  -H, --help          Print usage information
  -D, --dissasembly   Path to object you're disassembling
  -S, --simplify      Replace common types with shortened paths";

macro_rules! exit {
    () => {{
        std::process::exit(1);
    }};

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
    }};
}

macro_rules! assert_exit {
    ($cond:expr $(,)?) => {{
        if !($cond) {
            exit!();
        }
    }};

    ($cond:expr, $($arg:tt)+) => {{
        if !($cond) {
            exit!($($arg)*);
        }
    }};
}

#[derive(Debug, Clone)]
pub struct Cli {
    /// Strip symbols into a simpler format
    pub simplify: bool,

    /// Path to symbol being disassembled
    pub disassemble: PathBuf,
}

impl Cli {
    pub fn parse() -> Self {
        let mut cli = Cli {
            simplify: false,
            disassemble: PathBuf::new(),
        };

        let mut args = std::env::args().skip(1);
        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-H" | "--help" => exit!("{HELP}"),
                "-S" | "--simplify" => cli.simplify = true,
                "-D" | "--disassemble" => {
                    cli.disassemble = match args.next().as_deref() {
                        Some("") | None => exit!("Must enter path to disassembled file"),
                        Some(path) => PathBuf::from(path),
                    }
                }
                unknown => exit!("Unknown cmd arg '{unknown}' was entered"),
            }
        }

        cli.validate_args();
        cli
    }

    fn validate_args(&self) {
        assert_exit!(self.disassemble.is_file(), "{HELP}");
    }
}
