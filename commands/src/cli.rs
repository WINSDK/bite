use std::path::{Path, PathBuf};

macro_rules! exit {
    ($code:expr => $($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit($code);
    }};
}

const HELP: &str = "OVERVIEW: Debugger/Decompilation tool

USAGE: bite [options] <OBJECT>

OPTIONS:
  -H, --help          Print usage information
  -D, --disassemble   Path to object you're disassembling
  -C, --config        Path to config used for disassembling
  -B, --debug         Enable verbose internal info";

const ABBRV: &[&str] = &["-H", "-D", "-C", "-B"];
const NAMES: &[&str] = &[
    "--help",
    "--disassemble",
    "--config",
    "--debug",
];

#[derive(Default, Debug, Clone)]
pub struct Cli {
    /// Path to symbol being disassembled.
    pub path: PathBuf,

    /// Optional path to config.
    pub config: Option<PathBuf>,

    /// Show egui debug overlay.
    pub debug: bool,
}

impl Cli {
    pub fn parse() -> Self {
        let mut cli = Cli::default();
        let mut args = std::env::args().skip(1).peekable();

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-H" | "--help" => exit!(0 => "{HELP}"),
                "-D" | "--disassemble" => {
                    if let Some(path) = args.next().as_deref() {
                        if !NAMES.contains(&path) && !ABBRV.contains(&path) {
                            if cli.path != Path::new("") {
                                exit!(1 => "Path to object already given.");
                            }
                            cli.path = PathBuf::from(path);
                        }
                    }
                },
                "-C" | "--config" => {
                    if let Some(path) = args.next().as_deref() {
                        if !NAMES.contains(&path) && !ABBRV.contains(&path) {
                            if cli.config.is_some() {
                                exit!(1 => "Path to config already given.");
                            }
                            cli.config = Some(PathBuf::from(path));
                        }
                    }
                },
                "-B" | "--debug" => {
                    if cli.debug {
                        exit!(1 => "Debug flag already set.");
                    }
                    cli.debug = true
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
                        exit!(1 => "Unknown cmd arg '{unknown}' did you mean '{best_guess}'?")
                    } else {
                        exit!(1 => "Unknown cmd arg '{unknown}' was entered.");
                    }
                }
            }
        }

        cli.validate_args();
        cli
    }

    fn validate_args(&mut self) {
        if self.path == Path::new("") {
            // exit!(1 => "You must provide a path to disassemble.");
            return;
        }

        if !self.path.exists() {
            exit!(1 => "Object {:?} does not exist.", self.path);
        }

        if let Some(ref cfg) = self.config {
            if !cfg.exists() {
                exit!(1 => "Config {cfg:?} does not exist.");
            }
        }
    }
}
