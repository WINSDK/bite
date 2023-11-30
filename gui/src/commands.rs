use std::fmt::Write;
use std::path::Path;

use crate::panels::{Terminal, Panels};

const CMDS: &[&str] = &["exec", "pwd", "cd", "quit", "run", "goto", "set"];

macro_rules! print {
    ($dst:expr, $($arg:tt)*) => {{
        let _ = writeln!($dst, $($arg)*);
    }};
}

/// Print to the terminal outside of [`process_cmd`].
#[macro_export]
macro_rules! print_extern {
    ($dst:expr, $($arg:tt)*) => {{
        let _ = writeln!($dst, $($arg)*);
    }};
}


fn possible_command(unknown: &str) -> Option<&str> {
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

fn print_cwd(terminal: &mut Terminal) {
    match std::env::current_dir() {
        Ok(path) => print!(terminal, "Working directory {}.", path.display()),
        Err(err) => print!(terminal, "Failed to print pwd: '{err}'."),
    }
}

fn expand_homedir<P: AsRef<Path>>(path: P) -> std::path::PathBuf {
    let path = path.as_ref();

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

impl Panels {
    /// Runs a list of commands, returning if they succeeded.
    pub fn process_commands(&mut self, commands: &[String]) -> bool {
        commands.iter().all(|cmd| self.process_cmd(cmd))
    }

    /// Runs a singular commands, returning if it succeeded.
    fn process_cmd(&mut self, cmd: &str) -> bool {
        print!(self.terminal(), "(bite) {cmd}");

        let cmd = cmd.trim();
        let mut args = cmd.split_whitespace();
        let cmd_name = match args.next() {
            Some(cmd) => cmd,
            None => return true,
        };

        if cmd_name == "exec" || cmd_name == "e" {
            if let Some(unexpanded) = args.next() {
                let path = expand_homedir(unexpanded);

                self.ui_queue.push(crate::UIEvent::BinaryRequested(path));
                print!(self.terminal(), "Binary '{unexpanded}' was opened.");
                return true;
            }

            print!(self.terminal(), "Command 'exec' requires a path.");
            return true;
        }

        if cmd_name == "cd" {
            let path = expand_homedir(args.next().unwrap_or("~"));

            if let Err(err) = std::env::set_current_dir(path) {
                print!(self.terminal(), "Failed to change directory: '{err}'.");
                return true;
            }

            print_cwd(self.terminal());
            return true;
        }

        if cmd_name == "pwd" {
            print_cwd(self.terminal());
            return true;
        }

        if cmd_name == "r" || cmd_name == "run" {
            let mut args: Vec<String> = Vec::new();

            if let Some((_, raw_args)) = cmd.split_once("--") {
                args = raw_args.split_whitespace().map(ToString::to_string).collect();
            }

            self.ui_queue.push(crate::UIEvent::DebuggerExecute(args));
            return true;
        }

        if cmd_name == "set" {
            let (var, value) = match cmd.split_once("=") {
                Some(pair) => pair,
                None => {
                    print!(self.terminal(), "You must specify what env variable to set.");
                    return true;
                }
            };

            std::env::set_var(var, value);
            return true;
        }

        if cmd_name == "quit" || cmd_name == "q" {
            return false;
        }

        if cmd_name == "goto" || cmd_name == "g" {
            let listing = match self.listing() {
                Some(dissasembly) => dissasembly,
                None => {
                    print!(self.terminal(), "There are no targets to inspect.");
                    return true;
                }
            };

            let expr = cmd.strip_prefix("goto").or(cmd.strip_prefix("g")).unwrap_or(cmd);

            match disassembler::expr::parse(&listing.disassembly.symbols, expr) {
                Ok(addr) => {
                    if listing.disassembly_view.jump(&listing.disassembly, addr) {
                        listing.update();
                        print!(self.terminal(), "Jumped to address '{addr:#X}'.");
                    } else {
                        print!(self.terminal(), "Address '{addr:#X}' is undefined.");
                    }
                }
                Err(err) => print!(self.terminal(), "{err:?}."),
            }

            return true;
        }

        match possible_command(cmd_name) {
            Some(guess) => print!(
                self.terminal(),
                "Command '{cmd_name}' is unknown, did you mean '{guess}'?"
            ),
            None => print!(self.terminal(), "Command '{cmd_name}' is unknown."),
        }

        true
    }
}
