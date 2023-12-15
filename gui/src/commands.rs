use std::fmt::Write;
use std::path::Path;

use crate::panels::{Panels, Terminal};

const CMDS: &[&str] = &[
    "exec", "pwd", "cd", "quit", "run", "goto", "set", "break", "delete", "stop", "continue",
    "clear", "trace", "follow-children"
];

/// Print to the terminal.
#[macro_export]
macro_rules! tprint {
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
        Ok(path) => tprint!(terminal, "Working directory {}.", path.display()),
        Err(err) => tprint!(terminal, "Failed to print pwd: '{err}'."),
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
        tprint!(self.terminal(), "(bite) {cmd}");

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
                tprint!(self.terminal(), "Binary '{unexpanded}' was opened.");
                return true;
            }

            tprint!(self.terminal(), "Command 'exec' requires a path.");
            return true;
        }

        if cmd_name == "cd" {
            let path = expand_homedir(args.next().unwrap_or("~"));

            if let Err(err) = std::env::set_current_dir(path) {
                tprint!(self.terminal(), "Failed to change directory: '{err}'.");
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
            let expr = cmd.strip_prefix("set").unwrap().trim();
            if expr.split_once("=").is_none() {
                tprint!(
                    self.terminal(),
                    "You must specify what env variable to set."
                );
                return true;
            };

            self.ui_queue.push(crate::UIEvent::SetEnvironmental(expr.to_string()));
            return true;
        }

        if cmd_name == "quit" || cmd_name == "q" {
            return false;
        }

        if cmd_name == "goto" || cmd_name == "g" {
            let listing = match self.listing() {
                Some(dissasembly) => dissasembly,
                None => {
                    tprint!(self.terminal(), "There are no targets to inspect.");
                    return true;
                }
            };

            let expr = cmd.strip_prefix("goto").or(cmd.strip_prefix("g")).unwrap().trim();

            match disassembler::expr::parse(&listing.disassembly.processor.symbols(), expr) {
                Ok(addr) => {
                    if listing.disassembly_view.jump(&listing.disassembly, addr) {
                        listing.update();
                        tprint!(self.terminal(), "Jumped to address {addr:#X}.");
                    } else {
                        tprint!(self.terminal(), "Address {addr:#X} is undefined.");
                    }
                }
                Err(err) => tprint!(self.terminal(), "{err:?}."),
            }

            return true;
        }

        if cmd_name == "break" || cmd_name == "b" {
            let listing = match self.listing() {
                Some(dissasembly) => dissasembly,
                None => {
                    tprint!(self.terminal(), "There are no targets to debug.");
                    return true;
                }
            };

            let expr = cmd.strip_prefix("break").or(cmd.strip_prefix("b")).unwrap().trim();
            match disassembler::expr::parse(&listing.disassembly.processor.symbols(), expr) {
                Ok(addr) => {
                    if listing.disassembly.processor.instruction_by_addr(addr).is_none() {
                        tprint!(self.terminal(), "Address {addr:#X} is undefined.");
                    } else {
                        self.dbg_ctx.breakpoints.write().unwrap().create(addr);
                        tprint!(self.terminal(), "Breakpoint at {addr:#X} queued.");
                    }
                }
                Err(err) => tprint!(self.terminal(), "{err:?}."),
            }

            return true;
        }

        if cmd_name == "delete" || cmd_name == "db" {
            let listing = match self.listing() {
                Some(dissasembly) => dissasembly,
                None => {
                    tprint!(self.terminal(), "There are no targets to debug.");
                    return true;
                }
            };

            let expr = cmd.strip_prefix("delete").or(cmd.strip_prefix("db")).unwrap().trim();
            match disassembler::expr::parse(&listing.disassembly.processor.symbols(), expr) {
                Ok(addr) => {
                    if listing.disassembly.processor.instruction_by_addr(addr).is_none() {
                        tprint!(self.terminal(), "Address {addr:#X} is undefined.");
                    } else {
                        self.dbg_ctx.breakpoints.write().unwrap().remove(addr);
                        tprint!(self.terminal(), "Breakpoint {addr:#X} removed.");
                    }
                }
                Err(err) => tprint!(self.terminal(), "{err:?}."),
            }
            return true;
        }

        if cmd == "stop" || cmd == "s" {
            if !self.dbg_ctx.attached() {
                tprint!(self.terminal(), "There are no targets to stop.");
                return true;
            }

            self.dbg_ctx.queue.push(debugger::DebugeeEvent::Break);
            return true;
        }

        if cmd == "continue" || cmd == "c" {
            if !self.dbg_ctx.attached() {
                tprint!(self.terminal(), "There are no targets to continue.");
                return true;
            }

            self.dbg_ctx.queue.push(debugger::DebugeeEvent::Continue);
            return true;
        }

        if cmd == "trace" {
            // FIXME
        }

        if cmd == "follow-children" {
            // FIXME
        }

        if cmd == "clear" {
            log::LOGGER.lock().unwrap().clear();
            self.terminal().clear();
            return true;
        }

        match possible_command(cmd_name) {
            Some(guess) => tprint!(
                self.terminal(),
                "Command '{cmd_name}' is unknown, did you mean '{guess}'?"
            ),
            None => tprint!(self.terminal(), "Command '{cmd_name}' is unknown."),
        }

        true
    }
}
