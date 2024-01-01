use crate::tprint;
use commands::{Command, CommandError};

impl<Arch: crate::Target> super::UI<Arch> {
    /// Runs all queued commands, returning if they trigger a process exit.
    pub fn process_commands(&mut self, commands: &[String]) -> bool {
        commands.iter().all(|cmd| self.process_cmd(cmd))
    }

    /// Runs a singular commands, returning if it should exit the process.
    fn process_cmd(&mut self, cmd: &str) -> bool {
        let empty_index = disassembler::Index::new();
        let index = self
            .panels
            .listing()
            .map(|l| l.disassembly.processor.symbols())
            .unwrap_or(&empty_index);

        match Command::parse(index, cmd, 0) {
            Ok(Command::Load(path)) => self.offload_binary_processing(path),
            Ok(Command::PrintPath) => match std::env::current_dir() {
                Ok(path) => tprint!(
                    self.panels.terminal(),
                    "Working directory {}.",
                    path.display()
                ),
                Err(err) => tprint!(self.panels.terminal(), "Failed to print pwd: '{err}'."),
            },
            Ok(Command::ChangeDir(path)) => {
                if let Err(err) = std::env::set_current_dir(path) {
                    tprint!(
                        self.panels.terminal(),
                        "Failed to change directory: '{err}'."
                    );
                    return true;
                }

                match std::env::current_dir() {
                    Ok(path) => tprint!(
                        self.panels.terminal(),
                        "Changed working directory to {}.",
                        path.display()
                    ),
                    Err(err) => tprint!(self.panels.terminal(), "Failed to print pwd: '{err}'."),
                }
            }
            Ok(Command::Quit) => return false,
            Ok(Command::Run(args)) => self.offload_debugging(args),
            Ok(Command::Goto(addr)) => {
                let listing = match self.panels.listing() {
                    Some(dissasembly) => dissasembly,
                    None => {
                        tprint!(self.panels.terminal(), "There are no targets to inspect.");
                        return true;
                    }
                };

                if listing.disassembly_view.jump(&listing.disassembly, addr) {
                    listing.update();
                    tprint!(self.panels.terminal(), "Jumped to address {addr:#X}.");
                } else {
                    tprint!(self.panels.terminal(), "Address {addr:#X} is undefined.");
                }
            }
            Ok(Command::Break(addr)) => {
                if self.dbg_ctx.breakpoints.write().unwrap().create(addr) {
                    tprint!(self.panels.terminal(), "Breakpoint {addr:#X} set.");
                } else {
                    tprint!(self.panels.terminal(), "Breakpoint already set.");
                }
            }
            Ok(Command::BreakDelete(addr)) => {
                if self.dbg_ctx.breakpoints.write().unwrap().remove(addr) {
                    tprint!(self.panels.terminal(), "Breakpoint {addr:#X} removed.");
                } else {
                    tprint!(self.panels.terminal(), "Breakpoint {addr:#X} wasn't set.");
                }
            }
            Ok(Command::SetEnv(env)) => self.dbg_settings.env.push(env),
            Ok(Command::Stop) => {
                if !self.dbg_ctx.attached() {
                    tprint!(self.panels.terminal(), "There are no targets to stop.");
                    return true;
                }

                self.dbg_ctx.queue.push(debugger::DebugeeEvent::Break);
            }
            Ok(Command::Continue) => {
                if !self.dbg_ctx.attached() {
                    tprint!(self.panels.terminal(), "There are no targets to continue.");
                    return true;
                }

                self.dbg_ctx.queue.push(debugger::DebugeeEvent::Continue);
            }
            Ok(Command::Clear) => {
                log::LOGGER.lock().unwrap().clear();
                self.panels.terminal().clear();
            }
            Ok(Command::Trace) => {
                if self.dbg_ctx.attached() {
                    tprint!(self.panels.terminal(), "Debugger already running.");
                } else if self.dbg_settings.tracing == false {
                    tprint!(self.panels.terminal(), "Enabled syscall tracing.");
                    self.dbg_settings.tracing = true;
                } else if self.dbg_settings.tracing == true {
                    tprint!(self.panels.terminal(), "Disabled syscall tracing.");
                    self.dbg_settings.tracing = false;
                }
            }
            Ok(Command::FollowChildren) => {
                if self.dbg_ctx.attached() {
                    tprint!(self.panels.terminal(), "Debugger already running.");
                } else if self.dbg_settings.follow_children == false {
                    tprint!(self.panels.terminal(), "Enabled syscall tracing of children.");
                    self.dbg_settings.follow_children = true;
                } else if self.dbg_settings.follow_children == true {
                    tprint!(self.panels.terminal(), "Disabled syscall tracing of children.");
                    self.dbg_settings.follow_children = false;
                }
            },
            Ok(Command::Suggestion(_)) => {},
            Err(CommandError::Missing("command")) => {}
            Err(err) => {
                tprint!(self.panels.terminal(), "{err}");
            }
        }

        true
    }
}
