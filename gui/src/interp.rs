use crate::tprint;
use commands::{Command, CommandError};
use debugger::Event;

impl<Arch: crate::Target> super::UI<Arch> {
    /// Runs all queued commands, returning if they trigger a process exit.
    pub fn process_commands(&mut self, commands: &[String]) -> bool {
        commands.iter().all(|cmd| self.process_cmd(cmd))
    }

    /// Runs a singular commands, returning if it should exit the process.
    fn process_cmd(&mut self, cmd: &str) -> bool {
        let empty_index = symbols::Index::default();
        let index = self.panels.processor().map(|proc| &proc.index).unwrap_or(&empty_index);

        match Command::parse(index, cmd, 0) {
            Ok(Command::GotoSource(addr)) => {
                self.panels.load_source(addr);
            }
            Ok(Command::Load(path)) => self.offload_binary_processing(path),
            Ok(Command::PrintPath) => match std::env::current_dir() {
                Ok(path) => tprint!(
                    self.panels.terminal(),
                    "Working directory {}.",
                    path.display()
                ),
                Err(err) => tprint!(self.panels.terminal(), "Failed to print pwd: {err}."),
            },
            Ok(Command::ChangeDir(path)) => {
                if let Err(err) = std::env::set_current_dir(path) {
                    tprint!(self.panels.terminal(), "Failed to change directory: {err}.");
                    return true;
                }

                match std::env::current_dir() {
                    Ok(path) => tprint!(
                        self.panels.terminal(),
                        "Changed working directory to {}.",
                        path.display()
                    ),
                    Err(err) => tprint!(self.panels.terminal(), "Failed to print pwd: {err}."),
                }
            }
            Ok(Command::Quit) => return false,
            Ok(Command::Run(args)) => match self.debugger {
                Some(_) => tprint!(self.panels.terminal(), "Debugger already running."),
                None => self.offload_debugging(args),
            },
            Ok(Command::Goto(addr)) => {
                let listing = match self.panels.listing() {
                    Some(listing) => listing,
                    None => {
                        tprint!(self.panels.terminal(), "No targets loaded.");
                        return true;
                    }
                };

                if listing.jump(addr) {
                    listing.update();
                    tprint!(self.panels.terminal(), "Jumped to address {addr:#X}.");
                } else {
                    tprint!(self.panels.terminal(), "Address {addr:#X} is undefined.");
                }
            }
            Ok(Command::Break(_addr)) => {
                match self.debugger {
                    Some(ref mut _debugger) => {}
                    None => tprint!(self.panels.terminal(), "No targets running."),
                }

                // if self.dbg_ctx.breakpoints.write().unwrap().create(addr) {
                //     tprint!(self.panels.terminal(), "Breakpoint {addr:#X} set.");
                // } else {
                //     tprint!(self.panels.terminal(), "Breakpoint already set.");
                // }
            }
            Ok(Command::BreakDelete(_addr)) => {
                match self.debugger {
                    Some(ref mut _debugger) => {}
                    None => tprint!(self.panels.terminal(), "No targets running."),
                }
                // if self.dbg_ctx.breakpoints.write().unwrap().remove(addr) {
                //     tprint!(self.panels.terminal(), "Breakpoint {addr:#X} removed.");
                // } else {
                //     tprint!(self.panels.terminal(), "Breakpoint {addr:#X} wasn't set.");
                // }
            }
            Ok(Command::SetEnv(env)) => self.dbg_settings.env.push(env),
            Ok(Command::Stop) => match self.debugger {
                Some(ref debugger) => {
                    for pid in debugger.processes() {
                        match debugger.notify(pid, Event::Pause) {
                            Ok(()) => tprint!(self.panels.terminal(), "Child {pid} stopped."),
                            Err(err) => tprint!(self.panels.terminal(), "{err:?}"),
                        }
                    }
                }
                None => tprint!(self.panels.terminal(), "No targets running."),
            },
            Ok(Command::Continue) => match self.debugger {
                Some(ref debugger) => {
                    for pid in debugger.processes() {
                        match debugger.notify(pid, Event::Continue) {
                            Ok(()) => tprint!(self.panels.terminal(), "Child {pid} continued."),
                            Err(err) => tprint!(self.panels.terminal(), "{err:?}"),
                        }
                    }
                }
                None => tprint!(self.panels.terminal(), "No targets running."),
            },
            Ok(Command::Clear) => {
                log::LOGGER.write().unwrap().clear();
                self.panels.terminal().clear();
            }
            Ok(Command::Trace) => {
                if self.debugger.is_some() {
                    tprint!(self.panels.terminal(), "Debugger already running, can't set tracing.");
                } else if !self.dbg_settings.tracing {
                    tprint!(self.panels.terminal(), "Enabled syscall tracing.");
                    self.dbg_settings.tracing = true;
                } else if self.dbg_settings.tracing {
                    tprint!(self.panels.terminal(), "Disabled syscall tracing.");
                    self.dbg_settings.tracing = false;
                }
            }
            Ok(Command::FollowChildren) => {
                if self.debugger.is_some() {
                    tprint!(self.panels.terminal(), "Debugger already running, can't set follow.");
                } else if !self.dbg_settings.follow_children {
                    tprint!(self.panels.terminal(), "Enabled syscall tracing of children.");
                    self.dbg_settings.follow_children = true;
                } else if self.dbg_settings.follow_children {
                    tprint!(self.panels.terminal(), "Disabled syscall tracing of children.");
                    self.dbg_settings.follow_children = false;
                }
            }
            Ok(Command::Help) => tprint!(self.panels.terminal(), "{}", commands::CMD_HELP),
            Err((err, _)) => {
                if err != CommandError::Missing("command") {
                    tprint!(self.panels.terminal(), "{err}");
                }
            }
        }

        true
    }
}
