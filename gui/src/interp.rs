use crate::tprint;
use commands::Command;

fn print_cwd(terminal: &mut crate::widgets::Terminal) {
    match std::env::current_dir() {
        Ok(path) => tprint!(terminal, "Working directory {}.", path.display()),
        Err(err) => tprint!(terminal, "Failed to print pwd: '{err}'."),
    }
}


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

        let cmd = match Command::parse(index, cmd) {
            Ok(parsed) => parsed,
            Err(err) => {
                tprint!(self.panels.terminal(), "{err}");
                return true;
            }
        };
        
        match cmd {
            Command::Load(path) => self.offload_binary_processing(path),
            Command::PrintPath => print_cwd(self.panels.terminal()),
            Command::ChangeDir(path) => {
                if let Err(err) = std::env::set_current_dir(path) {
                    tprint!(self.panels.terminal(), "Failed to change directory: '{err}'.");
                    return true;
                }

                print_cwd(self.panels.terminal());
            },
            Command::Quit => return false,
            Command::Run(args) => self.offload_debugging(args),
            Command::Goto(addr) => {
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
            },
            Command::Break(addr) => {
                self.dbg_ctx.breakpoints.write().unwrap().create(addr);
                tprint!(self.panels.terminal(), "Breakpoint at {addr:#X} set.");
            },
            Command::DeleteBreak(addr) => {
                if self.dbg_ctx.breakpoints.write().unwrap().remove(addr).is_some() {
                    tprint!(self.panels.terminal(), "Breakpoint at {addr:#X} removed.");
                } else {
                    tprint!(self.panels.terminal(), "Breakpoint at {addr:#X} wasn't set.");
                }
            },
            Command::SetEnv(env) => self.dbg_settings.env.push(env),
            Command::Stop => {
                if !self.dbg_ctx.attached() {
                    tprint!(self.panels.terminal(), "There are no targets to stop.");
                    return true;
                }

                self.dbg_ctx.queue.push(debugger::DebugeeEvent::Break);
            },
            Command::Continue => {
                if !self.dbg_ctx.attached() {
                    tprint!(self.panels.terminal(), "There are no targets to continue.");
                    return true;
                }

                self.dbg_ctx.queue.push(debugger::DebugeeEvent::Continue);
            },
            Command::Clear => {
                log::LOGGER.lock().unwrap().clear();
                self.panels.terminal().clear();
            },
            Command::Trace => {},
            Command::FollowChildren => {},
        }

        true
    }
}
