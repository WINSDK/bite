use crate::tprint;
use commands::{Command, CommandError};

impl super::UI {
    /// Runs all queued commands, returning if they trigger a process exit.
    pub fn process_commands(&mut self, commands: &[String]) -> bool {
        commands.iter().all(|cmd| self.process_cmd(cmd))
    }

    /// Runs a singular commands, returning if it should exit the process.
    fn process_cmd(&mut self, cmd: &str) -> bool {
        let empty_index = debugvault::Index::default();
        let index = self.panels.processor().map(|proc| &proc.index).unwrap_or(&empty_index);

        match Command::parse(index, cmd, 0) {
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
            Ok(Command::Goto(addr)) => {
                let listing = match self.panels.listing() {
                    Some(listing) => listing,
                    None => {
                        tprint!(self.panels.terminal(), "No targets loaded.");
                        return true;
                    }
                };

                if listing.jump(addr) {
                    tprint!(self.panels.terminal(), "Jumped to address {addr:#X}.");
                } else {
                    tprint!(self.panels.terminal(), "Address {addr:#X} is undefined.");
                }

                self.panels.load_src(addr);
            }
            Ok(Command::Quit) => return false,
            Ok(Command::Clear) => {
                log::LOGGER.write().unwrap().clear();
                self.panels.terminal().clear();
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
