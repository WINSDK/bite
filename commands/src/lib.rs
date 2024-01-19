mod cli;
mod cfg;
mod debug;
mod gui;

pub use cli::Cli;
pub use gui::{Command, Error as CommandError, HELP as CMD_HELP};
use once_cell::sync::Lazy;

pub static ARGS: Lazy<cli::Cli> = Lazy::new(cli::Cli::parse);
pub static CONFIG: Lazy<cfg::Config> = Lazy::new(cfg::Config::parse);
