mod cli;
mod gui;
mod debug;

use once_cell::sync::Lazy;
pub use cli::Cli;
pub use gui::Command;
pub use gui::Error as CommandError;

pub static ARGS: Lazy<cli::Cli> = Lazy::new(cli::Cli::parse);
