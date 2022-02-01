#[derive(clap::Parser)]
#[clap(name = "rust object file dumper")]
pub struct Cli {
    /// Strips symbols into a simpler format
    #[clap(short, long)]
    pub simplify: bool,

    /// Path to the executable you want to disassembly
    #[clap(short)]
    pub disassemble: std::path::PathBuf,
}

impl Cli {
    pub fn new() -> Self {
        use clap::Parser;

        let args = Self::parse();
        assert!(
            args.disassemble.is_file(),
            "Path to executable doesn't exist"
        );

        args
    }
}
