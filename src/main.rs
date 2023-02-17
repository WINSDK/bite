#![allow(clippy::unusual_byte_groupings)]

mod args;
mod disassembler;
mod macros;
mod symbols;
mod ui;

use object::{Object, ObjectSection, SectionKind};
use once_cell::sync::Lazy;

static ARGS: Lazy<args::Cli> = Lazy::new(args::Cli::parse);
static CONFIG: Lazy<symbols::Config> = Lazy::new(|| symbols::Config::from_env(&ARGS));

fn set_panic_handler() {
    #[cfg(not(debug_assertions))]
    std::panic::set_hook(Box::new(|details| {
        if let Some(msg) = details.payload().downcast_ref::<String>() {
            return unchecked_println!("{msg}");
        }

        if let Some(msg) = details.payload().downcast_ref::<&str>() {
            return unchecked_println!("{msg}");
        }

        unchecked_println!("panic occurred")
    }));
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    set_panic_handler();

    let binary = std::fs::read(&ARGS.path)
        .expect("unexpected read of binary failed")
        .leak();

    let obj = object::File::parse(&*binary).expect("failed to parse binary");
    let symbols = symbols::table::parse(&obj).expect("failed to parse symbols table");

    if ARGS.libs {
        println!("{}:", ARGS.path.display());

        for import in obj.imports().expect("failed to resolve any symbols") {
            let library = match std::str::from_utf8(import.library()) {
                Ok(library) => library,
                Err(_) => continue,
            };

            match std::str::from_utf8(import.name()) {
                Ok(name) => println!("\t{library} => {name}"),
                Err(_) => println!("\t{library}"),
            };
        }
    }

    if ARGS.disassemble {
        if !ARGS.gui {
            let section = obj
                .sections()
                .filter(|s| s.kind() == SectionKind::Text)
                .find(|t| t.name() == Ok(".text"))
                .expect("failed to find `.text` section");

            let raw = section
                .uncompressed_data()
                .expect("failed to decompress .text section");

            unchecked_println!(
                "Disassembly of section {}:\n",
                section.name().unwrap_or("???")
            );

            let base_offset = section.address() as usize;
            let stream = disassembler::InstructionStream::new(
                &raw,
                obj.architecture(),
                base_offset,
                &symbols,
            );

            for instruction in stream {
                unchecked_println!("{instruction}");
            }
        }

        if ARGS.gui {
            tokio::runtime::Builder::new_multi_thread()
                .worker_threads(2)
                .build()
                .expect("failed to start tokio runtime")
                .block_on(ui::main())?;
        }
    }

    Ok(())
}
