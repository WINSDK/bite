#![allow(clippy::unusual_byte_groupings)]

#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Bite can only be build for windows, macos and linux.");

mod args;
mod disassembler;
mod macros;
mod symbols;
mod ui;

use object::{Object, ObjectSection, SectionKind};
use once_cell::sync::Lazy;

static ARGS: Lazy<args::Cli> = Lazy::new(args::Cli::parse);
static CONFIG: Lazy<symbols::Config> = Lazy::new(symbols::Config::from_env);

fn set_panic_handler() {
    #[cfg(not(debug_assertions))]
    std::panic::set_hook(Box::new(|details| {
        if let Some(msg) = details.payload().downcast_ref::<String>() {
            return unchecked_println!("{msg}");
        }

        if let Some(msg) = details.payload().downcast_ref::<&str>() {
            return unchecked_println!("{msg}");
        }

        unchecked_println!("Panic occurred.")
    }));
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    set_panic_handler();

    if ARGS.gui {
        tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2)
            .build()
            .expect("Failed to start tokio runtime.")
            .block_on(ui::main())?;

        return Ok(());
    }

    let binary = std::fs::read(ARGS.path.as_ref().unwrap())
        .expect("Unexpected read of binary failed.")
        .leak();

    let obj = object::File::parse(&*binary).expect("Failed to parse binary.");
    let symbols = symbols::table::parse(&obj).expect("Failed to parse symbols table.");
    let path = ARGS.path.as_ref().unwrap().display();

    if ARGS.libs {
        let imports = obj.imports().expect("Failed to resolve any symbols.");

        if imports.is_empty() {
            exit!("Object \"{path}\" doesn't import anything.");
        }

        println!("{path}:");

        for import in imports {
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

    if ARGS.names {
        if symbols.is_empty() {
            exit!(fail, "Object \"{path}\" doesn't export any symbols.");
        }

        for symbol in symbols.values() {
            let mut valid = true;

            valid &= !symbol.starts_with('_');
            valid &= !symbol.starts_with('.');
            valid &= !symbol.starts_with("GCC_except_table");
            valid &= !symbol.starts_with("anon.");
            valid &= !symbol.starts_with("str.");

            if valid {
                println!("{symbol}")
            }
        }
    }

    if ARGS.disassemble {
        let section = obj
            .sections()
            .filter(|s| s.kind() == SectionKind::Text)
            .find(|t| t.name() == Ok(".text"))
            .expect("Failed to find `.text` section.");

        let raw = section
            .uncompressed_data()
            .expect("Failed to decompress .text section.");

        unchecked_println!(
            "Disassembly of section {}:\n",
            section.name().unwrap_or("???")
        );

        let base_offset = section.address() as usize;
        let stream =
            disassembler::InstructionStream::new(&raw, obj.architecture(), base_offset, &symbols);

        for instruction in stream {
            unchecked_println!("{instruction}");
        }
    }

    Ok(())
}
