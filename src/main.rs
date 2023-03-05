#![allow(clippy::unusual_byte_groupings)]
#![allow(clippy::needless_range_loop)]
#![cfg_attr(
    all(not(debug_assertions), target_family = "windows"),
    windows_subsystem = "windows"
)]

#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Bite can only be build for windows, macos and linux.");

mod args;
mod colors;
mod disassembler;
mod gui;
mod macros;
mod symbols;
mod threading;

use disassembler::InstructionStream;
use object::{Object, ObjectSection, SectionKind};
use once_cell::sync::Lazy;
use std::fs;

static ARGS: Lazy<args::Cli> = Lazy::new(args::Cli::parse);
static CONFIG: Lazy<symbols::Config> = Lazy::new(symbols::Config::from_env);

fn set_panic_handler() {
    #[cfg(not(debug_assertions))]
    std::panic::set_hook(Box::new(|details| {
        if let Some(msg) = details.payload().downcast_ref::<String>() {
            fail!("{msg}");
        }

        if let Some(msg) = details.payload().downcast_ref::<&str>() {
            fail!("{msg}");
        }

        fail!("Panic occurred.");
    }));
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    set_panic_handler();

    if ARGS.gui {
        return match gui::main().await {
            Err(err) => panic!("{err:?}"),
            Ok(..) => Ok(()),
        };
    }

    let binary = fs::read(ARGS.path.as_ref().unwrap()).expect("Unexpected read of binary failed.");

    let obj = object::File::parse(&*binary).expect("Not a valid object.");
    let index = symbols::Index::parse(&obj)
        .await
        .expect("Failed to parse symbols table.");

    let path = ARGS.path.as_ref().unwrap().display();

    if ARGS.libs {
        let imports = obj.imports().expect("Failed to resolve any symbols.");

        if imports.is_empty() {
            exit!("Object \"{path}\" doesn't seem to import anything.");
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
        if index.is_empty() {
            exit!("Object \"{path}\" doesn't seem to export any symbols.");
        }

        for symbol in index.symbols() {
            let symbol = symbol.tokens().iter().map(|s| s.text);
            let symbol = String::from_iter(symbol);

            println!("{symbol}");
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
        let stream = match InstructionStream::new(&raw, obj.architecture(), base_offset) {
            Err(err) => fail!("Failed to disassemble: {err:?}"),
            Ok(stream) => stream,
        };

        for instruction in stream {
            unchecked_println!("{}", instruction.to_string());
        }
    }

    Ok(())
}
