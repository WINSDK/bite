#![allow(clippy::needless_range_loop)]
#![cfg_attr(
    all(not(debug_assertions), target_family = "windows"),
    windows_subsystem = "windows"
)]

#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Bite can only be build for windows, macos and linux.");

mod args;
mod disassembly;
mod gui;
mod macros;
mod symbols;
mod threading;

use object::Object;
use once_cell::sync::Lazy;
use std::fs;

static ARGS: Lazy<args::Cli> = Lazy::new(args::Cli::parse);

fn set_panic_handler() {
    #[cfg(not(debug_assertions))]
    std::panic::set_hook(Box::new(|details| {
        if let Some(msg) = details.payload().downcast_ref::<String>() {
            exit!("{msg}");
        }

        if let Some(msg) = details.payload().downcast_ref::<&str>() {
            exit!("{msg}");
        }

        exit!("Panic occurred.");
    }));
}

fn main() {
    set_panic_handler();

    if ARGS.disassemble {
        return tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(gui::init())
            .unwrap();
    }

    let binary = fs::read(ARGS.path.as_ref().unwrap()).expect("Unexpected read of binary failed.");
    let obj = object::File::parse(&*binary).expect("Not a valid object.");
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
        let index = symbols::Index::parse(&obj).expect("Failed to parse symbols table.");

        if index.is_empty() {
            exit!("Object \"{path}\" doesn't seem to export any symbols.");
        }

        for symbol in index.symbols() {
            let symbol = symbol.tokens().iter().map(|s| &s.text[..]);
            let symbol = String::from_iter(symbol);

            println!("{symbol}");
        }
    }
}
