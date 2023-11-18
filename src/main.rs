#![cfg_attr(
    all(not(debug_assertions), target_family = "windows"),
    windows_subsystem = "windows"
)]

#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Bite can only be build for windows, macos and linux.");

pub mod args;
pub mod expr;
pub mod wayland;
// pub mod commands;
// pub mod gui;

use once_cell::sync::Lazy;
use std::fs;

static ARGS: Lazy<args::Cli> = Lazy::new(args::Cli::parse);

fn main() {
    #[cfg(target_os = "linux")]
    if unsafe { libc::getuid() } == 0 {
        wayland::set_env();
    }

    if ARGS.disassemble {
        #[cfg(target_family = "windows")]
        let ui = gui::UI::<gui::windows::Arch>::new().unwrap();

        #[cfg(target_family = "unix")]
        let ui = gui::UI::<gui::unix::Arch>::new().unwrap();

        return ui.run();
    }

    let binary = fs::read(ARGS.path.as_ref().unwrap()).expect("Unexpected read of binary failed.");
    let obj = object::File::parse(&*binary).expect("Not a valid object.");
    let path = ARGS.path.as_ref().unwrap().display();

    if ARGS.libs {
        let mut index = symbols::Index::new();

        if let Err(err) = index.parse_imports(&binary, &obj) {
            log::error!("Failed to parse import table ({err:?})");
        }

        if index.is_empty() {
            log::error!("Object \"{path}\" doesn't seem to import anything.");
        }

        println!("{path}:");

        for function in index.symbols() {
            let symbol = function.name().iter().map(|s| &s.text[..]);
            let symbol = String::from_iter(symbol);

            match function.module() {
                Some(module) => println!("\t{} => {symbol}", &*module.text),
                None => println!("\t{symbol}"),
            };
        }
    }

    if ARGS.names {
        let mut index = symbols::Index::new();

        if let Err(err) = index.parse_debug(&obj) {
            log::error!("Failed to parse symbol table ({err:?})");
        }

        if index.is_empty() {
            log::error!("Object \"{path}\" doesn't seem to export any symbols.");
        }

        for function in index.symbols() {
            let symbol = function.name().iter().map(|s| &s.text[..]);
            let symbol = String::from_iter(symbol);

            println!("{symbol}");
        }
    }
}
