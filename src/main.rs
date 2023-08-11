#![cfg_attr(
    all(not(debug_assertions), target_family = "windows"),
    windows_subsystem = "windows"
)]

#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Bite can only be build for windows, macos and linux.");

pub mod args;
pub mod commands;
pub mod expr;
pub mod disassembly;
pub mod gui;
pub mod macros;
pub mod terminal;

use once_cell::sync::Lazy;
use std::fs;

static ARGS: Lazy<args::Cli> = Lazy::new(args::Cli::parse);

fn get_first_direntry<P: AsRef<std::path::Path>>(path: P) -> Option<std::ffi::OsString> {
    if let Ok(mut dir) = std::fs::read_dir(path) {
        if let Some(Ok(entry)) = dir.next() {
            return Some(entry.file_name());
        }
    }

    None
}

fn set_wayland_env() {
    // if the necessary wayland env variables aren't set, try to guess them
    if std::env::var("XDG_RUNTIME_DIR").is_err() || std::env::var("WAYLAND_DISPLAY").is_err() {
        let user = match get_first_direntry("/run/user") {
            Some(user) => user,
            None => {
                warning!("Failed to guess wayland environmental variables.");
                return;
            }
        };

        let mut path = std::path::PathBuf::from("/run/user/");
        path.push(user);
        path.push("wayland-1");

        std::env::set_var("WAYLAND_DISPLAY", path);
        std::env::set_var("XDG_RUNTIME_DIR", "/");
    }
}

fn main() {
    if unsafe { libc::getuid() } == 0 {
        set_wayland_env();
    }

    if ARGS.disassemble {
        return gui::init().unwrap();
    }

    let binary = fs::read(ARGS.path.as_ref().unwrap()).expect("Unexpected read of binary failed.");
    let obj = object::File::parse(&*binary).expect("Not a valid object.");
    let path = ARGS.path.as_ref().unwrap().display();

    if ARGS.libs {
        let mut index = symbols::Index::new();

        if let Err(err) = index.parse_imports(&binary, &obj) {
            exit!("Failed to parse import table ({err:?})");
        }

        if index.is_empty() {
            exit!("Object \"{path}\" doesn't seem to import anything.");
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
            exit!("Failed to parse symbol table ({err:?})");
        }

        if index.is_empty() {
            exit!("Object \"{path}\" doesn't seem to export any symbols.");
        }

        for function in index.symbols() {
            let symbol = function.name().iter().map(|s| &s.text[..]);
            let symbol = String::from_iter(symbol);

            println!("{symbol}");
        }
    }
}
