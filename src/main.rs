#![cfg_attr(
    all(not(debug_assertions), target_family = "windows"),
    windows_subsystem = "windows"
)]

#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Bite can only be build for windows, macos and linux.");

mod wayland;

use commands::ARGS;
use std::fs;

fn main() {
    #[cfg(target_os = "linux")]
    if nix::unistd::getuid() == 0.into() {
        wayland::set_env();
    }

    if ARGS.disassemble {
        let mut ui = gui::UI::new().unwrap();
        ui.process_args();
        ui.run();

        return;
    }

    let binary = fs::read(ARGS.path.as_ref().unwrap()).expect("Unexpected read of binary failed.");
    let obj = object::File::parse(&*binary).expect("Not a valid object.");
    let path = ARGS.path.as_ref().unwrap();
    let index = match debugvault::Index::parse(&obj, path) {
        Ok(index) => index,
        Err(err) => return eprintln!("{err}"),
    };
    let path = path.display();

    if ARGS.libs {
        if index.functions().next().is_none() {
            eprintln!("Object \"{path}\" doesn't seem to import anything.");
            std::process::exit(0);
        }

        println!("{path}:");

        for func in index.functions() {
            if !func.item.import() {
                continue;
            }

            let symbol = func.item.as_str().to_string();

            match func.item.module() {
                Some(module) => println!("\t{module} => {symbol}"),
                None => println!("\t{symbol}"),
            };
        }
    }

    if ARGS.names {
        if index.functions().next().is_none() {
            eprintln!("Object \"{path}\" doesn't seem to export any symbols.");
            std::process::exit(0);
        }

        for func in index.functions() {
            let symbol = func.item.as_str().to_string();
            println!("{symbol}");
        }
    }
}
