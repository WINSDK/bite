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
    // use tracing_subscriber::layer::SubscriberExt;
    // tracing::subscriber::set_global_default(
    //     tracing_subscriber::registry()
    //         .with(tracing_tracy::TracyLayer::new()),
    // ).expect("set up the subscriber");

    #[cfg(target_os = "linux")]
    if nix::unistd::getuid() == 0.into() {
        wayland::set_env();
    }

    if ARGS.disassemble {
        #[cfg(target_family = "windows")]
        let mut ui = gui::UI::<gui::windows::Arch>::new().unwrap();

        #[cfg(target_family = "unix")]
        let mut ui = gui::UI::<gui::unix::Arch>::new().unwrap();

        ui.process_args();
        ui.run();

        return;
    }

    let binary = fs::read(ARGS.path.as_ref().unwrap()).expect("Unexpected read of binary failed.");
    let obj = object::File::parse(&*binary).expect("Not a valid object.");
    let path = ARGS.path.as_ref().unwrap().display();

    if ARGS.libs {
        let mut index = symbols::Index::default();

        if let Err(err) = index.parse_imports(&obj) {
            eprintln!("{err}");
        }

        if index.is_empty() {
            eprintln!("Object \"{path}\" doesn't seem to import anything.");
            std::process::exit(0);
        }

        index.complete();
        println!("{path}:");

        for function in index.symbols() {
            let symbol = function.name().iter().map(|s| &s.text[..]);
            let symbol = String::from_iter(symbol);

            match function.module() {
                Some(module) => println!("\t{module} => {symbol}"),
                None => println!("\t{symbol}"),
            };
        }
    }

    if ARGS.names {
        let mut index = symbols::Index::default();

        if let Err(err) = index.parse_symbols(&obj) {
            eprintln!("{err}");
        }

        if let Err(err) = index.parse_pdb(&obj) {
            eprintln!("{err}");
        }

        index.complete();
        if index.is_empty() {
            eprintln!("Object \"{path}\" doesn't seem to export any symbols.");
            std::process::exit(0);
        }

        for function in index.symbols() {
            let symbol = function.name().iter().map(|s| &s.text[..]);
            let symbol = String::from_iter(symbol);

            println!("{symbol}");
        }
    }
}
