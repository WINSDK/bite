#![allow(clippy::unusual_byte_groupings)]
#![allow(clippy::needless_range_loop)]
#![cfg_attr(all(not(debug_assertions), target_family = "windows"), windows_subsystem = "windows")]

#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Bite can only be build for windows, macos and linux.");

mod args;
mod colors;
mod disassembler;
mod gui;
mod macros;
mod symbols;

use disassembler::InstructionStream;
use object::{Object, ObjectSection, SectionKind};
use once_cell::sync::Lazy;
use std::fs;

static ARGS: Lazy<args::Cli> = Lazy::new(args::Cli::parse);
static CONFIG: Lazy<symbols::Config> = Lazy::new(symbols::Config::from_env);

fn set_panic_handler() {
    #[cfg(not(debug_assertions))]
    std::panic::set_hook(Box::new(|details| {
        let mut panic_msg = String::new();

        if let Some(msg) = details.payload().downcast_ref::<String>() {
            panic_msg = msg.to_string();
        }

        if let Some(msg) = details.payload().downcast_ref::<&str>() {
            panic_msg = msg.to_string();
        }

        if panic_msg.is_empty() {
            panic_msg = "Panic occurred.".to_string();
        }

        #[cfg(target_family = "windows")]
        unsafe {
            use winit::platform::windows::HWND;

            extern "system" {
                fn MessageBoxA(handle: HWND, text: *const i8, title: *const i8, flags: i32) -> i32;
            }

            let title = std::ffi::CString::new("Bite failed at runtime").unwrap();
            let msg = std::ffi::CString::new(panic_msg.as_str()).unwrap();

            MessageBoxA(0, msg.as_ptr(), title.as_ptr(), 0);
        }

        unchecked_println!("{panic_msg}");
    }));
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    set_panic_handler();

    if ARGS.gui {
        let result = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2)
            .build()
            .expect("Failed to start tokio runtime.")
            .block_on(gui::main());

        if let Err(err) = result {
            panic!("{err:?}");
        }

        return Ok(());
    }

    let binary = fs::read(ARGS.path.as_ref().unwrap()).expect("Unexpected read of binary failed.");

    let obj = object::File::parse(&*binary).expect("Not a valid object.");
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
        let stream = match InstructionStream::new(&raw, obj.architecture(), base_offset, &symbols) {
            Err(err) => exit!("Failed to disassemble: {err:?}"),
            Ok(stream) => stream,
        };

        for instruction in stream {
            unchecked_println!("{}", instruction.to_string());
        }
    }

    Ok(())
}
