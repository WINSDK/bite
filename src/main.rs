use std::borrow::Cow;
use std::collections::BTreeMap;

use pdb::FallibleIterator;

use object::{Object, ObjectSection, ObjectSymbol, SectionKind};
use rayon::prelude::*;

mod args;
mod demangler;
mod disassembler;
mod replace;

#[macro_export]
macro_rules! exit {
    () => {
        std::process::exit(0)
    };

    (fail) => {
        std::process::exit(1)
    };

    (fail, $($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
    }};

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(0);
    }};
}

#[macro_export]
macro_rules! assert_exit {
    ($cond:expr $(,)?) => {{
        if !($cond) {
            $crate::exit!(fail);
        }
    }};

    ($cond:expr, $($arg:tt)+) => {{
        if !($cond) {
            $crate::exit!($($arg)*);
        }
    }};
}

macro_rules! unchecked_println {
    ($($arg:tt)*) => {{
        use std::io::Write;

        let mut stdout = std::io::stdout();
        match stdout.write_fmt(format_args!($($arg)*)) {
            Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => exit!(),
            _ => {}
        }

        match stdout.write(b"\n") {
            Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => exit!(),
            _ => {}
        }
    }};
}

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

    let args = args::Cli::parse();
    let config = replace::Config::from_env(&args);

    let binary = std::fs::read(&args.path).expect("unexpected read of binary failed").leak();
    let obj = object::File::parse(&*binary).expect("failed to parse binary");

    let mut symbols: BTreeMap<usize, Cow<'static, str>> = obj
        .symbols()
        .filter_map(|s| s.name().map(|name| (s.address() as usize, Cow::Borrowed(name))).ok())
        .filter(|(_, name)| !name.is_empty())
        .collect();

    if let Ok(Some(pdb)) = obj.pdb_info() {
        let read_symbols = |f: std::fs::File| {
            let mut symbols = Vec::new();
            let mut pdb = pdb::PDB::open(f)?;

            // get symbol table
            let symbol_table = pdb.global_symbols()?;

            // leak symbols onto the heap for later use
            let symbol_table = Box::leak(Box::new(symbol_table));

            // iterate through symbols collected earlier
            let mut symbol_table = symbol_table.iter();

            // retrieve addresses of symbols
            let address_map = pdb.address_map()?;

            while let Some(symbol) = symbol_table.next()? {
                let symbol = symbol.parse()?;

                let symbol = match symbol {
                    pdb::SymbolData::Public(symbol) if symbol.function => symbol,
                    _ => continue,
                };

                if let Some(addr) = symbol.offset.to_rva(&address_map) {
                    if let Ok(name) = std::str::from_utf8(symbol.name.as_bytes()) {
                        symbols.push((addr.0 as usize, Cow::Borrowed(name)));
                    }
                }
            }

            Ok(symbols)
        };

        symbols.extend(
            std::str::from_utf8(pdb.path())
                .map_err(|_| std::io::ErrorKind::InvalidData.into())
                .and_then(std::fs::File::open)
                .map_err(|_| pdb::Error::UnrecognizedFileFormat)
                .and_then(read_symbols)
                .unwrap_or_default(),
        );
    }

    if args.libs {
        println!("{}:", args.path.display());

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

    if args.names {
        if symbols.is_empty() {
            exit!(fail, "no symbols found: '{}'", args.path.display());
        }

        fn valid_symbol(symbol: &(&usize, &mut Cow<'static, str>)) -> bool {
            !symbol.1.starts_with("GCC_except_table")
                && !symbol.1.contains("cgu")
                && !symbol.1.is_empty()
        }

        symbols.par_iter_mut().filter(valid_symbol).for_each(|(_, symbol)| {
            match demangler::Symbol::parse_with_config(symbol, &config) {
                Ok(sym) => println!("{}", sym.display()),
                Err(..) => println!("{:#}", rustc_demangle::demangle(symbol)),
            }
        });
    }

    if !args.gui && args.disassemble {
        let section = obj
            .sections()
            .filter(|s| s.kind() == SectionKind::Text)
            .find(|t| t.name() == Ok(".text"))
            .expect("failed to find `.text` section");

        dbg!(section.address());
        dbg!(&symbols);

        if let Ok(raw) = section.uncompressed_data() {
            unchecked_println!("Disassembly of section {}:", section.name().unwrap_or("???"));

            let base = section.address() as usize;
            let stream = disassembler::InstructionStream::new(
                &raw,
                obj.architecture(),
                base,
            );

            for (off, instruction) in stream {
                if let Some(label) = symbols.get(&(base + off)) {
                    unchecked_println!("\n{off:012} <{label}>:");
                }

                unchecked_println!("\t{instruction}");
            }
        }
    }

    Ok(())
}
