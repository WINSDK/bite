use std::borrow::Cow;
use std::collections::BTreeMap;

use pdb::FallibleIterator;

use object::{Object, ObjectSection, ObjectSymbol, SectionKind};

mod args;
mod demangler;
mod disassembler;
mod replace;
mod macros;

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

        symbols.iter_mut().filter(valid_symbol).for_each(|(_, symbol)| {
            match demangler::Symbol::parse_with_config(symbol, &config) {
                Ok(sym) => println!("{}", sym.display()),
                Err(..) => println!("{:#}", rustc_demangle::demangle(symbol)),
            }
        });
    }

    if args.disassemble {
        let section = obj
            .sections()
            .filter(|s| s.kind() == SectionKind::Text)
            .find(|t| t.name() == Ok(".text"))
            .expect("failed to find `.text` section");

        let raw = section.uncompressed_data().expect("failed to decompress .text section");

        if !args.gui {
            unchecked_println!("Disassembly of section {}:", section.name().unwrap_or("???"));

            let base = section.address() as usize;
            let stream = disassembler::InstructionStream::new(
                &raw,
                obj.architecture(),
                base,
                symbols
            );

            for instruction in stream {
                unchecked_println!("\t{instruction}");
            }
        }

        if args.gui {
            panic!("no gui :(");
        }
    }

    Ok(())
}
