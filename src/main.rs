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
        std::process::exit(0);
    };

    (fail) => {
        std::process::exit(1);
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

fn demangle_line<'a>(args: &args::Cli, s: &'a str, config: &replace::Config) -> Cow<'a, str> {
    let mut left = 0;
    for idx in 0..s.len() {
        if s.as_bytes()[idx] == b'<' {
            left = idx;
            break;
        }
    }

    let mut right = 0;
    for idx in 0..s.len() {
        if s[left..].as_bytes()[idx] == b'>' {
            right = left + idx;
            break;
        }
    }

    for idx in left..right {
        if s.as_bytes()[idx] == b'+' {
            right = idx;
            break;
        }
    }

    if left == 0 || right == 0 {
        return Cow::Borrowed(s);
    }

    let m = &s[left + 1..=right - 1];

    let demangled: Cow<str> = if m.starts_with("__Z") | m.starts_with("_Z") | m.starts_with('Z') {
        Cow::Owned(format!("{:#}", rustc_demangle::demangle(m)))
    } else {
        let demangle_attempt = if args.simplify {
            demangler::Symbol::parse_with_config(m, config)
        } else {
            demangler::Symbol::parse(m)
        };

        match demangle_attempt {
            Ok(demangled) => Cow::Owned(demangled.display()),
            Err(..) => return Cow::Borrowed(s),
        }
    };

    Cow::Owned(format!("{}{}{}", &s[..=left], demangled, &s[right..]))
}

fn set_panic_handler() {
    #[cfg(not(debug_assertions))]
    std::panic::set_hook(Box::new(|details| {
        if let Some(msg) = details.payload().downcast_ref::<String>() {
            return println!("{msg}");
        }

        if let Some(msg) = details.payload().downcast_ref::<&str>() {
            return println!("{msg}");
        }

        println!("panic occurred")
    }));
}

fn main() {
    set_panic_handler();

    let args = args::Cli::parse();
    let config = replace::Config::from_env(&args);

    let binary = std::fs::read(&args.path).expect("unexpected read of binary failed").leak();
    let obj = object::File::parse(&*binary).expect("failed to parse binary");

    let mut symbols: BTreeMap<usize, Cow<'static, str>> = obj
        .symbols()
        .filter_map(|s| s.name().map(|name| (s.address() as usize, Cow::Borrowed(name))).ok())
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
            match demangler::Symbol::parse(symbol) {
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

        if let Ok(raw) = section.uncompressed_data() {
            println!("Disassembly of section {}:", section.name().unwrap_or("???"));
            // println!("{:02x?}\n", raw);

            let stream = disassembler::InstructionStream::new(&raw, obj.architecture());

            for (off, instruction) in stream {
                if off != 0 {
                    if let Some(label) = symbols.get(&off) {
                        println!("\n{off:#018} <{label}>:")
                    }
                }

                println!("\t{instruction}");
            }
        }
    }
}
