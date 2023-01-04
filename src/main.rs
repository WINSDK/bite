use std::borrow::Cow;
use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};

use pdb::FallibleIterator;

use object::{Object, ObjectSection, ObjectSymbol, SectionKind};
use rayon::prelude::*;

mod args;
mod assembler;
mod demangler;
mod replace;

#[macro_export]
macro_rules! exit {
    () => {
        std::process::exit(0);
    };

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(0);
    }};
}

#[macro_export]
macro_rules! assert_exit {
    ($cond:expr $(,)?) => {{
        if !($cond) {
            $crate::exit!();
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

fn objdump(args: &args::Cli, config: &replace::Config) {
    let syntax = "-Mintel";

    #[cfg(target_os = "macos")]
    let syntax = "-x86-asm-syntax=intel";

    let objdump = Command::new("objdump")
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .stdin(Stdio::null())
        .arg(syntax)
        .arg("-D")
        .arg(&args.path)
        .spawn()
        .unwrap();

    let mut stdout = BufReader::new(objdump.stdout.unwrap());
    for line in (&mut stdout).lines() {
        let line = match line {
            Ok(ref line) => demangle_line(args, line, config),
            Err(_) => Cow::Borrowed("???????????"),
        };

        println!("{line}");
    }
}

// TODO: impliment own version of `objdump`.
fn main() -> object::Result<()> {
    let args = args::Cli::parse();
    let config = replace::Config::from_env(&args);

    let binary = std::fs::read(&args.path).unwrap();
    let obj = object::File::parse(&*binary)?;
    let mut symbols: Vec<&str> = obj.symbols().filter_map(|s| s.name().ok()).collect();

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

            while let Some(symbol) = symbol_table.next()? {
                let symbol = symbol.parse()?;

                let symbol_name = match symbol {
                    pdb::SymbolData::Public(s) => std::str::from_utf8(s.name.as_bytes()),
                    pdb::SymbolData::Export(s) => std::str::from_utf8(s.name.as_bytes()),
                    _ => Ok(""),
                };

                if let Ok(symbol_name) = symbol_name {
                    symbols.push(symbol_name);
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

        for import in obj.imports()? {
            let library = match std::str::from_utf8(import.library()) {
                Ok(library) => library,
                Err(_) => continue,
            };

            match std::str::from_utf8(import.name()) {
                Ok(name) => println!("\t{library} => {name}"),
                Err(_) => println!("\t{library}"),
            };
        }

        exit!();
    }

    if args.names {
        if symbols.is_empty() {
            exit!("no symbols found: '{}'", args.path.display());
        }

        let demangled_symbols: std::collections::BTreeSet<String> = symbols
            .par_iter()
            .filter(|symbol| !symbol.starts_with("GCC_except_table") && !symbol.contains("cgu"))
            .map(|symbol| match demangler::Symbol::parse(symbol) {
                Ok(sym) => sym.display() + "\n",
                Err(..) => format!("{:#}\n", rustc_demangle::demangle(symbol)),
            })
            .collect();

        println!("{}", String::from_iter(demangled_symbols.into_iter()));
    }

    if args.disassemble {
        let text_sections = obj.sections().filter(|s| s.kind() == SectionKind::Text);

        for text in text_sections {
            if text.name() != Ok(".init") {
                continue;
            }

            if let Ok(raw) = text.uncompressed_data() {
                use object::Architecture::*;

                println!("Disassembly of section {}:\n", text.name().unwrap_or("???"));
                println!("{:x?}", raw);
                println!();

                let Some(size) = obj.architecture().address_size() else {
                    exit!("unknown target architecture");
                };

                let instruction = match obj.architecture() {
                    Aarch64 | Arm => assembler::arm::asm().to_string(),
                    X86_64 | X86_64_X32 => assembler::x86_64::asm(size, &raw).unwrap().to_string(),
                    Mips | Mips64 => assembler::mips::asm(&raw).unwrap().to_string(),
                    Riscv32 | Riscv64 => assembler::riscv::asm(&raw).to_string(),
                    _ => todo!(),
                };

                println!("{instruction}");
            }
        }

        // objdump(&args, &config);
    }

    Ok(())
}
