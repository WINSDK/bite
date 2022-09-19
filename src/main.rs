use std::borrow::Cow;
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use pdb::FallibleIterator;

use object::{Object, ObjectSection, ObjectSymbol, SectionKind};

mod args;
mod assembler;
mod demangler;
mod replace;

#[macro_export]
macro_rules! exit {
    () => {{
        std::process::exit(0);
    }};

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
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
                    if symbol_name.is_empty() {
                        continue;
                    }

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
                .unwrap_or_default()
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
        let thread_count = std::thread::available_parallelism().unwrap_or_else(|err| {
            eprintln!("Failed to get thread_count: {err}");
            unsafe { std::num::NonZeroUsize::new_unchecked(1) }
        });

        if symbols.is_empty() {
            println!("no symbols found: '{}'", args.path.display());
            return Ok(());
        }

        let symbols_per_thread = (symbols.len() + (thread_count.get() - 1)) / thread_count;

        std::thread::scope(|s| {
            let demangled = Arc::new(Mutex::new(Vec::with_capacity(symbols.len())));
            let threads_working = Arc::new(AtomicUsize::new(0));

            for symbols_chunk in symbols.chunks(symbols_per_thread) {
                let demangled = Arc::clone(&demangled);
                let threads_working = Arc::clone(&threads_working);

                // notify that a thread has started working
                threads_working.fetch_add(1, Ordering::AcqRel);

                s.spawn(move || {
                    for symbol in symbols_chunk {
                        if symbol.starts_with("GCC_except_table") || symbol.contains("cgu") {
                            continue;
                        }

                        let demangled_name = match demangler::Symbol::parse(symbol) {
                            Ok(sym) => sym.display(),
                            Err(..) => format!("{:#}", rustc_demangle::demangle(symbol)),
                        };

                        demangled.lock().unwrap().push(demangled_name)
                    }

                    // notify that a thread has finished working
                    threads_working.fetch_sub(1, Ordering::AcqRel);
                });
            }

            // wait till all threads are finished working
            //
            // NOTE: could technically be replaced by in-place sorting
            //       whilst values are being yielded amongst threads
            while threads_working.load(Ordering::Relaxed) != 0 {
                std::thread::yield_now();
            }

            let mut demangled = demangled.lock().unwrap();

            demangled.sort_unstable();
            demangled.dedup();

            let mut stdout = std::io::stdout();
            for name in demangled.iter_mut() {
                *name += "\n";
                stdout.write_all(name.as_bytes()).unwrap();
            }

            stdout.flush().unwrap();
        });
    }

    if args.disassemble {
        let raw = obj
            .sections()
            .find(|s| s.kind() == SectionKind::Text)
            .expect("Failed to find text section")
            .uncompressed_data();

        objdump(&args, &config);
    }

    Ok(())
}
