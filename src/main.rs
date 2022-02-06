use std::borrow::Cow;
use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};

use goblin::{mach, Object};
use rustc_demangle::demangle;

mod args;
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

// struct Instruction<'a> {
//     // Position relative to the start of the executable.
//     position: usize,
//     // Bytes that make up an instruction, largest instruction can be 15 bytes.
//     bytes: ([u8; 15], usize),
//     // Interpretation of the bytes that make up the instruction.
//     interpretation: &'a str,
// }
//
// pub struct Instructions<'a> {
//     inner: Vec<Instruction<'a>>,
//     source: String,
// }

fn demangle_line<'a>(args: &args::Cli, s: &'a str) -> Cow<'a, str> {
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

    let demangled = Cow::Owned(format!("{:#}", demangle(&s[left + 1..=right - 1])));
    let demangled = if args.simplify {
        replace::simplify_type(&demangled)
    } else {
        demangled
    };

    Cow::Owned(s[..=left].to_string() + demangled.as_ref() + &s[right..])
}

fn objdump(args: &args::Cli) {
    let objdump = Command::new("objdump")
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .stdin(Stdio::null())
        .arg("-x86-asm-syntax=intel")
        .arg("-D")
        .arg(&args.path)
        .spawn()
        .unwrap();

    let mut stdout = BufReader::new(objdump.stdout.unwrap());
    for line in (&mut stdout).lines() {
        let line = match line {
            Ok(ref line) => demangle_line(&args, line),
            Err(_) => Cow::Borrowed("???????????"),
        };

        println!("{line}");
    }
}

// TODO: impliment own version of `objdump`.
fn main() -> goblin::error::Result<()> {
    let args = args::Cli::parse();

    let object_bytes = std::fs::read(&args.path).unwrap();
    let object = goblin::Object::parse(object_bytes.as_slice()).unwrap();
    let object = match object {
        Object::Mach(mac) => match mac {
            mach::Mach::Fat(fat) => fat.get(0)?,
            mach::Mach::Binary(bin) => bin,
        },
        _ => todo!(),
    };

    if args.libs {
        println!("{}:", args.path.display());
        for lib in object.libs.iter().skip(1) {
            let lib = std::path::Path::new(lib);
            let lib_name = lib
                .file_name()
                .map(|v| v.to_str().unwrap())
                .unwrap_or("???? Invalid utf8");

            println!("\t{} => {}", lib_name, lib.display());
        }

        exit!();
    }

    if args.names {
        let symbols: Vec<&str> = object
            .symbols()
            .filter_map(|symbol| symbol.map(|v| v.0).ok())
            .collect();

        let thread_count = num_cpus::get();
        let symbols_per_thread = (symbols.len() + (thread_count - 1)) / thread_count;
        let mut handles = Vec::with_capacity(thread_count);

        for symbols_chunk in symbols.chunks(symbols_per_thread) {
            // FIXME: use thread::scoped when it becomes stable to replace this.
            let symbols_chunk: &[&'static str] = unsafe {
                &*(symbols_chunk as *const [&str] as *const [*const str] as *const [&'static str])
            };

            handles.push(std::thread::spawn(move || {
                let mut demangled_names = Vec::with_capacity(symbols_chunk.len());

                for symbol in symbols_chunk {
                    let mut demangled_name = format!("{:#}", demangle(symbol));

                    if args.simplify {
                        demangled_name = replace::simplify_type(&demangled_name).to_string();
                    }

                    if !demangled_name.is_empty() {
                        demangled_names.push(demangled_name);
                    }
                }

                demangled_names
            }))
        }

        for demangled_symbol in handles.into_iter().map(|t| t.join().unwrap()).flatten() {
            println!("{demangled_symbol}");
        }
    }

    if args.disassemble {
        objdump(&args);
    }

    Ok(())
}
