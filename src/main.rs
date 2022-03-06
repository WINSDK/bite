#![feature(bool_to_option)]

use std::borrow::Cow;
use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use goblin::{mach, Object};
use rustc_demangle::demangle;

mod args;
mod decode;
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

trait Crash<T> {
    fn panic_crash(self, msg: &str) -> T;
}

impl<T> Crash<T> for Option<T> {
    fn panic_crash(self, msg: &str) -> T {
        if let Some(unwrapped) = self {
            unwrapped
        } else {
            exit!("{}", msg)
        }
    }
}

impl<T, E> Crash<T> for Result<T, E> {
    fn panic_crash(self, msg: &str) -> T {
        if let Ok(unwrapped) = self {
            unwrapped
        } else {
            exit!("{}", msg)
        }
    }
}

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
    let demangled = if args.simplify { replace::simplify_type(&demangled) } else { demangled };

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
            let lib_name =
                lib.file_name().map(|v| v.to_str().unwrap()).unwrap_or("???? Invalid utf8");

            println!("\t{} => {}", lib_name, lib.display());
        }

        exit!();
    }

    if args.names {
        let symbols: Vec<&str> = object.symbols().filter_map(|sym| sym.map(|v| v.0).ok()).collect();
        let thread_count = std::thread::available_parallelism().unwrap_or_else(|err| {
            eprintln!("Failed to get thread_count: {err}");
            unsafe { std::num::NonZeroUsize::new_unchecked(1) }
        });

        let symbols_per_thread = (symbols.len() + (thread_count.get() - 1)) / thread_count;
        let finished_threads = Arc::new(AtomicUsize::new(0));

        let mut handles = Vec::with_capacity(thread_count.get());
        let (symbol_sender, demangled_symbols) = std::sync::mpsc::channel();

        for symbols_chunk in symbols.chunks(symbols_per_thread) {
            // FIXME: use thread::scoped when it becomes stable to replace this.
            // SAFETY: `symbols` is only dropped after the threads have joined therefore
            // it's safe to send to other threads as a &'static str.
            let symbols_chunk: &[&'static str] = unsafe {
                &*(symbols_chunk as *const [&str] as *const [*const str] as *const [&'static str])
            };

            // Clone versions to send to threads.
            let symbol_sender = symbol_sender.clone();
            let finished_threads = finished_threads.clone();

            handles.push(std::thread::spawn(move || {
                for symbol in symbols_chunk.iter().filter(|symbol| !symbol.is_empty()) {
                    let mut demangled_name = format!("{:#}", demangle(symbol));

                    if args.simplify {
                        demangled_name = replace::simplify_type(&demangled_name).to_string();
                    }

                    symbol_sender.send(demangled_name).unwrap();
                }

                finished_threads.fetch_add(1, Ordering::SeqCst);
            }))
        }

        while finished_threads.load(Ordering::SeqCst) != thread_count.get() {
            while let Ok(symbol) = demangled_symbols.try_recv() {
                println!("{symbol}");
            }
        }

        for handle in handles {
            let id = handle.thread().id();

            handle.join().unwrap_or_else(|e| {
                panic!("Failed to join thread with id: {:?}, error: {:?}", id, e)
            });
        }
    }

    if args.disassemble {
        let (_sec, _data) = object
            .segments
            .into_iter()
            .find(|seg| matches!(seg.name(), Ok("__TEXT")))
            .expect("Object is missing a `text` section")
            .sections()
            .expect("Failed to parse section")
            .into_iter()
            .find(|(sec, _)| matches!(sec.name(), Ok("__text")))
            .panic_crash("Object looks like it's been stripped");

        objdump(&args);
        todo!("{:?}", decode::x86_64::asm(decode::BitWidth::U64, &[0xf3, 0x48, 0xa5]));
    }

    Ok(())
}
