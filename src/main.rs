use std::borrow::Cow;
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

use rustc_demangle::demangle;

mod args;
mod replace;

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

// TODO: impliment own version of `objdump`.
fn main() {
    let args = args::Cli::parse();
    let objdump = Command::new("objdump")
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .stdin(Stdio::null())
        .arg("-x86-asm-syntax=intel")
        .arg("-D")
        .arg(&args.disassemble)
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
