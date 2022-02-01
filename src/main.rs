use std::borrow::Cow;
use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};

use rustc_demangle::try_demangle;

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

fn simplify_type<'a>(s: &'a str) -> Cow<'a, str> {
    Cow::Borrowed("")
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

    match try_demangle(&s[left + 1..=right - 1]) {
        Ok(demangled) => {
            let demangled = demangled.to_string();
            let mut dedemangled = s[..=left].to_string();

            let mut start = 0;
            for (idx, chr) in demangled.chars().enumerate() {
                if chr == '[' {
                    dedemangled.push_str(&demangled[start..idx]);
                }
                if chr == ']' {
                    start = idx + 1;
                }
            }

            if start != 0 {
                dedemangled.push_str(&demangled[start..]);
            }

            if args.simplify {}

            Cow::Owned(dedemangled + &s[right..])
        }
        Err(_error) => Cow::Borrowed(s),
    }
}

// TODO: impliment own version of `objdump`.
fn main() {
    let args = args::Cli::new();
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

        println!("{}", line);
    }
}
