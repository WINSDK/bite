use std::{
    borrow::Cow,
    io::{BufRead, BufReader, Write},
    path::PathBuf,
    process::{Command, Stdio},
};

use rustc_demangle::try_demangle;

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

fn demangle_line<'a>(s: &'a str) -> Cow<'a, str> {
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


            Cow::Owned(dedemangled + &s[right..])
        },
        Err(_error) => Cow::Borrowed(s),
    }
}

// TODO: impliment own version of `objdump`.
fn main() {
    let path = std::env::args()
        .skip(1)
        .next()
        .expect("Must enter path to executable.");
    let path = PathBuf::from(path);

    assert!(path.is_file(), "Path to executable doesn't exist");
    let objdump = Command::new("objdump")
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .stdin(Stdio::null())
        .arg("-x86-asm-syntax=intel")
        .arg("-D")
        .arg(path)
        .spawn()
        .unwrap();

    let mut stdout = BufReader::new(objdump.stdout.unwrap());
    for line in (&mut stdout).lines() {
        let line = match line {
            Ok(ref line) => {
                let line = demangle_line(line);
                
                line
            },
            Err(_) => Cow::Borrowed("???????????"),
        };

        println!("{}", line);
    }
}
