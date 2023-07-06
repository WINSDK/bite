use std::ffi::CString;

use crate::{Debugger, Tracee};

#[cfg(target_arch = "x86_64")]
pub type Sysno = syscalls::x86_64::Sysno;

#[cfg(target_arch = "x86")]
pub type Sysno = syscalls::x86::Sysno;

macro_rules! print_delimited {
    [$str:expr, $x:expr, $($xs:expr),+] => {{
        $str += &$x;
        $str += ", ";
        print_delimited![$str, $($xs),+];
    }};

    [$str:expr, $x:expr] => {{
        $str += &$x;
    }};
}

fn format_ptr(ptr: u64) -> String {
    if ptr == 0 {
        "NULL".to_string()
    } else {
        format!("{ptr:x}")
    }
}

fn format_fd(fd: u64) -> String {
    match fd as i32 {
        0 => "stdin".to_string(),
        1 => "stdout".to_string(),
        2 => "stderr".to_string(),
        n => n.to_string(),
    }
}

// Try to read some bytes.
fn format_bytes(session: &mut Debugger, addr: u64, len: u64) -> String {
    match session.read_process_memory(addr as usize, len as usize) {
        Ok(data) => {
            let mut data = format!("{data:x?}");

            if data.len() >= 60 {
                data.truncate(56);
                data.pop();
                data += ", ..]";
            }

            data
        }
        Err(..) => format!("\"???\""),
    }
}

/// Try to read a string with a known length and print the first 60 characters.
fn format_str(session: &mut Debugger, addr: u64, len: u64) -> String {
    match session.read_process_memory(addr as usize, len as usize) {
        Ok(data) => {
            let data = String::from_utf8_lossy(&data).into_owned();
            let mut data = data.escape_default().to_string();

            if data.len() >= 60 {
                data.truncate(57);
                data += "...";
            }

            format!("\"{data}\"")
        }
        Err(..) => format!("\"???\""),
    }
}

/// Try to read a string with a null terminator and print the first 60 ascii characters.
fn format_c_str(session: &mut Debugger, addr: u64) -> String {
    match session.read_process_memory(addr as usize, 61) {
        Ok(mut data) => {
            // add a null terminator if one wasn't found in the first 40 bytes
            match data.iter().position(|&b| b == b'\0') {
                Some(terminator) => data.truncate(terminator + 1),
                None => {
                    data.pop();
                    data.push(b'\0')
                }
            }

            let data = match CString::from_vec_with_nul(data.clone()) {
                Ok(data) => data.to_string_lossy().into_owned(),
                Err(err) => {
                    println!("access {data:?}");
                    return format!("\"{err}\"");
                }
            };

            let mut data = data.escape_default().to_string();
            if data.len() >= 60 {
                data.truncate(57);
                data += "...";
            }

            format!("\"{data}\"")
        }
        Err(..) => format!("\"???\""),
    }
}

macro_rules! print_flags {
    ($flags:expr => $ty:ty) => {
        match <$ty>::from_bits($flags as _) {
            Some(flag) => format!("{flag:#?}"),
            None => format!("???"),
        }
    };
}

impl super::Debugger {
    pub fn display(&mut self, syscall: Sysno, regs: nix::libc::user_regs_struct) -> String {
        let mut func = String::new();

        func += &syscall.to_string();
        func += "(";

        let args = [regs.rdi, regs.rsi, regs.rdx, regs.r10, regs.r8, regs.r9];

        match syscall {
            Sysno::brk => print_delimited![func, format_ptr(args[0])],
            Sysno::close => print_delimited![func, format_fd(args[0])],
            Sysno::openat => print_delimited![
                func,
                if args[0] == 4294967196 {
                    "AT_FDCWD".to_string()
                } else {
                    format_fd(args[0])
                },
                format_c_str(self, args[1]),
                print_flags!(args[2] => nix::fcntl::OFlag)
            ],
            Sysno::access => print_delimited![
                func,
                format_c_str(self, args[0]),
                print_flags!(args[1] => nix::unistd::AccessFlags)
            ],
            Sysno::mmap => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                print_flags!(args[2] => nix::sys::mman::ProtFlags),
                print_flags!(args[3] => nix::sys::mman::MapFlags),
                format_fd(args[4]),
                args[5].to_string()
            ],
            Sysno::mprotect => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                print_flags!(args[2] => nix::sys::mman::ProtFlags)
            ],
            Sysno::pread64 => print_delimited![
                func,
                args[0].to_string(),
                format_str(self, args[1], args[2]),
                args[2].to_string(),
                (args[3] as i64).to_string()
            ],
            Sysno::read => print_delimited![
                func,
                format_fd(args[0]),
                format_str(self, args[1], args[2]),
                args[2].to_string()
            ],
            Sysno::write => print_delimited![
                func,
                format_fd(args[0]),
                format_str(self, args[1], args[2]),
                args[2].to_string()
            ],
            Sysno::munmap => {
                print_delimited![func, format!("{:x}", args[0]), &args[1].to_string()]
            }
            Sysno::set_tid_address => print_delimited![func, format_ptr(args[0])],
            Sysno::set_robust_list => {
                print_delimited![func, format_ptr(args[0]), args[1].to_string()]
            }
            Sysno::getrandom => print_delimited![
                func,
                format_bytes(self, args[0], args[1]),
                args[1].to_string(),
                {
                    let has_random =
                        args[2] as u32 & nix::libc::GRND_RANDOM == nix::libc::GRND_RANDOM;
                    let has_nonblock =
                        args[2] as u32 & nix::libc::GRND_NONBLOCK == nix::libc::GRND_NONBLOCK;

                    match (has_random, has_nonblock) {
                        (true, true) => "GRND_NONBLOCK | GRND_RANDOM".to_string(),
                        (true, false) => "GRND_RANDOM".to_string(),
                        (false, true) => "GRND_NONBLOCK".to_string(),
                        _ => "NO_FLAGS".to_string(),
                    }
                }
            ],
            Sysno::newfstatat => print_delimited![
                func,
                format_fd(args[0]),
                format_c_str(self, args[1]),
                "(stat *)".to_string() + &format_ptr(args[2]),
                print_flags!(args[3] => nix::fcntl::AtFlags)
            ],
            _ => func += "..",
        }

        func += ") -> ";
        func += &format!("{:x}", regs.rax);
        func
    }
}
