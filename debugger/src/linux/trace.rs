use crate::{Debugger, Tracee};

#[cfg(target_arch = "x86_64")]
pub type Sysno = syscalls::x86_64::Sysno;

#[cfg(target_arch = "x86")]
pub type Sysno = syscalls::x86::Sysno;

macro_rules! print_delimited {
    ($str:expr, $x:expr, $($xs:expr),+ $(,)?) => {{
        $str += $x;
        $str += ", ";
        print_delimited!($str, $($xs),+);
    }};

    ($str:expr, $x:expr) => {{
        $str += $x;
    }};
}

fn print_pointer(ptr: u64) -> String {
    if ptr == 0 {
        "NULL".to_string()
    } else {
        format!("{ptr:x}")
    }
}

fn print_fd(fd: u64) -> String {
    match fd {
        0 => "stdin".to_string(),
        1 => "stdout".to_string(),
        2 => "stderr".to_string(),
        _ => fd.to_string(),
    }
}

/// Try to read string bytes and print the first 40.
fn print_str(session: &mut Debugger, addr: u64, len: u64) -> String {
    match session.read_process_memory(addr as usize, len as usize) {
        Ok(data) => {
            let mut data = String::from_utf8_lossy(&data).into_owned();

            if data.len() > 40 {
                data.truncate(37);
                data += "...";
            }

            format!("\"{}\"", data.escape_default())
        }
        Err(..) => format!("\"???\""),
    }
}

impl super::Debugger {
    pub fn display(&mut self, syscall: Sysno, regs: nix::libc::user_regs_struct) -> String {
        let mut func = String::new();

        func += &syscall.to_string();
        func += "(";

        let args = [regs.rdi, regs.rsi, regs.rdx, regs.r10, regs.r8, regs.r9];

        match syscall {
            Sysno::brk => print_delimited![func, &print_pointer(args[0])],
            Sysno::close => print_delimited![func, &print_fd(args[0])],
            Sysno::read => print_delimited![
                func,
                &args[0].to_string(),
                &print_str(self, args[1], args[2]),
                &args[2].to_string()
            ],
            Sysno::write => print_delimited![
                func,
                &args[0].to_string(),
                &print_str(self, args[1], args[2]),
                &args[2].to_string()
            ],
            Sysno::munmap => {
                print_delimited![func, &format!("{:x}", args[0]), &args[1].to_string()]
            }
            _ => func += "..",
        }

        func += ") -> ";
        func += &format!("{:x}", regs.rax);
        func
    }
}
