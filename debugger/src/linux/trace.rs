use crate::{Debugger, Tracee};
use nix::libc;
use nix::sys::{signal, stat};

use std::ffi::{c_int, CString};
use std::fmt;
use std::mem::size_of;
use std::os::fd::{AsRawFd, AsFd};
use std::ptr;

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

    [] => {{}};
}

macro_rules! format_flags {
    ($flags:expr => $ty:ty) => {
        match <$ty>::from_bits($flags as _) {
            Some(flag) => format!("{flag:?}"),
            None => format!("(unknown)"),
        }
    };
}

#[repr(transparent)]
struct PollFd(nix::poll::PollFd<'static>);

impl fmt::Debug for PollFd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fd = format_fd(self.0.as_fd().as_raw_fd() as u64);

        f.write_fmt(format_args!("{{fd: {fd}, ",))?;
        f.write_fmt(format_args!("events: {:?}}}", self.0.events()))
    }
}

#[repr(transparent)]
struct IoVec(libc::iovec);

impl fmt::Debug for IoVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let base = format_ptr(self.0.iov_base as u64);

        f.write_fmt(format_args!("{{base: {base}, ",))?;
        f.write_fmt(format_args!("len: {:?}}}", self.0.iov_len))
    }
}

#[repr(transparent)]
struct Fd(c_int);

impl fmt::Debug for Fd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format_fd(self.0 as u64))
    }
}

fn format_ptr(addr: u64) -> String {
    if addr == 0 {
        "NULL".to_string()
    } else {
        format!("{addr:x}")
    }
}

fn format_fd(fd: u64) -> String {
    match fd as c_int {
        0 => "stdin".to_string(),
        1 => "stdout".to_string(),
        2 => "stderr".to_string(),
        n => n.to_string(),
    }
}

fn format_fdset(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    match session.read_process_memory(addr as usize, size_of::<nix::sys::select::FdSet>()) {
        Ok(data) => {
            let set = unsafe { ptr::read(data.as_ptr() as *const nix::sys::select::FdSet) };
            let set: Vec<_> = set.fds(None).map(|fd| Fd(fd.as_raw_fd())).collect();

            format!("{set:?}")
        }
        Err(..) => format!("\"???\""),
    }
}

// Try to read 20 bytes.
fn format_bytes_u8(session: &mut Debugger, addr: u64, len: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    let bytes_to_read = std::cmp::min(len as usize, 20);
    match session.read_process_memory(addr as usize, bytes_to_read) {
        Ok(data) => {
            let mut data = format!("{data:x?}");

            if len > 60 {
                data.pop();
                data += ", ..]";
            }

            data
        }
        Err(..) => format!("\"???\""),
    }
}

/// Read first 20 elements of an array of type `T`.
fn format_array<T: std::fmt::Debug>(session: &mut Debugger, addr: u64, len: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    let width = size_of::<T>();
    let end_of_array = addr + len * width as u64;
    let mut items = Vec::new();
    for addr in (addr..end_of_array).step_by(width).take(20) {
        if let Ok(data) = session.read_process_memory(addr as usize, width) {
            let item = unsafe { ptr::read(data.as_ptr() as *const T) };
            items.push(item);
        }
    }

    format!("{items:?}")
}

/// Try to read a string with a known length and print the first 60 characters.
fn format_str(session: &mut Debugger, addr: u64, len: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    let bytes_to_read = std::cmp::min(len as usize, 60 * size_of::<char>());
    match session.read_process_memory(addr as usize, bytes_to_read) {
        Ok(data) => {
            let data = String::from_utf8_lossy(&data).into_owned();
            let mut data = data.escape_default().to_string();

            if data.len() > 60 {
                data.truncate(57);
                data += "...";
            }

            format!("\"{data}\"")
        }
        Err(..) => format!("\"???\""),
    }
}

/// Try to read a string with a null terminator and print the first 60 characters.
fn format_c_str(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    let bytes_to_read = 60 * size_of::<char>();
    match session.read_process_memory(addr as usize, bytes_to_read) {
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
            if data.len() > 60 {
                data.truncate(57);
                data += "...";
            }

            format!("\"{data}\"")
        }
        Err(..) => format!("???"),
    }
}

// FIXME: i don't think this is being interpreted correctly
fn format_sigset(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    match session.read_process_memory(addr as usize, size_of::<signal::SigSet>()) {
        Ok(data) => {
            let set = unsafe { ptr::read(data.as_ptr() as *const signal::SigSet) };

            if set == signal::SigSet::all() {
                return format!("~[]");
            }

            let set: Vec<signal::Signal> = set.iter().collect();

            // if all 31 signals are set, it must be an empty set mask
            match set.len() {
                31 => format!("~[]"),
                _ => format!("{set:?}"),
            }
        }
        Err(..) => format!("???"),
    }
}

fn format_sigaction(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    match session.read_process_memory(addr as usize, size_of::<signal::SigAction>()) {
        Ok(data) => {
            let action = unsafe { ptr::read(data.as_ptr() as *const signal::SigAction) };
            let handler = action.handler();
            let flags = action.flags();
            let mask = action.mask();
            let mask: Vec<signal::Signal> = mask.iter().collect();

            format!("{{sa_handler: {handler:?}, sa_mask: {mask:?}, sa_flags={flags:?}}}")
        }
        Err(..) => format!("???"),
    }
}

fn format_futex_op(op: u64) -> &'static str {
    let op = op as c_int;

    if op & libc::FUTEX_PRIVATE_FLAG == libc::FUTEX_PRIVATE_FLAG {
        match op - libc::FUTEX_PRIVATE_FLAG {
            libc::FUTEX_WAIT => "FUTEX_WAIT_PRIVATE",
            libc::FUTEX_WAKE => "FUTEX_WAKE_PRIVATE",
            libc::FUTEX_FD => "FUTEX_FD_PRIVATE",
            libc::FUTEX_REQUEUE => "FUTEX_REQUEUE_PRIVATE",
            libc::FUTEX_CMP_REQUEUE => "FUTEX_CMP_REQUEUE_PRIVATE",
            _ => "(unknown)",
        }
    } else {
        match op {
            libc::FUTEX_WAIT => "FUTEX_WAIT",
            libc::FUTEX_WAKE => "FUTEX_WAKE",
            libc::FUTEX_FD => "FUTEX_FD",
            libc::FUTEX_REQUEUE => "FUTEX_REQUEUE",
            libc::FUTEX_CMP_REQUEUE => "FUTEX_CMP_REQUEUE",
            _ => "(unknown)",
        }
    }
}

fn format_stat(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    match session.read_process_memory(addr as usize, size_of::<stat::FileStat>()) {
        Ok(data) => {
            let stats = unsafe { ptr::read(data.as_ptr() as *const stat::FileStat) };
            let mode = stats.st_mode;
            let size = stats.st_size;

            format!("{{st_mode={mode:?}, st_size={size}, ...}}")
        }
        Err(..) => format!("???"),
    }
}

impl super::Debugger {
    pub fn display(&mut self, syscall: Sysno, args: [u64; 6]) -> String {
        let mut func = String::new();

        func += &syscall.to_string();
        func += "(";

        match syscall {
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
            Sysno::open => print_delimited![
                func,
                format_c_str(self, args[0]),
                format_flags!(args[1] => nix::fcntl::OFlag)
            ],
            Sysno::close => print_delimited![func, format_fd(args[0])],
            Sysno::stat => print_delimited![func, format_c_str(self, args[0]), format_ptr(args[1])],
            Sysno::fstat => print_delimited![func, format_fd(args[0]), format_ptr(args[1])],
            Sysno::lstat => {
                print_delimited![func, format_c_str(self, args[0]), format_ptr(args[1])]
            }
            Sysno::poll => print_delimited![
                func,
                format_array::<PollFd>(self, args[0], args[1]),
                (args[1] as c_int).to_string(),
                (args[2] as c_int).to_string()
            ],
            Sysno::lseek => print_delimited![
                func,
                format_fd(args[0]),
                (args[1] as i64).to_string(),
                match args[2] {
                    0 => "SEEK_SET",
                    1 => "SEEK_CUR",
                    2 => "SEEK_END",
                    3 => "SEEK_DATA",
                    4 => "SEEK_HOLE",
                    _ => "(unknown)",
                }
            ],
            Sysno::mmap => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                format_flags!(args[2] => nix::sys::mman::ProtFlags),
                format_flags!(args[3] => nix::sys::mman::MapFlags),
                format_fd(args[4]),
                args[5].to_string()
            ],
            Sysno::mprotect => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                format_flags!(args[2] => nix::sys::mman::ProtFlags)
            ],
            Sysno::munmap => {
                print_delimited![func, format!("{:x}", args[0]), &args[1].to_string()]
            }
            Sysno::brk => print_delimited![func, format_ptr(args[0])],
            Sysno::rt_sigaction => print_delimited![
                func,
                signal::Signal::try_from(args[0] as c_int)
                    .map(|s| s.as_str())
                    .unwrap_or("(unknown)"),
                format_sigaction(self, args[1]),
                format_sigaction(self, args[2])
            ],
            Sysno::rt_sigprocmask => print_delimited![
                func,
                match args[0] {
                    0 => "SIG_BLOCK",
                    1 => "SIG_UNBLOCK",
                    2 => "SIG_SETMASK",
                    _ => "(unknown)",
                },
                format_sigset(self, args[1]),
                format_sigset(self, args[2])
            ],
            Sysno::rt_sigreturn => print_delimited![],
            Sysno::ioctl => print_delimited![
                func,
                args[0].to_string(),
                args[1].to_string(),
                // TODO: print all the ioctl call kinds
                format_ptr(args[2])
            ],
            Sysno::pread64 => print_delimited![
                func,
                format_fd(args[0]),
                format_str(self, args[1], args[2]),
                args[2].to_string(),
                (args[3] as i64).to_string()
            ],
            Sysno::pwrite64 => print_delimited![
                func,
                format_fd(args[0]),
                format_str(self, args[1], args[2]),
                args[2].to_string(),
                (args[3] as i64).to_string()
            ],
            Sysno::readv => print_delimited![
                func,
                format_fd(args[0]),
                format_array::<IoVec>(self, args[1], args[2]),
                args[2].to_string()
            ],
            Sysno::writev => print_delimited![
                func,
                format_fd(args[0]),
                format_array::<IoVec>(self, args[1], args[2]),
                args[2].to_string()
            ],
            Sysno::access => print_delimited![
                func,
                format_c_str(self, args[0]),
                format_flags!(args[1] => nix::unistd::AccessFlags)
            ],
            Sysno::pipe => print_delimited![func, format_array::<Fd>(self, args[0], 2)],
            Sysno::pipe2 => print_delimited![
                func,
                format_array::<Fd>(self, args[0], 2),
                format_flags!(args[1] => nix::fcntl::OFlag)
            ],
            Sysno::select => print_delimited![
                func,
                args[0].to_string(),
                format_fdset(self, args[1]),
                format_fdset(self, args[2]),
                format_fdset(self, args[3]),
                format_ptr(args[4])
            ],
            Sysno::pselect6 => print_delimited![
                func,
                args[0].to_string(),
                format_fdset(self, args[1]),
                format_fdset(self, args[2]),
                format_fdset(self, args[3]),
                format_ptr(args[4]),
                format_sigset(self, args[5])
            ],
            Sysno::sched_yield => print_delimited![],
            Sysno::mremap => {
                if args[3] as i32 & libc::MREMAP_FIXED == libc::MREMAP_FIXED {
                    print_delimited![
                        func,
                        format_ptr(args[0]),
                        args[1].to_string(),
                        args[2].to_string(),
                        format_flags!(args[3] => nix::sys::mman::MRemapFlags),
                        format_ptr(args[4])
                    ]
                } else {
                    print_delimited![
                        func,
                        format_ptr(args[0]),
                        args[1].to_string(),
                        args[2].to_string(),
                        format_flags!(args[3] => nix::sys::mman::MRemapFlags)
                    ]
                }
            }
            Sysno::msync => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                format_flags!(args[2] => nix::sys::mman::MsFlags)
            ],
            Sysno::mincore => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                format_bytes_u8(self, args[2], args[1])
            ],
            Sysno::openat => print_delimited![
                func,
                if args[0] == 4294967196 {
                    "AT_FDCWD".to_string()
                } else {
                    format_fd(args[0])
                },
                format_c_str(self, args[1]),
                format_flags!(args[2] => nix::fcntl::OFlag)
            ],
            Sysno::set_tid_address => print_delimited![func, format_ptr(args[0])],
            Sysno::set_robust_list => {
                print_delimited![func, format_ptr(args[0]), args[1].to_string()]
            }
            Sysno::getrandom => print_delimited![
                func,
                format_bytes_u8(self, args[0], args[1]),
                args[1].to_string(),
                {
                    let has_random = args[2] as u32 & libc::GRND_RANDOM == libc::GRND_RANDOM;
                    let has_nonblock = args[2] as u32 & libc::GRND_NONBLOCK == libc::GRND_NONBLOCK;

                    match (has_random, has_nonblock) {
                        (true, true) => "GRND_NONBLOCK | GRND_RANDOM",
                        (true, false) => "GRND_RANDOM",
                        (false, true) => "GRND_NONBLOCK",
                        _ => "(empty)",
                    }
                }
            ],
            Sysno::newfstatat => print_delimited![
                func,
                format_fd(args[0]),
                format_c_str(self, args[1]),
                format_stat(self, args[2]),
                format_flags!(args[3] => nix::fcntl::AtFlags)
            ],
            Sysno::futex => print_delimited![
                func,
                format_ptr(args[0]),
                format_futex_op(args[1]),
                args[2].to_string()
            ],
            Sysno::getuid => print_delimited![],
            Sysno::syslog => print_delimited![
                func,
                match args[0] {
                    0 => "SYSLOG_ACTION_CLOSE",
                    1 => "SYSLOG_ACTION_OPEN",
                    2 => "SYSLOG_ACTION_READ",
                    3 => "SYSLOG_ACTION_READ_ALL",
                    4 => "SYSLOG_ACTION_READ_CLEAR",
                    5 => "SYSLOG_ACTION_CONSOLE_OFF",
                    6 => "SYSLOG_ACTION_CONSOLE_ON",
                    7 => "SYSLOG_ACTION_CONSOLE_LEVEL",
                    8 => "SYSLOG_ACTION_SIZE_UNREAD",
                    9 => "SYSLOG_ACTION_SIZE_BUFFER",
                    _ => "(unknown)",
                },
                format_str(self, args[1], args[2])
            ],
            Sysno::getgid => print_delimited![],
            Sysno::setuid => print_delimited![func, args[0].to_string()],
            Sysno::setgid => print_delimited![func, args[0].to_string()],
            Sysno::geteuid => print_delimited![],
            Sysno::getegid => print_delimited![],
            Sysno::setpgid => print_delimited![func, args[0].to_string(), args[1].to_string()],
            Sysno::getppid => print_delimited![],
            Sysno::getpgrp => print_delimited![],
            Sysno::setsid => print_delimited![func, args[0].to_string()],
            Sysno::setreuid => print_delimited![func, args[0].to_string(), args[1].to_string()],
            Sysno::setregid => print_delimited![func, args[0].to_string(), args[1].to_string()],
            Sysno::getgroups => print_delimited![func, args[0].to_string(), format_ptr(args[1])],
            Sysno::setgroups => print_delimited![
                func,
                args[0].to_string(),
                format_array::<c_int>(self, args[1], args[0])
            ],
            Sysno::setresuid => print_delimited![
                func,
                args[0].to_string(),
                args[1].to_string(),
                args[2].to_string()
            ],
            Sysno::getresuid => print_delimited![
                func,
                format_ptr(args[0]),
                format_ptr(args[1]),
                format_ptr(args[2])
            ],
            Sysno::setresgid => print_delimited![
                func,
                args[0].to_string(),
                args[1].to_string(),
                args[2].to_string()
            ],
            Sysno::getresgid => print_delimited![
                func,
                format_ptr(args[0]),
                format_ptr(args[1]),
                format_ptr(args[2])
            ],
            Sysno::getpgid => print_delimited![func, args[0].to_string()],
            Sysno::setfsuid => print_delimited![func, args[0].to_string()],
            Sysno::setfsgid => print_delimited![func, args[0].to_string()],
            Sysno::getsid => print_delimited![func, args[0].to_string()],
            Sysno::exit => print_delimited![func, args[0].to_string()],
            Sysno::exit_group => print_delimited![func, args[0].to_string()],
            _ => func += "..",
        }

        func += ")";
        func
    }
}
