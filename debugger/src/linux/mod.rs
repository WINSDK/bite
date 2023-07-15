use std::ffi::CString;
use std::fmt;
use std::os::unix::ffi::OsStrExt;

use nix::sys::signal::{Signal, kill};
use nix::sys::wait::WaitStatus;
use nix::sys::{personality, ptrace};
use nix::unistd::{execvp, fork, ForkResult};

use crate::collections::Tree;
use crate::{Process, Tracee};

mod ioctl;
mod trace;

pub type Pid = nix::unistd::Pid;

pub enum Error {
    InvalidPathName,
    IncompleteRead(usize, usize),
    IncompleteWrite(usize, usize),
    Kernel(nix::errno::Errno),
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidPathName => f.write_str("There appears to be a '\\0' in the path name."),
            Self::IncompleteRead(req, res) => {
                f.write_fmt(format_args!("Tried to read {req} bytes, only read {res}."))
            }
            Self::IncompleteWrite(req, res) => f.write_fmt(format_args!(
                "Tried to write {req} bytes, only wrote {res}."
            )),
            Self::Kernel(err) => f.write_fmt(format_args!("{err}.")),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum State {
    WaitingForInit,
    Running,
}

pub struct Debugger {
    pids: Tree<Pid, State>,
    tracing_syscalls: bool,
    remote: bool,
}

impl Debugger {
    fn set_options(&mut self, pid: Pid) -> Result<(), Error> {
        let mut options = ptrace::Options::empty();

        if self.tracing_syscalls {
            // distinguish regular SIGTRAP's from syscalls
            options |= ptrace::Options::PTRACE_O_TRACESYSGOOD;
        }

        if !self.remote {
            // kill process when we exit
            options |= ptrace::Options::PTRACE_O_EXITKILL;
        }

        // break on common syscalls
        let options = options
            // new thread
            | ptrace::Options::PTRACE_O_TRACECLONE
            // new process
            | ptrace::Options::PTRACE_O_TRACEEXEC
            // exit child
            | ptrace::Options::PTRACE_O_TRACEEXIT
            // new child
            | ptrace::Options::PTRACE_O_TRACEFORK
            // new child in same memory space
            | ptrace::Options::PTRACE_O_TRACEVFORK;

        ptrace::setoptions(pid, options).map_err(Error::Kernel)
    }

    pub fn run_to_end(&mut self) -> Result<(), Error> {
        loop {
            let status = nix::sys::wait::wait().map_err(Error::Kernel)?;

            match status {
                WaitStatus::Stopped(
                    pid,
                    signal @ (Signal::SIGTRAP | Signal::SIGSTOP | Signal::SIGTSTP),
                ) => match self.pids.find(&pid) {
                    State::WaitingForInit => {
                        let _ = ptrace::detach(pid, None).unwrap();
                        self.set_options(pid)?;
                        *self.pids.find(&pid) = State::Running;
                    }
                    State::Running => {
                        println!("stopped by signal: {signal:?}");
                        self.pause();
                        continue;
                    }
                },
                WaitStatus::Stopped(
                    pid,
                    signal @ (Signal::SIGHUP
                    | Signal::SIGINT
                    | Signal::SIGQUIT
                    | Signal::SIGILL
                    | Signal::SIGABRT
                    | Signal::SIGBUS
                    | Signal::SIGFPE
                    | Signal::SIGKILL
                    | Signal::SIGUSR1
                    | Signal::SIGUSR2
                    | Signal::SIGSEGV
                    | Signal::SIGPIPE
                    | Signal::SIGTERM),
                ) => {
                    println!("\nexited by signal: '{signal:?}'");
                    self.pids.remove(&pid);
                }
                WaitStatus::Stopped(_, signal) => {
                    println!("signal '{signal:?}' happened");
                }
                WaitStatus::Signaled(pid, signal, ..) => {
                    self.pids.remove(&pid);
                    println!("\nexited by signal: {signal:?}");
                }
                WaitStatus::Exited(pid, _) => {
                    self.pids.remove(&pid);
                    println!("\nprocess '{pid:?}' exited");
                }
                WaitStatus::PtraceSyscall(pid) => {
                    let syscall = ptrace::getsyscallinfo(pid).map_err(Error::Kernel)?;

                    match syscall.op {
                        ptrace::SyscallInfoOp::Entry { nr, args } => {
                            let func = self.display(trace::Sysno::from(nr as i32), args);
                            print!("{func}");
                        },
                        ptrace::SyscallInfoOp::Exit { ret_val, is_error } => {
                            let mut ret = String::new();

                            ret += " -> ";
                            ret += &ret_val.to_string();

                            if is_error == 1 {
                                let err = nix::Error::from_i32(-ret_val as i32);

                                ret += " ";
                                ret += &err.to_string();
                            }

                            println!("{ret}");
                        }
                        _ => {},
                    }
                }
                WaitStatus::PtraceEvent(pid, _, event) => {
                    use nix::libc::*;

                    // if a new pid was created, store it as the child of it's parent
                    if let PTRACE_EVENT_FORK | PTRACE_EVENT_VFORK | PTRACE_EVENT_CLONE = event {
                        let created_pid = ptrace::getevent(pid).map_err(Error::Kernel)?;
                        let created_pid = nix::unistd::Pid::from_raw(created_pid as i32);

                        self.pids.push_child(&pid, created_pid, State::WaitingForInit);
                    }
                }
                _ => unreachable!("{status:?}"),
            }

            if self.pids.is_empty() {
                return Ok(());
            }

            self.kontinue();
        }
    }

    pub fn trace_syscalls(&mut self, tracing: bool) {
        self.tracing_syscalls = tracing;
    }
}

impl Process for Debugger {
    fn spawn<P: AsRef<std::path::Path>>(path: P, args: &[&str]) -> Result<Self, Error> {
        let c_path = CString::new(path.as_ref().as_os_str().as_bytes())
            .map_err(|_| Error::InvalidPathName)?;

        let mut c_args = Vec::with_capacity(args.len() + 1);

        // push program as first argument
        c_args.push(c_path.clone());

        // push arguments
        for arg in args {
            let arg = CString::new(*arg).map_err(|_| Error::InvalidPathName)?;
            c_args.push(arg);
        }

        match unsafe { fork().map_err(Error::Kernel)? } {
            ForkResult::Parent { child } => {
                let mut pids = Tree::new();
                pids.push_root(child, State::WaitingForInit);

                Ok(Debugger {
                    pids,
                    tracing_syscalls: false,
                    remote: false,
                })
            }
            ForkResult::Child => {
                // disable ASLR
                personality::set(personality::Persona::ADDR_NO_RANDOMIZE).map_err(Error::Kernel)?;

                // signal child process to be traced
                ptrace::traceme().map_err(Error::Kernel)?;

                // execute program
                execvp(&c_path, &c_args).map_err(Error::Kernel)?;

                // `execvp` can't exit successfully, this shouldn't be run
                unsafe { nix::libc::_exit(1) }
            }
        }
    }

    fn attach(pid: Pid) -> Result<Self, Error> {
        ptrace::attach(pid).map_err(Error::Kernel)?;
        let mut pids = Tree::new();
        pids.push_root(pid, State::WaitingForInit);

        Ok(Debugger {
            pids,
            tracing_syscalls: false,
            remote: true,
        })
    }
}

impl Tracee for Debugger {
    fn detach(self) {
        // ignore the result since detaching can't fail
        let _ = ptrace::detach(self.pids.root(), None);
    }

    fn kill(self) {
        // ignore the result since killing a process can't fail
        let _ = kill(self.pids.root(), Signal::SIGKILL);
    }

    fn pause(&self) {
        // ignore the result since it appears to be unlikely interrupting can fail
        //
        // https://github.com/torvalds/linux/blob/d528014517f2b0531862c02865b9d4c908019dc4/kernel/ptrace.c#L1137
        let _ = ptrace::interrupt(self.pids.root());
    }

    fn kontinue(&self) {
        // ignore the result since continuing a process can't fail
        if self.tracing_syscalls {
            let _ = ptrace::syscall(self.pids.root(), None);
        } else {
            let _ = ptrace::cont(self.pids.root(), None);
        }
    }

    fn read_process_memory(&self, base_addr: usize, len: usize) -> Result<Vec<u8>, Error> {
        // create buffer of that can hold `len` elements
        let mut buf: Vec<u8> = Vec::with_capacity(len);
        let uninit = unsafe { std::slice::from_raw_parts_mut(buf.as_mut_ptr(), len) };

        let local = std::io::IoSliceMut::new(uninit);
        let remote = nix::sys::uio::RemoteIoVec {
            base: base_addr,
            len,
        };

        let pid = self.pids.root();
        let bytes_read =
            nix::sys::uio::process_vm_readv(pid, &mut [local], &[remote]).map_err(Error::Kernel)?;

        if len != bytes_read {
            return Err(Error::IncompleteRead(len, bytes_read));
        }

        // set buffer size to `len` iff `len` number of bytes have been read
        unsafe { buf.set_len(len) }
        Ok(buf)
    }

    fn write_process_memory(&mut self, base_addr: usize, data: &[u8]) -> Result<(), Error> {
        let local = std::io::IoSlice::new(data);
        let remote = nix::sys::uio::RemoteIoVec {
            base: base_addr,
            len: data.len(),
        };

        let pid = self.pids.root();
        let bytes_wrote =
            nix::sys::uio::process_vm_writev(pid, &[local], &[remote]).map_err(Error::Kernel)?;

        if data.len() != bytes_wrote {
            return Err(Error::IncompleteWrite(data.len(), bytes_wrote));
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn spawn() {
        // let mut session = Debugger::spawn("sh", &["-c", "echo 10"]).unwrap();
        let mut session = Debugger::spawn("../target/debug/bite", &[]).unwrap();
        // let mut session = Debugger::spawn("./a.out", &[]).unwrap();
        session.trace_syscalls(true);
        session.run_to_end().unwrap();
    }
}
