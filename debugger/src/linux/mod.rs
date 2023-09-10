use std::ffi::CString;
use std::fmt;
use std::os::unix::ffi::OsStrExt;

use nix::sys::signal::{kill, Signal};
use nix::sys::wait::{self, WaitStatus};
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
    unprocessed_signal: Option<Signal>,
    last_syscall: ptrace::SyscallInfoOp,
    print_buf: String,
}

// fn set_empty_signal_handler(signal: Signal) -> Result<(), Error> {
//     unsafe {
//         signal::sigaction(
//             signal,
//             &signal::SigAction::new(
//                 signal::SigHandler::SigIgn,
//                 signal::SaFlags::empty(),
//                 signal::SigSet::empty(),
//             ),
//         )
//         .map_err(Error::Kernel)?;
//     }
//
//     Ok(())
// }

// fn set_signal_handlers() -> Result<(), Error> {
//     set_empty_signal_handler(Signal::SIGINT)?;
//     set_empty_signal_handler(Signal::SIGQUIT)?;
//     set_empty_signal_handler(Signal::SIGPIPE)?;
//     set_empty_signal_handler(Signal::SIGTERM)?;
//     Ok(())
// }

impl Debugger {
    fn local(root: Pid) -> Self {
        let mut pids = Tree::new();
        pids.push_root(root, State::WaitingForInit);

        Debugger {
            pids,
            tracing_syscalls: false,
            remote: false,
            unprocessed_signal: None,
            last_syscall: ptrace::SyscallInfoOp::None,
            print_buf: String::new(),
        }
    }

    fn remote(root: Pid) -> Self {
        let mut this = Debugger::local(root);
        this.remote = true;
        this
    }

    fn set_options(&mut self, pid: Pid) -> Result<(), Error> {
        let mut options = ptrace::Options::empty();

        if self.tracing_syscalls {
            // distinguish regular SIGTRAP's from syscalls
            options |= ptrace::Options::PTRACE_O_TRACESYSGOOD;
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
            // wait for child to give send a message
            let status = wait::waitpid(
                self.pids.root(),
                Some(wait::WaitPidFlag::WSTOPPED | wait::WaitPidFlag::WCONTINUED),
            )
            .map_err(Error::Kernel)?;

            let pid = status.pid().unwrap();

            let state = match self.pids.find(&pid) {
                Some(state) => state,
                // if a pid isn't being tracked, don't handle to it's event
                None => continue,
            };

            match state {
                State::WaitingForInit => {
                    assert!(
                        matches!(status, WaitStatus::Stopped(_, Signal::SIGSTOP)),
                        "got '{status:?}' expected a SIGSTOP"
                    );

                    *state = State::Running;
                    self.set_options(pid)?;
                    self.kontinue(pid);
                }
                State::Running => self.process_event(status)?,
            }

            if self.pids.is_empty() {
                return Ok(());
            }
        }
    }

    fn process_event(&mut self, status: wait::WaitStatus) -> Result<(), Error> {
        match status {
            WaitStatus::Stopped(pid, signal) => {
                self.unprocessed_signal = Some(signal);
                self.kontinue(pid);
            }
            WaitStatus::Signaled(pid, signal, ..) => {
                self.pids.remove(&pid);
                self.kontinue(pid);
                log::gray!("[debugger::event] child exited by signal: {signal:?}.");
            }
            WaitStatus::Exited(pid, code) => {
                self.pids.remove(&pid);
                self.kontinue(pid);
                log::gray!("[debugger::event] child '{pid}' exited with code '{code}'.");
            }
            WaitStatus::PtraceSyscall(pid) => {
                let syscall = ptrace::getsyscallinfo(pid).map_err(Error::Kernel)?;

                match syscall.op {
                    ptrace::SyscallInfoOp::Entry { nr, args } => {
                        let sysno = trace::Sysno::from(nr as i32);
                        let func = self.display(sysno, args);

                        //if sysno == trace::Sysno::exit_group {
                        self.print_buf += &func;
                        self.last_syscall = syscall.op;
                    }
                    ptrace::SyscallInfoOp::Exit { ret_val, is_error } => {
                        const EXIT: u64 = trace::Sysno::exit_group as u64;

                        // `exit_group` syscall doesn't have a return value
                        if let ptrace::SyscallInfoOp::Entry { nr: EXIT, .. } = self.last_syscall {
                            self.kontinue(pid);
                            return Ok(());
                        }

                        self.print_buf += " -> ";
                        self.print_buf += &ret_val.to_string();

                        if is_error == 1 {
                            let err = nix::Error::from_i32(-ret_val as i32);

                            self.print_buf += " ";
                            self.print_buf += &err.to_string();
                        }

                        log::gray!("[debugger::syscall] {}", self.print_buf);
                        self.print_buf.clear();
                    }
                    _ => {}
                }

                self.kontinue(pid);
            }
            WaitStatus::PtraceEvent(pid, _, event) => {
                use nix::libc::*;

                // if a new pid was created, store it as the child of it's parent
                if let PTRACE_EVENT_FORK | PTRACE_EVENT_VFORK | PTRACE_EVENT_CLONE = event {
                    let created_pid = ptrace::getevent(pid).map_err(Error::Kernel)?;
                    let created_pid = nix::unistd::Pid::from_raw(created_pid as i32);

                    self.pids.push_child(&pid, created_pid, State::WaitingForInit);
                }

                self.kontinue(pid);
            }
            _ => unreachable!("{status:?}"),
        }

        Ok(())
    }

    pub fn trace_syscalls(&mut self, tracing: bool) {
        self.tracing_syscalls = tracing;
    }
}

impl Process for Debugger {
    fn spawn<P: AsRef<std::path::Path>>(path: P, args: Vec<String>) -> Result<Self, Error> {
        let c_path = CString::new(path.as_ref().as_os_str().as_bytes())
            .map_err(|_| Error::InvalidPathName)?;

        let mut c_args = Vec::with_capacity(args.len() + 1);

        // push program as first argument
        c_args.push(c_path.clone());

        // push arguments
        for arg in args {
            let arg = CString::new(arg).map_err(|_| Error::InvalidPathName)?;
            c_args.push(arg);
        }

        let child = || {
            // disable ASLR
            personality::set(personality::Persona::ADDR_NO_RANDOMIZE)?;

            // signal child process to be traced
            ptrace::traceme()?;

            // stop child
            nix::sys::signal::raise(Signal::SIGSTOP)?;

            // execute program
            execvp(&c_path, &c_args)
        };

        match unsafe { fork().map_err(Error::Kernel)? } {
            ForkResult::Parent { child } => Ok(Debugger::local(child)),
            ForkResult::Child => match child() {
                Err(err) => panic!("{err:?}"),
                Ok(..) => unsafe { nix::libc::_exit(1) },
            },
        }
    }

    fn attach(pid: Pid) -> Result<Self, Error> {
        ptrace::attach(pid).map_err(Error::Kernel)?;
        Ok(Debugger::remote(pid))
    }
}

impl Tracee for Debugger {
    fn detach(&mut self) {
        for (pid, _) in self.pids.into_iter() {
            // ignore the result since detaching can't fail
            let _ = ptrace::detach(pid, None);
        }
    }

    fn kill(&mut self) {
        for (pid, _) in self.pids.into_iter() {
            // ignore the result since killing a process can't fail
            let _ = kill(pid, Signal::SIGKILL);
        }
    }

    fn pause(&self, pid: Pid) {
        // ignore the result since it appears to be unlikely interrupting can fail
        //
        // https://github.com/torvalds/linux/blob/d528014517f2b0531862c02865b9d4c908019dc4/kernel/ptrace.c#L1137
        let _ = ptrace::interrupt(pid);
    }

    fn kontinue(&mut self, pid: Pid) {
        let sig = std::mem::take(&mut self.unprocessed_signal);

        // ignore the result since continuing a process can't fail
        if self.tracing_syscalls {
            let _ = ptrace::syscall(pid, sig);
        } else {
            let _ = ptrace::cont(pid, sig);
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

impl Drop for Debugger {
    fn drop(&mut self) {
        if !self.remote {
            self.kill();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn spawn() {
        let mut session =
            Debugger::spawn("sh", vec!["-c".to_string(), "echo 10".to_string()]).unwrap();
        // let mut session = Debugger::spawn("../target/debug/bite", &[]).unwrap();
        session.trace_syscalls(true);
        session.run_to_end().unwrap();
    }
}
