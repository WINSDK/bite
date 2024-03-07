mod collections;
mod error;
mod ioctl;
mod memory;
mod readmem;
mod systrace;
mod tests;
mod writemem;

use std::fmt;
use std::ffi::{c_long, CString};
use std::os::fd::RawFd;
use std::path::PathBuf;

use nix::fcntl::{fcntl, FcntlArg, FdFlag, OFlag};
use nix::libc;
use nix::sys::ptrace;
use nix::sys::signal::Signal;
use nix::sys::wait::{waitid, waitpid, Id, WaitPidFlag, WaitStatus};
use nix::unistd::{fork, ForkResult, Pid};
use procfs::process::{MemoryMap, Process};

use crate::collections::Tree;

pub use error::Error;
pub use readmem::ReadMemory;
pub use writemem::WriteMemory;

pub type Tid = Pid;
pub type ExitStatus = i32;

/// Describes what to do with a standard I/O stream for a [`Tracee`] and it's children.
///
/// By default:
/// * Tests redirect to /dev/null
/// * Regular usage doesn't touch any file descriptors
#[derive(Debug, Clone, PartialEq)]
pub enum Stdio {
    /// Don't touch any of the IO.
    Console,
    /// Discard the IO
    Null,
    /// Pipe output to some file descriptor.
    Fd(std::os::fd::RawFd),
}

impl Default for Stdio {
    fn default() -> Self {
        #[cfg(test)]
        {
            Self::Null
        }
        #[cfg(not(test))]
        {
            Self::Console
        }
    }
}

impl Stdio {
    /// Open file descriptor to underlying option if necessary.
    /// Returns whether or not the option requires a redirection.
    ///
    /// Doesn't close the file descriptor (so it does for example keep /dev/null open.)
    fn to_fd(&self) -> Option<RawFd> {
        match self {
            Self::Console => None,
            Self::Fd(fd) => Some(*fd),
            Self::Null => {
                nix::fcntl::open("/dev/null", OFlag::O_RDWR, nix::sys::stat::Mode::empty()).ok()
            }
        }
    }
}

/// Anything related to creating a [`Debugger`].
#[derive(Default, Debug, Clone)]
pub struct DebuggerDescriptor {
    /// Program being traced.
    pub path: PathBuf,

    /// Environmental variables set for the target.
    pub env: Vec<String>,

    /// Process arguments.
    pub args: Vec<String>,

    /// Whether or not to trace syscalls.
    pub tracing: bool,

    /// Whether or not syscall tracing should apply to children.
    pub follow_children: bool,

    /// Stdin options for redirection.
    pub stdin: Stdio,

    /// Stdout options for redirection.
    pub stdout: Stdio,

    /// Stderr options for redirection.
    pub stderr: Stdio,
}

/// Debugger of a group of tracees
///
/// Debuggers maintain the state of the tracees in their group. It is possible
/// to have multiple [`Debugger`]s in a single process, allowing separated
/// sets of tracees to be debugged.
///
/// A debugger doesn't particularly manage a group of threads identical to a
/// process group or a thread group, instead it's an arbitrary collection of
/// tracees that the user specifies.
///
/// A [`Debugger`] will only consume events from its set of tracees. It also
/// will not manage tracees unless they are known about ahead of time. For
/// example, if a `waitpid()` returns a PID that we are not aware of, the tool
/// will throw an error, rather than silently adding it to the pool of tracees.
///
/// To make this all work requires that we have a coherent state machine of
/// processes we expect to be showing up. This is a hard problem with
/// `ptrace()` but if we can solve it, hopefully the debugger will be
/// significantly more reliable than GDB and LLDB which are a bit more
/// handwavey about unexpected states.
///
/// ```text
/// +---------------------------------------------------------+
/// | Your debugger process                                   |
/// |                                                         |
/// | +------------------+ +------------------+ +-----------+ |
/// | | Debugger         | | Debugger         | | ...       | |
/// | |                  | |                  | |           | |
/// | | +--------------+ | | +--------------+ | |           | |
/// | | | Tracee       | | | | Tracee       | | |           | |
/// | | |              | | | |              | | |           | |
/// | | +--------------+ | | +--------------+ | |           | |
/// | | +--------------+ | | +--------------+ | |           | |
/// | | | Tracee       | | | | ...          | | |           | |
/// | | |              | | | |              | | |           | |
/// | | +--------------+ | | +--------------+ | |           | |
/// | | +--------------+ | |                  | |           | |
/// | | | ...          | | |                  | |           | |
/// | | |              | | |                  | |           | |
/// | | +--------------+ | |                  | |           | |
/// | +------------------+ +------------------+ +-----------+ |
/// +---------------------------------------------------------+
/// ```
pub struct Debugger {
    /// This isn't really a tree, ptrace can report the exiting of a parent
    /// before it's children, therefore this can't really be a tree.
    /// It it however a mapping from [`Pid`]'s to [`Tracee`]'s with a root [`Tracee`].
    tracees: Tree,

    /// Whether or not to debug print syscall information.
    tracing: bool,

    /// Whether or not the previously ran syscall was a syscall that returned.
    print_next_syscall: bool,

    /// Buffer that holds a debug representation of a syscall, necessary as we need
    /// to store syscall info between entry and exit.
    print_buf: String,
}

impl Debugger {
    pub fn spawn(mut desc: DebuggerDescriptor) -> Result<Self, Error> {
        let c_path = CString::new(desc.path.as_os_str().as_encoded_bytes())
            .map_err(|_| Error::InvalidPathName)?;

        let mut c_args = Vec::with_capacity(desc.args.len() + 1);

        // Push program as first argument.
        c_args.push(c_path.clone());

        // Push program arguments.
        for arg in desc.args {
            let arg = CString::new(arg).map_err(|_| Error::InvalidPathName)?;
            c_args.push(arg);
        }

        // Create a communication pipe for error handling.
        let (pipe_read, pipe_write) = nix::unistd::pipe()?;

        // Tell write pipe to close on exec (required for checking successful execution).
        fcntl(pipe_write, FcntlArg::F_SETFD(FdFlag::FD_CLOEXEC))?;

        match unsafe { fork()? } {
            ForkResult::Child => {
                let _ = nix::unistd::close(pipe_read);

                // Duplicate FD to stdin.
                if let Some(fd) = desc.stdin.to_fd() {
                    let _ = nix::unistd::dup2(fd, 0);
                }

                if let Some(fd) = desc.stdout.to_fd() {
                    let _ = nix::unistd::dup2(fd, 1);
                }

                if let Some(fd) = desc.stderr.to_fd() {
                    let _ = nix::unistd::dup2(fd, 2);
                }

                let report_error = |err| {
                    // Convert error to bytes.
                    let errno = (err as i32).to_ne_bytes();

                    // Write error status to pipe.
                    let _ = nix::unistd::write(pipe_write, &errno);

                    // Explicitly close the write end of the pipe to ensure the parent can read EOF
                    // if exec hasn't been called.
                    let _ = nix::unistd::close(pipe_write);

                    unsafe { libc::_exit(0) }
                };

                // signal child process to be traced
                if let Err(err) = ptrace::traceme() {
                    report_error(err);
                }

                // inherit environmental variables
                let c_env: Vec<CString> = std::env::vars()
                    .flat_map(|(key, val)| CString::new(format!("{key}={val}")))
                    .chain(std::mem::take(&mut desc.env).into_iter().flat_map(CString::new))
                    .collect();

                // execute program
                if let Err(err) = nix::unistd::execvpe(&c_path, &c_args, &c_env) {
                    report_error(err);
                }

                unreachable!()
            }
            ForkResult::Parent { child } => {
                let _ = nix::unistd::close(pipe_write);

                let mut errno = [0; 4];
                match nix::unistd::read(pipe_read, &mut errno) {
                    // Child ran into error.
                    Ok(4) => {
                        let errno = i32::from_ne_bytes(errno);
                        let errno = nix::Error::from_i32(errno);
                        return Err(Error::Kernel(errno));
                    }
                    // Child ran successfully.
                    Ok(..) => {}
                    // Unexpected error.
                    Err(..) => {}
                }

                let _ = nix::unistd::close(pipe_read);
                let tracee = Tracee::process(child);

                Ok(Debugger {
                    tracees: Tree::new(child, tracee),
                    tracing: desc.tracing,
                    print_next_syscall: true,
                    print_buf: String::new(),
                })
            }
        }
    }

    fn debug_print_syscall(&mut self, pid: Pid) -> Result<(), Error> {
        let tracee = self.tracees.get_mut(pid)?;

        // Check condition for logging syscall.
        if !self.tracing {
            return Ok(());
        }

        match ptrace::getsyscallinfo(pid)?.op {
            ptrace::SyscallInfoOp::Entry { nr, args } => {
                let nr = nr as c_long;
                self.print_buf += &systrace::decode(tracee, nr, args);
                self.print_next_syscall = nr != libc::SYS_exit_group;
            }
            ptrace::SyscallInfoOp::Exit { ret_val, is_error } => {
                if !self.print_next_syscall {
                    return Ok(());
                }

                self.print_buf += " -> ";
                self.print_buf += &ret_val.to_string();

                if is_error == 1 {
                    let err = nix::Error::from_i32(-ret_val as i32);

                    self.print_buf += " ";
                    self.print_buf += &err.to_string();
                }

                log::trace!("[debugger::syscall] {}", self.print_buf);
                self.print_buf.clear();
            }
            _ => {}
        }

        Ok(())
    }

    /// Consumes an [`WaitStatus`], returning it's error code if all tracees exited.
    fn consume_tracee_status(&mut self, status: WaitStatus) -> Result<Option<ExitStatus>, Error> {
        match status {
            WaitStatus::Exited(pid, code) => {
                log::complex!(
                    w "[debugger::event] tracee ",
                    g pid.to_string(),
                    w " exited with code ",
                    y code.to_string(),
                    w "."
                );

                self.tracees.remove(pid);
                if self.tracees.is_empty() {
                    return Ok(Some(code));
                }
            }
            WaitStatus::Signaled(pid, signal, _) => {
                log::complex!(
                    w "[debugger::event] tracee ",
                    g pid.to_string(),
                    w " exited by signal ",
                    y signal.to_string(),
                    w "."
                );

                self.tracees.remove(pid);
                if self.tracees.is_empty() {
                    // exit code is determined by adding 128 to the signal
                    return Ok(Some(128 + signal as i32));
                }
            }
            WaitStatus::Stopped(pid, Signal::SIGTRAP) => {
                println!("hit breakpoint");
                ptrace::cont(pid, None)?;
            }
            WaitStatus::Stopped(pid, signal) => {
                ptrace::cont(pid, Some(signal))?;
            }
            // Events we enabled tracing for with ptrace::setoptions(..).
            WaitStatus::PtraceEvent(pid, Signal::SIGTRAP, event) => {
                assert!(event <= 3, "Unexpected ptrace event received.");
                let child = ptrace::getevent(pid)?;
                let child = Tid::from_raw(child as i32);

                if event == libc::PTRACE_EVENT_FORK {
                    let tracee = Tracee::process(child);
                    self.tracees.push(pid, child, tracee);

                    log::complex!(
                        w "[debugger::event] tracee ",
                        g child.to_string(),
                        w " created using fork."
                    );
                }

                if event == libc::PTRACE_EVENT_VFORK {
                    let tracee = Tracee::process(child);
                    self.tracees.push(pid, child, tracee);

                    log::complex!(
                        w "[debugger::event] tracee ",
                        g child.to_string(),
                        w " created using vfork."
                    );
                }

                // This might be a new thread or it might be a new process.
                // It depends on the flags passed on clone which makes this incorrect.
                if event == libc::PTRACE_EVENT_CLONE {
                    // waitpid(..) could return either a pid or a tid so we must query the tracee
                    let parent_tracee = self.tracees.get(pid)?;
                    let parent_pid = parent_tracee.pid;

                    let tracee = Tracee::thread(parent_pid, child);
                    self.tracees.push(pid, child, tracee);

                    log::complex!(
                        w "[debugger::event] tracee ",
                        g child.to_string(),
                        w " created using clone."
                    );
                }

                ptrace::cont(pid, None)?;
            }
            WaitStatus::PtraceSyscall(pid) => {
                self.debug_print_syscall(pid)?;
                ptrace::cont(pid, None)?;
            }
            _ => unreachable!(),
        }

        Ok(None)
    }

    pub fn run(&mut self) -> Result<ExitStatus, Error> {
        // Consume status send as a result of exec'ing.
        let root = self.tracees.root();
        assert_eq!(
            waitpid(root.pid, Some(WaitPidFlag::WSTOPPED))?,
            WaitStatus::Stopped(root.pid, Signal::SIGTRAP)
        );

        root.enable_additional_tracing()?;
        ptrace::cont(root.pid, None)?;

        loop {
            // Wait for any children to emit an event. Only peek at the status though.
            let mut status = waitid(
                Id::All,
                WaitPidFlag::WEXITED | WaitPidFlag::WSTOPPED | WaitPidFlag::WNOWAIT,
            )?;

            let pid = status.pid().unwrap();

            if self.tracees.get(pid).is_ok() {
                // If the event from a known pid, consume the event.
                status = waitpid(pid, Some(WaitPidFlag::WSTOPPED | WaitPidFlag::WNOHANG))?;
            } else {
                // Otherwise we must look ahead and see if any of the tracees emitted an event
                // that could've created a new child. This kind of event should only be a
                // PTRACE_EVENT_CLONE, PTRACE_EVENT_FORK or a PTRACE_EVENT_VFORK.
                let mut received_event = false;
                for pid in self.tracees.pids() {
                    let tracees_status =
                        waitpid(pid, Some(WaitPidFlag::WSTOPPED | WaitPidFlag::WNOHANG))?;

                    // I don't like this, sometimes the tracees_status is WaitStatus::Exited
                    // which I feel like shouldn't be possible. Only happens when running
                    // multiple tests at once.
                    if tracees_status != WaitStatus::StillAlive {
                        status = tracees_status;
                        received_event = true;
                        break;
                    }
                }

                // If both waitid(..) returned an unknown status and it didn't come
                // from any of the children then discard the event.
                if !received_event {
                    continue;
                }
            }

            if let Some(code) = self.consume_tracee_status(status)? {
                return Ok(code);
            }
        }
    }
}

/// ```text
/// A tracee is a thread, it knows what process it's part of.
/// You can read process memory through a thread.
/// If to read some_struct->some_variable, I need to know it's process.
/// It's process can be determined using a pid.
/// ```
#[derive(Debug)]
pub struct Tracee {
    pub pid: Pid,
    pub tid: Tid,
}

impl Tracee {
    fn process(pid: Pid) -> Self {
        Self { pid, tid: pid }
    }

    fn thread(pid: Pid, tid: Tid) -> Self {
        Self { pid, tid }
    }

    fn is_process(&self) -> bool {
        self.pid == self.tid
    }

    fn is_thread(&self) -> bool {
        self.pid != self.tid
    }

    /// Set additional ptrace options for tracing the creation of children.
    /// [`Tracee`] must be stopped before calling this.
    fn enable_additional_tracing(&self) -> Result<(), nix::Error> {
        ptrace::setoptions(
            self.pid,
            ptrace::Options::PTRACE_O_TRACEFORK
                | ptrace::Options::PTRACE_O_TRACEVFORK
                | ptrace::Options::PTRACE_O_TRACECLONE
                | ptrace::Options::PTRACE_O_TRACESYSGOOD
        )
    }

    /// Opens a procfs process and reads it's associated memory maps.
    /// Should be up to date as it's re-read on each call.
    fn memory_maps(&self) -> Result<Vec<MemoryMap>, Error> {
        let tracee_proc = Process::new(self.pid.as_raw())?;
        let memory_maps = tracee_proc.maps().map(|map| map.0)?;
        Ok(memory_maps)
    }
}

impl fmt::Display for Tracee {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_thread() {
            f.write_fmt(format_args!("Thread {{ pid: {}, tid: {} }}", self.pid, self.tid))
        } else {
            f.write_fmt(format_args!("Process {{ pid: {} }}", self.pid))
        }
    }
}
