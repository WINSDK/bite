mod collections;
mod error;
mod ioctl;
mod memory;
mod readmem;
mod systrace;
mod tests;
mod writemem;

use std::ffi::CString;
use std::os::fd::{BorrowedFd, RawFd, AsFd, AsRawFd};
use std::path::PathBuf;

use nix::poll::{poll, PollFd, PollFlags};
use nix::sys::signal::Signal;
use nix::fcntl::{fcntl, FcntlArg, FdFlag};
use nix::libc;
use nix::sys::ptrace;
use nix::sys::wait::{waitid, Id, WaitPidFlag, WaitStatus};
use nix::unistd::Pid;
use procfs::process::{MemoryMap, Process};

use crate::collections::Tree;

pub use error::Error;
pub use readmem::ReadMemory;
pub use writemem::WriteMemory;

pub type Tid = Pid;
pub type ExitStatus = i32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PidFd {
    inner: RawFd,
}

impl PidFd {
    fn as_raw(&self) -> RawFd {
        self.inner
    }
}

impl From<PollFd<'static>> for PidFd {
    fn from(value: PollFd<'_>) -> Self {
        Self { inner: value.as_fd().as_raw_fd() }
    }
}

impl std::os::fd::AsFd for PidFd {
    /// Convenient method for working with functions that don't accept [`RawFd`]'s.
    fn as_fd(&self) -> BorrowedFd<'static> {
        unsafe { BorrowedFd::borrow_raw(self.inner) }
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
struct Debugger {
    tracees: Tree<Tracee>,
}

/// Custom impl of fork using clone3.
/// Necessary as there aren't any convenient libc function for retrieving a clone's [`PidFd`].
fn fork() -> Result<Option<(Pid, PidFd)>, nix::Error> {
    #[repr(C)]
    #[derive(Default)]
    struct CloneArgs {
        flags: u64,
        pidfd: u64,
        child_tid: u64,
        parent_tid: u64,
        exit_signal: u64,
        stack: u64,
        stack_size: u64,
        tls: u64,
        set_tid: u64,
        set_tid_size: u64,
        cgroup: u64,
    }

    let mut pidfd = -1;
    let mut args = CloneArgs {
        flags: libc::CLONE_PIDFD as u64,
        pidfd: &mut pidfd as *mut libc::pid_t as u64,
        exit_signal: libc::SIGCHLD as u64,
        ..Default::default()
    };

    let res = unsafe {
        libc::syscall(
            libc::SYS_clone3,
            &mut args as *mut CloneArgs,
            std::mem::size_of::<CloneArgs>(),
        )
    };

    if res > 0 {
        // only have parent convert the Pid and PidFd
        let pid = Pid::from_raw(res as i32);
        let pidfd = PidFd { inner: pidfd };
        Ok(Some((pid, pidfd)))
    } else {
        Ok(None)
    }
}

impl Debugger {
    pub fn spawn(mut desc: DebuggerDescriptor) -> Result<Self, Error> {
        let c_path = CString::new(desc.path.as_os_str().as_encoded_bytes())
            .map_err(|_| Error::InvalidPathName)?;

        let mut c_args = Vec::with_capacity(desc.args.len() + 1);

        // push program as first argument
        c_args.push(c_path.clone());

        // push arguments
        for arg in desc.args {
            let arg = CString::new(arg).map_err(|_| Error::InvalidPathName)?;
            c_args.push(arg);
        }

        // create a communication pipe for error handling
        let (pipe_read, pipe_write) = nix::unistd::pipe()?;

        // tell write pipe to close on exec (required for checking successful execution)
        fcntl(pipe_write, FcntlArg::F_SETFD(FdFlag::FD_CLOEXEC))?;

        if let Some((pid, pidfd)) = fork()? {
            let _ = nix::unistd::close(pipe_write);

            let mut errno = [0; 4];
            match nix::unistd::read(pipe_read, &mut errno) {
                // child ran into error
                Ok(4) => {
                    let errno = i32::from_ne_bytes(errno);
                    let errno = nix::Error::from_i32(errno);
                    return Err(Error::Kernel(errno));
                }
                // child ran successfully
                Ok(..) => {}
                // unexpected error
                Err(..) => {}
            }

            let _ = nix::unistd::close(pipe_read);

            Ok(Debugger {
                tracees: Tree::new(pidfd, Tracee { pidfd, pid, tid: pid }),
            })
        } else {
            let _ = nix::unistd::close(pipe_read);

            let report_error = |err| {
                // convert error to bytes
                let errno = (err as i32).to_ne_bytes();

                // write error status to pipe
                let _ = nix::unistd::write(pipe_write, &errno);

                // explicitly close the write end of the pipe to ensure the parent can read EOF
                // if exec hasn't been called
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
    }

    pub fn run(&mut self) -> Result<ExitStatus, Error> {
        // consume status send as a result of exec'ing
        let fd = self.tracees.root().pidfd.as_fd();
        let pid = self.tracees.root().pid;
        assert_eq!(
            waitid(Id::PIDFd(fd), WaitPidFlag::WSTOPPED)?,
            WaitStatus::PtraceEvent(pid, Signal::SIGTRAP, 0)
        );

        ptrace::cont(self.tracees.root().pid, None).unwrap();

        loop {
            let poll_fds: Vec<PollFd> = self
                .tracees
                .values()
                .map(|tracee| PollFd::new(&tracee.pidfd, PollFlags::POLLIN))
                .collect();

            // SAFETY: PidFd's have a static lifetime, therefore their associated
            // PollFd's should also have a static lifetime. The reason we need a transmute here
            // is because PollFd::new creates a lifetime based on the reference not the fd itself.
            let mut poll_fds: Vec<PollFd<'static>> = unsafe { std::mem::transmute(poll_fds) };

            // poll on tracee's, waiting indefinitely
            poll(&mut poll_fds, -1)?;

            for pidfd in poll_fds {
                let pidfd = PidFd::from(pidfd);

                let status = waitid(
                    Id::PIDFd(pidfd.as_fd()),
                    WaitPidFlag::WSTOPPED | WaitPidFlag::WEXITED,
                )?;

                match status {
                    WaitStatus::Exited(_, code) => {
                        println!("tracee exited with code {code}");

                        let tracee = self.tracees.get(pidfd)?;
                        self.tracees.remove(tracee.pidfd);

                        if self.tracees.is_empty() {
                            return Ok(code);
                        }
                    }
                    _ => {
                        dbg!(status);

                        let tracee = self.tracees.get(pidfd)?;
                        ptrace::cont(tracee.pid, None)?;
                    }
                }
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
    pub pidfd: PidFd,
    pub tid: Tid,
}

impl Tracee {
    /// Opens a procfs process and reads it's associated memory maps.
    /// Should be up to date as it's re-read on each call.
    fn memory_maps(&self) -> Result<Vec<MemoryMap>, Error> {
        let tracee_proc = Process::new(self.pid.as_raw())?;
        let memory_maps = tracee_proc.maps().map(|map| map.0)?;
        Ok(memory_maps)
    }
}
