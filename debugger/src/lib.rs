mod collections;
mod error;
mod guard;
mod ioctl;
pub mod memory;
pub mod readmem;
mod systrace;
mod tests;
pub mod writemem;

use std::collections::{HashMap, VecDeque};
use std::ffi::{c_int, c_long, CString};
use std::fmt;
use std::mem::MaybeUninit;
use std::os::fd::RawFd;
use std::path::PathBuf;
use std::sync::{mpsc, Condvar};
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread::JoinHandle;

use nix::fcntl::{fcntl, open, FcntlArg, FdFlag, OFlag};
use nix::libc;
use nix::sched::CloneFlags;
use nix::sys::ptrace;
use nix::sys::signal::{self, Signal};
use nix::sys::wait::{waitid, waitpid, Id, WaitPidFlag, WaitStatus};
use nix::unistd::{fork, ForkResult, Pid};
use procfs::process::{MemoryMap, Process};

use crate::collections::Tree;

pub use error::Error;
pub use guard::StoppedDebugger;
pub use readmem::ReadMemory;
pub use writemem::WriteMemory;

pub type Tid = Pid;
pub type ExitStatus = i32;

/// Breakpoint instruction on x86/64 (int 3).
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
static BP_INST: u32 = 0xcc;

/// Breakpoint instruction on ARM (brk 0x3e8).
#[cfg(target_arch = "arm")]
static BP_INST: u32 = 0xd4207d00;

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
            Self::Null => open("/dev/null", OFlag::O_RDWR, nix::sys::stat::Mode::empty()).ok(),
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

pub struct Debugger {
    handle: JoinHandle<Result<ExitStatus, Error>>,
    parked: Arc<(Mutex<bool>, Condvar)>,
    inner: Arc<Mutex<DebuggerImpl>>,
}

impl Debugger {
    pub fn spawn(desc: DebuggerDescriptor) -> Result<Self, Error> {
        let (tx, rx) = mpsc::channel();
        let parked = Arc::new((Mutex::new(false), Condvar::new()));
        let parked_clone = Arc::clone(&parked);

        let debugger_task = move || {
            // Perform spawn in thread as we can't move DebuggerImpl.
            let debugger = DebuggerImpl::spawn(desc)?;
            let debugger = Arc::new(Mutex::new(debugger));

            tx.send(Arc::clone(&debugger)).unwrap();

            loop {
                // Wait for any children to emit an event. Only peek at the status though.
                let status = waitid(
                    Id::All,
                    WaitPidFlag::WEXITED | WaitPidFlag::WSTOPPED | WaitPidFlag::WNOWAIT,
                )?;

                let mut locked = debugger.lock().unwrap();
                match locked.ptrace_single(status) {
                    Ok(DebuggerState::Running) => {}
                    Ok(DebuggerState::Stopped(pid)) => {
                        drop(locked);

                        // Notify any threads calling wait_for_stop() that we've stopped.
                        let (lock, cvar) = &*parked_clone;
                        *lock.lock().unwrap() = true;
                        cvar.notify_all();
                        std::thread::park();

                        // kontinue() called or we weren't locked
                        // ...
                        let mut locked = debugger.lock().unwrap();
                        while let Some(cmd) = locked.pending_cmds.pop_back() {
                            match locked.handle_cmd(cmd) {
                                Err(Error::Kernel(nix::Error::ESRCH)) => {}
                                Err(err) => return Err(err),
                                Ok(()) => {}
                            }
                        }

                        // Continue tracee.
                        match locked.ptrace_restart(pid, None) {
                            Err(nix::Error::ESRCH) => {}
                            Err(err) => return Err(Error::Kernel(err)),
                            Ok(()) => {}
                        }
                    }
                    Ok(DebuggerState::Ended(code)) => return Ok(code),
                    Err(Error::Kernel(nix::Error::ESRCH)) => {
                        // The tracer cannot assume that the ptrace-stopped tracee exists.
                        // There are many scenarios when the tracee may die while stopped
                        // (such as SIGKILL).
                        //
                        // Unfortunately, the same error is returned if the tracee exists but
                        // is not ptrace-stopped (for commands which require a stopped tracee),
                        // or if it is not traced by the process which issued the ptrace call.
                    }
                    Err(err) => return Err(err),
                }
            }
        };

        let handle = std::thread::Builder::new()
            .name("debugger".to_string())
            .spawn(debugger_task)
            .expect("IO error from kernel (unhandled)");

        // This can fail if the debugger fails to spawn a process.
        let inner = match rx.recv() {
            Err(..) => match handle.join() {
                Err(_) => panic!("Debugger panicked."),
                Ok(result) => return Err(result.unwrap_err()),
            },
            Ok(inner) => inner,
        };

        Ok(Debugger {
            handle,
            parked,
            inner,
        })
    }

    pub fn wait_for_exit(self) -> Result<ExitStatus, Error> {
        match self.handle.join() {
            Err(_) => panic!("Debugger panicked."),
            Ok(guard) => guard,
        }
    }

    #[must_use]
    pub fn wait_for_stop(&self) -> StoppedDebugger {
        let (lock, cvar) = &*self.parked;
        let mut parked = lock.lock().unwrap();
        while !*parked {
            parked = cvar.wait(parked).unwrap();
        }

        StoppedDebugger::new(self.handle.thread(), parked, self.lock())
    }

    /// Send SIGSTKFLT which will trigger the debugger to discard that signal and park()
    /// it's thread until kontinue() is called.
    pub fn interrupt(&mut self) -> StoppedDebugger {
        let pid = self.lock().tracees.root().pid;
        let _ = signal::kill(pid, Signal::SIGSTKFLT);

        self.wait_for_stop()
    }

    pub fn lock(&self) -> MutexGuard<DebuggerImpl> {
        match self.inner.lock() {
            Err(_) => panic!("Debugger panicked."),
            Ok(guard) => guard,
        }
    }
}

/// Ptrace command's stored between wait_for_stop()'s and kontinue()'s.
/// Required as ptrace doesn't allow running commands from multiple threads.
#[derive(Debug)]
enum PTraceCommand {
    SetbreakPoint { addr: usize },
    UnsetbreakPoint { addr: usize },
}

/// State change from calling [`DebuggerImpl::ptrace_single`].
#[derive(Debug)]
enum DebuggerState {
    Running,
    Stopped(Pid),
    Ended(ExitStatus),
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
pub struct DebuggerImpl {
    /// This isn't really a tree, ptrace can report the exiting of a parent
    /// before it's children, therefore this can't really be a tree.
    /// It it however a mapping from [`Pid`]'s to [`Tracee`]'s with a root [`Tracee`].
    tracees: Tree,
    /// Whether or not to debug print syscall information.
    tracing: bool,
    /// We aren't able to execute [`ptrace::setoptions`] before [`Self::spawn`] calls execvpe.
    /// Therefore we'll receive a SIGTRAP on exec'ing but only for the first exec.
    did_initial_exec: bool,

    breakpoints: HashMap<usize, Breakpoint>,

    pending_cmds: VecDeque<PTraceCommand>,
}

impl DebuggerImpl {
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
            ForkResult::Child => unsafe {
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

                    libc::_exit(0);
                };

                // Disable address-space-layout randomization (might not be necessary).
                let persona = nix::sys::personality::Persona::ADDR_NO_RANDOMIZE;
                if let Err(err) = nix::sys::personality::set(persona) {
                    report_error(err);
                }

                // Signal child process to be traced.
                if let Err(err) = ptrace::traceme() {
                    report_error(err);
                }

                // Inherit environmental variables.
                let c_env: Vec<CString> = std::env::vars()
                    .flat_map(|(key, val)| CString::new(format!("{key}={val}")))
                    .chain(std::mem::take(&mut desc.env).into_iter().flat_map(CString::new))
                    .collect();

                // Execute program.
                if let Err(err) = nix::unistd::execvpe(&c_path, &c_args, &c_env) {
                    report_error(err);
                }

                std::hint::unreachable_unchecked();
            },
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
                tracee.enable_additional_tracing()?;

                Ok(DebuggerImpl {
                    tracees: Tree::new(child, tracee),
                    tracing: desc.tracing,
                    did_initial_exec: false,
                    breakpoints: HashMap::new(),
                    pending_cmds: VecDeque::new(),
                })
            }
        }
    }

    fn ptrace_single(&mut self, status: WaitStatus) -> Result<DebuggerState, Error> {
        let Some(status) = self.filter_status(status)? else {
            return Ok(DebuggerState::Running);
        };

        self.consume_tracee_status(status)
    }

    fn filter_status(&mut self, status: WaitStatus) -> Result<Option<WaitStatus>, Error> {
        let pid = status.pid().unwrap();
        let flags = Some(WaitPidFlag::WSTOPPED | WaitPidFlag::WNOHANG);

        if self.tracees.get(pid).is_ok() {
            // If the event from a known pid, consume the event.
            // This shouldn't fail as it's the result of waitid(..)
            let tstatus = waitpid(pid, flags)?;

            if tstatus != WaitStatus::StillAlive {
                return Ok(Some(tstatus));
            }
        } else {
            // Otherwise we must look ahead and see if any of the tracees emitted an event
            // that could've created a new child. This kind of event should only be a
            // PTRACE_EVENT_CLONE, PTRACE_EVENT_FORK or a PTRACE_EVENT_VFORK.
            for pid in self.tracees.pids() {
                // One of our tracees may have died, at the same time we might be processing
                // ptrace event's that happened before this death, therefore we must allow
                // for ECHILD error's in case.
                let tstatus = match waitpid(pid, flags) {
                    Ok(status) => status,
                    Err(nix::Error::ECHILD) => break,
                    Err(err) => return Err(Error::Kernel(err)),
                };

                if tstatus != WaitStatus::StillAlive {
                    return Ok(Some(tstatus));
                }
            }
        }

        // If both waitid(..) returned an unknown status and it didn't come
        // from any of the children then discard the event.
        Ok(None)
    }

    /// Consumes an [`WaitStatus`], returning it's error code if all tracees exited.
    fn consume_tracee_status(&mut self, status: WaitStatus) -> Result<DebuggerState, Error> {
        match dbg!(status) {
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
                    return Ok(DebuggerState::Ended(code));
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
                    // Exit code is determined by adding 128 to the signal.
                    return Ok(DebuggerState::Ended(128 + signal as i32));
                }
            }
            WaitStatus::Stopped(pid, Signal::SIGTRAP) if !self.did_initial_exec => {
                self.did_initial_exec = true;
                self.ptrace_restart(pid, None)?;
                return Ok(DebuggerState::Running);
            }
            WaitStatus::Stopped(pid, Signal::SIGTRAP) if self.did_initial_exec => {
                println!("hit breakpoint");
                self.handle_breakpoint(pid)?;
                return Ok(DebuggerState::Stopped(pid));
            }
            // We use the obsolete SIGSTKFLT signal to indicate a interrupt.
            WaitStatus::Stopped(pid, Signal::SIGSTKFLT) => {
                return Ok(DebuggerState::Stopped(pid));
            }
            WaitStatus::Stopped(pid, signal) => {
                self.ptrace_restart(pid, Some(signal))?;
            }
            WaitStatus::PtraceEvent(
                pid,
                Signal::SIGTRAP,
                libc::PTRACE_EVENT_FORK | libc::PTRACE_EVENT_VFORK,
            ) => {
                let child_pid = ptrace::getevent(pid)?;
                let child_pid = Tid::from_raw(child_pid as i32);
                let tracee = Tracee::process(child_pid);

                self.tracees.push(pid, child_pid, tracee);
                log::complex!(
                    w "[debugger::event] tracee ",
                    g child_pid.to_string(),
                    w " created using fork."
                );
                self.ptrace_restart(pid, None)?;
            }
            WaitStatus::PtraceEvent(pid, Signal::SIGTRAP, libc::PTRACE_EVENT_CLONE) => {
                let child_pid = ptrace::getevent(pid)?;
                let child_pid = Tid::from_raw(child_pid as i32);
                let tracee = self.tracees.get(pid)?;

                if clone_has_thread_flag(tracee)? {
                    let tracee = Tracee::thread(pid, child_pid);
                    self.tracees.push(pid, child_pid, tracee);

                    log::complex!(
                        w "[debugger::event] tracee ",
                        g child_pid.to_string(),
                        w " created using clone."
                    );
                }
                self.ptrace_restart(pid, None)?;
            }
            WaitStatus::PtraceEvent(pid, Signal::SIGTRAP, libc::PTRACE_EVENT_EXEC) => {
                // if a thread other than thread group leader does an execve(2), it disappears;
                // its PID will never be seen again, and any subsequent ptrace stops will be
                // reported under the thread group leader's PID.
                self.tracees.remove_children(pid);
                log::complex!(
                    w "[debugger::event] tracee ",
                    g pid.to_string(),
                    w " exec'd."
                );
                self.ptrace_restart(pid, None)?;
            }
            WaitStatus::PtraceSyscall(pid) => {
                assert!(
                    self.tracing,
                    "Can't receive WaitStatus::PtraceSyscall's\
                                       unless we've enabled tracing"
                );
                self.debug_print_syscall(pid)?;
                self.ptrace_restart(pid, None)?;
            }
            _ => unreachable!("{status:?}"),
        }

        Ok(DebuggerState::Running)
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
                let formatted_syscall = systrace::decode(tracee, nr, args);
                tracee.print_buf += &formatted_syscall;
                tracee.print_next_syscall = nr != libc::SYS_exit_group;
            }
            ptrace::SyscallInfoOp::Exit { ret_val, is_error } => {
                if !tracee.print_next_syscall {
                    return Ok(());
                }

                tracee.print_buf += " -> ";
                tracee.print_buf += &ret_val.to_string();

                if is_error == 1 {
                    let err = nix::Error::from_i32(-ret_val as i32);

                    tracee.print_buf += " ";
                    tracee.print_buf += &err.to_string();
                }

                log::trace!("[debugger::syscall] {}", tracee.print_buf);
                println!("{}", tracee.print_buf);

                tracee.print_buf.clear();
            }
            _ => {}
        }

        Ok(())
    }

    fn handle_cmd(&mut self, cmd: PTraceCommand) -> Result<(), Error> {
        match cmd {
            PTraceCommand::SetbreakPoint { addr } => {
                self.set_breakpoint(addr)?;
            }
            PTraceCommand::UnsetbreakPoint { addr } => {
                self.unset_breakpoint(addr)?;
            }
        }

        Ok(())
    }

    fn set_breakpoint(&mut self, addr: usize) -> Result<(), Error> {
        let tracee = self.tracees.root();
        let mut orig_bytes: u32 = 0;
        unsafe {
            ReadMemory::new(tracee).read(&mut orig_bytes, addr).apply()?;
        }
        println!("{:#X?}", orig_bytes.to_ne_bytes());

        let breakpoint = Breakpoint { orig_bytes };
        self.breakpoints.insert(addr, breakpoint);
        WriteMemory::new(tracee).write(&BP_INST, addr).apply()?;
        Ok(())
    }

    fn unset_breakpoint(&mut self, addr: usize) -> Result<(), Error> {
        let tracee = self.tracees.root();
        if let Some(breakpoint) = self.breakpoints.remove(&addr) {
            WriteMemory::new(tracee).write(&breakpoint.orig_bytes, addr).apply()?;
        }
        Ok(())
    }

    fn handle_breakpoint(&mut self, pid: Pid) -> Result<(), Error> {
        let tracee = self.tracees.get(pid)?;
        let regs = ptrace::getregs(pid)?;
        let rip = regs.rip as usize;

        // Breakpoint might be just an exec(..) call so we need to check for that.
        if let Some(breakpoint) = self.breakpoints.get(&(rip - 1)) {
            let new_regs = libc::user_regs_struct {
                rip: regs.rip - 1,
                ..regs
            };
            ptrace::setregs(pid, new_regs)?;
            WriteMemory::new(tracee).write(&breakpoint.orig_bytes, rip - 1).apply()?;
            ptrace::step(pid, None)?;
            WriteMemory::new(tracee).write(&BP_INST, rip - 1).apply()?;
        }
        Ok(())
    }

    fn ptrace_restart(&self, pid: Pid, signal: Option<Signal>) -> Result<(), nix::Error> {
        if self.tracing {
            ptrace::syscall(pid, signal)
        } else {
            ptrace::cont(pid, signal)
        }
    }
}

/// A process can only have threads as the result of a *clone* syscall with the
/// CLONE_THREAD flag.
fn clone_has_thread_flag(tracee: &Tracee) -> Result<bool, Error> {
    let regs = ptrace::getregs(tracee.tid)?;
    let syscall = regs.orig_rax as c_long;

    if syscall == libc::SYS_clone {
        let flags = CloneFlags::from_bits(regs.r10 as c_int).unwrap_or(CloneFlags::empty());
        if flags.contains(CloneFlags::CLONE_THREAD) {
            return Ok(true);
        }
    }

    if syscall == libc::SYS_clone3 {
        let clone_flags = unsafe {
            let mut clone_args = MaybeUninit::<libc::clone_args>::uninit();
            ReadMemory::new(tracee).read(&mut clone_args, regs.rdi as usize).apply()?;
            clone_args.assume_init().flags
        };

        let flags = CloneFlags::from_bits(clone_flags as c_int).unwrap_or(CloneFlags::empty());
        if flags.contains(CloneFlags::CLONE_THREAD) {
            return Ok(true);
        }
    }

    Ok(false)
}

#[derive(Debug)]
struct Breakpoint {
    orig_bytes: u32,
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

    /// Whether or not the previously ran syscall was a syscall that returned.
    print_next_syscall: bool,

    /// Buffer that holds a debug representation of a syscall, necessary as we need
    /// to store syscall info between entry and exit.
    print_buf: String,
}

impl Tracee {
    fn process(pid: Pid) -> Self {
        Self {
            pid,
            tid: pid,
            print_next_syscall: true,
            print_buf: String::new(),
        }
    }

    fn thread(pid: Pid, tid: Tid) -> Self {
        Self {
            pid,
            tid,
            print_next_syscall: true,
            print_buf: String::new(),
        }
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
                | ptrace::Options::PTRACE_O_TRACEEXEC
                | ptrace::Options::PTRACE_O_TRACESYSGOOD
                | ptrace::Options::PTRACE_O_EXITKILL,
        )
    }

    /// Opens a procfs process and reads it's associated memory maps.
    /// Should be up to date as it's re-read on each call.
    fn memory_maps(&self) -> Result<Vec<MemoryMap>, Error> {
        let tracee_proc = Process::new(self.pid.as_raw())?;
        let memory_maps = tracee_proc.maps().map(|map| map.0)?;
        Ok(memory_maps)
    }

    #[allow(dead_code)]
    fn kill(&self, signal: Signal) -> Result<(), nix::Error> {
        let res = unsafe {
            libc::syscall(
                libc::SYS_tgkill,
                self.pid.as_raw(),
                self.tid.as_raw(),
                signal,
            )
        };

        nix::Error::result(res).map(|_| ())
    }
}

impl fmt::Display for Tracee {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_thread() {
            f.write_fmt(format_args!(
                "Thread {{ pid: {}, tid: {} }}",
                self.pid, self.tid
            ))
        } else {
            f.write_fmt(format_args!("Process {{ pid: {} }}", self.pid))
        }
    }
}
