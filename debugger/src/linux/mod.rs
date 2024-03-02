use std::ffi::{c_void, CString};
use std::marker::PhantomData;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::sync::mpsc::{Receiver, Sender};
use std::sync::{Arc, MutexGuard, mpsc};

use nix::libc::{PTRACE_EVENT_CLONE, PTRACE_EVENT_FORK, PTRACE_EVENT_VFORK};
use nix::sys::ptrace;
use nix::sys::signal::Signal;
use nix::sys::wait::{waitid, waitpid, WaitPidFlag, WaitStatus};
use nix::unistd::{execvpe, fork, ForkResult};

use once_cell::sync::Lazy;
use procfs::process::MemoryMap;

use crate::collections::Tree;
use crate::{Debuggable, DebuggerDescriptor, DebuggerSettings, ExitCode, PhysAddr, VirtAddr};
use processor::Processor;

mod error;
mod ioctl;
mod tests;
mod trace;

pub type Tid = nix::unistd::Pid;
pub type Pid = nix::unistd::Pid;

pub enum Error {
    Unexpected,
    AlreadyAttached,
    InvalidPathName,
    PermissionDenied,
    ProcessLost(Pid),
    IncompleteRead(usize, usize),
    IncompleteWrite(usize, usize),
    Procfs(procfs::ProcError),
    Kernel(nix::errno::Errno),
}

fn procfs_process(pid: Pid) -> Result<procfs::process::Process, Error> {
    Ok(procfs::process::Process::new(pid.as_raw())?)
}

fn read_memory_maps(proc: &procfs::process::Process) -> Result<Vec<MemoryMap>, Error> {
    Ok(proc.maps().map(|map| map.0)?)
}

fn executable_name(path: &Path) -> String {
    path.file_name()
        .unwrap_or(std::ffi::OsStr::new("unknown"))
        .to_string_lossy()
        .into_owned()
}

/// Queued ptrace operations.
#[derive(Debug)]
pub enum Event {
    Kill,
    Pause,
    Continue,
    Attach,
    Detach,
}

/// A structure representing info related to a [`Pid`].
#[derive(Debug)]
pub struct Process {
    /// Unique process identifier.
    id: Pid,
    /// Mapping from [`PhysAddr`] to [`VirtAddr`].
    memory_maps: Vec<MemoryMap>,
    /// Name of the process, usually it's executable.
    ident: String,
    /// Whether or not the current syscall is `exit_group`.
    print_next_syscall: bool,
    /// Whether or not to print syscalls.
    tracing: bool,
    /// Whether or this is a process or thread.
    is_thread: bool,
}

impl Process {
    fn untraced(pid: Pid, ident: String, tracing: bool, is_thread: bool) -> Result<Self, Error> {
        Ok(Self {
            id: pid,
            memory_maps: read_memory_maps(&procfs_process(pid)?)?,
            ident,
            print_next_syscall: true,
            tracing,
            is_thread,
        })
    }

    fn configure(&mut self, remote: bool) -> Result<(), Error> {
        let mut options = ptrace::Options::empty()
            // new thread
            | ptrace::Options::PTRACE_O_TRACECLONE
            // new process
            | ptrace::Options::PTRACE_O_TRACEEXEC
            // new child
            | ptrace::Options::PTRACE_O_TRACEFORK
            // new child in same memory space
            | ptrace::Options::PTRACE_O_TRACEVFORK
            // distinguish regular SIGTRAP's from syscalls
            | ptrace::Options::PTRACE_O_TRACESYSGOOD;

        if !remote {
            // ensure tracee get's killed even if something goes wrong 
            options |= ptrace::Options::PTRACE_O_EXITKILL;
        }

        ptrace::setoptions(self.id, options)?;
        Ok(())
    }

    #[inline]
    pub fn id(&self) -> Pid {
        self.id
    }

    pub(crate) fn memory_maps(&self) -> impl Iterator<Item = &MemoryMap> {
        self.memory_maps.iter()
    }

    fn process_event(&mut self, event: Event) -> Result<(), Error> {
        match event {
            Event::Kill => self.kill()?,
            Event::Pause => self.pause()?,
            Event::Continue => self.kontinue(None)?,
            Event::Attach => self.attach()?,
            Event::Detach => self.detach()?,
        }

        Ok(())
    }

    fn attach(&mut self) -> Result<(), nix::Error> {
        ptrace::attach(self.id)
    }

    fn detach(&mut self) -> Result<(), nix::Error> {
        ptrace::detach(self.id, None)
    }

    fn kill(&mut self) -> Result<(), nix::Error> {
        ptrace::kill(self.id)
    }

    fn pause(&mut self) -> Result<(), nix::Error> {
        ptrace::interrupt(self.id)
    }

    fn kontinue(&mut self, signal: Option<Signal>) -> Result<(), nix::Error> {
        ptrace::syscall(self.id, signal)
    }

    pub fn virt_to_phys(&self, addr: VirtAddr) -> PhysAddr {
        for map in &self.memory_maps {
            let (start, end) = (map.address.0 as usize, map.address.1 as usize);
            if addr >= start && addr < end {
                let rva = addr - start;
                return map.offset as usize + rva;
            }
        }

        // failed to translate the address
        addr
    }

    pub fn trans(&self, addr: PhysAddr) -> VirtAddr {
        self.memory_maps()
            .find_map(|map| {
                let size = (map.address.1 - map.address.0) as usize;
                let start = map.offset as usize;
                let end = start + size;
                if addr >= start && addr < end {
                    Some(map.address.0 as usize + addr - start)
                } else {
                    None
                }
            })
            .unwrap_or(addr)
    }

    pub fn phys_to_virt(&self, addr: PhysAddr) -> VirtAddr {
        let addr = self
            .memory_maps
            .get(0)
            .map(|map| map.address.0 + addr as u64)
            .expect("Process somehow doesn't have any memory maps.");

        for map in &self.memory_maps {
            let (start, end) = (map.address.0, map.address.1);
            if addr >= start && addr < end {
                let rva = addr - start;
                return (map.offset + rva) as VirtAddr;
            }
        }

        // failed to translate the address
        addr as VirtAddr
    }
}

pub struct DebuggerImpl {
    /// Processes being traced.
    procs: Tree<Process>,
    /// Root process processor.
    module: Arc<Processor>,
    /// Whether or not the process should be killed on detaching.
    remote: bool,
    /// Whether or not to trace syscalls.
    tracing: bool,
    /// Whether or not syscall tracing should apply to children.
    follow_children: bool,
    /// Buffer for logging syscalls.
    print_buf: String,
    /// Exit code of most recently exited process.
    exit_code: ExitCode,
    /// Prevent [`Debugger`] from implementing Send.
    #[doc(hidden)]
    _not_send: PhantomData<*mut c_void>,
}

impl DebuggerImpl {
    fn new<S: Into<Vec<u8>>>(settings: &DebuggerSettings<S>, desc: &DebuggerDescriptor<S>) -> Self {
        assert!(
            !(!settings.tracing && settings.follow_children),
            "Can't enable 'follow_children' without tracing."
        );

        DebuggerImpl {
            procs: Tree::new(),
            module: Arc::clone(&desc.module),
            remote: false,
            print_buf: String::new(),
            tracing: settings.tracing,
            follow_children: settings.follow_children,
            exit_code: 0,
            _not_send: PhantomData,
        }
    }

    fn spawn<S: Into<Vec<u8>>>(
        &mut self,
        settings: &mut DebuggerSettings<S>,
        desc: &mut DebuggerDescriptor<S>,
    ) -> Result<(), Error> {
        if !self.procs.is_empty() {
            return Err(Error::AlreadyAttached);
        }

        let c_path = CString::new(desc.module.path.as_os_str().as_bytes())
            .map_err(|_| Error::InvalidPathName)?;

        let args = std::mem::take(&mut desc.args);
        let mut c_args = Vec::with_capacity(desc.args.len() + 1);

        // push program as first argument
        c_args.push(c_path.clone());

        // push arguments
        for arg in args {
            let arg = CString::new(arg).map_err(|_| Error::InvalidPathName)?;
            c_args.push(arg);
        }

        let mut exec_child = || {
            // signal child process to be traced
            ptrace::traceme()?;

            // stop child
            nix::sys::signal::raise(Signal::SIGSTOP)?;

            // inherit environmental variables
            let env = std::mem::take(&mut settings.env);
            let c_env: Vec<CString> = std::env::vars()
                .flat_map(|(key, val)| CString::new(format!("{key}={val}")))
                .chain(env.into_iter().flat_map(CString::new))
                .collect();

            // execute program
            execvpe(&c_path, &c_args, &c_env)
        };

        match unsafe { fork()? } {
            ForkResult::Parent { child } => {
                let ident = executable_name(&desc.module.path);
                let mut proc = Process::untraced(child, ident, self.tracing, false)?;
                proc.configure(self.remote)?;
                self.procs.push_root(child, proc);

                Ok(())
            }
            ForkResult::Child => {
                let _ = exec_child();

                // child may never continue execution on failure
                unsafe { nix::libc::_exit(1) }
            }
        }
    }

    fn attach<S: Into<Vec<u8>>>(
        &mut self,
        pid: Pid,
        desc: &DebuggerDescriptor<S>,
    ) -> Result<(), Error> {
        if !self.procs.is_empty() {
            return Err(Error::AlreadyAttached);
        }

        let ident = executable_name(&desc.module.path);
        let mut proc = Process::untraced(pid, ident, self.tracing, false)?;
        proc.attach()?;
        proc.configure(self.remote)?;

        self.procs.push_root(pid, proc);
        self.remote = true;
        Ok(())
    }

    fn view<S: Into<Vec<u8>>>(
        &mut self,
        pid: Pid,
        desc: &DebuggerDescriptor<S>,
    ) -> Result<(), Error> {
        if !self.procs.is_empty() {
            return Err(Error::AlreadyAttached);
        }

        let ident = executable_name(&desc.module.path);
        let mut proc = Process::untraced(pid, ident, self.tracing, false)?;
        proc.configure(self.remote)?;

        self.procs.push_root(pid, proc);
        self.remote = true;
        Ok(())
    }

    fn process_status(&mut self, status: WaitStatus) -> Result<(), Error> {
        match status {
            WaitStatus::Stopped(pid, Signal::SIGTRAP) => {
                println!("hit breakpoint");

                // discard the trap signal
                let proc = self.procs.get_mut(pid)?;
                proc.kontinue(None)?;
            }
            WaitStatus::Stopped(pid, signal) => {
                let proc = self.procs.get_mut(pid)?;
                proc.kontinue(Some(signal))?;
            }
            WaitStatus::Signaled(child, signal, ..) => {
                self.procs.remove(child);
                log::complex!(
                    w "[debugger::event] child ",
                    g child.to_string(),
                    w " exited by signal ",
                    y signal.to_string(),
                    w "."
                );
            }
            WaitStatus::Exited(child, code) => {
                self.exit_code = code;
                self.procs.remove(child);
                log::complex!(
                    w "[debugger::event] child ",
                    g child.to_string(),
                    w " exited with code ",
                    y code.to_string(),
                    w "."
                );
            }
            WaitStatus::PtraceSyscall(pid) => {
                self.process_syscall(pid)?;

                let proc = self.procs.get_mut(pid)?;
                proc.kontinue(None)?;
            }
            WaitStatus::PtraceEvent(pid, _, event) => {
                // if a new child was created, store it as the child of it's parent
                if event == PTRACE_EVENT_FORK
                    || event == PTRACE_EVENT_VFORK
                    || event == PTRACE_EVENT_CLONE
                {
                    let child = ptrace::getevent(pid)?;
                    let child = nix::unistd::Pid::from_raw(child as i32);

                    match event {
                        PTRACE_EVENT_FORK => {
                            log::complex!(
                                w "[debugger::event] process ",
                                g child.to_string(),
                                w " created using fork."
                            );
                        }
                        PTRACE_EVENT_VFORK => {
                            log::complex!(
                                w "[debugger::event] process ",
                                g child.to_string(),
                                w " created using vfork."
                            );
                        }
                        PTRACE_EVENT_CLONE => {
                            log::complex!(
                                w "[debugger::event] thread ",
                                g child.to_string(),
                                w " created using clone."
                            );
                        }
                        _ => {}
                    }

                    let parent_proc = self.procs.get_mut(pid)?;
                    let ident = parent_proc.ident.clone();

                    let mut proc = Process::untraced(
                        child,
                        ident,
                        self.follow_children,
                        event == PTRACE_EVENT_CLONE,
                    )?;
                    proc.configure(self.remote)?;
                    proc.kontinue(None)?;

                    self.procs.push_child(pid, child, proc);
                }

                // discard SIGTRAP generated by `PtraceEvent`
                let parent_proc = self.procs.get_mut(pid)?;
                parent_proc.kontinue(None)?;
            }
            _ => unreachable!("{status:?}"),
        }

        Ok(())
    }

    fn process_syscall(&mut self, pid: Pid) -> Result<(), Error> {
        let proc = self.procs.get_mut(pid)?;

        match ptrace::getsyscallinfo(pid)?.op {
            ptrace::SyscallInfoOp::Entry { nr, args } => {
                let sysno = trace::Sysno::from(nr as i32);

                if sysno == trace::Sysno::execveat {
                    let dirfd = args[0];
                    if let Ok(mut path) = std::fs::read_link(format!("/proc/{pid}/fd/{dirfd}")) {
                        let relative = trace::read_c_str(proc, args[1]);
                        path.push(relative);
                        proc.ident = executable_name(&path);
                    }
                }

                // update executable name as we've now replaced the process
                if sysno == trace::Sysno::execve {
                    let path = PathBuf::from(trace::read_c_str(proc, args[0]));
                    proc.ident = executable_name(&path);
                }

                if proc.tracing {
                    self.print_buf += &proc.display(sysno, args);
                    proc.print_next_syscall = nr != trace::Sysno::exit_group as u64;
                }
            }
            ptrace::SyscallInfoOp::Exit { ret_val, is_error } => {
                // check condition for logging syscall
                if proc.tracing && proc.print_next_syscall {
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
            }
            _ => {}
        }

        Ok(())
    }

    fn end_processes(&mut self) {
        for proc in self.procs.values_mut() {
            if proc.is_thread {
                continue;
            }

            if self.remote {
                let _ = proc.detach();
            } else {
                let _ = proc.kill();
            }
        }
    }
}

#[derive(Clone)]
pub struct Debugger<S: Into<Vec<u8>>> {
    /// Feature flags essentially.
    settings: DebuggerSettings<S>,
    /// Options related to creating a [`Debugger`].
    desc: DebuggerDescriptor<S>,
    /// Most of the debugging logic.
    inner: Arc<Mutex<DebuggerImpl>>,

    event_recv: Arc<Receiver<(Pid, Event)>>,
    event_sendr: Arc<Sender<(Pid, Event)>>,

    result_recv: Arc<Receiver<Result<(), Error>>>,
    result_sendr: Arc<Sender<Result<(), Error>>>,

    // Ptrace operations that are queued to be run on the tracing thread.
    // Required as ptrace requires all messages to come from the tracing thread.
    // Results received from operations run by the tracing thread.
}

unsafe impl<S: Into<Vec<u8>>> Send for Debugger<S> {}

impl<S: Into<Vec<u8>>> Debugger<S> {
    #[inline]
    pub fn lock(&self) -> MutexGuard<DebuggerImpl> {
        self.inner.lock().unwrap()
    }

    pub fn processes(&self) -> Vec<Pid> {
        self.lock()
            .procs
            .values()
            .filter(|proc| !proc.is_thread)
            .map(|proc| proc.id)
            .collect()
    }

    pub fn notify(&self, pid: Pid, event: Event) -> Result<(), Error> {
        self.event_sendr.send((pid, event)).map_err(|_| Error::Unexpected)?;
        self.result_recv.recv().map_err(|_| Error::Unexpected)?
    }

    fn event_loop(&self) -> Result<(), Error> {
        loop {
            // wait for any child to emit a message
            let mut status = waitid(
                nix::sys::wait::Id::All,
                WaitPidFlag::WEXITED | WaitPidFlag::WSTOPPED | WaitPidFlag::WNOWAIT,
            )?;

            let mut debugger = self.lock();

            // ensure we have a consistent view of the process
            if let Some(pid) = status.pid() {
                assert!(debugger.procs.get(pid).is_ok(), "Unknown pid {pid} occured");
                status = waitpid(pid, Some(WaitPidFlag::WSTOPPED))?;
            }

            debugger.process_status(status)?;

            // process [`Event`]'s
            while let Ok((pid, event)) = self.event_recv.try_recv() {
                let proc = debugger.procs.get_mut(pid)?;
                let result = proc.process_event(event);
                let _ = self.result_sendr.send(result);
            }

            // the process has exited
            if debugger.procs.is_empty() {
                return Ok(());
            }
        }
    }
}

impl<S: Into<Vec<u8>>> Debuggable<S> for Debugger<S>
where
    Self: Sized,
{
    fn me() -> Self {
        static MODULE: Lazy<Arc<Processor>> = Lazy::new(|| {
            let proc = procfs::process::Process::myself().unwrap();
            let path = proc.exe().unwrap();
            Arc::new(Processor::parse(path).unwrap())
        });

        let settings = DebuggerSettings {
            tracing: false,
            follow_children: false,
            env: Vec::new(),
        };
        let desc = DebuggerDescriptor {
            args: Vec::new(),
            module: Arc::clone(&MODULE),
        };
        Self::new(settings, desc)
    }

    fn new(settings: DebuggerSettings<S>, desc: DebuggerDescriptor<S>) -> Self {
        let inner = Arc::new(Mutex::new(DebuggerImpl::new(&settings, &desc)));
        let (event_sendr, event_recv) = mpsc::channel();
        let (result_sendr, result_recv) = mpsc::channel();

        Self {
            settings,
            desc,
            inner,
            event_recv: Arc::new(event_recv),
            event_sendr: Arc::new(event_sendr),
            result_recv: Arc::new(result_recv),
            result_sendr: Arc::new(result_sendr),
        }
    }

    fn spawn(&mut self) -> Result<(), Error> {
        self.inner.lock().unwrap().spawn(&mut self.settings, &mut self.desc)
    }

    fn attach(&mut self, pid: Pid) -> Result<(), Error> {
        self.inner.lock().unwrap().attach(pid, &self.desc)
    }

    fn view(&mut self, pid: Pid) -> Result<(), Error> {
        self.inner.lock().unwrap().view(pid, &self.desc)
    }

    fn trace(&mut self) -> Result<ExitCode, Error> {
        let result = self.event_loop();
        let mut debugger = self.lock();
        if result.is_err() {
            debugger.end_processes();
        }

        result.map(|_| debugger.exit_code)
    }
}
