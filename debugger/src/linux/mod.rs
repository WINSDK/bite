use std::ffi::CString;
use std::marker::PhantomData;
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;
use std::sync::Arc;

use nix::sys::signal::Signal;
use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};
use nix::sys::{personality, ptrace};
use nix::unistd::{execvpe, fork, ForkResult};
use procfs::process::MemoryMap;

use crate::{
    collections::Tree, BreakpointOp, Context, DebugeeEvent, Debuggable, DebuggerEvent, ExitCode,
    PhysAddr, Tracing, VirtAddr,
};
use disassembler::Processor;

mod fmt;
mod ioctl;
mod tests;
mod trace;

pub type Tid = nix::unistd::Pid;
pub type Pid = nix::unistd::Pid;

#[derive(PartialEq)]
pub enum Error {
    InvalidPathName,
    PermissionDenied,
    ProcessLost(Pid),
    IncompleteRead(usize, usize),
    IncompleteWrite(usize, usize),
    Kernel(nix::errno::Errno),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum State {
    /// Nothing.
    Running,
    /// Tracing options have yet to been set.
    WaitingForInit,
}

fn procfs_process(pid: Pid) -> Result<procfs::process::Process, Error> {
    match procfs::process::Process::new(pid.as_raw()) {
        Err(procfs::ProcError::PermissionDenied(_)) => return Err(Error::PermissionDenied),
        Err(procfs::ProcError::NotFound(_)) => return Err(Error::ProcessLost(pid)),
        Err(..) => unreachable!(),
        Ok(proc) => Ok(proc),
    }
}

fn read_memory_maps(proc: &procfs::process::Process) -> Result<Vec<MemoryMap>, Error> {
    Ok(proc
        .maps()
        .unwrap_or_else(|_| panic!("Failed to open memory map of {proc:?}"))
        .0)
}

pub struct Process {
    id: Pid,
    module: Arc<Processor>,
    state: State,
    unprocessed_signal: Option<Signal>,
    print_next_syscall: bool,
    tracing: bool,
    memory_maps: Vec<MemoryMap>,
}

impl Process {
    fn stopped(pid: Pid, module: Arc<Processor>) -> Result<Self, Error> {
        Ok(Self {
            id: pid,
            module,
            state: State::WaitingForInit,
            unprocessed_signal: None,
            print_next_syscall: true,
            tracing: false,
            memory_maps: read_memory_maps(&procfs_process(pid)?)?,
        })
    }

    /// Convert disassembler's address to virtual memory address.
    pub fn translate(&self, addr: PhysAddr) -> VirtAddr {
        for map in &self.memory_maps {
            let size = (map.address.1 - map.address.0) as usize;
            if addr >= map.offset as usize && addr < map.offset as usize + size {
                let rva = addr - map.offset as usize;
                return map.address.0 as usize + rva;
            }
        }

        // failed to translate the address
        addr
    }

    fn configure(&mut self) -> Result<(), Error> {
        let options = ptrace::Options::empty()
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

        ptrace::setoptions(self.id, options).map_err(Error::Kernel)?;
        self.state = State::Running;

        Ok(())
    }
}

impl Tracing for Process {
    fn attach(&mut self) {
        let _ = ptrace::attach(self.id);
    }
    fn detach(&mut self) {
        // ignore the result since detaching can't fail
        let _ = ptrace::detach(self.id, None);
    }

    fn kill(&mut self) {
        // ignore the result since killing a process can't fail
        let _ = ptrace::kill(self.id);
    }

    fn pause(&self) {
        // ignore the result since it appears to be unlikely interrupting can fail
        //
        // https://github.com/torvalds/linux/blob/d528014517f2b0531862c02865b9d4c908019dc4/kernel/ptrace.c#L1137
        let _ = ptrace::interrupt(self.id);
    }

    fn kontinue(&mut self) {
        let sig = std::mem::take(&mut self.unprocessed_signal);

        // ignore the result since continuing a process can't fail
        let _ = ptrace::syscall(self.id, sig).unwrap();
    }

    fn read_memory(&self, addr: VirtAddr, len: usize) -> Result<Vec<u8>, Error> {
        // create buffer of that can hold `len` elements
        let mut buf: Vec<u8> = Vec::with_capacity(len);
        let uninit = unsafe { std::slice::from_raw_parts_mut(buf.as_mut_ptr(), len) };

        let local = std::io::IoSliceMut::new(uninit);
        let remote = nix::sys::uio::RemoteIoVec { base: addr, len };

        let bytes_read = nix::sys::uio::process_vm_readv(self.id, &mut [local], &[remote])
            .map_err(Error::Kernel)?;

        if len != bytes_read {
            return Err(Error::IncompleteRead(len, bytes_read));
        }

        // set buffer size to `len` iff `len` number of bytes have been read
        unsafe { buf.set_len(len) }
        Ok(buf)
    }

    fn write_memory(&mut self, addr: VirtAddr, data: &[u8]) -> Result<(), Error> {
        let local = std::io::IoSlice::new(data);
        let remote = nix::sys::uio::RemoteIoVec {
            base: addr,
            len: data.len(),
        };

        let bytes_wrote = nix::sys::uio::process_vm_writev(self.id, &[local], &[remote])
            .map_err(Error::Kernel)?;

        if data.len() != bytes_wrote {
            return Err(Error::IncompleteWrite(data.len(), bytes_wrote));
        }

        Ok(())
    }
}

/// Optional settings related to creating a [`Debugger`].
#[derive(Clone)]
pub struct DebuggerSettings<S: Into<Vec<u8>>> {
    /// Environmental variables set for the target.
    pub env: Vec<S>,

    /// Whether or not to trace syscalls.
    pub tracing: bool,

    /// Whether or not syscall tracing should apply to children.
    pub follow_children: bool,
}

/// Required options related to creating a [`Debugger`].
#[derive(Clone)]
pub struct DebuggerDescriptor<S: Into<Vec<u8>>> {
    /// Process arguments.
    pub args: Vec<S>,

    /// Processes that have been disassembled.
    pub module: Arc<Processor>,
}

pub struct Debugger {
    /// Processes being traced.
    procs: Tree<Process>,

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
    _not_send: PhantomData<*mut ()>,
}

impl Debugger {
    fn new<S: Into<Vec<u8>>>(
        root: Pid,
        settings: DebuggerSettings<S>,
        desc: DebuggerDescriptor<S>,
    ) -> Result<Self, Error> {
        assert!(
            !(!settings.tracing && settings.follow_children),
            "Can't enable 'follow_children' without tracing."
        );

        Ok(Debugger {
            procs: Tree::new(root, Process::stopped(root, desc.module)?),
            remote: false,
            print_buf: String::new(),
            tracing: settings.tracing,
            follow_children: settings.follow_children,
            exit_code: 0,
            _not_send: PhantomData,
        })
    }

    fn parse_module_or_reuse(&self, path: PathBuf) -> Option<Arc<Processor>> {
        let full_path = path.canonicalize().ok();
        let proc_with_module = self
            .procs
            .values()
            .find(|proc| proc.module.path.canonicalize().ok() == full_path);

        match proc_with_module {
            Some(proc) => Some(Arc::clone(&proc.module)),
            None => Processor::parse_unknown(path).ok().map(Arc::new),
        }
    }

    fn handle_ptrace_status(&mut self, status: WaitStatus) -> Result<(), Error> {
        let pid = status.pid().unwrap();
        let proc = self.procs.get_mut(pid)?;

        if proc.state == State::WaitingForInit {
            proc.configure()?;
        }

        self.process_event(status)
    }

    fn handle_syscall(&mut self, pid: Pid) -> Result<(), Error> {
        let syscall = ptrace::getsyscallinfo(pid).map_err(Error::Kernel)?;

        match syscall.op {
            ptrace::SyscallInfoOp::Entry { nr, args } => {
                let sysno = trace::Sysno::from(nr as i32);

                if sysno == trace::Sysno::execveat {
                    let proc = self.procs.get(pid)?;

                    let dirfd = args[0];
                    if let Ok(mut path) = std::fs::read_link(format!("/proc/{pid}/fd/{dirfd}")) {
                        let relative = trace::read_c_str_slow(proc, args[1]);
                        path.push(relative);

                        if let Some(module) = self.parse_module_or_reuse(path) {
                            self.procs.get_mut(pid)?.module = module
                        }
                    }
                }

                if sysno == trace::Sysno::execve {
                    let proc = self.procs.get(pid)?;
                    let path = PathBuf::from(trace::read_c_str_slow(proc, args[0]));

                    if let Some(module) = self.parse_module_or_reuse(path) {
                        self.procs.get_mut(pid)?.module = module;
                    }
                }

                let proc = self.procs.get_mut(pid)?;
                if proc.tracing {
                    self.print_buf += &proc.display(sysno, args);
                    proc.print_next_syscall = nr != trace::Sysno::exit_group as u64;
                }

                proc.kontinue();
            }
            ptrace::SyscallInfoOp::Exit { ret_val, is_error } => {
                let proc = self.procs.get_mut(pid)?;

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

                proc.kontinue();
            }
            _ => {}
        }

        Ok(())
    }

    fn process_event(&mut self, status: WaitStatus) -> Result<(), Error> {
        match status {
            WaitStatus::Stopped(pid, signal) => {
                let proc = self.procs.get_mut(pid)?;
                proc.unprocessed_signal = Some(signal);
                proc.kontinue();
            }
            WaitStatus::Signaled(pid, signal, ..) => {
                self.procs.remove(pid);
                log::trace!("[debugger::event] child exited by signal: {signal:?}.");
            }
            WaitStatus::Exited(pid, code) => {
                self.exit_code = code;
                self.procs.remove(pid);
                log::trace!("[debugger::event] child {pid} exited with code {code}.");
            }
            WaitStatus::PtraceSyscall(pid) => self.handle_syscall(pid)?,
            WaitStatus::PtraceEvent(pid, _, event) => {
                use nix::libc::*;

                // if a new child was created, store it as the child of it's parent
                if event == PTRACE_EVENT_FORK || event == PTRACE_EVENT_VFORK {
                    let created_pid = ptrace::getevent(pid).map_err(Error::Kernel)?;
                    let created_pid = nix::unistd::Pid::from_raw(created_pid as i32);

                    // find module and add it to process
                    let module = Arc::clone(&self.procs.get(pid)?.module);
                    let mut proc = Process::stopped(created_pid, module)?;

                    proc.tracing = self.follow_children;
                    proc.attach();
                    proc.configure()?;
                    proc.kontinue();

                    self.procs.push_child(&pid, created_pid, proc);
                }

                let proc = self.procs.get_mut(pid)?;
                proc.kontinue();
            }
            _ => unreachable!("{status:?}"),
        }

        Ok(())
    }

    fn end_processes(&mut self) {
        for proc in self.procs.values_mut() {
            if self.remote {
                proc.detach();
            } else {
                proc.kill();
            }
        }
    }

    fn event_loop(&mut self, ctx: Arc<Context>) -> Result<(), Error> {
        'event_loop: loop {
            // check if there are still processes
            if self.procs.is_empty() {
                break;
            }

            while let Some(event) = ctx.queue.popd() {
                match event {
                    DebugeeEvent::Break => {
                        for proc in self.procs.values() {
                            proc.pause();
                        }
                    }
                    DebugeeEvent::Continue => {
                        for proc in self.procs.values_mut() {
                            proc.kontinue();
                        }
                    }
                    DebugeeEvent::Exit => {
                        self.end_processes();
                        ctx.breakpoints.write().unwrap().clear();
                        break 'event_loop;
                    }
                }
            }

            let breakpoints = &mut ctx.breakpoints.write().unwrap();
            let mut up_for_removal = Vec::new();
            for bp in breakpoints.values_mut() {
                match bp.op.take() {
                    Some(BreakpointOp::Create) => {
                        bp.set(self.procs.root())?;
                    }
                    Some(BreakpointOp::Delete) => {
                        bp.unset(self.procs.root())?;
                        up_for_removal.push(bp.addr);
                    }
                    None => {}
                }
            }

            for addr in up_for_removal {
                breakpoints.mapping.remove(&addr);
            }

            // wait for child to give send a message
            let pids: Vec<Pid> = self.procs.values().map(|proc| proc.id).collect();
            for pid in pids {
                let status = waitpid(pid, Some(WaitPidFlag::WNOHANG | WaitPidFlag::WSTOPPED))
                    .map_err(Error::Kernel)?;

                if status == WaitStatus::StillAlive {
                    continue;
                }

                self.handle_ptrace_status(status)?;
            }

            std::thread::sleep(std::time::Duration::from_nanos(10));
        }

        Ok(())
    }
}

impl Debuggable for Debugger {
    fn spawn<S: Into<Vec<u8>>>(
        mut settings: DebuggerSettings<S>,
        mut desc: DebuggerDescriptor<S>,
    ) -> Result<Self, Error> {
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
            // disable ASLR
            personality::set(personality::Persona::ADDR_NO_RANDOMIZE)?;

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

        match unsafe { fork().map_err(Error::Kernel)? } {
            ForkResult::Parent { child } => Debugger::new(child, settings, desc),
            ForkResult::Child => {
                let _ = exec_child();

                // child may never continue execution on failure
                unsafe { nix::libc::_exit(1) }
            }
        }
    }

    fn attach<S: Into<Vec<u8>>>(
        pid: Pid,
        settings: DebuggerSettings<S>,
        desc: DebuggerDescriptor<S>,
    ) -> Result<Self, Error> {
        ptrace::attach(pid).map_err(Error::Kernel)?;
        let mut debugger = Debugger::new(pid, settings, desc)?;
        debugger.remote = true;
        Ok(debugger)
    }

    fn run(mut self, ctx: Arc<Context>) -> Result<(), Error> {
        ctx.attach();

        if self.tracing {
            self.procs.root().tracing = true;
        }

        let result = self.event_loop(ctx.clone());

        if result.is_err() {
            self.end_processes();
        }

        ctx.breakpoints.write().unwrap().clear();
        ctx.queue.pushd(DebuggerEvent::Exited(self.exit_code));
        ctx.deattach();
        result
    }
}
