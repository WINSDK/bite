use std::ffi::CString;
use std::marker::PhantomData;
use std::os::unix::ffi::OsStrExt;
use std::sync::Arc;

use nix::sys::signal::Signal;
use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};
use nix::sys::{personality, ptrace};
use nix::unistd::{execvp, fork, ForkResult};
use procfs::process::MemoryMap;

use crate::{
    collections::Tree, Addr, BreakpointOp, Context, DebugeeEvent, Debuggable, DebuggerEvent,
    ExitCode, Rva, Tracing,
};

mod fmt;
mod ioctl;
mod tests;
mod trace;

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

#[derive(Debug, Clone, Copy)]
enum State {
    WaitingForInit,
    Running,
}

fn read_memory_maps(pid: Pid) -> Result<Vec<MemoryMap>, Error> {
    let proc = match procfs::process::Process::new(pid.as_raw()) {
        Err(procfs::ProcError::PermissionDenied(_)) => return Err(Error::PermissionDenied),
        Err(procfs::ProcError::NotFound(_)) => return Err(Error::ProcessLost(pid)),
        Err(..) => unreachable!(),
        Ok(proc) => proc,
    };

    Ok(proc.maps().unwrap_or_else(|_| panic!("Failed to open memory map on {pid}")).0)
}

#[derive(Debug)]
pub struct Process {
    id: Pid,
    state: State,
    memory_maps: Vec<MemoryMap>,
    unprocessed_signal: Option<Signal>,
    last_syscall: ptrace::SyscallInfoOp,
    tracing: bool,
}

impl Process {
    fn stopped(pid: Pid) -> Result<Self, Error> {
        Ok(Self {
            id: pid,
            state: State::WaitingForInit,
            memory_maps: read_memory_maps(pid)?,
            unprocessed_signal: None,
            last_syscall: ptrace::SyscallInfoOp::None,
            tracing: false,
        })
    }

    /// Convert disassembler's address to virtual memory address.
    pub fn translate(&self, rva: Rva) -> Addr {
        let mut base = 0;

        for map in &self.memory_maps {
            if map.address.0 > rva as u64 {
                base = map.address.1;
                break;
            }

            base = map.address.0;
        }

        rva.wrapping_add(base as usize)
    }

    fn configure(&mut self) -> Result<(), Error> {
        let mut options = ptrace::Options::empty();

        if self.tracing {
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

        ptrace::setoptions(self.id, options).map_err(Error::Kernel)?;
        self.state = State::Running;

        Ok(())
    }
}

impl Tracing for Process {
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
        let cont = if self.tracing {
            ptrace::syscall
        } else {
            ptrace::cont
        };

        let _ = cont(self.id, sig);
    }

    fn read_memory(&self, addr: Rva, len: usize) -> Result<Vec<u8>, Error> {
        let base = self.translate(addr);

        // create buffer of that can hold `len` elements
        let mut buf: Vec<u8> = Vec::with_capacity(len);
        let uninit = unsafe { std::slice::from_raw_parts_mut(buf.as_mut_ptr(), len) };

        let local = std::io::IoSliceMut::new(uninit);
        let remote = nix::sys::uio::RemoteIoVec { base, len };

        let bytes_read = nix::sys::uio::process_vm_readv(self.id, &mut [local], &[remote])
            .map_err(Error::Kernel)?;

        if len != bytes_read {
            return Err(Error::IncompleteRead(len, bytes_read));
        }

        // set buffer size to `len` iff `len` number of bytes have been read
        unsafe { buf.set_len(len) }
        Ok(buf)
    }

    fn write_memory(&mut self, addr: Rva, data: &[u8]) -> Result<(), Error> {
        let base = self.translate(addr);

        let local = std::io::IoSlice::new(data);
        let remote = nix::sys::uio::RemoteIoVec {
            base,
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

pub struct Debugger {
    procs: Tree<Pid, Process>,
    remote: bool,
    print_buf: String,

    /// Exit code of most recently exited process.
    exit_code: ExitCode,

    /// Prevent [`Debugger`] from implementing Send.
    #[doc(hidden)]
    _not_send: PhantomData<*mut ()>,
}

impl Debugger {
    fn local(root: Pid) -> Result<Self, Error> {
        Ok(Debugger {
            procs: Tree::new(root, Process::stopped(root)?),
            remote: false,
            print_buf: String::new(),
            exit_code: 0,
            _not_send: PhantomData,
        })
    }

    fn remote(root: Pid) -> Result<Self, Error> {
        let mut this = Debugger::local(root)?;
        this.remote = true;
        Ok(this)
    }

    pub fn enable_tracing(&mut self) {
        self.procs.root().tracing = true;
    }

    fn handle_ptrace_status(&mut self, status: WaitStatus) -> Result<(), Error> {
        let pid = status.pid().unwrap();
        let proc = self.procs.get(&pid).unwrap();

        match proc.state {
            State::WaitingForInit => {
                proc.configure()?;
                proc.kontinue();
            }
            State::Running => self.process_event(status)?,
        }

        Ok(())
    }

    fn process_event(&mut self, status: WaitStatus) -> Result<(), Error> {
        match status {
            WaitStatus::Stopped(pid, signal) => {
                let proc = self.procs.get(&pid).unwrap();
                proc.unprocessed_signal = Some(signal);
                proc.kontinue();
            }
            WaitStatus::Signaled(pid, signal, ..) => {
                self.procs.remove(&pid);
                log::trace!("[debugger::event] child exited by signal: {signal:?}.");
            }
            WaitStatus::Exited(pid, code) => {
                self.exit_code = code;
                self.procs.remove(&pid);
                log::trace!("[debugger::event] child {pid} exited with code {code}.");
            }
            WaitStatus::PtraceSyscall(pid) => {
                let proc = self.procs.get(&pid).unwrap();
                let syscall = ptrace::getsyscallinfo(pid).map_err(Error::Kernel)?;

                match syscall.op {
                    ptrace::SyscallInfoOp::Entry { nr, args } => {
                        let sysno = trace::Sysno::from(nr as i32);
                        let func = proc.display(sysno, args);

                        //if sysno == trace::Sysno::exit_group {
                        self.print_buf += &func;
                        proc.last_syscall = syscall.op;
                    }
                    ptrace::SyscallInfoOp::Exit { ret_val, is_error } => {
                        const EXIT: u64 = trace::Sysno::exit_group as u64;

                        // `exit_group` syscall doesn't have a return value
                        if let ptrace::SyscallInfoOp::Entry { nr: EXIT, .. } = proc.last_syscall {
                            proc.kontinue();
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

                proc.kontinue();
            }
            WaitStatus::PtraceEvent(pid, _, event) => {
                use nix::libc::*;

                // if a new pid was created, store it as the child of it's parent
                if let PTRACE_EVENT_FORK | PTRACE_EVENT_VFORK | PTRACE_EVENT_CLONE = event {
                    let created_pid = ptrace::getevent(pid).map_err(Error::Kernel)?;
                    let created_pid = nix::unistd::Pid::from_raw(created_pid as i32);

                    let mut proc = Process::stopped(created_pid)?;
                    proc.kontinue();
                    self.procs.push_child(&pid, created_pid, proc);
                }

                let proc = self.procs.get(&pid).unwrap();
                proc.kontinue();
            }
            _ => unreachable!("{status:?}"),
        }

        Ok(())
    }

    fn end_processes(&mut self) {
        for (_, proc) in self.procs.iter_mut() {
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
                    DebugeeEvent::Exit => {
                        self.end_processes();
                        break 'event_loop;
                    }
                }
            }

            let breakpoints = &mut ctx.breakpoints.write().unwrap();
            let mut up_for_removal = Vec::new();
            for bp in breakpoints.values_mut() {
                match bp.op.take() {
                    Some(BreakpointOp::Create) => {
                        let root_proc = self.procs.root();
                        bp.set(root_proc)?;
                        ctx.queue.pushd(DebuggerEvent::BreakpointSet(bp.addr));
                    }
                    Some(BreakpointOp::Delete) => {
                        let root_proc = self.procs.root();
                        bp.unset(root_proc)?;
                        up_for_removal.push(bp.addr);
                    }
                    None => {}
                }
            }

            for addr in up_for_removal {
                breakpoints.remove(addr);
            }

            // wait for child to give send a message
            let pids: Vec<Pid> = self.procs.iter().map(|(pid, _)| *pid).collect();
            for pid in pids {
                let status = waitpid(
                    pid,
                    Some(WaitPidFlag::WNOHANG | WaitPidFlag::WSTOPPED | WaitPidFlag::WCONTINUED),
                )
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
    fn spawn<P: AsRef<std::path::Path>, A: Into<Vec<u8>>>(
        path: P,
        args: Vec<A>,
    ) -> Result<Self, Error> {
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

        let exec_child = || {
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
            ForkResult::Parent { child } => Debugger::local(child),
            ForkResult::Child => {
                let _ = exec_child();

                // child may never continue execution on failure
                unsafe { nix::libc::_exit(1) }
            }
        }
    }

    fn attach(pid: Pid) -> Result<Self, Error> {
        ptrace::attach(pid).map_err(Error::Kernel)?;
        Debugger::remote(pid)
    }

    fn run(mut self, ctx: Arc<Context>) -> Result<(), Error> {
        ctx.attach();
        let result = self.event_loop(ctx.clone());

        if result.is_err() {
            self.end_processes();
        }

        ctx.queue.pushd(DebuggerEvent::Exited(self.exit_code));
        ctx.deattach();
        result
    }
}
