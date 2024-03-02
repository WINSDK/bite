use processor::Processor;
use processor_shared::{PhysAddr, VirtAddr};
use std::collections::BTreeMap;
use std::sync::Arc;

mod collections;
pub mod memory;
pub mod readmem;
pub mod writemem;

#[cfg(target_os = "linux")]
mod linux;

#[cfg(target_os = "macos")]
mod macos;

#[cfg(target_os = "windows")]
mod windows;

#[cfg(target_os = "linux")]
pub use linux::*;

#[cfg(target_os = "macos")]
pub use macos::*;

#[cfg(target_os = "windows")]
pub use windows::*;

pub use readmem::ReadMemory;
pub use writemem::WriteMemory;

pub type ExitCode = i32;

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

/// Behaviour related to creating starting a debug session.
pub trait Debuggable<S: Into<Vec<u8>>>
where
    Self: Sized,
{
    fn me() -> Self;

    fn new(settings: DebuggerSettings<S>, desc: DebuggerDescriptor<S>) -> Self;

    /// Creates a `Tracee`, debugging a newly launched process.
    fn spawn(&mut self) -> Result<(), Error>;

    /// Creates a `Tracee`, debugging an existing process.
    fn attach(&mut self, pid: Pid) -> Result<(), Error>;

    fn view(&mut self, pid: Pid) -> Result<(), Error>;

    /// Must be called from the same thread if a process was spawned.
    fn trace(&mut self) -> Result<ExitCode, Error>;
}

/// Common behaviour shared amongst processes.
// pub trait Tracing {
//     fn id(&self) -> Pid;
// 
//     /// Create debugger's hooks onto process.
//     fn attach(&mut self);
// 
//     /// Remove debugger's hooks from process, releasing it's control.
//     fn detach(&mut self);
// 
//     /// Remove debugger's hooks from process, killing it in the process.
//     fn kill(&mut self);
// 
//     fn pause(&mut self);
// 
//     fn kontinue(&mut self);
// 
//     /// Translate virtual memory address to physical address the related parsers understand.
//     fn virt_to_phys(&self, addr: VirtAddr) -> PhysAddr;
// 
//     fn uh(&self, module: &Processor, addr: PhysAddr) -> VirtAddr;
// 
//     /// Translate physical address to virtual memory address.
//     fn phys_to_virt(&self, addr: PhysAddr) -> VirtAddr;
// }

/// Operation to be executed on [`Breakpoint`].
enum BreakpointOp {
    Create,
    Delete,
}

pub struct Breakpoint {
    /// Physical address.
    addr: PhysAddr,

    /// Bytes in memory we overwrote with int3.
    shadow: u32,

    /// Operation to be executed by [`Process`].
    op: Option<BreakpointOp>,
}

impl Breakpoint {
    pub(crate) fn set(&mut self, proc: &mut Process) -> Result<(), Error> {
        let addr = proc.phys_to_virt(self.addr);

        unsafe {
            ReadMemory::new(proc).read(&mut self.shadow, addr).apply()?;
        }

        let trap_inst = (self.shadow & !0xff) | 0xcc;
        WriteMemory::new(proc).write(&trap_inst, addr).apply()
    }

    #[allow(dead_code)]
    pub(crate) fn unset(&mut self, proc: &mut Process) -> Result<(), Error> {
        let addr = proc.phys_to_virt(self.addr);
        WriteMemory::new(proc).write(&self.shadow, addr).apply()
    }
}

pub struct Breakpoints {
    mapping: BTreeMap<PhysAddr, Breakpoint>,
}

impl Breakpoints {
    fn new() -> Self {
        Self {
            mapping: BTreeMap::new(),
        }
    }

    /// Create a new [`Breakpoint`].
    pub fn create(&mut self, addr: PhysAddr) -> bool {
        // can't set a breakpoint twice
        if self.mapping.get(&addr).is_some() {
            return false;
        }

        self.mapping.insert(
            addr,
            Breakpoint {
                addr,
                shadow: 0,
                op: Some(BreakpointOp::Create),
            },
        );

        true
    }

    /// Mark a [`Breakpoint`] for removal.
    pub fn remove(&mut self, addr: PhysAddr) -> bool {
        match self.mapping.get_mut(&addr) {
            Some(bp) => {
                bp.op = Some(BreakpointOp::Delete);
                true
            }
            None => false,
        }
    }
}
