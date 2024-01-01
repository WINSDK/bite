use crossbeam_queue::SegQueue;
use disassembler::{PhysAddr, VirtAddr};
use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};

mod collections;

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

pub type ExitCode = i32;

/// Behaviour related to creating starting a debug session.
pub trait Debuggable
where
    Self: Sized,
{
    /// Creates a `Tracee`, debugging a newly launched process.
    fn spawn<S: Into<Vec<u8>>>(
        settings: DebuggerSettings<S>,
        desc: DebuggerDescriptor<S>,
    ) -> Result<Self, Error>;

    /// Creates a `Tracee`, debugging an existing process.
    fn attach<S: Into<Vec<u8>>>(
        pid: Pid,
        settings: DebuggerSettings<S>,
        desc: DebuggerDescriptor<S>,
    ) -> Result<Self, Error>;

    /// Run blocking event loop of [`Debugger`].
    fn run(self, ctx: Arc<Context>) -> Result<(), Error>;
}

/// Common behaviour shared amongst debugging sessions.
pub trait Tracing {
    /// Create debugger's hooks onto process.
    fn attach(&mut self);

    /// Remove debugger's hooks from process, releasing it's control.
    fn detach(&mut self);

    /// Remove debugger's hooks from process, killing it in the process.
    fn kill(&mut self);

    fn pause(&self);

    fn kontinue(&mut self);

    /// Read's a segment of memory.
    ///
    /// On linux this is currently only supports unprotected memory pages.
    /// Whilst this is faster than the ptrace alternative, it's more restrictive.
    fn read_memory(&self, addr: VirtAddr, len: usize) -> Result<Vec<u8>, Error>;

    /// Writes to a segment of memory.
    ///
    /// On linux this is currently only supports unprotected memory pages.
    /// Whilst this is faster than the ptrace alternative, it's more restrictive.
    fn write_memory(&mut self, addr: VirtAddr, data: &[u8]) -> Result<(), Error>;

    /// Writes to a protected segment of memory.
    ///
    /// On linux this uses ptrace::write in a loop which is slow for large write's.
    fn write_protected_memory(&mut self, addr: VirtAddr, data: &[u8]) -> Result<(), Error>;

    /// Translate virtual memory address to physical address the related parsers understand.
    fn virt_to_phys(&self, addr: VirtAddr) -> PhysAddr;

    /// Translate physical address to virtual memory address.
    fn phys_to_virt(&self, addr: PhysAddr) -> VirtAddr;
}

/// Operation to be executed on [`Breakpoint`].
enum BreakpointOp {
    Create,
    Delete,
}

pub struct Breakpoint {
    /// Physical address.
    addr: PhysAddr,

    /// Byte in memory we overwrote with int3.
    shadow: Vec<u8>,

    /// Operation to be executed by [`Process`].
    op: Option<BreakpointOp>,
}

impl Breakpoint {
    pub(crate) fn set(&mut self, proc: &mut dyn Tracing) -> Result<(), Error> {
        let vaddr = proc.phys_to_virt(self.addr);
        self.shadow = proc.read_memory(vaddr, 4)?;

        let orig_byte = self.shadow[0];
        self.shadow[0] = 0xcc;
        proc.write_protected_memory(vaddr, &self.shadow)?;
        self.shadow[0] = orig_byte;

        Ok(())
    }

    pub(crate) fn unset(&mut self, proc: &mut dyn Tracing) -> Result<(), Error> {
        let vaddr = proc.phys_to_virt(self.addr);
        proc.write_protected_memory(vaddr, &self.shadow)?;
        Ok(())
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
                shadow: Vec::new(),
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
            None => false
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum DebugeeEvent {
    Break,
    Continue,
    Exit,
}

#[derive(Debug, PartialEq)]
pub enum DebuggerEvent {
    Exited(ExitCode),
}

/// Primary interface for consumer's to interact with debugger.
/// Necessary as debugger runs on a different thread.
pub struct MessageQueue {
    debugee_queue: SegQueue<DebugeeEvent>,
    debugger_queue: SegQueue<DebuggerEvent>,
}

impl MessageQueue {
    /// Empty unbound queue.
    fn new() -> Self {
        Self {
            debugee_queue: SegQueue::new(),
            debugger_queue: SegQueue::new(),
        }
    }

    /// Send event to [`Debugger`].
    pub fn push(&self, event: DebugeeEvent) {
        self.debugee_queue.push(event);
    }

    /// Try to receive event from [`Debugger`].
    pub fn pop(&self) -> Option<DebuggerEvent> {
        self.debugger_queue.pop()
    }

    /// Send event to consumer.
    pub(crate) fn pushd(&self, event: DebuggerEvent) {
        self.debugger_queue.push(event);
    }

    /// Try to receive event from consumer.
    pub(crate) fn popd(&self) -> Option<DebugeeEvent> {
        self.debugee_queue.pop()
    }
}

pub struct Context {
    attached: AtomicBool,
    pub breakpoints: RwLock<Breakpoints>,
    pub queue: MessageQueue,
}

impl Context {
    pub fn new() -> Self {
        Self {
            attached: AtomicBool::new(false),
            breakpoints: RwLock::new(Breakpoints::new()),
            queue: MessageQueue::new(),
        }
    }

    /// Return whether queue is being used by a debugger.
    pub fn attached(&self) -> bool {
        self.attached.load(Ordering::Relaxed)
    }

    /// Mark queue as being used by a debugger.
    pub(crate) fn attach(&self) {
        self.attached.store(true, Ordering::Relaxed);
    }

    /// Mark queue as not being used by a debugger.
    pub(crate) fn deattach(&self) {
        self.attached.store(false, Ordering::Relaxed);
    }
}
