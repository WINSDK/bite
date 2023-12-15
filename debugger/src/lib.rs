use crossbeam_queue::SegQueue;
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
pub type PhysAddr = usize;
pub type VirtAddr = usize;

/// Behaviour related to creating starting a debug session.
pub trait Debuggable
where
    Self: Sized,
{
    /// Creates a `Tracee`, debugging a newly launched process.
    fn spawn<P: AsRef<std::path::Path>, A: Into<Vec<u8>>>(
        path: P,
        args: Vec<A>,
        desc: DebuggerDescriptor
    ) -> Result<Self, Error>;

    /// Creates a `Tracee`, debugging an existing process.
    fn attach(pid: Pid, desc: DebuggerDescriptor) -> Result<Self, Error>;

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
}

/// Operation to be executed on [`Breakpoint`].
enum BreakpointOp {
    Create,
    Delete,
}

pub struct Breakpoint {
    /// Virtual address.
    addr: VirtAddr,

    /// Byte in memory we overwrote with int3.
    shadow: u8,

    /// Operation to be executed by [`Process`].
    op: Option<BreakpointOp>,
}

impl Breakpoint {
    pub(crate) fn set(&mut self, tracee: &mut impl Tracing) -> Result<(), Error> {
        self.shadow = tracee.read_memory(self.addr, 1)?[0];
        let int3 = &[0xcc];
        tracee.write_memory(self.addr, int3)?;
        Ok(())
    }

    pub(crate) fn unset(&mut self, tracee: &mut impl Tracing) -> Result<(), Error> {
        tracee.write_memory(self.addr, &[self.shadow])?;
        Ok(())
    }
}

pub struct Breakpoints {
    mapping: BTreeMap<VirtAddr, Breakpoint>,
}

impl Breakpoints {
    fn new() -> Self {
        Self {
            mapping: BTreeMap::new(),
        }
    }

    /// Create a new [`Breakpoint`].
    pub fn create(&mut self, addr: VirtAddr) {
        // can't set a breakpoint twice
        if self.mapping.get(&addr).is_some() {
            return;
        }

        self.mapping.insert(
            addr,
            Breakpoint {
                addr,
                shadow: 0,
                op: Some(BreakpointOp::Create),
            },
        );
    }

    /// Mark a [`Breakpoint`] for removal.
    pub fn remove(&mut self, addr: VirtAddr) -> Option<()> {
        self.mapping.get_mut(&addr)?.op = Some(BreakpointOp::Delete);
        Some(())
    }
}

impl std::ops::Deref for Breakpoints {
    type Target = BTreeMap<VirtAddr, Breakpoint>;

    fn deref(&self) -> &Self::Target {
        &self.mapping
    }
}

impl std::ops::DerefMut for Breakpoints {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mapping
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
    BreakpointSet(VirtAddr),
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
