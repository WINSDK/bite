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
pub type Addr = usize;

/// Behaviour related to creating starting a debug session.
pub trait Process: Tracee
where
    Self: Sized,
{
    /// Creates a `Tracee`, debugging a newly launched process.
    fn spawn<P: AsRef<std::path::Path>, A: Into<Vec<u8>>>(
        path: P,
        args: Vec<A>,
    ) -> Result<Self, Error>;

    /// Creates a `Tracee`, debugging an existing process.
    fn attach(pid: Pid) -> Result<Self, Error>;

    /// Run blocking event loop of [`Debugger`].
    fn run(self, ctx: Arc<Context>) -> Result<(), Error>;
}

/// Common behaviour shared amongst debugging sessions.
pub trait Tracee {
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
    fn read_process_memory(&self, base_addr: Addr, len: usize) -> Result<Vec<u8>, Error>;

    /// Writes to a segment of memory.
    ///
    /// On linux this is currently only supports unprotected memory pages.
    /// Whilst this is faster than the ptrace alternative, it's more restrictive.
    fn write_process_memory(&mut self, base_addr: Addr, data: &[u8]) -> Result<(), Error>;
}

/// Operation to be executed on [`Breakpoint`].
enum BreakpointOp {
    Create,
    Delete
}

pub struct Breakpoint {
    /// Location in [`Tracee`]'s memory space.
    addr: Addr,

    /// Byte in memory we overwrote with int3.
    original_byte: u8,

    /// Operation to be executed by [`Tracee`].
    op: Option<BreakpointOp>,
}

impl Breakpoint {
    pub(crate) fn set(&mut self, tracee: &mut impl Tracee) -> Result<(), Error> {
        self.original_byte = tracee.read_process_memory(self.addr, 1)?[0];
        let int3 = &[0xcc];
        tracee.write_process_memory(self.addr, int3)?;
        Ok(())
    }

    pub(crate) fn unset(&mut self, tracee: &mut impl Tracee) -> Result<(), Error> {
        tracee.write_process_memory(self.addr, &[self.original_byte])?;
        Ok(())
    }
}

pub struct Breakpoints {
    mapping: BTreeMap<Addr, Breakpoint>,
}

impl Breakpoints {
    fn new() -> Self {
        Self {
            mapping: BTreeMap::new(),
        }
    }

    /// Create a new [`Breakpoint`], the address must be valid within the [`Tracee`]'s memory.
    pub fn create(&mut self, addr: Addr) {
        self.mapping.insert(addr, Breakpoint {
            addr,
            original_byte: 0,
            op: Some(BreakpointOp::Create),
        });
    }

    /// Mark a [`Breakpoint`] for removal.
    pub fn remove(&mut self, addr: Addr) -> Option<()> {
        self.mapping.get_mut(&addr)?.op = Some(BreakpointOp::Delete);
        Some(())
    }

    fn iter_mut(&mut self) -> impl Iterator<Item = &mut Breakpoint> {
        self.mapping.values_mut()
    }
}

#[derive(Debug, PartialEq)]
pub enum DebugeeEvent {
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
