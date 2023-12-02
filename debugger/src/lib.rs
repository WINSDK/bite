use crossbeam_queue::SegQueue;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

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
pub trait Process: Tracee
where
    Self: Sized,
{
    /// Creates a `Tracee`, debugging a newly launched process.
    fn spawn<P: AsRef<std::path::Path>, A: Into<Vec<u8>>>(
        queue: MessageQueue,
        path: P,
        args: Vec<A>,
    ) -> Result<Self, Error>;

    /// Creates a `Tracee`, debugging an existing process.
    fn attach(queue: MessageQueue, pid: Pid) -> Result<Self, Error>;

    /// Run blocking event loop of [`Debugger`].
    fn run(self) -> Result<(), Error>;
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
    fn read_process_memory(&self, base_addr: usize, len: usize) -> Result<Vec<u8>, Error>;

    /// Writes to a segment of memory.
    ///
    /// On linux this is currently only supports unprotected memory pages.
    /// Whilst this is faster than the ptrace alternative, it's more restrictive.
    fn write_process_memory(&mut self, base_addr: usize, data: &[u8]) -> Result<(), Error>;
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
#[derive(Clone)]
pub struct MessageQueue {
    attached: Arc<AtomicBool>,
    debugee_queue: Arc<SegQueue<DebugeeEvent>>,
    debugger_queue: Arc<SegQueue<DebuggerEvent>>,
}

impl MessageQueue {
    /// Empty unbound queue.
    pub fn new() -> Self {
        Self {
            attached: Arc::new(AtomicBool::new(false)),
            debugee_queue: Arc::new(SegQueue::new()),
            debugger_queue: Arc::new(SegQueue::new()),
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

    /// Return whether queue is being used by a debugger.
    pub fn attached(&self) -> bool {
        self.attached.load(Ordering::Relaxed)
    }

    /// Mark queue as being used by debugger.
    pub(crate) fn attach(&self) {
        self.attached.store(true, Ordering::Relaxed);
    }
}
