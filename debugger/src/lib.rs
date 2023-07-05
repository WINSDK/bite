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

/// Behaviour related to creating starting a debug session.
pub trait Process: Tracee
where
    Self: Sized,
{
    /// Creates a `Tracee`, debugging a newly launched process.
    fn spawn<P: AsRef<std::path::Path>>(path: P, args: &[&str]) -> Result<Self, Error>;

    /// Creates a `Tracee`, debugging an existing process.
    fn attach(pid: Pid) -> Result<Self, Error>;
}

/// Common behaviour shared amongst debugging sessions.
pub trait Tracee {
    /// Remove debugger's hooks from process, releasing it's control.
    fn detach(self);

    /// Remove debugger's hooks from process, killing it in the process.
    fn kill(self);

    fn pause(&self);

    fn kontinue(&self);

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
