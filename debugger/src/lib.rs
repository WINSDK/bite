#[cfg(target_os = "linux")]
mod linux;

#[cfg(target_os = "windows")]
mod windows;

#[cfg(target_os = "linux")]
use linux::*;

#[cfg(target_os = "windows")]
use windows::*;

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
}
