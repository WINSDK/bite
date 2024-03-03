use std::fmt;
use crate::PidFd;

pub enum Error {
    InvalidPathName,
    TraceeLost(PidFd),
    IncompleteRead { req: usize, read: usize },
    IncompleteWrite { req: usize, wrote: usize },
    Procfs(procfs::ProcError),
    Kernel(nix::errno::Errno),
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Self::Unexpected => f.write_str("Debugger unexpectedly failed."),
            // Self::AlreadyAttached => f.write_str("Debugger already attached to process."),
            Self::InvalidPathName => f.write_str("There appears to be a '\\0' in the path name."),
            // Self::PermissionDenied => f.write_str("Permission denied."),
            Self::TraceeLost(pidfd) => {
                f.write_fmt(format_args!("Tracee {} was lost by the debugger.", pidfd.as_raw()))
            }
            Self::IncompleteRead { req, read } => {
                f.write_fmt(format_args!("Tried to read {req} bytes, only read {read}."))
            }
            Self::IncompleteWrite { req, wrote } => f.write_fmt(format_args!(
                "Tried to write {req} bytes, only wrote {wrote}."
            )),
            Self::Procfs(err) => f.write_fmt(format_args!("Debugger failed with {err}.")),
            Self::Kernel(err) => f.write_fmt(format_args!("Debugger failed with {err}")),
        }
    }
}

impl From<nix::Error> for Error {
    fn from(error: nix::Error) -> Self {
        Error::Kernel(error)
    }
}

impl From<procfs::ProcError> for Error {
    fn from(error: procfs::ProcError) -> Self {
        Error::Procfs(error)
    }
}
