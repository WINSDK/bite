use super::Error;
use std::fmt;

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AlreadyAttached => f.write_str("Debugger already attached to process."),
            Self::InvalidPathName => f.write_str("There appears to be a '\\0' in the path name."),
            Self::PermissionDenied => f.write_str("Permission denied."),
            Self::ProcessLost(pid) => {
                f.write_fmt(format_args!("Process {pid} was lost by the debugger."))
            }
            Self::IncompleteRead(req, res) => {
                f.write_fmt(format_args!("Tried to read {req} bytes, only read {res}."))
            }
            Self::IncompleteWrite(req, res) => f.write_fmt(format_args!(
                "Tried to write {req} bytes, only wrote {res}."
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
