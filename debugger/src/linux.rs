use std::ffi::CString;
use std::fmt;
use std::os::unix::ffi::OsStrExt;

use nix::sys::ptrace;
use nix::unistd::{execvp, fork, ForkResult};

pub struct Pid(nix::unistd::Pid);

pub enum Error {
    InvalidPathName,
    Kernel(nix::errno::Errno),
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidPathName => f.write_str("There appears to be a '\\0' in the path name."),
            Self::Kernel(err) => f.write_fmt(format_args!("Kernel returned error: '{err}'.")),
        }
    }
}

pub struct Debugger {
    pub pid: Pid,
}

impl super::Process for Debugger {
    fn spawn<P: AsRef<std::path::Path>>(path: P, args: &[&str]) -> Result<Self, Error> {
        let path = CString::new(path.as_ref().as_os_str().as_bytes())
            .map_err(|_| Error::InvalidPathName)?;

        let mut prog_args = Vec::with_capacity(args.len());
        for arg in args {
            let arg = CString::new(*arg).map_err(|_| Error::InvalidPathName)?;
            prog_args.push(arg);
        }

        match unsafe { fork().map_err(Error::Kernel)? } {
            ForkResult::Parent { child, .. } => Ok(Debugger { pid: Pid(child) }),
            ForkResult::Child => {
                // signal child process to be traced
                ptrace::traceme().map_err(Error::Kernel)?;

                // execute program
                execvp(path.as_c_str(), &prog_args).map_err(Error::Kernel)?;

                // `execvp` can't exit successfully
                unreachable!()
            }
        }
    }

    fn attach(pid: Pid) -> Result<Self, Error> {
        ptrace::attach(pid.0).map_err(Error::Kernel)?;
        Ok(Debugger { pid })
    }
}

impl super::Tracee for Debugger {
    fn detach(self) {
        // ignore the result since detaching can't fail
        let _ = ptrace::detach(self.pid.0, None);
    }

    fn kill(self) {
        // ignore the result since killing a process can't fail
        let _ = ptrace::kill(self.pid.0);
    }
}
