use std::ffi::CString;
use std::fmt;
use std::os::unix::ffi::OsStrExt;

use nix::sys::{personality, ptrace};
use nix::unistd::{execvp, fork, ForkResult};

pub struct Pid(nix::unistd::Pid);

pub enum Error {
    InvalidPathName,
    IncompleteRead(usize, usize),
    IncompleteWrite(usize, usize),
    Kernel(nix::errno::Errno),
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidPathName => f.write_str("There appears to be a '\\0' in the path name."),
            Self::IncompleteRead(req, res) => {
                f.write_fmt(format_args!("Tried to read {req} bytes, only read {res}."))
            }
            Self::IncompleteWrite(req, res) => f.write_fmt(format_args!(
                "Tried to write {req} bytes, only wrote {res}."
            )),
            Self::Kernel(err) => f.write_fmt(format_args!("{err}.")),
        }
    }
}

pub struct Debugger {
    pub pid: Pid,
}

impl super::Process for Debugger {
    fn spawn<P: AsRef<std::path::Path>>(path: P, args: &[&str]) -> Result<Self, Error> {
        let c_path = CString::new(path.as_ref().as_os_str().as_bytes())
            .map_err(|_| Error::InvalidPathName)?;

        let mut prog_args = Vec::with_capacity(args.len());
        for arg in args {
            let arg = CString::new(*arg).map_err(|_| Error::InvalidPathName)?;
            prog_args.push(arg);
        }

        match unsafe { fork().map_err(Error::Kernel)? } {
            ForkResult::Parent { child, .. } => Ok(Debugger { pid: Pid(child) }),
            ForkResult::Child => {
                // disable ASLR
                personality::set(personality::Persona::ADDR_NO_RANDOMIZE).map_err(Error::Kernel)?;

                // signal child process to be traced
                ptrace::traceme().map_err(Error::Kernel)?;

                // execute program
                execvp(c_path.as_c_str(), &prog_args).map_err(Error::Kernel)?;

                // `execvp` can't exit successfully, this shouldn't be run
                unsafe { nix::libc::_exit(1) }
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

    fn read_process_memory(&self, base_addr: usize, len: usize) -> Result<Vec<u8>, Error> {
        let mut buf: Vec<u8> = Vec::with_capacity(len);
        let uninit: &mut [u8] = unsafe { std::mem::transmute(buf.spare_capacity_mut()) };

        let local = std::io::IoSliceMut::new(uninit);
        let remote = nix::sys::uio::RemoteIoVec {
            base: base_addr,
            len,
        };

        let bytes_read = nix::sys::uio::process_vm_readv(self.pid.0, &mut [local], &[remote])
            .map_err(Error::Kernel)?;

        if len != bytes_read {
            return Err(Error::IncompleteRead(len, bytes_read));
        }

        Ok(buf)
    }

    fn write_process_memory(&mut self, base_addr: usize, data: &[u8]) -> Result<(), Error> {
        let local = std::io::IoSlice::new(data);
        let remote = nix::sys::uio::RemoteIoVec {
            base: base_addr,
            len: data.len(),
        };

        let bytes_wrote = nix::sys::uio::process_vm_writev(self.pid.0, &[local], &[remote])
            .map_err(Error::Kernel)?;

        if data.len() != bytes_wrote {
            return Err(Error::IncompleteWrite(data.len(), bytes_wrote));
        }

        Ok(())
    }
}

#[test]
fn spawn() {
    use crate::{Process, Tracee};

    let session = Debugger::spawn("../target/debug/bite", &[]).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(10));
    session.kill();
}
