use crate::memory::{split_protected, MemoryOp};
use crate::{Error, Process};

use nix::sys::ptrace;
use procfs::process::MMPermissions;
use std::ffi::c_void;
use std::marker::PhantomData;

/// Read operations don't have any unique properties at this time.
/// If needed, later this can be replaced with `struct ReadOp(MemoryOp, <extra props>)`.
type ReadOp = MemoryOp;

/// Allows to read memory from different locations in debuggee's memory as a single operation.
pub struct ReadMemory<'a> {
    proc: &'a Process,
    read_ops: Vec<ReadOp>,
    /// This requires a mutable reference because we rewrite values of variables in `ReadOp`.
    _marker: PhantomData<&'a mut ()>,
}

impl<'a> ReadMemory<'a> {
    pub fn new(proc: &'a Process) -> Self {
        ReadMemory {
            proc,
            read_ops: Vec::new(),
            _marker: PhantomData,
        }
    }

    /// Reads a value of type `T` from debuggee's memory at location `remote_base`.
    /// This value will be written to the provided variable `val`.
    /// You should call `apply` in order to execute the memory read operation.
    /// The provided variable `val` can't be accessed until either `apply` is called or `self` is
    /// dropped.
    ///
    /// # Safety
    ///
    /// The type `T` must not have any invalid values.
    /// For example, `T` must not be a `bool`, as `transmute::<u8, bool>(2)`
    /// is not a valid value for a bool.
    /// In case of doubt, wrap the type in [`std::mem::MaybeUninit`].
    // todo: further document mem safety - e.g., what happens in the case of partial read
    pub unsafe fn read<T>(mut self, val: &'a mut T, remote_base: usize) -> Self {
        ReadOp::split_on_page_boundary(
            &ReadOp {
                remote_base,
                local_ptr: val as *mut T as *mut u8,
                local_len: std::mem::size_of::<T>(),
            },
            &mut self.read_ops,
        );
        self
    }

    /// Reads a value of type `*mut T` from debuggee's memory at location `remote_base`.
    /// This value will be written to the provided pointer `ptr`.
    /// You should call `apply` in order to execute the memory read operation.
    /// The provided pointer `ptr` can't be accessed until either `apply` is called or `self` is
    /// dropped.
    ///
    /// # Safety
    ///
    /// Memory location at `ptr` must be of valid size and must not be outlived by `ReadMem`.
    /// You need to ensure the lifetime guarantees, and generally you should
    /// prefer using `read<T>(&mut val)`.
    // todo: further document mem safety - e.g., what happens in the case of partial read
    pub unsafe fn read_ptr<T>(mut self, ptr: *mut T, remote_base: usize) -> Self {
        ReadOp::split_on_page_boundary(
            &ReadOp {
                remote_base,
                local_ptr: ptr as *mut u8,
                local_len: std::mem::size_of::<T>(),
            },
            &mut self.read_ops,
        );
        self
    }

    /// Reads a slice of type `&mut [T]` from debuggee's memory at location `remote_base`.
    /// This value will be written to the provided slice `val`.
    /// You should call `apply` in order to execute the memory read operation.
    /// The provided value `val` can't be accessed until either `apply` is called or `self` is
    /// dropped.
    ///
    /// # Safety
    ///
    /// The type `T` must not have any invalid values. For example, `T` must not be a `bool`, as
    /// `transmute::<u8, bool>(2)` is not a valid value for a bool. In case of doubt, wrap the type
    /// in [`mem::MaybeUninit`].
    // todo: further document mem safety - e.g., what happens in the case of partial read
    pub unsafe fn read_slice<T>(mut self, val: &'a mut [T], remote_base: usize) -> Self {
        ReadOp::split_on_page_boundary(
            &ReadOp {
                remote_base,
                local_ptr: val.as_mut_ptr() as *mut u8,
                local_len: val.len() * std::mem::size_of::<T>(),
            },
            &mut self.read_ops,
        );
        self
    }

    /// Reads a `u8` byte slice from debuggee's memory at location `remote_base`.
    /// This value will be written to the provided slice `val`.
    /// You should call `apply` in order to execute the memory read operation.
    pub fn read_byte_slice<T>(mut self, val: &'a mut [u8], remote_base: usize) -> Self {
        ReadOp::split_on_page_boundary(
            &ReadOp {
                remote_base,
                local_ptr: val.as_mut_ptr() as *mut u8,
                local_len: val.len(),
            },
            &mut self.read_ops,
        );
        self
    }

    /// Executes the memory read operation.
    pub fn apply(self) -> Result<(), Error> {
        // FIXME: Probably a better way to do this - see if we can get info about pages protection
        // from cache and predict whether this operation will require ptrace or plain
        // read_process_vm would work.
        match self.read_process_vm(&self.read_ops) {
            Err(Error::Kernel(nix::Error::EFAULT)) | Err(Error::IncompleteRead(_, _)) => {
                let protected_maps: Vec<_> = self
                    .proc
                    .memory_maps()
                    .filter(|map| !map.perms.contains(MMPermissions::READ))
                    .collect();

                let (protected, readable) = split_protected(&protected_maps, &self.read_ops);

                self.read_process_vm(&readable)?;
                self.read_ptrace(&protected)?;
            }
            _ => {}
        }

        Ok(())
    }

    /// Executes memory reading operations using ptrace only.
    /// This function should be used only for testing purposes.
    #[cfg(test)]
    unsafe fn apply_ptrace(self) -> Result<(), Error> {
        self.read_ptrace(&self.read_ops)
    }

    /// Allows to read from several different locations with one system call. It will error on
    /// pages that are not readable. Returns number of bytes read at granularity of ReadOps.
    fn read_process_vm(&self, read_ops: &[ReadOp]) -> Result<usize, Error> {
        let pid = self.proc.id();
        let bytes_expected = read_ops.iter().fold(0, |sum, read_op| sum + read_op.local_len);

        if bytes_expected > isize::MAX as usize {
            panic!("Read size too big");
        };

        // Create a list of `IoVec`s and remote `IoVec`s
        let remote: Vec<_> = read_ops.iter().map(|read_op| read_op.as_remote_iovec()).collect();
        let mut local: Vec<_> = read_ops.iter().map(|read_op| read_op.as_local_mut()).collect();

        let bytes_read = nix::sys::uio::process_vm_readv(pid, &mut local, &remote)?;
        if bytes_read != bytes_expected {
            return Err(Error::IncompleteRead(bytes_expected, bytes_read));
        }

        Ok(bytes_read)
    }

    /// Allows to read from protected memory pages.
    /// This operation results in multiple system calls and is inefficient.
    fn read_ptrace(&self, read_ops: &[ReadOp]) -> Result<(), Error> {
        let pid = self.proc.id();
        let bytes_expected = self.read_ops.iter().fold(0, |sum, read_op| sum + read_op.local_len);

        let long_size = std::mem::size_of::<std::os::raw::c_long>();
        let mut bytes_read = 0;

        for read_op in read_ops {
            let mut offset: usize = 0;

            // Read until all of the data is read
            while offset < read_op.local_len {
                let data = match ptrace::read(pid, (read_op.remote_base + offset) as *mut c_void) {
                    Err(_) => return Err(Error::IncompleteRead(bytes_read, bytes_expected)),
                    Ok(read) => read,
                };

                // Read full word. No need to preserve other data
                if (read_op.local_len - offset) >= long_size {
                    // todo: document unsafety
                    unsafe {
                        *((read_op.local_ptr as usize + offset) as *mut i64) = data;
                    }

                // Read part smaller than word. Need to preserve other data
                } else {
                    // todo: document unsafety
                    unsafe {
                        let previous_bytes: &mut [u8] = std::slice::from_raw_parts_mut(
                            (read_op.local_ptr as usize + offset) as *mut u8,
                            read_op.local_len - offset,
                        );
                        let data_bytes = data.to_ne_bytes();

                        previous_bytes[0..(read_op.local_len - offset)]
                            .clone_from_slice(&data_bytes[0..(read_op.local_len - offset)]);
                    }
                }
                offset += long_size;
            }

            bytes_read += offset;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::memory::PAGE_SIZE;
    use crate::readmem::ReadMemory;
    use crate::{Debuggable, Debugger};

    use nix::sys::mman::{mprotect, ProtFlags};
    use nix::sys::ptrace;
    use nix::sys::signal::{self, Signal};
    use nix::sys::wait;
    use nix::unistd::{fork, ForkResult};

    use std::alloc::{alloc_zeroed, dealloc, Layout};
    use std::ffi::c_void;
    use std::ptr;

    #[test]
    fn read_memory_proc_vm() {
        let var: usize = 52;
        let var2: u8 = 128;

        let mut read_var_op: usize = 0;
        let mut read_var2_op: u8 = 0;

        // Have debugger view process without attaching.
        let mut debugger = Debugger::<&str>::me();
        debugger.view(nix::unistd::getpid()).unwrap();
        let mut debugger = debugger.lock();
        let process = debugger.processes().next().expect("No processes");

        unsafe {
            ReadMemory::new(&process)
                .read(&mut read_var_op, &var as *const _ as usize)
                .read(&mut read_var2_op, &var2 as *const _ as usize)
                .apply()
                .expect("Failed to read memory");
        }

        unsafe {
            assert_eq!(ptr::read_volatile(&read_var_op), var);
            assert_eq!(ptr::read_volatile(&read_var2_op), var2);
        }
    }

    #[test]
    fn read_memory_ptrace() {
        let var: usize = 52;
        let var2: u8 = 128;
        let dyn_array = vec![1, 2, 3, 4];

        let mut read_var_op: usize = 0;
        let mut read_var2_op: u8 = 0;
        let mut read_array = [0u8; 4];

        match unsafe { fork() } {
            Ok(ForkResult::Child) => {
                ptrace::traceme().unwrap();

                // Wait for the parent process to signal to continue.
                signal::raise(Signal::SIGSTOP).unwrap();

                // Return an explicit status code.
                std::process::exit(0);
            }
            Ok(ForkResult::Parent { child, .. }) => {
                // Wait for child.
                let wait_status = wait::waitpid(child, None).unwrap();

                match wait_status {
                    wait::WaitStatus::Stopped(_pid, _sig) => {}
                    status => {
                        signal::kill(child, Signal::SIGKILL).unwrap();
                        panic!("Unexpected child status: {:?}", status);
                    }
                }

                // Have debugger view process without attaching.
                let mut debugger = Debugger::<&str>::me();
                debugger.view(child).unwrap();
                let mut debugger = debugger.lock();
                let process = debugger.processes().next().expect("No processes");

                // Read memory from the child's process.
                unsafe {
                    ReadMemory::new(&process)
                        .read(&mut read_var_op, &var as *const _ as usize)
                        .read(&mut read_var2_op, &var2 as *const _ as usize)
                        .read_slice(&mut read_array, dyn_array.as_ptr() as usize)
                        .apply_ptrace()
                        .expect("Failed to read memory");

                    assert_eq!(read_var_op, var);
                    assert_eq!(read_var2_op, var2);
                    assert_eq!(read_array, &dyn_array[..]);
                }

                ptrace::detach(child, Some(Signal::SIGCONT)).unwrap();

                // Check if the child assertions are successful.
                let exit_status = wait::waitpid(child, None).unwrap();

                match exit_status {
                    wait::WaitStatus::Exited(_pid, 0) => {} // normal exit
                    wait::WaitStatus::Exited(_pid, err_code) => {
                        panic!(
                            "Child exited with an error {err_code}, run this test with \
                               --nocapture to see the full output.",
                        );
                    }
                    status => panic!("Unexpected child status: {status:?}"),
                }
            }
            Err(x) => panic!("{x}"),
        }
    }

    #[test]
    fn write_protected_memory() {
        let var: usize = 101;
        let var2: u8 = 102;

        let mut read_var_op: usize = 0;
        let mut read_var2_op: u8 = 0;

        // Allocate an empty page and make it read-only.
        let layout = Layout::from_size_align(2 * *PAGE_SIZE, *PAGE_SIZE).unwrap();
        let (write_protected_ptr, write_protected_ptr2) = unsafe {
            let ptr = alloc_zeroed(layout);
            mprotect(ptr as *mut c_void, *PAGE_SIZE, ProtFlags::PROT_WRITE)
                .expect("Failed to mprotect");

            (ptr as *mut usize, ptr.add(std::mem::size_of::<usize>()))
        };

        match unsafe { fork() } {
            Ok(ForkResult::Child) => {
                ptrace::traceme().unwrap();

                // Write to the protected memory.
                unsafe {
                    ptr::write_volatile(write_protected_ptr, var);
                    ptr::write_volatile(write_protected_ptr2, var2);
                }

                // Wait for the parent process to signal to continue.
                signal::raise(Signal::SIGSTOP).unwrap();

                // Return an explicit status code.
                std::process::exit(0);
            }
            Ok(ForkResult::Parent { child, .. }) => {
                // Wait for child.
                let wait_status = wait::waitpid(child, None).unwrap();

                match wait_status {
                    wait::WaitStatus::Stopped(_pid, _sig) => {}
                    status => {
                        signal::kill(child, Signal::SIGKILL).unwrap();
                        panic!("Unexpected child status: {:?}", status);
                    }
                }

                // Have debugger view process without attaching.
                let mut debugger = Debugger::<&str>::me();
                debugger.view(child).unwrap();
                let mut debugger = debugger.lock();
                let process = debugger.processes().next().expect("No processes");

                // Read memory from the child's process.
                unsafe {
                    ReadMemory::new(process)
                        .read(&mut read_var_op, write_protected_ptr as usize)
                        .read(&mut read_var2_op, write_protected_ptr2 as usize)
                        .apply()
                        .unwrap();

                    assert_eq!(var, read_var_op);
                    assert_eq!(var2, read_var2_op);
                }

                ptrace::detach(child, Some(Signal::SIGCONT)).unwrap();

                // 'Unprotect' memory so that it can be deallocated.
                unsafe {
                    mprotect(
                        write_protected_ptr as *mut _,
                        *PAGE_SIZE,
                        ProtFlags::PROT_WRITE | ProtFlags::PROT_READ,
                    )
                    .expect("Failed to mprotect");
                    dealloc(write_protected_ptr as *mut _, layout);
                }

                // Check if the child assertions are successful.
                let exit_status = wait::waitpid(child, None).unwrap();

                match exit_status {
                    wait::WaitStatus::Exited(_pid, 0) => {} // normal exit
                    wait::WaitStatus::Exited(_pid, err_code) => {
                        panic!(
                            "Child exited with an error {err_code}, run this test with \
                               --nocapture to see the full output.",
                        );
                    }
                    status => panic!("Unexpected child status: {status:?}"),
                }
            },
            Err(x) => panic!("{x}"),
        };
    }
}
