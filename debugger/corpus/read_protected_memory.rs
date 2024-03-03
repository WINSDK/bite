use nix::sys::mman::{mprotect, ProtFlags};
use nix::sys::ptrace;
use nix::sys::signal::{self, Signal};

use std::alloc::{alloc_zeroed, dealloc, Layout};
use std::ffi::c_void;
use std::ptr;

fn main() {
    let var: usize = 101;
    let var2: u8 = 102;

    // Allocate an empty page and make it read-only.
    let layout = Layout::from_size_align(2 * *PAGE_SIZE, *PAGE_SIZE).unwrap();
    let (write_protected_ptr, write_protected_ptr2) = unsafe {
        let ptr = alloc_zeroed(layout);
        mprotect(ptr as *mut c_void, *PAGE_SIZE, ProtFlags::PROT_WRITE)
            .expect("Failed to mprotect");

        (ptr as *mut usize, ptr.add(std::mem::size_of::<usize>()))
    };

    ptrace::traceme().unwrap();

    // Write to the protected memory.
    unsafe {
        ptr::write_volatile(write_protected_ptr, var);
        ptr::write_volatile(write_protected_ptr2, var2);
    }

    // Wait for the parent to signal to continue.
    signal::raise(Signal::SIGSTOP).unwrap();
}
