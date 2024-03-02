//! Utility functions to work with memory.
//!
//! Based on the work done by headcrab: https://github.com/headcrab-rs/headcrab

use nix::sys::uio::RemoteIoVec;
use once_cell::sync::Lazy;
use procfs::process::MemoryMap;
use std::cmp::Ordering;
use std::io::{IoSliceMut, IoSlice};

/// Memory page size from system configuration.
pub static PAGE_SIZE: Lazy<usize> = Lazy::new(|| {
    nix::unistd::sysconf(nix::unistd::SysconfVar::PAGE_SIZE)
        .expect("Failed to get page size")
        .map(|size| size as usize)
        .unwrap_or(4096)
});

/// Individual memory operation (reading or writing).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct MemoryOp {
    /// Remote memory location.
    pub remote_base: usize,
    /// Pointer to a local destination or source buffer.
    pub local_ptr: *mut u8,
    /// Size of the `local_ptr` buffer.
    pub local_len: usize,
}

impl MemoryOp {
    /// Converts the memory operation into a remote IoVec suitable for use in vector read/write syscalls.
    pub fn as_remote_iovec(&self) -> RemoteIoVec {
        RemoteIoVec {
            base: self.remote_base,
            len: self.local_len,
        }
    }

    /// Converts the memory operation into a local IoVec suitable for use in vector read syscalls.
    pub fn as_local(&self) -> IoSlice {
        let slice = unsafe { std::slice::from_raw_parts_mut(self.local_ptr, self.local_len) };
        IoSlice::new(slice)
    }

    /// Converts the memory operation into a local IoVec suitable for use in vector write syscalls.
    pub fn as_local_mut(&self) -> IoSliceMut {
        let slice = unsafe { std::slice::from_raw_parts_mut(self.local_ptr, self.local_len) };
        IoSliceMut::new(slice)
    }

    /// Splits `MemoryOp` so that each resulting `MemoryOp` resides in only one memory page.
    pub(crate) fn split_on_page_boundary(&self, out: &mut Vec<impl From<MemoryOp>>) {
        // Number of bytes left to be read or written
        let mut left = self.local_len;

        let next_page_distance = *PAGE_SIZE - ((*PAGE_SIZE - 1) & self.remote_base);
        let to_next_read_op = std::cmp::min(left, next_page_distance);
        // Read or write from remote_base to the end or to the next page
        out.push(From::from(MemoryOp {
            remote_base: self.remote_base,
            local_ptr: self.local_ptr,
            local_len: to_next_read_op,
        }));
        left -= to_next_read_op;

        while left > 0 {
            if left < *PAGE_SIZE {
                // Read or write from beginning of the page to a part in the middle (last read or write)
                out.push(From::from(MemoryOp {
                    remote_base: self.remote_base + (self.local_len - left),
                    local_ptr: (self.local_ptr as usize + (self.local_len - left)) as *mut u8,
                    local_len: left,
                }));
                break;
            } else {
                // Whole page is being read or written
                out.push(From::from(MemoryOp {
                    remote_base: self.remote_base + (self.local_len - left),
                    local_ptr: (self.local_ptr as usize + (self.local_len - left)) as *mut u8,
                    local_len: *PAGE_SIZE,
                }));
                left -= *PAGE_SIZE;
            }
        }
    }
}

/// Splits memory operations to those that can access protected memory and those that do not.
/// This function can be used for both write or read operations, and `protected_maps` should be
/// pre-filtered to contain only protected pages, e.g.:
pub(crate) fn split_protected(
    protected_maps: &[&MemoryMap],
    operations: &[MemoryOp],
) -> (Vec<MemoryOp>, Vec<MemoryOp>) {
    let (protected, permissioned) = operations.iter().partition(|op| {
        protected_maps
            .binary_search_by(|map| {
                if op.remote_base < map.address.0 as usize {
                    Ordering::Greater
                } else if op.remote_base > map.address.1 as usize {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            })
            .is_ok()
    });

    (protected, permissioned)
}
