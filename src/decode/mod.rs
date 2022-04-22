use std::fmt;
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicUsize, Ordering};

pub mod arm;
pub mod riscv;
pub mod x86_64;

mod lookup;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum BitWidth {
    U128,
    U64,
    U32,
    U16,
}

pub struct Array<T, const S: usize> {
    bytes: [T; S],
    len: AtomicUsize,
}

impl<T: Default, const S: usize> Array<T, S> {
    pub fn new() -> Self {
        let mut bytes: MaybeUninit<[T; S]> = MaybeUninit::uninit();
        let mut ptr = bytes.as_mut_ptr() as *mut T;

        for _ in 0..S {
            unsafe {
                ptr.write(T::default());
                ptr = ptr.offset(1);
            }
        }

        Self { bytes: unsafe { bytes.assume_init() }, len: AtomicUsize::new(0) }
    }
}

impl<T, const S: usize> Array<T, S> {
    #[inline]
    pub fn iter<'a>(&'a self) -> std::slice::Iter<'a, T> {
        self.as_ref().iter()
    }

    #[inline]
    pub fn iter_mut<'a>(&'a mut self) -> std::slice::IterMut<'a, T> {
        self.as_mut().iter_mut()
    }

    pub fn len(&self) -> usize {
        self.len.load(Ordering::Relaxed)
    }

    pub fn remove(&mut self, idx: usize) {
        let len = self.len.load(Ordering::Acquire);
        assert!(idx < len, "idx is {idx} which is out of bounce for len of {len}");

        unsafe {
            std::ptr::copy(
                self.bytes.as_ptr().add(idx + 1),
                self.bytes.as_mut_ptr().add(idx),
                len - idx - 1,
            );
        }

        self.len.store(len - 1, Ordering::Release);
    }
}

impl<T: Clone, const S: usize> Clone for Array<T, S> {
    fn clone(&self) -> Self {
        Self { bytes: self.bytes.clone(), len: AtomicUsize::new(self.len.load(Ordering::SeqCst)) }
    }
}

impl<T: PartialEq, const S: usize> PartialEq for Array<T, S> {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}

impl<T: Eq, const S: usize> Eq for Array<T, S> {}

impl<T: Default + Copy, const S: usize> Default for Array<T, S> {
    fn default() -> Self {
        Self { bytes: [T::default(); S], len: AtomicUsize::new(0) }
    }
}

impl<T, const S: usize> std::ops::Index<usize> for Array<T, S> {
    type Output = T;

    #[inline]
    fn index(&self, idx: usize) -> &Self::Output {
        self.len.fetch_max(idx + 1, Ordering::AcqRel);
        &self.bytes[idx]
    }
}

impl<T, const S: usize> std::ops::IndexMut<usize> for Array<T, S> {
    #[inline]
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        self.len.fetch_max(idx + 1, Ordering::AcqRel);
        &mut self.bytes[idx]
    }
}

impl<T, const S: usize> AsRef<[T]> for Array<T, S> {
    fn as_ref(&self) -> &[T] {
        let len = self.len();
        &self.bytes[..len]
    }
}

impl<T, const S: usize> AsMut<[T]> for Array<T, S> {
    fn as_mut(&mut self) -> &mut [T] {
        let len = self.len();
        &mut self.bytes[..len]
    }
}

impl<T: fmt::Debug, const S: usize> fmt::Debug for Array<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", &self.bytes[..self.len.load(Ordering::Relaxed)]))
    }
}

pub struct Reader<'a> {
    pub buf: &'a [u8],
    pub pos: AtomicUsize,
}

#[allow(dead_code)]
impl<'a> Reader<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        Self { buf, pos: AtomicUsize::new(0) }
    }

    pub fn inner(&self) -> &'a [u8] {
        &self.buf[self.pos.load(Ordering::SeqCst)..]
    }

    pub fn offset(&self, num_bytes: isize) {
        let pos = self.pos.load(Ordering::Acquire) as isize;
        self.pos.store((pos + num_bytes) as usize, Ordering::Release);
    }

    pub fn take(&self, byte: u8) -> bool {
        let pos = self.pos.load(Ordering::SeqCst);
        if self.buf.get(pos) == Some(&byte) {
            self.pos.store(pos + 1, Ordering::SeqCst);
            true
        } else {
            false
        }
    }

    pub fn seek(&self) -> Option<u8> {
        let pos = self.pos.load(Ordering::SeqCst);
        unsafe { (pos < self.buf.len()).then_some(*self.buf.get_unchecked(pos)) }
    }

    pub fn seek_exact(&self, num_bytes: usize) -> Option<&[u8]> {
        unsafe {
            let pos = self.pos.load(Ordering::SeqCst);
            (pos + num_bytes < self.buf.len())
                .then_some(self.buf.get_unchecked(pos..).get_unchecked(..num_bytes))
        }
    }

    /// Returns `None` if either the reader is at the end of a byte stream or the conditional
    pub fn seek_eq<F: FnOnce(u8) -> bool>(&self, f: F) -> Option<u8> {
        self.buf.get(self.pos.load(Ordering::Relaxed)).filter(|x| f(**x)).cloned()
    }

    pub fn consume(&self) -> Option<u8> {
        let pos = self.pos.fetch_add(1, Ordering::AcqRel);
        unsafe { (pos < self.buf.len()).then_some(*self.buf.get_unchecked(pos)) }
    }

    pub fn consume_exact(&self, num_bytes: usize) -> Option<&[u8]> {
        unsafe {
            let pos = self.pos.fetch_add(num_bytes, Ordering::AcqRel);
            (pos < self.buf.len())
                .then_some(self.buf.get_unchecked(pos..).get_unchecked(..num_bytes))
        }
    }

    /// Returns `None` if either the reader is at the end of a byte stream or the conditional
    /// fails, on success will increment internal position.
    pub fn consume_eq<F: FnOnce(u8) -> bool>(&self, f: F) -> Option<u8> {
        let pos = self.pos.load(Ordering::Acquire);
        self.buf.get(pos).filter(|x| f(**x)).map(|x| {
            self.pos.store(pos + 1, Ordering::Release);
            *x
        })
    }

    /// Returns `None` if either the reader is at the end of a byte stream or the conditional
    /// fails, on success will increment internal position.
    pub fn consume_exact_eq<F: FnMut(u8) -> bool>(
        &self,
        mut f: F,
        num_bytes: usize,
    ) -> Option<&[u8]> {
        let pos = self.pos.load(Ordering::Acquire);

        if let Some(bytes) = self.buf.get(pos..pos + num_bytes) {
            if bytes.iter().all(|x| f(*x)) {
                self.pos.store(pos + 1, Ordering::Release);
                return Some(bytes);
            }
        }

        None
    }

    pub unsafe fn consume_into<T>(&self) -> Option<&T> {
        let size = std::mem::size_of::<T>();
        let pos = self.pos.fetch_add(size, Ordering::AcqRel);

        (size <= self.buf.len() - pos)
            .then_some(&*(self.buf.get_unchecked(pos..).as_ptr() as *const T))
    }
}

unsafe impl Send for Reader<'_> {}
unsafe impl Sync for Reader<'_> {}
