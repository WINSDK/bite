use std::fmt;
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicUsize, Ordering};

pub mod arm;
pub mod riscv;
pub mod x86_64;

pub(crate) struct Array<T, const S: usize> {
    bytes: [T; S],
    pub width: usize,
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

        Self { bytes: unsafe { bytes.assume_init() }, width: S }
    }

    pub unsafe fn uninit() -> Self {
        Self { bytes: MaybeUninit::uninit().assume_init(), width: S }
    }
}

impl<T, const S: usize> std::ops::Deref for Array<T, S> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.bytes[..self.width]
    }
}

impl<T, const S: usize> std::ops::DerefMut for Array<T, S> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bytes[..self.width]
    }
}

impl<T: fmt::Debug, const S: usize> fmt::Debug for Array<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", &**self))
    }
}

pub(crate) struct Reader<'a> {
    buf: &'a [u8],
    pos: AtomicUsize,
}

#[allow(dead_code)]
impl<'a> Reader<'a> {
    pub const fn new(buf: &'a [u8]) -> Self {
        Self { buf, pos: AtomicUsize::new(0) }
    }

    pub fn increment(&self, num_bytes: usize) {
        let pos = self.pos.fetch_add(num_bytes, Ordering::SeqCst);
        assert!(pos < self.buf.len());
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

    pub fn consume(&self) -> Option<u8> {
        let pos = self.pos.fetch_add(1, Ordering::SeqCst);
        unsafe { (pos < self.buf.len()).then_some(*self.buf.get_unchecked(pos)) }
    }

    pub fn consume_exact(&self, num_bytes: usize) -> Option<&[u8]> {
        unsafe {
            let pos = self.pos.fetch_add(num_bytes, Ordering::SeqCst);
            (pos < self.buf.len())
                .then_some(self.buf.get_unchecked(pos..).get_unchecked(..num_bytes))
        }
    }

    pub fn consume_eq(&self, other: u8) -> Option<u8> {
        let pos = self.pos.fetch_add(1, Ordering::SeqCst);
        if pos < self.buf.len() {
            if self.buf[pos] == other {
                return Some(unsafe { *self.buf.get_unchecked(pos) });
            }
        }

        None
    }

    pub fn consume_neq(&self, other: u8) -> Option<u8> {
        let pos = self.pos.fetch_add(1, Ordering::SeqCst);
        if pos < self.buf.len() {
            if self.buf[pos] != other {
                return Some(unsafe { *self.buf.get_unchecked(pos) });
            }
        }

        None
    }

    pub unsafe fn consume_into<T>(&self) -> Option<&T> {
        (std::mem::size_of::<T>() <= self.pos.load(Ordering::SeqCst))
            .then_some(&*(self.buf[self.pos.load(Ordering::SeqCst)..].as_ptr() as *const T))
    }
}
