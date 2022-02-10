use std::fmt;
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicUsize, Ordering};

pub mod arm;
pub mod riscv;
pub mod x86_64;

mod lookup;

pub(crate) struct Array<T, const S: usize> {
    bytes: [T; S],
    pub width: AtomicUsize,
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

        Self { bytes: unsafe { bytes.assume_init() }, width: AtomicUsize::new(0) }
    }

    pub unsafe fn uninit() -> Self {
        Self { bytes: MaybeUninit::uninit().assume_init(), width: AtomicUsize::new(0) }
    }
}

impl<T, const S: usize> std::ops::Index<usize> for Array<T, S> {
    type Output = T;

    #[inline]
    fn index(&self, idx: usize) -> &Self::Output {
        self.width.fetch_max(idx, Ordering::SeqCst);
        &self.bytes[idx]
    }
}

impl<T, const S: usize> std::ops::IndexMut<usize> for Array<T, S> {
    #[inline]
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        self.width.fetch_max(idx, Ordering::SeqCst);
        &mut self.bytes[idx]
    }
}

impl<T: fmt::Debug, const S: usize> fmt::Debug for Array<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.width.load(Ordering::SeqCst);
        f.write_fmt(format_args!("{:?}", &self.bytes[..self.width.load(Ordering::SeqCst)]))
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
        let pos = self.pos.fetch_add(num_bytes, Ordering::AcqRel);
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

    pub fn consume_eq(&self, other: u8) -> Option<u8> {
        let pos = self.pos.load(Ordering::Acquire);
        if pos < self.buf.len() {
            self.pos.store(pos + 1, Ordering::Release);
            if self.buf[pos] == other {
                return Some(unsafe { *self.buf.get_unchecked(pos) });
            }
        }

        None
    }

    pub fn consume_neq(&self, other: u8) -> Option<u8> {
        let pos = self.pos.load(Ordering::Acquire);
        if pos < self.buf.len() {
            self.pos.store(pos + 1, Ordering::Release);
            if self.buf[pos] != other {
                return Some(unsafe { *self.buf.get_unchecked(pos) });
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
