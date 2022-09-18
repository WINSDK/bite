use std::fmt;
use std::mem::MaybeUninit;

pub mod arm;
pub mod mips;
pub mod riscv;

#[allow(dead_code, unused_variables, unused_assignments)]
pub mod x86_64;

mod lookup;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BitWidth {
    U64,
    U32,
}

pub struct Array<T, const S: usize> {
    bytes: [MaybeUninit<T>; S],
    len: usize,
}

impl<T, const S: usize> Array<T, S> {
    pub fn new() -> Self {
        let bytes = unsafe { MaybeUninit::<[MaybeUninit<T>; S]>::uninit().assume_init() };

        Self { bytes, len: 0 }
    }
}

impl<T, const S: usize> Array<T, S> {
    pub fn push(&mut self, val: T) {
        assert!(
            self.len <= self.bytes.len(),
            "pushing to array would overflow length {}",
            self.len
        );

        self.bytes[self.len] = MaybeUninit::new(val);
        self.len += 1;
    }

    pub fn remove(&mut self, idx: usize) {
        assert!(idx < self.len, "idx is {idx} which is out of bounce for len of {}", self.len);

        unsafe {
            let ptr = self.bytes.as_mut_ptr().add(idx);
            std::ptr::copy(ptr.offset(1), ptr, self.len - idx - 1);
        }

        self.len -= 1;
    }
}

impl<T: PartialEq, const S: usize> PartialEq for Array<T, S> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let a: &[T] = unsafe { std::mem::transmute(&self.bytes[..self.len]) };
        let b: &[T] = unsafe { std::mem::transmute(&other.bytes[..other.len]) };

        a == b
    }
}

impl<T: Eq, const S: usize> Eq for Array<T, S> {}

impl<T, const S: usize> std::ops::Index<usize> for Array<T, S> {
    type Output = T;

    #[inline]
    fn index(&self, idx: usize) -> &Self::Output {
        if idx >= self.len {
            panic!("idx `{idx}` is out of bound of array with len `{}`", self.len);
        }

        unsafe { self.bytes.get_unchecked(idx).assume_init_ref() }
    }
}

impl<T, const S: usize> std::ops::IndexMut<usize> for Array<T, S> {
    #[inline]
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        if idx >= self.len {
            panic!("idx `{idx}` is out of bound of array with len `{}`", self.len)
        }

        unsafe { self.bytes.get_unchecked_mut(idx).assume_init_mut() }
    }
}

impl<T, const S: usize> AsRef<[T]> for Array<T, S> {
    fn as_ref(&self) -> &[T] {
        unsafe { std::mem::transmute::<&[MaybeUninit<T>], &[T]>(&self.bytes[..self.len]) }
    }
}

impl<T, const S: usize> AsMut<[T]> for Array<T, S> {
    fn as_mut(&mut self) -> &mut [T] {
        unsafe {
            std::mem::transmute::<&mut [MaybeUninit<T>], &mut [T]>(&mut self.bytes[..self.len])
        }
    }
}

impl<T: fmt::Debug, const S: usize> fmt::Debug for Array<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.as_ref())
    }
}

pub struct Reader<'a> {
    pub buf: &'a [u8],
    pub pos: usize,
}

impl<'a> Reader<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        Self { buf, pos: 0 }
    }

    pub fn inner(&self) -> &'a [u8] {
        &self.buf[self.pos..]
    }

    pub fn offset(&mut self, num_bytes: isize) {
        self.pos = (self.pos as isize + num_bytes) as usize;
    }

    pub fn take(&mut self, byte: u8) -> bool {
        if self.buf.get(self.pos) == Some(&byte) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    pub fn take_buf(&mut self, bytes: &[u8]) -> bool {
        if self.buf.get(self.pos..self.pos + bytes.len()) == Some(bytes) {
            self.buf = &self.buf[bytes.len()..];
            true
        } else {
            false
        }
    }

    pub fn seek(&self) -> Option<u8> {
        self.buf.get(self.pos).copied()
    }

    pub fn consume(&mut self) -> Option<u8> {
        self.buf.get(self.pos).map(|&val| {
            self.pos += 1;
            val
        })
    }

    pub fn consume_exact(&mut self, num_bytes: usize) -> Option<&[u8]> {
        self.inner().get(..num_bytes)
    }

    /// Returns `None` if either the reader is at the end of a byte stream or the conditional
    /// fails, on success will increment internal position.
    pub fn consume_eq<F: FnOnce(u8) -> bool>(&mut self, f: F) -> Option<u8> {
        let pos = self.pos;
        self.buf.get(pos).filter(|&&x| f(x)).map(|&val| {
            self.pos += 1;
            val
        })
    }
}
