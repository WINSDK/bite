use std::fmt;
use std::mem::MaybeUninit;

pub mod arm;
pub mod mips;
pub mod riscv;

#[allow(dead_code, unused_variables, unused_assignments)]
pub mod x86_64;

mod lookup;

pub struct Array<T, const S: usize> {
    values: [MaybeUninit<T>; S],
    len: usize,
}

impl<T, const S: usize> Array<T, S> {
    pub fn new() -> Self {
        let bytes = unsafe { MaybeUninit::uninit().assume_init() };

        Self { values: bytes, len: 0 }
    }
}

impl<T, const S: usize> Array<T, S> {
    pub fn push(&mut self, val: T) {
        assert!(
            self.len <= self.values.len(),
            "pushing to array would overflow length {}",
            self.len
        );

        self.values[self.len] = MaybeUninit::new(val);
        self.len += 1;
    }

    pub fn remove(&mut self, idx: usize)
    where
        T: Copy,
    {
        assert!(
            self.len.checked_sub(idx).map(|diff| diff > 0) == Some(true),
            "idx is {idx} which is out of bounce for len of {}",
            self.len
        );

        self.values.copy_within(idx + 1.., idx);
        self.len -= 1;
    }
}

impl<T: PartialEq, const S: usize> PartialEq for Array<T, S> {
    fn eq(&self, other: &Self) -> bool {
        let (a, b) = unsafe {
            std::mem::transmute::<(&[MaybeUninit<T>], &[MaybeUninit<T>]), (&[T], &[T])>((
                &self.values[..self.len],
                &other.values[..other.len],
            ))
        };

        a == b
    }
}

impl<T: Eq, const S: usize> Eq for Array<T, S> {}

impl<T, const S: usize> std::ops::Index<usize> for Array<T, S> {
    type Output = T;

    fn index(&self, idx: usize) -> &Self::Output {
        if idx >= self.len {
            panic!("idx `{idx}` is out of bound of array with len `{}`", self.len);
        }

        unsafe { self.values.get_unchecked(idx).assume_init_ref() }
    }
}

impl<T, const S: usize> std::ops::IndexMut<usize> for Array<T, S> {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        if idx >= self.len {
            panic!("idx `{idx}` is out of bound of array with len `{}`", self.len)
        }

        unsafe { self.values.get_unchecked_mut(idx).assume_init_mut() }
    }
}

impl<T, const S: usize> AsRef<[T]> for Array<T, S> {
    fn as_ref(&self) -> &[T] {
        unsafe { std::mem::transmute::<&[MaybeUninit<T>], &[T]>(&self.values[..self.len]) }
    }
}

impl<T, const S: usize> AsMut<[T]> for Array<T, S> {
    fn as_mut(&mut self) -> &mut [T] {
        unsafe {
            std::mem::transmute::<&mut [MaybeUninit<T>], &mut [T]>(&mut self.values[..self.len])
        }
    }
}

impl<T: fmt::Debug, const S: usize> fmt::Debug for Array<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.as_ref())
    }
}

pub struct Reader<'a, T> {
    pub buf: &'a [T],
    pub pos: usize,
}

impl<'a, T> Reader<'a, T> {
    pub fn new(buf: &'a [T]) -> Self {
        Self { buf, pos: 0 }
    }

    pub fn inner(&self) -> &'a [T] {
        &self.buf[self.pos..]
    }

    pub fn offset(&mut self, num_bytes: isize) {
        self.pos = (self.pos as isize + num_bytes) as usize;
    }

    pub fn take(&mut self, value: T) -> bool
    where
        T: PartialEq,
    {
        let eq = self.buf.get(self.pos) == Some(&value);
        self.pos += eq as usize;
        eq
    }

    pub fn take_buf(&mut self, values: &[T]) -> bool
    where
        T: PartialEq,
    {
        let eq = self.buf.get(self.pos..self.pos + values.len()) == Some(values);
        self.buf = &self.buf[(values.len()) * eq as usize..];
        eq
    }

    pub fn seek(&self) -> Option<T>
    where
        T: Copy,
    {
        self.buf.get(self.pos).copied()
    }

    pub fn consume(&mut self) -> Option<T>
    where
        T: Copy,
    {
        self.buf.get(self.pos).map(|&val| {
            self.pos += 1;
            val
        })
    }

    pub fn consume_exact(&mut self, num_bytes: usize) -> Option<&[T]> {
        self.inner().get(..num_bytes)
    }

    /// Returns `None` if either the reader is at the end of a byte stream or the conditional
    /// fails, on success will increment internal position.
    pub fn consume_eq<F: FnOnce(T) -> bool>(&mut self, f: F) -> Option<T>
    where
        T: Copy,
    {
        self.buf.get(self.pos).filter(|&&x| f(x)).map(|&val| {
            self.pos += 1;
            val
        })
    }
}
