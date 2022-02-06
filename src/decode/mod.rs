use std::mem::MaybeUninit;
use std::fmt;

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

macro_rules! consume_exact {
    ($reader:expr, $byte_count:expr) => {{
        use std::io::Read;

        let mut buf = [0u8; $byte_count];
        let res = $reader.read_exact(&mut buf);
        res.map(|_| buf)
    }};
}

macro_rules! consume {
    ($reader:expr, $ty:ident) => {{
        use std::io::Read;

        let mut buf = [0u8; std::mem::size_of::<$ty>()];
        let res = $reader.read_exact(&mut buf);
        res.map(|_| <$ty>::from_le_bytes(buf))
    }};
}

pub(crate) use consume;
pub(crate) use consume_exact;
