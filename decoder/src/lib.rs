//! Shared behaviour required between decoder crates. 

use tokenizing::Token;

pub trait ToTokens {
    fn tokenize(self, stream: &mut TokenStream);
}

pub trait Streamable {
    type Item: ToTokens;
    type Error;

    fn next(&mut self) -> Option<Result<Self::Item, Self::Error>>;
}

pub struct TokenStream {
    inner: [Token; 25],
    token_count: usize,
}

impl TokenStream {
    pub fn new() -> Self {
        Self {
            inner: [tokenizing::EMPTY_TOKEN; 25],
            token_count: 0,
        }
    }

    pub fn tokens(&self) -> &[tokenizing::Token] {
        &self.inner[..self.token_count]
    }

    pub fn push(&mut self, text: &'static str, color: tokenizing::Color) {
        #[cfg(debug_assertions)]
        if self.token_count == self.inner.len() {
            panic!("failed to push token");
        }

        self.inner[self.token_count] = tokenizing::Token {
            text: std::borrow::Cow::Borrowed(text),
            color
        };

        self.token_count += 1;
    }

    pub fn push_owned(&mut self, text: String, color: tokenizing::Color) {
        #[cfg(debug_assertions)]
        if self.token_count == self.inner.len() {
            panic!("failed to push token");
        }

        self.inner[self.token_count] = tokenizing::Token {
            text: std::borrow::Cow::Owned(text),
            color
        };

        self.token_count += 1;
    }
}

impl ToString for TokenStream {
    fn to_string(&self) -> String {
        self.tokens().iter().map(|t| &t.text as &str).collect()
    }
}

#[macro_export]
macro_rules! push_unsafe {
    ($v:expr, $off:expr, $c:expr) => {
        unsafe {
            *$v.get_unchecked_mut($off) = $c;
            $off += 1;
        }
    };
}

// #[rustfmt::skip]
// const ENCODED_NUGGETS: [u8; 16] = [
//     b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9',
//     b'a', b'b', b'c', b'd', b'e', b'f',
// ];
//
// #[inline(always)]
// #[rustfmt::skip]
// fn reverse_hex_nuggets(mut imm: usize) -> usize {
//     imm = (imm & 0x00000000ffffffff) << 32 | (imm & 0xffffffff00000000) >> 32;
//     imm = (imm & 0x0000ffff0000ffff) << 16 | (imm & 0xffff0000ffff0000) >> 16;
//     imm = (imm & 0x00ff00ff00ff00ff) << 8  | (imm & 0xff00ff00ff00ff00) >> 8;
//     imm = (imm & 0x0f0f0f0f0f0f0f0f) << 4  | (imm & 0xf0f0f0f0f0f0f0f0) >> 4;
//     imm
// }

// FIXME: `encode_hex` has a bug related to ilog
pub fn encode_hex(imm: i64) -> String {
    if imm.is_negative() {
        return format!("-{:#x}", -imm);
    }

    format!("{imm:#x}")

    // let mut hex = String::with_capacity(20); // max length of an i64
    // let raw = unsafe { hex.as_mut_vec() };
    // let mut off = 0;

    // if imm < 0 {
    //     push_unsafe!(raw, off, b'-');
    //     imm = -imm;
    // }

    // push_unsafe!(raw, off, b'0');
    // push_unsafe!(raw, off, b'x');

    // // log of 0 is undefined
    // if imm == 0 {
    //     push_unsafe!(raw, off, b'0');
    //     unsafe { raw.set_len(off) }
    //     return hex;
    // }

    // let num_len = imm.ilog(16) as usize + 1;
    // let leading_zeros = (16 - num_len) * 4;
    // let mut imm = reverse_hex_nuggets(imm as usize);

    // imm >>= leading_zeros;
    // for _ in 0..num_len {
    //     push_unsafe!(raw, off, ENCODED_NUGGETS[imm & 0b1111]);
    //     imm >>= 4;
    // }

    // unsafe { raw.set_len(off) }
    // hex
}
