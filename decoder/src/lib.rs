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

impl std::ops::Deref for TokenStream {
    type Target = [Token];

    fn deref(&self) -> &Self::Target {
        &self.inner[..self.token_count]
    }
}

impl ToString for TokenStream {
    fn to_string(&self) -> String {
        self.iter().map(|t| &t.text as &str).collect()
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

pub fn encode_hex_bytes(bytes: &[u8]) -> String {
    let mut repr = String::with_capacity(bytes.len() * 3);

    const TABLE: &[u8] = "0123456789ABCDEF".as_bytes();

    for byte in bytes {
        repr.push(TABLE[(byte >> 4) as usize] as char);
        repr.push(TABLE[(byte & 0x0f) as usize] as char);
        repr.push(' ');
    }

    repr
}

/// Truncates string past the max width with a '..'.
pub fn encode_hex_bytes_padded(bytes: &[u8], max_width: usize) -> String {
    assert!(max_width > 2, "max width most be at least 2");

    let pad = max_width.saturating_sub(bytes.len() * 3);
    let mut repr = String::with_capacity(bytes.len() * 3 + pad);

    const TABLE: &[u8] = "0123456789ABCDEF".as_bytes();

    // truncation has to occur
    if bytes.len() * 3 > max_width {
        let bytes = &bytes[..(max_width - 2) / 3];

        for byte in bytes {
            repr.push(TABLE[(byte >> 4) as usize] as char);
            repr.push(TABLE[(byte & 0x0f) as usize] as char);
            repr.push(' ');
        }

        repr.push_str("..  ");
        return repr;
    }

    for byte in bytes {
        repr.push(TABLE[(byte >> 4) as usize] as char);
        repr.push(TABLE[(byte & 0x0f) as usize] as char);
        repr.push(' ');
    }

    for _ in 0..pad {
        repr.push(' ');
    }

    repr
}
