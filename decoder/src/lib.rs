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

const HEX_NUGGET: [u8; 16] = *b"0123456789abcdef";

#[inline]
#[cold]
fn cold() {}

#[inline]
fn likely(b: bool) -> bool {
    if !b { cold() }
    b
}

#[inline]
fn unlikely(b: bool) -> bool {
    if b { cold() }
    b
}

/// Encode 64-bit number with a leading '0x' and in lowercase.
pub fn encode_hex(mut imm: i64) -> String {
    unsafe {
        let mut buffer = Vec::with_capacity(19);
        let slice = &mut buffer[..];
        let mut idx = 0;

        if imm.is_negative() {
            imm = -imm;
            *slice.get_unchecked_mut(idx) = b'-';
            idx += 1;
        }

        *slice.get_unchecked_mut(idx) = b'0';
        idx += 1;
        *slice.get_unchecked_mut(idx) = b'x';
        idx += 1;

        if unlikely(imm == 0) {
            *slice.get_unchecked_mut(idx) = b'0';
            idx += 1;
            buffer.set_len(idx);
            return String::from_utf8_unchecked(buffer)
        }

        // imm is already checked to not be zero, therefore this can't fail
        let len = imm.checked_ilog(16).unwrap_unchecked() as usize + 1;
        let mut jdx = idx + len;

        while likely(jdx != idx) {
            let digit = imm & 0b1111;
            let chr = HEX_NUGGET[digit as usize];

            imm >>= 4;
            jdx -= 1;

            *slice.get_unchecked_mut(jdx) = chr;
        }

        buffer.set_len(idx + len);
        String::from_utf8_unchecked(buffer)
    }
}

/// Encode bytes as 2 digit hex number separated by a space with a leading space.
pub fn encode_hex_bytes(bytes: &[u8]) -> String {
    unsafe {
        let mut buffer = Vec::with_capacity(bytes.len());
        let slice = &mut buffer[..];
        let mut idx = 0;

        for byte in bytes {
            *slice.get_unchecked_mut(idx) = HEX_NUGGET[(byte >> 4) as usize];
            *slice.get_unchecked_mut(idx + 1) = HEX_NUGGET[(byte & 0b1111) as usize];
            *slice.get_unchecked_mut(idx + 2) = b' ';
            idx += 3;
        }

        buffer.set_len(idx);
        String::from_utf8_unchecked(buffer)
    }
}

/// Truncates string past the max width with a '..'.
pub fn encode_hex_bytes_truncated(bytes: &[u8], max_width: usize) -> String {
    unsafe {
        assert!(max_width > 2, "max width most be at least 2");

        let pad = max_width.saturating_sub(bytes.len() * 3);
        let mut buffer = Vec::with_capacity(bytes.len() * 3 + pad);
        let slice = &mut buffer[..];
        let mut idx = 0;

        // truncation has to occur
        if bytes.len() * 3 > max_width {
            let bytes = &bytes[..max_width / 3];

            for byte in bytes {
                *slice.get_unchecked_mut(idx) = HEX_NUGGET[(byte >> 4) as usize];
                *slice.get_unchecked_mut(idx + 1) = HEX_NUGGET[(byte & 0b1111) as usize];
                *slice.get_unchecked_mut(idx + 2) = b' ';
                idx += 3;
            }

            *slice.get_unchecked_mut(idx) = b'.';
            *slice.get_unchecked_mut(idx + 1) = b'.';
            *slice.get_unchecked_mut(idx + 2) = b' ';
            idx += 3;

            buffer.set_len(idx);
            return String::from_utf8_unchecked(buffer);
        }

        for byte in bytes {
            *slice.get_unchecked_mut(idx) = HEX_NUGGET[(byte >> 4) as usize];
            *slice.get_unchecked_mut(idx + 1) = HEX_NUGGET[(byte & 0b1111) as usize];
            *slice.get_unchecked_mut(idx + 2) = b' ';
            idx += 3;
        }

        for _ in 0..pad {
            *slice.get_unchecked_mut(idx) = b' ';
            idx += 1;
        }

        buffer.set_len(idx);
        String::from_utf8_unchecked(buffer)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn encode_hex() {
        assert_eq!(super::encode_hex(0x123123), "0x123123");
        assert_eq!(super::encode_hex(-0x123123), "-0x123123");
        assert_eq!(super::encode_hex(-0x48848), "-0x48848");

        assert_eq!(super::encode_hex(0x0), "0x0");
        assert_eq!(super::encode_hex(-0x800000000000000), "-0x800000000000000");
        assert_eq!(super::encode_hex(0x7fffffffffffffff), "0x7fffffffffffffff");
    }

    #[test]
    fn encode_hex_bytes() {
        assert_eq!(super::encode_hex_bytes(&[0x10, 0x12, 0x3]), "10 12 03 ");
        assert_eq!(super::encode_hex_bytes(&[0x10]), "10 ");
        assert_eq!(super::encode_hex_bytes(&[0xff, 0x1, 0x1, 0x1]), "ff 01 01 01 ");
    }

    #[test]
    fn encode_hex_bytes_truncted() {
        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 5),
            "10 .. "
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 7),
            "10 12 .. "
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 8),
            "10 12 .. "
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 9),
            "10 12 03 "
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 10),
            "10 12 03  "
        );
    }
}
