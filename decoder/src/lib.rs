//! Shared behaviour required between decoder crates.

use std::fmt::Debug;
use tokenizing::{Color, Token};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Error {
    /// What kind of error happened in decoding an instruction.
    pub kind: ErrorKind,

    /// How many bytes in the stream did the invalid instruction consume.
    size: u8,
}

impl Error {
    pub fn new(kind: ErrorKind, size: usize) -> Self {
        Self {
            kind,
            size: size as u8,
        }
    }

    pub fn size(&self) -> usize {
        self.size as usize
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ErrorKind {
    /// Opcode in instruction is impossible/unknown.
    InvalidOpcode,

    /// Operand in instruction is impossible/unknown.
    InvalidOperand,

    /// Prefix in instruction is impossible/unknown.
    InvalidPrefixes,

    /// Register in instruction is impossible/unknown.
    InvalidRegister,

    /// There weren't any bytes left in the stream to decode.
    ExhaustedInput,

    /// Impossibly long instruction (x86/64 specific).
    TooLong,

    /// Some unknown variation of errors happened.
    IncompleteDecoder,
}

pub trait ToTokens {
    fn tokenize(&self, stream: &mut TokenStream);
}

pub trait Decoded: ToTokens {
    fn width(&self) -> usize;
    fn tokens(&self) -> Vec<Token> {
        let mut stream = TokenStream::new();
        self.tokenize(&mut stream);
        stream.into_tokens()
    }
    fn find_xrefs(&mut self, _addr: usize, _symbols: &symbols::Index) {}
}

pub trait Decodable {
    type Instruction: Decoded;

    fn decode(&self, reader: &mut Reader) -> Result<Self::Instruction, Error>;
    fn max_width(&self) -> usize;
}

#[derive(Clone)]
pub struct Xref {
    pub addr: usize,
    pub text: symbols::Function,
}

#[derive(Debug)]
pub struct TokenStream {
    inner: Vec<Token>,
}

impl TokenStream {
    pub fn new() -> Self {
        Self {
            inner: Vec::with_capacity(25),
        }
    }

    pub fn push_token(&mut self, token: Token) {
        self.inner.push(token);
    }

    pub fn push(&mut self, text: &'static str, color: Color) {
        self.push_token(Token::from_str(text, color));
    }

    pub fn push_owned(&mut self, text: String, color: Color) {
        self.push_token(Token::from_string(text, color));
    }

    pub fn into_tokens(self) -> Vec<Token> {
        self.inner
    }
}

impl ToString for TokenStream {
    fn to_string(&self) -> String {
        self.inner.iter().map(|t| &t.text as &str).collect()
    }
}

pub struct Reader<'data> {
    start: *const u8,
    position: *const u8,
    end: *const u8,
    mark: *const u8,
    _marker: core::marker::PhantomData<&'data [u8]>,
}

impl<'data> Reader<'data> {
    pub fn new(data: &'data [u8]) -> Self {
        Self {
            start: data.as_ptr(),
            position: data.as_ptr(),
            end: unsafe { data.as_ptr().add(data.len()) },
            mark: data.as_ptr(),
            _marker: core::marker::PhantomData,
        }
    }

    #[inline]
    pub fn next(&mut self) -> Option<u8> {
        let width = self.end as usize - self.position as usize;

        if width == 0 {
            return None;
        }

        unsafe {
            let byte = self.position.read();
            self.position = self.position.add(1);
            Some(byte)
        }
    }

    /// read `buf`-many items from this reader in bulk. if `Reader` cannot read `buf`-many items,
    /// return `ReadError::ExhaustedInput`.
    #[inline]
    pub fn next_n(&mut self, buf: &mut [u8]) -> Option<()> {
        let width = self.end as usize - self.position as usize;

        if buf.len() > width {
            return None;
        }

        unsafe {
            core::ptr::copy_nonoverlapping(self.position, buf.as_mut_ptr(), buf.len());

            self.position = self.position.add(buf.len());
            Some(())
        }
    }

    /// mark the current position as where to measure `offset` against.
    #[inline]
    pub fn mark(&mut self) {
        self.mark = self.position;
    }

    /// the difference, between the current `Reader` position and its last `mark`.
    /// when created, a `Reader`'s initial position is `mark`ed, so creating a `Reader` and
    /// immediately calling `offset()` must return 0.
    #[inline]
    pub fn offset(&mut self) -> usize {
        self.position as usize - self.mark as usize
    }

    /// the difference, between the current `Reader` position and the initial offset
    /// when constructed.
    #[inline]
    pub fn total_offset(&mut self) -> usize {
        self.position as usize - self.start as usize
    }
}

const HEX_NUGGET: [u8; 16] = *b"0123456789abcdef";

#[inline]
#[cold]
fn cold() {}

#[inline]
fn likely(b: bool) -> bool {
    if !b {
        cold()
    }
    b
}

#[inline]
fn unlikely(b: bool) -> bool {
    if b {
        cold()
    }
    b
}

/// Encode 64-bit number with a leading '0x' and in lowercase.
pub fn encode_hex(mut imm: i64) -> String {
    unsafe {
        let mut buffer = Vec::with_capacity(19);
        let slice = &mut buffer[..];
        let mut idx = 0;

        if imm.is_negative() {
            *slice.get_unchecked_mut(idx) = b'-';
            idx += 1;
            imm = imm.wrapping_neg()
        }

        *slice.get_unchecked_mut(idx) = b'0';
        idx += 1;
        *slice.get_unchecked_mut(idx) = b'x';
        idx += 1;

        if unlikely(imm == 0) {
            *slice.get_unchecked_mut(idx) = b'0';
            idx += 1;
            buffer.set_len(idx);
            return String::from_utf8_unchecked(buffer);
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
        let mut buffer = Vec::with_capacity(bytes.len() * 3);
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
            let bytes = &bytes[..max_width / 3 - 1];

            for byte in bytes {
                *slice.get_unchecked_mut(idx) = HEX_NUGGET[(byte >> 4) as usize];
                *slice.get_unchecked_mut(idx + 1) = HEX_NUGGET[(byte & 0b1111) as usize];
                *slice.get_unchecked_mut(idx + 2) = b' ';
                idx += 3;
            }

            *slice.get_unchecked_mut(idx) = b'.';
            *slice.get_unchecked_mut(idx + 1) = b'.';
            *slice.get_unchecked_mut(idx + 2) = b' ';
            *slice.get_unchecked_mut(idx + 3) = b' ';
            idx += 4;

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
        assert_eq!(
            super::encode_hex_bytes(&[0xff, 0x1, 0x1, 0x1]),
            "ff 01 01 01 "
        );
    }

    #[test]
    fn encode_hex_bytes_truncted() {
        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 6),
            "10 ..  "
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 9),
            "10 12 03 "
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 10),
            "10 12 03  "
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 11),
            "10 12 03   "
        );
    }
}
