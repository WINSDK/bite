use object::SectionKind;
use std::borrow::Cow;

/// Address in memory.
pub type VirtAddr = usize;

/// Address in the file world.
pub type PhysAddr = usize;

#[derive(Debug, Clone)]
pub struct Section {
    /// Section identifier.
    pub name: Cow<'static, str>,

    /// What kind of data the section holds.
    pub kind: SectionKind,

    /// Whether or not it's being shown in the ASM listing as regular code/data.
    pub loaded: bool,

    /// Section data.
    bytes: &'static [u8],

    /// Virtual address.
    pub addr: VirtAddr,

    /// Address where section starts.
    pub start: PhysAddr,

    /// Section start + size of uncompressed data.
    pub end: PhysAddr,
}

impl Section {
    pub fn new(
        name: Cow<'static, str>,
        kind: SectionKind,
        loaded: bool,
        bytes: &'static [u8],
        addr: VirtAddr,
        start: PhysAddr,
        end: PhysAddr,
    ) -> Self {
        Self {
            name,
            kind,
            loaded,
            bytes,
            addr,
            start,
            end
        }
    }

    #[inline]
    pub fn bytes<'a>(&'a self) -> &'a [u8] {
        self.bytes
    }

    pub fn bytes_by_addr<'a>(&self, addr: PhysAddr, len: usize) -> &'a [u8] {
        let rva = addr - self.start;
        let bytes = &self.bytes[rva..];
        let bytes = &bytes[..std::cmp::min(bytes.len(), len)];
        bytes
    }
}

#[derive(Debug)]
pub struct Segment {
    /// Segment identifier.
    pub name: Cow<'static, str>,

    /// Physical address.
    pub start: PhysAddr,

    /// Physical address + size.
    pub end: PhysAddr,
}

/// Truncates string past the max width with a '..'.
pub fn encode_hex_bytes_truncated(bytes: &[u8], max_width: usize, is_padded: bool) -> String {
    unsafe {
        const HEX_NUGGET: [u8; 16] = *b"0123456789abcdef";

        assert!(max_width > 2, "max width most be at least 2");

        let len = bytes.len() * 3;
        let pad = is_padded as usize * max_width.saturating_sub(len);
        let mut buffer = Vec::with_capacity(len + pad);
        let slice = &mut buffer[..];
        let mut idx = 0;

        // truncation has to occur
        if bytes.len() * 3 > max_width {
            let bytes = &bytes[..max_width / 3 - 1];

            for byte in bytes {
                *slice.get_unchecked_mut(idx) = HEX_NUGGET[(byte >> 4) as usize];
                *slice.get_unchecked_mut(idx + 1) = HEX_NUGGET[(byte & 0b1111) as usize];
                idx += 2;

                if idx + 1 != len || is_padded {
                    *slice.get_unchecked_mut(idx) = b' ';
                    idx += 1;
                }
            }

            *slice.get_unchecked_mut(idx) = b'.';
            *slice.get_unchecked_mut(idx + 1) = b'.';
            idx += 2;

            if is_padded {
                *slice.get_unchecked_mut(idx) = b' ';
                *slice.get_unchecked_mut(idx + 1) = b' ';
                idx += 2;
            }

            buffer.set_len(idx);
            return String::from_utf8_unchecked(buffer);
        }

        for byte in bytes {
            *slice.get_unchecked_mut(idx) = HEX_NUGGET[(byte >> 4) as usize];
            *slice.get_unchecked_mut(idx + 1) = HEX_NUGGET[(byte & 0b1111) as usize];
            idx += 2;

            if idx + 1 != len || is_padded {
                *slice.get_unchecked_mut(idx) = b' ';
                idx += 1;
            }
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
    fn encode_hex_bytes_truncted() {
        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 6, false),
            "10 .."
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 6, true),
            "10 ..  "
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 9, false),
            "10 12 03"
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 10, true),
            "10 12 03  "
        );

        assert_eq!(
            super::encode_hex_bytes_truncated(&[0x10, 0x12, 0x3], 11, true),
            "10 12 03   "
        );
    }
}
