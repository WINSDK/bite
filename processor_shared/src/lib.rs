use object::{Pod, ReadRef};

/// Address in memory.
pub type VirtAddr = usize;

/// Address in the file world.
pub type PhysAddr = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum SectionKind {
    /// Anything we don't know how to parse or anything that is just bytes.
    Raw,
    /// Something we don't know how to parse in chunks of 4 bytes.
    Raw4,
    /// Something we don't know how to parse in chunks of 8 bytes.
    Raw8,
    /// Something we don't know how to parse in chunks of 16 bytes.
    Raw16,
    /// Instructions.
    Code,
    /// 32-bite pointers.
    Ptr32,
    /// 64-bite pointers.
    Ptr64,
    /// Not yet initialized pointers we can find by looking up the address (32-bit).
    Got32,
    /// Not yet initialized pointers we can find by looking up the address (64-bit).
    Got64,
    /// Null terminated string literals.
    CString,
    /// ExceptionDirectoryEntry's (PE only).
    ExceptionDirEntry,
    /// Elf32Sym.
    Elf32Sym,
    /// Elf64Sym.
    Elf64Sym,
    /// Elf32Dyn.
    Elf32Dyn,
    /// Elf64Dyn.
    Elf64Dyn,
    /// DWARF debug info.
    Debug,
    /// Zero sized special sections.
    Unloaded
}

#[derive(Debug, Clone)]
pub struct Section {
    /// Section name.
    pub name: String,

    /// Some more fancy Identifier.
    pub ident: &'static str,

    /// What kind of data the section holds.
    pub kind: SectionKind,

    /// Section data.
    bytes: &'static [u8],

    /// Address where section starts.
    pub start: PhysAddr,

    /// Section start + size of uncompressed data.
    pub end: PhysAddr,
}

impl Section {
    pub fn new(
        name: String,
        ident: &'static str,
        kind: SectionKind,
        bytes: &'static [u8],
        start: PhysAddr,
        end: PhysAddr,
    ) -> Self {
        Self {
            name,
            ident,
            kind,
            bytes,
            start,
            end
        }
    }

    #[inline]
    pub fn bytes(&self) -> &[u8] {
        self.bytes
    }

    pub fn bytes_by_addr(&self, addr: PhysAddr, len: usize) -> &[u8] {
        let rva = addr - self.start;
        let bytes = &self.bytes.get(rva..).unwrap_or(&[]);
        &bytes[..std::cmp::min(bytes.len(), len)]
    }

    pub fn read_at<T: Pod>(&self, addr: PhysAddr) -> Result<&T, ()> {
        let rva = addr - self.start;
        let bytes = &self.bytes.get(rva..).unwrap_or(&[]);
        bytes.read_at(rva as u64)
    }
}

#[derive(Debug)]
pub struct Segment {
    /// Segment identifier.
    pub name: String,

    /// Physical address.
    pub start: PhysAddr,

    /// Physical address + size.
    pub end: PhysAddr,
}


#[derive(Debug, Clone)]
pub struct AddressMap<T> {
    pub mapping: Vec<Addressed<T>>,
}

impl<T> AddressMap<T> {
    /// Binary search, assumes [`Self`] is sorted.
    #[inline]
    pub fn search(&self, addr: usize) -> Result<usize, usize> {
        self.mapping.binary_search_by_key(&addr, |item| item.addr)
    }

    #[inline]
    pub fn extend(&mut self, other: Self) {
        self.mapping.extend(other.mapping)
    }
}

impl<T> Default for AddressMap<T> {
    fn default() -> Self {
        Self { mapping: Vec::new() }
    }
}

impl<T> std::ops::Deref for AddressMap<T> {
    type Target = Vec<Addressed<T>>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.mapping
    }
}

impl<T> std::ops::DerefMut for AddressMap<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mapping
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Addressed<T> {
    pub addr: usize,
    pub item: T,
}

impl<T> PartialEq for Addressed<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr
    }
}

impl<T> Eq for Addressed<T> {}

impl<T> PartialOrd for Addressed<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.addr.cmp(&other.addr))
    }
}

impl<T> Ord for Addressed<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.addr.cmp(&other.addr)
    }
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
