use super::{consume, Array};
use std::io;

// The maximum length of an Intel 64 and IA-32 instruction remains 15 bytes.
pub struct Instruction {
    bytes: Array<u8, 15>,
}

/// Implementation of the basic x86_64 prefixes.
#[allow(dead_code)]
#[repr(u8)]
#[derive(Debug)]
enum Prefix {
    Empty = 0x0,

    /// Makes instruction atomic. E.g. `lock mov edx, [reg]` is just `mov edx, [reg]`
    /// with the lock prefix.
    Lock = 0xf0,

    /// Repeat instruction by x (value in ecx or ZF CPU flag) number of times.
    RepeatNotEqual = 0xf2,

    /// Repeat instruction while x does not equal value of ZF CPU flag.
    Repeat = 0xf3,

    /// Magic to me.
    Segment = 0x2e,

    /// Switch between register size. E.g. `mov ax` is `mov eax` but with the size prefix.
    OperandSize = 0x66,

    /// Switch between address size. E.g. `mov [eax]` is `mov [rax]` but with the address prefix.
    AddrSize = 0x67,
}

impl Default for Prefix {
    #[inline]
    fn default() -> Self {
        Self::Empty
    }
}

impl Prefix {
    #[inline]
    fn matches(byte: u8) -> Option<Self> {
        match byte {
            0xf0 | 0xf2 | 0xf3 | 0x2e | 0x66 | 0x67 => unsafe { std::mem::transmute(byte) },
            _ => None,
        }
    }
}

pub fn asm(asm_block: &[u8]) -> io::Result<Instruction> {
    let mut asm_block = std::io::BufReader::new(asm_block);

    // An instruction can have up to 4 prefixes.
    let prefixes: Array<Prefix, 4> = {
        let mut arr = Array::new();
        let mut idx = 0;

        while let Some(prefix) = Prefix::matches(consume!(asm_block, u8)?) {
            arr[idx] = prefix;
            idx += 1;
        }

        arr.width = idx;
        arr
    };

    while let Ok(byte) = consume!(asm_block, u8) {
        print!("{byte}");
    }

    todo!("prefixes = {:?}", prefixes);
}

#[cfg(test)]
mod test {
    #[test]
    pub fn asm() {}
}

#[allow(dead_code)]
const LOOKUP_TABLE: &[&str] = &[];
