use super::{Array, Reader};

#[derive(Debug, PartialEq, Eq)]
pub enum DecodeError {
    /// The instruction has an impossible size.
    InvalidInputSize(usize),
}

// An Intel/AMD/IA-32 instruction is made up of up to 15 bytes.
pub struct Instruction {
    prefixes: Array<Prefix, 4>,
    bytes: Array<u8, 11>,
}

/// Implementation of the basic x86_64 prefixes.
#[allow(dead_code)]
#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
enum Prefix {
    // No prefix.
    None = 0x0,

    /// Makes instruction atomic. E.g. `lock mov edx, [reg]` is just `mov edx, [reg]`
    /// with the lock prefix.
    Lock = 0xf0,

    /// Repeat instruction by x (value in ecx or ZF CPU flag) number of times.
    RepeatNotEqual = 0xf2,

    /// Repeat instruction while x does not equal value of ZF CPU flag.
    Repeat = 0xf3,

    ExtraSegment = 0x26,

    CodeSegment = 0x2e,

    StackSegment = 0x36,

    /// E.g. `mov rcx, ds:[rax+0x20]`.
    DataSegment = 0x3e,

    /// 64-bit only segment override.
    FSegment = 0x64,

    /// 64-bit only segment override.
    GSegment = 0x65,

    /// Switch between register size. E.g. `mov ax` is `mov eax` but with the size prefix.
    OperandSize = 0x66,

    /// Switch between address size. E.g. `mov [eax]` is `mov [rax]` but with the address prefix.
    AddrSize = 0x67,
}

impl Default for Prefix {
    #[inline]
    fn default() -> Self {
        Self::None
    }
}

impl Prefix {
    #[inline]
    fn translate(byte: u8) -> Self {
        match byte {
            0xf0 | 0xf2 | 0xf3 | 0x2e | 0x66 | 0x67 => unsafe { std::mem::transmute(byte) },
            _ => Prefix::None,
        }
    }
}

pub fn asm(asm_bytes: &[u8]) -> Result<Instruction, DecodeError> {
    if asm_bytes.len() < 1 || asm_bytes.len() > 15 {
        return Err(DecodeError::InvalidInputSize(asm_bytes.len()));
    }

    let asm_bytes = Reader::new(asm_bytes);

    // An instruction can have up to 4 prefixes.
    let prefixes: Array<Prefix, 4> = {
        let mut arr = Array::new();
        let mut idx = 0;

        while let Some(byte) = asm_bytes.consume_neq(Prefix::None as u8) {
            arr[idx] = Prefix::translate(byte);
            idx += 1;

            if idx == 4 || arr[idx] == Prefix::None {
                break;
            }
        }

        arr.width = idx;
        arr
    };

    while let Some(byte) = asm_bytes.consume_exact(10) {
        print!("{byte:?}");
    }

    todo!("prefixes = {:?}", prefixes);
}

#[cfg(test)]
mod test {
    #[test]
    pub fn asm() {}
}

// Every instruction has default register's to pick from
#[allow(dead_code)]
const LOOKUP_TABLE: &[&str] = &[];
