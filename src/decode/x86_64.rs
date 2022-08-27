use std::fmt;

use super::{lookup, Array, BitWidth, Reader};

#[derive(Debug, PartialEq, Eq)]
pub enum DecodeError {
    /// The instruction has an impossible size.
    InvalidInputSize(usize),

    /// Somehow the instruction doesn't have an opcode.
    MissingOpcode,
}

// An Intel/AMD/IA-32 instruction is made up of up to 15 bytes.
#[derive(Debug, PartialEq, Eq)]
pub struct Instruction {
    rex_prefix: Option<u8>,
    prefixes: Array<Prefix, 4>,
    repr: lookup::X86,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Addressing {
    Direct(&'static str),
    Indirect(String),
}

/// Implementation of the basic x86_64 prefixes.
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Prefix {
    /// Makes instruction atomic. E.g. `lock mov edx, [reg]` is just `mov edx, [reg]`
    /// with the lock prefix.
    Lock = 0xf0,

    /// Repeat instruction by x (value in ecx or ZF CPU flag) number of times.
    RepeatNotEqual = 0xf2,

    /// Repeat instruction while x does not equal value of ZF CPU flag.
    Repeat = 0xf3,

    /// Extra segment override.
    ES = 0x26,

    /// Stack segment override.
    SS = 0x36,

    /// Code segment override. Hint that branch is not `preferred` for jump instructions.
    CS = 0x2e,

    /// Data segment override. Hint that branch is `preferred` for jump instructions.
    DS = 0x3e,

    /// 64-bit only segment override. On windows it's used for pointing to thread information
    /// blocks in processes. Any other major OS doesn't use the prefix.
    FS = 0x64,

    /// 64-bit only segment override. A pointer to thread local storage.
    GS = 0x65,

    /// Switch between register size.
    OperandSize = 0x66,

    /// Switch between address size.
    AddrSize = 0x67,
}

#[allow(clippy::upper_case_acronyms)]
#[rustfmt::skip]
#[repr(usize)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Register {
    RAX,  RCX,  RDX,  RBX,  RSP,  RBP,  RSI,  RDI,
    EAX,  ECX,  EDX,  EBX,  ESP,  EBP,  ESI,  EDI,
    AX,   CX,   DX,   BX,   SP,   BP,   SI,   DI,
    AL,   CL,   DL,   BL,   AH,   CH,   DH,   BH,
    MM0,  MM1,  MM2,  MM3,  MM4,  MM5,  MM6,  MM7,
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[rustfmt::skip]
        pub const REPR: &[&str] = &[
            "eax",  "ecx",  "edx",  "ebx",  "esp",  "ebp",  "esi",  "edi",
            "ax",   "cx",   "dx",   "bx",   "sp",   "bp",   "si",   "di",
            "al",   "cl",   "dl",   "bl",   "ah",   "ch",   "dh",   "bh",
            "mm0",  "mm1",  "mm2",  "mm3",  "mm4",  "mm5",  "mm6",  "mm7",
            "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"
        ];

        f.write_str(REPR[unsafe { std::mem::transmute::<_, usize>(*self) }])
    }
}

#[rustfmt::skip]
#[repr(usize)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Segment {
    ES, CS, SS, DS, FS, GS
}

impl fmt::Display for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const REPR: &[&str] = &["ES", "CS", "SS", "DS", "FS", "GS"];
        f.write_str(REPR[unsafe { std::mem::transmute::<_, usize>(*self) }])
    }
}

// Derived from APPENDIX A.2.1 in the intel x86/64 instruction set reference.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AddressingMethod {
    /// The VEX.vvvv field of the VEX prefix selects a general purpose register.
    VexRegister,

    /// The VEX.vvvv field of the VEX prefix selects a 128-bit XMM register or a 256-bit YMM
    /// register, determined by operand type. For legacy SSE encodings this operand does not exist,
    /// changing the instruction to destructive form.
    WideVexRegister,

    /// The reg field of the ModR/M byte selects a 128-bit XMM register or a 256-bit YMM register,
    /// determined by operand type.
    WideRegister,

    /// The R/M field of the ModR/M byte selects a 128-bit XMM register or a 256-bit YMM register,
    /// determined by operand type.
    WideRMRegister,

    /// The upper 4 bits of the 8-bit immediate selects a 128-bit XMM register or a 256-bit YMM
    /// register, determined by operand type.
    WideImmediateRegister,

    /// The reg field of the ModR/M byte selects a general register.
    Register,

    /// The R/M field of the ModR/M byte may refer only to a general register.
    RMRegister,

    /// The reg field of the ModR/M byte selects a control register.
    ControlRegister,

    /// The reg field of the ModR/M byte selects a debug register
    DebugRegister,

    /// The reg field of the ModR/M byte selects a packed-quadword, MMX register (sse predecessor).
    MXXRegister,

    /// The R/M field of the ModR/M byte selects a packed-quadword, MMX register (sse predecessor).
    RMMXXRegister,

    /// The reg field of the ModR/M byte selects a segment register
    SegmentRegister,

    /// EFLAGS/RFLAGS Register.
    FlagsRegister,

    FixedSegment(Segment),

    /// Hardcoded registers.
    FixedRegister(Register),

    /// Hardcoded set of registers.
    FixedRegisters(&'static [Register]),

    /// The instruction has no ModR/M byte. The offset of the operand is coded as a word or double
    /// word (depending on address size attribute) in the instruction. No base register, index
    /// register, or scaling factor can be applied.
    NoModRM,

    /// A ModR/M byte follows the opcode and specifies the operand. The operand is either a
    /// general-purpose register or a memory address. If it is a memory address, the address is
    /// computed from a segment register and any of the following values: a base register, an index
    /// register, a scaling factor, a displacement.
    ModRM,

    /// The operand value is encoded in subsequent bytes of the instruction.
    Immediate,

    /// The instruction contains a relative offset to be added to the instruction pointer register.
    RelativeOffset,

    /// The instruction has no ModR/M byte; the address of the operand is encoded in the
    /// instruction. No base register, index register, or scaling factor can be applied.
    Direct,

    /// The ModR/M byte may refer only to memory.
    Memory,

    /// A ModR/M byte follows the opcode and specifies the operand. The operand is either an MMX
    /// technology register or a memory address. If it is a memory address, the address is computed
    /// from a segment register and any of the following values: a base register, an index
    /// register, a scaling factor, and a displacement.
    WhatTheFuckIsThisMode,

    /// A ModR/M byte follows the opcode and specifies the operand. The operand is either a 128-bit
    /// XMM register, a 256-bit YMM register (determined by operand type), or a memory address. If
    /// it is a memory address, the address is computed from a segment register and any of the
    /// following values: a base register, an index register, a scaling factor, and a displacement.
    WhatTheFuckIsThisAswell,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OperandType {
    /// 8-bit integer, regardless of operand-size attribute.
    U8,

    /// 16-bit integer, regardless of operand-size attribute.
    U16,

    /// Signed 16-bit, regardless of operand-size attribute (only exists for x87 FPU instructions).
    I16,

    /// 32-bit integer, regardless of operand-size attribute.
    U32,

    /// Signed 32-bit, regardless of operand-size attribute (only exists for x87 FPU instructions).
    I32,

    /// 64-bit integer, regardless of operand-size attribute.
    U64,

    /// Signed 64-bit, regardless of operand-size attribute (only exists for x87 FPU instructions).
    I64,

    /// 64-bit integer, depending on if REX.W is set. I'm confused by the difference
    /// between this and `DoubleOrQuad` or even a regular u64.
    PromotedU64,

    /// 128-bit integer, regardless of operand-size attribute.
    U128,

    /// Two 16-bit operands in memory or two 32-bit operands in memory, depending on
    /// operand-size attribute (only used by `bound` instruction).
    DoubleMemory,

    /// Byte, sign-extended to the size of the destination operand.
    ByteOperandExtended,

    /// Byte, sign-extened to 64 bits.
    Byte64BitExtended,

    /// Byte, sign-extended to the size of the stack pointer
    ByteStackExtended,

    /// Byte made up of 2 `digits` sized 4 bits each (only exists for x87 FPU instructions).
    PackedByte,

    /// 8/16/32-bit integer, sign-extended extended to the size of the first operand.
    ByteOrSingleOrDoubleExtended,

    /// 16/32-bit integer, depending on operand-size attribute.
    SingleOrDouble,

    /// 16/32-bit integer, sign extended to the size of the stack pointer.
    SingleOrDoubleExtended,

    /// 16/32-bit integer, depending on operand-size attribute, or sign-extended to 64 bits for
    /// 64-bit operand size.
    SingleOrDoubleQuadExtended,

    /// 16/64-bit integer, depending on if the operand-size prefix is set.
    SingleOrQuad,

    /// 16/32-bit integer, depending on if the operand-size prefix is set or 64-bit depending on
    /// whether REX.W is set in 64-bit mode
    SingleOrDoubleOrQuad,

    /// 32/64-bit integer, depends on whether REX.W is set in 64-bit mode.
    DoubleOrQuad,

    /// Single-real. Only x87 FPU instructions.
    SingleReal,

    /// Double-real. Only x87 FPU instructions
    DoubleReal,

    /// 32-bit integer, sign-extended to 64 bits
    U32Extended,

    /// x87 FPU environment
    Environment,

    /// x87 FPU state
    FPUState,

    /// x87 FPU and SIMD state
    SIMDState,

    /// Extended-real. Only x87 FPU instructions (for example, FLD).
    ExtendedReal,

    /// 64-bit MMX?
    MMXData,

    /// 128-bit packed double-precision floating-point digits.
    PackedDouble,

    /// Scalar element of a 128-bit packed double-precision floating digits.
    ScalerPackedDouble,

    /// 128-bit packed single-precision floating-point digits.
    PackedSingle,

    /// Scalar element of a 128-bit packed single-precision floating data.
    ScalerPackedSingle,

    /// 64-bit packed single-precision floating-point digits.
    PackedSingleSmall,

    /// 32-bit or 48-bit pointer, depending on operand-size attribute
    Pointer,

    /// 32-bit or 48-bit pointer, depending on operand-size attribute, or 80-bit far pointer
    /// depending on whether REX.W is set in 64-bit mode.
    FatPointer,

    /// 6-byte pseudo-descriptor, or 10-byte pseudo-descriptor in 64-bit mode.
    PsuedoDescriptor,

    /// Doubleword integer register.
    #[allow(dead_code)]
    DoubleRegister,
}

pub fn asm(width: BitWidth, raw_bytes: &[u8]) -> Result<Instruction, DecodeError> {
    if raw_bytes.is_empty() || raw_bytes.len() > 15 {
        return Err(DecodeError::InvalidInputSize(raw_bytes.len()));
    }

    let mut bytes = Reader::new(raw_bytes);

    // read instruction prefixes (0-4 optional bytes).
    let mut prefixes = Array::<Prefix, 4>::new();
    for _ in 0..4 {
        if let Some(prefix @ (0xf0 | 0xf2 | 0xf3 | 0x2e | 0x66 | 0x67)) = bytes.seek() {
            prefixes.push(unsafe { std::mem::transmute(prefix) });
        } else {
            break;
        }
    }

    // check if opcode starts with a `required prefix` + escape opcode or just escape opcode.
    let multibyte = match prefixes.as_ref().get(0) {
        Some(Prefix::OperandSize | Prefix::RepeatNotEqual | Prefix::Repeat) if bytes.take(0x0f) => {
            prefixes.remove(0);
            true
        },
        _ => bytes.take(0x0f),
    };

    // read optional REX prefix (1 byte).
    let mut rex_prefix = None;
    if let Some(prefix) = bytes.consume_eq(|x| (x >> 4) == 0b0100) {
        if width == BitWidth::U64 {
            debug_assert_eq!(prefix >> 4, 0b0100);

            let is_64_operand = (prefix & 0b00001000) == 0b00001000;
            let mod_rm_reg = (prefix & 0b00000100) == 0b00000100;
            let sib_idx_field = (prefix & 0b00000010) == 0b00000010;
            let extension = (prefix & 0b00000001) == 0b00000001;

            rex_prefix = Some(prefix);
        }
    }

    let opcode = bytes.consume_exact(multibyte as usize + 1).ok_or(DecodeError::MissingOpcode)?;

    println!("{:x?}", opcode);

    // read opcode bytes (1-3 bytes).
    let repr = if multibyte {
        todo!()
    } else {
        let opcode = opcode[0];

        let row = (opcode & 0b11110000) >> 4;
        let col = (opcode & 0b00001111) >> 0;


        eprintln!("row: 0x{row:x}, col: 0x{col:x}");
        dbg!(lookup::X86_SINGLE[row as usize][col as usize])
    };

    // read ModR/M (optional 1 byte).
    //
    //    2        3         2
    // ┌─────┬────────────┬─────┐
    // │ mod │ reg/opcode │ R/M │
    // └─────┴────────────┴─────┘
    //
    if let Some(byte) = bytes.consume() {
        let memory_addressing_mode = byte >> 6;
        let register_addressing_mode = (byte & 0b00111111) >> 3;
        let memory_operand_mode = byte & 0b00000111;

        let is_64bit_addr = prefixes.as_ref().contains(&Prefix::AddrSize);

        dbg!(is_64bit_addr);
        eprintln!("addressing_mode: 0b{:02b}", memory_operand_mode);
        eprintln!("0b{byte:08b}");

        // match memory_addressing_mode {
        //     0b00 => {
        //         // Displacement only addressing.
        //         if register_addressing_mode == 0b110 {}
        //     }
        //     // Register addressing mode
        //     0b11 => if is_64bit_addr {},
        // }

        // the ModR/M is a register.
        if byte >= 0xc0 {
            panic!("{}", unsafe { std::mem::transmute::<_, Register>(byte as usize - 0xc0) });
        }

        panic!("0x{:x}", byte);
    }

    // Read SIB (optional 1 byte).
    //
    //     2       3      3
    // ┌───────┬───────┬──────┐
    // │ scale │ index │ base │
    // └───────┴───────┴──────┘
    //

    // A displacement is the constant offset that is applied when reading memory.
    // E.g. mov edx, [rsp + displacement]

    // Read displacement (0-4 optional bytes).

    // Read immediate (0-4 optional bytes).

    let mut displacement = Array::<u8, 4>::new();
    while let Some(byte) = bytes.consume() {
        displacement.push(byte);
    }

    Ok(Instruction { prefixes, repr, rex_prefix })
}

#[cfg(test)]
mod tests {
    macro_rules! eq {
        ($bitness:tt, [$($bytes:tt),+] => $repr:expr) => {
            assert_eq!(
                $crate::decode::x86_64::asm(
                    $crate::decode::BitWidth::$bitness,
                    &[$($bytes),+]
                ).map(|x| x.to_string()).as_deref(),
                Ok($repr)
            )
        };
    }

    #[test]
    pub fn random_complex() {
        eq!(U64, [0xf3, 0x48, 0xa5] => "rep movsq qword ptr es:[rdi], qword ptr [rsi]");
        eq!(U64, [0x66, 0x0f, 0x38, 0x01, 0xc1] => "phaddw xmm0, xmm1");
        eq!(U64, [0x5d] => "pop rbp");
    }

    #[test]
    fn rex_prefix() {
        eq!(U64, [0xf3, 0x41, 0x0f, 0xe6, 0x02] => "cvtdq2pd xmm0, qword ptr [r10]");
    }

    #[test]
    fn add() {
        eq!(U64, [0x48, 0x83, 0xc0, 0x0a] => "add rax, 10")
    }

    #[test]
    fn mov() {
        eq!(U64, [0xb8, 0x1, 0x0, 0x0, 0x0] => "mov eax, 10")
    }
}

// Segment prefixes are ignored for jump instructions.
