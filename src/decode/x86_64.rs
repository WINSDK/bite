#![allow(dead_code)]

use std::fmt;

use super::{lookup, Array, BitWidth, Reader};

#[derive(Debug, PartialEq, Eq)]
pub enum DecodeError {
    /// The instruction has an impossible size.
    InvalidInputSize(usize),
}

// An Intel/AMD/IA-32 instruction is made up of up to 15 bytes.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Instruction {
    rex_prefix: Option<u8>,
    prefixes: Array<Prefix, 4>,
    bytes: Array<u8, 11>,
    repr: lookup::X86,
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
    // No prefix.
    None = 0x0,

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

#[rustfmt::skip]
#[repr(usize)]
#[derive(PartialEq, Eq, Clone, Copy)]
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
        pub const REPR: &[&'static str] = &[
            "eax",  "ecx",  "edx",  "ebx",  "esp",  "ebp",  "esi",  "edi",
            "ax",   "cx",   "dx",   "bx",   "sp",   "bp",   "si",   "di",
            "al",   "cl",   "dl",   "bl",   "ah",   "ch",   "dh",   "bh",
            "mm0",  "mm1",  "mm2",  "mm3",  "mm4",  "mm5",  "mm6",  "mm7",
            "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"
        ];

        f.write_str(REPR[unsafe { std::mem::transmute::<_, usize>(*self) }])
    }
}

impl fmt::Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self))
    }
}

#[rustfmt::skip]
#[repr(usize)]
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Segment {
    ES, CS, SS, DS, FS, GS
}

impl fmt::Display for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        pub const REPR: &[&'static str] = &["ES", "CS", "SS", "DS", "FS", "GS"];
        f.write_str(REPR[unsafe { std::mem::transmute::<_, usize>(*self) }])
    }
}

impl fmt::Debug for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self))
    }
}

// Derived from APPENDIX A.2.1 in the intel x86/64 instruction set reference.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AddressingMethod {
    /// The VEX.vvvv field of the VEX prefix selects a general purpose register.
    GeneralVexRegister,

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
    GeneralRegister,

    /// The R/M field of the ModR/M byte may refer only to a general register.
    RMGeneralRegister,

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
    ImmediateData,

    /// The instruction contains a relative offset to be added to the instruction pointer register.
    RelativeOffset,

    /// The instruction has no ModR/M byte; the address of the operand is encoded in the
    /// instruction. No base register, index register, or scaling factor can be applied.
    Direct,

    /// The ModR/M byte may refer only to memory.
    Memory,

    /// Memory addressed by the DS:rSI register pair (for example, MOVS, CMPS, OUTS, or LODS).
    DataSegmentRSIRegister,

    /// Memory addressed by the ES:rDI register pair (for example, MOVS, CMPS, INS, STOS, or SCAS).
    ExtraSegmentRDIRegister,

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

    /// Two 32-bit operands in memory or two 64-bit operands in memory, depending on
    /// operand-size attribute.
    DoubleMemory,

    /// Byte, sign-extended to the size of the destination operand.
    ByteOperandExtended,

    /// Byte, sign-extened to 64 bits.
    Byte64BitExtended,

    /// Byte, sign-extended to the size of the stack pointer
    ByteStackExtended,

    /// Byte made up of 2 `digits` sized 4 bits each (only exists for x87 FPU instructions).
    PackedByte,

    /// 8/16-bit integer, depending on operand-size attribute.
    #[allow(dead_code)]
    ByteOrDouble,

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

impl Default for Prefix {
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

pub fn asm(width: BitWidth, asm_bytes: &[u8]) -> Result<Instruction, DecodeError> {
    let asm_reader = Reader::new(asm_bytes);
    if asm_bytes.len() < 1 || asm_bytes.len() > 15 {
        return Err(DecodeError::InvalidInputSize(asm_bytes.len()));
    }

    // Read instruction prefixes (1-4 optional bytes).
    let mut prefixes: Array<Prefix, 4> = Array::new();
    for idx in 0..4 {
        if let Some(prefix) = asm_reader.consume_eq(|x| Prefix::translate(x) != Prefix::None) {
            prefixes[idx] = unsafe { std::mem::transmute(prefix) };
        }
    }

    // Read optional REX prefix (1 byte).
    let mut rex_prefix = None;
    if width == BitWidth::U64 {
        if let Some(prefix) = asm_reader.consume_eq(|x| (x >> 4) == 0b0100) {
            debug_assert_eq!(prefix >> 4, 0b0100);

            let is_64_operand = (prefix & 0b00001000) == 0b00001000;
            let mod_rm_reg = (prefix & 0b00000100) == 0b00000100;
            let sib_idx_field = (prefix & 0b00000010) == 0b00000010;
            let extension = (prefix & 0b00000001) == 0b00000001;

            dbg!(is_64_operand);
            rex_prefix = Some(prefix);
        }
    }

    // Check if opcode is multibyte.
    let mut multibyte = false;
    if prefixes[0] as u8 == 0x66 || prefixes[0] as u8 == 0xf2 || prefixes[0] as u8 == 0xf3 {
        debug_assert!(asm_bytes.len() > prefixes.len());

        if asm_reader.consume_eq(|x| x == 0x0f).is_some() {
            multibyte = true;
            prefixes.remove(0);
        }
    }

    // Read opcode bytes (1-3 bytes).
    let repr = todo!("{:?}", lookup::X86_SINGLE[asm_reader.consume().unwrap() as usize]);

    // Read ModR/M (optional 1 byte).
    //
    //    2        3         2
    // ┌─────┬────────────┬─────┐
    // │ mod │ reg/opcode │ R/M │
    // └─────┴────────────┴─────┘
    //
    let mut displacement = 0;
    if let Some(byte) = asm_reader.consume() {
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

    // Read displacement (1-4 optional bytes).

    // Read immediate (1-4 optional bytes).

    let (mut idx, mut bytes) = (0, Array::new());
    while let Some(byte) = asm_reader.consume() {
        bytes[idx] = byte;
        idx += 1;
    }

    Ok(Instruction { prefixes, bytes, repr, rex_prefix })
}

#[cfg(test)]
mod tests {
    use crate::decode::BitWidth;

    #[test]
    pub fn asm() {
        eprintln!("\n---------------------\nrep movsq qword ptr es:[rdi], qword ptr [rsi]\n---------------------");
        assert_eq!(
            super::asm(BitWidth::U64, &[0xf3, 0x48, 0xa5]).map(|ins| ins.repr.mnemomic),
            Ok("movsq")
        );

        eprintln!("\n---------------------\nphaddw xmm0, xmm1\n---------------------");
        assert_eq!(
            super::asm(BitWidth::U64, &[0x66, 0x0f, 0x38, 0x01, 0xc1]).map(|ins| ins.repr.mnemomic),
            Ok("phaddw")
        );

        eprintln!("\n---------------------\npop rbp\n---------------------");
        assert_eq!(super::asm(BitWidth::U64, &[0x5d]).map(|ins| ins.repr.mnemomic), Ok("pop"));
        eprintln!("{:?}", super::asm(BitWidth::U64, &[0x5d]).unwrap());

        todo!()
    }

    #[test]
    fn rex_prefix() {
        eprintln!("\n---------------------\ncvtdq2pd xmm0, qword ptr [r10]\n---------------------");
        panic!("{:?}", super::asm(BitWidth::U64, &[0xf3, 0x41, 0x0f, 0xe6, 0x02]));
    }
}

// Segment prefixes are ignored for jump instructions.
