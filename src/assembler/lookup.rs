// NOTE: #[allow(dead_code)] uses exist as some attributes are not used by either AMD or intel.

use super::BitWidth;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct X86 {
    pub mnemomic: &'static str,
    pub desc: &'static str,
    pub addressing: &'static [super::x86_64::AddressingMethod],
    pub operands: &'static [super::x86_64::OperandType],
    pub size_exclusive: Option<BitWidth>,
}

// NOTE: Operands and addressing have to be in order,
// E.g. &[ModRM, RegGeneralRegister] is ax, 0x10000000 whilst
//      &[RegGeneralRegister, ModRM] is 0x10000000, ax.
//
// `m8` is the content of an AX/EAX/rAX register
// r/m8 is either a `m8` or a byte from memory.

macro_rules! x86 {
    ($desc:literal) => {
        X86 {
            mnemomic: "Invalid instruction",
            desc: $desc,
            addressing: &[],
            operands: &[],
            size_exclusive: None,
        }
    };

    ($mnemomic:literal : $desc:literal) => {
        X86 {
            mnemomic: $mnemomic,
            desc: $desc,
            addressing: &[],
            operands: &[],
            size_exclusive: None,
        }
    };

    ($mnemomic:literal : $desc:literal => $([$addressing:expr, $operands:ident]),*) => {{
        #[allow(unused_imports)]
        use $crate::assembler::x86_64::AddressingMethod::*;

        let (addressing, operands) = (
            &[$($addressing),*],
            &[$($crate::assembler::x86_64::OperandType::$operands),*],
        );

        X86 {
            mnemomic: $mnemomic,
            desc: $desc,
            addressing,
            operands,
            size_exclusive: None,
        }
    }};

    ($mnemomic:literal : $desc:literal => $([$addressing:expr, $operands:ident]),*, $bitwidth:expr) => {{
        #[allow(unused_imports)]
        use $crate::assembler::x86_64::AddressingMethod::*;

        let (addressing, operands) = (
            &[$($addressing),*],
            &[$($crate::assembler::x86_64::OperandType::$operands),*],
        );

        X86 {
            mnemomic: $mnemomic,
            desc: $desc,
            addressing,
            operands,
            size_exclusive: Some($bitwidth),
        }
    }};
}

// Can and should only be used inside `x86` macro
macro_rules! registers {
    ($reg:ident) => {
        FixedRegister($crate::assembler::x86_64::Register::$reg)
    };
    ($($reg:ident),+) => {{
        FixedRegisters(
            &[$($crate::assembler::x86_64::Register::$reg),+]
        )
    }}
}

// Can and should only be used inside `x86` macro
macro_rules! segment {
    ($seg:ident) => {
        FixedSegment($crate::assembler::x86_64::Segment::$seg)
    };
}

// 16x16 1-byte instruction lookup
pub const X86_SINGLE: [[X86; 16]; 8] = [
    [
        x86!("add" : "Add r8 to r/m8" =>
             [ModRM, U8], [Register, U8]
        ),
        x86!("add" : "Add r8/r16/r32 to r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("add" : "Add r/m8 to r8" =>
            [Register, U8], [ModRM, U8]
        ),
        x86!("add" : "Add r/m8, r/m16 or r/m32 to r8/r16/r32 respectively" =>
            [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("add" : "Add imm8 to AL" =>
            [registers![AL], U8], [Immediate, U8]
        ),
        x86!("add" : "Add imm8/imm16/imm32 to AL/AX/EAX/RAX and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], SingleOrDoubleOrQuad], [Immediate, SingleOrDoubleOrQuad]
        ),
        x86!("push" : "push ES" =>
            [segment![ES], U16]
        ),
        x86!("pop" : "pop ES" =>
            [segment![ES], U16]
        ),
        x86!("or" : "r8 or r/m8" =>
             [ModRM, U8], [Register, U8]
        ),
        x86!("or" : "r8/r16/r32 or r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("or" : "r/m8 or r8" =>
            [Register, U8], [ModRM, U8]
        ),
        x86!("or" : "r/m8, r/m16 or r/m32 or r8/r16/r32 respectively" =>
            [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("or" : "imm8 or AL" =>
            [registers![AL], U8], [Immediate, U8]
        ),
        x86!("or" : "AL/AX/EAX/RAX or imm8/imm16/imm32 and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], SingleOrDoubleOrQuad], [Immediate, SingleOrDoubleOrQuad]
        ),
        x86!("push" : "push CS" =>
            [segment![CS], U16]
        ),
        x86!("2-byte escape code)"),
    ],
    [
        x86!("adc" : "Add with carry r8 to r/m8" =>
             [ModRM, U8], [Register, U8]
        ),
        x86!("adc" : "Add with carry r8/r16/r32 to r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("adc" : "Add with carry r/m8 to r8" =>
            [Register, U8], [ModRM, U8]
        ),
        x86!("adc" : "Add with carry r/m8, r/m16 or r/m32 to r8/r16/r32 respectively" =>
            [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("adc" : "Add with carry imm8 to AL" =>
            [registers![AL], U8], [Immediate, U8]
        ),
        x86!("adc" : "Add with carry imm8/imm16/imm32 to AL/AX/EAX/RAX and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], SingleOrDoubleOrQuad], [Immediate, SingleOrDoubleQuadExtended]
        ),
        x86!("push" : "push SS" =>
            [segment![SS], U16]
        ),
        x86!("pop" : "pop SS" =>
            [segment![SS], U16]
        ),
        x86!("sbb" : "Subtract with borrow r8 from r/m8" =>
             [ModRM, U8], [Register, U8]
        ),
        x86!("sbb" : "Subtract with borrow r8/r16/r32 from r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("sbb" : "Subtract with borrow r/m8 from r8" =>
            [Register, U8], [ModRM, U8]
        ),
        x86!("sbb" : "Subtract with borrow r/m8, r/m16 or r/m32 from r8/r16/r32 respectively" =>
            [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("sbb" : "Subtract with borrow imm8 from AL" =>
            [registers![AL], U8], [Immediate, U8]
        ),
        x86!("sbb" : "Subtract with borrow imm8/imm16/imm32 from AL/AX/EAX/RAX and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], SingleOrDoubleOrQuad], [Immediate, SingleOrDoubleQuadExtended]
        ),
        x86!("push" : "push DS" =>
            [segment![DS], U16]
        ),
        x86!("pop" : "pop DS" =>
            [segment![DS], U16]
        ),
    ],
    [
        x86!("and" : "r8 and r/m8" =>
             [ModRM, U8], [Register, U8]
        ),
        x86!("and" : "r8/r16/r32 and r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("and" : "r/m8 and r8" =>
            [Register, U8], [ModRM, U8]
        ),
        x86!("and" : "r/m8, r/m16 or r/m32 and r8/r16/r32 respectively" =>
            [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("and" : "imm8 and AL" =>
            [registers![AL], U8], [Immediate, U8]
        ),
        x86!("and" : "AL/AX/EAX/RAX and imm8/imm16/imm32 also sign-extending to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], SingleOrDoubleOrQuad], [Immediate, SingleOrDoubleQuadExtended]
        ),
        x86!("ES prefix"),
        x86!("daa" : "Decimal adjust AL after addition" =>),
        x86!("sub" : "Subtract r8 from r/m8" =>
             [ModRM, U8], [Register, U8]
        ),
        x86!("sub" : "Subtract r8/r16/r32 from r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("sub" : "Subtract r/m8 from r8" =>
            [Register, U8], [ModRM, U8]
        ),
        x86!("sub" : "Subtract r/m8, r/m16 or r/m32 from r8/r16/r32 respectively" =>
            [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("sub" : "Subtract imm8 from AL" =>
            [registers![AL], U8], [Immediate, U8]
        ),
        x86!("sub" : "Subtract imm8/imm16/imm32 from AL/AX/EAX/RAX and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], SingleOrDoubleOrQuad], [Immediate, SingleOrDoubleOrQuad]
        ),
        x86!("CS prefix"),
        x86!("das" : "Decimal adjust AL after subtraction"),
    ],
    [
        x86!("xor" : "r8 xor r/m8" =>
             [ModRM, U8], [Register, U8]
        ),
        x86!("xor" : "r8/r16/r32 xor r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("xor" : "r/m8 xor r8" =>
            [Register, U8], [ModRM, U8]
        ),
        x86!("xor" : "r/m8, r/m16 or r/m32 xor r8/r16/r32 respectively" =>
            [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("xor" : "imm8 xor AL" =>
            [registers![AL], U8], [Immediate, U8]
        ),
        x86!("xor" : "AL/AX/EAX/RAX xor imm8/imm16/imm32 and sign-extending to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], SingleOrDoubleOrQuad], [Immediate, SingleOrDoubleQuadExtended]
        ),
        x86!("SS prefix"),
        x86!("aaa" : "ASCII adjust AL after addition"),
        x86!("cmp" : "Compare r8 to r/m8" =>
             [ModRM, U8], [Register, U8]
        ),
        x86!("cmp" : "Compare r8/r16/r32 to r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("cmp" : "Compare r/m8 to r8" =>
            [Register, U8], [ModRM, U8]
        ),
        x86!("cmp" : "Compare r/m8, r/m16 or r/m32 to r8/r16/r32 respectively" =>
            [ModRM, SingleOrDoubleOrQuad], [Register, SingleOrDoubleOrQuad]
        ),
        x86!("cmp" : "Compare imm8 to AL" =>
            [registers![AL], U8], [Immediate, U8]
        ),
        x86!("cmp" : "Compare imm8/imm16/imm32 to AL/AX/EAX/RAX and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], SingleOrDoubleOrQuad], [Immediate, SingleOrDoubleQuadExtended]
        ),
        x86!("DS prefix"),
        x86!("aaa" : "ASCII adjust AL after subtraction"),
    ],
    [
        x86!("inc" : "Increment AH or EAX depending on if RAX prefix is set" =>
             [registers![AH, EAX], DoubleOrQuad]
        ),
        x86!("inc" : "Increment CH or ECX depending on if RAX prefix is set" =>
             [registers![CH, ECX], DoubleOrQuad]
        ),
        x86!("inc" : "Increment DH or EDX depending on if RAX prefix is set" =>
             [registers![DH, EBX], DoubleOrQuad]
        ),
        x86!("inc" : "Increment BH or EBX depending on if RAX prefix is set" =>
             [registers![BH, EBX], DoubleOrQuad]
        ),
        x86!("inc" : "Increment SP or ESP depending on if RAX prefix is set" =>
             [registers![SP, ESP], DoubleOrQuad]
        ),
        x86!("inc" : "Increment BP or EBP depending on if RAX prefix is set" =>
             [registers![BP, EBP], DoubleOrQuad]
        ),
        x86!("inc" : "Increment SI or ESI depending on if RAX prefix is set" =>
             [registers![SI, ESI], DoubleOrQuad]
        ),
        x86!("inc" : "Increment DI or EDI depending on if RAX prefix is set" =>
             [registers![DI, EDI], DoubleOrQuad]
        ),
        x86!("dec" : "Decrement AH or EAX depending on if RAX prefix is set" =>
             [registers![AH, EAX], DoubleOrQuad]
        ),
        x86!("dec" : "Decrement CH or ECX depending on if RAX prefix is set" =>
             [registers![CH, ECX], DoubleOrQuad]
        ),
        x86!("dec" : "Decrement DH or EDX depending on if RAX prefix is set" =>
             [registers![DH, EBX], DoubleOrQuad]
        ),
        x86!("dec" : "Decrement BH or EBX depending on if RAX prefix is set" =>
             [registers![BH, EBX], DoubleOrQuad]
        ),
        x86!("dec" : "Decrement SP or ESP depending on if RAX prefix is set" =>
             [registers![SP, ESP], DoubleOrQuad]
        ),
        x86!("dec" : "Decrement BP or EBP depending on if RAX prefix is set" =>
             [registers![BP, EBP], DoubleOrQuad]
        ),
        x86!("dec" : "Decrement SI or ESI depending on if RAX prefix is set" =>
             [registers![SI, ESI], DoubleOrQuad]
        ),
        x86!("dec" : "Decrement DI or EDI depending on if RAX prefix is set" =>
             [registers![DI, EDI], DoubleOrQuad]
        ),
    ],
    [
        x86!("push" : "Decrement stack pointer and then stores the RAX on top of the stack" =>
             [registers![RAX], U64], BitWidth::U64
        ),
        x86!("push" : "Decrement stack pointer and then stores the RCX on top of the stack" =>
             [registers![RCX], U64], BitWidth::U64
        ),
        x86!("push" : "Decrement stack pointer and then stores the RDX on top of the stack" =>
             [registers![RDX], U64], BitWidth::U64
        ),
        x86!("push" : "Decrement stack pointer and then stores the RBX on top of the stack" =>
             [registers![RBX], U64], BitWidth::U64
        ),
        x86!("push" : "Decrement stack pointer and then stores the RSP on top of the stack" =>
             [registers![RSP], U64], BitWidth::U64
        ),
        x86!("push" : "Decrement stack pointer and then stores the RBP on top of the stack" =>
             [registers![RBP], U64], BitWidth::U64
        ),
        x86!("push" : "Decrement stack pointer and then stores the RSI on top of the stack" =>
             [registers![RSI], U64], BitWidth::U64
        ),
        x86!("push" : "Decrement stack pointer and then stores the RDI on top of the stack" =>
             [registers![RDI], U64], BitWidth::U64
        ),
        x86!("pop" : "Loads value from top of stack into RAX and then increments the stack pointer" =>
             [registers![RAX], U64], BitWidth::U64
        ),
        x86!("pop" : "Loads value from top of stack into RCX and then increments the stack pointer" =>
             [registers![RCX], U64], BitWidth::U64
        ),
        x86!("pop" : "Loads value from top of stack into RDX and then increments the stack pointer" =>
             [registers![RDX], U64], BitWidth::U64
        ),
        x86!("pop" : "Loads value from top of stack into RBX and then increments the stack pointer" =>
             [registers![RBX], U64], BitWidth::U64
        ),
        x86!("pop" : "Loads value from top of stack into RSP and then increments the stack pointer" =>
             [registers![RSP], U64], BitWidth::U64
        ),
        x86!("pop" : "Loads value from top of stack into RBP and then increments the stack pointer" =>
             [registers![RBP], U64], BitWidth::U64
        ),
        x86!("pop" : "Loads value from top of stack into RSI and then increments the stack pointer" =>
             [registers![RSI], U64], BitWidth::U64
        ),
        x86!("pop" : "Loads value from top of stack into RDI and then increments the stack pointer" =>
             [registers![RDI], U64], BitWidth::U64
        ),
    ],
    [
        x86!("pusha" : "Decrement stack pointer and then stores all general-purpose registers on the top of the stack" =>
             [Register, SingleOrDouble]
        ),
        x86!("popa" : "Loads value from top of stack into all general-purpose and then increments the stack pointer" =>
             [Register, SingleOrDouble]
        ),
        x86!("bound" : "Checks if the first operand (index) is within the bounds of the second operand (array)" =>
             [Register, DoubleMemory], [Memory, DoubleMemory]
        ),
        // FIXME: `arpl` instruction in 32-bit mode and in the future maybe consider adding
        // multiple instruction variants per table entry.
        x86!("movsx" : "Copies the contents of the source operand to the destination operand and sign extends the source to 16 or 32 bits" =>
             [ModRM, SingleOrDoubleOrQuad], [Register, ByteOrSingleOrDoubleExtended]
        ),
        x86!("FS prefix"),
        x86!("GS prefix"),
        x86!("Operand size prefix"),
        x86!("Address size prefix"),
        x86!("push" : "Decrement stack pointer and then stores the immediate on top of the stack" =>
             [Immediate, SingleOrDouble], BitWidth::U64
        ),
        x86!("imul" : "Second operand is multiplied by the third operand and then stored in the first operand" =>
             [Register, SingleOrDoubleOrQuad], [ModRM, SingleOrDoubleOrQuad], [Immediate, SingleOrDouble]
        ),
        x86!("push" : "Decrement stack pointer and then stores the immediate on top of the stack" =>
             [Immediate, U8], BitWidth::U64
        ),
        x86!("imul" : "Second operand is multiplied by the third operand and then stored in the first operand" =>
             [Register, SingleOrDoubleOrQuad], [ModRM, SingleOrDoubleOrQuad], [Immediate, U8]
        ),
        x86!("insb" : "Copies byte from I/O port specified by `DX` into memory location specified in ES:EDI or RDI"),
        x86!("ins" : "Copies single/word/quad from I/O port specified by `DX` into memory location specified in ES:EDI or RDI"),
        x86!("outsb" : "Copies byte from memory specified in ES:EDI or RDI into I/O port specified by `DX`"),
        x86!("outs" : "Copies single/word/quad from memory specified in ES:EDI or RDI into I/O port specified by `DX`"),
    ],
    [
        x86!("jo" : "Jumps to the first operand if the overflow flag (OF) is set" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jno" : "Jumps to the first operand if the overflow flag (OF) is not set" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jb/jnae/jc" : "Jumps to the first operand if the carry flag (CF) is set and ????????????????" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jnb/jae/jnc" : "Jumps to the first operand if the carry flag (CF) is not set and ????????????????" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("je" : "Jumps to the first operand if the zero flag (ZF) is set" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jne" : "Jumps to the first operand if the zero flag (ZF) is not set" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jbe" : "Jumps to the first operand if either the carry flag (CF) or zero flag (ZF) are set" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jnbe" : "Jumps to the first operand if neither the carry flag (CF) or zero flag (ZF) are set" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("js" : "Jumps to the first operand if the sign flag (SF) is set" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jns" : "Jumps to the first operand if the sign flag (SF) is not set" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jp" : "Jumps to the first operand if the parity flag (PF) is set" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jnp" : "Jumps to the first operand if the parity flag (PF) is not set" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jl/jnge" : "Jumps to the first operand if the sign flag (SF) does not equal the overflow flag (OF) and ?????????????????" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jnl/jge" : "Jumps to the first operand if the sign flag (SF) equals the overflow flag (OF) and ?????????????????" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jle/jng" : "Jumps to the first operand if the zero flag (ZF) is set or if the sign flag (SF) does not equal the overflow flag (OF) and ?????????????????" =>
             [NoModRM, SingleOrDouble]
        ),
        x86!("jle/jng" : "Jumps to the first operand if the zero flag (ZF) is not set and if the sign flag (SF) equals the overflow flag (OF) and ?????????????????" =>
             [NoModRM, SingleOrDouble]
        ),
    ]
];

macro_rules! mips {
    () => {
        $crate::assembler::mips::Instruction {
            mnemomic: "Invalid instruction",
            desc: "",
            format: Format::Imm(0),
        }
    };

    ($mnemomic:literal, $desc:literal, $format:expr) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: $format,
        }
    };
}

macro_rules! format {
    (Simple) => { Format::Simple };
    (Imm) => { Format::Imm(0) };
    (Reg) => { Format::Reg(Register(0)) };
    (RegReg) => { Format::RegReg(Register(0), Register(0)) };
    (RegImm) => { Format::RegImm(Register(0), 0) };
    (RegRegReg) => { Format::RegRegReg(Register(0), Register(0), Register(0)) };
    (RegRegImm) => { Format::RegRegImm(Register(0), Register(0), 0) };
    (RegRegOff) => { Format::RegRegOff(Register(0), Register(0), 0) };
}

#[derive(Debug, Clone, Copy)]
pub struct Register(u8);

impl Register {
    pub fn new(val: u8) -> Self {
        debug_assert!(MIPS_REGS.get(val as usize).is_some());
        Self(val)
    }
}

impl std::ops::Deref for Register {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        MIPS_REGS[self.0 as usize]
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Format {
    /// ins
    Simple,

    /// ins imm
    Imm(u32),

    /// ins $rs
    Reg(Register),

    /// ins $rs, $rt
    RegReg(Register, Register),

    /// ins $rs, imm
    RegImm(Register, u32),

    /// ins $rt, $rt, $rd
    RegRegReg(Register, Register, Register),

    /// ins $rs, $rt, imm
    RegRegImm(Register, Register, u32),

    /// ins $rt, imm($rs)
    RegRegOff(Register, Register, u32)
}

pub const MIPS_I_TYPES: [crate::assembler::mips::Instruction; 44] = [
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!("beq", "", format!(RegRegImm)),
    mips!("bne", "", format!(RegRegImm)),
    mips!("blez", "", format!(RegImm)),
    mips!("bgtz", "", format!(RegImm)),
    mips!("addi", "", format!(RegRegImm)),
    mips!("addiu", "", format!(RegRegImm)),
    mips!("slti", "", format!(RegRegImm)),
    mips!("sltiu", "", format!(RegRegImm)),
    mips!("andi", "", format!(RegRegImm)),
    mips!("ori", "", format!(RegRegImm)),
    mips!("xori", "", format!(RegRegImm)),
    mips!("lui", "", format!(RegImm)),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!("lb", "", format!(RegRegOff)),
    mips!("lh", "", format!(RegRegOff)),
    mips!("lw", "", format!(RegRegOff)),
    mips!("lbu", "", format!(RegRegOff)),
    mips!("lhu", "", format!(RegRegOff)),
    mips!(),
    mips!(),
    mips!("sb", "", format!(RegRegOff)),
    mips!("sh", "", format!(RegRegOff)),
    mips!("sw", "", format!(RegRegOff)),
];

pub const MIPS_J_TYPES: [crate::assembler::mips::Instruction; 4] = [
    mips!(),
    mips!(),
    mips!("j", "Jump to target address", format!(Imm)),
    mips!("jr", "Call the target address and save return addr in $ra", format!(Imm)),
];

pub const MIPS_R_TYPES: [crate::assembler::mips::Instruction; 43] = [
    mips!("sll", "", format!(RegRegImm)),
    mips!(),
    mips!("srl", "", format!(RegRegImm)),
    mips!("sra", "", format!(RegRegImm)),
    mips!("sllv", "", format!(RegRegReg)),
    mips!(),
    mips!("srlv", "", format!(RegRegReg)),
    mips!("srav", "", format!(RegRegReg)),
    mips!("jr", "", format!(Reg)),
    mips!(),
    mips!(),
    mips!("syscall", "", format!(Simple)),
    mips!(),
    mips!(),
    mips!(),
    mips!("mfhi", "", format!(Reg)),
    mips!("mthi", "", format!(Reg)),
    mips!("mflo", "", format!(Reg)),
    mips!("mtlo", "", format!(Reg)),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!("mult", "", format!(RegReg)),
    mips!("multu", "", format!(RegReg)),
    mips!("div", "", format!(RegReg)),
    mips!("divu", "", format!(RegReg)),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!("add", "", format!(RegRegReg)),
    mips!("addu", "", format!(RegRegReg)),
    mips!("sub", "", format!(RegRegReg)),
    mips!("subu", "", format!(RegRegReg)),
    mips!("and", "", format!(RegRegReg)),
    mips!("or", "", format!(RegRegReg)),
    mips!("xor", "", format!(RegRegReg)),
    mips!("nor", "", format!(RegRegReg)),
    mips!(),
    mips!(),
    mips!("slt", "", format!(RegRegReg)),
    mips!("sltu", "", format!(RegRegReg)),
];

pub const MIPS_REGS: [&'static str; 32] = [
    "$zero",
    "$at",
    "$v0",
    "$v1",
    "$a0",
    "$a1",
    "$a2",
    "$a3",
    "$t0",
    "$t1",
    "$t2",
    "$t3",
    "$t4",
    "$t5",
    "$t6",
    "$t7",
    "$s0",
    "$s1",
    "$s2",
    "$s3",
    "$s4",
    "$s5",
    "$s6",
    "$s7",
    "$t8",
    "$t9",
    "$k0",
    "$k1",
    "$gp",
    "$sp",
    "$fp",
    "$ra",
];
