// #[allow(dead_code)] uses exist as some attributes are not used by either AMD or intel.

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct X86 {
    pub mnemomic: &'static str,
    pub desc: &'static str,
    pub addressing: &'static [super::x86_64::AddressingMethod],
    pub operands: &'static [super::x86_64::OperandType],
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
        }
    };

    ($mnemomic:literal : $desc:literal => [$($addressing:expr),*], [$($operands:ident),*]) => {{
        #[allow(unused_imports)]
        use $crate::decode::x86_64::AddressingMethod::*;

        X86 {
            mnemomic: $mnemomic,
            desc: $desc,
            addressing: &[$($addressing),*],
            operands: &[$($crate::decode::x86_64::OperandType::$operands),*],
        }
    }};
}

// Can and should only be used inside `x86` macro
macro_rules! registers {
    ($reg:ident) => {
        FixedRegister($crate::decode::x86_64::Register::$reg)
    };
    ($($reg:ident),+) => {{
        FixedRegisters(
            &[$($crate::decode::x86_64::Register::$reg),+]
        )
    }}
}

// Can and should only be used inside `x86` macro
macro_rules! segment {
    ($seg:ident) => {
        FixedSegment($crate::decode::x86_64::Segment::$seg)
    };
}

// 16x16 1-byte instruction lookup
pub const X86_SINGLE: [[X86; 16]; 5] = [
    [
        x86!("add" : "Add r8 to r/m8" =>
             [ModRM, GeneralRegister], [U8, U8]
        ),
        x86!("add" : "Add r8/r16/r32 to r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("add" : "Add r/m8 to r8" =>
            [GeneralRegister, ModRM], [U8, U8]
        ),
        x86!("add" : "Add r/m8, r/m16 or r/m32 to r8/r16/r32 respectively" =>
            [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("add" : "Add imm8 to AL" =>
            [registers![AL], ImmediateData], [U8, U8]
        ),
        x86!("add" : "Add imm8/imm16/imm32 to AL/AX/EAX/RAX and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], ImmediateData], [SingleOrDoubleOrQuad, SingleOrDoubleQuadExtended]
        ),
        x86!("push" : "push ES" =>
            [segment![ES]], [U16]
        ),
        x86!("pop" : "pop ES" =>
            [segment![ES]], [U16]
        ),
        x86!("or" : "r8 or r/m8" =>
             [ModRM, GeneralRegister], [U8, U8]
        ),
        x86!("or" : "r8/r16/r32 or r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("or" : "r/m8 or r8" =>
            [GeneralRegister, ModRM], [U8, U8]
        ),
        x86!("or" : "r/m8, r/m16 or r/m32 or r8/r16/r32 respectively" =>
            [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("or" : "imm8 or AL" =>
            [registers![AL], ImmediateData], [U8, U8]
        ),
        x86!("or" : "AL/AX/EAX/RAX or imm8/imm16/imm32 and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], ImmediateData], [SingleOrDoubleOrQuad, SingleOrDoubleQuadExtended]
        ),
        x86!("push" : "push CS" =>
            [segment![CS]], [U16]
        ),
        x86!("2-byte escape code)"),
    ],
    [
        x86!("adc" : "Add with carry r8 to r/m8" =>
             [ModRM, GeneralRegister], [U8, U8]
        ),
        x86!("adc" : "Add with carry r8/r16/r32 to r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("adc" : "Add with carry r/m8 to r8" =>
            [GeneralRegister, ModRM], [U8, U8]
        ),
        x86!("adc" : "Add with carry r/m8, r/m16 or r/m32 to r8/r16/r32 respectively" =>
            [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("adc" : "Add with carry imm8 to AL" =>
            [registers![AL], ImmediateData], [U8, U8]
        ),
        x86!("adc" : "Add with carry imm8/imm16/imm32 to AL/AX/EAX/RAX and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], ImmediateData], [SingleOrDoubleOrQuad, SingleOrDoubleQuadExtended]
        ),
        x86!("push" : "push SS" =>
            [segment![SS]], [U16]
        ),
        x86!("pop" : "pop SS" =>
            [segment![SS]], [U16]
        ),
        x86!("sbb" : "Subtract with borrow r8 from r/m8" =>
             [ModRM, GeneralRegister], [U8, U8]
        ),
        x86!("sbb" : "Subtract with borrow r8/r16/r32 from r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("sbb" : "Subtract with borrow r/m8 from r8" =>
            [GeneralRegister, ModRM], [U8, U8]
        ),
        x86!("sbb" : "Subtract with borrow r/m8, r/m16 or r/m32 from r8/r16/r32 respectively" =>
            [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("sbb" : "Subtract with borrow imm8 from AL" =>
            [registers![AL], ImmediateData], [U8, U8]
        ),
        x86!("sbb" : "Subtract with borrow imm8/imm16/imm32 from AL/AX/EAX/RAX and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], ImmediateData], [SingleOrDoubleOrQuad, SingleOrDoubleQuadExtended]
        ),
        x86!("push" : "push DS" =>
            [segment![DS]], [U16]
        ),
        x86!("pop" : "pop DS" =>
            [segment![DS]], [U16]
        ),
    ],
    [
        x86!("and" : "r8 and r/m8" =>
             [ModRM, GeneralRegister], [U8, U8]
        ),
        x86!("and" : "r8/r16/r32 and r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("and" : "r/m8 and r8" =>
            [GeneralRegister, ModRM], [U8, U8]
        ),
        x86!("and" : "r/m8, r/m16 or r/m32 and r8/r16/r32 respectively" =>
            [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("and" : "imm8 and AL" =>
            [registers![AL], ImmediateData], [U8, U8]
        ),
        x86!("and" : "AL/AX/EAX/RAX and imm8/imm16/imm32 also sign-extending to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], ImmediateData], [SingleOrDoubleOrQuad, SingleOrDoubleQuadExtended]
        ),
        x86!("ES prefix"),
        x86!("daa" : "Decimal adjust AL after addition" => [], []),
        x86!("sub" : "Subtract r8 from r/m8" =>
             [ModRM, GeneralRegister], [U8, U8]
        ),
        x86!("sub" : "Subtract r8/r16/r32 from r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("sub" : "Subtract r/m8 from r8" =>
            [GeneralRegister, ModRM], [U8, U8]
        ),
        x86!("sub" : "Subtract r/m8, r/m16 or r/m32 from r8/r16/r32 respectively" =>
            [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("sub" : "Subtract imm8 from AL" =>
            [registers![AL], ImmediateData], [U8, U8]
        ),
        x86!("sub" : "Subtract imm8/imm16/imm32 from AL/AX/EAX/RAX and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], ImmediateData], [SingleOrDoubleOrQuad, SingleOrDoubleQuadExtended]
        ),
        x86!("CS prefix"),
        x86!("das" : "Decimal adjust AL after subtraction" => [], []),
    ],
    [
        x86!("xor" : "r8 xor r/m8" =>
             [ModRM, GeneralRegister], [U8, U8]
        ),
        x86!("xor" : "r8/r16/r32 xor r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("xor" : "r/m8 xor r8" =>
            [GeneralRegister, ModRM], [U8, U8]
        ),
        x86!("xor" : "r/m8, r/m16 or r/m32 xor r8/r16/r32 respectively" =>
            [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("xor" : "imm8 xor AL" =>
            [registers![AL], ImmediateData], [U8, U8]
        ),
        x86!("xor" : "AL/AX/EAX/RAX xor imm8/imm16/imm32 and sign-extending to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], ImmediateData], [SingleOrDoubleOrQuad, SingleOrDoubleQuadExtended]
        ),
        x86!("SS prefix"),
        x86!("aaa" : "ASCII adjust AL after addition" => [], []),
        x86!("cmp" : "Compare r8 to r/m8" =>
             [ModRM, GeneralRegister], [U8, U8]
        ),
        x86!("cmp" : "Compare r8/r16/r32 to r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("cmp" : "Compare r/m8 to r8" =>
            [GeneralRegister, ModRM], [U8, U8]
        ),
        x86!("cmp" : "Compare r/m8, r/m16 or r/m32 to r8/r16/r32 respectively" =>
            [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("cmp" : "Compare imm8 to AL" =>
            [registers![AL], ImmediateData], [U8, U8]
        ),
        x86!("cmp" : "Compare imm8/imm16/imm32 to AL/AX/EAX/RAX and sign-extend to imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], ImmediateData], [SingleOrDoubleOrQuad, SingleOrDoubleQuadExtended]
        ),
        x86!("DS prefix"),
        x86!("aaa" : "ASCII adjust AL after subtraction" => [], []),
    ],
    [
        x86!("inc" : "Increment AH or EAX depending on if RAX prefix is set" =>
             [registers![AH, EAX]], [SingleOrDouble]
        ),
        x86!("inc" : "Increment CH or ECX depending on if RAX prefix is set" =>
             [registers![CH, ECX]], [SingleOrDouble]
        ),
        x86!("inc" : "Increment DH or EDX depending on if RAX prefix is set" =>
             [registers![DH, EBX]], [SingleOrDouble]
        ),
        x86!("inc" : "Increment BH or EBX depending on if RAX prefix is set" =>
             [registers![BH, EBX]], [SingleOrDouble]
        ),
        x86!("inc" : "Increment SP or ESP depending on if RAX prefix is set" =>
             [registers![SP, ESP]], [DoubleOrQuad]
        ),
        x86!("inc" : "Increment BP or EBP depending on if RAX prefix is set" =>
             [registers![BP, EBP]], [DoubleOrQuad]
        ),
        x86!("inc" : "Increment SI or ESI depending on if RAX prefix is set" =>
             [registers![SI, ESI]], [DoubleOrQuad]
        ),
        x86!("inc" : "Increment DI or EDI depending on if RAX prefix is set" =>
             [registers![DI, EDI]], [DoubleOrQuad]
        ),
        x86!("dec" : "Decrement AH or EAX depending on if RAX prefix is set" =>
             [registers![AH, EAX]], [SingleOrDouble]
        ),
        x86!("dec" : "Decrement CH or ECX depending on if RAX prefix is set" =>
             [registers![CH, ECX]], [SingleOrDouble]
        ),
        x86!("dec" : "Decrement DH or EDX depending on if RAX prefix is set" =>
             [registers![DH, EBX]], [SingleOrDouble]
        ),
        x86!("dec" : "Decrement BH or EBX depending on if RAX prefix is set" =>
             [registers![BH, EBX]], [SingleOrDouble]
        ),
        x86!("dec" : "Decrement SP or ESP depending on if RAX prefix is set" =>
             [registers![SP, ESP]], [DoubleOrQuad]
        ),
        x86!("dec" : "Decrement BP or EBP depending on if RAX prefix is set" =>
             [registers![BP, EBP]], [DoubleOrQuad]
        ),
        x86!("dec" : "Decrement SI or ESI depending on if RAX prefix is set" =>
             [registers![SI, ESI]], [DoubleOrQuad]
        ),
        x86!("dec" : "Decrement DI or EDI depending on if RAX prefix is set" =>
             [registers![DI, EDI]], [DoubleOrQuad]
        ),
    ],
];
