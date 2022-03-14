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
    () => {
        X86 {
            mnemomic: "Invalid instruction",
            desc: "Instruction doesn't exist",
            addressing: &[],
            operands: &[],
        }
    };

    ($mnemomic:literal : $desc:literal => [$($addressing:expr),*], [$($operands:ident),*]) => {{
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

pub const X86_SINGLE: &[&[X86]] = &[
    &[
        x86!("add" : "Add r8 to r/m8" =>
             [ModRM, GeneralRegister], [U8, U8]
        ),
        x86!("add" : "Add r8/r16/r32 to a r/m8, r/m16 or r/m32 respectively" =>
             [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("add" : "Add r/m8 to r8" =>
            [GeneralRegister, ModRM], [U8, U8]
        ),
        x86!("add" : "Add r/m8, r/m16 or r/m32 to a r8/r16/r32 respectively" =>
            [ModRM, GeneralRegister], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
        x86!("add" : "Add imm8 to AL" =>
            [registers![AL], ImmediateData], [U8, U8]
        ),
        x86!("add" : "Add imm8/imm16/imm32 to AL/AX/EAX/rAX and sign-extending imm32 if necessary" =>
            [registers![AL, AX, EAX, RAX], ImmediateData], [SingleOrDoubleOrQuad, SingleOrDoubleOrQuad]
        ),
    ],
];
