//! MIPS V assembler

use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    ImpossibleInputSize,
    InvalidInstruction,
    UnknownRegister,
    UnknownOpcode,
}

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    pub mnemomic: &'static str,
    pub desc: &'static str,
    pub operands: [usize; 4],
    pub format: &'static [usize],
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Write;

        f.write_str(self.mnemomic)?;
        f.write_char(' ')?;

        // check if the instruction uses an offset (load/store instructions)
        if self.format == [1, 3, 2] {
            f.write_str(unsafe { REGISTERS.get_unchecked(self.operands[1]) })?;
            f.write_str(", ")?;
            f.write_str(&format!("0x{:x}", self.operands[3]))?;
            f.write_char('(')?;
            f.write_str(unsafe { REGISTERS.get_unchecked(self.operands[2]) })?;
            f.write_char(')')?;

            return Ok(());
        }

        for idx in 0..self.format.len() {
            // index into next operand
            let format = self.format[idx];

            // operand specified by the bitmask
            let operand = self.operands[format];

            // check if we are formatting an immediate
            if format == 3 {
                f.write_str(&format!("0x{operand:x}"))?;
            } else {
                f.write_str(unsafe { REGISTERS.get_unchecked(operand) })?;
            }

            // check if we've reached the last operand
            if idx != self.format.len() - 1 {
                f.write_str(", ")?;
            }
        }

        Ok(())
    }
}

#[allow(dead_code)]
pub fn asm(raw: &[u8]) -> Result<Instruction, Error> {
    if raw.len() != 4 {
        return Err(Error::ImpossibleInputSize);
    }

    let opcode = raw[0] >> 2;
    let funct = raw[3] & 0b111111;

    let mut inst = *match opcode {
        0 => R_TYPES.get(funct as usize).ok_or(Error::UnknownOpcode)?,
        2 | 3 => J_TYPES.get(opcode as usize).ok_or(Error::UnknownOpcode)?,
        _ => I_TYPES.get(opcode as usize).ok_or(Error::UnknownOpcode)?,
    };

    let [ref mut rd, ref mut rt, ref mut rs, ref mut imm] = inst.operands;

    *rs = ((raw[0] & 0b11) << 3 | raw[1] >> 5) as usize;
    *rt = (raw[1] & 0b11111) as usize;
    *rd = (raw[2] >> 3) as usize;

    if opcode == 2 || opcode == 3 {
        *imm = u32::from_be_bytes([raw[0] & 0b11, raw[1], raw[2], raw[3]]) as usize;
    }

    if opcode == 0 && (funct == 0 || funct == 2 || funct == 3) {
        *imm = ((raw[2] & 0b111) << 2 | (raw[3] >> 6)) as usize;
    }

    if opcode > 3 {
        *imm = u16::from_be_bytes([raw[2], raw[3]]) as usize
    }

    if REGISTERS.get(*rs).and(REGISTERS.get(*rt)).and(REGISTERS.get(*rd)).is_none() {
        return Err(Error::UnknownRegister);
    }

    if inst.mnemomic.is_empty() {
        return Err(Error::InvalidInstruction);
    }

    Ok(inst)
}

/// Bitmask for order of operands [rd, rt, rs, imm].
macro_rules! mips {
    () => {
        $crate::assembler::mips::Instruction {
            mnemomic: "",
            desc: "",
            operands: [0, 0, 0, 0],
            format: &[],
        }
    };

    ($mnemomic:literal, $desc:literal, rd, rt, imm) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[0, 1, 3],
        }
    };

    ($mnemomic:literal, $desc:literal, rt, rs, imm) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[1, 2, 3],
        }
    };

    ($mnemomic:literal, $desc:literal, rs, rt, imm) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[2, 1, 3],
        }
    };

    ($mnemomic:literal, $desc:literal, rd, rt, rs) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[0, 1, 2],
        }
    };

    ($mnemomic:literal, $desc:literal, rd, rs, rt) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[0, 2, 1],
        }
    };

    ($mnemomic:literal, $desc:literal, rt, imm, rs) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[1, 3, 2],
        }
    };

    ($mnemomic:literal, $desc:literal, rs, imm) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[2, 3],
        }
    };

    ($mnemomic:literal, $desc:literal, rt, imm) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[1, 3],
        }
    };

    ($mnemomic:literal, $desc:literal, rd, rs) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[0, 2],
        }
    };

    ($mnemomic:literal, $desc:literal, rs, rt) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[2, 1],
        }
    };

    ($mnemomic:literal, $desc:literal, imm) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[3],
        }
    };

    ($mnemomic:literal, $desc:literal, rs) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[2],
        }
    };

    ($mnemomic:literal, $desc:literal, rd) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[0],
        }
    };

    ($mnemomic:literal, $desc:literal) => {
        $crate::assembler::mips::Instruction {
            mnemomic: $mnemomic,
            desc: $desc,
            operands: [0, 0, 0, 0],
            format: &[],
        }
    };
}

pub const I_TYPES: [crate::assembler::mips::Instruction; 44] = [
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!("beq", "", rs, rt, imm),
    mips!("bne", "", rs, rt, imm),
    mips!("blez", "", rs, imm),
    mips!("bgtz", "", rs, imm),
    mips!("addi", "", rt, rs, imm),
    mips!("addiu", "", rt, rs, imm),
    mips!("slti", "", rt, rs, imm),
    mips!("sltiu", "", rt, rs, imm),
    mips!("andi", "", rt, rs, imm),
    mips!("ori", "", rt, rs, imm),
    mips!("xori", "", rt, rs, imm),
    mips!("lui", "", rt, imm),
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
    mips!("lb", "", rt, imm, rs),
    mips!("lh", "", rt, imm, rs),
    mips!("lw", "", rt, imm, rs),
    mips!(),
    mips!("lbu", "", rt, imm, rs),
    mips!("lhu", "", rt, imm, rs),
    mips!(),
    mips!(),
    mips!("sb", "", rt, imm, rs),
    mips!("sh", "", rt, imm, rs),
    mips!(),
    mips!("sw", "", rt, imm, rs),
];

pub const J_TYPES: [crate::assembler::mips::Instruction; 4] = [
    mips!(),
    mips!(),
    mips!("j", "Jump to target address", imm),
    mips!("jr", "Call the target address and save return addr in $ra", imm),
];

pub const R_TYPES: [crate::assembler::mips::Instruction; 43] = [
    mips!("sll", "", rd, rt, imm),
    mips!(),
    mips!("srl", "", rd, rt, imm),
    mips!("sra", "", rd, rt, imm),
    mips!("sllv", "", rd, rt, rs),
    mips!(),
    mips!("srlv", "", rd, rt, rs),
    mips!("srav", "", rd, rt, rs),
    mips!("jr", "", rs),
    mips!(),
    mips!(),
    mips!("syscall", ""),
    mips!(),
    mips!(),
    mips!(),
    mips!("mfhi", "", rd),
    mips!("mthi", "", rs),
    mips!("mflo", "", rd),
    mips!("mtlo", "", rs),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!("mult", "", rs, rt),
    mips!("multu", "", rs, rt),
    mips!("div", "", rs, rt),
    mips!("divu", "", rs, rt),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!("add", "", rd, rs, rt),
    mips!("addu", "", rd, rs, rt),
    mips!("sub", "", rd, rs, rt),
    mips!("subu", "", rd, rs, rt),
    mips!("and", "", rd, rs, rt),
    mips!("or", "", rd, rs, rt),
    mips!("xor", "", rd, rs, rt),
    mips!("nor", "", rd, rs, rt),
    mips!(),
    mips!(),
    mips!("slt", "", rd, rs, rt),
    mips!("sltu", "", rd, rs, rt),
];

#[rustfmt::skip]
pub const REGISTERS: [&str; 32] = [
    "$zero", "$at",
    "$v0", "$v1",
    "$a0", "$a1", "$a2", "$a3",
    "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
    "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
    "$t8", "$t9",
    "$k0", "$k1", 
    "$gp", "$sp", "$fp", "$ra",
];

#[cfg(test)]
mod tests {
    macro_rules! eq {
        ([$($bytes:tt),+] => $repr:expr) => {
            assert_eq!(
                $crate::assembler::mips::asm(&[$($bytes),+])
                    .map(|x| x.to_string())
                    .as_deref(),
                Ok($repr)
            )
        };
    }

    #[test]
    fn jump() {
        eq!([0x9, 0, 0, 0] => "j 0x1000000");
    }

    #[test]
    fn beq() {
        eq!([0x11, 0x2a, 0x10, 0x0] => "beq $t1, $t2, 0x1000");
    }

    #[test]
    fn sll() {
        eq!([0x0, 0xa, 0x4c, 0x80] => "sll $t1, $t2, 0x12");
    }

    #[test]
    fn sllv() {
        eq!([0x1, 0x49, 0x48, 0x4] => "sllv $t1, $t1, $t2");
    }

    #[test]
    fn lb() {
        eq!([0x81, 0x49, 0x0, 0x10] => "lb $t1, 0x10($t2)")
    }
}
