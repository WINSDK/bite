//! MIPS V assembler

use std::fmt;
use super::lookup::MIPS_REGS;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    ImpossibleInputSize(usize),
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
        if self.format == &[1, 3, 2] {
            f.write_str(unsafe { MIPS_REGS.get_unchecked(self.operands[1]) })?;
            f.write_str(", ")?;
            f.write_str(&format!("0x{:x}", self.operands[3]))?;
            f.write_char('(')?;
            f.write_str(unsafe { MIPS_REGS.get_unchecked(self.operands[2]) })?;
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
                f.write_str(unsafe { MIPS_REGS.get_unchecked(operand) })?;
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
        return Err(Error::ImpossibleInputSize(raw.len()));
    }

    let opcode = raw[0] >> 2;
    let funct = raw[3] & 0b111111;

    let mut inst = *match opcode {
        0 => super::lookup::MIPS_R_TYPES.get(funct as usize).ok_or(Error::UnknownOpcode)?,
        2 | 3 => super::lookup::MIPS_J_TYPES.get(opcode as usize).ok_or(Error::UnknownOpcode)?,
        _ => super::lookup::MIPS_I_TYPES.get(opcode as usize).ok_or(Error::UnknownOpcode)?,
    };

    let [ref mut rd, ref mut rt, ref mut rs, ref mut imm] = inst.operands;

    *rs = ((raw[0] & 0b11) << 3 | raw[1] >> 5) as usize;
    *rt = (raw[1] & 0b11111) as usize;
    *rd = (raw[2] >> 3) as usize;

    if MIPS_REGS.get(*rs).is_none() || MIPS_REGS.get(*rt).is_none() || MIPS_REGS.get(*rd).is_none() {
        return Err(Error::UnknownRegister);
    }

    if opcode == 2 || opcode == 3 {
        *imm = u32::from_be_bytes([raw[0] & 0b11, raw[1], raw[2], raw[3]]) as usize;
    }

    if opcode == 0 && (funct == 0 || funct == 2 || funct == 3) {
        *imm = ((raw[2] & 0b111) << 2 | (raw[3] >> 6)) as usize;
    }

    if opcode > 3 {
        *imm = u16::from_be_bytes([raw[2], raw[3]]) as usize
    }

    Ok(inst)
}

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
