//! MIPS V assembler

use std::fmt;
use super::lookup::{Format, Register};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    ImpossibleInputSize(usize),
    UnknownOpcode,
}

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    pub mnemomic: &'static str,
    pub desc: &'static str,
    pub format: Format,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::fmt::Write;

        f.write_str(self.mnemomic)?;
        f.write_char(' ')?;

        match &self.format {
            Format::Imm(addr) => f.write_str(&format!("0x{addr:x}"))?,
            Format::RegRegImm(rs, rt, imm) => {
                f.write_str(&rs)?;
                f.write_str(", ")?;
                f.write_str(&rt)?;
                f.write_str(", ")?;
                f.write_str(&format!("0x{imm:x}"))?;
            },
            Format::RegRegReg(rs, rt, rd) => {
                f.write_str(&rd)?;
                f.write_str(", ")?;
                f.write_str(&rt)?;
                f.write_str(", ")?;
                f.write_str(&rs)?;
            }
            _ => todo!()
        }

        Ok(())
    }
}

pub fn asm(raw_bytes: &[u8]) -> Result<Instruction, Error> {
    if raw_bytes.len() != 4 {
        return Err(Error::ImpossibleInputSize(raw_bytes.len()));
    }

    let opcode = raw_bytes[0] >> 2;
    let rs = Register::new((raw_bytes[0] & 0b11) << 3 | raw_bytes[1] >> 5);
    let rt = Register::new(raw_bytes[1] & 0b11111);
    let rd = Register::new(raw_bytes[2] >> 3);
    let shamt = (raw_bytes[2] & 0b111) << 2 | (raw_bytes[3] >> 6);
    let funct = raw_bytes[3] & 0b111111;

    let addr = match opcode {
        2 | 3 => u32::from_be_bytes([raw_bytes[0] & 0b11, raw_bytes[1], raw_bytes[2], raw_bytes[3]]),
        0 if funct == 0 || funct == 2 || funct == 3 => shamt as u32,
        _ =>  u16::from_be_bytes([raw_bytes[2], raw_bytes[3]]) as u32
    };

    eprintln!("{:#010b}", raw_bytes[0]);
    eprintln!("{:#010b}", raw_bytes[1]);
    eprintln!("{:#010b}", raw_bytes[2]);
    eprintln!("{:#010b}", raw_bytes[3]);
    eprintln!("opcode: {opcode:#018b}");
    eprintln!("shamt: {shamt:#018b}");
    eprintln!("funct: {funct:#018b}");
    eprintln!("addr: {addr:#018b}");

    let mut inst = *match opcode {
        0 => super::lookup::MIPS_R_TYPES.get(funct as usize).ok_or(Error::UnknownOpcode)?,
        2 | 3 => super::lookup::MIPS_J_TYPES.get(opcode as usize).ok_or(Error::UnknownOpcode)?,
        _ => super::lookup::MIPS_I_TYPES.get(opcode as usize).ok_or(Error::UnknownOpcode)?,
    };

    inst.format = match inst.format {
        Format::Imm(..) => Format::Imm(addr),
        Format::RegRegImm(..) => Format::RegRegImm(rs, rt, addr),
        Format::RegRegReg(..) => Format::RegRegReg(rs, rt, rd),
        _ => todo!()
    };

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
}
