//! MIPS V disassembler.

use super::{Error, DecodableInstruction};
use std::borrow::Cow;

macro_rules! operands {
    [] => {([$crate::disassembler::EMPTY_OPERAND; 3], 0)};
    [$($x:expr),+ $(,)?] => {{
        let mut operands = [$crate::disassembler::EMPTY_OPERAND; 3];
        let mut idx = 0;
        $(
            idx += 1;
            operands[idx - 1] = $x;
        )*

        (operands, idx)
    }};
}

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

enum Format {
    R,
    I,
    J,
}

pub(super) struct Stream<'data> {
    pub bytes: &'data [u8],
    pub offset: usize,
}

pub(super) struct Instruction {
    mnemomic: &'static str,
    operands: [std::borrow::Cow<'static, str>; 3],
    operand_count: usize,
}

struct TableInstruction {
    mnemomic: &'static str,
    #[allow(dead_code)]
    desc: &'static str,
    format: &'static [usize],
}

impl super::DecodableInstruction for Instruction {
    fn decode(&self) -> String {
        let mut repr = self.mnemomic.to_string();
        let operands = self.operands();

        if !operands.is_empty() {
            repr += " ";
        }

        if operands.len() > 1 {
            for operand in &operands[..operands.len() - 1] {
                repr += operand;
                repr += ", ";
            }
        }

        repr += &operands[operands.len() - 1];
        repr
    }

    fn operands(&self) -> &[std::borrow::Cow<'static, str>] {
        &self.operands[..self.operand_count]
    }
}

impl super::Streamable for Stream<'_> {
    type Item = Instruction;
    type Error = super::Error;

    fn next(&mut self) -> Result<Self::Item, Error> {
        let dword = self
            .bytes
            .get(self.offset..self.offset + 4)
            .map(|b| u32::from_be_bytes([b[0], b[1], b[2], b[3]]))
            .ok_or(Error::NoBytesLeft)? as usize;

        // nop instruction isn't included in any MIPS spec
        if dword == 0b00000000_00000000_00000000_00000000 {
            let (operands, operand_count) = operands![];
            return Ok(Instruction { mnemomic: "nop", operands, operand_count });
        }

        // break instruction has a unique instruction format
        if dword & 0b111111 == 0b001101 {
            let (operands, operand_count) = operands![];
            return Ok(Instruction { mnemomic: "break", operands, operand_count });
        }

        let mut operands = [super::EMPTY_OPERAND; 3];
        let opcode = dword >> 26;
        let funct = dword & 0b111111;

        let (format, inst) = match opcode {
            0 => (Format::R, R_TYPES.get(funct).ok_or(Error::UnknownOpcode)?),
            2 | 3 => (Format::J, J_TYPES.get(opcode).ok_or(Error::UnknownOpcode)?),
            _ => (Format::I, I_TYPES.get(opcode).ok_or(Error::UnknownOpcode)?),
        };

        let rs = dword >> 21 & 0b11111;
        let rt = dword >> 16 & 0b11111;
        let rd = dword >> 11 & 0b11111;

        if inst.mnemomic.is_empty() {
            return Err(Error::InvalidInstruction);
        }

        self.offset += 4;

        match format {
            Format::R => {
                match (REGISTERS.get(rs), REGISTERS.get(rt), REGISTERS.get(rd)) {
                    (Some(_), Some(_), Some(_)) => {}
                    _ => return Err(Error::UnknownRegister),
                }

                let shamt = dword >> 6 & 0b11111;

                for idx in 0..inst.format.len() {
                    // index into next operand
                    let mask = inst.format[idx];

                    // operand specified by the bitmask
                    let operand = match mask {
                        0 => rd,
                        1 => rt,
                        2 => rs,
                        3 => shamt,
                        _ => unsafe { core::hint::unreachable_unchecked() },
                    };

                    if operand == shamt {
                        operands[idx] = Cow::Owned(format!("0x{shamt:x}"));
                    } else {
                        operands[idx] = Cow::Borrowed(REGISTERS[operand]);
                    }
                }

                Ok(Instruction {
                    mnemomic: inst.mnemomic,
                    operands,
                    operand_count: inst.format.len(),
                })
            }
            Format::I => {
                match (REGISTERS.get(rs), REGISTERS.get(rt)) {
                    (Some(_), Some(_)) => {}
                    _ => return Err(Error::UnknownRegister),
                }

                let immediate = dword & 0b11111111_11111111;

                // check if the instruction uses an offset (load/store instructions)
                if inst.format == [1, 3, 2] {
                    let (operands, operand_count) = operands![
                        Cow::Borrowed(REGISTERS[rt]),
                        Cow::Owned(format!("{immediate:#x}")),
                        Cow::Owned(format!("({})", REGISTERS[rs]))
                    ];

                    return Ok(Instruction {
                        mnemomic: inst.mnemomic,
                        operands,
                        operand_count
                    });
                }

                for idx in 0..inst.format.len() {
                    // index into next operand
                    let mask = inst.format[idx];

                    // operand specified by the bitmask
                    let operand = match mask {
                        0 => rd,
                        1 => rt,
                        2 => rs,
                        3 => immediate,
                        _ => unsafe { core::hint::unreachable_unchecked() },
                    };

                    if operand == immediate {
                        operands[idx] = Cow::Owned(format!("0x{immediate:x}"));
                    } else {
                        operands[idx] = Cow::Borrowed(REGISTERS[operand]);
                    }
                }

                Ok(Instruction {
                    mnemomic: inst.mnemomic,
                    operands,
                    operand_count: inst.format.len(),
                })
            }
            Format::J => {
                let immediate = dword & 0b11111111_11111111_11111111;
                let (operands, operand_count) = operands![Cow::Owned(format!("0x{immediate:x}"))];

                Ok(Instruction { mnemomic: inst.mnemomic, operands, operand_count })
            }
        }
    }

    fn format(&self, next: Result<Self::Item, Error>) -> Option<String> {
        match next {
            Err(Error::NoBytesLeft) => None,
            Err(err) => {
                let mut fmt = String::new();

                let bytes = &self.bytes[self.offset - 4..][..4];
                let bytes: Vec<String> =
                    bytes.iter().map(|byte| format!("{:02x}", byte)).collect();

                let bytes = bytes.join(" ");

                fmt += &format!("\t{bytes:11}  <{err:?}>");

                Some(fmt)
            }
            Ok(mut inst) => {
                let mut fmt = String::new();

                let bytes = &self.bytes[self.offset - 4..][..4];
                let bytes: Vec<String> =
                    bytes.iter().map(|byte| format!("{:02x}", byte)).collect();

                let bytes = bytes.join(" ");

                fmt += &format!("\t{bytes:11}  {}", inst.psuedo_decode());
                Some(fmt)

                // Some(self.section_base + self.offset, fmt))
            }
        }
    }
}

/// Bitmask for order of operands [rd, rt, rs, imm].
macro_rules! mips {
    () => {
        $crate::disassembler::mips::TableInstruction { mnemomic: "", desc: "", format: &[] }
    };

    ($mnemomic:literal : $desc:literal, rd, rt, imm) => {
        $crate::disassembler::mips::TableInstruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: &[0, 1, 3],
        }
    };

    ($mnemomic:literal : $desc:literal, rt, rs, imm) => {
        $crate::disassembler::mips::TableInstruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: &[1, 2, 3],
        }
    };

    ($mnemomic:literal : $desc:literal, rs, rt, imm) => {
        $crate::disassembler::mips::TableInstruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: &[2, 1, 3],
        }
    };

    ($mnemomic:literal : $desc:literal, rd, rt, rs) => {
        $crate::disassembler::mips::TableInstruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: &[0, 1, 2],
        }
    };

    ($mnemomic:literal : $desc:literal, rd, rs, rt) => {
        $crate::disassembler::mips::TableInstruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: &[0, 2, 1],
        }
    };

    ($mnemomic:literal : $desc:literal, rt, imm, rs) => {
        $crate::disassembler::mips::TableInstruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: &[1, 3, 2],
        }
    };

    ($mnemomic:literal : $desc:literal, rs, imm) => {
        $crate::disassembler::mips::TableInstruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: &[2, 3],
        }
    };

    ($mnemomic:literal : $desc:literal, rt, imm) => {
        $crate::disassembler::mips::TableInstruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: &[1, 3],
        }
    };

    ($mnemomic:literal : $desc:literal, rd, rs) => {
        $crate::disassembler::mips::TableInstruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: &[0, 2],
        }
    };

    ($mnemomic:literal : $desc:literal, rs, rt) => {
        $crate::disassembler::mips::TableInstruction {
            mnemomic: $mnemomic,
            desc: $desc,
            format: &[2, 1],
        }
    };

    ($mnemomic:literal : $desc:literal, imm) => {
        $crate::disassembler::mips::TableInstruction { mnemomic: $mnemomic, desc: $desc, format: &[3] }
    };

    ($mnemomic:literal : $desc:literal, rs) => {
        $crate::disassembler::mips::TableInstruction { mnemomic: $mnemomic, desc: $desc, format: &[2] }
    };

    ($mnemomic:literal : $desc:literal, rd) => {
        $crate::disassembler::mips::TableInstruction { mnemomic: $mnemomic, desc: $desc, format: &[0] }
    };

    ($mnemomic:literal : $desc:literal) => {
        $crate::disassembler::mips::TableInstruction { mnemomic: $mnemomic, desc: $desc, format: &[] }
    };
}

const I_TYPES: [TableInstruction; 44] = [
    mips!("bgez" : "Branch to immediate if value of $rs is greater than or equal to zero", rs, imm),
    mips!(),
    mips!(),
    mips!(),
    mips!("beq" : "Branch to immediate if values of $rs and $rt are equal", rs, rt, imm),
    mips!("bne" : "Branch to immediate if values of $rs and $rt are not equal", rs, rt, imm),
    mips!("blez" : "Branch to immediate if value of $rs is less than or equal to zero", rs, imm),
    mips!("bgtz" : "Branch to immediate if value of $rs is greater than or equal to zero", rs, imm),
    mips!("addi" : "Add $rs to the immediate and store result in $rt (signed)", rt, rs, imm),
    mips!("addiu" : "Add $rs to the immediate and store result in $rt (unsigned)", rt, rs, imm),
    mips!("slti" : "If $rs is less then immediate, $rt is set to 1 otherwise to 0 (signed)", rt, rs, imm),
    mips!("sltiu" : "If $rs is less then immediate, $rt is set to 1 otherwise to 0 (unsigned)", rt, rs, imm),
    mips!("andi" : "Bitwise AND between $rs and immediate storing the result in $rt", rt, rs, imm),
    mips!("ori" : "Bitwise OR between $rs and immediate storing the result in $rt", rt, rs, imm),
    mips!("xori" : "Bitwise XOR between $rs and immediate storing the result in $rt", rt, rs, imm),
    mips!("lui" : "Stores immediate in the upper 16 bits of $rt", rt, imm),
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
    mips!("lb" : "Load byte from value at address in $rs with a given offset into $rt sign extending it to 32 bits (signed)", rt, imm, rs),
    mips!("lh" : "Load 2 bytes from value at address in $rs with a given offset into $rt sign extending it to 32 bits (signed)", rt, imm, rs),
    mips!("lwl" : "Load the most-significant part of a word as a signed value from an unaligned memory address", rt, imm, rs),
    mips!("lw" : "Load 4 bytes from value at address in $rs with a given offset into $rt (signed)", rt, imm, rs),
    mips!("lbu" : "Load byte from value at address in $rs with a given offset into $rt sign extending it to 32 bits (unsigned)", rt, imm, rs),
    mips!("lhu" : "Load 2 bytes from value at address in $rs with a given offset into $rt sign extending it to 32 bits (unsigned)", rt, imm, rs),
    mips!("lwr" : "Load the least-significant part of a word as a signed value from an unaligned memory address", rt, imm, rs),
    mips!("lwu" : "Load 4 bytes from value at address in $rs with a given offset into $rt (unsigned)", rt, imm, rs),
    mips!("sb" : "Store byte at address in $rs with a given offset into $rt", rt, imm, rs),
    mips!("sh" : "Store 2 bytes at address in $rs with a given offset into $rt", rt, imm, rs),
    mips!(),
    mips!("sw" : "Store 4 bytes at address in $rs with a given offset into $rt", rt, imm, rs),
];

const J_TYPES: [TableInstruction; 4] = [
    mips!(),
    mips!(),
    mips!("j" : "Jump to target address", imm),
    mips!("jr" : "Call the target address and save return addr in $ra", imm),
];

const R_TYPES: [TableInstruction; 44] = [
    mips!("sll" : "Shift value in $rt `immediate` number of times to the left storing the result in $rd and zero extending the shifted bits", rd, rt, imm),
    mips!(),
    mips!("srl" : "Shift value in $rt `immediate` number of times to the right storing the result in $rd and zero extending the shifted bits", rd, rt, imm),
    mips!("sra" : "Shift value in $rt `immediate` number of times to the right storing the result in $rd and sign extending the shifted bits", rd, rt, imm),
    mips!("sllv" : "Shift value in $rt `$rs` number of times to the left storing the result in $rd and zero extending the shifted bits", rd, rt, rs),
    mips!(),
    mips!("srlv" : "Shift value in $rt `$rs` number of times to the right storing the result in $rd and zero extending the shifted bits", rd, rt, rs),
    mips!("srav" : "Shift value in $rt `$rs` number of times to the right storing the result in $rd and sign extending the shifted bits", rd, rt, rs),
    mips!("jr" : "Jump to address of $rs", rs),
    mips!(),
    mips!(),
    mips!("syscall" : "Trigger exception tranfering control from user space to kernel space where the call is handled"),
    mips!(),
    mips!(),
    mips!(),
    mips!("mfhi" : "Store value from $hi (internal register used for multiplication/division) in $rd", rd),
    mips!("mthi" : "Store value from $rs in $hi (internal register used for multiplication/division)", rs),
    mips!("mflo" : "Store value from $lo (internal register used for multiplication/division) in $rd", rd),
    mips!("mtlo" : "Store value from $rs in $lo (internal register used for multiplication/division)", rs),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!("mult" : "Multiply $rs and $rt (signed) storing the result in the split between $hi and $lo (internal registers)", rs, rt),
    mips!("multu" : "Multiply $rs and $rt (unsigned) storing the result in the split between $hi and $lo (internal registers)", rs, rt),
    mips!("div" : "Divide $rs by $rt (signed) storing the result in the split between $hi and $lo (internal registers)", rs, rt),
    mips!("divu" : "Divide $rs by $rt (unsigned) storing the result in the split between $hi and $lo (internal registers)", rs, rt),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!(),
    mips!("add" : "Add $rs to $rt storing the result in $rd (signed)", rd, rs, rt),
    mips!("addu" : "Add $rs to $rt storing the result in $rd (unsigned)", rd, rs, rt),
    mips!("sub" : "Subtract $rs from $rt storing the result in $rd (signed)", rd, rs, rt),
    mips!("subu" : "Subtract $rs from $rt storing the result in $rd (unsigned)", rd, rs, rt),
    mips!("and" : "Bitwise AND between $rs and $rt storing the result in $rd", rd, rs, rt),
    mips!("or" : "Bitwise OR between $rs and $rt storing the result in $rd", rd, rs, rt),
    mips!("xor" : "Bitwise XOR between $rs and $rt storing the result in $rd", rd, rs, rt),
    mips!("nor" : "Bitwise NOR between $rs and $rt storing the result in $rd", rd, rs, rt),
    mips!(),
    mips!(),
    mips!("slt" : "If $rs is less then $rt, $rd is set to 1 otherwise to 0 (signed)", rd, rs, rt),
    mips!("sltu" : "If $rs is less then $rt, $rd is set to 1 otherwise to 0 (unsigned)", rd, rs, rt),
];

#[cfg(test)]
mod tests {
    macro_rules! eq {
        ([$($bytes:tt),+] => $mnemomic:literal, $($operand:literal),*) => {{
            use $crate::disassembler::Streamable;

            let mut stream = $crate::disassembler::mips::Stream {
                bytes: &[$($bytes),+],
                offset: 0,
            };

            match stream.next() {
                Ok(inst) => {
                    assert_eq!(inst.mnemomic, $mnemomic);

                    let mut idx = 0;
                    $(
                        idx += 1;

                        assert_eq!(
                            $operand,
                            inst.operands.get(idx - 1).expect("not enough operands")
                        );
                    )*
                }
                Err(e) => panic!("failed to decode instruction: {e:?}"),
            }
        }};
    }

    #[test]
    fn jump() {
        eq!([0x9, 0, 0, 0] => "j", "0x0");
    }

    #[test]
    fn beq() {
        eq!([0x11, 0x2a, 0x10, 0x0] => "beq", "$t1", "$t2", "0x1000");
    }

    #[test]
    fn sll() {
        eq!([0x0, 0xa, 0x4c, 0x80] => "sll", "$t1", "$t2", "0x12");
    }

    #[test]
    fn sllv() {
        eq!([0x1, 0x49, 0x48, 0x4] => "sllv", "$t1", "$t1", "$t2");
    }

    #[test]
    fn lb() {
        eq!([0x81, 0x49, 0x0, 0x10] => "lb", "$t1", "0x10", "($t2)")
    }
}
