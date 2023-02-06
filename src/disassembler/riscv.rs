//! riscv64gc/riscv32gc disdisassembler

use super::{Error, GenericInstruction};
use object::Architecture as Arch;

use std::borrow::Cow;

macro_rules! riscv {
    () => {
        $crate::disassembler::riscv::Instruction {
            mnemomic: "",
            format: $crate::disassembler::riscv::Format::Unique,
        }
    };

    ($mnemomic:literal, $format:expr) => {
        $crate::disassembler::riscv::Instruction { mnemomic: $mnemomic, format: $format }
    };
}

// NOTE: registers starting with f have to be floating-point whilst all other are integers
#[rustfmt::skip]
pub const REGISTERS: [&str; 63] = [
    "zero", "ra", "sp", "gp", "tp",
    "t0", "t2", "t3",
    "s0", "s1",
    "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
    "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
    "t3", "t4", "t5", "t6",
    "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15", 
    "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23", "f24", "f25", "f26", "f27", "f28",
    "f29", "f30", "f31"
];

#[derive(Debug, Clone, Copy)]
enum Format {
    Unique,
    R,
    I,
    S,
    B,
    U,
    J,
    A,
    CR,
    CI,
    CSS,
    CIW,
    CL,
    CS,
    CA,
    CB,
    CJ,
}

#[derive(Debug, Clone, Copy)]
struct Instruction {
    mnemomic: &'static str,
    format: Format,
}

pub(super) fn next(stream: &mut super::InstructionStream) -> Result<GenericInstruction, Error> {
    let dword = stream
        .bytes
        .get(stream.start..stream.start + 4)
        .map(|b| u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
        .ok_or(Error::NoBytesLeft)? as usize;

    let opcode = dword & 0b111111;
    let mut operands = [super::EMPTY_OPERAND; 5];

    // the instruction is compressed
    if dword as u16 & 0b11 != 0b11 {
        let word = dword as u16;
        let opcode = word & 0b11;
        let jump3 = word >> 13 & 0b111;

        return match opcode {
            0b00 => {
                let f1 = word >> 7 & 0b111;
                let f2 = word >> 2 & 0b111;
                let uimm = word >> 4 & 0b11 + word >> 10 & 0b111;

                let inst = match jump3 {
                    0b000 => riscv!("addi4spn", Format::Unique),
                    0b001 => riscv!("fld", Format::CL),
                    0b010 => riscv!("lw", Format::CL),
                    0b011 if stream.arch == Arch::Riscv32 => riscv!("flw", Format::CL),
                    0b011 if stream.arch == Arch::Riscv64 => riscv!("ld", Format::CL),
                    0b101 => riscv!("fsd", Format::CL),
                    0b110 => riscv!("sw", Format::CL),
                    0b111 if stream.arch == Arch::Riscv32 => riscv!("fsw", Format::CL),
                    0b111 if stream.arch == Arch::Riscv64 => riscv!("sd", Format::CL),
                    _ => return Err(Error::UnknownOpcode),
                };

                let operand_count = match inst.format {
                    Format::Unique => 0,
                    Format::CL => {
                        let rs1 = REGISTERS.get(f1 as usize).ok_or(Error::UnknownRegister)?;
                        let rs2 = REGISTERS.get(f2 as usize).ok_or(Error::UnknownRegister)?;

                        operands[0] = Cow::Borrowed(rs1);
                        operands[1] = Cow::Owned(format!("{uimm}({rs2})"));

                        2
                    }
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                Ok(GenericInstruction {
                    width: 2,
                    mnemomic: inst.mnemomic,
                    operands,
                    operand_count,
                })
            }
            0b01 => {
                let f1 = word >> 12 & 0b1;
                let f2 = word >> 7 & 0b11111;
                let f3 = word >> 2 & 0b11111;
                let nzimm = word >> 2 & 0b11111 + f1 << 6;

                let inst = match jump3 {
                    0b000 if f2 == 0 => riscv!("nop", Format::Unique),
                    0b000 if f2 != 0 => riscv!("addi", Format::CI),
                    0b001 if stream.arch == Arch::Riscv32 => riscv!("jal", Format::CJ),
                    0b001 if stream.arch == Arch::Riscv64 => riscv!("addiw", Format::CI),
                    0b010 => riscv!("li", Format::CI),
                    0b011 if f2 == 2 => riscv!("addi16sp", Format::CI),
                    0b011 if f2 != 2 => riscv!("lui", Format::CI),
                    0b100 => match f2 >> 10 & 0b11 {
                        0b00 => riscv!("srli", Format::CI),
                        0b01 => riscv!("srai", Format::CI),
                        0b11 => match f2 >> 5 & 0b11 {
                            0b00 if stream.arch == Arch::Riscv32 => riscv!("sub", Format::CA),
                            0b00 if stream.arch == Arch::Riscv64 => riscv!("subw", Format::CA),
                            0b01 if stream.arch == Arch::Riscv64 => riscv!("addw", Format::CA),
                            0b01 => riscv!("xor", Format::CA),
                            0b10 => riscv!("or", Format::CA),
                            0b11 => riscv!("and", Format::CA),
                            _ => return Err(Error::UnknownOpcode),
                        },
                        _ => return Err(Error::UnknownOpcode),
                    },
                    0b101 => riscv!("j", Format::CJ),
                    0b110 => riscv!("beqz", Format::CB),
                    0b111 => riscv!("bnez", Format::CB),
                    _ => return Err(Error::UnknownOpcode),
                };

                let operand_count = match inst.format {
                    Format::Unique => 0,
                    Format::CI => {
                        let rs1 = REGISTERS.get(f2 as usize).ok_or(Error::UnknownRegister)?;

                        operands[0] = Cow::Borrowed(rs1);
                        operands[1] = Cow::Borrowed(rs1);
                        operands[2] = Cow::Owned(format!("{nzimm}"));

                        3
                    }
                    Format::CJ => {
                        let imm = word >> 2 & 0b1110000000000;
                        operands[0] = Cow::Owned(format!("0x{imm:x}"));

                        1
                    }
                    Format::CA => {
                        let rs1 =
                            REGISTERS.get(f2 as usize & 0b111).ok_or(Error::UnknownRegister)?;
                        let rs2 =
                            REGISTERS.get(f3 as usize & 0b111).ok_or(Error::UnknownRegister)?;

                        operands[0] = Cow::Borrowed(rs1);
                        operands[1] = Cow::Borrowed(rs1);
                        operands[2] = Cow::Borrowed(rs2);

                        3
                    }
                    Format::CB => {
                        let imm = word >> 2 & 0b11111 + (word >> 10 & 0b111) << 6;
                        let rs1 =
                            REGISTERS.get(f2 as usize & 0b111).ok_or(Error::UnknownRegister)?;

                        operands[0] = Cow::Borrowed(rs1);
                        operands[1] = Cow::Borrowed(rs1);
                        operands[2] = Cow::Owned(format!("0x{imm}"));

                        3
                    }
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                Ok(GenericInstruction {
                    width: 2,
                    mnemomic: inst.mnemomic,
                    operands,
                    operand_count,
                })
            }
            0b10 => {
                let f1 = word >> 12 & 0b1;
                let f2 = word >> 7 & 0b11111;
                let f3 = word >> 2 & 0b11111;
                let nzimm = word >> 2 & 0b11111 + f1 << 6;

                let inst = match jump3 {
                    0b000 => riscv!("slli", Format::CI),
                    0b001 => riscv!("fldsp", Format::CI),
                    0b010 => riscv!("lwsp", Format::CI),
                    0b011 if stream.arch == Arch::Riscv32 => riscv!("flwsp", Format::CI),
                    0b011 if stream.arch == Arch::Riscv64 => riscv!("ldsp", Format::CI),
                    0b100 if f1 == 0 && f3 == 0 => riscv!("jr", Format::CI),
                    0b100 if f1 == 0 && f3 != 0 => riscv!("mv", Format::CI),
                    0b100 if f1 == 1 && f2 == 0 && f3 == 0 => riscv!("ebreak", Format::CI),
                    0b100 if f1 == 1 && f2 != 0 && f3 == 0 => riscv!("jalr", Format::CI),
                    0b100 if f1 == 1 && f2 != 0 && f3 != 0 => riscv!("add", Format::CI),
                    0b101 => riscv!("fsdsp", Format::CSS),
                    0b110 => riscv!("swsp", Format::CSS),
                    0b111 if stream.arch == Arch::Riscv32 => riscv!("fswsp", Format::CSS),
                    0b111 if stream.arch == Arch::Riscv64 => riscv!("sdsp", Format::CSS),
                    _ => return Err(Error::UnknownOpcode),
                };

                let operand_count = match inst.format {
                    Format::CI => {
                        let rs1 = REGISTERS.get(f2 as usize).ok_or(Error::UnknownRegister)?;

                        operands[0] = Cow::Borrowed(rs1);
                        operands[1] = Cow::Borrowed(rs1);
                        operands[2] = Cow::Owned(format!("{nzimm}"));

                        3
                    }
                    Format::CSS => {
                        //println!("{word:#018b}");
                        let uimm = ((word >> 7) & 0b11111) * 8;
                        let rs1 = REGISTERS.get(f3 as usize).ok_or(Error::UnknownRegister)?;

                        operands[0] = Cow::Borrowed(rs1);
                        operands[1] = Cow::Owned(format!("{uimm}"));

                        2
                    }
                    _ => 0,
                };

                Ok(GenericInstruction {
                    width: 2,
                    mnemomic: inst.mnemomic,
                    operands,
                    operand_count,
                })
            }
            _ => return Err(Error::UnknownOpcode),
        };
    }

    if opcode == 0b0001111 {
        return Ok(GenericInstruction { width: 4, mnemomic: "fence", operands, operand_count: 0 });
    }

    if dword == 0b000000000000_00000_000_00000_1110011 {
        return Ok(GenericInstruction { width: 4, mnemomic: "ecall", operands, operand_count: 0 });
    }

    if dword == 0b000000000001_00000_000_00000_1110011 {
        return Ok(GenericInstruction { width: 4, mnemomic: "ebreak", operands, operand_count: 0 });
    }

    let mut inst = match opcode {
        0b0110111 => riscv!("lui", Format::U),
        0b0010111 => riscv!("auipc", Format::U),
        0b1101111 => riscv!("jal", Format::J),
        0b1100111 => riscv!("jalr", Format::I),
        0b1100011 => match dword >> 12 & 0b111 {
            0b000 => riscv!("beq", Format::B),
            0b001 => riscv!("bne", Format::B),
            0b100 => riscv!("blt", Format::B),
            0b101 => riscv!("bge", Format::B),
            0b110 => riscv!("bltu", Format::B),
            0b111 => riscv!("bgeu", Format::B),
            _ => return Err(Error::UnknownOpcode),
        },
        0b0000011 => match dword >> 12 & 0b111 {
            0b000 => riscv!("lb", Format::I),
            0b001 => riscv!("lh", Format::I),
            0b010 => riscv!("lw", Format::I),
            0b011 if stream.arch == Arch::Riscv64 => riscv!("ld", Format::I),
            0b100 => riscv!("lbu", Format::I),
            0b101 => riscv!("lhu", Format::I),
            0b110 if stream.arch == Arch::Riscv64 => riscv!("lwu", Format::I),
            _ => return Err(Error::UnknownOpcode),
        },
        0b0100011 => match dword >> 12 & 0b111 {
            0b000 => riscv!("sb", Format::S),
            0b001 => riscv!("sh", Format::S),
            0b010 => riscv!("sw", Format::S),
            0b011 if stream.arch == Arch::Riscv64 => riscv!("sd", Format::S),
            _ => return Err(Error::UnknownOpcode),
        },
        0b0010011 => match dword >> 12 & 0b111 {
            0b000 => riscv!("addi", Format::I),
            0b010 => riscv!("alti", Format::I),
            0b011 => riscv!("altiu", Format::I),
            0b100 => riscv!("xori", Format::I),
            0b110 => riscv!("ori", Format::I),
            0b111 => riscv!("andi", Format::I),
            0b001 => riscv!("slli", Format::A),
            0b101 if dword >> 25 == 0b0000000 => riscv!("srli", Format::A),
            0b101 if dword >> 25 == 0b0100000 => riscv!("srai", Format::A),
            _ => return Err(Error::UnknownOpcode),
        },
        0b0011011 => match dword >> 12 & 0b111 {
            _ if stream.arch == Arch::Riscv32 => return Err(Error::UnknownOpcode),
            0b000 => riscv!("addiw", Format::I),
            0b001 => riscv!("slliw", Format::A),
            0b101 if dword >> 25 == 0b0000000 => riscv!("srliw", Format::A),
            0b101 if dword >> 25 == 0b0100000 => riscv!("sraiw", Format::A),
            _ => return Err(Error::UnknownOpcode)
        }
        0b0110011 => match dword >> 25 {
            0b0000000 => match dword >> 12 & 0b111 {
                0b000 => riscv!("add", Format::R),
                0b001 => riscv!("sll", Format::R),
                0b010 => riscv!("slt", Format::R),
                0b011 => riscv!("sltu", Format::R),
                0b100 => riscv!("xor", Format::R),
                0b101 => riscv!("srl", Format::R),
                0b110 => riscv!("or", Format::R),
                0b111 => riscv!("and", Format::R),
                _ => return Err(Error::UnknownOpcode),
            },
            0b0100000 => match dword >> 12 & 0b111 {
                0b000 => riscv!("sub", Format::R),
                0b101 => riscv!("sra", Format::R),
                _ => return Err(Error::UnknownOpcode),
            },
            _ => return Err(Error::UnknownOpcode),
        },
        0b0111011 => match dword >> 25 {
            _ if stream.arch == Arch::Riscv32 => return Err(Error::UnknownOpcode),
            0b0000000 => match dword >> 12 & 0b111 {
                0b000 => riscv!("addw", Format::R),
                0b001 => riscv!("sllw", Format::R),
                0b101 => riscv!("srlw", Format::R),
                _ => return Err(Error::UnknownOpcode),
            },
            0b0100000 => match dword >> 12 & 0b111 {
                0b000 => riscv!("subw", Format::R),
                0b101 => riscv!("sraw", Format::R),
                _ => return Err(Error::UnknownOpcode),
            },
            _ => return Err(Error::UnknownOpcode),
        }
        _ => return Err(Error::UnknownOpcode),
    };

    let operand_count = match inst.format {
        Format::Unique => 0,
        Format::R => {
            let rd = REGISTERS.get(dword >> 7 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs1 = REGISTERS.get(dword >> 15 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs2 = REGISTERS.get(dword >> 20 & 0b1111).ok_or(Error::UnknownRegister)?;

            operands[0] = Cow::Borrowed(rd);
            operands[1] = Cow::Borrowed(rs1);
            operands[2] = Cow::Borrowed(rs2);

            3
        }
        Format::I => {
            let rd = REGISTERS.get(dword >> 7 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs1 = REGISTERS.get(dword >> 15 & 0b1111).ok_or(Error::UnknownRegister)?;
            let imm = dword >> 20;

            operands[0] = Cow::Borrowed(rd);
            operands[1] = Cow::Borrowed(rs1);
            operands[2] = Cow::Owned(format!("{imm}"));

            3
        }
        Format::S => {
            let imm = dword >> 7 & 0b1111 + dword >> 20 << 5;
            let rs1 = REGISTERS.get(dword >> 15 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs2 = REGISTERS.get(dword >> 20 & 0b1111).ok_or(Error::UnknownRegister)?;

            operands[0] = Cow::Borrowed(rs1);
            operands[1] = Cow::Owned(format!("{imm}({rs2})"));

            2
        }
        Format::B => {
            let imm = dword >> 7 & 0b1111 + dword >> 20 << 5;
            let rs1 = REGISTERS.get(dword >> 15 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs2 = REGISTERS.get(dword >> 20 & 0b1111).ok_or(Error::UnknownRegister)?;

            operands[0] = Cow::Borrowed(rs1);
            operands[1] = Cow::Borrowed(rs2);
            operands[2] = Cow::Owned(format!("{imm}"));

            3
        }
        Format::U => {
            let imm = dword >> 12;
            let rd = REGISTERS.get(dword >> 7 & 0b1111).ok_or(Error::UnknownRegister)?;

            operands[0] = Cow::Borrowed(rd);
            operands[1] = Cow::Owned(format!("{imm}"));

            2
        }
        Format::J => {
            let imm = dword >> 12;
            let rd = dword >> 7 & 0b1111;

            if rd == 0 && inst.mnemomic == "jal" {
                inst.mnemomic = "j";
                operands[0] = Cow::Owned(format!("0x{imm:x}"));
                1
            } else {
                operands[0] = Cow::Borrowed(REGISTERS.get(rd).ok_or(Error::UnknownRegister)?);
                operands[1] = Cow::Owned(format!("0x{imm:x}"));
                2
            }
        }
        Format::A => {
            let rd = REGISTERS.get(dword >> 7 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs1 = REGISTERS.get(dword >> 15 & 0b1111).ok_or(Error::UnknownRegister)?;

            operands[0] = Cow::Borrowed(rd);
            operands[1] = Cow::Borrowed(rs1);

            if stream.arch == Arch::Riscv32 {
                let shamt = dword >> 20 & 0b11111;
                operands[2] = Cow::Owned(format!("{shamt}"));
            }

            if stream.arch == Arch::Riscv64 {
                let shamt = dword >> 20 & 0b1111;
                operands[2] = Cow::Owned(format!("{shamt}"));
            }

            3
        }
        _ => unsafe { core::hint::unreachable_unchecked() }
    };

    Ok(GenericInstruction { width: 4, mnemomic: inst.mnemomic, operands, operand_count })
}
