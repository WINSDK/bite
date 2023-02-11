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
pub const ABI_REGISTERS: [&str; 63] = [
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

pub const INT_COMP_REGISTERS: [&str; 8] = ["s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5"];
pub const FP_COMP_REGISTERS: [&str; 8] = ["fs0", "fs1", "fa0", "fa1", "fa2", "fa3", "fa4", "fa5"];

#[allow(dead_code)]
#[derive(Clone, Copy)]
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
    CS,
    CA,
    CB,
    CJ,
}

#[derive(Clone, Copy)]
struct Instruction {
    mnemomic: &'static str,
    format: Format,
}

static PSUEDOS: phf::Map<&str, fn(&mut GenericInstruction)> = phf::phf_map! {
    "li" => |inst| {
         if inst.operands[0] == inst.operands[1] {
             inst.operands.swap(1, 2);
             inst.operand_count = 2;
         }
    },
    "addi" => |inst| {
        if inst.operands[0] == "zero" && inst.operands[1] == "zero" && inst.operands[2] == "0" {
            inst.mnemomic = "nop";
            inst.operand_count = 0;
            return;
        }

        if inst.operands[2] == "0" {
            inst.mnemomic = "mv";
            inst.operand_count = 2;
        }
    },
    "xori" => |inst| {
        if inst.operands[2] == "-1" {
            inst.mnemomic = "not";
            inst.operand_count = 2;
        }
    },
    "sub" => |inst| {
        if inst.operands[1] == "zero" {
            inst.mnemomic = "neg";
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    },
    "subw" => |inst| {
        if inst.operands[1] == "zero" {
            inst.mnemomic = "negw";
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    },
    "addiw" => |inst| {
        if inst.operands[2] == "0" {
            inst.mnemomic = "sext.w";
            inst.operand_count = 2;
        }
    },
    "sltiu" => |inst| {
        if inst.operands[2] == "1" {
            inst.mnemomic = "seqz";
            inst.operand_count = 2;
        }
    },
    "sltu" => |inst| {
        if inst.operands[1] == "zero" {
            inst.mnemomic = "snez";
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    },
    "slt" => |inst| {
        if inst.operands[2] == "zero" {
            inst.mnemomic = "sltz";
            inst.operand_count = 2;
            return;
        }

        if inst.operands[1] == "zero" {
            inst.mnemomic = "sgtz";
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    },
    "fsgnj.s" => |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.mnemomic = "fmv.s";
            inst.operand_count = 2;
        }
    },
    "fsgnjx.s" => |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.mnemomic = "fabs.s";
            inst.operand_count = 2;
        }
    },
    "fsgnjn.s" => |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.mnemomic = "fneg.s";
            inst.operand_count = 2;
        }
    },
    "fsgnj.d" => |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.mnemomic = "mov.d";
            inst.operand_count = 2;
        }
    },
    "fsgnjx.d" => |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.mnemomic = "fabs.d";
            inst.operand_count = 2;
        }
    },
    "fsgnjn.d" => |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.mnemomic = "fneg.d";
            inst.operand_count = 2;
        }
    },
    "beq" => |inst| {
        if inst.operands[1] == "zero" {
            inst.mnemomic = "beqz";
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    },
    "bne" => |inst| {
        if inst.operands[1] == "zero" {
            inst.mnemomic = "bnez";
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    },
    "bge" => |inst| {
        if inst.operands[0] == "zero" {
            inst.mnemomic = "blez";
            inst.operands.swap(0, 1);
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
            return;
        }

        if inst.operands[1] == "zero" {
            inst.mnemomic = "bgez";
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    },
    "blt" => |inst| {
        if inst.operands[1] == "zero" {
            inst.mnemomic = "bltz";
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
            return;
        }

        if inst.operands[0] == "zero" {
            inst.mnemomic = "bgtz";
            inst.operands.swap(0, 1);
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    },
    "jal" => |inst| {
        if inst.operands[0] == "zero" {
            inst.mnemomic = "j";
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
            return;
        }

        if inst.operands[0] == "ra" {
            inst.mnemomic = "jal";
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
        }
    },
    "jalr" => |inst| {
        if inst.operands[0] == inst.operands[1] && inst.operands[2] == "0" {
            inst.mnemomic = "ret";
            inst.operand_count = 0;
            return;
        }

        if inst.operands[0] == "zero" && inst.operands[2] == "0" {
            inst.mnemomic = "jr";
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
            return;
        }

        if inst.operands[0] == "ra" && inst.operands[2] == "0" {
            inst.mnemomic = "jalr";
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
        }
    },
    "auipc" => |inst| {
        if inst.operands[0] == "t2" {
            todo!();
        }
    }
    // TODO: table p2
};

pub(super) fn next(stream: &mut super::InstructionStream) -> Result<GenericInstruction, Error> {
    let bytes = match stream.bytes.get(stream.start..) {
        Some(bytes) => {
            if bytes.len() < 2 {
                return Err(Error::NoBytesLeft);
            }

            if bytes.len() < 4 {
                u16::from_le_bytes([bytes[0], bytes[1]]) as usize
            } else {
                u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as usize
            }
        }
        None => return Err(Error::NoBytesLeft),
    };

    let opcode = bytes & 0b1111111;
    let mut operands = [super::EMPTY_OPERAND; 5];

    // the instruction is compressed
    if bytes as u16 & 0b11 != 0b11 {
        let bytes = bytes as u16 as usize;
        let opcode = bytes & 0b11;
        let jump3 = bytes >> 13 & 0b111;

        let mut inst = match opcode {
            0b00 => {
                let inst = match jump3 {
                    0b000 => riscv!("addi4spn", Format::Unique),
                    0b001 => riscv!("fld", Format::CS),
                    0b010 => riscv!("lw", Format::CS),
                    0b011 if stream.arch == Arch::Riscv32 => riscv!("flw", Format::CS),
                    0b011 if stream.arch == Arch::Riscv64 => riscv!("ld", Format::CS),
                    0b101 => riscv!("fsd", Format::CS),
                    0b110 => riscv!("sw", Format::CS),
                    0b111 if stream.arch == Arch::Riscv32 => riscv!("fsw", Format::CS),
                    0b111 if stream.arch == Arch::Riscv64 => riscv!("sd", Format::CS),
                    _ => return Err(Error::UnknownOpcode),
                };

                let operand_count = match inst.format {
                    Format::Unique => 0,
                    Format::CS => {
                        let rs1 = INT_COMP_REGISTERS
                            .get(bytes >> 7 & 0b111)
                            .ok_or(Error::UnknownRegister)?;

                        let rs2 = INT_COMP_REGISTERS
                            .get(bytes >> 2 & 0b111)
                            .ok_or(Error::UnknownRegister)?;

                        let imm = bytes >> 5 & 0b11 + ((bytes >> 10 & 0b111) << 3);

                        operands[0] = Cow::Borrowed(rs2);
                        operands[1] = Cow::Borrowed(rs1);
                        operands[2] = Cow::Owned(imm.to_string());

                        3
                    }
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                GenericInstruction { width: 2, mnemomic: inst.mnemomic, operands, operand_count }
            }
            0b01 => {
                let inst = match jump3 {
                    0b000 if bytes >> 7 & 0b11111 == 0 => riscv!("nop", Format::Unique),
                    0b000 if bytes >> 7 & 0b11111 != 0 => riscv!("addi", Format::CI),
                    0b001 if stream.arch == Arch::Riscv32 => riscv!("jal", Format::CJ),
                    0b001 if stream.arch == Arch::Riscv64 => riscv!("addiw", Format::CI),
                    0b010 => riscv!("li", Format::CI),
                    0b011 if bytes >> 7 & 0b11111 == 2 => riscv!("addi16sp", Format::CI),
                    0b011 if bytes >> 7 & 0b11111 != 2 => riscv!("lui", Format::CI),
                    0b100 => match bytes >> 10 & 0b11 {
                        0b00 => riscv!("srli", Format::CI),
                        0b01 => riscv!("srai", Format::CI),
                        0b11 => match bytes >> 5 & 0b11 {
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
                        let rs1 = ABI_REGISTERS
                            .get(bytes >> 7 & 0b11111)
                            .ok_or(Error::UnknownRegister)?;

                        let mut imm = (((bytes >> 7) & 0b100000) | ((bytes >> 2) & 0b11111)) as i16;

                        if imm & 0b100000 != 0 {
                            imm = (imm | 0b11000000) as i8 as i16;
                        }

                        operands[0] = Cow::Borrowed(rs1);
                        operands[1] = Cow::Borrowed(rs1);
                        operands[2] = Cow::Owned(imm.to_string());

                        3
                    }
                    Format::CJ => {
                        let mut imm = 0;

                        imm |= (bytes >> 1) & 0b100000000000;
                        imm |= (bytes << 2) & 0b010000000000;
                        imm |= (bytes >> 1) & 0b001100000000;
                        imm |= (bytes << 1) & 0b000010000000;
                        imm |= (bytes >> 1) & 0b000001000000;
                        imm |= (bytes << 3) & 0b000000100000;
                        imm |= (bytes >> 7) & 0b000000010000;
                        imm |= (bytes >> 2) & 0b000000001110;

                        // cast to i32 to prevent rust overflowing literal complaints
                        let mut imm = imm as i32;

                        if imm & 0b100000000000 != 0 {
                            imm |= (imm | 0b100000000000) as i8 as i32;
                        }

                        operands[0] = Cow::Owned(match imm {
                            i32::MIN..=-1 => format!("-{:#x}", imm.abs()),
                            0..=i32::MAX => format!("{:#x}", imm)
                        });

                        1
                    }
                    Format::CA => {
                        let rs1 = INT_COMP_REGISTERS
                            .get(bytes >> 7 & 0b111)
                            .ok_or(Error::UnknownRegister)?;

                        let rs2 = INT_COMP_REGISTERS
                            .get(bytes >> 2 & 0b111)
                            .ok_or(Error::UnknownRegister)?;

                        operands[0] = Cow::Borrowed(rs2);
                        operands[1] = Cow::Borrowed(rs1);
                        operands[2] = Cow::Borrowed(rs1);

                        3
                    }
                    Format::CB => {
                        let imm = bytes >> 2 & 0b11111 + ((bytes >> 10 & 0b111) << 6);

                        let rs1 = INT_COMP_REGISTERS
                            .get(bytes >> 7 & 0b111)
                            .ok_or(Error::UnknownRegister)?;

                        operands[0] = Cow::Borrowed(rs1);
                        operands[1] = Cow::Borrowed(rs1);
                        operands[2] = Cow::Owned(format!("0x{imm}"));

                        3
                    }
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };

                GenericInstruction { width: 2, mnemomic: inst.mnemomic, operands, operand_count }
            }
            0b10 => {
                let f1 = bytes >> 12 & 0b1;
                let f2 = bytes >> 7 & 0b11111;
                let f3 = bytes >> 2 & 0b11111;

                let inst = match jump3 {
                    0b000 => riscv!("slli", Format::CI),
                    0b001 => riscv!("fldsp", Format::CI),
                    0b010 => riscv!("lwsp", Format::CI),
                    0b011 if stream.arch == Arch::Riscv32 => riscv!("flwsp", Format::CI),
                    0b011 if stream.arch == Arch::Riscv64 => riscv!("ldsp", Format::CI),
                    0b100 if f1 == 0 && f3 == 0 => riscv!("jalr", Format::CI),
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
                        let shamt = bytes >> 2 & 0b11111 + ((bytes >> 12 & 0b1) << 5);
                        let rs1 = ABI_REGISTERS.get(f2).ok_or(Error::UnknownRegister)?;

                        operands[0] = Cow::Borrowed(rs1);
                        operands[1] = Cow::Borrowed(rs1);
                        operands[2] = Cow::Owned(shamt.to_string());

                        3
                    }
                    Format::CSS => {
                        let imm = ((bytes >> 7) & 0b11111) * 8;
                        let rs1 = ABI_REGISTERS.get(f3).ok_or(Error::UnknownRegister)?;

                        operands[0] = Cow::Borrowed(rs1);
                        operands[1] = Cow::Owned(imm.to_string());

                        2
                    }
                    _ => 0,
                };

                GenericInstruction { width: 2, mnemomic: inst.mnemomic, operands, operand_count }
            }
            _ => return Err(Error::UnknownOpcode),
        };

        PSUEDOS.get(inst.mnemomic).map(|map_to_psuedo| map_to_psuedo(&mut inst));
        return Ok(inst);
    }

    if opcode == 0b0001111 {
        return Ok(GenericInstruction { width: 4, mnemomic: "fence", operands, operand_count: 0 });
    }

    if bytes == 0b000000000000_00000_000_00000_1110011 {
        return Ok(GenericInstruction { width: 4, mnemomic: "ecall", operands, operand_count: 0 });
    }

    if bytes == 0b000000000001_00000_000_00000_1110011 {
        return Ok(GenericInstruction { width: 4, mnemomic: "ebreak", operands, operand_count: 0 });
    }

    let inst = match opcode {
        0b0110111 => riscv!("lui", Format::U),
        0b0010111 => riscv!("auipc", Format::U),
        0b1101111 => riscv!("jal", Format::J),
        0b1100111 => riscv!("jalr", Format::I),
        0b1100011 => match bytes >> 12 & 0b111 {
            0b000 => riscv!("beq", Format::B),
            0b001 => riscv!("bne", Format::B),
            0b100 => riscv!("blt", Format::B),
            0b101 => riscv!("bge", Format::B),
            0b110 => riscv!("bltu", Format::B),
            0b111 => riscv!("bgeu", Format::B),
            _ => return Err(Error::UnknownOpcode),
        },
        0b0000011 => match bytes >> 12 & 0b111 {
            0b000 => riscv!("lb", Format::I),
            0b001 => riscv!("lh", Format::I),
            0b010 => riscv!("lw", Format::I),
            0b011 if stream.arch == Arch::Riscv64 => riscv!("ld", Format::I),
            0b100 => riscv!("lbu", Format::I),
            0b101 => riscv!("lhu", Format::I),
            0b110 if stream.arch == Arch::Riscv64 => riscv!("lwu", Format::I),
            _ => return Err(Error::UnknownOpcode),
        },
        0b0100011 => match bytes >> 12 & 0b111 {
            0b000 => riscv!("sb", Format::S),
            0b001 => riscv!("sh", Format::S),
            0b010 => riscv!("sw", Format::S),
            0b011 if stream.arch == Arch::Riscv64 => riscv!("sd", Format::S),
            _ => return Err(Error::UnknownOpcode),
        },
        0b0010011 => match bytes >> 12 & 0b111 {
            0b000 => riscv!("addi", Format::I),
            0b010 => riscv!("alti", Format::I),
            0b011 => riscv!("altiu", Format::I),
            0b100 => riscv!("xori", Format::I),
            0b110 => riscv!("ori", Format::I),
            0b111 => riscv!("andi", Format::I),
            0b001 => riscv!("slli", Format::A),
            0b101 if bytes >> 26 == 0b0000001 => riscv!("srli", Format::A),
            0b101 if bytes >> 26 == 0b0000000 => riscv!("srai", Format::A),
            _ => return Err(Error::UnknownOpcode),
        },
        0b0011011 => match bytes >> 12 & 0b111 {
            _ if stream.arch == Arch::Riscv32 => return Err(Error::UnknownOpcode),
            0b000 => riscv!("addiw", Format::I),
            0b001 => riscv!("slliw", Format::A),
            0b101 if bytes >> 25 == 0b0000000 => riscv!("srliw", Format::A),
            0b101 if bytes >> 25 == 0b0100000 => riscv!("sraiw", Format::A),
            _ => return Err(Error::UnknownOpcode),
        },
        0b0110011 => match bytes >> 25 {
            0b0000000 => match bytes >> 12 & 0b111 {
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
            0b0100000 => match bytes >> 12 & 0b111 {
                0b000 => riscv!("sub", Format::R),
                0b101 => riscv!("sra", Format::R),
                _ => return Err(Error::UnknownOpcode),
            },
            _ => return Err(Error::UnknownOpcode),
        },
        0b0111011 => match bytes >> 25 {
            _ if stream.arch == Arch::Riscv32 => return Err(Error::UnknownOpcode),
            0b0000000 => match bytes >> 12 & 0b111 {
                0b000 => riscv!("addw", Format::R),
                0b001 => riscv!("sllw", Format::R),
                0b101 => riscv!("srlw", Format::R),
                _ => return Err(Error::UnknownOpcode),
            },
            0b0100000 => match bytes >> 12 & 0b111 {
                0b000 => riscv!("subw", Format::R),
                0b101 => riscv!("sraw", Format::R),
                _ => return Err(Error::UnknownOpcode),
            },
            _ => return Err(Error::UnknownOpcode),
        },
        _ => return Err(Error::UnknownOpcode),
    };

    let operand_count = match inst.format {
        Format::Unique => 0,
        Format::R => {
            let rd = ABI_REGISTERS.get(bytes >> 7 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs1 = ABI_REGISTERS.get(bytes >> 15 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs2 = ABI_REGISTERS.get(bytes >> 20 & 0b1111).ok_or(Error::UnknownRegister)?;

            operands[0] = Cow::Borrowed(rd);
            operands[1] = Cow::Borrowed(rs1);
            operands[2] = Cow::Borrowed(rs2);

            3
        }
        Format::I => {
            let rd = ABI_REGISTERS.get(bytes >> 7 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs1 = ABI_REGISTERS.get(bytes >> 15 & 0b1111).ok_or(Error::UnknownRegister)?;
            let imm = bytes >> 20;

            operands[0] = Cow::Borrowed(rd);
            operands[1] = Cow::Borrowed(rs1);
            operands[2] = Cow::Owned(imm.to_string());

            3
        }
        Format::S => {
            let imm = bytes >> 7 & 0b1111 + bytes >> 25 << 5;
            let rs1 = ABI_REGISTERS.get(bytes >> 15 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs2 = ABI_REGISTERS.get(bytes >> 20 & 0b1111).ok_or(Error::UnknownRegister)?;

            operands[0] = Cow::Borrowed(rs1);
            operands[1] = Cow::Borrowed(rs2);
            operands[2] = Cow::Owned(imm.to_string());

            3
        }
        Format::B => {
            let imm = bytes >> 7 & 0b1111 + bytes >> 20 << 5;
            let rs1 = ABI_REGISTERS.get(bytes >> 15 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs2 = ABI_REGISTERS.get(bytes >> 20 & 0b1111).ok_or(Error::UnknownRegister)?;

            operands[0] = Cow::Borrowed(rs1);
            operands[1] = Cow::Borrowed(rs2);
            operands[2] = Cow::Owned(imm.to_string());

            3
        }
        Format::U => {
            let imm = bytes >> 12;
            let rd = ABI_REGISTERS.get(bytes >> 7 & 0b1111).ok_or(Error::UnknownRegister)?;

            operands[0] = Cow::Borrowed(rd);
            operands[1] = Cow::Owned(imm.to_string());

            2
        }
        Format::J => {
            let rd = bytes >> 7 & 0b1111;
            let mut imm = 0;

            // 18 bits (riscv instruction jumps are 16-byte alligned)
            imm += bytes & 0b10000000000000000000000000000000; // 1 bit
            imm += bytes & 0b01111111110000000000000000000000; // 9 bits
            imm += bytes & 0b00000000001000000000000000000000; // 1 bit
            imm += bytes & 0b00000000000111111100000000000000; // 7 bits
            imm >>= 14;

            operands[0] = Cow::Borrowed(ABI_REGISTERS.get(rd).ok_or(Error::UnknownRegister)?);
            operands[1] = Cow::Owned(format!("{imm:#x}"));
            2
        }
        Format::A => {
            let rd = ABI_REGISTERS.get(bytes >> 7 & 0b1111).ok_or(Error::UnknownRegister)?;
            let rs1 = ABI_REGISTERS.get(bytes >> 15 & 0b1111).ok_or(Error::UnknownRegister)?;

            operands[0] = Cow::Borrowed(rd);
            operands[1] = Cow::Borrowed(rs1);

            if stream.arch == Arch::Riscv32 {
                let shamt = bytes >> 20 & 0b11111;
                operands[2] = Cow::Owned(shamt.to_string());
            }

            if stream.arch == Arch::Riscv64 {
                let shamt = bytes >> 20 & 0b1111;
                operands[2] = Cow::Owned(shamt.to_string());
            }

            3
        }
        _ => unsafe { core::hint::unreachable_unchecked() },
    };

    let mut inst =
        GenericInstruction { width: 4, mnemomic: inst.mnemomic, operands, operand_count };
    PSUEDOS.get(inst.mnemomic).map(|map_to_psuedo| map_to_psuedo(&mut inst));
    Ok(inst)
}

#[cfg(test)]
mod tests {
    use crate::disassembler::InstructionStream;
    use object::{Object, ObjectSection, SectionKind};

    use std::io::Write;
    use std::process::Stdio;

    macro_rules! decode_instructions {
        ($code:literal) => {{
            let mut cc = std::process::Command::new("clang")
                .arg("-Oz")
                .arg("-g3")
                .arg("-nostdlib")
                .arg("-ffreestanding")
                .arg("-fuse-ld=lld")
                .arg("--output=/dev/stdout")
                .arg("--target=riscv64-gc-unknown")
                .arg("-Werror")
                .arg("-xc")
                .arg("-")
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .stdin(Stdio::piped())
                .spawn()?;

            cc.stdin.as_mut().unwrap().write($code.as_bytes())?;

            let cc = cc.wait_with_output()?;

            if !cc.stderr.is_empty() {
                eprintln!("{}", String::from_utf8_lossy(&cc.stderr[..]));
            }

            if !cc.status.success() {
                return Err(format!("clang failed with exit code: {}", cc.status).into());
            }

            let binary = object::File::parse(&cc.stdout[..])?;
            let binary = binary
                .sections()
                .filter(|s| s.kind() == SectionKind::Text)
                .find(|t| t.name() == Ok(".text"))
                .expect("failed to find `.text` section")
                .uncompressed_data()?;

            let mut stream = InstructionStream::new(&binary, object::Architecture::Riscv64);
            let mut decoded = Vec::new();

            loop {
                match (stream.interpreter)(&mut stream) {
                    Ok(inst) => {
                        decoded.push(inst.decode());

                        stream.start += inst.width;
                        stream.end += inst.width;
                        stream.end += inst.width * (stream.end != 0) as usize;
                    }
                    Err($crate::disassembler::Error::NoBytesLeft) => break,
                    Err(..) => {
                        decoded.push("????".to_string());

                        stream.start += 4;
                        stream.end += 4;
                        stream.end += 4 * (stream.end != 0) as usize;
                    }
                }
            }

            decoded
        }};
    }

    #[test]
    fn deref() -> Result<(), Box<dyn std::error::Error>> {
        let decoded = decode_instructions!(
            r#"
            int _start() {
                *(int *)0x1000000 = 12;

                return 0;
            }
       "#
        );

        let test = ["lui a0, 4096", "li a1, 12", "sw a1, a0, 0", "li a0, 0", "ret"];

        for (test, decoded) in test.iter().zip(decoded) {
            assert_eq!(test, &decoded);
        }

        Ok(())
    }

    #[test]
    fn jump() -> Result<(), Box<dyn std::error::Error>> {
        let decoded = decode_instructions!(
            r#"
            int _start() {
                __asm__("j -0x12");
            
                return -32;
            }
       "#
        );

        let test = ["li a0, -32", "j -0x12", "ret"];

        for (test, decoded) in test.iter().zip(decoded) {
            assert_eq!(test, &decoded);
        }

        Ok(())
    }

    #[test]
    fn sha256() -> Result<(), Box<dyn std::error::Error>> {
        let decoded = decode_instructions!(
            r#"
            /*********************************************************************
            * Author:     Brad Conte (brad AT bradconte.com)
            * Copyright:
            * Disclaimer: This code is presented "as is" without any guarantees.
            * Details:    Implementation of the SHA-256 hashing algorithm.
                          SHA-256 is one of the three algorithms in the SHA2
                          specification. The others, SHA-384 and SHA-512, are not
                          offered in this implementation.
                          Algorithm specification can be found here:
                           * http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf
                          This implementation uses little endian byte order.
            *********************************************************************/

            /*************************** HEADER FILES ***************************/
            #include <stdint.h>
            #include <stddef.h>

            /****************************** MACROS ******************************/
            #define SHA256_BLOCK_SIZE 32            // SHA256 outputs a 32 byte digest

            /**************************** DATA TYPES ****************************/
            typedef unsigned char BYTE;             // 8-bit byte
            typedef unsigned int  WORD;             // 32-bit word, change to "long" for 16-bit machines

            typedef struct {
                BYTE data[64];
                WORD datalen;
                unsigned long long bitlen;
                WORD state[8];
            } SHA256_CTX;

            /*********************** FUNCTION DECLARATIONS **********************/
            void sha256_init(SHA256_CTX *ctx);
            void sha256_update(SHA256_CTX *ctx, const BYTE data[], size_t len);
            void sha256_final(SHA256_CTX *ctx, BYTE hash[]);

            /****************************** MACROS ******************************/
            #define ROTLEFT(a,b) (((a) << (b)) | ((a) >> (32-(b))))
            #define ROTRIGHT(a,b) (((a) >> (b)) | ((a) << (32-(b))))

            #define CH(x,y,z) (((x) & (y)) ^ (~(x) & (z)))
            #define MAJ(x,y,z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
            #define EP0(x) (ROTRIGHT(x,2) ^ ROTRIGHT(x,13) ^ ROTRIGHT(x,22))
            #define EP1(x) (ROTRIGHT(x,6) ^ ROTRIGHT(x,11) ^ ROTRIGHT(x,25))
            #define SIG0(x) (ROTRIGHT(x,7) ^ ROTRIGHT(x,18) ^ ((x) >> 3))
            #define SIG1(x) (ROTRIGHT(x,17) ^ ROTRIGHT(x,19) ^ ((x) >> 10))

            /**************************** VARIABLES *****************************/
            static const WORD k[64] = {
                0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
                0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
                0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
                0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
                0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
                0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
                0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
                0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2
            };

            /*********************** FUNCTION DEFINITIONS ***********************/
            void* memset(void *s, int c, size_t len) {
                unsigned char *dst = s;
                while (len > 0) {
                    *dst = (unsigned char) c;
                    dst++;
                    len--;
                }
                return s;
            }

            void sha256_transform(SHA256_CTX *ctx, const BYTE data[])
            {
                WORD a, b, c, d, e, f, g, h, i, j, t1, t2, m[64];

                for (i = 0, j = 0; i < 16; ++i, j += 4)
                    m[i] = (data[j] << 24) | (data[j + 1] << 16) | (data[j + 2] << 8) | (data[j + 3]);
                for ( ; i < 64; ++i)
                    m[i] = SIG1(m[i - 2]) + m[i - 7] + SIG0(m[i - 15]) + m[i - 16];

                a = ctx->state[0];
                b = ctx->state[1];
                c = ctx->state[2];
                d = ctx->state[3];
                e = ctx->state[4];
                f = ctx->state[5];
                g = ctx->state[6];
                h = ctx->state[7];

                for (i = 0; i < 64; ++i) {
                    t1 = h + EP1(e) + CH(e,f,g) + k[i] + m[i];
                    t2 = EP0(a) + MAJ(a,b,c);
                    h = g;
                    g = f;
                    f = e;
                    e = d + t1;
                    d = c;
                    c = b;
                    b = a;
                    a = t1 + t2;
                }

                ctx->state[0] += a;
                ctx->state[1] += b;
                ctx->state[2] += c;
                ctx->state[3] += d;
                ctx->state[4] += e;
                ctx->state[5] += f;
                ctx->state[6] += g;
                ctx->state[7] += h;
            }

            void sha256_init(SHA256_CTX *ctx)
            {
                ctx->datalen = 0;
                ctx->bitlen = 0;
                ctx->state[0] = 0x6a09e667;
                ctx->state[1] = 0xbb67ae85;
                ctx->state[2] = 0x3c6ef372;
                ctx->state[3] = 0xa54ff53a;
                ctx->state[4] = 0x510e527f;
                ctx->state[5] = 0x9b05688c;
                ctx->state[6] = 0x1f83d9ab;
                ctx->state[7] = 0x5be0cd19;
            }

            void sha256_update(SHA256_CTX *ctx, const BYTE data[], size_t len)
            {
                WORD i;

                for (i = 0; i < len; ++i) {
                    ctx->data[ctx->datalen] = data[i];
                    ctx->datalen++;
                    if (ctx->datalen == 64) {
                        sha256_transform(ctx, ctx->data);
                        ctx->bitlen += 512;
                        ctx->datalen = 0;
                    }
                }
            }

            void sha256_final(SHA256_CTX *ctx, BYTE hash[])
            {
                WORD i;

                i = ctx->datalen;

                // Pad whatever data is left in the buffer.
                if (ctx->datalen < 56) {
                    ctx->data[i++] = 0x80;
                    while (i < 56)
                        ctx->data[i++] = 0x00;
                }
                else {
                    ctx->data[i++] = 0x80;
                    while (i < 64)
                        ctx->data[i++] = 0x00;
                    sha256_transform(ctx, ctx->data);
                    memset(ctx->data, 0, 56);
                }

                // Append to the padding the total message's length in bits and transform.
                ctx->bitlen += ctx->datalen * 8;
                ctx->data[63] = ctx->bitlen;
                ctx->data[62] = ctx->bitlen >> 8;
                ctx->data[61] = ctx->bitlen >> 16;
                ctx->data[60] = ctx->bitlen >> 24;
                ctx->data[59] = ctx->bitlen >> 32;
                ctx->data[58] = ctx->bitlen >> 40;
                ctx->data[57] = ctx->bitlen >> 48;
                ctx->data[56] = ctx->bitlen >> 56;
                sha256_transform(ctx, ctx->data);

                // Since this implementation uses little endian byte ordering and SHA uses big endian,
                // reverse all the bytes when copying the final state to the output hash.
                for (i = 0; i < 4; ++i) {
                    hash[i]      = (ctx->state[0] >> (24 - i * 8)) & 0x000000ff;
                    hash[i + 4]  = (ctx->state[1] >> (24 - i * 8)) & 0x000000ff;
                    hash[i + 8]  = (ctx->state[2] >> (24 - i * 8)) & 0x000000ff;
                    hash[i + 12] = (ctx->state[3] >> (24 - i * 8)) & 0x000000ff;
                    hash[i + 16] = (ctx->state[4] >> (24 - i * 8)) & 0x000000ff;
                    hash[i + 20] = (ctx->state[5] >> (24 - i * 8)) & 0x000000ff;
                    hash[i + 24] = (ctx->state[6] >> (24 - i * 8)) & 0x000000ff;
                    hash[i + 28] = (ctx->state[7] >> (24 - i * 8)) & 0x000000ff;
                }
            }

            void _start()
            {
                SHA256_CTX ctx;

                sha256_init(&ctx);
                sha256_update(&ctx, (BYTE*)0x1000, 1024);
                sha256_final(&ctx, (BYTE*)0x2000);
            }
       "#
        );

       let test = [
            "li a3, 0",
            "beq a2, a3, 0x1126c",
            "add a4, a0, a3",
            "sb a1, a4, 0",
            "addi a3, a3, 1",
            "bne a2, a3, 0x1125e",
            "ret",
            "addi sp, sp, -352",
            "sd s0, sp, 344",
            "sd s1, sp, 336",
            "sd s2, sp, 328",
            "sd s3, sp, 320",
            "sd s4, sp, 312",
            "sd s5, sp, 304",
            "sd s6, sp, 296",
            "sd s7, sp, 288",
            "sd s8, sp, 280",
            "sd s9, sp, 272",
            "sd s10, sp, 264",
            "sd s11, sp, 256",
            "li a2, 0",
            "li a3, 64",
            "mv a6, sp",
            "beq a2, a3, 0x112c0",
            "add a5, a1, a2",
            "lb s1, a5, 0",
            "lbu s0, a5, 1",
            "slli s1, s1, 24",
            "lbu a4, a5, 2",
            "slli s0, s0, 16",
            "lbu a5, a5, 3",
            "or s1, s1, s0",
            "slli a4, a4, 8",
            "or a4, a4, s1",
            "or a4, a4, a5",
            "add a5, a6, a2",
            "sw a4, a5, 0",
            "addi a2, a2, 4",
            "bne a2, a3, 0x11294",
            "li a1, 0",
            "li a7, 192",
            "mv a6, sp",
            "beq a1, a7, 0x11320",
            "add a4, a6, a1",
            "lwu a5, a4, 56",
            "srli s1, a5, 17",
            "slliw s0, a5, 15",
            "or s1, s1, s0",
            "srli s0, a5, 19",
            "slliw a3, a5, 13",
            "or a3, a3, s0",
            "xor a3, a3, s1",
            "lw s1, a4, 36",
            "lwu s0, a4, 4",
            "srli a5, a5, 10",
            "xor a3, a3, a5",
            "addw a3, a3, s1",
            "srli a5, s0, 7",
            "slli s1, s0, 25",
            "or a5, a5, s1",
            "srli s1, s0, 18",
            "slli a2, s0, 14",
            "or a2, a2, s1",
            "lw s1, a4, 0",
            "xor a2, a2, a5",
            "srli a5, s0, 3",
            "xor a2, a2, a5",
            "addw a3, a3, s1",
            "addw a2, a2, a3",
            "sw a2, a4, 64",
            "addi a1, a1, 4",
            "bne a1, a7, 0x112cc",
            "li s9, 0",
            "lw t5, a0, 80",
            "lw t4, a0, 84",
            "lw t3, a0, 88",
            "lw t2, a0, 92",
            "lw t1, a0, 96",
            "lw t0, a0, 100",
            "lw a7, a0, 104",
            "lw a6, a0, 108",
            "li t6, 256",
            "lui a2, 16",
            "addi s3, a2, 344",
            "mv s2, sp",
            "mv s7, t3",
            "mv s4, t2",
            "mv s6, t1",
            "mv a3, t0",
            "mv s10, a7",
            "mv s5, a6",
            "mv s8, t4",
            "mv s0, t5",
            "mv s11, s10",
            "mv s10, a3",
            "mv a3, s6",
            "mv s1, s7",
            "beq s9, t6, 0x113fa",
            "srliw a4, a3, 6",
            "slliw a1, a3, 26",
            "or a1, a1, a4",
            "srliw a4, a3, 11",
            "slliw a2, a3, 21",
            "or a2, a2, a4",
            "xor a1, a1, a2",
            "srliw a2, a3, 25",
            "slliw a4, a3, 7",
            "or a2, a2, a4",
            "xor s6, a1, a2",
            "and a2, s10, a3",
            "not a4, a3",
            "and a4, s11, a4",
            "add a5, s3, s9",
            "lw a5, a5, 0",
            "add a1, s2, s9",
            "lw a1, a1, 0",
            "addw a2, s6, a2",
            "addw a2, a2, s5",
            "addw a2, a2, a4",
            "addw a2, a2, a5",
            "addw a1, a1, a2",
            "srliw a2, s0, 2",
            "slliw a4, s0, 30",
            "or a2, a2, a4",
            "srliw a4, s0, 13",
            "slliw a5, s0, 19",
            "or a4, a4, a5",
            "xor a2, a2, a4",
            "srliw a4, s0, 22",
            "slli a5, s0, 10",
            "or a4, a4, a5",
            "xor a2, a2, a4",
            "xor a4, s8, s1",
            "and a4, a4, s0",
            "and a5, s8, s1",
            "xor a4, a4, a5",
            "addw a2, a2, a4",
            "addw s6, a1, s4",
            "mv s7, s8",
            "mv s8, s0",
            "addw s0, a2, a1",
            "addi s9, s9, 4",
            "mv s4, s1",
            "mv s5, s11",
            "j 0x11360",
            "addw a1, s0, t5",
            "sw a1, a0, 80",
            "addw a1, s8, t4",
            "sw a1, a0, 84",
            "addw a1, s1, t3",
            "sw a1, a0, 88",
            "addw a1, s4, t2",
            "sw a1, a0, 92",
            "addw a1, a3, t1",
            "sw a1, a0 96",
            "addw a1, s10, t0",
            "sw a1, a0, 100",
            "addw a1, s11, a7",
            "sw a1, a0, 104",
            "addw a1, s5, a6",
            "sw a1, a0, 108",
            "ld s0, sp, 344",
            "ld s1, sp, 336",
            "ld s2, sp, 328",
            "ld s3, sp, 320",
            "ld s4, sp, 312",
            "ld s5, sp, 304",
            "ld s6, sp, 296",
            "ld s7, sp, 288",
            "ld s8, sp, 280",
            "ld s9, sp, 272",
            "ld s10, sp, 26",
            "ld s11, sp, 256",
            "addi sp, sp, 352",
            "ret",
            "lui a1, 18",
            "ld a1, a1, 1640",
            "lui a2, 18",
            "ld a2, a2, 1648",
            "sd a1, a0, 80",
            "lui a1, 18",
            "ld a1, a1, 1656",
            "sd a2, a0, 88",
            "lui a2, 18",
            "ld a2, a2, 1664",
            "sd a1, a0, 96",
            "li a1, 0",
            "sw a1, a0, 64",
            "sd a1, a0, 72",
            "sd a2, a0, 104",
            "ret",
            "addi sp, sp, -48",
            "sd ra, sp, 40",
            "sd s0, sp, 32",
            "sd s1, sp, 24",
            "sd s2, sp, 16",
            "sd s3, sp, 8",
            "sd s4, sp, 0",
            "mv s3, a2",
            "mv s2, a1",
            "mv s1, a0",
            "li s0, 0",
            "li s4, 64",
            "slli a0, s0, 32",
            "srli a0, a0, 32",
            "bgeu a0, s3, 0x114cc",
            "add a0, a0, s2",
            "lwu a1, s1, 64",
            "lb a0, a0, 0",
            "add a1, a1, s1",
            "sb a0, a1, 0",
            "lw a0, s1, 64",
            "addiw a0, a0, 1",
            "sw a0, s1, 64",
            "bne a0, s4, 0x114c8",
            "mv a0, s1",
            "mv a1, s1",
            "jal 0x1126e",
            "ld a0, s1, 72",
            "addi a0, a0, 512",
            "sd a0, s1, 72",
            "sw zero, s1, 64",
            "addiw s0, s0, 1",
            "j 0x11490",
            "ld ra, sp, 40",
            "ld s0, sp, 32",
            "ld s1, sp, 24",
            "ld s2, sp, 16",
            "ld s3, sp, 8",
            "ld s4, sp, 0",
            "addi sp, sp, 48",
            "ret",
            "addi sp, sp, -32",
            "sd ra, sp, 24",
            "sd s0, sp, 16",
            "sd s1, sp, 8",
            "mv s0, a0",
            "lwu a0, a0, 64",
            "mv s1, a1",
            "sext.w a1, a0",
            "add a2, s0, a0",
            "li a3, 128",
            "li a4, 56",
            "sb a3, a2, 0",
            "bgeu a1, a4, 0x115e6",
            "addi a1, s0, 1",
            "li a2, 55",
            "beq a0, a2, 0x1151e",
            "add a3, a1, a0",
            "addi a0, a0, 1",
            "sb zero, a3, 0",
            "bne a0, a2, 0x11510",
            "lw a0, s0, 64",
            "ld a1, s0, 72",
            "slli a0, a0, 35",
            "srli a0, a0, 32",
            "add a0, a0, a1",
            "sd a0, s0, 72",
            "sb a0, s0, 63",
            "srli a1, a0, 8",
            "sb a1, s0, 62",
            "srli a1, a0, 16",
            "sb a1, s0, 61",
            "srli a1, a0, 24",
            "sb a1, s0, 60",
            "srli a1, a0, 32",
            "sb a1, s0, 59",
            "srli a1, a0, 40",
            "sb a1, s0, 58",
            "srli a1, a0, 48",
            "sb a1, s0, 57",
            "srli a0, a0, 56",
            "sb a0, s0, 56",
            "mv a0, s0",
            "mv a1, s0",
            "jal 0x1126e",
            "li a0, 0",
            "addi a1, s1, 16",
            "li a2, 4",
            "li a3, 24",
            "beq a0, a2, 0x115dc",
            "lw a4, s0, 80",
            "slliw a5, a0, 3",
            "subw a5, a3, a5",
            "srlw a4, a4, a5",
            "add s1, a1, a0",
            "sb a4, s1, -16",
            "lw a4, s0, 84",
            "srlw a4, a4, a5",
            "sb a4, s1, -12",
            "lw a4, s0, 88",
            "srlw a4, a4, a5",
            "sb a4, s1, -8",
            "lw a4, s0, 92",
            "srlw a4, a4, a5",
            "sb a4, s1, -4",
            "lw a4, s0, 96",
            "srlw a4, a4, a5",
            "sb a4, s1, 0",
            "lw a4, s0, 100",
            "srlw a4, a4, a5",
            "sb a4, s1, 4",
            "lw a4, s0, 104",
            "srlw a4, a4, a5",
            "sb a4, s1, 8",
            "lw a4, s0, 108",
            "srlw a4, a4, a5",
            "sb a4, s1, 12",
            "addi a0, a0, 1",
            "bne a0, a2, 0x1157a",
            "ld ra, sp, 24",
            "ld s0, sp, 16",
            "ld s1, sp, 8",
            "addi sp, sp, 32",
            "ret",
            "li a1, 63",
            "addiw a0, a0, 1",
            "bltu a1, a0, 0x115fa",
            "add a2, s0, a0",
            "sb zero, a2, 0",
            "j 0x115ea",
            "mv a0, s0",
            "mv a1, s0",
            "jal 0x1126e",
            "li a0, 0",
            "li a1, 56",
            "beq a0, a1, 0x1151e",
            "add a2, s0, a0",
            "sb zero, a2, 0",
            "addi a0, a0, 1",
            "bne a0, a1, 0x1160c",
            "j 0x1151e",
            "addi sp, sp, -128",
            "sd ra, sp, 120",
            "sw zero, sp, 72",
            "sd zero, sp, 80",
            "lui a0, 18",
            "ld a0, a0, 1672",
            "lui a1, 18",
            "ld a1, a1, 1680",
            "lui a2, 18",
            "ld a2, a2, 1688",
            "lui a3, 18",
            "ld a3, a3, 1696",
            "sd a0, sp, 88",
            "sd a1, sp, 96",
            "sd a2, sp, 104",
            "sd a3, sp, 112",
            "addi a0, sp, 8",
            "lui a1, 1",
            "li a2, 1024",
            "jal 0x11476",
            "addi a0, sp, 8",
            "lui a1, 2",
            "jal 0x114dc",
            "ld ra, sp, 120",
            "addi sp, sp, 128",
            "ret"
        ];

        for (test, decoded) in test.iter().zip(decoded) {
            assert_eq!(test, &decoded);
        }

        Ok(())
    }
}
