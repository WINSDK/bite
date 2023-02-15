//! Riscv64gc/Riscv32gc disassembler.

use super::{DecodableInstruction, Error, encode_hex};
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

pub(super) struct Stream<'data> {
    pub bytes: &'data [u8],
    pub offset: usize,
    pub width: usize,
    pub is_64: bool,
    pub section_base: usize,
}

pub(super) struct Instruction {
    mnemomic: &'static str,
    operands: [std::borrow::Cow<'static, str>; 3],
    operand_count: usize,
}

impl DecodableInstruction for Instruction {
    fn psuedo_decode(&mut self) -> String {
        if let Some(map_to_psuedo) = PSUEDOS.get(self.mnemomic) {
            map_to_psuedo(self);
        }

        self.decode()
    }

    fn decode(&self) -> String {
        let mut repr = self.mnemomic.to_string();
        let operands = self.operands();

        if operands.is_empty() {
            return repr;
        }

        repr += " ";

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
        let bytes = match self.bytes.get(self.offset..) {
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

        // the instruction is compressed
        if bytes as u16 & 0b11 != 0b11 {
            let bytes = bytes as u16 as usize;
            let opcode = bytes & 0b11;
            let jump3 = bytes >> 13 & 0b111;

            let decoded_inst = match opcode {
                0b00 => match jump3 {
                    0b000 => decode_addi4spn(bytes),
                    0b001 => decode_comp_fsld("fld", bytes),
                    0b010 => decode_comp_slw("lw", bytes),
                    0b011 if !self.is_64 => decode_comp_fslw("flw", bytes),
                    0b011 if self.is_64 => decode_comp_sld("ld", bytes),
                    0b101 => decode_comp_fsld("fsd", bytes),
                    0b110 => decode_comp_slw("sw", bytes),
                    0b111 if !self.is_64 => decode_comp_fslw("fsw", bytes),
                    0b111 if self.is_64 => decode_comp_sld("sd", bytes),
                    _ => Err(Error::UnknownOpcode),
                },
                0b01 => match jump3 {
                    0b000 if bytes >> 7 & 0b11111 == 0 => decode_comp_unique("nop"),
                    0b000 if bytes >> 7 & 0b11111 != 0 => decode_comp_addi("addi", bytes),
                    0b001 if !self.is_64 => decode_comp_jump("jal", bytes, &self),
                    0b001 if self.is_64 => decode_comp_addi("addiw", bytes),
                    0b010 => decode_comp_li("li", bytes),
                    0b011 if bytes >> 7 & 0b11111 == 2 => decode_addi16sp(bytes),
                    0b011 if bytes >> 7 & 0b11111 != 2 => decode_comp_li("lui", bytes),
                    0b100 => match bytes >> 10 & 0b11 {
                        0b00 => decode_comp_shift("srli", bytes),
                        0b01 => decode_comp_shift("srai", bytes),
                        0b11 => match (bytes >> 5 & 0b11, bytes >> 12 & 0b1) {
                            (0b00, 0b0) if !self.is_64 => decode_comp_arith("sub", bytes),
                            (0b01, 0b0) => decode_comp_arith("xor", bytes),
                            (0b10, 0b0) => decode_comp_arith("or", bytes),
                            (0b11, 0b0) => decode_comp_arith("and", bytes),
                            (0b00, 0b1) if self.is_64 => decode_comp_arith("subw", bytes),
                            (0b01, 0b1) if self.is_64 => decode_comp_arith("addw", bytes),
                            _ => Err(Error::UnknownOpcode),
                        },
                        _ => Err(Error::UnknownOpcode),
                    },
                    0b101 => decode_comp_jump("j", bytes, &self),
                    0b110 => decode_comp_branch("beq", bytes, &self),
                    0b111 => decode_comp_branch("bne", bytes, &self),
                    _ => Err(Error::UnknownOpcode),
                },
                0b10 => match jump3 {
                    0b000 => decode_comp_shift("slli", bytes),
                    0b001 => decode_comp_ldsp("fldsp", bytes),
                    0b010 => decode_comp_lwsp("lwsp", bytes),
                    0b011 if !self.is_64 => decode_comp_lwsp("flwsp", bytes),
                    0b011 if self.is_64 => decode_comp_ldsp("ldsp", bytes),
                    0b100 => match (bytes >> 12 & 0b1, bytes >> 2 & 0b11111) {
                        (0b0, 0b0) => decode_comp_jumpr(bytes),
                        (0b0, _) => decode_comp_mv(bytes),
                        (0b1, 0b0) => decode_comp_unique("ebreak"),
                        (0b1, _) => decode_comp_add(bytes),
                        _ => Err(Error::UnknownOpcode),
                    },
                    0b101 => decode_comp_sdsp("fsdsp", bytes),
                    0b110 => decode_comp_swsp("swsp", bytes),
                    0b111 if !self.is_64 => decode_comp_swsp("fswsp", bytes),
                    0b111 if self.is_64 => decode_comp_sdsp("sdsp", bytes),
                    _ => Err(Error::UnknownOpcode),
                },
                _ => Err(Error::UnknownOpcode),
            };

            self.width = 2;
            self.offset += 2;

            return decoded_inst;
        }

        let decoded_inst = match opcode {
            _ if bytes == 0b000000000000_00000_000_00000_1110011 => decode_unique("ecall"),
            _ if bytes == 0b000000000001_00000_000_00000_1110011 => decode_unique("ebreak"),
            0b0001111 => decode_unique("fence"),
            0b0110111 => decode_double("lui", bytes),
            0b0010111 => decode_double("auipc", bytes),
            0b1101111 => decode_jump(bytes, &self),
            0b1100111 => decode_jumpr(bytes, &self),
            0b1100011 => match bytes >> 12 & 0b111 {
                0b000 => decode_branch("beq", bytes, &self),
                0b001 => decode_branch("bne", bytes, &self),
                0b100 => decode_branch("blt", bytes, &self),
                0b101 => decode_branch("bge", bytes, &self),
                0b110 => decode_branch("bltu", bytes, &self),
                0b111 => decode_branch("bgeu", bytes, &self),
                _ => Err(Error::UnknownOpcode),
            },
            0b0000011 => match bytes >> 12 & 0b111 {
                0b000 => decode_immediate("lb", bytes),
                0b001 => decode_immediate("lh", bytes),
                0b010 => decode_immediate("lw", bytes),
                0b011 if self.is_64 => decode_immediate("ld", bytes),
                0b100 => decode_immediate("lbu", bytes),
                0b101 => decode_immediate("lhu", bytes),
                0b110 if self.is_64 => decode_immediate("lwu", bytes),
                _ => Err(Error::UnknownOpcode),
            },
            0b0100011 => match bytes >> 12 & 0b111 {
                0b000 => decode_store("sb", bytes),
                0b001 => decode_store("sh", bytes),
                0b010 => decode_store("sw", bytes),
                0b011 if self.is_64 => decode_store("sd", bytes),
                _ => Err(Error::UnknownOpcode),
            },
            0b0010011 => match bytes >> 12 & 0b111 {
                0b000 => decode_immediate("addi", bytes),
                0b010 => decode_immediate("alti", bytes),
                0b011 => decode_immediate("altiu", bytes),
                0b100 => decode_immediate("xori", bytes),
                0b110 => decode_immediate("ori", bytes),
                0b111 => decode_immediate("andi", bytes),
                0b001 => decode_arith("slli", bytes, &self),
                0b101 if bytes >> 26 == 0b0000001 => decode_arith("srai", bytes, &self),
                0b101 if bytes >> 26 == 0b0000000 => decode_arith("srli", bytes, &self),
                _ => Err(Error::UnknownOpcode),
            },
            0b0011011 => match bytes >> 12 & 0b111 {
                _ if !self.is_64 => Err(Error::UnknownOpcode),
                0b000 => decode_immediate("addiw", bytes),
                0b001 => decode_arith("slliw", bytes, &self),
                0b101 if bytes >> 25 == 0b0000000 => decode_arith("srliw", bytes, &self),
                0b101 if bytes >> 25 == 0b0100000 => decode_arith("sraiw", bytes, &self),
                _ => Err(Error::UnknownOpcode),
            },
            0b0110011 => match bytes >> 25 {
                0b0000000 => match bytes >> 12 & 0b111 {
                    0b000 => decode_triplet("add", bytes),
                    0b001 => decode_triplet("sll", bytes),
                    0b010 => decode_triplet("slt", bytes),
                    0b011 => decode_triplet("sltu", bytes),
                    0b100 => decode_triplet("xor", bytes),
                    0b101 => decode_triplet("srl", bytes),
                    0b110 => decode_triplet("or", bytes),
                    0b111 => decode_triplet("and", bytes),
                    _ => Err(Error::UnknownOpcode),
                },
                0b0100000 => match bytes >> 12 & 0b111 {
                    0b000 => decode_triplet("sub", bytes),
                    0b101 => decode_triplet("sra", bytes),
                    _ => Err(Error::UnknownOpcode),
                },
                _ => Err(Error::UnknownOpcode),
            },
            0b0111011 => match bytes >> 25 {
                _ if !self.is_64 => return Err(Error::UnknownOpcode),
                0b0000000 => match bytes >> 12 & 0b111 {
                    0b000 => decode_triplet("addw", bytes),
                    0b001 => decode_triplet("sllw", bytes),
                    0b101 => decode_triplet("srlw", bytes),
                    _ => Err(Error::UnknownOpcode),
                },
                0b0100000 => match bytes >> 12 & 0b111 {
                    0b000 => decode_triplet("subw", bytes),
                    0b101 => decode_triplet("sraw", bytes),
                    _ => Err(Error::UnknownOpcode),
                },
                _ => Err(Error::UnknownOpcode),
            },
            _ => Err(Error::UnknownOpcode),
        };

        self.width = 4;
        self.offset += 4;

        decoded_inst
    }

    fn format(&self, next: Result<Self::Item, Error>) -> Option<String> {
        match next {
            Err(Error::NoBytesLeft) => None,
            Err(err) => {
                let mut fmt = String::new();

                let bytes = &self.bytes[self.offset - self.width..][..self.width];
                let bytes: Vec<String> = bytes.iter().map(|byte| format!("{:02x}", byte)).collect();
                let bytes = bytes.join(" ");

                fmt += &format!("\t{bytes:11}  <{err:?}>");
                Some(fmt)
            }
            Ok(mut inst) => {
                let mut fmt = String::new();

                let bytes = &self.bytes[self.offset - self.width..][..self.width];
                let bytes: Vec<String> = bytes.iter().map(|byte| format!("{:02x}", byte)).collect();
                let bytes = bytes.join(" ");

                fmt += &format!("\t{bytes:11}  {}", inst.psuedo_decode());
                Some(fmt)
            }
        }
    }
}

// NOTE: registers starting with f have to be floating-point whilst all other are integers
pub const ABI_REGISTERS: [&str; 63] = [
    "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4",
    "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4",
    "t5", "t6", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12", "f13",
    "f14", "f15", "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23", "f24", "f25", "f26",
    "f27", "f28", "f29", "f30", "f31",
];

pub const INT_REGISTERS: [&str; 8] = ["s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5"];
pub const FP_REGISTERS: [&str; 8] = ["fs0", "fs1", "fa0", "fa1", "fa2", "fa3", "fa4", "fa5"];

static PSUEDOS: phf::Map<&str, fn(&mut Instruction)> = phf::phf_map! {
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

        if inst.operands[1] == "zero" {
            inst.mnemomic = "li";
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
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
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
        }
    },
    "auipc" => |inst| {
        if inst.operands[0] == "t2" {
            // todo!();
        }
    }
    // TODO: table p2
};

/// Decode's beqz and bnez instructions.
fn decode_comp_branch(
    mnemomic: &'static str,
    bytes: usize,
    stream: &Stream,
) -> Result<Instruction, Error> {
    let rs = INT_REGISTERS
        .get(bytes >> 7 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let mut imm = 0;

    imm |= bytes >> 4 & 0b100000000;
    imm |= bytes << 1 & 0b011000000;
    imm |= bytes << 3 & 0b000100000;
    imm |= bytes >> 7 & 0b000011000;
    imm |= bytes >> 2 & 0b000000110;

    // cast to i32 to prevent rust overflowing literal complaints
    let mut imm = imm as i64;

    if imm & 0b100000000 != 0 {
        imm |= (imm | 0b1111111000000000) as i16 as i64;
    }

    imm += (stream.section_base + stream.offset) as i64;

    let (operands, operand_count) = operands![
        Cow::Borrowed(rs),
        Cow::Borrowed(rs),
        Cow::Owned(encode_hex(imm))
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's beqz and bnez instructions.
fn decode_comp_jump(
    mnemomic: &'static str,
    bytes: usize,
    stream: &Stream,
) -> Result<Instruction, Error> {
    let mut imm = 0;

    imm |= bytes >> 1 & 0b100000000000;
    imm |= bytes << 2 & 0b010000000000;
    imm |= bytes >> 1 & 0b001100000000;
    imm |= bytes << 1 & 0b000010000000;
    imm |= bytes >> 1 & 0b000001000000;
    imm |= bytes << 3 & 0b000000100000;
    imm |= bytes >> 7 & 0b000000010000;
    imm |= bytes >> 2 & 0b000000001110;

    // cast to i32 to prevent rust overflowing literal complaints
    let mut imm = imm as i64;

    if imm & 0b100000000000 != 0 {
        imm |= (imm | 0b1111000000000000) as i16 as i64;
    }

    imm += (stream.section_base + stream.offset) as i64;

    let (operands, operand_count) = operands![Cow::Owned(encode_hex(imm))];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's j and jal instructions.
fn decode_comp_jumpr(bytes: usize) -> Result<Instruction, Error> {
    let rs = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    let (operands, operand_count) =
        operands![Cow::Borrowed("ra"), Cow::Borrowed(rs), Cow::Borrowed("0")];

    Ok(Instruction {
        mnemomic: "jalr",
        operands,
        operand_count,
    })
}

/// Decode's sub, or, xor, and, subw and addw instructions.
fn decode_comp_arith(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rd = INT_REGISTERS
        .get(bytes >> 7 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let rs = INT_REGISTERS
        .get(bytes >> 2 & 0b111)
        .ok_or(Error::UnknownRegister)?;

    let (operands, operand_count) =
        operands![Cow::Borrowed(rd), Cow::Borrowed(rd), Cow::Borrowed(rs)];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's srli, srai and slli instructions.
fn decode_comp_shift(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rd = INT_REGISTERS
        .get(bytes >> 7 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let shamt = (bytes >> 7 & 0b100000) | (bytes >> 2 & 0b11111);

    let (operands, operand_count) = operands![
        Cow::Borrowed(rd),
        Cow::Borrowed(rd),
        Cow::Owned(shamt.to_string()),
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's addi and addiw instructions.
fn decode_comp_addi(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let mut imm = (((bytes >> 7) & 0b100000) | ((bytes >> 2) & 0b11111)) as i16;

    if imm & 0b100000 != 0 {
        imm = (imm | 0b11000000) as i8 as i16;
    }

    let (operands, operand_count) = operands![
        Cow::Borrowed(rd),
        Cow::Borrowed(rd),
        Cow::Owned(imm.to_string()),
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's addi16sp instruction also represented as addi sp, sp, imm*16.
fn decode_addi16sp(bytes: usize) -> Result<Instruction, Error> {
    let mut imm = 0;

    imm |= bytes >> 3 & 0b1000000000;
    imm |= bytes >> 2 & 0b0000010000;
    imm |= bytes << 1 & 0b0001000000;
    imm |= bytes << 4 & 0b0110000000;
    imm |= bytes << 3 & 0b0000100000;

    let mut imm = imm as i32;

    if imm & 0b1000000000 != 0 {
        imm = (imm | 0b1111110000000000) as i16 as i32;
    }

    let (operands, operand_count) = operands![Cow::Owned(imm.to_string())];

    Ok(Instruction {
        mnemomic: "addi16sp",
        operands,
        operand_count,
    })
}

/// Decode's addi14spn instruction also represented as addi sp, rs, imm*4.
fn decode_addi4spn(bytes: usize) -> Result<Instruction, Error> {
    let rd = INT_REGISTERS
        .get(bytes >> 2 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let mut imm = 0;

    imm |= bytes >> 1 & 0b1111000000;
    imm |= bytes >> 7 & 0b0000110000;
    imm |= bytes >> 2 & 0b0000001000;
    imm |= bytes >> 4 & 0b0000000100;

    let (operands, operand_count) = operands![Cow::Borrowed(rd), Cow::Owned(imm.to_string())];

    Ok(Instruction {
        mnemomic: "addi4spn",
        operands,
        operand_count,
    })
}

/// Decode's add instruction.
fn decode_comp_add(bytes: usize) -> Result<Instruction, Error> {
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let rs = ABI_REGISTERS
        .get(bytes >> 2 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    let (operands, operand_count) =
        operands![Cow::Borrowed(rd), Cow::Borrowed(rd), Cow::Borrowed(rs)];

    Ok(Instruction {
        mnemomic: "add",
        operands,
        operand_count,
    })
}

/// Decode's li and lui instructions.
fn decode_comp_li(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let mut imm = (((bytes >> 7) & 0b100000) | ((bytes >> 2) & 0b11111)) as i16;
    let rs = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    if imm & 0b100000 != 0 {
        imm = (imm | 0b11000000) as i8 as i16;
    }

    let (operands, operand_count) = operands![Cow::Borrowed(rs), Cow::Owned(imm.to_string())];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's store word relative to sp instruction for both integers and floats.
fn decode_comp_swsp(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rd = ABI_REGISTERS
        .get(bytes >> 2 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let imm = (bytes >> 1 & 0b11000000) | (bytes >> 7 & 0b111100);

    let (operands, operand_count) = operands![Cow::Borrowed(rd), Cow::Owned(imm.to_string())];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's store double relative to sp instruction for both integers and floats.
fn decode_comp_sdsp(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rd = ABI_REGISTERS
        .get(bytes >> 2 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let imm = (bytes >> 1 & 0b111000000) | (bytes >> 7 & 0b111000);

    let (operands, operand_count) = operands![Cow::Borrowed(rd), Cow::Owned(imm.to_string())];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's load word relative to sp instruction for both integers and floats.
fn decode_comp_lwsp(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let imm = (bytes << 4 & 0b11000000) | (bytes >> 7 & 0b100000) | (bytes >> 2 & 0b11100);

    let (operands, operand_count) = operands![Cow::Borrowed(rd), Cow::Owned(imm.to_string())];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's load double relative to sp instruction for both integers and floats.
fn decode_comp_ldsp(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let imm = (bytes << 4 & 0b111000000) | (bytes >> 7 & 0b100000) | (bytes >> 2 & 0b11000);

    let (operands, operand_count) = operands![Cow::Borrowed(rd), Cow::Owned(imm.to_string())];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's load word instruction for integers.
fn decode_comp_slw(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rs1 = INT_REGISTERS
        .get(bytes >> 2 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let rs2 = INT_REGISTERS
        .get(bytes >> 7 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let imm = (bytes << 1 & 0b1000000) | (bytes >> 7 & 0b111000) | (bytes >> 4 & 0b100);

    let (operands, operand_count) = operands![
        Cow::Borrowed(rs1),
        Cow::Borrowed(rs2),
        Cow::Owned(imm.to_string())
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's load double instruction for integers.
fn decode_comp_sld(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rs1 = INT_REGISTERS
        .get(bytes >> 2 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let rs2 = INT_REGISTERS
        .get(bytes >> 7 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let imm = (bytes << 1 & 0b11000000) | (bytes >> 7 & 0b111000);

    let (operands, operand_count) = operands![
        Cow::Borrowed(rs1),
        Cow::Borrowed(rs2),
        Cow::Owned(imm.to_string())
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's load word instruction for floats.
fn decode_comp_fslw(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rs1 = FP_REGISTERS
        .get(bytes >> 2 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let rs2 = FP_REGISTERS
        .get(bytes >> 7 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let imm = (bytes << 1 & 0b1000000) | (bytes >> 7 & 0b111000) | (bytes >> 4 & 0b100);

    let (operands, operand_count) = operands![
        Cow::Borrowed(rs1),
        Cow::Borrowed(rs2),
        Cow::Owned(imm.to_string())
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's load double instruction for floats.
fn decode_comp_fsld(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rs1 = FP_REGISTERS
        .get(bytes >> 2 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let rs2 = FP_REGISTERS
        .get(bytes >> 7 & 0b111)
        .ok_or(Error::UnknownRegister)?;
    let imm = (bytes << 1 & 0b11000000) | (bytes >> 7 & 0b111000);

    let (operands, operand_count) = operands![
        Cow::Borrowed(rs1),
        Cow::Borrowed(rs2),
        Cow::Owned(imm.to_string())
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's move instruction.
fn decode_comp_mv(bytes: usize) -> Result<Instruction, Error> {
    let rs = ABI_REGISTERS
        .get(bytes >> 2 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    let (operands, operand_count) = operands![Cow::Borrowed(rd), Cow::Borrowed(rs)];

    Ok(Instruction {
        mnemomic: "mv",
        operands,
        operand_count,
    })
}

/// Decode's instructions with weird formatting that aren't yet handled.
fn decode_comp_unique(mnemomic: &'static str) -> Result<Instruction, Error> {
    let (operands, operand_count) = operands![];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's instructions with weird formatting that aren't yet handled.
fn decode_unique(mnemomic: &'static str) -> Result<Instruction, Error> {
    let (operands, operand_count) = operands![];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's sb, sh, sw and sd store instructions.
fn decode_store(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let mut imm = 0;

    imm |= ((bytes & 0b11111110000000000000000000000000) as i32 >> 20) as usize;
    imm |= bytes >> 7 & 0b11111;

    let imm = imm as i32;

    let rs1 = ABI_REGISTERS
        .get(bytes >> 15 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let rs2 = ABI_REGISTERS
        .get(bytes >> 20 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    let (operands, operand_count) = operands![
        Cow::Borrowed(rs2),
        Cow::Borrowed(rs1),
        Cow::Owned(imm.to_string()),
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's beq, bne, blt, bge, bltu and bgeu branch instructions.
fn decode_branch(
    mnemomic: &'static str,
    bytes: usize,
    stream: &Stream,
) -> Result<Instruction, Error> {
    let rs1 = ABI_REGISTERS
        .get(bytes >> 15 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let rs2 = ABI_REGISTERS
        .get(bytes >> 20 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    let mut imm = 0;

    // FIXME: this can be done better
    imm |= ((bytes & 0b10000000000000000000000000000000) as i32 >> 19) as usize;
    imm |= bytes << 4 & 0b100000000000;
    imm |= bytes >> 20 & 0b011111100000;
    imm |= bytes >> 7 & 0b000000011110;

    let imm = imm as i64 + (stream.section_base + stream.offset) as i64;

    let (operands, operand_count) = operands![
        Cow::Borrowed(rs1),
        Cow::Borrowed(rs2),
        Cow::Owned(encode_hex(imm)),
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's jump instruction.
fn decode_jump(bytes: usize, stream: &Stream) -> Result<Instruction, Error> {
    let mut imm = 0;
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    // 18 bits (riscv instruction jumps are 16-byte alligned)
    imm |= ((bytes & 0b10000000000000000000000000000000) as i32 >> 11) as usize;
    imm |= bytes & 0b11111111000000000000;
    imm |= bytes >> 9 & 0b00000000100000000000;
    imm |= bytes >> 20 & 0b00000000011111111110;

    let imm = imm as i64 + (stream.section_base + stream.offset) as i64;

    let (operands, operand_count) = operands![Cow::Borrowed(rd), Cow::Owned(encode_hex(imm))];

    Ok(Instruction {
        mnemomic: "jal",
        operands,
        operand_count,
    })
}

/// Decode's ret instruction.
fn decode_jumpr(bytes: usize, stream: &Stream) -> Result<Instruction, Error> {
    let mut imm = (bytes as i32 >> 20) as i64;
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    imm += (stream.section_base + stream.offset) as i64;

    let (operands, operand_count) = operands![Cow::Borrowed(rd), Cow::Owned(encode_hex(imm))];

    Ok(Instruction {
        mnemomic: "jalr",
        operands,
        operand_count,
    })
}

/// Decode's instructions that have two registers and an immediate.
fn decode_immediate(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let rs = ABI_REGISTERS
        .get(bytes >> 15 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let imm = (bytes as i32) >> 20;

    let (operands, operand_count) = operands![
        Cow::Borrowed(rd),
        Cow::Borrowed(rs),
        Cow::Owned(imm.to_string()),
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's slli, srai, srli, slliw, sraiw and srliw  instruction's.
fn decode_arith(
    mnemomic: &'static str,
    bytes: usize,
    stream: &Stream,
) -> Result<Instruction, Error> {
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let rs = ABI_REGISTERS
        .get(bytes >> 15 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    let shamt = if stream.is_64 {
        bytes >> 20 & 0b111111
    } else {
        bytes >> 20 & 0b11111
    };

    let (operands, operand_count) = operands![
        Cow::Borrowed(rd),
        Cow::Borrowed(rs),
        Cow::Owned(shamt.to_string()),
    ];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's instructions that have three registers.
fn decode_triplet(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let rs1 = ABI_REGISTERS
        .get(bytes >> 15 & 0b11111)
        .ok_or(Error::UnknownRegister)?;
    let rs2 = ABI_REGISTERS
        .get(bytes >> 20 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    let (operands, operand_count) =
        operands![Cow::Borrowed(rd), Cow::Borrowed(rs1), Cow::Borrowed(rs2)];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

/// Decode's instructions that have have a registers and an immediate.
fn decode_double(mnemomic: &'static str, bytes: usize) -> Result<Instruction, Error> {
    let imm = bytes >> 12;
    let rd = ABI_REGISTERS
        .get(bytes >> 7 & 0b11111)
        .ok_or(Error::UnknownRegister)?;

    let (operands, operand_count) = operands![Cow::Borrowed(rd), Cow::Owned(imm.to_string())];

    Ok(Instruction {
        mnemomic,
        operands,
        operand_count,
    })
}

#[cfg(test)]
#[rustfmt::skip]
mod tests {
    use object::{Object, ObjectSection, SectionKind};

    use std::io::Write;
    use std::process::Stdio;

    use crate::disassembler::{DecodableInstruction, Streamable};

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
            let section = binary
                .sections()
                .filter(|s| s.kind() == SectionKind::Text)
                .find(|t| t.name() == Ok(".text"))
                .expect("failed to find `.text` section");

            let binary = section.uncompressed_data()?;
            let mut decoded = Vec::new();
            let mut stream = $crate::disassembler::riscv::Stream {
                bytes: &*binary,
                offset: 0,
                width: 4,
                is_64: true,
                section_base: section.address() as usize,
            };

            loop {
                match stream.next() {
                    Ok(ref mut inst) => decoded.push(inst.psuedo_decode()),
                    Err($crate::disassembler::Error::NoBytesLeft) => break,
                    Err(..) => decoded.push("????".to_string()),
                }
            }

            decoded
        }};
    }

    #[test]
    fn deref() -> Result<(), Box<dyn std::error::Error>> {
        let decoded = decode_instructions!(r#"
            int _start() {
                *(int *)0x1000000 = 12;

                return 0;
            }
       "#);

        let test = [
            "lui a0, 4096",
            "li a1, 12",
            "sw a1, a0, 0",
            "li a0, 0",
            "ret",
        ];

        for (test, decoded) in test.iter().zip(decoded) {
            if *test != decoded {
                eprintln!("objdump: '{test}' != our: '{decoded}'");
                panic!("instructions don't match");
            }
        }

        Ok(())
    }

    #[test]
    fn sha256() -> Result<(), Box<dyn std::error::Error>> {
        let decoded = decode_instructions!(r#"
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
       "#);

        let test = [
            "li a3, 0",
            "beq a2, a3, 0x1126c",
            "add a4, a0, a3",
            "sb a1, a4, 0",
            "addi a3, a3, 1",
            "bne a2, a3, 0x1125e",
            "ret",
            "addi16sp -352",
            "sdsp s0, 344",
            "sdsp s1, 336",
            "sdsp s2, 328",
            "sdsp s3, 320",
            "sdsp s4, 312",
            "sdsp s5, 304",
            "sdsp s6, 296",
            "sdsp s7, 288",
            "sdsp s8, 280",
            "sdsp s9, 272",
            "sdsp s10, 264",
            "sdsp s11, 256",
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
            "sw a1, a0, 96",
            "addw a1, s10, t0",
            "sw a1, a0, 100",
            "addw a1, s11, a7",
            "sw a1, a0, 104",
            "addw a1, s5, a6",
            "sw a1, a0, 108",
            "ldsp s0, 344",
            "ldsp s1, 336",
            "ldsp s2, 328",
            "ldsp s3, 320",
            "ldsp s4, 312",
            "ldsp s5, 304",
            "ldsp s6, 296",
            "ldsp s7, 288",
            "ldsp s8, 280",
            "ldsp s9, 272",
            "ldsp s10, 264",
            "ldsp s11, 256",
            "addi16sp 352",
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
            "addi16sp -48",
            "sdsp ra, 40",
            "sdsp s0, 32",
            "sdsp s1, 24",
            "sdsp s2, 16",
            "sdsp s3, 8",
            "sdsp s4, 0",
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
            "ldsp ra, 40",
            "ldsp s0, 32",
            "ldsp s1, 24",
            "ldsp s2, 16",
            "ldsp s3, 8",
            "ldsp s4, 0",
            "addi16sp 48",
            "ret",
            "addi sp, sp, -32",
            "sdsp ra, 24",
            "sdsp s0, 16",
            "sdsp s1, 8",
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
            "ldsp ra, 24",
            "ldsp s0, 16",
            "ldsp s1, 8",
            "addi16sp 32",
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
            "addi16sp -128",
            "sdsp ra, 120",
            "swsp zero, 72",
            "sdsp zero, 80",
            "lui a0, 18",
            "ld a0, a0, 1672",
            "lui a1, 18",
            "ld a1, a1, 1680",
            "lui a2, 18",
            "ld a2, a2, 1688",
            "lui a3, 18",
            "ld a3, a3, 1696",
            "sdsp a0, 88",
            "sdsp a1, 96",
            "sdsp a2, 104",
            "sdsp a3, 112",
            "addi4spn a0, 8",
            "lui a1, 1",
            "li a2, 1024",
            "jal 0x11476",
            "addi4spn a0, 8",
            "lui a1, 2",
            "jal 0x114dc",
            "ldsp ra, 120",
            "addi16sp 128",
            "ret",
        ];

        for (test, decoded) in test.iter().zip(decoded) {
            if *test != decoded {
                eprintln!("objdump: '{test}' != our: '{decoded}'");
                panic!("instructions don't match");
            }
        }

        Ok(())
    }
}
