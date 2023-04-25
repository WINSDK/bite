//! Riscv64gc/Riscv32gc disassembler.

mod tests;

use decoder::encode_hex;
use tokenizing::{ColorScheme, Colors};
use std::borrow::Cow;

#[derive(Debug)]
pub enum Error {
    /// Register in instruction is impossible/unknown.
    UnknownRegister,

    /// Opcode in instruction is impossible/unknown.
    UnknownOpcode,
}

macro_rules! operands {
    [] => {([$crate::EMPTY_OPERAND; 3], 0)};
    [$($x:expr),+ $(,)?] => {{
        let mut operands = [$crate::EMPTY_OPERAND; 3];
        let mut idx = 0;
        $(
            idx += 1;
            operands[idx - 1] = $x;
        )*

        (operands, idx)
    }};
}

pub struct Stream<'data> {
    pub bytes: &'data [u8],
    pub offset: usize,
    pub width: usize,
    pub is_64: bool,
    pub section_base: usize,
}

pub struct Instruction {
    mnemomic: &'static str,
    operands: [std::borrow::Cow<'static, str>; 3],
    operand_count: usize,
}

impl decoder::Decodable for Instruction {
    fn is_call(&self) -> bool {
        self.mnemomic == "call"
    }

    fn is_ret(&self) -> bool {
        self.mnemomic == "ret"
    }

    fn is_jump(&self) -> bool {
        self.mnemomic.starts_with("jal")
    }
}

impl decoder::ToTokens for Instruction {
    fn tokenize(mut self, stream: &mut decoder::TokenStream) {
        stream.push(self.mnemomic, Colors::opcode());

        // there are operands
        if self.operand_count > 0 {
            stream.push(" ", Colors::spacing());

            // iterate through operands
            for idx in 0..self.operand_count {
                let operand = std::mem::take(&mut self.operands[idx]);

                match operand {
                    Cow::Owned(s) => stream.push_owned(s, Colors::immediate()),
                    Cow::Borrowed(s) => stream.push(s, Colors::register()),
                };

                // separator
                if idx != self.operand_count - 1 {
                    stream.push(", ", Colors::expr());
                }
            }
        }
    }
}

impl Stream<'_> {
    fn decode(&mut self, bytes: usize) -> Result<Instruction, Error> {
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
                    0b001 if !self.is_64 => decode_comp_jump("jal", bytes, self),
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
                    0b101 => decode_comp_jump("j", bytes, self),
                    0b110 => decode_comp_branch("beq", bytes, self),
                    0b111 => decode_comp_branch("bne", bytes, self),
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

            return match decoded_inst {
                Ok(mut inst) => {
                    if let Some(map_to_psuedo) = PSUEDOS.get(inst.mnemomic) {
                        map_to_psuedo(&mut inst);
                    }

                    self.width = 2;
                    self.offset += self.width;
                    Ok(inst)
                }
                Err(err) => {
                    self.width = 1;
                    self.offset += self.width;
                    Err(err)
                }
            }
        }

        let decoded_inst = match opcode {
            _ if bytes == 0b000000000000_00000_000_00000_1110011 => decode_unique("ecall"),
            _ if bytes == 0b000000000001_00000_000_00000_1110011 => decode_unique("ebreak"),
            0b0001111 => decode_unique("fence"),
            0b0110111 => decode_double("lui", bytes),
            0b0010111 => decode_double("auipc", bytes),
            0b1101111 => decode_jump(bytes, self),
            0b1100111 => decode_jumpr(bytes, self),
            0b1100011 => match bytes >> 12 & 0b111 {
                0b000 => decode_branch("beq", bytes, self),
                0b001 => decode_branch("bne", bytes, self),
                0b100 => decode_branch("blt", bytes, self),
                0b101 => decode_branch("bge", bytes, self),
                0b110 => decode_branch("bltu", bytes, self),
                0b111 => decode_branch("bgeu", bytes, self),
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
                0b001 => decode_arith("slli", bytes, self),
                0b101 if bytes >> 26 == 0b0000001 => decode_arith("srai", bytes, self),
                0b101 if bytes >> 26 == 0b0000000 => decode_arith("srli", bytes, self),
                _ => Err(Error::UnknownOpcode),
            },
            0b0011011 => match bytes >> 12 & 0b111 {
                _ if !self.is_64 => Err(Error::UnknownOpcode),
                0b000 => decode_immediate("addiw", bytes),
                0b001 => decode_arith("slliw", bytes, self),
                0b101 if bytes >> 25 == 0b0000000 => decode_arith("srliw", bytes, self),
                0b101 if bytes >> 25 == 0b0100000 => decode_arith("sraiw", bytes, self),
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
                _ if !self.is_64 => Err(Error::UnknownOpcode),
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

        match decoded_inst {
            Ok(mut inst) => {
                if let Some(map_to_psuedo) = PSUEDOS.get(inst.mnemomic) {
                    map_to_psuedo(&mut inst);
                }

                self.width = 4;
                self.offset += self.width;
                Ok(inst)
            }
            Err(err) => {
                self.width = 1;
                self.offset += self.width;
                Err(err)
            }
        }
    }
}

impl decoder::Streamable for Stream<'_> {
    type Item = Instruction;
    type Error = Error;

    fn next(&mut self) -> Option<Result<Self::Item, Error>> {
        let bytes = self.bytes.get(self.offset..)?;

        if bytes.len() < 2 {
            return None;
        }

        let bytes = if bytes.len() < 4 {
            u16::from_le_bytes([bytes[0], bytes[1]]) as usize
        } else {
            u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as usize
        };

        Some(self.decode(bytes))
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

        if inst.operands[0] == "zero" || inst.operands[0] == "ra" {
            inst.mnemomic = "call";
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
            return;
        }
    },
    "auipc" => |inst| {
        if inst.operands[0] == "t1" || inst.operands[0] == "ra" {
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
            inst.mnemomic = "call";
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

    // sign extend the 32nd bit and shift it into the 13th bit
    imm |= ((bytes & (1 << 31)) as i32 >> 19) as usize;
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

const EMPTY_OPERAND: std::borrow::Cow<'static, str> = std::borrow::Cow::Borrowed("");
