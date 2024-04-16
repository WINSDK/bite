//! Riscv64gc/Riscv32gc disassembler.

mod tests;

use decoder::{Error, ErrorKind, ToTokens};
use debugvault::Index;
use once_cell::sync::Lazy;
use tokenizing::{TokenStream, ColorScheme, Colors};

macro_rules! operands {
    [] => {([$crate::Operand::Nothing; 3], 0)};
    [$($x:expr),+ $(,)?] => {{
        let mut operands = [$crate::Operand::Nothing; 3];
        let mut idx = 0;
        $(
            idx += 1;
            operands[idx - 1] = $x;
        )*

        (operands, idx)
    }};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
#[rustfmt::skip]
pub enum Register {
    Zero, Ra, Sp, Gp, Tp,
    T0, T1, T2,
    S0, S1,
    A0, A1, A2, A3, A4, A5, A6, A7,
    S2, S3, S4, S5, S6, S7, S8, S9, S10, S11,
    T3, T4, T5, T6,
    Ft0, Ft1, Ft2, Ft3, Ft4, Ft5, Ft6, Ft7,
    Fs0, Fs1,
    Fa0, Fa1, Fa2, Fa3, Fa4, Fa5, Fa6, Fa7,
    Fs2, Fs3, Fs4, Fs5, Fs6, Fs7, Fs8, Fs9, Fs10, Fs11,
    Ft8, Ft9, Ft10, Ft11
}

impl Register {
    pub fn as_str(&self) -> &'static str {
        #[rustfmt::skip]
        const REG_LITERALS: [&str; 64] = [
            "zero", "ra", "sp", "gp", "tp",
            "t0", "t1", "t2",
            "s0", "s1",
            "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
            "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
            "t3", "t4", "t5", "t6",
            "ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7",
            "fs0", "fs1",
            "fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7",
            "fs2", "fs3", "fs4", "fs5", "fs6", "fs7", "fs8", "fs9", "fs10", "fs11",
            "ft8", "ft9", "ft10", "ft11"
        ];

        REG_LITERALS[*self as usize]
    }
}

impl Register {
    #[inline]
    fn get(num: u32) -> Result<Self, ErrorKind> {
        // if the num isn't one of the 64 variants, fail
        if num >= 64 {
            return Err(ErrorKind::InvalidRegister);
        }

        Ok(unsafe { std::mem::transmute(num) })
    }

    #[inline]
    fn get_int(num: u16) -> Result<Self, ErrorKind> {
        // if the num isn't between $s0 and $a5
        if num >= 8 {
            return Err(ErrorKind::InvalidRegister);
        }

        Ok(unsafe { std::mem::transmute(num as u32 + 8) })
    }

    #[inline]
    fn get_fp(num: u16) -> Result<Self, ErrorKind> {
        // if the num isn't between $fs0 and Ffa5
        if num >= 8 {
            return Err(ErrorKind::InvalidRegister);
        }

        Ok(unsafe { std::mem::transmute(num as u32 + 40) })
    }
}

/// Opcodes for risc-v 32-bit and 64-bit instructions.
///
/// *There is no support for 128-bit instruction encoding.*
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u32)]
pub enum Opcode {
    #[default]
    Invalid,
    // *psuedo instructions*
    LA,
    LLA,
    NOP,
    LI,
    MV,
    NOT,
    NEG,
    NEGW,
    SEXT_W,
    SEQZ,
    SNEZ,
    SLTZ,
    SGTZ,
    FMV_S,
    FABS_S,
    FNEG_S,
    FMV_D,
    FABS_D,
    FNEG_D,
    BEQZ,
    BNEZ,
    BLEZ,
    BGEZ,
    BLTZ,
    BGTZ,
    BGT,
    BLE,
    BGTU,
    BLEU,
    J,
    JR,
    RET,
    CALL,
    TAIL,
    RDINSTRET,
    RDCYCLE,
    RDTIME,
    CSRR,
    CSRW,
    CSRS,
    CSRC,
    CSRWI,
    CSRSI,
    CSRCI,
    FRCSR,
    FSCSR,
    FRRM,
    FSRM,
    FRFLAGS,
    FSFLAGS,
    // *rv32i instructions*
    LUI,
    AUIPC,
    JAL,
    JALR,
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,
    LB,
    LH,
    LW,
    LBU,
    LHU,
    SB,
    SH,
    SW,
    ADDI,
    SLTI,
    SLTIU,
    XORI,
    ORI,
    ANDI,
    SLLI,
    SRLI,
    SRAI,
    ADD,
    SUB,
    SLL,
    SLT,
    SLTU,
    XOR,
    SRL,
    SRA,
    OR,
    AND,
    FENCE,
    ECALL,
    EBREAK,
    // *rv64i instructions*
    LWU,
    LD,
    SD,
    // slli, srli and srai would be here however whilst they have
    // the same opcode, their encoding is different
    ADDIW,
    SLLIW,
    SRLIW,
    SRAIW,
    ADDW,
    SUBW,
    SLLW,
    SRLW,
    SRAW,
    // *rv32/rv64 zifencei instructions*
    FENCE_I,
    // *rv32/rv64 zicsr instructions*
    CSRRW,
    CSRRS,
    CSRRC,
    CSRRWI,
    CSRRSI,
    CSRRCI,
    // *rv32m instructions*
    MUL,
    MULH,
    MULHSU,
    MULHU,
    DIV,
    DIVU,
    REM,
    REMU,
    // *rv64m instructions*
    MULW,
    DIVW,
    DIVUW,
    REMW,
    REMUW,
    // *rv32a instructions*
    LR_W,
    SC_W,
    AMOSWAP_W,
    AMOADD_W,
    AMOXOR_W,
    AMOAND_W,
    AMOOR_W,
    AMOMIN_W,
    AMOMAX_W,
    AMOMINU_W,
    AMOMAXU_W,
    // *rv64a instructions*
    LR_D,
    SC_D,
    AMOSWAP_D,
    AMOADD_D,
    AMOXOR_D,
    AMOAND_D,
    AMOOR_D,
    AMOMIN_D,
    AMOMAX_D,
    AMOMINU_D,
    AMOMAXU_D,
    // *rv32f instructions*
    FLW,
    FSW,
    FMADD_S,
    FMSUB_S,
    FNMSUB_S,
    FNMADD_S,
    FADD_S,
    FSUB_S,
    FMUL_S,
    FDIV_S,
    FSQRT_S,
    FSGNJ_S,
    FSGNJN_S,
    FSGNJX_S,
    FMIN_S,
    FMAX_S,
    FCVT_W_S,
    FCVT_WU_S,
    FMV_X_W,
    FEQ_S,
    FLT_S,
    FLE_S,
    FCLASS_S,
    FCVT_S_W,
    FCVT_S_WU,
    FMV_W_X,
    // *rv64f instructions*
    FCVT_L_S,
    FCVT_LU_S,
    FCVT_S_L,
    FCVT_S_LU,
    // *rv32d instructions*
    FLD,
    FSD,
    FMADD_D,
    FMSUB_D,
    FNMSUB_D,
    FNMADD_D,
    FADD_D,
    FSUB_D,
    FMUL_D,
    FDIV_D,
    FSQRT_D,
    FSGNJ_D,
    FSGNJN_D,
    FSGNJX_D,
    FMIN_D,
    FMAX_D,
    FCVT_S_D,
    FCVT_D_S,
    FEQ_D,
    FLT_D,
    FLE_D,
    FCLASS_D,
    FCVT_W_D,
    FCVT_WU_D,
    FCVT_D_W,
    FCVT_D_WU,
    // *rv64d instructions*
    FCVT_L_D,
    FCVT_LU_D,
    FMV_X_D,
    FCVT_D_L,
    FCVT_D_LU,
    FMV_D_X,
    // *rv32q instructions*
    FLQ,
    FSQ,
    FMADD_Q,
    FMSUB_Q,
    FNMSUB_Q,
    FNMADD_Q,
    FADD_Q,
    FSUB_Q,
    FMUL_Q,
    FDIV_Q,
    FSQRT_Q,
    FSGNJ_Q,
    FSGNJN_Q,
    FSGNJX_Q,
    FMIN_Q,
    FMAX_Q,
    FCVT_S_Q,
    FCVT_Q_S,
    FCVT_D_Q,
    FCVT_Q_D,
    FEQ_Q,
    FLT_Q,
    FLE_Q,
    FCLASS_Q,
    FCVT_W_Q,
    FCVT_WU_Q,
    FCVT_Q_W,
    FCVT_Q_WU,
    // *rv64q instructions*
    FCVT_L_Q,
    FCVT_LU_Q,
    FCVT_Q_L,
    FCVT_Q_LU,
    // *rv32c/rv64c instructions*
    C_ADDI4SPN,
    C_FLD,
    C_LW,
    C_FLW,
    C_LD,
    C_FSD,
    C_SW,
    C_FSW,
    C_SD,
    C_NOP,
    C_ADDI,
    C_JAL,
    C_ADDIW,
    C_LI,
    C_ADDI16SP,
    C_LUI,
    C_SRLI,
    C_SRLI64,
    C_SRAI,
    C_SRAI64,
    C_ANDI,
    C_SUB,
    C_XOR,
    C_OR,
    C_AND,
    C_SUBW,
    C_ADDW,
    C_J,
    C_BEQZ,
    C_BNEZ,
    C_SLLI,
    C_SLLI64,
    C_FLDSP,
    C_LWSP,
    C_FLWSP,
    C_LDSP,
    C_JR,
    C_MV,
    C_EBREAK,
    C_JALR,
    C_ADD,
    C_FSDSP,
    C_SWSP,
    C_FSWSP,
    C_SDSP,
}

impl Opcode {
    fn is_relative(&self) -> bool {
        matches!(
            self,
            Self::JAL |
            Self::JALR |
            Self::BEQ |
            Self::BNE |
            Self::BLT |
            Self::BGE |
            Self::BLTU |
            Self::BGEU |
            Self::BEQZ |
            Self::BNEZ |
            Self::BLEZ |
            Self::BGEZ |
            Self::BLTZ |
            Self::BGTZ |
            Self::C_JAL |
            Self::C_BEQZ |
            Self::C_BNEZ
        )
    }
}

static OPCODE_NAMES: [&str; 284] = [
    "invalid",
    "la",
    "lla",
    "nop",
    "li",
    "mv",
    "not",
    "neg",
    "negw",
    "sext.w",
    "seqz",
    "snez",
    "sltz",
    "sgtz",
    "fmv.s",
    "fabs.s",
    "fneg.s",
    "fmv.d",
    "fabs.d",
    "fneg.d",
    "beqz",
    "bnez",
    "blez",
    "bgez",
    "bltz",
    "bgtz",
    "bgt",
    "ble",
    "bgtu",
    "bleu",
    "j",
    "jr",
    "ret",
    "call",
    "tail",
    "rdinstret",
    "rdcycle",
    "rdtime",
    "csrr",
    "csrw",
    "csrs",
    "csrc",
    "csrwi",
    "csrsi",
    "csrci",
    "frcsr",
    "fscsr",
    "frrm",
    "fsrm",
    "frflags",
    "fsflags",
    "lui",
    "auipc",
    "jal",
    "jalr",
    "beq",
    "bne",
    "blt",
    "bge",
    "bltu",
    "bgeu",
    "lb",
    "lh",
    "lw",
    "lbu",
    "lhu",
    "sb",
    "sh",
    "sw",
    "addi",
    "slti",
    "sltiu",
    "xori",
    "ori",
    "andi",
    "slli",
    "srli",
    "srai",
    "add",
    "sub",
    "sll",
    "slt",
    "sltu",
    "xor",
    "srl",
    "sra",
    "or",
    "and",
    "fence",
    "ecall",
    "ebreak",
    "lwu",
    "ld",
    "sd",
    "addiw",
    "slliw",
    "srliw",
    "sraiw",
    "addw",
    "subw",
    "sllw",
    "srlw",
    "sraw",
    "fence.i",
    "csrrw",
    "csrrs",
    "csrrc",
    "csrrwi",
    "csrrsi",
    "csrrci",
    "mul",
    "mulh",
    "mulhsu",
    "mulhu",
    "div",
    "divu",
    "rem",
    "remu",
    "mulw",
    "divw",
    "divuw",
    "remw",
    "remuw",
    "lr.w",
    "sc.w",
    "amoswap.w",
    "amoadd.w",
    "amoxor.w",
    "amoand.w",
    "amoor.w",
    "amomin.w",
    "amomax.w",
    "amominu.w",
    "amomaxu.w",
    "lr.d",
    "sc.d",
    "amoswap.d",
    "amoadd.d",
    "amoxor.d",
    "amoand.d",
    "amoor.d",
    "amomin.d",
    "amomax.d",
    "amominu.d",
    "amomaxu.d",
    "flw",
    "fsw",
    "fmadd.s",
    "fmsub.s",
    "fnmsub.s",
    "fnmadd.s",
    "fadd.s",
    "fsub.s",
    "fmul.s",
    "fdiv.s",
    "fsqrt.s",
    "fsgnj.s",
    "fsgnjn.s",
    "fsgnjx.s",
    "fmin.s",
    "fmax.s",
    "fcvt.w.s",
    "fcvt.wu.s",
    "fmv.x.w",
    "feq.s",
    "flt.s",
    "fle.s",
    "fclass.s",
    "fcvt.s.w",
    "fcvt.s.wu",
    "fmv.w.x",
    "fcvt.l.s",
    "fcvt.lu.s",
    "fcvt.s.l",
    "fcvt.s.lu",
    "fld",
    "fsd",
    "fmadd.d",
    "fmsub.d",
    "fnmsub.d",
    "fnmadd.d",
    "fadd.d",
    "fsub.d",
    "fmul.d",
    "fdiv.d",
    "fsqrt.d",
    "fsgnj.d",
    "fsgnjn.d",
    "fsgnjx.d",
    "fmin.d",
    "fmax.d",
    "fcvt.s.d",
    "fcvt.d.s",
    "feq.d",
    "flt.d",
    "fle.d",
    "fclass.d",
    "fcvt.w.d",
    "fcvt.wu.d",
    "fcvt.d.w",
    "fcvt.d.wu",
    "fcvt.l.d",
    "fcvt.lu.d",
    "fmv.x.d",
    "fcvt.d.l",
    "fcvt.d.lu",
    "fmv.d.x",
    "flq",
    "fsq",
    "fmadd.q",
    "fmsub.q",
    "fnmsub.q",
    "fnmadd.q",
    "fadd.q",
    "fsub.q",
    "fmul.q",
    "fdiv.q",
    "fsqrt.q",
    "fsgnj.q",
    "fsgnjn.q",
    "fsgnjx.q",
    "fmin.q",
    "fmax.q",
    "fcvt.s.q",
    "fcvt.q.s",
    "fcvt.d.q",
    "fcvt.q.d",
    "feq.q",
    "flt.q",
    "fle.q",
    "fclass.q",
    "fcvt.w.q",
    "fcvt.wu.q",
    "fcvt.q.w",
    "fcvt.q.wu",
    "fcvt.l.q",
    "fcvt.lu.q",
    "fcvt.q.l",
    "fcvt.q.lu",
    "c.addi4spn",
    "c.fld",
    "c.lw",
    "c.flw",
    "c.ld",
    "c.fsd",
    "c.sw",
    "c.fsw",
    "c.sd",
    "c.nop",
    "c.addi",
    "c.jal",
    "c.addiw",
    "c.li",
    "c.addi16sp",
    "c.lui",
    "c.srli",
    "c.srli64",
    "c.srai",
    "c.srai64",
    "c.andi",
    "c.sub",
    "c.xor",
    "c.or",
    "c.and",
    "c.subw",
    "c.addw",
    "c.j",
    "c.beqz",
    "c.bnez",
    "c.slli",
    "c.slli64",
    "c.fldsp",
    "c.lwsp",
    "c.flwsp",
    "c.ldsp",
    "c.jr",
    "c.mv",
    "c.ebreak",
    "c.jalr",
    "c.add",
    "c.fsdsp",
    "c.swsp",
    "c.fswsp",
    "c.sdsp",
];

impl Opcode {
    pub fn as_str(&self) -> &'static str {
        OPCODE_NAMES[*self as usize]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Operand {
    #[default]
    Nothing,
    Register(Register),
    Immediate(i32),
}

impl ToTokens for Operand {
    fn tokenize(&self, stream: &mut TokenStream, symbols: &Index) {
        match self {
            Self::Register(reg) => stream.push(reg.as_str(), Colors::register()),
            Self::Immediate(imm) => {
                match symbols.get_sym_by_addr(*imm as usize) {
                    Some(symbol) => {
                        for token in symbol.name() {
                            stream.push_token(token.clone());
                        }
                    }
                    None => stream.push_owned(imm.to_string(), Colors::immediate()),
                }
            }
            Self::Nothing => unreachable!("empty operand encountered"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    opcode: Opcode,
    operands: [Operand; 3],
    operand_count: usize,
    len: usize,
}

impl decoder::Decoded for Instruction {
    fn width(&self) -> usize {
        self.len
    }

    fn update_rel_addrs(&mut self, addr: usize, _: Option<&Instruction>) {
        if !self.opcode.is_relative() {
            return;
        }

        for operand in &mut self.operands[..self.operand_count] {
            if let Operand::Immediate(imm) = operand {
                // questionable at best.
                // ensuring all the i64 conversion are correct is hard
                *imm = imm.saturating_add_unsigned(addr as u32);
            }
        }
    }
}

pub struct Decoder {
    pub is_64: bool,
}

impl decoder::Decodable for Decoder {
    type Instruction = Instruction;

    fn decode(&self, reader: &mut decoder::Reader) -> Result<Self::Instruction, Error> {
        decode(reader, self).map_err(|err| Error::new(err, 4))
    }

    fn max_width(&self) -> usize {
        4
    }
}

fn decode(reader: &mut decoder::Reader, decoder: &Decoder) -> Result<Instruction, ErrorKind> {
    use Opcode::*;

    let is_64 = decoder.is_64;
    let mut word1 = [0u8; 2];
    reader.next_n(&mut word1).ok_or(ErrorKind::ExhaustedInput)?;

    // check if the instruction is compressed
    if word1[0] & 0b11 != 0b11 {
        let bytes = u16::from_le_bytes(word1);
        let opcode = bytes & 0b11;
        let jump3 = bytes >> 13 & 0b111;

        let decoded_inst = match opcode {
            0b00 => match jump3 {
                0b000 => decode_addi4spn(bytes),
                0b001 => decode_comp_fsld(C_FLD, bytes),
                0b010 => decode_comp_slw(C_LW, bytes),
                0b011 if !is_64 => decode_comp_fslw(C_FLW, bytes),
                0b011 if is_64 => decode_comp_sld(C_LD, bytes),
                0b101 => decode_comp_fsld(C_FSD, bytes),
                0b110 => decode_comp_slw(C_SW, bytes),
                0b111 if !is_64 => decode_comp_fslw(C_FSW, bytes),
                0b111 if is_64 => decode_comp_sld(C_SD, bytes),
                _ => Err(ErrorKind::InvalidOpcode),
            },
            0b01 => match jump3 {
                0b000 if bytes >> 7 & 0b11111 == 0 => decode_comp_unique(C_NOP),
                0b000 if bytes >> 7 & 0b11111 != 0 => decode_comp_addi(C_ADDI, bytes),
                0b001 if !is_64 => decode_comp_jump(C_JAL, bytes),
                0b001 if is_64 => decode_comp_addi(C_ADDIW, bytes),
                0b010 => decode_comp_li(C_LI, bytes),
                0b011 if bytes >> 7 & 0b11111 == 2 => decode_addi16sp(bytes),
                0b011 if bytes >> 7 & 0b11111 != 2 => decode_comp_li(C_LUI, bytes),
                0b100 => match bytes >> 10 & 0b11 {
                    0b00 => decode_comp_shift(C_SRLI, bytes),
                    0b01 => decode_comp_shift(C_SRAI, bytes),
                    0b11 => match (bytes >> 5 & 0b11, bytes >> 12 & 0b1) {
                        (0b00, 0b0) if !is_64 => decode_comp_arith(C_SUB, bytes),
                        (0b01, 0b0) => decode_comp_arith(C_XOR, bytes),
                        (0b10, 0b0) => decode_comp_arith(C_OR, bytes),
                        (0b11, 0b0) => decode_comp_arith(C_AND, bytes),
                        (0b00, 0b1) if is_64 => decode_comp_arith(C_SUBW, bytes),
                        (0b01, 0b1) if is_64 => decode_comp_arith(C_ADDW, bytes),
                        _ => Err(ErrorKind::InvalidOpcode),
                    },
                    _ => Err(ErrorKind::InvalidOpcode),
                },
                0b101 => decode_comp_jump(C_J, bytes),
                0b110 => decode_comp_branch(C_BEQZ, bytes),
                0b111 => decode_comp_branch(C_BNEZ, bytes),
                _ => Err(ErrorKind::InvalidOpcode),
            },
            0b10 => match jump3 {
                0b000 => decode_comp_shift(C_SLLI, bytes),
                0b001 => decode_comp_ldsp(C_FLDSP, bytes),
                0b010 => decode_comp_lwsp(C_LWSP, bytes),
                0b011 if !is_64 => decode_comp_lwsp(C_FLWSP, bytes),
                0b011 if is_64 => decode_comp_ldsp(C_LDSP, bytes),
                0b100 => match (bytes >> 12 & 0b1, bytes >> 2 & 0b11111) {
                    (0b0, 0b0) => decode_comp_jumpr(bytes),
                    (0b0, _) => decode_comp_mv(bytes),
                    (0b1, 0b0) => decode_comp_unique(C_EBREAK),
                    (0b1, _) => decode_comp_add(bytes),
                    _ => Err(ErrorKind::InvalidOpcode),
                },
                0b101 => decode_comp_sdsp(C_FSDSP, bytes),
                0b110 => decode_comp_swsp(C_SWSP, bytes),
                0b111 if !is_64 => decode_comp_swsp(C_FSWSP, bytes),
                0b111 if is_64 => decode_comp_sdsp(C_SDSP, bytes),
                _ => Err(ErrorKind::InvalidOpcode),
            },
            _ => Err(ErrorKind::InvalidOpcode),
        };

        return decoded_inst.map(map_to_psuedo);
    }

    let mut word2 = [0u8; 2];
    reader.next_n(&mut word2).ok_or(ErrorKind::ExhaustedInput)?;
    let dword = u32::from_le_bytes([word1[0], word1[1], word2[0], word2[1]]);
    let opcode = word1[0] & 0b1111111;

    let decoded_inst = match opcode {
        _ if dword == 0b000000000000_00000_000_00000_1110011 => decode_unique(ECALL),
        _ if dword == 0b000000000001_00000_000_00000_1110011 => decode_unique(EBREAK),
        0b0001111 => decode_unique(FENCE),
        0b0110111 => decode_double(LUI, dword),
        0b0010111 => decode_double(AUIPC, dword),
        0b1101111 => decode_jump(dword),
        0b1100111 => decode_jumpr(dword),
        0b1100011 => match dword >> 12 & 0b111 {
            0b000 => decode_branch(BEQ, dword),
            0b001 => decode_branch(BNE, dword),
            0b100 => decode_branch(BLT, dword),
            0b101 => decode_branch(BGE, dword),
            0b110 => decode_branch(BLTU, dword),
            0b111 => decode_branch(BGEU, dword),
            _ => Err(ErrorKind::InvalidOpcode),
        },
        0b0000011 => match dword >> 12 & 0b111 {
            0b000 => decode_immediate(LB, dword),
            0b001 => decode_immediate(LH, dword),
            0b010 => decode_immediate(LW, dword),
            0b011 if is_64 => decode_immediate(LD, dword),
            0b100 => decode_immediate(LBU, dword),
            0b101 => decode_immediate(LHU, dword),
            0b110 if is_64 => decode_immediate(LWU, dword),
            _ => Err(ErrorKind::InvalidOpcode),
        },
        0b0100011 => match dword >> 12 & 0b111 {
            0b000 => decode_store(SB, dword),
            0b001 => decode_store(SH, dword),
            0b010 => decode_store(SW, dword),
            0b011 if is_64 => decode_store(SD, dword),
            _ => Err(ErrorKind::InvalidOpcode),
        },
        0b0010011 => match dword >> 12 & 0b111 {
            0b000 => decode_immediate(ADDI, dword),
            0b010 => decode_immediate(SLTI, dword),
            0b011 => decode_immediate(SLTIU, dword),
            0b100 => decode_immediate(XORI, dword),
            0b110 => decode_immediate(ORI, dword),
            0b111 => decode_immediate(ANDI, dword),
            0b001 => decode_arith(SLLI, dword, decoder),
            0b101 if dword >> 26 == 0b0000001 => decode_arith(SRAI, dword, decoder),
            0b101 if dword >> 26 == 0b0000000 => decode_arith(SRLI, dword, decoder),
            _ => Err(ErrorKind::InvalidOpcode),
        },
        0b0011011 => match dword >> 12 & 0b111 {
            _ if !is_64 => Err(ErrorKind::InvalidOpcode),
            0b000 => decode_immediate(ADDIW, dword),
            0b001 => decode_arith(SLLIW, dword, decoder),
            0b101 if dword >> 25 == 0b0000000 => decode_arith(SRLIW, dword, decoder),
            0b101 if dword >> 25 == 0b0100000 => decode_arith(SRAIW, dword, decoder),
            _ => Err(ErrorKind::InvalidOpcode),
        },
        0b0110011 => match dword >> 25 {
            0b0000000 => match dword >> 12 & 0b111 {
                0b000 => decode_triplet(ADD, dword),
                0b001 => decode_triplet(SLL, dword),
                0b010 => decode_triplet(SLT, dword),
                0b011 => decode_triplet(SLTU, dword),
                0b100 => decode_triplet(XOR, dword),
                0b101 => decode_triplet(SRL, dword),
                0b110 => decode_triplet(OR, dword),
                0b111 => decode_triplet(AND, dword),
                _ => Err(ErrorKind::InvalidOpcode),
            },
            0b0100000 => match dword >> 12 & 0b111 {
                0b000 => decode_triplet(SUB, dword),
                0b101 => decode_triplet(SRA, dword),
                _ => Err(ErrorKind::InvalidOpcode),
            },
            _ => Err(ErrorKind::InvalidOpcode),
        },
        0b0111011 => match dword >> 25 {
            _ if !is_64 => Err(ErrorKind::InvalidOpcode),
            0b0000000 => match dword >> 12 & 0b111 {
                0b000 => decode_triplet(ADDW, dword),
                0b001 => decode_triplet(SLLW, dword),
                0b101 => decode_triplet(SRLW, dword),
                _ => Err(ErrorKind::InvalidOpcode),
            },
            0b0100000 => match dword >> 12 & 0b111 {
                0b000 => decode_triplet(SUBW, dword),
                0b101 => decode_triplet(SRAW, dword),
                _ => Err(ErrorKind::InvalidOpcode),
            },
            _ => Err(ErrorKind::InvalidOpcode),
        },
        _ => Err(ErrorKind::InvalidOpcode),
    };

    decoded_inst.map(map_to_psuedo)
}

impl ToTokens for Instruction {
    fn tokenize(&self, stream: &mut TokenStream, symbols: &Index) {
        stream.push(self.opcode.as_str(), Colors::opcode());

        // there are operands
        if self.operand_count > 0 {
            stream.push(" ", Colors::spacing());

            // iterate through operands
            for idx in 0..self.operand_count {
                self.operands[idx].tokenize(stream, symbols);

                // separator
                if idx != self.operand_count - 1 {
                    stream.push(", ", Colors::expr());
                }
            }
        }
    }
}

// NOTE: doing closure assignment in `map_to_psuedo` makes the compiler
// assign function mappings in the array on each call.
static MAPPING: Lazy<[fn(&mut Instruction); 284]> = Lazy::new(|| unsafe {
    const DO_NOTHING: fn(&mut Instruction) = |_| {};
    static mut MAPPING: [fn(&mut Instruction); 284] = [DO_NOTHING; 284];

    MAPPING[Opcode::C_ADDI as usize] = |inst| {
        if inst.operands[0] == Operand::Register(Register::Zero)
            && inst.operands[1] == Operand::Register(Register::Zero)
            && inst.operands[2] == Operand::Immediate(0)
        {
            inst.opcode = Opcode::NOP;
            inst.operand_count = 0;
            return;
        }

        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::LI;
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
            return;
        }

        if inst.operands[0] == inst.operands[1] && inst.operands[2] == Operand::Immediate(0) {
            inst.opcode = Opcode::MV;
            inst.operand_count = 2;
            return;
        }

        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_ADDW as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::ADDW as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::ADDI as usize] = |inst| {
        if inst.operands[0] == Operand::Register(Register::Zero)
            && inst.operands[1] == Operand::Register(Register::Zero)
            && inst.operands[2] == Operand::Immediate(0)
        {
            inst.opcode = Opcode::NOP;
            inst.operand_count = 0;
            return;
        }

        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::LI;
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
            return;
        }

        if inst.operands[0] == inst.operands[1] && inst.operands[2] == Operand::Immediate(0) {
            inst.opcode = Opcode::MV;
            inst.operand_count = 2;
            return;
        }

        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::ADD as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_ADD as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_ADDIW as usize] = |inst| {
        if inst.operands[2] == Operand::Immediate(0) {
            inst.opcode = Opcode::SEXT_W;
            inst.operand_count = 2;
            return;
        }

        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::ADDIW as usize] = |inst| {
        if inst.operands[2] == Operand::Immediate(0) {
            inst.opcode = Opcode::SEXT_W;
            inst.operand_count = 2;
            return;
        }

        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_XOR as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::XOR as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::XORI as usize] = |inst| {
        if inst.operands[2] == Operand::Immediate(-1) {
            inst.opcode = Opcode::NOT;
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_AND as usize] = |inst| {
        if inst.operands[2] == Operand::Immediate(-1) {
            inst.opcode = Opcode::NOT;
            inst.operand_count = 2;
            return;
        }

        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::AND as usize] = |inst| {
        if inst.operands[2] == Operand::Immediate(-1) {
            inst.opcode = Opcode::NOT;
            inst.operand_count = 2;
            return;
        }

        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::OR as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_OR as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_SUB as usize] = |inst| {
        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::NEG;
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::SUB as usize] = |inst| {
        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::NEG;
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_SUBW as usize] = |inst| {
        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::NEG;
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::SUBW as usize] = |inst| {
        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::NEG;
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::SLTIU as usize] = |inst| {
        if inst.operands[2] == Operand::Immediate(1) {
            inst.opcode = Opcode::SEQZ;
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::SLTU as usize] = |inst| {
        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::SNEZ;
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::SLT as usize] = |inst| {
        if inst.operands[2] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::SLTZ;
            inst.operand_count = 2;
            return;
        }

        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::SGTZ;
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::FSGNJ_S as usize] = |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.opcode = Opcode::FMV_S;
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::FSGNJX_S as usize] = |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.opcode = Opcode::FABS_S;
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::FSGNJN_S as usize] = |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.opcode = Opcode::FNEG_S;
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::FSGNJ_D as usize] = |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.opcode = Opcode::FMV_D;
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::FSGNJX_D as usize] = |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.opcode = Opcode::FABS_D;
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::FSGNJN_D as usize] = |inst| {
        if inst.operands[1] == inst.operands[2] {
            inst.opcode = Opcode::FNEG_D;
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::BEQ as usize] = |inst| {
        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::BEQZ;
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::BNE as usize] = |inst| {
        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::BNEZ;
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::BGE as usize] = |inst| {
        if inst.operands[0] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::BLEZ;
            inst.operands.swap(0, 1);
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
            return;
        }

        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::BGEZ;
            inst.operands.swap(0, 1);
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::BLT as usize] = |inst| {
        if inst.operands[0] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::BLTZ;
            inst.operands.swap(0, 1);
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
            return;
        }

        if inst.operands[1] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::BGTZ;
            inst.operands.swap(0, 1);
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_JAL as usize] = |inst| {
        if inst.operands[0] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::C_J;
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Ra) {
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
        }
    };

    MAPPING[Opcode::JAL as usize] = |inst| {
        if inst.operands[0] == Operand::Register(Register::Zero) {
            inst.opcode = Opcode::J;
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Ra) {
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
        }
    };

    MAPPING[Opcode::C_JALR as usize] = |inst| {
        if inst.operands[0] == Operand::Register(Register::Zero)
            && inst.operands[1] == Operand::Register(Register::Ra)
            && inst.operands[2] == Operand::Immediate(0)
        {
            inst.opcode = Opcode::RET;
            inst.operand_count = 0;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Zero)
            && inst.operands[2] == Operand::Immediate(0)
        {
            inst.opcode = Opcode::C_JR;
            inst.operands.swap(0, 2);
            inst.operand_count = 1;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Ra)
            && inst.operands[1] == Operand::Register(Register::Ra)
            && inst.operands[2] == Operand::Immediate(0)
        {
            inst.opcode = Opcode::RET;
            inst.operand_count = 0;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Ra)
            && inst.operands[2] == Operand::Immediate(0)
        {
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Ra)
            && inst.operands[1] == Operand::Register(Register::Ra)
        {
            inst.opcode = Opcode::CALL;
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Zero)
            && inst.operands[1] == Operand::Register(Register::T1)
        {
            inst.opcode = Opcode::TAIL;
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
        }
    };

    MAPPING[Opcode::JALR as usize] = |inst| {
        if inst.operands[0] == Operand::Register(Register::Zero)
            && inst.operands[1] == Operand::Register(Register::Ra)
            && inst.operands[2] == Operand::Immediate(0)
        {
            inst.opcode = Opcode::RET;
            inst.operand_count = 0;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Zero)
            && inst.operands[2] == Operand::Immediate(0)
        {
            inst.opcode = Opcode::JR;
            inst.operands.swap(0, 2);
            inst.operand_count = 1;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Ra)
            && inst.operands[1] == Operand::Register(Register::Ra)
            && inst.operands[2] == Operand::Immediate(0)
        {
            inst.opcode = Opcode::RET;
            inst.operand_count = 0;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Ra)
            && inst.operands[2] == Operand::Immediate(0)
        {
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Ra)
            && inst.operands[1] == Operand::Register(Register::Ra)
        {
            inst.opcode = Opcode::CALL;
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
            return;
        }

        if inst.operands[0] == Operand::Register(Register::Zero)
            && inst.operands[1] == Operand::Register(Register::T1)
        {
            inst.opcode = Opcode::TAIL;
            inst.operands.swap(0, 1);
            inst.operand_count = 1;
        }
    };

    MAPPING[Opcode::AUIPC as usize] = |inst| {
        if inst.operands[0] == Operand::Register(Register::Ra) {
            inst.opcode = Opcode::CALL;

            match (inst.operands[0], inst.operands[1]) {
                (Operand::Register(reg), Operand::Immediate(mut imm)) => {
                    // offset[31 : 12] + offset[11] where register is bit's [11:6]
                    imm <<= 1;
                    imm |= ((reg as u16 & 0b10000) >> 4) as i32;
                    inst.operands[0] = Operand::Immediate(imm);
                    inst.operand_count = 1;
                }
                _ => unreachable!(),
            }

            return;
        }

        if inst.operands[0] == Operand::Register(Register::T1) {
            inst.opcode = Opcode::TAIL;

            match (inst.operands[0], inst.operands[1]) {
                (Operand::Register(reg), Operand::Immediate(mut imm)) => {
                    // offset[31 : 12] + offset[11] where register is bit's [11:6]
                    imm <<= 1;
                    imm |= ((reg as u16 & 0b10000) >> 4) as i32;
                    inst.operands[0] = Operand::Immediate(imm);
                    inst.operand_count = 1;
                }
                _ => unreachable!(),
            }
        }
    };

    MAPPING[Opcode::C_SRAI as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::SRAI as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_SRAI64 as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_SRLI as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::SRLI as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_SRLI64 as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_SLLI as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::SLLI as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING[Opcode::C_SLLI64 as usize] = |inst| {
        if inst.operands[0] == inst.operands[1] {
            inst.operands.swap(1, 2);
            inst.operand_count = 2;
        }
    };

    MAPPING
});

#[inline]
fn map_to_psuedo(mut inst: Instruction) -> Instruction {
    MAPPING[inst.opcode as usize](&mut inst);
    inst
}

/// Decode's beqz and bnez instructions.
fn decode_comp_branch(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rs = Register::get_int(word >> 7 & 0b111)?;
    let mut imm = 0;

    imm |= word >> 4 & 0b100000000;
    imm |= word << 1 & 0b011000000;
    imm |= word << 3 & 0b000100000;
    imm |= word >> 7 & 0b000011000;
    imm |= word >> 2 & 0b000000110;

    // cast to i32 to prevent rust overflowing literal complaints
    let mut imm = imm as i32;

    if imm & 0b100000000 != 0 {
        imm |= (imm | 0b1111111000000000) as i16 as i32;
    }

    let (operands, operand_count) = operands![
        Operand::Register(rs),
        Operand::Register(rs),
        Operand::Immediate(imm)
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's beqz and bnez instructions.
fn decode_comp_jump(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let mut imm = 0;

    imm |= word >> 1 & 0b100000000000;
    imm |= word << 2 & 0b010000000000;
    imm |= word >> 1 & 0b001100000000;
    imm |= word << 1 & 0b000010000000;
    imm |= word >> 1 & 0b000001000000;
    imm |= word << 3 & 0b000000100000;
    imm |= word >> 7 & 0b000000010000;
    imm |= word >> 2 & 0b000000001110;

    // cast to i64 to prevent rust overflowing literal complaints
    let mut imm = imm as i32;

    if imm & 0b100000000000 != 0 {
        imm |= (imm | 0b1111000000000000) as i16 as i32;
    }

    let (operands, operand_count) = operands![Operand::Immediate(imm)];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's j and jal instructions.
fn decode_comp_jumpr(word: u16) -> Result<Instruction, ErrorKind> {
    let rs = Register::get((word >> 7 & 0b11111) as u32)?;

    let (operands, operand_count) = operands![
        Operand::Register(Register::Ra),
        Operand::Register(rs),
        Operand::Immediate(0)
    ];

    Ok(Instruction {
        opcode: Opcode::C_JALR,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's sub, or, xor, and, subw and addw instructions.
fn decode_comp_arith(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rd = Register::get_int(word >> 7 & 0b111)?;
    let rs = Register::get_int(word >> 2 & 0b111)?;

    let (operands, operand_count) = operands![
        Operand::Register(rd),
        Operand::Register(rd),
        Operand::Register(rs),
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's srli, srai and slli instructions.
fn decode_comp_shift(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rd = Register::get_int(word >> 7 & 0b111)?;
    let shamt = (word >> 7 & 0b100000) | (word >> 2 & 0b11111);

    let (operands, operand_count) = operands![
        Operand::Register(rd),
        Operand::Register(rd),
        Operand::Immediate(shamt as i32)
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's addi and addiw instructions.
fn decode_comp_addi(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rd = Register::get((word >> 7 & 0b11111) as u32)?;
    let mut imm = (((word >> 7) & 0b100000) | ((word >> 2) & 0b11111)) as i16;

    if imm & 0b100000 != 0 {
        imm = (imm | 0b11000000) as i8 as i16;
    }

    let (operands, operand_count) = operands![
        Operand::Register(rd),
        Operand::Register(rd),
        Operand::Immediate(imm as i32),
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's addi16sp instruction also represented as addi sp, sp, imm*16.
fn decode_addi16sp(word: u16) -> Result<Instruction, ErrorKind> {
    let mut imm = 0;

    imm |= word >> 3 & 0b1000000000;
    imm |= word >> 2 & 0b0000010000;
    imm |= word << 1 & 0b0001000000;
    imm |= word << 4 & 0b0110000000;
    imm |= word << 3 & 0b0000100000;

    let mut imm = imm as i32;

    if imm & 0b1000000000 != 0 {
        imm = (imm | 0b1111110000000000) as i16 as i32;
    }

    let (operands, operand_count) = operands![Operand::Immediate(imm)];

    Ok(Instruction {
        opcode: Opcode::C_ADDI16SP,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's addi14spn instruction also represented as addi sp, rs, imm*4.
fn decode_addi4spn(word: u16) -> Result<Instruction, ErrorKind> {
    let rd = Register::get_int(word >> 2 & 0b111)?;
    let mut imm = 0;

    imm |= word >> 1 & 0b1111000000;
    imm |= word >> 7 & 0b0000110000;
    imm |= word >> 2 & 0b0000001000;
    imm |= word >> 4 & 0b0000000100;

    let (operands, operand_count) =
        operands![Operand::Register(rd), Operand::Immediate(imm as i32)];

    Ok(Instruction {
        opcode: Opcode::C_ADDI4SPN,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's add instruction.
fn decode_comp_add(word: u16) -> Result<Instruction, ErrorKind> {
    let rd = Register::get((word >> 7 & 0b11111) as u32)?;
    let rs = Register::get((word >> 2 & 0b11111) as u32)?;

    let (operands, operand_count) = operands![
        Operand::Register(rd),
        Operand::Register(rd),
        Operand::Register(rs)
    ];

    Ok(Instruction {
        opcode: Opcode::C_ADD,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's li and lui instructions.
fn decode_comp_li(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let mut imm = (((word >> 7) & 0b100000) | ((word >> 2) & 0b11111)) as i16;
    let rs = Register::get((word >> 7 & 0b11111) as u32)?;

    if imm & 0b100000 != 0 {
        imm = (imm | 0b11000000) as i8 as i16;
    }

    let (operands, operand_count) =
        operands![Operand::Register(rs), Operand::Immediate(imm as i32)];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's store word relative to sp instruction for both integers and floats.
fn decode_comp_swsp(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rd = Register::get((word >> 2 & 0b11111) as u32)?;
    let imm = (word >> 1 & 0b11000000) | (word >> 7 & 0b111100);

    let (operands, operand_count) =
        operands![Operand::Register(rd), Operand::Immediate(imm as i32)];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's store double relative to sp instruction for both integers and floats.
fn decode_comp_sdsp(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rd = Register::get((word >> 2 & 0b11111) as u32)?;
    let imm = (word >> 1 & 0b111000000) | (word >> 7 & 0b111000);

    let (operands, operand_count) =
        operands![Operand::Register(rd), Operand::Immediate(imm as i32)];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's load word relative to sp instruction for both integers and floats.
fn decode_comp_lwsp(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rd = Register::get((word >> 7 & 0b11111) as u32)?;
    let imm = (word << 4 & 0b11000000) | (word >> 7 & 0b100000) | (word >> 2 & 0b11100);

    let (operands, operand_count) =
        operands![Operand::Register(rd), Operand::Immediate(imm as i32)];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 4,
    })
}

/// Decode's load double relative to sp instruction for both integers and floats.
fn decode_comp_ldsp(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rd = Register::get((word >> 7 & 0b11111) as u32)?;
    let imm = (word << 4 & 0b111000000) | (word >> 7 & 0b100000) | (word >> 2 & 0b11000);

    let (operands, operand_count) =
        operands![Operand::Register(rd), Operand::Immediate(imm as i32)];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's load word instruction for integers.
fn decode_comp_slw(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rs1 = Register::get_int(word >> 2 & 0b111)?;
    let rs2 = Register::get_int(word >> 7 & 0b111)?;
    let imm = (word << 1 & 0b1000000) | (word >> 7 & 0b111000) | (word >> 4 & 0b100);

    let (operands, operand_count) = operands![
        Operand::Register(rs1),
        Operand::Register(rs2),
        Operand::Immediate(imm as i32)
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's load double instruction for integers.
fn decode_comp_sld(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rs1 = Register::get_int(word >> 2 & 0b111)?;
    let rs2 = Register::get_int(word >> 7 & 0b111)?;
    let imm = (word << 1 & 0b11000000) | (word >> 7 & 0b111000);

    let (operands, operand_count) = operands![
        Operand::Register(rs1),
        Operand::Register(rs2),
        Operand::Immediate(imm as i32)
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's load word instruction for floats.
fn decode_comp_fslw(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rs1 = Register::get_fp(word >> 2 & 0b111)?;
    let rs2 = Register::get_fp(word >> 7 & 0b111)?;
    let imm = (word << 1 & 0b1000000) | (word >> 7 & 0b111000) | (word >> 4 & 0b100);

    let (operands, operand_count) = operands![
        Operand::Register(rs1),
        Operand::Register(rs2),
        Operand::Immediate(imm as i32)
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's load double instruction for floats.
fn decode_comp_fsld(opcode: Opcode, word: u16) -> Result<Instruction, ErrorKind> {
    let rs1 = Register::get_fp(word >> 2 & 0b111)?;
    let rs2 = Register::get_fp(word >> 7 & 0b111)?;
    let imm = (word << 1 & 0b11000000) | (word >> 7 & 0b111000);

    let (operands, operand_count) = operands![
        Operand::Register(rs1),
        Operand::Register(rs2),
        Operand::Immediate(imm as i32)
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's move instruction.
fn decode_comp_mv(word: u16) -> Result<Instruction, ErrorKind> {
    let rs = Register::get((word >> 2 & 0b11111) as u32)?;
    let rd = Register::get((word >> 7 & 0b11111) as u32)?;

    let (operands, operand_count) = operands![Operand::Register(rd), Operand::Register(rs)];

    Ok(Instruction {
        opcode: Opcode::C_MV,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's instructions with weird formatting that aren't yet handled.
fn decode_comp_unique(opcode: Opcode) -> Result<Instruction, ErrorKind> {
    let (operands, operand_count) = operands![];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 2,
    })
}

/// Decode's instructions with weird formatting that aren't yet handled.
fn decode_unique(opcode: Opcode) -> Result<Instruction, ErrorKind> {
    let (operands, operand_count) = operands![];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 4,
    })
}

/// Decode's sb, sh, sw and sd store instructions.
fn decode_store(opcode: Opcode, dword: u32) -> Result<Instruction, ErrorKind> {
    let mut imm = 0;

    imm |= ((dword & 0b11111110000000000000000000000000) as i32 >> 20) as u32;
    imm |= dword >> 7 & 0b11111;

    let imm = imm as i32;
    let rs1 = Register::get(dword >> 15 & 0b11111)?;
    let rs2 = Register::get(dword >> 20 & 0b11111)?;

    let (operands, operand_count) = operands![
        Operand::Register(rs2),
        Operand::Register(rs1),
        Operand::Immediate(imm),
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 4,
    })
}

/// Decode's beq, bne, blt, bge, bltu and bgeu branch instructions.
fn decode_branch(opcode: Opcode, dword: u32) -> Result<Instruction, ErrorKind> {
    let rs1 = Register::get(dword >> 15 & 0b11111)?;
    let rs2 = Register::get(dword >> 20 & 0b11111)?;

    let mut imm = 0;

    // sign extend the 32nd bit and shift it into the 13th bit
    imm |= ((dword & 0b10000000000000000000000000000000) as i32 >> 19) as u32;
    imm |= dword << 4 & 0b100000000000;
    imm |= dword >> 20 & 0b011111100000;
    imm |= dword >> 7 & 0b000000011110;

    let (operands, operand_count) = operands![
        Operand::Register(rs1),
        Operand::Register(rs2),
        Operand::Immediate(imm as i32),
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 4,
    })
}

/// Decode's jump instruction.
fn decode_jump(dword: u32) -> Result<Instruction, ErrorKind> {
    let mut imm = 0;
    let rd = Register::get(dword >> 7 & 0b11111)?;

    // 18 bits (riscv instruction jumps are 16-byte alligned)
    imm |= ((dword & 0b10000000000000000000000000000000) as i32 >> 11) as u32;
    imm |= dword & 0b11111111000000000000;
    imm |= dword >> 9 & 0b00000000100000000000;
    imm |= dword >> 20 & 0b00000000011111111110;

    let (operands, operand_count) =
        operands![Operand::Register(rd), Operand::Immediate(imm as i32)];

    Ok(Instruction {
        opcode: Opcode::JAL,
        operands,
        operand_count,
        len: 4,
    })
}

/// Decode's ret instruction.
fn decode_jumpr(bytes: u32) -> Result<Instruction, ErrorKind> {
    let imm = bytes as i32 >> 20;
    let rd = Register::get(bytes >> 7 & 0b11111)?;
    let (operands, operand_count) = operands![Operand::Register(rd), Operand::Immediate(imm)];

    Ok(Instruction {
        opcode: Opcode::JALR,
        operands,
        operand_count,
        len: 4,
    })
}

/// Decode's instructions that have two registers and an immediate.
fn decode_immediate(opcode: Opcode, dword: u32) -> Result<Instruction, ErrorKind> {
    let rd = Register::get(dword >> 7 & 0b11111)?;
    let rs = Register::get(dword >> 15 & 0b11111)?;
    let imm = dword as i32 >> 20;

    let (operands, operand_count) = operands![
        Operand::Register(rd),
        Operand::Register(rs),
        Operand::Immediate(imm),
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 4,
    })
}

/// Decode's slli, srai, srli, slliw, sraiw and srliw  instruction's.
fn decode_arith(opcode: Opcode, dword: u32, opts: &Decoder) -> Result<Instruction, ErrorKind> {
    let rd = Register::get(dword >> 7 & 0b11111)?;
    let rs = Register::get(dword >> 15 & 0b11111)?;

    let shamt = if opts.is_64 {
        dword >> 20 & 0b111111
    } else {
        dword >> 20 & 0b11111
    };

    let (operands, operand_count) = operands![
        Operand::Register(rd),
        Operand::Register(rs),
        Operand::Immediate(shamt as i32),
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 4,
    })
}

/// Decode's instructions that have three registers.
fn decode_triplet(opcode: Opcode, dword: u32) -> Result<Instruction, ErrorKind> {
    let rd = Register::get(dword >> 7 & 0b11111)?;
    let rs1 = Register::get(dword >> 15 & 0b11111)?;
    let rs2 = Register::get(dword >> 20 & 0b11111)?;

    let (operands, operand_count) = operands![
        Operand::Register(rd),
        Operand::Register(rs1),
        Operand::Register(rs2)
    ];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 4,
    })
}

/// Decode's instructions that have have a registers and an immediate.
fn decode_double(opcode: Opcode, dword: u32) -> Result<Instruction, ErrorKind> {
    let imm = dword >> 12;
    let rd = Register::get(dword >> 7 & 0b11111)?;

    let (operands, operand_count) =
        operands![Operand::Register(rd), Operand::Immediate(imm as i32)];

    Ok(Instruction {
        opcode,
        operands,
        operand_count,
        len: 4,
    })
}
