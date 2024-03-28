#![allow(non_snake_case)]
/// `sha256: 294668ae6480133b32d85e9567cc77c5eb0e1232decdf42cac7ab480e884f6e0`

use core::fmt::{self, Display, Formatter};

use decoder::{Decoded, Decodable, Error, ErrorKind, Reader, ToTokens, TokenStream};
use symbols::Index;
use tokenizing::{ColorScheme, Colors};

mod thumb;

// opcode, s, w, cond
/// a struct for the combined display of an opcode and possible suffixes.
///
/// this includes the opcode, its optional `.s` suffix, optional `.w` suffix, and condition code,
/// if any.
pub struct ConditionedOpcode(pub Opcode, pub bool, pub bool, pub ConditionCode);

impl Display for ConditionedOpcode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}{}{}{}",
            self.0,
            if self.1 { "s" } else { "" },
            if self.2 { ".w" } else { "" },
            self.3
        )
    }
}

impl ToTokens for ConditionedOpcode {
    fn tokenize(&self, stream: &mut TokenStream, _: &Index) {
        match self.0 {
            Opcode::UDF | Opcode::Invalid => {
                // invalid_op
                stream.push_owned(self.to_string(), Colors::invalid())
            }
            Opcode::TBB
            | Opcode::TBH
            | Opcode::CBZ
            | Opcode::CBNZ
            | Opcode::IT
            | Opcode::B
            | Opcode::BL
            | Opcode::BLX
            | Opcode::BX
            | Opcode::BXJ => {
                // control_flow_op
                stream.push_owned(self.to_string(), Colors::opcode())
            }
            Opcode::AND
            | Opcode::EOR
            | Opcode::ORR
            | Opcode::ORN
            | Opcode::LSL
            | Opcode::LSR
            | Opcode::ROR
            | Opcode::ASR
            | Opcode::RRX
            | Opcode::BIC
            | Opcode::ADR
            | Opcode::SUB
            | Opcode::RSB
            | Opcode::ADD
            | Opcode::ADC
            | Opcode::SBC
            | Opcode::RSC
            | Opcode::QADD
            | Opcode::QSUB
            | Opcode::QDADD
            | Opcode::QDSUB
            | Opcode::SADD16
            | Opcode::QADD16
            | Opcode::SHADD16
            | Opcode::SASX
            | Opcode::QASX
            | Opcode::SHASX
            | Opcode::SSAX
            | Opcode::QSAX
            | Opcode::SHSAX
            | Opcode::SSUB16
            | Opcode::QSUB16
            | Opcode::SHSUB16
            | Opcode::SADD8
            | Opcode::QADD8
            | Opcode::SHADD8
            | Opcode::SSUB8
            | Opcode::QSUB8
            | Opcode::SHSUB8
            | Opcode::UADD16
            | Opcode::UQADD16
            | Opcode::UHADD16
            | Opcode::UASX
            | Opcode::UQASX
            | Opcode::UHASX
            | Opcode::USAX
            | Opcode::UQSAX
            | Opcode::UHSAX
            | Opcode::USUB16
            | Opcode::UQSUB16
            | Opcode::UHSUB16
            | Opcode::UADD8
            | Opcode::UQADD8
            | Opcode::UHADD8
            | Opcode::USUB8
            | Opcode::UQSUB8
            | Opcode::UHSUB8
            | Opcode::CLZ
            | Opcode::MUL
            | Opcode::MLA
            | Opcode::UMAAL
            | Opcode::MLS
            | Opcode::UMULL
            | Opcode::UMLAL
            | Opcode::SMLSD
            | Opcode::SMMLA
            | Opcode::SMMLS
            | Opcode::USADA8
            | Opcode::USAD8
            | Opcode::SDIV
            | Opcode::UDIV
            | Opcode::SMLALD(_)
            | Opcode::SMLSLD(_)
            | Opcode::SMLAD
            | Opcode::SMUSD
            | Opcode::SMMUL
            | Opcode::SMULW(_)
            | Opcode::SMUAD
            | Opcode::SMULL
            | Opcode::SMUL(_, _)
            | Opcode::SMAL(_, _)
            | Opcode::SMLA(_, _)
            | Opcode::SMLAW(_)
            | Opcode::SMLAL
            | Opcode::SMLAL_halfword(_, _) => {
                // arithmetic_op
                stream.push_owned(self.to_string(), Colors::opcode())
            }
            Opcode::PUSH | Opcode::POP => {
                // stack_op
                stream.push_owned(self.to_string(), Colors::opcode())
            }
            Opcode::TST | Opcode::TEQ | Opcode::CMP | Opcode::CMN => {
                // comparison_op
                stream.push_owned(self.to_string(), Colors::opcode())
            }
            Opcode::LDRSH
            | Opcode::LDRSHT
            | Opcode::LDRSB
            | Opcode::LDRSBT
            | Opcode::STRD
            | Opcode::LDRD
            | Opcode::LDREXH
            | Opcode::STREXH
            | Opcode::LDREXB
            | Opcode::STREXB
            | Opcode::LDREXD
            | Opcode::STREXD
            | Opcode::LDREX
            | Opcode::STREX
            | Opcode::LDM(false, false, _, _)
            | Opcode::LDM(false, true, _, _)
            | Opcode::LDM(true, false, _, _)
            | Opcode::LDM(true, true, _, _)
            | Opcode::STM(false, false, _, _)
            | Opcode::STM(false, true, _, _)
            | Opcode::STM(true, false, _, _)
            | Opcode::STM(true, true, _, _)
            | Opcode::LDR
            | Opcode::STR
            | Opcode::LDRH
            | Opcode::STRH
            | Opcode::LDRB
            | Opcode::STRB
            | Opcode::LDRT
            | Opcode::STRT
            | Opcode::LDRHT
            | Opcode::STRHT
            | Opcode::LDRBT
            | Opcode::STRBT
            | Opcode::SWP
            | Opcode::SWPB
            | Opcode::MSR
            | Opcode::MRS
            | Opcode::CLREX
            | Opcode::SXTAB
            | Opcode::SXTAB16
            | Opcode::SXTAH
            | Opcode::SXTB
            | Opcode::SXTB16
            | Opcode::SXTH
            | Opcode::UXTAB
            | Opcode::UXTAB16
            | Opcode::UXTAH
            | Opcode::UXTB
            | Opcode::UXTB16
            | Opcode::UXTH
            | Opcode::PKHTB
            | Opcode::PKHBT
            | Opcode::REV
            | Opcode::REV16
            | Opcode::REVSH
            | Opcode::SSAT
            | Opcode::SSAT16
            | Opcode::SBFX
            | Opcode::USAT
            | Opcode::USAT16
            | Opcode::UBFX
            | Opcode::BFI
            | Opcode::BFC
            | Opcode::RBIT
            | Opcode::SEL
            | Opcode::MOV
            | Opcode::MOVT
            | Opcode::MVN => {
                // data_op
                stream.push_owned(self.to_string(), Colors::opcode())
            }
            Opcode::HINT
            | Opcode::NOP
            | Opcode::PLD
            | Opcode::PLI
            | Opcode::ISB
            | Opcode::DMB
            | Opcode::DSB
            | Opcode::CSDB
            | Opcode::SRS(_, _)
            | Opcode::BKPT => {
                // misc_op
                stream.push_owned(self.to_string(), Colors::opcode())
            }
            Opcode::DBG
            | Opcode::CPS(_)
            | Opcode::CPS_modeonly
            | Opcode::SETEND
            | Opcode::ENTERX
            | Opcode::LEAVEX
            | Opcode::YIELD
            | Opcode::WFE
            | Opcode::WFI
            | Opcode::SEV
            | Opcode::ERET
            | Opcode::RFE(_, _)
            | Opcode::HVC
            | Opcode::SVC
            | Opcode::SMC
            | Opcode::LDC(_)
            | Opcode::LDCL(_)
            | Opcode::LDC2(_)
            | Opcode::LDC2L(_)
            | Opcode::STC(_)
            | Opcode::STCL(_)
            | Opcode::STC2(_)
            | Opcode::STC2L(_)
            | Opcode::MCRR2(_, _)
            | Opcode::MCR2(_, _, _)
            | Opcode::MRRC2(_, _)
            | Opcode::MRC2(_, _, _)
            | Opcode::MCRR(_, _)
            | Opcode::MRRC(_, _)
            | Opcode::CDP2(_, _, _) => {
                // platform_op
                stream.push_owned(self.to_string(), Colors::opcode())
            }
        }
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            Opcode::LDRSH => {
                write!(f, "ldrsh")
            }
            Opcode::LDRSHT => {
                write!(f, "ldrsht")
            }
            Opcode::LDRSB => {
                write!(f, "ldrsb")
            }
            Opcode::LDRSBT => {
                write!(f, "ldrsbt")
            }
            Opcode::STRD => {
                write!(f, "strd")
            }
            Opcode::LDRD => {
                write!(f, "ldrd")
            }
            Opcode::LDC(_) => {
                write!(f, "ldc")
            }
            Opcode::LDCL(_) => {
                write!(f, "ldcl")
            }
            Opcode::LDC2(_) => {
                write!(f, "ldc2")
            }
            Opcode::LDC2L(_) => {
                write!(f, "ldc2l")
            }
            Opcode::STC(_) => {
                write!(f, "stc")
            }
            Opcode::STCL(_) => {
                write!(f, "stcl")
            }
            Opcode::STC2(_) => {
                write!(f, "stc2")
            }
            Opcode::STC2L(_) => {
                write!(f, "stc2l")
            }
            Opcode::MCRR2(_, _) => {
                write!(f, "mcrr2")
            }
            Opcode::MCR2(_, _, _) => {
                write!(f, "mcr2")
            }
            Opcode::MRRC2(_, _) => {
                write!(f, "mrrc2")
            }
            Opcode::MRC2(_, _, _) => {
                write!(f, "mrc2")
            }
            Opcode::MCRR(_, _) => {
                write!(f, "mcrr")
            }
            Opcode::MRRC(_, _) => {
                write!(f, "mrrc")
            }
            Opcode::CDP2(_, _, _) => {
                write!(f, "cdp2")
            }
            Opcode::SRS(p, u) => {
                write!(
                    f,
                    "srs{}{}",
                    if *u { "i" } else { "d" },
                    if *p { "b" } else { "a" }
                )
            }
            Opcode::RFE(p, u) => {
                write!(
                    f,
                    "rfe{}{}",
                    if *u { "i" } else { "d" },
                    if *p { "b" } else { "a" }
                )
            }
            Opcode::ERET => {
                write!(f, "eret")
            }
            Opcode::HVC => {
                write!(f, "hvc")
            }
            Opcode::BKPT => {
                write!(f, "bkpt")
            }
            Opcode::SMC => {
                write!(f, "smc")
            }
            Opcode::MOVT => {
                write!(f, "movt")
            }
            Opcode::QADD => {
                write!(f, "qadd")
            }
            Opcode::QSUB => {
                write!(f, "qsub")
            }
            Opcode::QDADD => {
                write!(f, "qdadd")
            }
            Opcode::QDSUB => {
                write!(f, "qdsub")
            }
            Opcode::Invalid => {
                write!(f, "invalid")
            }
            Opcode::POP => {
                write!(f, "pop")
            }
            Opcode::PUSH => {
                write!(f, "push")
            }
            Opcode::B => {
                write!(f, "b")
            }
            Opcode::BL => {
                write!(f, "bl")
            }
            Opcode::BLX => {
                write!(f, "blx")
            }
            Opcode::BX => {
                write!(f, "bx")
            }
            Opcode::BXJ => {
                write!(f, "bxj")
            }
            Opcode::CLZ => {
                write!(f, "clz")
            }
            Opcode::AND => {
                write!(f, "and")
            }
            Opcode::EOR => {
                write!(f, "eor")
            }
            Opcode::SUB => {
                write!(f, "sub")
            }
            Opcode::RSB => {
                write!(f, "rsb")
            }
            Opcode::ADD => {
                write!(f, "add")
            }
            Opcode::ADC => {
                write!(f, "adc")
            }
            Opcode::SBC => {
                write!(f, "sbc")
            }
            Opcode::RSC => {
                write!(f, "rsc")
            }
            Opcode::TST => {
                write!(f, "tst")
            }
            Opcode::TEQ => {
                write!(f, "teq")
            }
            Opcode::CMP => {
                write!(f, "cmp")
            }
            Opcode::CMN => {
                write!(f, "cmn")
            }
            Opcode::ORR => {
                write!(f, "orr")
            }
            Opcode::MOV => {
                write!(f, "mov")
            }
            Opcode::MSR => {
                write!(f, "msr")
            }
            Opcode::MRS => {
                write!(f, "mrs")
            }
            Opcode::BIC => {
                write!(f, "bic")
            }
            Opcode::MVN => {
                write!(f, "mvn")
            }
            Opcode::LSL => {
                write!(f, "lsl")
            }
            Opcode::LSR => {
                write!(f, "lsr")
            }
            Opcode::ASR => {
                write!(f, "asr")
            }
            Opcode::RRX => {
                write!(f, "rrx")
            }
            Opcode::ROR => {
                write!(f, "ror")
            }
            Opcode::ADR => {
                write!(f, "adr")
            }
            Opcode::LDREXH => {
                write!(f, "ldrexh")
            }
            Opcode::STREXH => {
                write!(f, "strexh")
            }
            Opcode::LDREXB => {
                write!(f, "ldrexb")
            }
            Opcode::STREXB => {
                write!(f, "strexb")
            }
            Opcode::LDREXD => {
                write!(f, "ldrexd")
            }
            Opcode::STREXD => {
                write!(f, "strexd")
            }
            Opcode::LDREX => {
                write!(f, "ldrex")
            }
            Opcode::STREX => {
                write!(f, "strex")
            }
            Opcode::LDM(false, false, _, _) => {
                write!(f, "ldmda")
            }
            Opcode::LDM(false, true, _, _) => {
                write!(f, "ldmdb")
            }
            // TODO: seems like these are backwards
            Opcode::LDM(true, false, _, _) => {
                write!(f, "ldm")
            }
            Opcode::LDM(true, true, _, _) => {
                write!(f, "ldmia")
            }
            Opcode::STM(false, false, _, _) => {
                write!(f, "stmda")
            }
            Opcode::STM(false, true, _, _) => {
                write!(f, "stmdb")
            }
            // TODO: seems like these are backwards
            Opcode::STM(true, false, _, _) => {
                write!(f, "stm")
            }
            Opcode::STM(true, true, _, _) => {
                write!(f, "stmia")
            }
            Opcode::LDR => {
                write!(f, "ldr")
            }
            Opcode::STR => {
                write!(f, "str")
            }
            Opcode::LDRH => {
                write!(f, "ldrh")
            }
            Opcode::STRH => {
                write!(f, "strh")
            }
            Opcode::LDRB => {
                write!(f, "ldrb")
            }
            Opcode::STRB => {
                write!(f, "strb")
            }
            Opcode::LDRT => {
                write!(f, "ldrt")
            }
            Opcode::STRT => {
                write!(f, "strt")
            }
            Opcode::LDRHT => {
                write!(f, "ldrht")
            }
            Opcode::STRHT => {
                write!(f, "strht")
            }
            Opcode::LDRBT => {
                write!(f, "ldrbt")
            }
            Opcode::STRBT => {
                write!(f, "strbt")
            }
            Opcode::SWP => {
                write!(f, "swp")
            }
            Opcode::SWPB => {
                write!(f, "swpb")
            }
            Opcode::SDIV => {
                write!(f, "sdiv")
            }
            Opcode::UDIV => {
                write!(f, "udiv")
            }
            Opcode::MUL => {
                write!(f, "mul")
            }
            Opcode::MLA => {
                write!(f, "mla")
            }
            Opcode::UMAAL => {
                write!(f, "umaal")
            }
            Opcode::MLS => {
                write!(f, "mls")
            }
            Opcode::UMULL => {
                write!(f, "umull")
            }
            Opcode::UMLAL => {
                write!(f, "umlal")
            }
            Opcode::SMULL => {
                write!(f, "smull")
            }
            Opcode::SMLA(first, second) => {
                write!(
                    f,
                    "smla{}{}",
                    if *first { "t" } else { "b" },
                    if *second { "t" } else { "b" }
                )
            }
            Opcode::SMLAL => {
                write!(f, "smlal")
            }
            Opcode::SMLAL_halfword(first, second) => {
                write!(
                    f,
                    "smlal{}{}",
                    if *first { "t" } else { "b" },
                    if *second { "t" } else { "b" }
                )
            }
            Opcode::SMUL(first, second) => {
                write!(
                    f,
                    "smul{}{}",
                    if *first { "t" } else { "b" },
                    if *second { "t" } else { "b" }
                )
            }
            Opcode::SMAL(first, second) => {
                write!(
                    f,
                    "smal{}{}",
                    if *first { "t" } else { "b" },
                    if *second { "t" } else { "b" }
                )
            }
            Opcode::SMLAW(second) => {
                write!(f, "smlaw{}", if *second { "t" } else { "b" })
            }
            Opcode::SMULW(second) => {
                write!(f, "smulw{}", if *second { "t" } else { "b" })
            }
            Opcode::SMLALD(second) => {
                write!(f, "smlald{}", if *second { "t" } else { "b" })
            }
            Opcode::SMLSLD(second) => {
                write!(f, "smlsld{}", if *second { "t" } else { "b" })
            }
            Opcode::SMLSD => {
                write!(f, "smlsd")
            }
            Opcode::SMMLA => {
                write!(f, "smmla")
            }
            Opcode::SMMLS => {
                write!(f, "smmls")
            }
            Opcode::USADA8 => {
                write!(f, "usada8")
            }
            Opcode::USAD8 => {
                write!(f, "usad8")
            }
            Opcode::SMLAD => {
                write!(f, "smlad")
            }
            Opcode::SMUSD => {
                write!(f, "smusd")
            }
            Opcode::SMMUL => {
                write!(f, "smmul")
            }
            Opcode::SMUAD => {
                write!(f, "smuad")
            }
            Opcode::TBB => {
                write!(f, "tbb")
            }
            Opcode::TBH => {
                write!(f, "tbh")
            }
            Opcode::UDF => {
                write!(f, "udf")
            }
            Opcode::SVC => {
                write!(f, "svc")
            }
            Opcode::WFE => {
                write!(f, "wfe")
            }
            Opcode::WFI => {
                write!(f, "wfi")
            }
            Opcode::SEV => {
                write!(f, "sev")
            }
            Opcode::CSDB => {
                write!(f, "csdb")
            }
            Opcode::YIELD => {
                write!(f, "yield")
            }
            Opcode::HINT => {
                write!(f, "hint")
            }
            Opcode::NOP => {
                write!(f, "nop")
            }
            Opcode::LEAVEX => {
                write!(f, "leavex")
            }
            Opcode::ENTERX => {
                write!(f, "enterx")
            }
            Opcode::CLREX => {
                write!(f, "clrex")
            }
            Opcode::DSB => {
                write!(f, "dsb")
            }
            Opcode::DMB => {
                write!(f, "dmb")
            }
            Opcode::ISB => {
                write!(f, "isb")
            }
            Opcode::SXTH => {
                write!(f, "sxth")
            }
            Opcode::UXTH => {
                write!(f, "uxth")
            }
            Opcode::SXTB16 => {
                write!(f, "sxtb16")
            }
            Opcode::UXTB16 => {
                write!(f, "uxtb16")
            }
            Opcode::SXTB => {
                write!(f, "sxtb")
            }
            Opcode::UXTB => {
                write!(f, "uxtb")
            }
            Opcode::SXTAH => {
                write!(f, "sxtah")
            }
            Opcode::UXTAH => {
                write!(f, "uxtah")
            }
            Opcode::SXTAB16 => {
                write!(f, "sxtab16")
            }
            Opcode::UXTAB16 => {
                write!(f, "uxtab16")
            }
            Opcode::SXTAB => {
                write!(f, "sxtab")
            }
            Opcode::UXTAB => {
                write!(f, "uxtab")
            }
            Opcode::CBZ => {
                write!(f, "cbz")
            }
            Opcode::CBNZ => {
                write!(f, "cbnz")
            }
            Opcode::SETEND => {
                write!(f, "setend")
            }
            Opcode::CPS(disable) => {
                write!(f, "cps{}", if *disable { "id" } else { "ie" })
            }
            Opcode::CPS_modeonly => {
                write!(f, "cps")
            }
            Opcode::REV => {
                write!(f, "rev")
            }
            Opcode::REV16 => {
                write!(f, "rev16")
            }
            Opcode::REVSH => {
                write!(f, "revsh")
            }
            Opcode::IT => {
                write!(f, "it")
            }
            Opcode::PKHTB => {
                write!(f, "pkhtb")
            }
            Opcode::PKHBT => {
                write!(f, "pkhbt")
            }
            Opcode::ORN => {
                write!(f, "orn")
            }
            Opcode::SSAT => {
                write!(f, "ssat")
            }
            Opcode::SSAT16 => {
                write!(f, "ssat16")
            }
            Opcode::SBFX => {
                write!(f, "sbfx")
            }
            Opcode::USAT => {
                write!(f, "usat")
            }
            Opcode::USAT16 => {
                write!(f, "usat16")
            }
            Opcode::UBFX => {
                write!(f, "ubfx")
            }
            Opcode::BFI => {
                write!(f, "bfi")
            }
            Opcode::BFC => {
                write!(f, "bfc")
            }
            Opcode::DBG => {
                write!(f, "dbg")
            }
            Opcode::PLD => {
                write!(f, "pld")
            }
            Opcode::PLI => {
                write!(f, "pli")
            }
            Opcode::RBIT => {
                write!(f, "rbit")
            }
            Opcode::SEL => {
                write!(f, "sel")
            }
            Opcode::SADD16 => {
                write!(f, "sadd16")
            }
            Opcode::QADD16 => {
                write!(f, "qadd16")
            }
            Opcode::SHADD16 => {
                write!(f, "shadd16")
            }
            Opcode::SASX => {
                write!(f, "sasx")
            }
            Opcode::QASX => {
                write!(f, "qasx")
            }
            Opcode::SHASX => {
                write!(f, "shasx")
            }
            Opcode::SSAX => {
                write!(f, "ssax")
            }
            Opcode::QSAX => {
                write!(f, "qsax")
            }
            Opcode::SHSAX => {
                write!(f, "shsax")
            }
            Opcode::SSUB16 => {
                write!(f, "ssub16")
            }
            Opcode::QSUB16 => {
                write!(f, "qsub16")
            }
            Opcode::SHSUB16 => {
                write!(f, "shsub16")
            }
            Opcode::SADD8 => {
                write!(f, "sadd8")
            }
            Opcode::QADD8 => {
                write!(f, "qadd8")
            }
            Opcode::SHADD8 => {
                write!(f, "shadd8")
            }
            Opcode::SSUB8 => {
                write!(f, "ssub8")
            }
            Opcode::QSUB8 => {
                write!(f, "qsub8")
            }
            Opcode::SHSUB8 => {
                write!(f, "shsub8")
            }
            Opcode::UADD16 => {
                write!(f, "uadd16")
            }
            Opcode::UQADD16 => {
                write!(f, "uqadd16")
            }
            Opcode::UHADD16 => {
                write!(f, "uhadd16")
            }
            Opcode::UASX => {
                write!(f, "uasx")
            }
            Opcode::UQASX => {
                write!(f, "uqasx")
            }
            Opcode::UHASX => {
                write!(f, "uhasx")
            }
            Opcode::USAX => {
                write!(f, "usax")
            }
            Opcode::UQSAX => {
                write!(f, "uqsax")
            }
            Opcode::UHSAX => {
                write!(f, "uhsax")
            }
            Opcode::USUB16 => {
                write!(f, "usub16")
            }
            Opcode::UQSUB16 => {
                write!(f, "uqsub16")
            }
            Opcode::UHSUB16 => {
                write!(f, "uhsub16")
            }
            Opcode::UADD8 => {
                write!(f, "uadd8")
            }
            Opcode::UQADD8 => {
                write!(f, "uqadd8")
            }
            Opcode::UHADD8 => {
                write!(f, "uhadd8")
            }
            Opcode::USUB8 => {
                write!(f, "usub8")
            }
            Opcode::UQSUB8 => {
                write!(f, "uqsub8")
            }
            Opcode::UHSUB8 => {
                write!(f, "uhsub8")
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[allow(missing_docs)]
pub enum Opcode {
    Invalid,
    /*
     * These two don't really have direct encodings, but are for the specific instances
     * where the semantics of the original instruction are the same as push (specifically
     * ldm/stm/mov that write to the stack and increment/decrement appropriately
     */
    POP,
    PUSH,

    B,
    BL,
    BLX,
    BX,
    BXJ,
    AND,
    EOR,
    SUB,
    RSB,
    ADD,
    ADC,
    SBC,
    RSC,
    TST,
    TEQ,
    CMP,
    CMN,
    ORR,
    MOV,
    BIC,
    MVN,
    LSL,
    LSR,
    ASR,
    RRX,
    ROR,
    ADR,
    MSR,
    MRS,
    CLZ,
    LDREXH,
    STREXH,
    LDREXB,
    STREXB,
    LDREXD,
    STREXD,
    LDREX,
    STREX,
    LDM(bool, bool, bool, bool),
    STM(bool, bool, bool, bool),
    LDR,
    STR,
    LDRH,
    STRH,
    LDRB,
    STRB,
    LDRSH,
    LDRSHT,
    LDRSB,
    LDRSBT,
    STRD,
    LDRD,
    LDC(u8),
    LDCL(u8),
    LDC2(u8),
    LDC2L(u8),
    STC(u8),
    STCL(u8),
    STC2(u8),
    STC2L(u8),
    MCRR2(u8, u8),
    MCR2(u8, u8, u8),
    MRRC2(u8, u8),
    MCRR(u8, u8),
    MRRC(u8, u8),
    MRC2(u8, u8, u8),
    CDP2(u8, u8, u8),
    SRS(bool, bool),
    RFE(bool, bool),
    LDRT,
    STRT,
    LDRHT,
    STRHT,
    LDRBT,
    STRBT,
    SWP,
    SWPB,
    MUL,
    MLA,
    UMAAL,
    MLS,
    UMULL,
    UMLAL,
    SMULL,
    SMUL(bool, bool),
    SMLA(bool, bool),
    SMLAL,
    SMLAL_halfword(bool, bool),
    SMAL(bool, bool),
    SMLAW(bool),
    ERET,
    BKPT,
    HVC,
    SMC,
    MOVT,
    QDSUB,
    QDADD,
    QSUB,
    QADD,

    TBB,
    TBH,
    UDF,
    SVC,
    WFE,
    WFI,
    SEV,
    CSDB,
    YIELD,
    HINT,
    NOP,
    LEAVEX,
    ENTERX,
    CLREX,
    DSB,
    DMB,
    ISB,
    SXTH,
    UXTH,
    SXTB16,
    UXTB16,
    SXTB,
    UXTB,
    SXTAH,
    UXTAH,
    SXTAB16,
    UXTAB16,
    SXTAB,
    UXTAB,
    CBZ,
    CBNZ,
    SETEND,
    CPS(bool),
    CPS_modeonly,
    REV,
    REV16,
    REVSH,
    IT,

    PKHTB,
    PKHBT,
    ORN,
    SSAT,
    SSAT16,
    SBFX,
    USAT,
    USAT16,
    UBFX,
    BFI,
    BFC,
    DBG,
    PLD,
    PLI,
    RBIT,
    SEL,

    SADD16,
    QADD16,
    SHADD16,
    SASX,
    QASX,
    SHASX,
    SSAX,
    QSAX,
    SHSAX,
    SSUB16,
    QSUB16,
    SHSUB16,
    SADD8,
    QADD8,
    SHADD8,
    SSUB8,
    QSUB8,
    SHSUB8,
    UADD16,
    UQADD16,
    UHADD16,
    UASX,
    UQASX,
    UHASX,
    USAX,
    UQSAX,
    UHSAX,
    USUB16,
    UQSUB16,
    UHSUB16,
    UADD8,
    UQADD8,
    UHADD8,
    USUB8,
    UQSUB8,
    UHSUB8,

    SMLSD,
    SMMLA,
    SMMLS,
    USADA8,
    USAD8,
    SMLAD,
    SMUSD,
    SMMUL,
    SMULW(bool),
    SMUAD,
    SDIV,
    UDIV,
    SMLALD(bool),
    SMLSLD(bool),
}

static DATA_PROCESSING_OPCODES: [Opcode; 16] = [
    Opcode::AND,
    Opcode::EOR,
    Opcode::SUB,
    Opcode::RSB,
    Opcode::ADD,
    Opcode::ADC,
    Opcode::SBC,
    Opcode::RSC,
    Opcode::TST,
    Opcode::TEQ,
    Opcode::CMP,
    Opcode::CMN,
    Opcode::ORR,
    Opcode::MOV,
    Opcode::BIC,
    Opcode::MVN,
];

/// a struct describiing a shifted register operand. this is primarily interesting in that it can
/// be translated to a `RegShiftStyle` for further interpretation.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[repr(transparent)]
pub struct RegShift {
    data: u16,
}

impl RegShift {
    /// convert an instruction's `RegShift` operand into something more appropriate for
    /// programmatic use.
    pub fn into_shift(&self) -> RegShiftStyle {
        if self.data & 0b10000 == 0 {
            RegShiftStyle::RegImm(RegImmShift { data: self.data })
        } else {
            RegShiftStyle::RegReg(RegRegShift { data: self.data })
        }
    }

    /// don't use this. it's for armv7 testing only.
    #[doc(hidden)]
    pub fn from_raw(data: u16) -> Self {
        RegShift { data }
    }
}

/// an enum describing one of two ways a shifted register operand may be shifted.
pub enum RegShiftStyle {
    /// a register shifted by an immediate.
    RegImm(RegImmShift),
    /// a register shifted by a register.
    RegReg(RegRegShift),
}

/// a register shifted by a register.
#[repr(transparent)]
pub struct RegRegShift {
    data: u16,
}

/// the way a shift operation is carried out.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ShiftStyle {
    /// left-shift the value, filling in zeroes.
    LSL = 0,
    /// right-shift the value, filling in zeroes.
    LSR = 1,
    /// arithmetic shift right, filling with the top bit of the value (sign-extending).
    ASR = 2,
    /// rotate-right, filling with bits shifted out of the value.
    ROR = 3,
}

impl ShiftStyle {
    fn as_str(&self) -> &'static str {
        match self {
            ShiftStyle::LSL => "lsl",
            ShiftStyle::LSR => "lsr",
            ShiftStyle::ASR => "asr",
            ShiftStyle::ROR => "ror",
        }
    }

    fn from(bits: u8) -> ShiftStyle {
        match bits {
            0b00 => ShiftStyle::LSL,
            0b01 => ShiftStyle::LSR,
            0b10 => ShiftStyle::ASR,
            0b11 => ShiftStyle::ROR,
            _ => unreachable!("bad ShiftStyle index"),
        }
    }
}

impl RegRegShift {
    /// the general-purpose register, an amount to shift the shiftee.
    pub fn shifter(&self) -> Reg {
        Reg::from_u8((self.data >> 8) as u8 & 0b1111)
    }
    /// the way in which this register is shifted.
    pub fn stype(&self) -> ShiftStyle {
        ShiftStyle::from((self.data >> 5) as u8 & 0b11)
    }
    /// the general-purpose register to be shifted.
    pub fn shiftee(&self) -> Reg {
        Reg::from_u8(self.data as u8 & 0b1111)
    }
}

/// a register shifted by an immediate.
#[repr(transparent)]
pub struct RegImmShift {
    data: u16,
}

impl RegImmShift {
    /// the immediate this register is shifted by.
    pub fn imm(&self) -> u8 {
        (self.data >> 7) as u8 & 0b11111
    }
    /// the way in which this register is shifted.
    pub fn stype(&self) -> ShiftStyle {
        ShiftStyle::from((self.data >> 5) as u8 & 0b11)
    }
    /// the general-purpose register to be shifted.
    pub fn shiftee(&self) -> Reg {
        Reg::from_u8(self.data as u8 & 0b1111)
    }
}

/// a struct describing an `arm` register.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct Reg {
    bits: u8,
}

impl Reg {
    #[allow(non_snake_case)]
    fn from_sysm(R: bool, M: u8) -> Option<Operand> {
        /*
         * Is one of:
         * • <Rm>_<mode>, encoded with R==0.
         * • ELR_hyp, encoded with R==0.
         * • SPSR_<mode>, encoded with R==1.
         * For a full description of the encoding of this field, see Encoding and use of Banked register
         * transfer
         * instructions on page B9-1959.
         * */
        if R == false {
            [
                Some(Operand::BankedReg(Bank::Usr, Reg::from_u8(8))),
                Some(Operand::BankedReg(Bank::Usr, Reg::from_u8(9))),
                Some(Operand::BankedReg(Bank::Usr, Reg::from_u8(10))),
                Some(Operand::BankedReg(Bank::Usr, Reg::from_u8(11))),
                Some(Operand::BankedReg(Bank::Usr, Reg::from_u8(12))),
                Some(Operand::BankedReg(Bank::Usr, Reg::from_u8(13))),
                Some(Operand::BankedReg(Bank::Usr, Reg::from_u8(14))),
                None,
                Some(Operand::BankedReg(Bank::Fiq, Reg::from_u8(8))),
                Some(Operand::BankedReg(Bank::Fiq, Reg::from_u8(9))),
                Some(Operand::BankedReg(Bank::Fiq, Reg::from_u8(10))),
                Some(Operand::BankedReg(Bank::Fiq, Reg::from_u8(11))),
                Some(Operand::BankedReg(Bank::Fiq, Reg::from_u8(12))),
                Some(Operand::BankedReg(Bank::Fiq, Reg::from_u8(13))),
                Some(Operand::BankedReg(Bank::Fiq, Reg::from_u8(14))),
                None,
                Some(Operand::BankedReg(Bank::Irq, Reg::from_u8(13))),
                Some(Operand::BankedReg(Bank::Irq, Reg::from_u8(14))),
                Some(Operand::BankedReg(Bank::Svc, Reg::from_u8(13))),
                Some(Operand::BankedReg(Bank::Svc, Reg::from_u8(14))),
                Some(Operand::BankedReg(Bank::Abt, Reg::from_u8(13))),
                Some(Operand::BankedReg(Bank::Abt, Reg::from_u8(14))),
                Some(Operand::BankedReg(Bank::Und, Reg::from_u8(13))),
                Some(Operand::BankedReg(Bank::Und, Reg::from_u8(14))),
                None,
                None,
                None,
                None,
                Some(Operand::BankedReg(Bank::Mon, Reg::from_u8(13))),
                Some(Operand::BankedReg(Bank::Mon, Reg::from_u8(14))),
                Some(Operand::BankedReg(Bank::Hyp, Reg::from_u8(13))),
                Some(Operand::BankedReg(Bank::Hyp, Reg::from_u8(14))),
            ][M as usize]
        } else {
            if M == 0b01110 {
                Some(Operand::BankedSPSR(Bank::Fiq))
            } else if M == 0b10000 {
                Some(Operand::BankedSPSR(Bank::Irq))
            } else if M == 0b10010 {
                Some(Operand::BankedSPSR(Bank::Svc))
            } else if M == 0b10100 {
                Some(Operand::BankedSPSR(Bank::Abt))
            } else if M == 0b10110 {
                Some(Operand::BankedSPSR(Bank::Und))
            } else if M == 0b11100 {
                Some(Operand::BankedSPSR(Bank::Mon))
            } else if M == 0b11110 {
                Some(Operand::BankedSPSR(Bank::Hyp))
            } else {
                None
            }
        }
    }

    /// create a new `Reg` with the specified number.
    ///
    /// panics if `bits` is out of range (16 or above).
    pub fn from_u8(bits: u8) -> Reg {
        if bits > 0b1111 {
            panic!("register number out of range");
        }

        Reg { bits }
    }

    /// get the number of this register. the returned value will be between 0 and 15.
    pub fn number(&self) -> u8 {
        self.bits
    }

    pub fn as_str(&self) -> &'static str {
        match self.number() {
            0 => "r0",
            1 => "r1",
            2 => "r2",
            3 => "r3",
            4 => "r4",
            5 => "r5",
            6 => "r6",
            7 => "r7",
            8 => "r8",
            9 => "sb",
            10 => "r10",
            11 => "fp",
            12 => "ip",
            13 => "sp",
            14 => "lr",
            15 => "pc",
            _ => unreachable!(),
        }
    }
}

/// a control register.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct CReg {
    bits: u8,
}

impl CReg {
    fn as_str(&self) -> &'static str {
        const LOOKUP: &[&str] = &[
            "c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "c13",
            "c14", "c15"
        ];

        LOOKUP[self.bits as usize]
    }
}

impl CReg {
    /// create a new `CReg` with the specified number.
    ///
    /// panics if `bits` is out of range (16 or above).
    pub fn from_u8(bits: u8) -> CReg {
        if bits > 0b1111 {
            panic!("register number out of range");
        }

        CReg { bits }
    }

    /// get the number of this register. the returned value will be between 0 and 15.
    pub fn number(&self) -> u8 {
        self.bits
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[allow(missing_docs)]
pub enum StatusRegMask {
    // Note 0b0000 is unused (as is 0b10000)
    CPSR_C = 0b0001,
    CPSR_X = 0b0010,
    CPSR_XC = 0b0011,
    APSR_G = 0b0100,
    CPSR_SC = 0b0101,
    CPSR_SX = 0b0110,
    CPSR_SXC = 0b0111,
    APSR_NZCVQ = 0b1000,
    CPSR_FC = 0b1001,
    CPSR_FX = 0b1010,
    CPSR_FXC = 0b1011,
    APSR_NZCVQG = 0b1100,
    CPSR_FSC = 0b1101,
    CPSR_FSX = 0b1110,
    CPSR_FSXC = 0b1111,
    SPSR = 0b10000,
    SPSR_C = 0b10001,
    SPSR_X = 0b10010,
    SPSR_XC = 0b10011,
    SPSR_S = 0b10100,
    SPSR_SC = 0b10101,
    SPSR_SX = 0b10110,
    SPSR_SXC = 0b10111,
    SPSR_F = 0b11000,
    SPSR_FC = 0b11001,
    SPSR_FX = 0b11010,
    SPSR_FXC = 0b11011,
    SPSR_FS = 0b11100,
    SPSR_FSC = 0b11101,
    SPSR_FSX = 0b11110,
    SPSR_FSXC = 0b11111,
}

impl StatusRegMask {
    fn from_raw(raw: u8) -> Result<StatusRegMask, ErrorKind> {
        if raw == 0 {
            // invalid status reg mask value
            return Err(ErrorKind::InvalidOperand);
        }
        Ok([
            StatusRegMask::CPSR_C, // actually unreachable
            StatusRegMask::CPSR_C,
            StatusRegMask::CPSR_X,
            StatusRegMask::CPSR_XC,
            StatusRegMask::APSR_G,
            StatusRegMask::CPSR_SC,
            StatusRegMask::CPSR_SX,
            StatusRegMask::CPSR_SXC,
            StatusRegMask::APSR_NZCVQ,
            StatusRegMask::CPSR_FC,
            StatusRegMask::CPSR_FX,
            StatusRegMask::CPSR_FXC,
            StatusRegMask::APSR_NZCVQG,
            StatusRegMask::CPSR_FSC,
            StatusRegMask::CPSR_FSX,
            StatusRegMask::CPSR_FSXC,
            StatusRegMask::SPSR,
            StatusRegMask::SPSR_C,
            StatusRegMask::SPSR_X,
            StatusRegMask::SPSR_XC,
            StatusRegMask::SPSR_S,
            StatusRegMask::SPSR_SC,
            StatusRegMask::SPSR_SX,
            StatusRegMask::SPSR_SXC,
            StatusRegMask::SPSR_F,
            StatusRegMask::SPSR_FC,
            StatusRegMask::SPSR_FX,
            StatusRegMask::SPSR_FXC,
            StatusRegMask::SPSR_FS,
            StatusRegMask::SPSR_FSC,
            StatusRegMask::SPSR_FSX,
            StatusRegMask::SPSR_FSXC,
        ][raw as usize])
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            StatusRegMask::CPSR_C => "cpsr_c",
            StatusRegMask::CPSR_X => "cpsr_x",
            StatusRegMask::CPSR_XC => "cpsr_xc",
            StatusRegMask::APSR_G => "apsr_g",
            StatusRegMask::CPSR_SC => "cpsr_sc",
            StatusRegMask::CPSR_SX => "cpsr_sx",
            StatusRegMask::CPSR_SXC => "cpsr_sxc",
            StatusRegMask::APSR_NZCVQ => "apsr_nzcvq",
            StatusRegMask::CPSR_FC => "cpsr_fc",
            StatusRegMask::CPSR_FX => "cpsr_fx",
            StatusRegMask::CPSR_FXC => "cpsr_fxc",
            StatusRegMask::APSR_NZCVQG => "apsr_nzcvqg",
            StatusRegMask::CPSR_FSC => "cpsr_fsc",
            StatusRegMask::CPSR_FSX => "cpsr_fsx",
            StatusRegMask::CPSR_FSXC => "cpsr_fsxc",
            StatusRegMask::SPSR => "spsr",
            StatusRegMask::SPSR_C => "spsr_c",
            StatusRegMask::SPSR_X => "spsr_x",
            StatusRegMask::SPSR_XC => "spsr_xc",
            StatusRegMask::SPSR_S => "spsr_s",
            StatusRegMask::SPSR_SC => "spsr_sc",
            StatusRegMask::SPSR_SX => "spsr_sx",
            StatusRegMask::SPSR_SXC => "spsr_sxc",
            StatusRegMask::SPSR_F => "spsr_f",
            StatusRegMask::SPSR_FC => "spsr_fc",
            StatusRegMask::SPSR_FX => "spsr_fx",
            StatusRegMask::SPSR_FXC => "spsr_fxc",
            StatusRegMask::SPSR_FS => "spsr_fs",
            StatusRegMask::SPSR_FSC => "spsr_fsc",
            StatusRegMask::SPSR_FSX => "spsr_fsx",
            StatusRegMask::SPSR_FSXC => "spsr_fsxc",
        }
    }
}

/// an operand in an `arm` instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operand {
    /// a general-purpose register.
    Reg(Reg),
    /// a general-purpose register, with writeback. these generally imply an increment by width of
    /// a memort operand, depending on the instruction.
    RegWBack(Reg, bool),
    /// a list of registers specified as a bitmask from bits 0 to 15.
    RegList(u16),
    /// a memory access, dereferencing a general-purpose register.
    RegDeref(Reg),
    /// a memory access, dereferencing a shifted general-purpose register with register or
    /// immediate offset.
    RegShift(RegShift),
    /// a memory access of a register, post-indexed a register shifted by register or immediate.
    /// the first bool indicates if the shifted-register is added or subtracted ot the base
    /// register, while the second bool indicates if the resulting address is written back to the
    /// base register.
    RegDerefPostindexRegShift(Reg, RegShift, bool, bool), // add/sub, wback
    /// a memory access of a register, pre-indexed with a register shifted by register or
    /// immediate. the first bool indicates if the shifted-register is added or subtracted ot the
    /// base register, while the second bool indicates if the resulting address is written back to
    /// the base register.
    RegDerefPreindexRegShift(Reg, RegShift, bool, bool), // add/sub, wback
    /// a memory access of a register, post-indexed with an immediate. the first bool indicates if
    /// the shifted-register is added or subtracted ot the base register, while the second bool
    /// indicates if the resulting address is written back to the base register.
    RegDerefPostindexOffset(Reg, u16, bool, bool), // add/sub, wback
    /// a memory access of a register, pre-indexed with an immediate. the first bool indicates if
    /// the shifted-register is added or subtracted ot the base register, while the second bool
    /// indicates if the resulting address is written back to the base register.
    RegDerefPreindexOffset(Reg, u16, bool, bool), // add/sub, wback
    /// a memory access of a register, post-indexed with a register. the first bool indicates if the
    /// shifted-register is added or subtracted ot the base register, while the second bool
    /// indicates if the resulting address is written back to the base register.
    RegDerefPostindexReg(Reg, Reg, bool, bool), // add/sub, wback
    /// a memory access of a register, pre-indexed with a register. the first bool indicates if the
    /// shifted-register is added or subtracted ot the base register, while the second bool
    /// indicates if the resulting address is written back to the base register.
    RegDerefPreindexReg(Reg, Reg, bool, bool), // add/sub, wback
    /// a 12-bit immediate, stored in a `u16`.
    Imm12(u16),
    /// a 32-bit immediate, stored in a `u32`.
    Imm32(u32),
    /// a pc-relative branch, with 32-bit signed offset, left-shifted by 2.
    BranchOffset(i32),
    /// a pc-relative branch, with 32-bit signed offset, left-shifted by 1.
    BranchThumbOffset(i32),
    /// a coprocessor index.
    Coprocessor(u8),
    /// a coprocessor option number.
    CoprocOption(u8),
    /// an `arm` control register.
    CReg(CReg),
    /// an `arm` banked register, either `usr` (general-purpose) bank or one of the alternate sets
    /// of `arm` registers.
    BankedReg(Bank, Reg),
    /// `spsr` in some `arm` register bank.
    BankedSPSR(Bank),
    /// a mask of bits for the `spsr` register.
    StatusRegMask(StatusRegMask),
    /// the `apsr` register.
    APSR,
    /// the `spsr` register.
    SPSR,
    /// the `cpsr` register.
    CPSR,
    /// "no operand". since an instruction's `operands` array is always four entries, this is used
    /// to fill space, if any, after recording an instruction's extant operands.
    Nothing,
}

/// a register bank for a register in `armv7` or below.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Bank {
    Usr,
    Fiq,
    Irq,
    Svc,
    Abt,
    Und,
    Mon,
    Hyp,
}

impl Bank {
    fn as_str(&self) -> &'static str {
        match self {
            Bank::Usr => "usr",
            Bank::Fiq => "fiq",
            Bank::Irq => "irq",
            Bank::Svc => "svc",
            Bank::Abt => "abt",
            Bank::Und => "und",
            Bank::Mon => "mon",
            Bank::Hyp => "hyp",
        }
    }
}

impl Operand {
    fn tokenize(&self, stream: &mut TokenStream, _symbols: &Index, _imm_override: Option<usize>) {
        match self {
            Operand::RegList(list) => format_reg_list(stream, *list),
            Operand::BankedReg(bank, reg) => {
                stream.push(reg.as_str(), Colors::register());
                stream.push("_", Colors::expr());
                stream.push(bank.as_str(), Colors::register());
            }
            Operand::BankedSPSR(bank) => {
                stream.push("spsr", Colors::register());
                stream.push("_", Colors::expr());
                stream.push(bank.as_str(), Colors::register());
            }
            Operand::Reg(reg) => {
                stream.push(reg.as_str(), Colors::register());
            }
            Operand::RegDeref(reg) => {
                stream.push("[", Colors::brackets());
                stream.push(reg.as_str(), Colors::register());
                stream.push("]", Colors::brackets());
            }
            Operand::RegShift(shift) => format_shift(stream, *shift),
            Operand::RegDerefPostindexRegShift(reg, shift, add, wback) => {
                format_reg_shift_mem(stream, *reg, *shift, *add, false, *wback)
            }
            Operand::RegDerefPreindexRegShift(reg, shift, add, wback) => {
                format_reg_shift_mem(stream, *reg, *shift, *add, true, *wback)
            }
            Operand::RegDerefPostindexOffset(reg, offs, add, wback) => {
                format_reg_imm_mem(stream, *reg, *offs, *add, false, *wback)
            }
            Operand::RegDerefPreindexOffset(reg, offs, add, wback) => {
                format_reg_imm_mem(stream, *reg, *offs, *add, true, *wback)
            }
            Operand::RegDerefPostindexReg(reg, offsreg, add, wback) => {
                stream.push("[", Colors::brackets());
                stream.push(reg.as_str(), Colors::register());
                stream.push("]", Colors::brackets());
                stream.push(", ", Colors::expr());

                if !*add {
                    stream.push("-", Colors::expr());
                }

                stream.push(offsreg.as_str(), Colors::register());

                if *wback {
                    stream.push("!", Colors::expr());
                }
            }
            Operand::RegDerefPreindexReg(reg, offsreg, add, wback) => {
                stream.push("[", Colors::brackets());
                stream.push(reg.as_str(), Colors::register());
                stream.push(", ", Colors::expr());

                if !*add {
                    stream.push("-", Colors::expr());
                }

                stream.push(offsreg.as_str(), Colors::register());
                stream.push("]", Colors::brackets());

                if *wback {
                    stream.push("!", Colors::expr());
                }
            }
            Operand::Imm12(imm) => {
                stream.push_owned(decoder::encode_hex(*imm as i64), Colors::immediate());
            }
            Operand::Imm32(imm) => {
                stream.push_owned(decoder::encode_hex(*imm as i64), Colors::immediate());
            }
            Operand::BranchOffset(imm) => {
                if *imm >= 0 {
                    stream.push("$+", Colors::immediate());
                } else {
                    stream.push("$", Colors::immediate());
                }
                stream.push_owned(decoder::encode_hex((imm * 4) as i64), Colors::immediate());
            }
            Operand::BranchThumbOffset(imm) => {
                if *imm >= 0 {
                    stream.push("$+", Colors::immediate());
                } else {
                    stream.push("$", Colors::immediate());
                }
                stream.push_owned(decoder::encode_hex((imm * 2) as i64), Colors::immediate());
            }
            Operand::Coprocessor(num) => {
                stream.push("p", Colors::register());
                stream.push_owned(num.to_string(), Colors::register());
            }
            Operand::CoprocOption(num) => {
                stream.push("{", Colors::brackets());
                stream.push_owned(decoder::encode_hex(*num as i64), Colors::register());
                stream.push("}", Colors::brackets());
            }
            Operand::RegWBack(reg, wback) => {
                stream.push(reg.as_str(), Colors::register());

                if *wback {
                    stream.push("!", Colors::expr());
                }
            }
            Operand::CReg(creg) => stream.push(creg.as_str(), Colors::register()),
            Operand::StatusRegMask(mask) => stream.push(mask.as_str(), Colors::register()),
            Operand::APSR => stream.push("apsr", Colors::register()),
            Operand::SPSR => stream.push("spsr", Colors::register()),
            Operand::CPSR => stream.push("cpsr", Colors::register()),
            Operand::Nothing => panic!("tried to print Nothing operand"),
        }
    }
}

/// a `armv7` or below instruction.
#[derive(Debug, PartialEq, Eq)]
pub struct Instruction {
    /// the condition code for this instruction, defaults to `AL` if the instruction is
    /// unconditional.
    pub condition: ConditionCode,
    /// the opcode of this instruction.
    pub opcode: Opcode,
    /// operands for the decoded instruction. operands are populated from index 0, to 1, 2, and 3.
    /// operands from the instruction are non-`Operand::Nothing`.
    pub operands: [Operand; 4],
    /// does this instruction update flags, while variants that do not update flags exist?
    pub s: bool,
    /// is this a 32-bit thumb instruction?
    pub wide: bool,
    /// and if it is a 32-bit thumb instruction, should the .w suffix be shown?
    pub thumb_w: bool,
    /// and generally speaking, was this just a thumb-encoded instruction?
    pub thumb: bool,
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction {
            condition: ConditionCode::AL,
            opcode: Opcode::Invalid,
            operands: [
                Operand::Nothing,
                Operand::Nothing,
                Operand::Nothing,
                Operand::Nothing,
            ],
            s: false,
            thumb_w: false,
            wide: false,
            thumb: false,
        }
    }
}

impl Instruction {
    fn set_s(&mut self, value: bool) {
        self.s = value;
    }
    /// does this instruction set status flags?
    pub fn s(&self) -> bool {
        self.s
    }
    pub(crate) fn set_w(&mut self, value: bool) {
        self.thumb_w = value;
    }
    /// was this instruction encoded in `thumb` mode and still 4 bytes, *and* requires a `.w`
    /// suffix on the opcode?
    pub fn w(&self) -> bool {
        self.thumb_w
    }
    pub(crate) fn set_wide(&mut self, value: bool) {
        self.wide = value;
    }
    /// was this instruction encoded in `thumb` mode and still 4 bytes?
    pub fn wide(&self) -> bool {
        self.wide
    }
    pub(crate) fn set_thumb(&mut self, value: bool) {
        self.thumb = value;
    }
    /// was this instruction encoded in `thumb` mode?
    pub fn thumb(&self) -> bool {
        self.thumb
    }
}

fn format_reg_list(stream: &mut TokenStream, mut list: u16) {
    stream.push("{", Colors::brackets());
    let mut i = 0;
    let mut tail = false;
    while i < 16 {
        let present = (list & 1) == 1;
        if present {
            if tail {
                stream.push(", ", Colors::expr());
            } else {
                tail = true;
            }
            stream.push(Reg::from_u8(i).as_str(), Colors::register());
        }
        i += 1;
        list >>= 1;
    }
    stream.push("}", Colors::brackets());
}

fn format_shift(stream: &mut TokenStream, shift: RegShift) {
    match shift.into_shift() {
        RegShiftStyle::RegImm(imm_shift) => {
            if imm_shift.imm() == 0 && imm_shift.stype() == ShiftStyle::LSL {
                stream.push(imm_shift.shiftee().as_str(), Colors::register());
            } else {
                stream.push(imm_shift.shiftee().as_str(), Colors::register());
                stream.push(", ", Colors::expr());
                stream.push(imm_shift.stype().as_str(), Colors::segment());
                stream.push(" ", Colors::expr());
                stream.push_owned(imm_shift.imm().to_string(), Colors::immediate());
            }
        }
        RegShiftStyle::RegReg(reg_shift) => {
            stream.push(reg_shift.shiftee().as_str(), Colors::register());
            stream.push(", ", Colors::expr());
            stream.push(reg_shift.stype().as_str(), Colors::segment());
            stream.push(" ", Colors::expr());
            stream.push(reg_shift.shifter().as_str(), Colors::register());
        }
    }
}

fn format_reg_shift_mem(
    stream: &mut TokenStream,
    rd: Reg,
    shift: RegShift,
    add: bool,
    pre: bool,
    wback: bool,
) {
    let op = if add { "" } else { "-" };

    match (pre, wback) {
        (true, true) => {
            stream.push("[", Colors::brackets());
            stream.push(rd.as_str(), Colors::register());
            stream.push(", ", Colors::expr());
            stream.push(rd.as_str(), Colors::register());
            stream.push(op, Colors::immediate());
            format_shift(stream, shift);
            stream.push("]", Colors::brackets());
            stream.push("!", Colors::expr());
        }

        (true, false) => {
            stream.push("[", Colors::brackets());
            stream.push(rd.as_str(), Colors::register());
            stream.push(", ", Colors::expr());
            stream.push(rd.as_str(), Colors::register());
            stream.push(op, Colors::immediate());
            format_shift(stream, shift);
            stream.push("]", Colors::brackets());
        }
        (false, true) => {
            unreachable!(
                "preindex with writeback is not an ARM addressing mode.\
                          this is a decoder bug."
            );
        }
        (false, false) => {
            stream.push("[", Colors::brackets());
            stream.push(rd.as_str(), Colors::register());
            stream.push("]", Colors::brackets());
            stream.push(", ", Colors::expr());
            stream.push(op, Colors::immediate());
            format_shift(stream, shift)
        }
    }
}

fn format_reg_imm_mem(
    stream: &mut TokenStream,
    rn: Reg,
    imm: u16,
    add: bool,
    pre: bool,
    wback: bool,
) {
    if imm != 0 {
        match (pre, wback) {
            (true, true) => {
                stream.push("[", Colors::brackets());
                stream.push(rn.as_str(), Colors::register());
                stream.push(", ", Colors::expr());
                if add {
                    stream.push_owned(decoder::encode_hex(imm as i64), Colors::immediate());
                } else {
                    stream.push_owned(decoder::encode_hex(imm as i64 * -1), Colors::immediate());
                }
                stream.push("]", Colors::brackets());
                stream.push("!", Colors::expr());
            }
            (true, false) => {
                stream.push("[", Colors::brackets());
                stream.push(rn.as_str(), Colors::register());
                stream.push(", ", Colors::expr());
                if add {
                    stream.push_owned(decoder::encode_hex(imm as i64), Colors::immediate());
                } else {
                    stream.push_owned(decoder::encode_hex(imm as i64 * -1), Colors::immediate());
                }
                stream.push("]", Colors::brackets());
            }
            (false, _) => {
                stream.push("[", Colors::brackets());
                stream.push(rn.as_str(), Colors::register());
                stream.push("]", Colors::brackets());
                stream.push(", ", Colors::expr());
                if add {
                    stream.push_owned(decoder::encode_hex(imm as i64), Colors::immediate());
                } else {
                    stream.push_owned(decoder::encode_hex(imm as i64 * -1), Colors::immediate());
                }
            }
        }
    } else {
        match (pre, wback) {
            (true, true) => {
                stream.push("[", Colors::brackets());
                stream.push(rn.as_str(), Colors::register());
                stream.push("]", Colors::brackets());
                stream.push("!", Colors::expr());
            }
            (true, false) => {
                stream.push("[", Colors::brackets());
                stream.push(rn.as_str(), Colors::register());
                stream.push("]", Colors::brackets());
            }
            (false, _) => {
                stream.push("[", Colors::brackets());
                stream.push(rn.as_str(), Colors::register());
                stream.push("]", Colors::brackets());
            }
        }
    }
}

impl Decoded for Instruction {
    #[inline]
    fn width(&self) -> usize {
        4
    }

    fn update_rel_addrs(&mut self, _addr: usize) {}
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut stream = TokenStream::new();
        let symbols = Index::default();
        self.tokenize(&mut stream, &symbols);
        f.write_str(&stream.to_string())?;
        Ok(())
    }
}

impl ToTokens for Instruction {
    fn tokenize(&self, stream: &mut TokenStream, symbols: &Index) {
        let imm_override = None;

        match self.opcode {
            Opcode::IT => {
                if let (Operand::Imm32(cond), Operand::Imm32(mask)) =
                    (&self.operands[0], &self.operands[1])
                {
                    let inv = cond & 1 == 1;
                    let condition = ConditionCode::build(*cond as u8);
                    if mask & 0b0001 != 0 {
                        // three flags
                        let op = format!(
                            "it{}{}{} {}",
                            if inv ^ ((mask & 0b1000) != 0) {
                                "e"
                            } else {
                                "t"
                            },
                            if inv ^ ((mask & 0b0100) != 0) {
                                "e"
                            } else {
                                "t"
                            },
                            if inv ^ ((mask & 0b0010) != 0) {
                                "e"
                            } else {
                                "t"
                            },
                            condition,
                        );
                        stream.push_owned(op, Colors::opcode());
                    } else if mask & 0b0010 != 0 {
                        // two flags
                        let op = format!(
                            "it{}{} {}",
                            if inv ^ ((mask & 0b1000) != 0) {
                                "e"
                            } else {
                                "t"
                            },
                            if inv ^ ((mask & 0b0100) != 0) {
                                "e"
                            } else {
                                "t"
                            },
                            condition,
                        );
                        stream.push_owned(op, Colors::opcode());
                    } else if mask & 0b0100 != 0 {
                        // one flag
                        let op = format!(
                            "it{} {}",
                            if inv ^ ((mask & 0b1000) != 0) {
                                "e"
                            } else {
                                "t"
                            },
                            condition,
                        );
                        stream.push_owned(op, Colors::opcode());
                    } else {
                        // no flags
                        let op = format!("it {}", condition);
                        stream.push_owned(op, Colors::opcode());
                    }
                    // if the condition is AL, it won't get displayed. append it here.
                    if *cond == 14 {
                        stream.push("al", Colors::opcode());
                    }
                    return;
                } else {
                    panic!("impossible it operand");
                }
            }
            Opcode::CPS(_) => {
                if let Operand::Imm12(aif) = &self.operands[0] {
                    let op = format!(
                        "{} {}{}{}",
                        &self.opcode,
                        if aif & 0b100 != 0 { "a" } else { "" },
                        if aif & 0b010 != 0 { "i" } else { "" },
                        if aif & 0b001 != 0 { "f" } else { "" },
                    );
                    stream.push_owned(op, Colors::opcode());
                    if let Operand::Imm12(mode) = &self.operands[1] {
                        stream.push(", #", Colors::expr());
                        stream.push_owned(decoder::encode_hex(*mode as i64), Colors::immediate());
                    }
                    return;
                } else {
                    panic!("impossible cps operand");
                }
            }
            Opcode::SETEND => {
                if let Operand::Imm12(i) = &self.operands[0] {
                    if *i == 0 {
                        stream.push("setend le", Colors::opcode());
                    } else {
                        stream.push("setend be", Colors::opcode());
                    }
                    return;
                } else {
                    panic!("impossible setend operand");
                }
            }
            Opcode::LDR => {
                match self.operands {
                    // TODO: should this be PostindexOffset?
                    [Operand::Reg(rt), Operand::RegDerefPostindexOffset(Reg { bits: 13 }, 4, true, false), Operand::Nothing, Operand::Nothing] =>
                    {
                        ConditionedOpcode(Opcode::POP, self.s(), self.w(), self.condition)
                            .tokenize(stream, symbols);
                        stream.push(" {", Colors::brackets());
                        stream.push(rt.as_str(), Colors::register());
                        stream.push("}", Colors::brackets());
                        return;
                    }
                    _ => {}
                }
            }
            Opcode::STR => {
                match self.operands {
                    // TODO: should this be PreindexOffset?
                    [Operand::Reg(rt), Operand::RegDerefPreindexOffset(Reg { bits: 13 }, 4, false, true), Operand::Nothing, Operand::Nothing] =>
                    {
                        ConditionedOpcode(Opcode::PUSH, self.s(), self.w(), self.condition)
                            .tokenize(stream, symbols);
                        stream.push(" {", Colors::brackets());
                        stream.push(rt.as_str(), Colors::register());
                        stream.push("}", Colors::brackets());
                        return;
                    }
                    _ => {}
                }
            }
            Opcode::LDM(true, false, false, _usermode) => {
                // TODO: what indicates usermode in the ARM syntax?
                match self.operands {
                    [Operand::RegWBack(Reg { bits: 13 }, true), Operand::RegList(list), Operand::Nothing, Operand::Nothing] =>
                    {
                        ConditionedOpcode(Opcode::POP, self.s(), self.w(), self.condition)
                            .tokenize(stream, symbols);
                        stream.push(" ", Colors::expr());
                        format_reg_list(stream, list);
                        return;
                    }
                    _ => {}
                }
            }
            Opcode::STM(false, true, false, _usermode) => {
                // TODO: what indicates usermode in the ARM syntax?
                match self.operands {
                    [Operand::RegWBack(Reg { bits: 13 }, true), Operand::RegList(list), Operand::Nothing, Operand::Nothing] =>
                    {
                        ConditionedOpcode(Opcode::PUSH, self.s(), self.w(), self.condition)
                            .tokenize(stream, symbols);
                        stream.push(" ", Colors::expr());
                        format_reg_list(stream, list);
                        return;
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        match self.opcode {
            // TODO: [add, pre, usermode]
            Opcode::STM(_add, _pre, _wback, _usermode)
            | Opcode::LDM(_add, _pre, _wback, _usermode) => match self.operands {
                [Operand::RegWBack(rr, wback), Operand::RegList(list), Operand::Nothing, Operand::Nothing] =>
                {
                    ConditionedOpcode(self.opcode, self.s(), self.w(), self.condition)
                        .tokenize(stream, symbols);
                    stream.push(" ", Colors::expr());
                    stream.push(rr.as_str(), Colors::register());
                    if wback {
                        stream.push("!", Colors::expr());
                    }
                    stream.push(", ", Colors::expr());
                    format_reg_list(stream, list);
                    return;
                }
                _ => {
                    unreachable!();
                }
            },
            Opcode::STCL(coproc) => {
                stream.push("stcl ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }
            }
            Opcode::STC(coproc) => {
                stream.push("stc ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }
            }
            Opcode::STC2L(coproc) => {
                stream.push("stc2l ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }
            }
            Opcode::STC2(coproc) => {
                stream.push("stc2 ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }
            }
            Opcode::LDC(coproc) => {
                stream.push("ldc ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }
            }
            Opcode::LDCL(coproc) => {
                stream.push("ldcl ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }
            }
            Opcode::LDC2(coproc) => {
                stream.push("ldc2 ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }
            }
            Opcode::LDC2L(coproc) => {
                stream.push("ldc2l ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }
            }
            Opcode::MRRC2(coproc, opc) => {
                stream.push("mrrc2 ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                stream.push(", ", Colors::expr());
                stream.push_owned(opc.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }
            }
            Opcode::MCRR2(coproc, opc) => {
                stream.push("mcrr2 ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                stream.push(", ", Colors::expr());
                stream.push_owned(opc.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }
            }
            Opcode::MRC2(coproc, opc1, opc2) => {
                stream.push("mrc2 ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                stream.push(", ", Colors::expr());
                stream.push_owned(opc1.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }

                stream.push(", ", Colors::expr());
                stream.push_owned(opc2.to_string(), Colors::register());
            }
            Opcode::MCR2(coproc, opc1, opc2) => {
                stream.push("mcr2 ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                stream.push(", ", Colors::expr());
                stream.push_owned(opc1.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }

                stream.push(", ", Colors::expr());
                stream.push_owned(opc2.to_string(), Colors::register());
            }
            Opcode::CDP2(coproc, opc1, opc2) => {
                stream.push("cdp2 ", Colors::opcode());
                stream.push("p", Colors::register());
                stream.push_owned(coproc.to_string(), Colors::register());
                stream.push(", ", Colors::expr());
                stream.push_owned(opc1.to_string(), Colors::register());
                let ops = self.operands.iter();
                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, imm_override);
                }

                stream.push(", ", Colors::expr());
                stream.push_owned(opc2.to_string(), Colors::register());
            }
            _ => {
                ConditionedOpcode(self.opcode, self.s(), self.w(), self.condition)
                    .tokenize(stream, symbols);
                let mut ops = self.operands.iter();
                if let Some(first_op) = ops.next() {
                    if let Operand::Nothing = first_op {
                        return;
                    }
                    stream.push(" ", Colors::expr());
                    first_op.tokenize(stream, symbols, None);
                } else {
                    return;
                }

                for op in ops {
                    if let Operand::Nothing = op {
                        break;
                    }
                    stream.push(", ", Colors::expr());
                    op.tokenize(stream, symbols, None);
                }
            }
        }
    }
}

/// a condition code for am `armv7` or below instruction.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum ConditionCode {
    EQ,
    NE,
    HS,
    LO,
    MI,
    PL,
    VS,
    VC,
    HI,
    LS,
    GE,
    LT,
    GT,
    LE,
    AL,
}

impl Display for ConditionCode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            ConditionCode::EQ => write!(f, "eq"),
            ConditionCode::NE => write!(f, "ne"),
            ConditionCode::HS => write!(f, "hs"),
            ConditionCode::LO => write!(f, "lo"),
            ConditionCode::MI => write!(f, "mi"),
            ConditionCode::PL => write!(f, "pl"),
            ConditionCode::VS => write!(f, "vs"),
            ConditionCode::VC => write!(f, "vc"),
            ConditionCode::HI => write!(f, "hi"),
            ConditionCode::LS => write!(f, "ls"),
            ConditionCode::GE => write!(f, "ge"),
            ConditionCode::LT => write!(f, "lt"),
            ConditionCode::GT => write!(f, "gt"),
            ConditionCode::LE => write!(f, "le"),
            ConditionCode::AL => Ok(()),
        }
    }
}

impl ConditionCode {
    fn build(value: u8) -> ConditionCode {
        match value {
            0b0000 => ConditionCode::EQ,
            0b0001 => ConditionCode::NE,
            0b0010 => ConditionCode::HS,
            0b0011 => ConditionCode::LO,
            0b0100 => ConditionCode::MI,
            0b0101 => ConditionCode::PL,
            0b0110 => ConditionCode::VS,
            0b0111 => ConditionCode::VC,
            0b1000 => ConditionCode::HI,
            0b1001 => ConditionCode::LS,
            0b1010 => ConditionCode::GE,
            0b1011 => ConditionCode::LT,
            0b1100 => ConditionCode::GT,
            0b1101 => ConditionCode::LE,
            0b1110 => ConditionCode::AL,
            _ => {
                // this means the argument `value` must never be outside [0,15]
                // which itself means this function shouldn't be public
                unreachable!();
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
enum DecodeMode {
    User,
    FIQ,
    IRQ,
    Supervisor,
    Monitor,
    Abort,
    Hyp,
    Undefined,
    System,
    /// Catch-all mode to try decoding all ARM instructions. Some instructions are `UNDEFINED` or
    /// `UNPREDICTABLE` in some modes, but `Any` will attempt to decode all.
    Any,
}

impl Default for DecodeMode {
    fn default() -> Self {
        DecodeMode::Any
    }
}

impl DecodeMode {
    fn is_user(&self) -> bool {
        match self {
            DecodeMode::Any | DecodeMode::User => true,
            _ => false,
        }
    }
    #[allow(dead_code)]
    fn is_supervisor(&self) -> bool {
        match self {
            DecodeMode::Any | DecodeMode::Supervisor => true,
            _ => false,
        }
    }
    fn is_hyp(&self) -> bool {
        match self {
            DecodeMode::Any | DecodeMode::Hyp => true,
            _ => false,
        }
    }
    fn is_system(&self) -> bool {
        match self {
            DecodeMode::Any | DecodeMode::System => true,
            _ => false,
        }
    }
    fn is_any(&self) -> bool {
        match self {
            DecodeMode::Any => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
enum ARMVersion {
    v4,
    v5,
    v6,
    v6t2,
    v7,
    v7ve,
    v7vese,
    Any,
}

impl Default for ARMVersion {
    fn default() -> Self {
        ARMVersion::Any
    }
}

// nothing checks/rejects by arm version yet, but.. soon....
/// a struct with decode configuration for `ARMv7` and below. the same decoder is used for `thumb`
/// and non-`thumb` modes, and the same instruction struct is used for decoded instructions in
/// either mode.
///
/// NOTE: helper functions here create `Decoder` for specific revisions, extensions, or lack
/// thereof, in the supported instruction set. `yaxpeax-arm` does not actually honor these settings
/// yet. this means any `Decoder` will decode all known instructions through the latest `ARMv7`
/// extensions.
#[allow(unused)]
#[derive(Debug)]
pub struct Decoder {
    mode: DecodeMode,
    version: ARMVersion,
    should_is_must: bool,
    thumb: bool,
}

impl Default for Decoder {
    fn default() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::Any,
            should_is_must: true,
            thumb: false,
        }
    }
}

impl Decoder {
    /// set the decoder to decoding in thumb mode as the specified bool provides; `true` means
    /// "yes, decode in `thumb` mode", where `false` means to decode as a normal `arm` instruction.
    pub fn set_thumb_mode(&mut self, thumb: bool) {
        self.thumb = thumb;
    }

    /// set the decoder to decoding in thumb mode as the specified bool provides; `true` means
    /// "yes, decode in `thumb` mode", where `false` means to decode as a normal `arm` instruction.
    ///
    /// (this consumes and returns the `Decoder` to support use in chained calls.)`
    pub fn with_thumb_mode(mut self, thumb: bool) -> Self {
        self.set_thumb_mode(thumb);
        self
    }

    /// initialize a new `arm` `Decoder` with default ("everything") support, but in `thumb`
    /// mode.
    pub fn default_thumb() -> Self {
        Self::default().with_thumb_mode(true)
    }

    /// create an `Decoder` that supports only instructions through to `ARMv4`.
    pub fn armv4() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::v4,
            should_is_must: true,
            thumb: false,
        }
    }

    /// create an `Decoder` that supports only instructions through to `ARMv5`.
    pub fn armv5() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::v5,
            should_is_must: true,
            thumb: false,
        }
    }

    /// create an `Decoder` that supports only instructions through to `ARMv6`.
    pub fn armv6() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::v6,
            should_is_must: true,
            thumb: false,
        }
    }

    /// create an `Decoder` that supports only instructions through to `ARMv6t2`.
    pub fn armv6t2() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::v6t2,
            should_is_must: true,
            thumb: false,
        }
    }

    /// create an `Decoder` that supports only instructions through to `ARMv6t2` in thumb mode.
    pub fn armv6t2_thumb() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::v6t2,
            should_is_must: true,
            thumb: true,
        }
    }

    /// create an `Decoder` that supports only instructions through to `ARMv7`.
    pub fn armv7() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::v7,
            should_is_must: true,
            thumb: false,
        }
    }

    /// create an `Decoder` that supports only instructions through to `ARMv7` in thumb mode.
    pub fn armv7_thumb() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::v7,
            should_is_must: true,
            thumb: true,
        }
    }

    /// create an `Decoder` that supports only instructions through to `ARMv7ve`.
    pub fn armv7ve() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::v7ve,
            should_is_must: true,
            thumb: false,
        }
    }

    /// create an `Decoder` that supports only instructions through to `ARMv7ve` in thumb mode.
    pub fn armv7ve_thumb() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::v7ve,
            should_is_must: true,
            thumb: true,
        }
    }

    /// create an `Decoder` that supports only instructions through to `ARMv7vese`.
    pub fn armv7vese() -> Self {
        Self {
            mode: DecodeMode::Any,
            version: ARMVersion::v7vese,
            should_is_must: true,
            thumb: false,
        }
    }

    fn unpredictable(&self) -> Result<(), ErrorKind> {
        if self.mode != DecodeMode::Any {
            Err(ErrorKind::Unpredictable)
        } else {
            Ok(())
        }
    }
}

impl Decodable for Decoder {
    type Instruction = Instruction;

    fn decode(&self, reader: &mut decoder::Reader) -> Result<Self::Instruction, Error> {
        let mut inst = Instruction::default();
        read(self, reader, &mut inst).map_err(|err| Error::new(err, 4))?;
        Ok(inst)
    }

    fn max_width(&self) -> usize {
        4
    }
}

#[inline(always)]
fn read(decoder: &Decoder, words: &mut Reader, inst: &mut Instruction) -> Result<(), ErrorKind> {
    inst.set_w(false);
    inst.set_wide(false);
    if decoder.thumb {
        return thumb::read(decoder, words, inst);
    } else {
        inst.set_thumb(false);
    }

    let mut word_bytes = [0u8; 4];
    words.next_n(&mut word_bytes).ok_or(ErrorKind::ExhaustedInput)?;
    let word = u32::from_le_bytes(word_bytes);

    let (cond, opc_upper) = {
        let top_byte = word >> 24;
        (((top_byte >> 4) & 0xf) as u8, ((top_byte >> 1) & 0x7) as u8)
    };

    if cond == 0b1111 {
        // unconditional instructions, section A5.7/page A5-214
        inst.condition = ConditionCode::AL;
        let op1 = (word >> 20) as u8;
        if op1 >= 0b1000_0000 {
            match (op1 >> 5) & 0b11 {
                0b00 => {
                    match op1 & 0b101 {
                        0b000 | 0b101 => {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                        0b100 => {
                            // SRS (see table A5.7, op1 = 0b100xx1x0, page A5-214)
                            if !decoder.mode.is_any() && decoder.mode.is_hyp() {
                                return Err(ErrorKind::Undefined);
                            }
                            if decoder.should_is_must {
                                if word & 0x000fffe0 != 0x000d0500 {
                                    return Err(ErrorKind::Nonconforming);
                                }
                            }
                            let puxw = (word >> 21) & 0b1111;
                            let P = puxw & 0b1000 != 0;
                            let U = puxw & 0b0100 != 0;
                            let W = puxw & 0b0001 != 0;
                            inst.opcode = Opcode::SRS(P, U);
                            inst.operands = [
                                Operand::RegWBack(Reg::from_u8(13), W),
                                Operand::Imm32(word & 0b1111),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b001 => {
                            // RFE (see table A5.7, op1 = 0b100xx0x1, page A5-214)
                            if !decoder.mode.is_any() && decoder.mode.is_hyp() {
                                return Err(ErrorKind::Undefined);
                            }
                            if decoder.should_is_must {
                                if word & 0xffff != 0x0a00 {
                                    return Err(ErrorKind::Nonconforming);
                                }
                            }
                            let puxw = (word >> 21) & 0b1111;
                            let P = puxw & 0b1000 != 0;
                            let U = puxw & 0b0100 != 0;
                            let W = puxw & 0b0001 != 0;
                            inst.opcode = Opcode::RFE(P, U);
                            inst.operands = [
                                Operand::RegWBack(Reg::from_u8((word >> 16) as u8 & 0b1111), W),
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        _ => {
                            unreachable!("op1 mask is 0b101 but somehow we got an invalid pattern");
                        }
                    }
                }
                0b01 => {
                    inst.opcode = Opcode::BLX;
                    let operand = ((word & 0xffffff) as i32) << 8 >> 7;
                    inst.operands = [
                        Operand::BranchThumbOffset(operand | (((word >> 24) & 0b1) as i32)),
                        Operand::Nothing,
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                }
                0b10 => {
                    // op1=0b110xxxxx, see table A5-23
                    if (word >> 20) & 0b11010 == 0b00000 {
                        // the `not 11000x0{0,1}` cases in table A5-23, MCRR or MRRC
                        // but first check that bit 2 of op1 is in fact 1:
                        if (word >> 20) & 0b00100 != 0 {
                            // actually MCRR or MRRC
                            let CRm = word as u8 & 0b1111;
                            let opc1 = (word >> 4) as u8 & 0b1111;
                            let coproc = (word >> 8) as u8 & 0b1111;
                            if coproc & 0b1110 == 0b1010 {
                                // TODO: `UNDEFINED`
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            let Rt = (word >> 12) as u8 & 0b1111;
                            let Rt2 = (word >> 16) as u8 & 0b1111;
                            if Rt == 15 || Rt2 == 15 || Rt == Rt2 {
                                // TODO: actually `UNPREDICTABLE`
                                return Err(ErrorKind::InvalidOperand);
                            }
                            if (word >> 20) & 0b00001 != 0 {
                                inst.opcode = Opcode::MRRC2(coproc, opc1);
                            } else {
                                inst.opcode = Opcode::MCRR2(coproc, opc1);
                            }
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(Rt)),
                                Operand::Reg(Reg::from_u8(Rt2)),
                                Operand::CReg(CReg::from_u8(CRm)),
                                Operand::Nothing,
                            ];
                        } else {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                    } else {
                        // STC or LDC
                        let pudw = (word >> 21) as u8 & 0b1111;
                        let Rn = (word >> 16) as u8 & 0b1111;
                        let CRd = (word >> 12) as u8 & 0b1111;
                        let coproc = (word >> 8) as u8 & 0b1111;
                        let imm8 = word & 0b11111111;

                        if coproc & 0b1110 == 0b1010 {
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        if (word >> 20) & 0b00001 == 0 {
                            // op=110xxxx0, STC
                            // page A8-663

                            if pudw & 0b0010 != 0 {
                                inst.opcode = Opcode::STC2L(coproc);
                            } else {
                                inst.opcode = Opcode::STC2(coproc);
                            }
                        } else {
                            // op=110xxxx1, LDC
                            // page A8-393

                            if pudw & 0b0010 != 0 {
                                inst.opcode = Opcode::LDC2L(coproc);
                            } else {
                                inst.opcode = Opcode::LDC2(coproc);
                            }
                        }

                        let P = pudw & 0b1000 != 0;
                        let U = pudw & 0b0100 != 0;
                        let W = pudw & 0b0001 != 0;

                        inst.operands = [
                            Operand::CReg(CReg::from_u8(CRd)),
                            if P {
                                Operand::RegDerefPreindexOffset(
                                    Reg::from_u8(Rn),
                                    (imm8 << 2) as u16,
                                    U,
                                    W,
                                )
                            } else {
                                if W {
                                    // preindex has no wback
                                    Operand::RegDerefPostindexOffset(
                                        Reg::from_u8(Rn),
                                        (imm8 << 2) as u16,
                                        U,
                                        false,
                                    )
                                } else {
                                    Operand::RegDeref(Reg::from_u8(Rn))
                                }
                            },
                            if !P && !W {
                                // TODO: not sure what ldc2{l}'s <option> field really means?
                                // at this point the invalid encoding and mrrc/mcrr forms have
                                // been tested, so..
                                debug_assert!(U);
                                Operand::CoprocOption(imm8 as u8)
                            } else {
                                Operand::Nothing
                            },
                            Operand::Nothing,
                        ];
                    }
                }
                0b11 => {
                    // operands are shared between cdp2 and mcr2/mrc2, but Rt is repurposed as
                    // CRd
                    let CRm = word as u8 & 0b1111;
                    let opc2 = (word >> 5) as u8 & 0b111;
                    let coproc = (word >> 8) as u8 & 0b1111;
                    let Rt = (word >> 12) as u8 & 0b1111;
                    let CRn = (word >> 16) as u8 & 0b1111;

                    if (word >> 4) & 1 == 0 {
                        // CDP2, page A8-356
                        let opc1 = (word >> 20) as u8 & 0b1111;
                        inst.opcode = Opcode::CDP2(coproc, opc1, opc2);
                        inst.operands = [
                            Operand::CReg(CReg::from_u8(Rt)),
                            Operand::CReg(CReg::from_u8(CRn)),
                            Operand::CReg(CReg::from_u8(CRm)),
                            Operand::Nothing,
                        ];
                    } else {
                        // MCR2/MRC2, page A8-477/A8-493
                        let opc1 = (word >> 21) as u8 & 0b111;
                        if (word >> 20) & 1 == 0 {
                            inst.opcode = Opcode::MCR2(coproc, opc1, opc2);
                        } else {
                            inst.opcode = Opcode::MRC2(coproc, opc1, opc2);
                        }
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(Rt)),
                            Operand::CReg(CReg::from_u8(CRn)),
                            Operand::CReg(CReg::from_u8(CRm)),
                            Operand::Nothing,
                        ];
                    }
                }
                _ => {
                    unreachable!("op1 is two bits");
                }
            }
        } else {
            // op1=0xxxxxxx, "Memory hints, Advanced SIMD instructions, and miscellaneous
            // instructions on pge A5-215"
            return Err(ErrorKind::Incomplete);
        }
        return Ok(());
    } else {
        inst.condition = ConditionCode::build(cond);
    }

    // distinction at this point is on page A5-192
    match opc_upper {
        0b000 => {
            // the instruction looks like
            // |c o n d|0 0 0|x x x x|x|x x x x|x x x x|x x x x x|x x|x|x x x x|
            let (s, opcode) = {
                let part = word >> 20;
                ((part & 0x01) == 1, ((part >> 1) & 0x0f) as u8)
            };

            if (word & 0b10010000) == 0b10010000 {
                // the instruction looks like
                // |c o n d|0 0 0|x x x x|x|x x x x|x x x x|x x x x 1|x x|1|x x x x|
                // which is a category of multiplies and extra load/store
                if (word & 0x0f0000f0) == 0x00000090 {
                    // |c o n d|0 0 0 0|x x x x x x x x x x x x x x x x|1 0 0 1|x x x x|
                    // Multiply instruction extension space
                    // (page A5-200)
                    let op = ((word >> 20) & 0x0f) as u8;
                    let s = (op & 1) == 1;
                    let op = op >> 1;
                    let R = [
                        (word & 0x0f) as u8,
                        ((word >> 8) & 0x0f) as u8,
                        ((word >> 12) & 0x0f) as u8,
                        ((word >> 16) & 0x0f) as u8,
                    ];
                    inst.set_s(s);
                    match op {
                        0b000 => {
                            inst.opcode = Opcode::MUL;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(R[3])),
                                Operand::Reg(Reg::from_u8(R[0])),
                                Operand::Reg(Reg::from_u8(R[1])),
                                Operand::Nothing,
                            ];
                        }
                        0b001 => {
                            inst.opcode = Opcode::MLA;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(R[3])),
                                Operand::Reg(Reg::from_u8(R[0])),
                                Operand::Reg(Reg::from_u8(R[1])),
                                Operand::Reg(Reg::from_u8(R[2])),
                            ];
                        }
                        0b010 => {
                            if s {
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            inst.opcode = Opcode::UMAAL;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(R[2])),
                                Operand::Reg(Reg::from_u8(R[3])),
                                Operand::Reg(Reg::from_u8(R[0])),
                                Operand::Reg(Reg::from_u8(R[1])),
                            ];
                        }
                        0b011 => {
                            if s {
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            inst.opcode = Opcode::MLS;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(R[3])),
                                Operand::Reg(Reg::from_u8(R[0])),
                                Operand::Reg(Reg::from_u8(R[1])),
                                Operand::Reg(Reg::from_u8(R[2])),
                            ];
                        }
                        0b100 => {
                            inst.opcode = Opcode::UMULL;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(R[2])),
                                Operand::Reg(Reg::from_u8(R[3])),
                                Operand::Reg(Reg::from_u8(R[0])),
                                Operand::Reg(Reg::from_u8(R[1])),
                            ];
                        }
                        0b101 => {
                            inst.opcode = Opcode::UMLAL;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(R[2])),
                                Operand::Reg(Reg::from_u8(R[3])),
                                Operand::Reg(Reg::from_u8(R[0])),
                                Operand::Reg(Reg::from_u8(R[1])),
                            ];
                        }
                        0b110 => {
                            inst.opcode = Opcode::SMULL;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(R[2])),
                                Operand::Reg(Reg::from_u8(R[3])),
                                Operand::Reg(Reg::from_u8(R[0])),
                                Operand::Reg(Reg::from_u8(R[1])),
                            ];
                        }
                        0b111 => {
                            inst.opcode = Opcode::SMLAL;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(R[2])),
                                Operand::Reg(Reg::from_u8(R[3])),
                                Operand::Reg(Reg::from_u8(R[0])),
                                Operand::Reg(Reg::from_u8(R[1])),
                            ];
                        }
                        _ => {
                            unreachable!("mul opcode is only three bits, got: {:x}", op)
                        }
                    }
                } else {
                    // |c o n d|0 0 0 u|x x x x x x x x x x x x x x x x|1 u u 1|x x x x|
                    // with at least one of u being 1
                    // misc instructions
                    let (flags, Rn, Rd, HiOffset, op, LoOffset) = {
                        let LoOffset = (word & 0x0f) as u8;
                        let word = word >> 5;
                        let op = (word & 0x3) as u8;
                        let word = word >> 3;
                        let HiOffset = (word & 0x0f) as u8;
                        let word = word >> 4;
                        let Rd = (word & 0x0f) as u8;
                        let word = word >> 4;
                        let Rn = (word & 0x0f) as u8;
                        let word = word >> 4;
                        let flags = (word & 0x1f) as u8;
                        (flags, Rn, Rd, HiOffset, op, LoOffset)
                    };
                    match op {
                        0b00 => {
                            // |c o n d|0 0 0 1|x x x x x x x x x x x x x x x x|1 0 0 1|x x x x|
                            // this is swp or {ld,st}ex, conditional on bit 23
                            // see page A5-203
                            match flags {
                                0b10000 => {
                                    inst.opcode = Opcode::SWP;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        Operand::Reg(Reg::from_u8(LoOffset)),
                                        Operand::RegDeref(Reg::from_u8(Rn)),
                                        Operand::Nothing,
                                    ];
                                }
                                0b10001 | 0b10010 | 0b10011 => {
                                    inst.opcode = Opcode::Invalid;
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                                0b10100 => {
                                    inst.opcode = Opcode::SWPB;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        Operand::Reg(Reg::from_u8(LoOffset)),
                                        Operand::RegDeref(Reg::from_u8(Rn)),
                                        Operand::Nothing,
                                    ];
                                }
                                0b10101 | 0b10110 | 0b10111 => {
                                    inst.opcode = Opcode::Invalid;
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                                0b11000 => {
                                    // TODO: flag v6
                                    inst.opcode = Opcode::STREX;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        Operand::Reg(Reg::from_u8(LoOffset)),
                                        Operand::RegDeref(Reg::from_u8(Rn)),
                                        Operand::Nothing,
                                    ];
                                }
                                0b11001 => {
                                    // TODO: flag v6
                                    inst.opcode = Opcode::LDREX;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        Operand::RegDeref(Reg::from_u8(Rn)),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b11010 => {
                                    inst.opcode = Opcode::STREXD;
                                    if LoOffset == 0b1110
                                        || (LoOffset & 1 == 1)
                                        || Rd == 15
                                        || Rn == 15
                                    {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        Operand::Reg(Reg::from_u8(LoOffset)),
                                        Operand::Reg(Reg::from_u8(LoOffset + 1)),
                                        Operand::RegDeref(Reg::from_u8(Rn)),
                                    ];
                                }
                                0b11011 => {
                                    inst.opcode = Opcode::LDREXD;
                                    if Rd == 0b1110 || (Rd & 1 == 1) || Rn == 15 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        Operand::Reg(Reg::from_u8(Rd + 1)),
                                        Operand::RegDeref(Reg::from_u8(Rn)),
                                        Operand::Nothing,
                                    ];
                                }
                                0b11100 => {
                                    inst.opcode = Opcode::STREXB;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        Operand::Reg(Reg::from_u8(LoOffset)),
                                        Operand::RegDeref(Reg::from_u8(Rn)),
                                        Operand::Nothing,
                                    ];
                                }
                                0b11101 => {
                                    inst.opcode = Opcode::LDREXB;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        Operand::RegDeref(Reg::from_u8(Rn)),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b11110 => {
                                    inst.opcode = Opcode::STREXH;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        Operand::Reg(Reg::from_u8(LoOffset)),
                                        Operand::RegDeref(Reg::from_u8(Rn)),
                                        Operand::Nothing,
                                    ];
                                }
                                0b11111 => {
                                    inst.opcode = Opcode::LDREXH;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        Operand::RegDeref(Reg::from_u8(Rn)),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                _ => {
                                    /*
                                     * This is unreachable because we have checked op is b1001,
                                     * meaning the high bit of flags *MUST* be 1.
                                     *
                                     * high bit and mid-bits of op all being 0 was checked
                                     * before reaching here.
                                     */
                                    unreachable!("load/store flags: {:x}", flags);
                                }
                            }
                        }
                        0b01 => {
                            // |c o n d|0 0 0 x|x x x x x x x x x x x x x x x x|1 0 1 1|x x x x|
                            // page A5-201
                            let P = flags & 0b10000 != 0;
                            let U = flags & 0b01000 != 0;
                            let W = flags & 0b00010 != 0;

                            match flags & 0b00101 {
                                0b00000 => {
                                    // STRHT or STRH
                                    if !P && W {
                                        // flags == 0b0x010
                                        inst.opcode = Opcode::STRHT;
                                    } else {
                                        inst.opcode = Opcode::STRH;
                                    }
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        if P {
                                            Operand::RegDerefPreindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(LoOffset),
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either this is !P && W, so STRHT, and no wback,
                                            // or this is !W, and no wback
                                            Operand::RegDerefPostindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(LoOffset),
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b00001 => {
                                    // LDRHT or LDRH
                                    if !P && W {
                                        // flags == 0b0x011
                                        inst.opcode = Opcode::LDRHT;
                                    } else {
                                        inst.opcode = Opcode::LDRH;
                                    }
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        if P {
                                            Operand::RegDerefPreindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(LoOffset),
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either this is !P && W, so LDRHT, and no wback,
                                            // or this is !W, and no wback
                                            Operand::RegDerefPostindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(LoOffset),
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b00100 => {
                                    // STRHT or STRH
                                    if !P && W {
                                        // flags == 0b0x110
                                        inst.opcode = Opcode::STRHT;
                                    } else {
                                        inst.opcode = Opcode::STRH;
                                    }
                                    let imm = (HiOffset << 4) as u16 | LoOffset as u16;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        if P {
                                            Operand::RegDerefPreindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either this is !P && W, so STRHT, and no wback,
                                            // or this is !W, and no wback
                                            Operand::RegDerefPostindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b00101 => {
                                    // LDRHT or LDRH
                                    if !P && W {
                                        // flags == 0b0x111
                                        inst.opcode = Opcode::LDRHT;
                                    } else {
                                        inst.opcode = Opcode::LDRH;
                                    }
                                    let imm = (HiOffset << 4) as u16 | LoOffset as u16;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rd)),
                                        if P {
                                            Operand::RegDerefPreindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either this is !P && W, so LDRHT, and no wback,
                                            // or this is !W, and no wback
                                            Operand::RegDerefPostindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                o => {
                                    unreachable!("all of op2=01 in table A5-10 should be handled, got unexpected pattern {:b}", o);
                                }
                            }
                            return Ok(());
                        }
                        0b10 => {
                            // |c o n d|0 0 0 x|x x x x x x x x x x x x x x x x|1 1 0 1|x x x x|
                            // page A5-201
                            let P = flags & 0b10000 != 0;
                            let U = flags & 0b01000 != 0;
                            let W = flags & 0b00010 != 0;

                            match flags & 0b00101 {
                                0b00000 => {
                                    // LDRD or invalid
                                    if !P && W {
                                        // flags == 0b0x010
                                        return Err(ErrorKind::InvalidOperand);
                                    } else {
                                        inst.opcode = Opcode::LDRD;
                                    }
                                    if decoder.should_is_must {
                                        if (word >> 8) & 0b1111 == 0 {
                                            return Err(ErrorKind::Nonconforming);
                                        }
                                    }
                                    let Rm = word as u8 & 0b1111;
                                    let Rt = (word >> 12) as u8 & 0b1111;
                                    if Rt & 1 != 0 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    if Rt == 15 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    let Rn = (word >> 16) as u8 & 0b1111;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rt)),
                                        Operand::Reg(Reg::from_u8(Rt + 1)),
                                        if P {
                                            Operand::RegDerefPreindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(Rm),
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either !P & W (invalid) or !W
                                            Operand::RegDerefPostindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(Rm),
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                    ];
                                }
                                0b00001 => {
                                    // LDRSB or LDRSBT
                                    if !P && W {
                                        // flags == 0b0x010
                                        inst.opcode = Opcode::LDRSBT;
                                    } else {
                                        inst.opcode = Opcode::LDRSB;
                                    }
                                    if decoder.should_is_must {
                                        if (word >> 8) & 0b1111 == 0 {
                                            return Err(ErrorKind::Nonconforming);
                                        }
                                    }
                                    let Rm = word as u8 & 0b1111;
                                    let Rt = (word >> 12) as u8 & 0b1111;
                                    let Rn = (word >> 16) as u8 & 0b1111;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rt)),
                                        if P {
                                            Operand::RegDerefPreindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(Rm),
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either !P & W (ldrsbt) or !W
                                            Operand::RegDerefPostindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(Rm),
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b00100 => {
                                    // LDRD (immediate)
                                    if !P && W {
                                        // flags == 0b0x110
                                        return Err(ErrorKind::InvalidOperand);
                                    } else {
                                        inst.opcode = Opcode::LDRD;
                                    }
                                    let Rt = (word >> 12) as u8 & 0b1111;
                                    if Rt & 1 != 0 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    if Rt == 14 || Rn == 15 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    let Rn = (word >> 16) as u8 & 0b1111;
                                    let imm = (HiOffset << 4) as u16 | LoOffset as u16;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rt)),
                                        Operand::Reg(Reg::from_u8(Rt + 1)),
                                        if P {
                                            Operand::RegDerefPreindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either !P & W (invalid) or !W
                                            Operand::RegDerefPostindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                    ];
                                }
                                0b00101 => {
                                    // LDRSB or LDRSBT
                                    if !P && W {
                                        // flags == 0b0x010
                                        inst.opcode = Opcode::LDRSBT;
                                    } else {
                                        inst.opcode = Opcode::LDRSB;
                                    }
                                    if decoder.should_is_must {
                                        if (word >> 8) & 0b1111 == 0 {
                                            return Err(ErrorKind::Nonconforming);
                                        }
                                    }
                                    let Rt = (word >> 12) as u8 & 0b1111;
                                    let Rn = (word >> 16) as u8 & 0b1111;
                                    let imm = (HiOffset << 4) as u16 | LoOffset as u16;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rt)),
                                        if P {
                                            Operand::RegDerefPreindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either !P & W (ldrsbt) or !W
                                            Operand::RegDerefPostindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                _ => {
                                    unreachable!("impossible bit pattern");
                                }
                            }
                        }
                        0b11 => {
                            // |c o n d|0 0 0 x|x x x x x x x x x x x x x x x x|1 1 1 1|x x x x|
                            // page A5-201
                            let P = flags & 0b10000 != 0;
                            let U = flags & 0b01000 != 0;
                            let W = flags & 0b00010 != 0;

                            match flags & 0b00101 {
                                0b00000 => {
                                    // STRD or invalid
                                    if !P && W {
                                        // flags == 0b0x010
                                        return Err(ErrorKind::InvalidOperand);
                                    } else {
                                        inst.opcode = Opcode::STRD;
                                    }
                                    if decoder.should_is_must {
                                        if (word >> 8) & 0b1111 == 0 {
                                            return Err(ErrorKind::Nonconforming);
                                        }
                                    }
                                    let Rm = word as u8 & 0b1111;
                                    let Rt = (word >> 12) as u8 & 0b1111;
                                    if Rt & 1 != 0 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    let Rn = (word >> 16) as u8 & 0b1111;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rt)),
                                        Operand::Reg(Reg::from_u8(Rt + 1)),
                                        if P {
                                            Operand::RegDerefPreindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(Rm),
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either !P & W (invalid) or !W
                                            Operand::RegDerefPostindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(Rm),
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                    ];
                                }
                                0b00001 => {
                                    // LDRSH or LDRSHT
                                    if !P && W {
                                        // flags == 0b0x010
                                        inst.opcode = Opcode::LDRSHT;
                                    } else {
                                        inst.opcode = Opcode::LDRSH;
                                    }
                                    if decoder.should_is_must {
                                        if (word >> 8) & 0b1111 == 0 {
                                            return Err(ErrorKind::Nonconforming);
                                        }
                                    }
                                    let Rm = word as u8 & 0b1111;
                                    let Rt = (word >> 12) as u8 & 0b1111;
                                    let Rn = (word >> 16) as u8 & 0b1111;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rt)),
                                        if P {
                                            Operand::RegDerefPreindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(Rm),
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either !P & W (ldrsht) or !W
                                            Operand::RegDerefPostindexReg(
                                                Reg::from_u8(Rn),
                                                Reg::from_u8(Rm),
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b00100 => {
                                    // STRD (immediate)
                                    if !P && W {
                                        // flags == 0b0x110
                                        return Err(ErrorKind::InvalidOperand);
                                    } else {
                                        inst.opcode = Opcode::STRD;
                                    }
                                    let Rt = (word >> 12) as u8 & 0b1111;
                                    if Rt & 1 != 0 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    if Rt == 15 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    let Rn = (word >> 16) as u8 & 0b1111;
                                    let imm = (HiOffset << 4) as u16 | LoOffset as u16;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rt)),
                                        Operand::Reg(Reg::from_u8(Rt + 1)),
                                        if P {
                                            Operand::RegDerefPreindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either !P & W (invalid) or !W
                                            Operand::RegDerefPostindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                    ];
                                }
                                0b00101 => {
                                    // LDRSH or LDRSHT
                                    if !P && W {
                                        // flags == 0b0x010
                                        inst.opcode = Opcode::LDRSHT;
                                    } else {
                                        inst.opcode = Opcode::LDRSH;
                                    }
                                    if decoder.should_is_must {
                                        if (word >> 8) & 0b1111 == 0 {
                                            return Err(ErrorKind::Nonconforming);
                                        }
                                    }
                                    let Rt = (word >> 12) as u8 & 0b1111;
                                    let Rn = (word >> 16) as u8 & 0b1111;
                                    let imm = (HiOffset << 4) as u16 | LoOffset as u16;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(Rt)),
                                        if P {
                                            Operand::RegDerefPreindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                W,
                                            )
                                        } else {
                                            // either !P & W (ldrsht) or !W
                                            Operand::RegDerefPostindexOffset(
                                                Reg::from_u8(Rn),
                                                imm,
                                                U,
                                                false,
                                            )
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                _ => {
                                    unreachable!("impossible bit pattern");
                                }
                            }
                        }
                        _ => {
                            unreachable!("op is two bits");
                        }
                    }
                }
            } else {
                // we know this is data processing with imm or reg shift, OR
                // misc instructions in Figure A5-4

                if s == false && opcode >= 0b1000 && opcode < 0b1100 {
                    // Data-processing and miscellaneous instructions on page A5-194
                    let op2 = ((word >> 4) & 0x0f) as u8;
                    // the instruction looks like
                    // |c o n d|0 0 0|1 0 x x|0|x x x x|x x x x|x x x x x|x x|x|x x x x|
                    if op2 & 0x08 == 0x00 {
                        let op2 = op2 & 0x07;
                        // |c o n d|0 0 0|1 0 x x|0|x x x x|x x x x|x x x x|0|x x|x|x x x x|
                        // misc instructions (page A5-194)
                        match op2 {
                            0b000 => {
                                // test B bit (bit 9)
                                if word & 0b10_0000_0000 != 0 {
                                    // TODO: ARMv7VE flag
                                    let SYSm = (((word >> 9) & 1) << 4) as u8
                                        | ((word >> 16) & 0x0f) as u8;
                                    let R = (word >> 22) & 1;

                                    if opcode & 0b01 == 0b01 {
                                        if decoder.should_is_must {
                                            if word & 0b1111_1100_0000_0000 != 0xf000 {
                                                return Err(ErrorKind::InvalidOperand);
                                            }
                                        }
                                        inst.opcode = Opcode::MSR;
                                        inst.operands[0] =
                                            if let Some(reg) = Reg::from_sysm(R != 0, SYSm) {
                                                reg
                                            } else {
                                                return Err(ErrorKind::InvalidOperand);
                                            };
                                        inst.operands[1] =
                                            Operand::Reg(Reg::from_u8(word as u8 & 0b1111));
                                        inst.operands[2] = Operand::Nothing;
                                        inst.operands[3] = Operand::Nothing;
                                    } else {
                                        if decoder.should_is_must {
                                            if word & 0b0000_1100_0000_1111 != 0x0000 {
                                                return Err(ErrorKind::InvalidOperand);
                                            }
                                        }
                                        inst.opcode = Opcode::MRS;
                                        inst.operands[0] =
                                            Operand::Reg(Reg::from_u8((word >> 12) as u8 & 0b1111));
                                        inst.operands[1] =
                                            if let Some(reg) = Reg::from_sysm(R != 0, SYSm) {
                                                reg
                                            } else {
                                                return Err(ErrorKind::InvalidOperand);
                                            };
                                        inst.operands[2] = Operand::Nothing;
                                        inst.operands[3] = Operand::Nothing;
                                    }
                                } else {
                                    match opcode & 0b11 {
                                        0b00 | 0b10 => {
                                            inst.opcode = Opcode::MRS;
                                            let src = if decoder.mode.is_system() {
                                                let R = (word >> 22) & 1 != 0;
                                                if R {
                                                    Operand::SPSR
                                                } else {
                                                    Operand::CPSR
                                                }
                                            } else {
                                                Operand::APSR
                                            };
                                            inst.operands[0] = Operand::Reg(Reg::from_u8(
                                                (word >> 12) as u8 & 0b1111,
                                            ));
                                            inst.operands[1] = src;
                                            inst.operands[2] = Operand::Nothing;
                                            inst.operands[3] = Operand::Nothing;
                                        }
                                        0b01 => {
                                            inst.opcode = Opcode::MSR;
                                            let mask = (word >> 16) & 0b1111;
                                            if mask & 0b11 == 0 {
                                                if decoder.mode.is_user() {
                                                    inst.operands[0] = Operand::StatusRegMask(
                                                        StatusRegMask::from_raw(mask as u8)?,
                                                    );
                                                    inst.operands[1] = Operand::Reg(Reg::from_u8(
                                                        word as u8 & 0b1111,
                                                    ));
                                                    inst.operands[2] = Operand::Nothing;
                                                    inst.operands[3] = Operand::Nothing;
                                                } else {
                                                    // MSR with op == 01, op == xx00, but not
                                                    // user mode
                                                    return Err(ErrorKind::InvalidOperand);
                                                }
                                            } else {
                                                if decoder.mode.is_system() {
                                                    // bit 22 is the high bit of opcode, so..
                                                    let R = (word >> 22) as u8 & 1;
                                                    inst.operands[0] = Operand::StatusRegMask(
                                                        StatusRegMask::from_raw(
                                                            (R << 4) | mask as u8,
                                                        )?,
                                                    );
                                                    inst.operands[1] = Operand::Reg(Reg::from_u8(
                                                        word as u8 & 0b1111,
                                                    ));
                                                    inst.operands[2] = Operand::Nothing;
                                                    inst.operands[3] = Operand::Nothing;
                                                } else {
                                                    // MSR with op == 01, op != xx00
                                                    return Err(ErrorKind::InvalidOperand);
                                                }
                                            }
                                        }
                                        0b11 => {
                                            if !decoder.mode.is_system() {
                                                return Err(ErrorKind::InvalidOperand);
                                            }

                                            inst.opcode = Opcode::MSR;
                                            let mask = (word >> 16) & 0b1111;
                                            // bit 22 is the high bit of opcode, so..
                                            let R = (word >> 22) & 1;
                                            inst.operands[0] =
                                                Operand::StatusRegMask(StatusRegMask::from_raw(
                                                    (R << 4) as u8 | mask as u8,
                                                )?);
                                            inst.operands[1] =
                                                Operand::Reg(Reg::from_u8(word as u8 & 0b1111));
                                            inst.operands[2] = Operand::Nothing;
                                            inst.operands[3] = Operand::Nothing;
                                        }
                                        _ => {
                                            unreachable!("opcode is masked to two bits here");
                                        }
                                    }
                                }
                            }
                            0b001 => match opcode & 0b11 {
                                0b01 => {
                                    inst.opcode = Opcode::BX;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(word as u8 & 0b1111)),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b11 => {
                                    inst.opcode = Opcode::CLZ;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8((word >> 12) as u8 & 0b1111)),
                                        Operand::Reg(Reg::from_u8(word as u8 & 0b1111)),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                _ => {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            },
                            0b010 => {
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            0b011 => {
                                if opcode & 0b11 == 0b01 {
                                    inst.opcode = Opcode::BLX;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(word as u8 & 0x0f)),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                    return Ok(());
                                } else {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            }
                            0b100 => {
                                // no row for op2 == 0b100 in table A5-14 (page A5-203)
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            0b101 => {
                                // TODO: "Saturating addition and subtraction" page A5-200
                                match (word >> 21) & 0b11 {
                                    0b00 => {
                                        inst.opcode = Opcode::QADD;
                                    }
                                    0b01 => {
                                        inst.opcode = Opcode::QSUB;
                                    }
                                    0b10 => {
                                        inst.opcode = Opcode::QDADD;
                                    }
                                    0b11 => {
                                        inst.opcode = Opcode::QDSUB;
                                    }
                                    _ => {
                                        unreachable!(
                                            "bit pattern masked by 0b11 but large value observed"
                                        );
                                    }
                                }

                                if decoder.should_is_must {
                                    if (word >> 8) & 0b1111 != 0 {
                                        return Err(ErrorKind::Nonconforming);
                                    }
                                }

                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(((word >> 12) & 0b1111) as u8)),
                                    Operand::Reg(Reg::from_u8((word & 0b1111) as u8)),
                                    Operand::Reg(Reg::from_u8(((word >> 16) & 0b1111) as u8)),
                                    Operand::Nothing,
                                ];
                            }
                            0b110 => {
                                if (word >> 21) & 0b11 != 0b11 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                // "ERET" page B9-1968, from A5.2.12, page A5-205
                                if decoder.should_is_must {
                                    if word & 0b1111_1111_1111_1111 != 0b0000_0000_0000_0110_1110 {
                                        return Err(ErrorKind::Nonconforming);
                                    }
                                }
                                inst.opcode = Opcode::ERET;
                                inst.operands = [
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            0b111 => {
                                // "BKPT, HVC, SMC" from table A5-14/page A5-205
                                match (word >> 21) & 0b11 {
                                    0b00 => {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                    0b01 => {
                                        // BKPT
                                        if decoder.should_is_must {
                                            if (word >> 28) & 0b1111 != 0b1110 {
                                                // "BKPT must be encoded with AL condition"
                                                return Err(ErrorKind::InvalidOpcode);
                                            }
                                        }
                                        inst.opcode = Opcode::BKPT;
                                        let immlo = word & 0x0f;
                                        let imm = ((word >> 4) & 0xfff0) | immlo;
                                        inst.operands = [
                                            Operand::Imm32(imm),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b10 => {
                                        // HVC
                                        if decoder.should_is_must {
                                            if (word >> 28) & 0b1111 != 0b1110 {
                                                // "HVC must be encoded with AL condition"
                                                return Err(ErrorKind::InvalidOpcode);
                                            }
                                        }
                                        inst.opcode = Opcode::HVC;
                                        let immlo = word & 0x0f;
                                        let imm = ((word >> 4) & 0xfff0) | immlo;
                                        inst.operands = [
                                            Operand::Imm32(imm),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b11 => {
                                        // SMC
                                        if decoder.should_is_must {
                                            if word & 0xf_ff_ff_70 == 0x1_60_00_70 {
                                                // "SMC must be encoded with AL condition"
                                                return Err(ErrorKind::InvalidOpcode);
                                            }
                                        }
                                        inst.opcode = Opcode::SMC;
                                        let immlo = word & 0x0f;
                                        let imm = ((word >> 4) & 0xfff0) | immlo;
                                        inst.operands = [
                                            Operand::Imm32(imm),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                    _ => {
                                        unreachable!("impossible bit pattern");
                                    }
                                }
                            }
                            _ => {
                                unreachable!("op2 is a three bit field, got an invalid pattern");
                            }
                        }
                    } else {
                        // |c o n d|0 0 0|1 0 x x|0|x x x x|x x x x|x x x x|1|x x|x|x x x x|
                        // Halfword multiply and multiply accumulate on page A5-200
                        match (word >> 21) & 0b11 {
                            0b00 => {
                                let Rn_b = ((word >> 6) & 1) == 0;
                                let Rm_b = ((word >> 5) & 1) == 0;
                                inst.opcode = Opcode::SMLA(Rn_b, Rm_b);
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8((word >> 16) as u8 & 0b1111)),
                                    Operand::Reg(Reg::from_u8(word as u8 & 0b1111)),
                                    Operand::Reg(Reg::from_u8((word >> 8) as u8 & 0b1111)),
                                    Operand::Reg(Reg::from_u8((word >> 12) as u8 & 0b1111)),
                                ];
                                return Ok(());
                            }
                            0b01 => {
                                if word & 0b10000 == 0 {
                                    // SMLAWB, SMLAWT page A8-631
                                    let Rm_b = ((word >> 5) & 1) == 0;
                                    inst.opcode = Opcode::SMLAW(Rm_b);
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8((word >> 16) as u8 & 0b1111)),
                                        Operand::Reg(Reg::from_u8(word as u8 & 0b1111)),
                                        Operand::Reg(Reg::from_u8((word >> 8) as u8 & 0b1111)),
                                        Operand::Reg(Reg::from_u8((word >> 12) as u8 & 0b1111)),
                                    ];
                                    return Ok(());
                                } else {
                                    // SMULWB, SMULWT page A8-649
                                    let Rm_b = ((word >> 5) & 1) == 0;
                                    inst.opcode = Opcode::SMLAW(Rm_b);
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8((word >> 16) as u8 & 0b1111)),
                                        Operand::Reg(Reg::from_u8(word as u8 & 0b1111)),
                                        Operand::Reg(Reg::from_u8((word >> 8) as u8 & 0b1111)),
                                        Operand::Nothing,
                                    ];
                                    return Ok(());
                                }
                            }
                            0b10 => {
                                let Rn_b = ((word >> 6) & 1) == 0;
                                let Rm_b = ((word >> 5) & 1) == 0;
                                inst.opcode = Opcode::SMLAL_halfword(Rn_b, Rm_b);
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8((word >> 12) as u8 & 0b1111)),
                                    Operand::Reg(Reg::from_u8((word >> 16) as u8 & 0b1111)),
                                    Operand::Reg(Reg::from_u8(word as u8 & 0b1111)),
                                    Operand::Reg(Reg::from_u8((word >> 8) as u8 & 0b1111)),
                                ];
                                if inst.operands[0] == inst.operands[1] {
                                    // TODO: this is actually "UNPREDICTABLE" (A8-627)
                                    return Err(ErrorKind::InvalidOperand);
                                }
                                return Ok(());
                            }
                            0b11 => {
                                let Rn_b = ((word >> 6) & 1) == 0;
                                let Rm_b = ((word >> 5) & 1) == 0;
                                inst.opcode = Opcode::SMUL(Rn_b, Rm_b);
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8((word >> 16) as u8 & 0b1111)),
                                    Operand::Reg(Reg::from_u8(word as u8 & 0b1111)),
                                    Operand::Reg(Reg::from_u8((word >> 8) as u8 & 0b1111)),
                                    Operand::Nothing,
                                ];
                                if inst.operands[0] == inst.operands[1] {
                                    // TODO: this is actually "UNPREDICTABLE" (A8-627)
                                    return Err(ErrorKind::InvalidOperand);
                                }
                                return Ok(());
                            }
                            _ => {
                                unreachable!("word masked to two bits");
                            }
                        }
                    }
                } else {
                    // `Table A5-3 Data-processing (register) instructions`, not op=10xx0.
                    if opcode >= 16 {
                        unreachable!();
                    }
                    inst.opcode = DATA_PROCESSING_OPCODES[opcode as usize];
                    inst.set_s(s);

                    // at this point we know this is a data processing instruction
                    // either immediate shift or register shift
                    if word & 0b00010000 == 0 {
                        // |c o n d|0 0 0|x x x x|0|x x x x|x x x x|x x x x x|x x|0|x x x x|
                        // interpret the operands as
                        // | Rn | Rd | shift amount | shift | 0 | Rm |
                        let (Rn, Rd, shift_spec, Rm) = {
                            let Rm = (word & 0x0f) as u8;
                            let shift_spec = (word & 0xfff) as u16;
                            let word = word >> 12;
                            let Rd = (word & 0x0f) as u8;
                            let Rn = ((word >> 4) & 0x0f) as u8;
                            (Rn, Rd, shift_spec, Rm)
                        };

                        if shift_spec & 0xff0 == 0 {
                            if (0b1101 & opcode) == 0b1101 {
                                if decoder.should_is_must {
                                    if Rn != 0 {
                                        return Err(ErrorKind::Nonconforming);
                                    }
                                }
                                // MOV or MVN
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(Rd)),
                                    Operand::Reg(Reg::from_u8(Rm)),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(Rd)),
                                    Operand::Reg(Reg::from_u8(Rn)),
                                    Operand::Reg(Reg::from_u8(Rm)),
                                    Operand::Nothing,
                                ];
                            }
                        } else {
                            if decoder.should_is_must {
                                if opcode == 0b1101 && Rn != 0 {
                                    // Rn "should" be zero
                                    return Err(ErrorKind::Nonconforming);
                                }
                            }

                            inst.operands = [
                                Operand::Reg(Reg::from_u8(Rd)),
                                Operand::Reg(Reg::from_u8(Rn)),
                                Operand::RegShift(RegShift::from_raw(shift_spec)),
                                Operand::Nothing,
                            ];
                        }
                    } else {
                        //    known 0 because it and bit 5 are not both 1 --v
                        // |c o n d|0 0 0|1 0 x x|0|x x x x|x x x x|x x x x 0|x x|1|x x x x|
                        // interpret the operands as
                        // | Rn | Rd | Rs | 0 | shift | 1 | Rm |
                        let (Rn, Rd, shift_spec) = {
                            let shift_spec = (word & 0xfff) as u16;
                            let word = word >> 12;
                            let Rd = (word & 0x0f) as u8;
                            let Rn = ((word >> 4) & 0x0f) as u8;
                            (Rn, Rd, shift_spec)
                        };
                        // page A5-200 indicates that saturating add and subtract should be
                        // here?
                        if (0b1101 & opcode) == 0b1101 {
                            // these are all invalid
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOpcode);
                        } else {
                            // TODO: unsure about this RegShift...
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(Rd)),
                                Operand::Reg(Reg::from_u8(Rn)),
                                Operand::RegShift(RegShift::from_raw(shift_spec)),
                                Operand::Nothing,
                            ];
                        }
                    }
                }
            }
        }
        0b001 => {
            // the instruction looks like
            // |c o n d|0 0 1|x x x x|x|x x x x|x x x x|x x x x x|x x|x|x x x x|
            // bottom part of table A5-2 on page A5-194
            let (s, opcode) = {
                let part = word >> 20;
                ((part & 0x01) == 1, ((part >> 1) & 0x0f) as u8)
            };
            if s == false && opcode >= 0b1000 && opcode < 0b1100 {
                // the instruction looks like
                // |c o n d|0 0 1|1 0 x x|0|x x x x|x x x x|x x x x x|x x|x|x x x x|
                // which means 16-bit immediate load, high half immediate load, or MSR (immediate)
                // + hints.
                // See table A5-2, page A5-194.
                match opcode & 0b0011 {
                    0b00 => {
                        // 16-bit immediate load, MOV (immediate) on page A8-485
                        inst.opcode = Opcode::MOV;
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(((word >> 12) & 0b1111) as u8)),
                            Operand::Imm32(((word >> 4) & 0xf000) | (word & 0xfff)),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    }
                    0b10 => {
                        // High halfword 16-bit immediate load, MOVT on page A8-492
                        inst.opcode = Opcode::MOVT;
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(((word >> 12) & 0b1111) as u8)),
                            Operand::Imm32(((word >> 4) & 0xf000) | (word & 0xfff)),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    }
                    0b01 => {
                        // MSR (immediate), and hints on page A5-204, op==0
                        let op1 = (word >> 16) & 0b1111;
                        match op1 {
                            0b0000 => {}
                            _ => {
                                // Move to Special register, Application level MSR (immediate) on
                                // page A8-499
                                if decoder.should_is_must {
                                    if (word >> 12) & 0b1111 != 0b1111 {
                                        return Err(ErrorKind::Nonconforming);
                                    }
                                }
                                inst.operands = [
                                    Operand::StatusRegMask(StatusRegMask::from_raw(op1 as u8)?),
                                    Operand::Imm32(word & 0xfff),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                                inst.opcode = Opcode::MSR;
                            }
                        }
                    }
                    0b11 => {
                        // MSR (immediate), and hints on page A5-204, op==1
                        if decoder.should_is_must {
                            if (word >> 12) & 0b1111 != 0b1111 {
                                return Err(ErrorKind::Nonconforming);
                            }
                        }
                        inst.operands = [
                            Operand::StatusRegMask(StatusRegMask::from_raw(
                                (word >> 16) as u8 & 0b1111 | 0b10000,
                            )?),
                            Operand::Imm32(word & 0xfff),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                        inst.opcode = Opcode::MSR;
                    }
                    _ => {
                        unreachable!("impossible bit pattern");
                    }
                }
            } else {
                // Data-processing (immediate)
                // Page A5-197
                if opcode >= 16 {
                    unreachable!();
                }
                inst.opcode = DATA_PROCESSING_OPCODES[opcode as usize];
                inst.set_s(s);

                let (Rn, Rd, imm) = {
                    let rot = word & 0x00000f00;
                    let imm = word & 0x000000ff;
                    let imm = (imm as u32).rotate_right(2 * (rot >> 8));
                    ((word >> 16) as u8 & 0x0f, (word >> 12) as u8 & 0x0f, imm)
                };
                if (opcode == 0b0010 || opcode == 0b0100) && Rn == 0b1111 {
                    inst.opcode = Opcode::ADR;
                }
                match opcode {
                    0b1101 => {
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(Rd)),
                            Operand::Imm32(imm),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    }
                    _ => {
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(Rd)),
                            Operand::Reg(Reg::from_u8(Rn)),
                            Operand::Imm32(imm),
                            Operand::Nothing,
                        ];
                    }
                }
            }
            /* ... */
        }
        0b010 => {
            // Load/store word and unsigned byte
            // A5.3
            // Page A5-206
            let Rn = ((word >> 16) & 0x0f) as u8;
            let op = ((word >> 20) & 0x1f) as u8;
            let add = (op & 0b01000) != 0;
            let (imm, Rt) = { ((word & 0x0fff) as u16, ((word >> 12) & 0x0f) as u8) };
            if (op & 0b10010) == 0b00010 {
                let op = op & 0b00111;
                // |c o n d|0 1 0|0 x x 1 x|x x x x x x x x x x x x x x x|x|x x x x|
                /*
                0x010 -> STRT
                0x011 -> LDRT
                0x110 -> STRBT
                0x111 -> LDRBT
                */
                inst.opcode = match op {
                    0b010 => Opcode::STRT,
                    0b011 => Opcode::LDRT,
                    0b110 => Opcode::STRBT,
                    0b111 => Opcode::LDRBT,
                    _ => {
                        unreachable!();
                    }
                };
                inst.operands = [
                    Operand::Reg(Reg::from_u8(Rt)),
                    Operand::RegDerefPostindexOffset(Reg::from_u8(Rn), imm as u16, add, false),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else {
                /*
                xx0x0 not 0x010 -> STR (imm)
                xx0x1 not 0x011 -> LDR (imm)
                xx1x0 not 0x110 -> STRB (imm)
                xx1x1 not 0x111 -> LDRB (imm)
                */
                let P = (op & 0b10000) != 0;
                let W = (op & 0b00010) != 0;
                let op = op & 0b00101;
                inst.opcode = if !P && W {
                    // Table A5-15
                    // strt, ldrt, strbt, ldrbt
                    match op {
                        0b000 => Opcode::STRT,
                        0b001 => Opcode::LDRT,
                        0b100 => Opcode::STRBT,
                        0b101 => Opcode::LDRBT,
                        _ => {
                            unreachable!("bad bit pattern, table A5-15");
                        }
                    }
                } else {
                    match op {
                        0b000 => Opcode::STR,
                        0b001 => {
                            if Rn == 0b1111 {
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(Rt)),
                                    if P {
                                        Operand::RegDerefPreindexOffset(
                                            Reg::from_u8(Rn),
                                            imm as u16,
                                            add,
                                            W,
                                        )
                                    } else {
                                        Operand::RegDerefPostindexOffset(
                                            Reg::from_u8(Rn),
                                            imm as u16,
                                            add,
                                            W,
                                        )
                                    },
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                                inst.opcode = Opcode::LDR;
                                return Ok(());
                            }
                            Opcode::LDR
                        }
                        0b100 => Opcode::STRB,
                        0b101 => {
                            if Rn == 0b1111 {
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(Rt)),
                                    if P {
                                        Operand::RegDerefPreindexOffset(
                                            Reg::from_u8(Rn),
                                            imm as u16,
                                            add,
                                            W,
                                        )
                                    } else {
                                        Operand::RegDerefPostindexOffset(
                                            Reg::from_u8(Rn),
                                            imm as u16,
                                            add,
                                            W,
                                        )
                                    },
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                                inst.opcode = Opcode::LDRB;
                                return Ok(());
                            }
                            Opcode::LDRB
                        }
                        _ => {
                            unreachable!();
                        }
                    }
                };
                inst.operands = [
                    Operand::Reg(Reg::from_u8(Rt)),
                    if P {
                        Operand::RegDerefPreindexOffset(Reg::from_u8(Rn), imm as u16, add, W)
                    } else {
                        Operand::RegDerefPostindexOffset(Reg::from_u8(Rn), imm as u16, add, W)
                    },
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            }
        }
        0b011 => {
            // page A5-192 to distinguish the following:
            // check for media instructions, and if not, load/store word and unsigned byte
            if (word & 0x00000010) != 0 {
                // |c o n d|0 1 1|x x x x|x|x x x x|x x x x|x x x x x|x x|1|x x x x|
                // using language from A5-206: A == 1 and B == 1
                // so this is media instructions (A5-207)
                return Err(ErrorKind::Incomplete);
            } else {
                // |c o n d|0 1 1|x x x x|x|x x x x|x x x x|x x x x x|x x|0|x x x x|
                // instructions here are A == 1, B == 0 in A5-206
                let op = ((word >> 20) & 0x1f) as u8;

                let P = (op & 0b10000) != 0;
                let U = (op & 0b01000) != 0;
                let W = (op & 0b00010) != 0;
                /*
                    xx0x0 not 0x010 -> STR (register)
                    0x010 -> STRT
                    xx0x1 not 0x011 -> LDR (register)
                    0x011 -> LDRT
                    xx1x0 not 0x110 -> STRB (register)
                    0x110 -> STRBT
                    xx1x1 not 0x111 -> LDRB (register)
                    0x111 -> LDRBT
                */
                let _Rn = ((word >> 16) & 0x0f) as u8;
                if (op & 0b10010) == 0b00010 {
                    let op = op & 0b00111;
                    // |c o n d|0 1 1|0 x x 1 x|x x x x x x x x x x x x x x x|0|x x x x|
                    /*
                    0x010 -> STRT
                    0x011 -> LDRT
                    0x110 -> STRBT
                    0x111 -> LDRBT
                    */
                    inst.opcode = match op {
                        0b010 => Opcode::STRT,
                        0b011 => Opcode::LDRT,
                        0b110 => Opcode::STRBT,
                        0b111 => Opcode::LDRBT,
                        _ => {
                            unreachable!();
                        }
                    };
                } else {
                    /*
                    xx0x0 not 0x010 -> STR (imm)
                    xx0x1 not 0x011 -> LDR (imm)
                    xx1x0 not 0x110 -> STRB (imm)
                    xx1x1 not 0x111 -> LDRB (imm)
                    */
                    let op = op & 0b00101;
                    inst.opcode = match op {
                        0b000 => Opcode::STR,
                        0b001 => Opcode::LDR,
                        0b100 => Opcode::STRB,
                        0b101 => Opcode::LDRB,
                        _ => {
                            unreachable!();
                        }
                    };
                }
                let (Rt, shift) = {
                    let shift = (word & 0xfff) as u16;
                    let word = word >> 12;
                    let Rt = (word & 0xf) as u8;
                    (Rt, shift)
                };
                let Rn = (word >> 16) as u8 & 0b1111;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(Rt)),
                    if P {
                        Operand::RegDerefPreindexRegShift(
                            Reg::from_u8(Rn),
                            RegShift::from_raw(shift),
                            U,
                            W,
                        )
                    } else {
                        Operand::RegDerefPostindexRegShift(
                            Reg::from_u8(Rn),
                            RegShift::from_raw(shift),
                            U,
                            false,
                        )
                    },
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            }
            return Ok(());
        }
        0b100 | 0b101 => {
            // branch, branch with link, and block data transfer
            // page A5-212
            let op = (word >> 20) & 0x3f;
            if op < 0b100000 {
                let wback = (op & 0b000010) != 0;
                let add = (op & 0b001000) != 0;
                let pre = (op & 0b010000) != 0;
                let usermode = (op & 0b000100) != 0;
                inst.opcode = if (op & 1) == 0 {
                    Opcode::STM(add, pre, false, usermode)
                } else {
                    Opcode::LDM(add, pre, false, usermode)
                };
                inst.operands = [
                    Operand::RegWBack(Reg::from_u8(((word >> 16) & 0xf) as u8), wback),
                    Operand::RegList((word & 0xffff) as u16),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if op < 0b110000 {
                // 10xxxx
                inst.opcode = Opcode::B;

                // the + 2 is to compensate for an architecturally-defined initial offset
                let imm24 = ((((word & 0x00ff_ffff) + 2) << 8) as i32) >> 8;

                inst.operands = [
                    Operand::BranchOffset(imm24),
                    Operand::Nothing,
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else {
                // 11xxxx

                // the + 2 is to compensate for an architecturally-defined initial offset
                let imm24 = ((((word & 0x00ff_ffff) + 2) << 8) as i32) >> 8;

                inst.opcode = Opcode::BL;
                inst.operands = [
                    Operand::BranchOffset(imm24),
                    Operand::Nothing,
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            }
        }
        0b110 | 0b111 => {
            // coprocessor instructions and supervisor call
            // page A5-213
            // low bit of 0b110 or 0b111 corresponds to high bit of op1
            return Err(ErrorKind::Incomplete);
        }
        _ => {
            unreachable!("opc category is three bits");
        }
    }
    Ok(())
}

/*
 * tests: (armv7?)
 * [0x00, 0x00, 0x90, 0xe0]
 * adds r0, r0, r0
 *
 * [0x00, 0x00, 0x82, 0xe0]
 * add r0, r2, r0
 *
 * [0x00, 0x00, 0x88, 0xe0]
 * add r0, r8, r0
 *
 * [0x00, 0x01, 0x80, 0xe0]
 * add r0, r0, r0, lsl 2
 *
 * [0x00, 0x80, 0x00, 0x00]
 * andeq r8, r0, r0
 *
 * [0xc0, 0x80, 0x20, 0x00]
 * eoreq r8, r0, r0, asr 1
 *
 * [0x00, 0x00, 0xa2, 0xe1]
 * mov r0, r0
 *
 * [0x00, 0x00, 0xaf, 0xe1]
 * mov r0, r0
 *
 * [0x10, 0x00, 0xaf, 0xe1]
 * invalid
 *
 * [0x01, 0x00, 0xaf, 0xe1]
 * mov r0, r1
 *
 * [0x00, 0x01, 0xaf, 0xe1]
 * invalid
 *
 * [0x00, 0x00, 0xa0, 0xe1]
 * mov r0, r0
 *
 * [0x00, 0x01, 0xa0, 0xe1]
 * lsl r0, r0, 2
 * # is this equivalent to mov r0, r0<<2?
 * 0180afe1 invalid
 * 018076e1 cmn r6, r1
 * 015096e1 orrs r5, r6, r1
 * 018086e1 orr r8, r6, r1
 * 0180a6e1 invalid
 * 0180d6e1 bics r8, r6, r1
 * 0180d0e1 bics r8, r0, r1
 * 8110d0e1 bics r1, r0, r1, lsl 1
 * 1200dfe1 bics r0, pc, r2, lsl r0
 * f110d0e1 ldrsh r1, [r0, 1]
 * 0101a0e1 lsl r0, r1, 2
 * 0110a0e1 mov r1, r1
 * 0111a0e1 lsl r1, r1, 2
 * 4110a0e1 asr r1, r1, 0x20
 * 2110a0e1 lsr r1, r1, 0x20
 *
 */
