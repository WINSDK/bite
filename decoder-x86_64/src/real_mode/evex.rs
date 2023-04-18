// use crate::long_mode::{OperandSpec, DecodeError, RegSpec, RegisterBank, Instruction, Opcode};
use crate::real_mode::{Arch, DecodeError, RegSpec, RegisterBank, Instruction, Opcode};
use crate::real_mode::{read_modrm, read_E_vex, read_imm_unsigned};
use yaxpeax_arch::Reader;

const DEFAULT_EVEX_REGISTER_SIZE: RegisterBank = RegisterBank::D;
const DEFAULT_EVEX_REGISTER_WIDTH: u8 = 4;

fn isa_has_qwords() -> bool {
    false
}

include!("../shared/generated_evex.in");
include!("../shared/evex.in");
