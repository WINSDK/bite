use crate::long_mode::{read_E_vex, read_imm_unsigned, read_modrm};
use crate::long_mode::{Instruction, Opcode, RegSpec, RegisterBank};
use crate::Error;

const DEFAULT_EVEX_REGISTER_SIZE: RegisterBank = RegisterBank::Q;
const DEFAULT_EVEX_REGISTER_WIDTH: u8 = 8;

fn isa_has_qwords() -> bool {
    true
}

include!("../shared/generated_evex.in");
include!("../shared/evex.in");
