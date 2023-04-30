use crate::protected_mode::{read_E_vex, read_imm_unsigned, read_modrm};
use crate::protected_mode::{Instruction, Opcode, RegSpec, RegisterBank};
use crate::Error;

const DEFAULT_EVEX_REGISTER_SIZE: RegisterBank = RegisterBank::D;
const DEFAULT_EVEX_REGISTER_WIDTH: u8 = 4;

fn isa_has_qwords() -> bool {
    false
}

include!("../shared/generated_evex.in");
include!("../shared/evex.in");
