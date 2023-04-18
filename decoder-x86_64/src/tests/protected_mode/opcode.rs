use crate::protected_mode::{ConditionCode, Opcode};

#[test]
fn conditional_instructions() {
    const JCC: &'static [(Opcode, ConditionCode); 16] = &[
        (Opcode::JO, ConditionCode::O),
        (Opcode::JNO, ConditionCode::NO),
        (Opcode::JB, ConditionCode::B),
        (Opcode::JNB, ConditionCode::AE),
        (Opcode::JZ, ConditionCode::Z),
        (Opcode::JNZ, ConditionCode::NZ),
        (Opcode::JA, ConditionCode::A),
        (Opcode::JNA, ConditionCode::BE),
        (Opcode::JS, ConditionCode::S),
        (Opcode::JNS, ConditionCode::NS),
        (Opcode::JP, ConditionCode::P),
        (Opcode::JNP, ConditionCode::NP),
        (Opcode::JL, ConditionCode::L),
        (Opcode::JGE, ConditionCode::GE),
        (Opcode::JG, ConditionCode::G),
        (Opcode::JLE, ConditionCode::LE),
    ];
    for (opc, cond) in JCC.iter() {
        assert!(opc.is_jcc());
        assert!(!opc.is_setcc());
        assert!(!opc.is_cmovcc());
        assert_eq!(opc.condition(), Some(*cond));
    }

    const SETCC: &'static [(Opcode, ConditionCode); 16] = &[
        (Opcode::SETO, ConditionCode::O),
        (Opcode::SETNO, ConditionCode::NO),
        (Opcode::SETB, ConditionCode::B),
        (Opcode::SETAE, ConditionCode::AE),
        (Opcode::SETZ, ConditionCode::Z),
        (Opcode::SETNZ, ConditionCode::NZ),
        (Opcode::SETA, ConditionCode::A),
        (Opcode::SETBE, ConditionCode::BE),
        (Opcode::SETS, ConditionCode::S),
        (Opcode::SETNS, ConditionCode::NS),
        (Opcode::SETP, ConditionCode::P),
        (Opcode::SETNP, ConditionCode::NP),
        (Opcode::SETL, ConditionCode::L),
        (Opcode::SETGE, ConditionCode::GE),
        (Opcode::SETG, ConditionCode::G),
        (Opcode::SETLE, ConditionCode::LE),
    ];
    for (opc, cond) in SETCC.iter() {
        assert!(!opc.is_jcc());
        assert!(opc.is_setcc());
        assert!(!opc.is_cmovcc());
        assert_eq!(opc.condition(), Some(*cond));
    }

    const CMOVCC: &'static [(Opcode, ConditionCode); 16] = &[
        (Opcode::CMOVO, ConditionCode::O),
        (Opcode::CMOVNO, ConditionCode::NO),
        (Opcode::CMOVB, ConditionCode::B),
        (Opcode::CMOVNB, ConditionCode::AE),
        (Opcode::CMOVZ, ConditionCode::Z),
        (Opcode::CMOVNZ, ConditionCode::NZ),
        (Opcode::CMOVA, ConditionCode::A),
        (Opcode::CMOVNA, ConditionCode::BE),
        (Opcode::CMOVS, ConditionCode::S),
        (Opcode::CMOVNS, ConditionCode::NS),
        (Opcode::CMOVP, ConditionCode::P),
        (Opcode::CMOVNP, ConditionCode::NP),
        (Opcode::CMOVL, ConditionCode::L),
        (Opcode::CMOVGE, ConditionCode::GE),
        (Opcode::CMOVG, ConditionCode::G),
        (Opcode::CMOVLE, ConditionCode::LE),
    ];
    for (opc, cond) in CMOVCC.iter() {
        assert!(!opc.is_jcc());
        assert!(!opc.is_setcc());
        assert!(opc.is_cmovcc());
        assert_eq!(opc.condition(), Some(*cond));
    }
}
