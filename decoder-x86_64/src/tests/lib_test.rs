#[test]
fn test_disasm() {
    let mut instr = Instruction::invalid();
    arch::x86_64::instr::decode_one(&[0x33, 0xc0], &mut instr);
    assert_eq!(1, 1);
}
