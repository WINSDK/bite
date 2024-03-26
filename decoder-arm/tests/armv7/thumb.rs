use arm::armv7::{ConditionCode, Instruction, Opcode, Operand, Reg, RegShift};
use decoder::{ErrorKind, Decodable, Decoded, Reader};

type InstDecoder = arm::armv7::Decoder;

#[allow(dead_code)]
fn test_invalid_under(decoder: &InstDecoder, data: &[u8]) {
    let mut reader = Reader::new(&data[..]);
    match decoder.decode(&mut reader) {
        Err(_) => { },
        Ok(inst) => {
            panic!(
                "unexpected successful decode for {:#x?}\ngot: {}",
                data,
                inst
            );
        }
    }
}

#[allow(dead_code)]
fn test_display_under(decoder: &InstDecoder, data: [u8; 4], expected: &'static str) {
    let mut reader = Reader::new(&data[..]);
    let instr = match decoder.decode(&mut reader) {
        Err(e) => {
            panic!("failed to decode {:#x?}: {:?}", data, e)
        }
        Ok(instr) => instr,
    };
    let displayed = format!("{}", instr);
    assert!(
        displayed == expected,
        "decode error for {:#x?}:\n  decoded: {:?}\n expected: {:?}\n",
        data,
        displayed, expected
    );
}

#[allow(dead_code)]
fn test_decode(data: &[u8], expected: Instruction) {
    let mut reader = Reader::new(data);
    let instr = match InstDecoder::default_thumb().decode(&mut reader) {
        Err(e) => {
            panic!("failed to decode {:#x?}: {:?}", data, e)
        }
        Ok(instr) => instr,
    };
    assert!(
        instr == expected,
        "decode error for {:#x?}:\n  decoded: {:?}\n expected: {:?}\n",
        data,
        instr, expected
    );
}

#[allow(dead_code)]
fn test_invalid(data: &[u8]) {
   test_invalid_under(&InstDecoder::default_thumb(), data);
}

fn test_display(data: &[u8], expected: &'static str) {
    let mut reader = Reader::new(data);
    let instr = match InstDecoder::default_thumb().decode(&mut reader) {
        Err(e) => {
            panic!("failed to decode {:#x?}: {:?}", data, e)
        }
        Ok(instr) => instr,
    };
    let text = format!("{}", instr);
    assert!(
        text == expected,
        "display error for {:#x?}\n  decoded: {:?}\n displayed: {}\n expected: {}\n",
        data,
        instr,
        text, expected
    );
}

#[test]
fn test_unpredictable_instructions() {
    test_invalid(&[0x80, 0xfa, 0x40, 0x00]);
}

#[test]
fn test_decode_add_cases() {
    test_display(
        &[0x01, 0x44],
        "add r1, r0"
    );
    test_display(
        &[0x01, 0xa8],
        "add r0, sp, 0x4"
    );
    test_display(
        &[0x01, 0xa9],
        "add r1, sp, 0x4"
    );
    test_display(
        &[0x01, 0xaa],
        "add r2, sp, 0x4"
    );
    test_display(
        &[0x01, 0xab],
        "add r3, sp, 0x4"
    );
    test_display(
        &[0x01, 0xad],
        "add r5, sp, 0x4"
    );
    test_display(
        &[0x01, 0xae],
        "add r6, sp, 0x4"
    );
    test_display(
        &[0x01, 0xaf],
        "add r7, sp, 0x4"
    );
    test_display(
        &[0x05, 0xac],
        "add r4, sp, 0x14"
    );
    test_display(
        &[0x61, 0xb0],
        "add sp, sp, 0x184"
    );
    test_display(
        &[0x01, 0xb0],
        "add sp, sp, 0x4"
    );
    test_display(
        &[0x02, 0x44],
        "add r2, r0"
    );
    test_display(
        &[0x02, 0xb0],
        "add sp, sp, 0x8"
    );
    test_display(
        &[0x03, 0x44],
        "add r3, r0"
    );
    test_display(
        &[0x17, 0x44],
        "add r7, r2"
    );
    test_display(
        &[0x1b, 0x44],
        "add r3, r3"
    );
    test_display(
        &[0x54, 0x44],
        "add r4, r10"
    );
    test_display(
        &[0x57, 0x44],
        "add r7, r10"
    );
    test_display(
        &[0x5a, 0x44],
        "add r2, fp"
    );
    test_display(
        &[0x61, 0x44],
        "add r1, ip"
    );
    test_display(
        &[0x68, 0x44],
        "add r0, sp"
    );
    test_display(
        &[0x69, 0x44],
        "add r1, sp"
    );
    test_display(
        &[0x6a, 0x44],
        "add r2, sp"
    );
    test_display(
        &[0x6b, 0x44],
        "add r3, sp"
    );
    test_display(
        &[0x6d, 0x44],
        "add r5, sp"
    );
    test_display(
        &[0x6e, 0x44],
        "add r6, sp"
    );
    test_display(
        &[0x6f, 0x44],
        "add r7, sp"
    );
    test_display(
        &[0x75, 0x44],
        "add r5, lr"
    );
    test_display(
        &[0x79, 0x44],
        "add r1, pc"
    );
    test_display(
        &[0xc0, 0x44],
        "add r8, r8"
    );
    test_display(
        &[0xc1, 0x44],
        "add sb, r8"
    );
    test_display(
        &[0xc2, 0x44],
        "add r10, r8"
    );
    test_display(
        &[0xc3, 0x44],
        "add fp, r8"
    );
    test_display(
        &[0xc4, 0x44],
        "add ip, r8"
    );
    test_display(
        &[0xc5, 0x44],
        "add sp, r8"
    );
    test_display(
        &[0xc6, 0x44],
        "add lr, r8"
    );
    test_display(
        &[0xc7, 0x44],
        "add pc, r8"
    );
    test_display(
        &[0xc8, 0x44],
        "add r8, sb"
    );
    test_display(
        &[0xc9, 0x44],
        "add sb, sb"
    );
    test_display(
        &[0xca, 0x44],
        "add r10, sb"
    );
    test_display(
        &[0xcb, 0x44],
        "add fp, sb"
    );
    test_display(
        &[0xcc, 0x44],
        "add ip, sb"
    );
    test_display(
        &[0xcd, 0x44],
        "add sp, sb"
    );
    test_display(
        &[0xce, 0x44],
        "add lr, sb"
    );
    test_display(
        &[0xcf, 0x44],
        "add pc, sb"
    );
    test_display(
        &[0xd0, 0x44],
        "add r8, r10"
    );
    test_display(
        &[0xd1, 0x44],
        "add sb, r10"
    );
    test_display(
        &[0xd2, 0x44],
        "add r10, r10"
    );
    test_display(
        &[0xd3, 0x44],
        "add fp, r10"
    );
    test_display(
        &[0xd4, 0x44],
        "add ip, r10"
    );
    test_display(
        &[0xd5, 0x44],
        "add sp, r10"
    );
    test_display(
        &[0xd6, 0x44],
        "add lr, r10"
    );
    test_display(
        &[0xd7, 0x44],
        "add pc, r10"
    );
    test_display(
        &[0xd8, 0x44],
        "add r8, fp"
    );
    test_display(
        &[0xd9, 0x44],
        "add sb, fp"
    );
    test_display(
        &[0xda, 0x44],
        "add r10, fp"
    );
    test_display(
        &[0xdb, 0x44],
        "add fp, fp"
    );
    test_display(
        &[0xdc, 0x44],
        "add ip, fp"
    );
    test_display(
        &[0xdd, 0x44],
        "add sp, fp"
    );
    test_display(
        &[0xde, 0x44],
        "add lr, fp"
    );
    test_display(
        &[0xdf, 0x44],
        "add pc, fp"
    );
    test_display(
        &[0xe0, 0x44],
        "add r8, ip"
    );
    test_display(
        &[0xe1, 0x44],
        "add sb, ip"
    );
    test_display(
        &[0xe2, 0x44],
        "add r10, ip"
    );
    test_display(
        &[0xe3, 0x44],
        "add fp, ip"
    );
    test_display(
        &[0xe4, 0x44],
        "add ip, ip"
    );
    test_display(
        &[0xe5, 0x44],
        "add sp, ip"
    );
    test_display(
        &[0xe6, 0x44],
        "add lr, ip"
    );
    test_display(
        &[0xe7, 0x44],
        "add pc, ip"
    );
    test_display(
        &[0xe8, 0x44],
        "add r8, sp"
    );
    test_display(
        &[0xe9, 0x44],
        "add sb, sp"
    );
    test_display(
        &[0xea, 0x44],
        "add r10, sp"
    );
    test_display(
        &[0xeb, 0x44],
        "add fp, sp"
    );
    test_display(
        &[0xec, 0x44],
        "add ip, sp"
    );
    test_display(
        &[0xed, 0x44],
        "add sp, sp"
    );
    test_display(
        &[0xee, 0x44],
        "add lr, sp"
    );
    test_display(
        &[0xef, 0x44],
        "add pc, sp"
    );
    test_display(
        &[0xf0, 0x44],
        "add r8, lr"
    );
    test_display(
        &[0xf1, 0x44],
        "add sb, lr"
    );
    test_display(
        &[0xf2, 0x44],
        "add r10, lr"
    );
    test_display(
        &[0xf3, 0x44],
        "add fp, lr"
    );
    test_display(
        &[0xf4, 0x44],
        "add ip, lr"
    );
    test_display(
        &[0xf5, 0x44],
        "add sp, lr"
    );
    test_display(
        &[0xf6, 0x44],
        "add lr, lr"
    );
    test_display(
        &[0xf7, 0x44],
        "add pc, lr"
    );
    test_display(
        &[0xf8, 0x44],
        "add r8, pc"
    );
    test_display(
        &[0xf9, 0x44],
        "add sb, pc"
    );
    test_display(
        &[0xfa, 0x44],
        "add r10, pc"
    );
    test_display(
        &[0xfb, 0x44],
        "add fp, pc"
    );
    test_display(
        &[0xfc, 0x44],
        "add ip, pc"
    );
    test_display(
        &[0xfd, 0x44],
        "add sp, pc"
    );
    test_display(
        &[0xfe, 0x44],
        "add lr, pc"
    );
    test_display(
        &[0xff, 0x44],
        "add pc, pc"
    );
}
#[test]
fn test_decode_adr_cases() {
    test_display(
        &[0x00, 0xa3],
        "adr r3, 0x0"
    );
    test_display(
        &[0x28, 0xa7],
        "adr r7, 0xa0"
    );
    test_display(
        &[0x29, 0xa0],
        "adr r0, 0xa4"
    );
    test_display(
        &[0xff, 0xa6],
        "adr r6, 0x3fc"
    );
    test_display(
        &[0xff, 0xa7],
        "adr r7, 0x3fc"
    );
    test_display(
        &[0x0f, 0xf2, 0x4f, 0x56],
        "add r6, pc, 0x54f"
    );
    test_display(
        &[0xaf, 0xf2, 0x4f, 0x56],
        "sub r6, pc, 0x54f"
    );
}
#[test]
fn test_decode_bcc_cases() {
    test_display(
        &[0xfe, 0xe7],
        "b $-0x4"
    );
    test_display(
        &[0x80, 0x47],
        "blx r0"
    );
    test_display(
        &[0x88, 0x47],
        "blx r1"
    );
    test_display(
        &[0x90, 0x47],
        "blx r2"
    );
    test_display(
        &[0x98, 0x47],
        "blx r3"
    );
    test_display(
        &[0xa0, 0x47],
        "blx r4"
    );
    test_display(
        &[0xa8, 0x47],
        "blx r5"
    );
    test_display(
        &[0xb0, 0x47],
        "blx r6"
    );
    test_display(
        &[0xb8, 0x47],
        "blx r7"
    );
    test_display(
        &[0xc0, 0x47],
        "blx r8"
    );
    test_display(
        &[0xc8, 0x47],
        "blx sb"
    );
    test_display(
        &[0xd0, 0x47],
        "blx r10"
    );
    test_display(
        &[0xd8, 0x47],
        "blx fp"
    );
    test_display(
        &[0xe0, 0x47],
        "blx ip"
    );
    test_display(
        &[0xe8, 0x47],
        "blx sp"
    );
    test_display(
        &[0xf0, 0x47],
        "blx lr"
    );
    test_display(
        &[0xf8, 0x47],
        "blx pc"
    );
    test_display(
        &[0xfe, 0xd0],
        "beq $-0x2"
    );
    test_display(
        &[0xfe, 0xd1],
        "bne $-0x2"
    );
    test_display(
        &[0xfe, 0xd2],
        "bhs $-0x2"
    );
    test_display(
        &[0xfe, 0xd3],
        "blo $-0x2"
    );
    test_display(
        &[0xfe, 0xd4],
        "bmi $-0x2"
    );
    test_display(
        &[0xfe, 0xd5],
        "bpl $-0x2"
    );
    test_display(
        &[0xfe, 0xd6],
        "bvs $-0x2"
    );
    test_display(
        &[0xfe, 0xd7],
        "bvc $-0x2"
    );
    test_display(
        &[0xfe, 0xd8],
        "bhi $-0x2"
    );
    test_display(
        &[0xfe, 0xd9],
        "bls $-0x2"
    );
    test_display(
        &[0xfe, 0xda],
        "bge $-0x2"
    );
    test_display(
        &[0xfe, 0xdb],
        "blt $-0x2"
    );
    test_display(
        &[0xfe, 0xdc],
        "bgt $-0x2"
    );
    test_display(
        &[0xfe, 0xdd],
        "ble $-0x2"
    );
    test_display(
        &[0xd3, 0xd0],
        "beq $-0x58"
    );
    test_display(
        &[0xd3, 0xd1],
        "bne $-0x58"
    );
    test_display(
        &[0xd3, 0xd2],
        "bhs $-0x58"
    );
    test_display(
        &[0xd3, 0xd3],
        "blo $-0x58"
    );
    test_display(
        &[0xd3, 0xd4],
        "bmi $-0x58"
    );
    test_display(
        &[0xd3, 0xd5],
        "bpl $-0x58"
    );
    test_display(
        &[0xd3, 0xd6],
        "bvs $-0x58"
    );
    test_display(
        &[0xd3, 0xd7],
        "bvc $-0x58"
    );
    test_display(
        &[0xd3, 0xd8],
        "bhi $-0x58"
    );
    test_display(
        &[0xd3, 0xd9],
        "bls $-0x58"
    );
    test_display(
        &[0xd3, 0xda],
        "bge $-0x58"
    );
    test_display(
        &[0xd3, 0xdb],
        "blt $-0x58"
    );
    test_display(
        &[0xd3, 0xdc],
        "bgt $-0x58"
    );
    test_display(
        &[0xd3, 0xdd],
        "ble $-0x58"
    );
    test_display(
        &[0xfd, 0xd0],
        "beq $-0x4"
    );
    test_display(
        &[0xfd, 0xd1],
        "bne $-0x4"
    );
    test_display(
        &[0xfd, 0xd2],
        "bhs $-0x4"
    );
    test_display(
        &[0xfd, 0xd3],
        "blo $-0x4"
    );
    test_display(
        &[0xfd, 0xd4],
        "bmi $-0x4"
    );
    test_display(
        &[0xfd, 0xd5],
        "bpl $-0x4"
    );
    test_display(
        &[0xfd, 0xd6],
        "bvs $-0x4"
    );
    test_display(
        &[0xfd, 0xd7],
        "bvc $-0x4"
    );
    test_display(
        &[0xfd, 0xd8],
        "bhi $-0x4"
    );
    test_display(
        &[0xfd, 0xd9],
        "bls $-0x4"
    );
    test_display(
        &[0xfd, 0xda],
        "bge $-0x4"
    );
    test_display(
        &[0xfd, 0xdb],
        "blt $-0x4"
    );
    test_display(
        &[0xfd, 0xdc],
        "bgt $-0x4"
    );
    test_display(
        &[0xfd, 0xdd],
        "ble $-0x4"
    );
}
#[test]
fn test_decode_32b_branch_cases() {
    test_display(
        &[0xf3, 0xf7, 0x7c, 0xbe],
        "b.w $-0xc308"
    );
    test_display(
        &[0x0c, 0xf0, 0x84, 0xb9],
        "b.w $+0xc308"
    );
    test_display(
        &[0x00, 0xf4, 0x00, 0x90],
        "b.w $-0x1000000"
    );
    test_display(
        &[0xff, 0xf3, 0xff, 0x97],
        "b.w $+0xfffffe"
    );
    test_display(
        &[0x00, 0xf4, 0x00, 0xc0],
        "blx.w $-0x1000000"
    );
    test_display(
        &[0xff, 0xf3, 0xfe, 0xc7],
        "blx.w $+0xfffffc"
    );
    test_display(
        &[0x00, 0xf4, 0x00, 0xd0],
        "bl.w $-0x1000000"
    );
    test_display(
        &[0xff, 0xf3, 0xff, 0xd7],
        "bl.w $+0xfffffe"
    );
    test_display(
        &[0x3f, 0xf4, 0xfe, 0xaf],
        "b.weq $-0x4"
    );
    // Test for yaxpeax-arm #3
    // branch target was 0x12198 in initial test case at offset 0x82fc
    // 0x12198 - 0x82fc - 0x4 (offsets taken after IP already incremented)
    test_display(
        &[0x09, 0xf0, 0x4c, 0xbf],
        "b.w $+0x9e98"
    );
}
#[test]
fn test_decode_bkpt_cases() {
    test_display(
        &[0x00, 0xbe],
        "bkpt 0x0"
    );
    test_display(
        &[0x75, 0xbe],
        "bkpt 0x75"
    );
    test_display(
        &[0xff, 0xbe],
        "bkpt 0xff"
    );
}
#[test]
fn test_decode_bx_cases() {
    test_display(
        &[0x00, 0x47],
        "bx r0"
    );
    test_display(
        &[0x01, 0x47],
        "bx r0"
    );
    test_display(
        &[0x02, 0x47],
        "bx r0"
    );
    test_display(
        &[0x03, 0x47],
        "bx r0"
    );
    test_display(
        &[0x04, 0x47],
        "bx r0"
    );
    test_display(
        &[0x05, 0x47],
        "bx r0"
    );
    test_display(
        &[0x06, 0x47],
        "bx r0"
    );
    test_display(
        &[0x07, 0x47],
        "bx r0"
    );
    test_display(
        &[0x08, 0x47],
        "bx r1"
    );
    test_display(
        &[0x09, 0x47],
        "bx r1"
    );
    test_display(
        &[0x0a, 0x47],
        "bx r1"
    );
    test_display(
        &[0x0b, 0x47],
        "bx r1"
    );
    test_display(
        &[0x0c, 0x47],
        "bx r1"
    );
    test_display(
        &[0x0d, 0x47],
        "bx r1"
    );
    test_display(
        &[0x0e, 0x47],
        "bx r1"
    );
    test_display(
        &[0x0f, 0x47],
        "bx r1"
    );
    test_display(
        &[0x10, 0x47],
        "bx r2"
    );
    test_display(
        &[0x11, 0x47],
        "bx r2"
    );
    test_display(
        &[0x12, 0x47],
        "bx r2"
    );
    test_display(
        &[0x13, 0x47],
        "bx r2"
    );
    test_display(
        &[0x14, 0x47],
        "bx r2"
    );
    test_display(
        &[0x15, 0x47],
        "bx r2"
    );
    test_display(
        &[0x16, 0x47],
        "bx r2"
    );
    test_display(
        &[0x17, 0x47],
        "bx r2"
    );
    test_display(
        &[0x18, 0x47],
        "bx r3"
    );
    test_display(
        &[0x19, 0x47],
        "bx r3"
    );
    test_display(
        &[0x1a, 0x47],
        "bx r3"
    );
    test_display(
        &[0x1b, 0x47],
        "bx r3"
    );
    test_display(
        &[0x1c, 0x47],
        "bx r3"
    );
    test_display(
        &[0x1d, 0x47],
        "bx r3"
    );
    test_display(
        &[0x1e, 0x47],
        "bx r3"
    );
    test_display(
        &[0x1f, 0x47],
        "bx r3"
    );
    test_display(
        &[0x20, 0x47],
        "bx r4"
    );
    test_display(
        &[0x21, 0x47],
        "bx r4"
    );
    test_display(
        &[0x22, 0x47],
        "bx r4"
    );
    test_display(
        &[0x23, 0x47],
        "bx r4"
    );
    test_display(
        &[0x24, 0x47],
        "bx r4"
    );
    test_display(
        &[0x25, 0x47],
        "bx r4"
    );
    test_display(
        &[0x26, 0x47],
        "bx r4"
    );
    test_display(
        &[0x27, 0x47],
        "bx r4"
    );
    test_display(
        &[0x28, 0x47],
        "bx r5"
    );
    test_display(
        &[0x29, 0x47],
        "bx r5"
    );
    test_display(
        &[0x2a, 0x47],
        "bx r5"
    );
    test_display(
        &[0x2b, 0x47],
        "bx r5"
    );
    test_display(
        &[0x2c, 0x47],
        "bx r5"
    );
    test_display(
        &[0x2d, 0x47],
        "bx r5"
    );
    test_display(
        &[0x2e, 0x47],
        "bx r5"
    );
    test_display(
        &[0x2f, 0x47],
        "bx r5"
    );
    test_display(
        &[0x30, 0x47],
        "bx r6"
    );
    test_display(
        &[0x31, 0x47],
        "bx r6"
    );
    test_display(
        &[0x32, 0x47],
        "bx r6"
    );
    test_display(
        &[0x33, 0x47],
        "bx r6"
    );
    test_display(
        &[0x34, 0x47],
        "bx r6"
    );
    test_display(
        &[0x35, 0x47],
        "bx r6"
    );
    test_display(
        &[0x36, 0x47],
        "bx r6"
    );
    test_display(
        &[0x37, 0x47],
        "bx r6"
    );
    test_display(
        &[0x38, 0x47],
        "bx r7"
    );
    test_display(
        &[0x39, 0x47],
        "bx r7"
    );
    test_display(
        &[0x3a, 0x47],
        "bx r7"
    );
    test_display(
        &[0x3b, 0x47],
        "bx r7"
    );
    test_display(
        &[0x3c, 0x47],
        "bx r7"
    );
    test_display(
        &[0x3d, 0x47],
        "bx r7"
    );
    test_display(
        &[0x3e, 0x47],
        "bx r7"
    );
    test_display(
        &[0x3f, 0x47],
        "bx r7"
    );
    test_display(
        &[0x40, 0x47],
        "bx r8"
    );
    test_display(
        &[0x41, 0x47],
        "bx r8"
    );
    test_display(
        &[0x42, 0x47],
        "bx r8"
    );
    test_display(
        &[0x43, 0x47],
        "bx r8"
    );
    test_display(
        &[0x44, 0x47],
        "bx r8"
    );
    test_display(
        &[0x45, 0x47],
        "bx r8"
    );
    test_display(
        &[0x46, 0x47],
        "bx r8"
    );
    test_display(
        &[0x47, 0x47],
        "bx r8"
    );
    test_display(
        &[0x48, 0x47],
        "bx sb"
    );
    test_display(
        &[0x49, 0x47],
        "bx sb"
    );
    test_display(
        &[0x4a, 0x47],
        "bx sb"
    );
    test_display(
        &[0x4b, 0x47],
        "bx sb"
    );
    test_display(
        &[0x4c, 0x47],
        "bx sb"
    );
    test_display(
        &[0x4d, 0x47],
        "bx sb"
    );
    test_display(
        &[0x4e, 0x47],
        "bx sb"
    );
    test_display(
        &[0x4f, 0x47],
        "bx sb"
    );
    test_display(
        &[0x50, 0x47],
        "bx r10"
    );
    test_display(
        &[0x51, 0x47],
        "bx r10"
    );
    test_display(
        &[0x52, 0x47],
        "bx r10"
    );
    test_display(
        &[0x53, 0x47],
        "bx r10"
    );
    test_display(
        &[0x54, 0x47],
        "bx r10"
    );
    test_display(
        &[0x55, 0x47],
        "bx r10"
    );
    test_display(
        &[0x56, 0x47],
        "bx r10"
    );
    test_display(
        &[0x57, 0x47],
        "bx r10"
    );
    test_display(
        &[0x58, 0x47],
        "bx fp"
    );
    test_display(
        &[0x59, 0x47],
        "bx fp"
    );
    test_display(
        &[0x5a, 0x47],
        "bx fp"
    );
    test_display(
        &[0x5b, 0x47],
        "bx fp"
    );
    test_display(
        &[0x5c, 0x47],
        "bx fp"
    );
    test_display(
        &[0x5d, 0x47],
        "bx fp"
    );
    test_display(
        &[0x5e, 0x47],
        "bx fp"
    );
    test_display(
        &[0x5f, 0x47],
        "bx fp"
    );
    test_display(
        &[0x60, 0x47],
        "bx ip"
    );
    test_display(
        &[0x61, 0x47],
        "bx ip"
    );
    test_display(
        &[0x62, 0x47],
        "bx ip"
    );
    test_display(
        &[0x63, 0x47],
        "bx ip"
    );
    test_display(
        &[0x64, 0x47],
        "bx ip"
    );
    test_display(
        &[0x65, 0x47],
        "bx ip"
    );
    test_display(
        &[0x66, 0x47],
        "bx ip"
    );
    test_display(
        &[0x67, 0x47],
        "bx ip"
    );
    test_display(
        &[0x68, 0x47],
        "bx sp"
    );
    test_display(
        &[0x69, 0x47],
        "bx sp"
    );
    test_display(
        &[0x6a, 0x47],
        "bx sp"
    );
    test_display(
        &[0x6b, 0x47],
        "bx sp"
    );
    test_display(
        &[0x6c, 0x47],
        "bx sp"
    );
    test_display(
        &[0x6d, 0x47],
        "bx sp"
    );
    test_display(
        &[0x6e, 0x47],
        "bx sp"
    );
    test_display(
        &[0x6f, 0x47],
        "bx sp"
    );
    test_display(
        &[0x70, 0x47],
        "bx lr"
    );
    test_display(
        &[0x71, 0x47],
        "bx lr"
    );
    test_display(
        &[0x72, 0x47],
        "bx lr"
    );
    test_display(
        &[0x73, 0x47],
        "bx lr"
    );
    test_display(
        &[0x74, 0x47],
        "bx lr"
    );
    test_display(
        &[0x75, 0x47],
        "bx lr"
    );
    test_display(
        &[0x76, 0x47],
        "bx lr"
    );
    test_display(
        &[0x77, 0x47],
        "bx lr"
    );
    test_display(
        &[0x78, 0x47],
        "bx pc"
    );
    test_display(
        &[0x79, 0x47],
        "bx pc"
    );
    test_display(
        &[0x7a, 0x47],
        "bx pc"
    );
    test_display(
        &[0x7b, 0x47],
        "bx pc"
    );
    test_display(
        &[0x7c, 0x47],
        "bx pc"
    );
    test_display(
        &[0x7d, 0x47],
        "bx pc"
    );
    test_display(
        &[0x7e, 0x47],
        "bx pc"
    );
    test_display(
        &[0x7f, 0x47],
        "bx pc"
    );
}
#[test]
fn test_decode_cbz_cbnz_cases() {
    test_display(
        &[0x01, 0xb1],
        "cbz r1, $+0x2" // original test: 0x4. assume address is 0, so $ is 2, +2 makes 4?
    );
    test_display(
        &[0x01, 0xb3],
        "cbz r1, $+0x42" // original test: 0x4. assume address is 0, so $ is 2, +2 makes 4?
    );
    test_display(
        &[0x01, 0xb9],
        "cbnz r1, $+0x2" // original test: 0x4. assume address is 0, so $ is 2, +2 makes 4?
    );
    test_display(
        &[0x01, 0xbb],
        "cbnz r1, $+0x42" // original test: 0x4. assume address is 0, so $ is 2, +2 makes 4?
    );
    test_display(
        &[0x07, 0xb1],
        "cbz r7, $+0x2" // original test: 0x4. assume address is 0, so $ is 2, +2 makes 4?
    );
    test_display(
        &[0x07, 0xb3],
        "cbz r7, $+0x42"
    );
    test_display(
        &[0x07, 0xb9],
        "cbnz r7, $+0x2"
    );
    test_display(
        &[0x07, 0xbb],
        "cbnz r7, $+0x42"
    );
    test_display(
        &[0xff, 0xb1],
        "cbz r7, $+0x40"
    );
    test_display(
        &[0xff, 0xb3],
        "cbz r7, $+0x80"
    );
    test_display(
        &[0xff, 0xb9],
        "cbnz r7, $+0x40"
    );
    test_display(
        &[0xff, 0xbb],
        "cbnz r7, $+0x80"
    );
}
#[test]
fn test_decode_cmn_test_cases() {
    test_display(
        &[0x33, 0x42],
        "tst r3, r6"
    );
    test_display(
        &[0xf3, 0x42],
        "cmn r3, r6"
    );
}
#[test]
fn test_decode_cmp_cases() {
    test_display(
        &[0x00, 0x45],
        "cmp r0, r0"
    );
    test_display(
        &[0x01, 0x45],
        "cmp r1, r0"
    );
    test_display(
        &[0x02, 0x28],
        "cmp r0, 0x2"
    );
    test_display(
        &[0x02, 0x29],
        "cmp r1, 0x2"
    );
    test_display(
        &[0x02, 0x2a],
        "cmp r2, 0x2"
    );
    test_display(
        &[0x02, 0x2b],
        "cmp r3, 0x2"
    );
    test_display(
        &[0x02, 0x2c],
        "cmp r4, 0x2"
    );
    test_display(
        &[0x02, 0x2d],
        "cmp r5, 0x2"
    );
    test_display(
        &[0x02, 0x2e],
        "cmp r6, 0x2"
    );
    test_display(
        &[0x02, 0x2f],
        "cmp r7, 0x2"
    );
    test_display(
        &[0xff, 0x28],
        "cmp r0, 0xff"
    );
    test_display(
        &[0xff, 0x29],
        "cmp r1, 0xff"
    );
    test_display(
        &[0xff, 0x2a],
        "cmp r2, 0xff"
    );
    test_display(
        &[0xff, 0x2b],
        "cmp r3, 0xff"
    );
    test_display(
        &[0xff, 0x2c],
        "cmp r4, 0xff"
    );
    test_display(
        &[0xff, 0x2d],
        "cmp r5, 0xff"
    );
    test_display(
        &[0xff, 0x2e],
        "cmp r6, 0xff"
    );
    test_display(
        &[0xff, 0x2f],
        "cmp r7, 0xff"
    );
    test_display(
        &[0x53, 0x45],
        "cmp r3, r10"
    );
    test_display(
        &[0x6c, 0x45],
        "cmp r4, sp"
    );
    test_display(
        &[0x7e, 0x45],
        "cmp r6, pc"
    );
    test_display(
        &[0xfe, 0x45],
        "cmp lr, pc"
    );
    test_display(
        &[0xff, 0x45],
        "cmp pc, pc"
    );
    test_display(
        &[0xb4, 0xeb, 0x55, 0x4f],
        "cmp.w r4, r5, lsr 17"
    );
}
#[test]
fn test_decode_it_cases() {
    test_display(
        &[0x01, 0xbf],
        "itttt eq"
    );
    test_display(
        &[0x03, 0xbf],
        "ittte eq"
    );
    test_display(
        &[0x05, 0xbf],
        "ittet eq"
    );
    test_display(
        &[0x07, 0xbf],
        "ittee eq"
    );
    test_display(
        &[0x09, 0xbf],
        "itett eq"
    );
    test_display(
        &[0x0b, 0xbf],
        "itete eq"
    );
    test_display(
        &[0x0d, 0xbf],
        "iteet eq"
    );
    test_display(
        &[0x0f, 0xbf],
        "iteee eq"
    );
    test_display(
        &[0x11, 0xbf],
        "iteee ne"
    );
    test_display(
        &[0x13, 0xbf],
        "iteet ne"
    );
    test_display(
        &[0x15, 0xbf],
        "itete ne"
    );
    test_display(
        &[0x17, 0xbf],
        "itett ne"
    );
    test_display(
        &[0x19, 0xbf],
        "ittee ne"
    );
    test_display(
        &[0x1b, 0xbf],
        "ittet ne"
    );
    test_display(
        &[0x1d, 0xbf],
        "ittte ne"
    );
    test_display(
        &[0x1f, 0xbf],
        "itttt ne"
    );
    test_display(
        &[0x21, 0xbf],
        "itttt hs"
    );
    test_display(
        &[0x23, 0xbf],
        "ittte hs"
    );
    test_display(
        &[0x25, 0xbf],
        "ittet hs"
    );
    test_display(
        &[0x27, 0xbf],
        "ittee hs"
    );
    test_display(
        &[0x29, 0xbf],
        "itett hs"
    );
    test_display(
        &[0x2b, 0xbf],
        "itete hs"
    );
    test_display(
        &[0x2d, 0xbf],
        "iteet hs"
    );
    test_display(
        &[0x2f, 0xbf],
        "iteee hs"
    );
    test_display(
        &[0x31, 0xbf],
        "iteee lo"
    );
    test_display(
        &[0x33, 0xbf],
        "iteet lo"
    );
    test_display(
        &[0x35, 0xbf],
        "itete lo"
    );
    test_display(
        &[0x37, 0xbf],
        "itett lo"
    );
    test_display(
        &[0x39, 0xbf],
        "ittee lo"
    );
    test_display(
        &[0x3b, 0xbf],
        "ittet lo"
    );
    test_display(
        &[0x3d, 0xbf],
        "ittte lo"
    );
    test_display(
        &[0x3f, 0xbf],
        "itttt lo"
    );
    test_display(
        &[0x41, 0xbf],
        "itttt mi"
    );
    test_display(
        &[0x43, 0xbf],
        "ittte mi"
    );
    test_display(
        &[0x45, 0xbf],
        "ittet mi"
    );
    test_display(
        &[0x47, 0xbf],
        "ittee mi"
    );
    test_display(
        &[0x49, 0xbf],
        "itett mi"
    );
    test_display(
        &[0x4b, 0xbf],
        "itete mi"
    );
    test_display(
        &[0x4d, 0xbf],
        "iteet mi"
    );
    test_display(
        &[0x4f, 0xbf],
        "iteee mi"
    );
    test_display(
        &[0x51, 0xbf],
        "iteee pl"
    );
    test_display(
        &[0x53, 0xbf],
        "iteet pl"
    );
    test_display(
        &[0x55, 0xbf],
        "itete pl"
    );
    test_display(
        &[0x57, 0xbf],
        "itett pl"
    );
    test_display(
        &[0x59, 0xbf],
        "ittee pl"
    );
    test_display(
        &[0x5b, 0xbf],
        "ittet pl"
    );
    test_display(
        &[0x5d, 0xbf],
        "ittte pl"
    );
    test_display(
        &[0x5f, 0xbf],
        "itttt pl"
    );
    test_display(
        &[0x61, 0xbf],
        "itttt vs"
    );
    test_display(
        &[0x63, 0xbf],
        "ittte vs"
    );
    test_display(
        &[0x65, 0xbf],
        "ittet vs"
    );
    test_display(
        &[0x67, 0xbf],
        "ittee vs"
    );
    test_display(
        &[0x69, 0xbf],
        "itett vs"
    );
    test_display(
        &[0x6b, 0xbf],
        "itete vs"
    );
    test_display(
        &[0x6d, 0xbf],
        "iteet vs"
    );
    test_display(
        &[0x6f, 0xbf],
        "iteee vs"
    );
    test_display(
        &[0x71, 0xbf],
        "iteee vc"
    );
    test_display(
        &[0x73, 0xbf],
        "iteet vc"
    );
    test_display(
        &[0x75, 0xbf],
        "itete vc"
    );
    test_display(
        &[0x77, 0xbf],
        "itett vc"
    );
    test_display(
        &[0x79, 0xbf],
        "ittee vc"
    );
    test_display(
        &[0x7b, 0xbf],
        "ittet vc"
    );
    test_display(
        &[0x7d, 0xbf],
        "ittte vc"
    );
    test_display(
        &[0x7f, 0xbf],
        "itttt vc"
    );
    test_display(
        &[0x81, 0xbf],
        "itttt hi"
    );
    test_display(
        &[0x83, 0xbf],
        "ittte hi"
    );
    test_display(
        &[0x85, 0xbf],
        "ittet hi"
    );
    test_display(
        &[0x87, 0xbf],
        "ittee hi"
    );
    test_display(
        &[0x89, 0xbf],
        "itett hi"
    );
    test_display(
        &[0x8b, 0xbf],
        "itete hi"
    );
    test_display(
        &[0x8d, 0xbf],
        "iteet hi"
    );
    test_display(
        &[0x8f, 0xbf],
        "iteee hi"
    );
    test_display(
        &[0x91, 0xbf],
        "iteee ls"
    );
    test_display(
        &[0x93, 0xbf],
        "iteet ls"
    );
    test_display(
        &[0x95, 0xbf],
        "itete ls"
    );
    test_display(
        &[0x97, 0xbf],
        "itett ls"
    );
    test_display(
        &[0x99, 0xbf],
        "ittee ls"
    );
    test_display(
        &[0x9b, 0xbf],
        "ittet ls"
    );
    test_display(
        &[0x9d, 0xbf],
        "ittte ls"
    );
    test_display(
        &[0x9f, 0xbf],
        "itttt ls"
    );
    test_display(
        &[0xa1, 0xbf],
        "itttt ge"
    );
    test_display(
        &[0xa3, 0xbf],
        "ittte ge"
    );
    test_display(
        &[0xa5, 0xbf],
        "ittet ge"
    );
    test_display(
        &[0xa7, 0xbf],
        "ittee ge"
    );
    test_display(
        &[0xa9, 0xbf],
        "itett ge"
    );
    test_display(
        &[0xab, 0xbf],
        "itete ge"
    );
    test_display(
        &[0xad, 0xbf],
        "iteet ge"
    );
    test_display(
        &[0xaf, 0xbf],
        "iteee ge"
    );
    test_display(
        &[0xb1, 0xbf],
        "iteee lt"
    );
    test_display(
        &[0xb3, 0xbf],
        "iteet lt"
    );
    test_display(
        &[0xb5, 0xbf],
        "itete lt"
    );
    test_display(
        &[0xb7, 0xbf],
        "itett lt"
    );
    test_display(
        &[0xb9, 0xbf],
        "ittee lt"
    );
    test_display(
        &[0xbb, 0xbf],
        "ittet lt"
    );
    test_display(
        &[0xbd, 0xbf],
        "ittte lt"
    );
    test_display(
        &[0xbf, 0xbf],
        "itttt lt"
    );
    test_display(
        &[0xc1, 0xbf],
        "itttt gt"
    );
    test_display(
        &[0xc3, 0xbf],
        "ittte gt"
    );
    test_display(
        &[0xc5, 0xbf],
        "ittet gt"
    );
    test_display(
        &[0xc7, 0xbf],
        "ittee gt"
    );
    test_display(
        &[0xc9, 0xbf],
        "itett gt"
    );
    test_display(
        &[0xcb, 0xbf],
        "itete gt"
    );
    test_display(
        &[0xcd, 0xbf],
        "iteet gt"
    );
    test_display(
        &[0xcf, 0xbf],
        "iteee gt"
    );
    test_display(
        &[0xd1, 0xbf],
        "iteee le"
    );
    test_display(
        &[0xd3, 0xbf],
        "iteet le"
    );
    test_display(
        &[0xd5, 0xbf],
        "itete le"
    );
    test_display(
        &[0xd7, 0xbf],
        "itett le"
    );
    test_display(
        &[0xd9, 0xbf],
        "ittee le"
    );
    test_display(
        &[0xdb, 0xbf],
        "ittet le"
    );
    test_display(
        &[0xdd, 0xbf],
        "ittte le"
    );
    test_display(
        &[0xdf, 0xbf],
        "itttt le"
    );
    test_display(
        &[0xe1, 0xbf],
        "itttt al"
    );
    test_display(
        &[0xe3, 0xbf],
        "ittte al"
    );
    test_display(
        &[0xe5, 0xbf],
        "ittet al"
    );
    test_display(
        &[0xe7, 0xbf],
        "ittee al"
    );
    test_display(
        &[0xe9, 0xbf],
        "itett al"
    );
    test_display(
        &[0xeb, 0xbf],
        "itete al"
    );
    test_display(
        &[0xed, 0xbf],
        "iteet al"
    );
    test_display(
        &[0xef, 0xbf],
        "iteee al"
    );
}
#[test]
fn test_decode_ldm_16b_cases() {
    test_display(
        &[0x80, 0xc8],
        "ldm r0!, {r7}"
    );
    test_display(
        &[0x80, 0xc9],
        "ldm r1!, {r7}"
    );
    test_display(
        &[0x80, 0xca],
        "ldm r2!, {r7}"
    );
    test_display(
        &[0x80, 0xcb],
        "ldm r3!, {r7}"
    );
    test_display(
        &[0x80, 0xcc],
        "ldm r4!, {r7}"
    );
    test_display(
        &[0x80, 0xcd],
        "ldm r5!, {r7}"
    );
    test_display(
        &[0x80, 0xce],
        "ldm r6!, {r7}"
    );
    test_display(
        &[0x80, 0xcf],
        "ldm r7, {r7}"
    );
    test_display(
        &[0xb0, 0xce],
        "ldm r6!, {r4, r5, r7}"
    );
    test_display(
        &[0xb0, 0xcf],
        "ldm r7, {r4, r5, r7}"
    );
    test_display(
        &[0xc0, 0xcb],
        "ldm r3!, {r6, r7}"
    );
    test_display(
        &[0xfe, 0xc8],
        "ldm r0!, {r1, r2, r3, r4, r5, r6, r7}"
    );
    test_display(
        &[0xed, 0xcc],
        "ldm r4!, {r0, r2, r3, r5, r6, r7}"
    );
    test_display(
        &[0xef, 0xcc],
        "ldm r4!, {r0, r1, r2, r3, r5, r6, r7}"
    );
    test_display(
        &[0xfe, 0xce],
        "ldm r6, {r1, r2, r3, r4, r5, r6, r7}"
    );
    test_display(
        &[0xff, 0xcd],
        "ldm r5, {r0, r1, r2, r3, r4, r5, r6, r7}"
    );
}
#[test]
fn test_decode_ldr_16b_cases() {
    test_display(
        &[0x00, 0x48],
        "ldr r0, [pc]"
    );
    test_display(
        &[0x00, 0x49],
        "ldr r1, [pc]"
    );
    test_display(
        &[0x00, 0x4a],
        "ldr r2, [pc]"
    );
    test_display(
        &[0x00, 0x4b],
        "ldr r3, [pc]"
    );
    test_display(
        &[0x00, 0x4c],
        "ldr r4, [pc]"
    );
    test_display(
        &[0x00, 0x4d],
        "ldr r5, [pc]"
    );
    test_display(
        &[0x00, 0x4e],
        "ldr r6, [pc]"
    );
    test_display(
        &[0x00, 0x4f],
        "ldr r7, [pc]"
    );
    test_display(
        &[0x00, 0x56],
        "ldrsb r0, [r0, r0]"
    );
    test_display(
        &[0x00, 0x57],
        "ldrsb r0, [r0, r4]"
    );
    test_display(
        &[0x00, 0x58],
        "ldr r0, [r0, r0]"
    );
    test_display(
        &[0x00, 0x59],
        "ldr r0, [r0, r4]"
    );
    test_display(
        &[0x00, 0x5a],
        "ldrh r0, [r0, r0]"
    );
    test_display(
        &[0x00, 0x5b],
        "ldrh r0, [r0, r4]"
    );
    test_display(
        &[0x00, 0x5c],
        "ldrb r0, [r0, r0]"
    );
    test_display(
        &[0x00, 0x5d],
        "ldrb r0, [r0, r4]"
    );
    test_display(
        &[0x00, 0x5e],
        "ldrsh r0, [r0, r0]"
    );
    test_display(
        &[0x00, 0x5f],
        "ldrsh r0, [r0, r4]"
    );
    test_display(
        &[0x00, 0x68],
        "ldr r0, [r0]"
    );
    test_display(
        &[0x00, 0x69],
        "ldr r0, [r0, 0x10]"
    );
    test_display(
        &[0x00, 0x6a],
        "ldr r0, [r0, 0x20]"
    );
    test_display(
        &[0x00, 0x6b],
        "ldr r0, [r0, 0x30]"
    );
    test_display(
        &[0x00, 0x6c],
        "ldr r0, [r0, 0x40]"
    );
    test_display(
        &[0x00, 0x6d],
        "ldr r0, [r0, 0x50]"
    );
    test_display(
        &[0x00, 0x6e],
        "ldr r0, [r0, 0x60]"
    );
    test_display(
        &[0x00, 0x6f],
        "ldr r0, [r0, 0x70]"
    );
    test_display(
        &[0x00, 0x78],
        "ldrb r0, [r0]"
    );
    test_display(
        &[0x00, 0x79],
        "ldrb r0, [r0, 0x4]"
    );
    test_display(
        &[0x00, 0x7a],
        "ldrb r0, [r0, 0x8]"
    );
    test_display(
        &[0x00, 0x7b],
        "ldrb r0, [r0, 0xc]"
    );
    test_display(
        &[0x00, 0x7c],
        "ldrb r0, [r0, 0x10]"
    );
    test_display(
        &[0x00, 0x7d],
        "ldrb r0, [r0, 0x14]"
    );
    test_display(
        &[0x00, 0x7e],
        "ldrb r0, [r0, 0x18]"
    );
    test_display(
        &[0x00, 0x7f],
        "ldrb r0, [r0, 0x1c]"
    );
    test_display(
        &[0x00, 0x88],
        "ldrh r0, [r0]"
    );
    test_display(
        &[0x00, 0x89],
        "ldrh r0, [r0, 0x8]"
    );
    test_display(
        &[0x00, 0x8a],
        "ldrh r0, [r0, 0x10]"
    );
    test_display(
        &[0x00, 0x8b],
        "ldrh r0, [r0, 0x18]"
    );
    test_display(
        &[0x00, 0x8c],
        "ldrh r0, [r0, 0x20]"
    );
    test_display(
        &[0x00, 0x8d],
        "ldrh r0, [r0, 0x28]"
    );
    test_display(
        &[0x00, 0x8e],
        "ldrh r0, [r0, 0x30]"
    );
    test_display(
        &[0x00, 0x8f],
        "ldrh r0, [r0, 0x38]"
    );
    test_display(
        &[0x00, 0x98],
        "ldr r0, [sp]"
    );
    test_display(
        &[0x00, 0x99],
        "ldr r1, [sp]"
    );
    test_display(
        &[0x00, 0x9a],
        "ldr r2, [sp]"
    );
    test_display(
        &[0x00, 0x9b],
        "ldr r3, [sp]"
    );
    test_display(
        &[0x00, 0x9c],
        "ldr r4, [sp]"
    );
    test_display(
        &[0x00, 0x9d],
        "ldr r5, [sp]"
    );
    test_display(
        &[0x00, 0x9e],
        "ldr r6, [sp]"
    );
    test_display(
        &[0x00, 0x9f],
        "ldr r7, [sp]"
    );
    test_display(
        &[0x01, 0x48],
        "ldr r0, [pc, 0x4]"
    );
    test_display(
        &[0x01, 0x49],
        "ldr r1, [pc, 0x4]"
    );
    test_display(
        &[0x01, 0x4a],
        "ldr r2, [pc, 0x4]"
    );
    test_display(
        &[0x01, 0x4b],
        "ldr r3, [pc, 0x4]"
    );
    test_display(
        &[0x01, 0x4c],
        "ldr r4, [pc, 0x4]"
    );
    test_display(
        &[0x01, 0x4d],
        "ldr r5, [pc, 0x4]"
    );
    test_display(
        &[0x01, 0x4e],
        "ldr r6, [pc, 0x4]"
    );
    test_display(
        &[0x01, 0x4f],
        "ldr r7, [pc, 0x4]"
    );
    test_display(
        &[0xff, 0x48],
        "ldr r0, [pc, 0x3fc]"
    );
    test_display(
        &[0xff, 0x49],
        "ldr r1, [pc, 0x3fc]"
    );
    test_display(
        &[0xff, 0x4a],
        "ldr r2, [pc, 0x3fc]"
    );
    test_display(
        &[0xff, 0x4b],
        "ldr r3, [pc, 0x3fc]"
    );
    test_display(
        &[0xff, 0x4c],
        "ldr r4, [pc, 0x3fc]"
    );
    test_display(
        &[0xff, 0x4d],
        "ldr r5, [pc, 0x3fc]"
    );
    test_display(
        &[0xff, 0x4e],
        "ldr r6, [pc, 0x3fc]"
    );
    test_display(
        &[0xff, 0x4f],
        "ldr r7, [pc, 0x3fc]"
    );
    test_display(
        &[0xff, 0x56],
        "ldrsb r7, [r7, r3]"
    );
    test_display(
        &[0xff, 0x58],
        "ldr r7, [r7, r3]"
    );
    test_display(
        &[0xff, 0x5a],
        "ldrh r7, [r7, r3]"
    );
    test_display(
        &[0xff, 0x5c],
        "ldrb r7, [r7, r3]"
    );
    test_display(
        &[0xff, 0x5e],
        "ldrsh r7, [r7, r3]"
    );
    test_display(
        &[0xff, 0x68],
        "ldr r7, [r7, 0xc]"
    );
    test_display(
        &[0xff, 0x69],
        "ldr r7, [r7, 0x1c]"
    );
    test_display(
        &[0xff, 0x6a],
        "ldr r7, [r7, 0x2c]"
    );
    test_display(
        &[0xff, 0x6b],
        "ldr r7, [r7, 0x3c]"
    );
    test_display(
        &[0xff, 0x6c],
        "ldr r7, [r7, 0x4c]"
    );
    test_display(
        &[0xff, 0x6d],
        "ldr r7, [r7, 0x5c]"
    );
    test_display(
        &[0xff, 0x6e],
        "ldr r7, [r7, 0x6c]"
    );
    test_display(
        &[0xff, 0x6f],
        "ldr r7, [r7, 0x7c]"
    );
    test_display(
        &[0xff, 0x78],
        "ldrb r7, [r7, 0x3]"
    );
    test_display(
        &[0xff, 0x79],
        "ldrb r7, [r7, 0x7]"
    );
    test_display(
        &[0xff, 0x7a],
        "ldrb r7, [r7, 0xb]"
    );
    test_display(
        &[0xff, 0x7b],
        "ldrb r7, [r7, 0xf]"
    );
    test_display(
        &[0xff, 0x7c],
        "ldrb r7, [r7, 0x13]"
    );
    test_display(
        &[0xff, 0x7d],
        "ldrb r7, [r7, 0x17]"
    );
    test_display(
        &[0xff, 0x7e],
        "ldrb r7, [r7, 0x1b]"
    );
    test_display(
        &[0xff, 0x7f],
        "ldrb r7, [r7, 0x1f]"
    );
    test_display(
        &[0xff, 0x88],
        "ldrh r7, [r7, 0x6]"
    );
    test_display(
        &[0xff, 0x89],
        "ldrh r7, [r7, 0xe]"
    );
    test_display(
        &[0xff, 0x8a],
        "ldrh r7, [r7, 0x16]"
    );
    test_display(
        &[0xff, 0x8b],
        "ldrh r7, [r7, 0x1e]"
    );
    test_display(
        &[0xff, 0x8c],
        "ldrh r7, [r7, 0x26]"
    );
    test_display(
        &[0xff, 0x8d],
        "ldrh r7, [r7, 0x2e]"
    );
    test_display(
        &[0xff, 0x8e],
        "ldrh r7, [r7, 0x36]"
    );
    test_display(
        &[0xff, 0x8f],
        "ldrh r7, [r7, 0x3e]"
    );
    test_display(
        &[0xff, 0x98],
        "ldr r0, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x99],
        "ldr r1, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x9a],
        "ldr r2, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x9b],
        "ldr r3, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x9c],
        "ldr r4, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x9d],
        "ldr r5, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x9e],
        "ldr r6, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x9f],
        "ldr r7, [sp, 0x3fc]"
    );
}
#[test]
fn test_decode_misc_cases() {
    test_display(
        &[0x00, 0xbf],
        "nop"
    );
    test_display(
        &[0x10, 0xbf],
        "yield"
    );
    test_display(
        &[0x20, 0xbf],
        "wfe"
    );
    test_display(
        &[0x30, 0xbf],
        "wfi"
    );
    test_display(
        &[0x40, 0xbf],
        "sev"
    );
    test_display(
        &[0x50, 0xb6],
        "setend le"
    );
    test_display(
        &[0x50, 0xbf],
        "hint 0x5"
    );
    test_display(
        &[0x58, 0xb6],
        "setend be"
    );
    test_display(
        &[0x60, 0xbf],
        "hint 0x6"
    );
    test_display(
        &[0x61, 0xb6],
        "cpsie f"
    );
    test_display(
        &[0x62, 0xb6],
        "cpsie i"
    );
    test_display(
        &[0x63, 0xb6],
        "cpsie if"
    );
    test_display(
        &[0x64, 0xb6],
        "cpsie a"
    );
    test_display(
        &[0x65, 0xb6],
        "cpsie af"
    );
    test_display(
        &[0x66, 0xb6],
        "cpsie ai"
    );
    test_display(
        &[0x67, 0xb6],
        "cpsie aif"
    );
    test_display(
        &[0x70, 0xbf],
        "hint 0x7"
    );
    test_display(
        &[0x71, 0xb6],
        "cpsid f"
    );
    test_display(
        &[0x72, 0xb6],
        "cpsid i"
    );
    test_display(
        &[0x73, 0xb6],
        "cpsid if"
    );
    test_display(
        &[0x74, 0xb6],
        "cpsid a"
    );
    test_display(
        &[0x75, 0xb6],
        "cpsid af"
    );
    test_display(
        &[0x76, 0xb6],
        "cpsid ai"
    );
    test_display(
        &[0x77, 0xb6],
        "cpsid aif"
    );
    test_display(
        &[0x80, 0xbf],
        "hint 0x8"
    );
    test_display(
        &[0x90, 0xbf],
        "hint 0x9"
    );
    test_display(
        &[0xa0, 0xbf],
        "hint 0xa"
    );
    test_display(
        &[0xb0, 0xbf],
        "hint 0xb"
    );
    test_display(
        &[0xc0, 0xbf],
        "hint 0xc"
    );
    test_display(
        &[0xd0, 0xbf],
        "hint 0xd"
    );
    test_display(
        &[0xe0, 0xbf],
        "hint 0xe"
    );
    test_display(
        &[0xf0, 0xbf],
        "hint 0xf"
    );
    test_display(
        &[0xfe, 0xde],
        "udf 0xfe"
    );
}
#[test]
fn test_decode_mov_cases() {
    test_display(
        &[0x21, 0x46],
        "mov r1, r4"
    );
    test_display(
        &[0x90, 0x46],
        "mov r8, r2"
    );
    test_display(
        &[0x91, 0x46],
        "mov sb, r2"
    );
    test_display(
        &[0x92, 0x46],
        "mov r10, r2"
    );
    test_display(
        &[0x93, 0x46],
        "mov fp, r2"
    );
    test_display(
        &[0x94, 0x46],
        "mov ip, r2"
    );
    test_display(
        &[0x95, 0x46],
        "mov sp, r2"
    );
    test_display(
        &[0x96, 0x46],
        "mov lr, r2"
    );
    test_display(
        &[0x97, 0x46],
        "mov pc, r2"
    );
    test_display(
        &[0xc0, 0x46],
        "mov r8, r8"
    );
    test_display(
        &[0xc1, 0x46],
        "mov sb, r8"
    );
    test_display(
        &[0xc2, 0x46],
        "mov r10, r8"
    );
    test_display(
        &[0xc3, 0x46],
        "mov fp, r8"
    );
    test_display(
        &[0xc4, 0x46],
        "mov ip, r8"
    );
    test_display(
        &[0xc5, 0x46],
        "mov sp, r8"
    );
    test_display(
        &[0xc6, 0x46],
        "mov lr, r8"
    );
    test_display(
        &[0xc7, 0x46],
        "mov pc, r8"
    );
    test_display(
        &[0xc8, 0x46],
        "mov r8, sb"
    );
    test_display(
        &[0xc9, 0x46],
        "mov sb, sb"
    );
    test_display(
        &[0xca, 0x46],
        "mov r10, sb"
    );
    test_display(
        &[0xcb, 0x46],
        "mov fp, sb"
    );
    test_display(
        &[0xcc, 0x46],
        "mov ip, sb"
    );
    test_display(
        &[0xcd, 0x46],
        "mov sp, sb"
    );
    test_display(
        &[0xce, 0x46],
        "mov lr, sb"
    );
    test_display(
        &[0xcf, 0x46],
        "mov pc, sb"
    );
    test_display(
        &[0xfc, 0x46],
        "mov ip, pc"
    );
    test_display(
        &[0xfd, 0x46],
        "mov sp, pc"
    );
    test_display(
        &[0xfe, 0x46],
        "mov lr, pc"
    );
    test_display(
        &[0xff, 0x46],
        "mov pc, pc"
    );
}
#[test]
fn test_decode_op_s_cases() {
    test_display(
        &[0x00, 0x18],
        "adds r0, r0, r0"
    );
    test_display(
        &[0x00, 0x19],
        "adds r0, r0, r4"
    );
    test_display(
        &[0x00, 0x1c],
        "adds r0, r0, 0x0"
    );
    test_display(
        &[0x00, 0x1d],
        "adds r0, r0, 0x4"
    );
    test_display(
        &[0x00, 0x30],
        "adds r0, 0x0"
    );
    test_display(
        &[0x00, 0x31],
        "adds r1, 0x0"
    );
    test_display(
        &[0x00, 0x32],
        "adds r2, 0x0"
    );
    test_display(
        &[0x00, 0x33],
        "adds r3, 0x0"
    );
    test_display(
        &[0x00, 0x34],
        "adds r4, 0x0"
    );
    test_display(
        &[0x00, 0x35],
        "adds r5, 0x0"
    );
    test_display(
        &[0x00, 0x36],
        "adds r6, 0x0"
    );
    test_display(
        &[0x00, 0x37],
        "adds r7, 0x0"
    );
    test_display(
        &[0x00, 0x40],
        "ands r0, r0"
    );
    test_display(
        &[0x00, 0x43],
        "orrs r0, r0"
    );
    test_display(
        &[0x01, 0x08],
        "lsrs r1, r0, 0x20"
    );
    test_display(
        &[0x01, 0x09],
        "lsrs r1, r0, 0x4"
    );
    test_display(
        &[0x01, 0x0a],
        "lsrs r1, r0, 0x8"
    );
    test_display(
        &[0x01, 0x0b],
        "lsrs r1, r0, 0xc"
    );
    test_display(
        &[0x01, 0x0c],
        "lsrs r1, r0, 0x10"
    );
    test_display(
        &[0x01, 0x0d],
        "lsrs r1, r0, 0x14"
    );
    test_display(
        &[0x01, 0x0e],
        "lsrs r1, r0, 0x18"
    );
    test_display(
        &[0x01, 0x0f],
        "lsrs r1, r0, 0x1c"
    );
    test_display(
        &[0x01, 0x10],
        "asrs r1, r0, 0x20"
    );
    test_display(
        &[0x01, 0x11],
        "asrs r1, r0, 0x4"
    );
    test_display(
        &[0x01, 0x12],
        "asrs r1, r0, 0x8"
    );
    test_display(
        &[0x01, 0x13],
        "asrs r1, r0, 0xc"
    );
    test_display(
        &[0x01, 0x14],
        "asrs r1, r0, 0x10"
    );
    test_display(
        &[0x01, 0x15],
        "asrs r1, r0, 0x14"
    );
    test_display(
        &[0x01, 0x16],
        "asrs r1, r0, 0x18"
    );
    test_display(
        &[0x01, 0x17],
        "asrs r1, r0, 0x1c"
    );
    test_display(
        &[0x01, 0x18],
        "adds r1, r0, r0"
    );
    test_display(
        &[0x01, 0x19],
        "adds r1, r0, r4"
    );
    test_display(
        &[0x01, 0x1c],
        "adds r1, r0, 0x0"
    );
    test_display(
        &[0x01, 0x1d],
        "adds r1, r0, 0x4"
    );
    test_display(
        &[0x01, 0x30],
        "adds r0, 0x1"
    );
    test_display(
        &[0x01, 0x31],
        "adds r1, 0x1"
    );
    test_display(
        &[0x01, 0x32],
        "adds r2, 0x1"
    );
    test_display(
        &[0x01, 0x33],
        "adds r3, 0x1"
    );
    test_display(
        &[0x01, 0x34],
        "adds r4, 0x1"
    );
    test_display(
        &[0x01, 0x35],
        "adds r5, 0x1"
    );
    test_display(
        &[0x01, 0x36],
        "adds r6, 0x1"
    );
    test_display(
        &[0x01, 0x37],
        "adds r7, 0x1"
    );
    test_display(
        &[0x0a, 0x01],
        "lsls r2, r1, 0x4"
    );
    test_display(
        &[0x0a, 0x02],
        "lsls r2, r1, 0x8"
    );
    test_display(
        &[0x0a, 0x03],
        "lsls r2, r1, 0xc"
    );
    test_display(
        &[0x0a, 0x04],
        "lsls r2, r1, 0x10"
    );
    test_display(
        &[0x0a, 0x05],
        "lsls r2, r1, 0x14"
    );
    test_display(
        &[0x0a, 0x06],
        "lsls r2, r1, 0x18"
    );
    test_display(
        &[0x0a, 0x07],
        "lsls r2, r1, 0x1c"
    );
    test_display(
        &[0x13, 0x1a],
        "subs r3, r2, r0"
    );
    test_display(
        &[0x13, 0x1b],
        "subs r3, r2, r4"
    );
    test_display(
        &[0x13, 0x1e],
        "subs r3, r2, 0x0"
    );
    test_display(
        &[0x13, 0x1f],
        "subs r3, r2, 0x4"
    );
    test_display(
        &[0x3d, 0x40],
        "ands r5, r7"
    );
    test_display(
        &[0x3d, 0x43],
        "orrs r5, r7"
    );
    test_display(
        &[0x7d, 0x40],
        "eors r5, r7"
    );
    test_display(
        &[0x7d, 0x41],
        "adcs r5, r7"
    );
    test_display(
        &[0x7d, 0x42],
        "rsbs r5, r7, 0x0"
    );
    test_display(
        &[0x7d, 0x43],
        "muls r5, r7, r5"
    );
    test_display(
        &[0xbd, 0x41],
        "sbcs r5, r7"
    );
    test_display(
        &[0xbd, 0x43],
        "bics r5, r7"
    );
    test_display(
        &[0xd6, 0x41],
        "rors r6, r2"
    );
    test_display(
        &[0xd6, 0x43],
        "mvns r6, r2"
    );
    test_display(
        &[0xfd, 0x20],
        "movs r0, 0xfd"
    );
    test_display(
        &[0xfd, 0x20],
        "movs r0, 0xfd"
    );
    test_display(
        &[0xfd, 0x21],
        "movs r1, 0xfd"
    );
    test_display(
        &[0xfd, 0x21],
        "movs r1, 0xfd"
    );
    test_display(
        &[0xfd, 0x22],
        "movs r2, 0xfd"
    );
    test_display(
        &[0xfd, 0x22],
        "movs r2, 0xfd"
    );
    test_display(
        &[0xfd, 0x23],
        "movs r3, 0xfd"
    );
    test_display(
        &[0xfd, 0x23],
        "movs r3, 0xfd"
    );
    test_display(
        &[0xfd, 0x24],
        "movs r4, 0xfd"
    );
    test_display(
        &[0xfd, 0x24],
        "movs r4, 0xfd"
    );
    test_display(
        &[0xfd, 0x25],
        "movs r5, 0xfd"
    );
    test_display(
        &[0xfd, 0x25],
        "movs r5, 0xfd"
    );
    test_display(
        &[0xfd, 0x26],
        "movs r6, 0xfd"
    );
    test_display(
        &[0xfd, 0x26],
        "movs r6, 0xfd"
    );
    test_display(
        &[0xfd, 0x27],
        "movs r7, 0xfd"
    );
    test_display(
        &[0xfd, 0x27],
        "movs r7, 0xfd"
    );
    test_display(
        &[0xfe, 0x00],
        "lsls r6, r7, 0x3"
    );
    test_display(
        &[0xfe, 0x01],
        "lsls r6, r7, 0x7"
    );
    test_display(
        &[0xfe, 0x02],
        "lsls r6, r7, 0xb"
    );
    test_display(
        &[0xfe, 0x03],
        "lsls r6, r7, 0xf"
    );
    test_display(
        &[0xfe, 0x04],
        "lsls r6, r7, 0x13"
    );
    test_display(
        &[0xfe, 0x05],
        "lsls r6, r7, 0x17"
    );
    test_display(
        &[0xfe, 0x06],
        "lsls r6, r7, 0x1b"
    );
    test_display(
        &[0xfe, 0x07],
        "lsls r6, r7, 0x1f"
    );
    test_display(
        &[0xfe, 0x08],
        "lsrs r6, r7, 0x3"
    );
    test_display(
        &[0xfe, 0x09],
        "lsrs r6, r7, 0x7"
    );
    test_display(
        &[0xfe, 0x0a],
        "lsrs r6, r7, 0xb"
    );
    test_display(
        &[0xfe, 0x0b],
        "lsrs r6, r7, 0xf"
    );
    test_display(
        &[0xfe, 0x0c],
        "lsrs r6, r7, 0x13"
    );
    test_display(
        &[0xfe, 0x0d],
        "lsrs r6, r7, 0x17"
    );
    test_display(
        &[0xfe, 0x0e],
        "lsrs r6, r7, 0x1b"
    );
    test_display(
        &[0xfe, 0x0f],
        "lsrs r6, r7, 0x1f"
    );
    test_display(
        &[0xfe, 0x10],
        "asrs r6, r7, 0x3"
    );
    test_display(
        &[0xfe, 0x11],
        "asrs r6, r7, 0x7"
    );
    test_display(
        &[0xfe, 0x12],
        "asrs r6, r7, 0xb"
    );
    test_display(
        &[0xfe, 0x13],
        "asrs r6, r7, 0xf"
    );
    test_display(
        &[0xfe, 0x14],
        "asrs r6, r7, 0x13"
    );
    test_display(
        &[0xfe, 0x15],
        "asrs r6, r7, 0x17"
    );
    test_display(
        &[0xfe, 0x16],
        "asrs r6, r7, 0x1b"
    );
    test_display(
        &[0xfe, 0x17],
        "asrs r6, r7, 0x1f"
    );
    test_display(
        &[0xfe, 0x40],
        "lsrs r6, r7"
    );
    test_display(
        &[0xfe, 0x41],
        "rors r6, r7"
    );
    test_display(
        &[0xfe, 0x43],
        "mvns r6, r7"
    );
    test_display(
        &[0xff, 0x18],
        "adds r7, r7, r3"
    );
    test_display(
        &[0xff, 0x19],
        "adds r7, r7, r7"
    );
    test_display(
        &[0xff, 0x1c],
        "adds r7, r7, 0x3"
    );
    test_display(
        &[0xff, 0x1d],
        "adds r7, r7, 0x7"
    );
    test_display(
        &[0xff, 0x30],
        "adds r0, 0xff"
    );
    test_display(
        &[0xff, 0x31],
        "adds r1, 0xff"
    );
    test_display(
        &[0xff, 0x32],
        "adds r2, 0xff"
    );
    test_display(
        &[0xff, 0x33],
        "adds r3, 0xff"
    );
    test_display(
        &[0xff, 0x34],
        "adds r4, 0xff"
    );
    test_display(
        &[0xff, 0x35],
        "adds r5, 0xff"
    );
    test_display(
        &[0xff, 0x36],
        "adds r6, 0xff"
    );
    test_display(
        &[0xff, 0x37],
        "adds r7, 0xff"
    );
}
#[test]
fn test_decode_pop_cases() {
    test_display(
        &[0x00, 0xbd],
        "pop {pc}"
    );
    test_display(
        &[0x01, 0xbc],
        "pop {r0}"
    );
    test_display(
        &[0x7f, 0xbd],
        "pop {r0, r1, r2, r3, r4, r5, r6, pc}"
    );
    test_display(
        &[0xff, 0xbd],
        "pop {r0, r1, r2, r3, r4, r5, r6, r7, pc}"
    );
}
#[test]
fn test_decode_push_cases() {
    test_display(
        &[0x00, 0xb5],
        "push {lr}"
    );
    test_display(
        &[0x01, 0xb4],
        "push {r0}"
    );
    test_display(
        &[0xff, 0xb5],
        "push {r0, r1, r2, r3, r4, r5, r6, r7, lr}"
    );
}
#[test]
fn test_decode_rev_cases() {
    test_display(
        &[0x0a, 0xba],
        "rev r2, r1"
    );
    test_display(
        &[0x4a, 0xba],
        "rev16 r2, r1"
    );
    test_display(
        &[0xca, 0xba],
        "revsh r2, r1"
    );
}
#[test]
fn test_decode_stm_16b_cases() {
    test_display(
        &[0x01, 0xc0],
        "stmia r0!, {r0}"
    );
    test_display(
        &[0x01, 0xc1],
        "stmia r1!, {r0}"
    );
    test_display(
        &[0x01, 0xc2],
        "stmia r2!, {r0}"
    );
    test_display(
        &[0x01, 0xc3],
        "stmia r3!, {r0}"
    );
    test_display(
        &[0x01, 0xc4],
        "stmia r4!, {r0}"
    );
    test_display(
        &[0x01, 0xc5],
        "stmia r5!, {r0}"
    );
    test_display(
        &[0x01, 0xc6],
        "stmia r6!, {r0}"
    );
    test_display(
        &[0x01, 0xc7],
        "stmia r7!, {r0}"
    );
    test_display(
        &[0xff, 0xc3],
        "stmia r3!, {r0, r1, r2, r3, r4, r5, r6, r7}"
    );
}
#[test]
fn test_decode_str_16b_cases() {
    test_display(
        &[0x00, 0x50],
        "str r0, [r0, r0]"
    );
    test_display(
        &[0x00, 0x51],
        "str r0, [r0, r4]"
    );
    test_display(
        &[0x00, 0x52],
        "strh r0, [r0, r0]"
    );
    test_display(
        &[0x00, 0x53],
        "strh r0, [r0, r4]"
    );
    test_display(
        &[0x00, 0x54],
        "strb r0, [r0, r0]"
    );
    test_display(
        &[0x00, 0x55],
        "strb r0, [r0, r4]"
    );
    test_display(
        &[0xfe, 0x60],
        "str r6, [r7, 0xc]"
    );
    test_display(
        &[0xfe, 0x61],
        "str r6, [r7, 0x1c]"
    );
    test_display(
        &[0xfe, 0x62],
        "str r6, [r7, 0x2c]"
    );
    test_display(
        &[0xfe, 0x63],
        "str r6, [r7, 0x3c]"
    );
    test_display(
        &[0xfe, 0x64],
        "str r6, [r7, 0x4c]"
    );
    test_display(
        &[0xfe, 0x65],
        "str r6, [r7, 0x5c]"
    );
    test_display(
        &[0xfe, 0x66],
        "str r6, [r7, 0x6c]"
    );
    test_display(
        &[0xfe, 0x67],
        "str r6, [r7, 0x7c]"
    );
    test_display(
        &[0xfe, 0x70],
        "strb r6, [r7, 0x3]"
    );
    test_display(
        &[0xfe, 0x71],
        "strb r6, [r7, 0x7]"
    );
    test_display(
        &[0xfe, 0x72],
        "strb r6, [r7, 0xb]"
    );
    test_display(
        &[0xfe, 0x73],
        "strb r6, [r7, 0xf]"
    );
    test_display(
        &[0xfe, 0x74],
        "strb r6, [r7, 0x13]"
    );
    test_display(
        &[0xfe, 0x75],
        "strb r6, [r7, 0x17]"
    );
    test_display(
        &[0xfe, 0x76],
        "strb r6, [r7, 0x1b]"
    );
    test_display(
        &[0xfe, 0x77],
        "strb r6, [r7, 0x1f]"
    );
    test_display(
        &[0xfe, 0x80],
        "strh r6, [r7, 0x6]"
    );
    test_display(
        &[0xfe, 0x81],
        "strh r6, [r7, 0xe]"
    );
    test_display(
        &[0xfe, 0x82],
        "strh r6, [r7, 0x16]"
    );
    test_display(
        &[0xfe, 0x83],
        "strh r6, [r7, 0x1e]"
    );
    test_display(
        &[0xfe, 0x84],
        "strh r6, [r7, 0x26]"
    );
    test_display(
        &[0xfe, 0x85],
        "strh r6, [r7, 0x2e]"
    );
    test_display(
        &[0xfe, 0x86],
        "strh r6, [r7, 0x36]"
    );
    test_display(
        &[0xfe, 0x87],
        "strh r6, [r7, 0x3e]"
    );
    test_display(
        &[0xfe, 0x90],
        "str r0, [sp, 0x3f8]"
    );
    test_display(
        &[0xfe, 0x91],
        "str r1, [sp, 0x3f8]"
    );
    test_display(
        &[0xfe, 0x92],
        "str r2, [sp, 0x3f8]"
    );
    test_display(
        &[0xfe, 0x93],
        "str r3, [sp, 0x3f8]"
    );
    test_display(
        &[0xfe, 0x94],
        "str r4, [sp, 0x3f8]"
    );
    test_display(
        &[0xfe, 0x95],
        "str r5, [sp, 0x3f8]"
    );
    test_display(
        &[0xfe, 0x96],
        "str r6, [sp, 0x3f8]"
    );
    test_display(
        &[0xfe, 0x97],
        "str r7, [sp, 0x3f8]"
    );
    test_display(
        &[0xff, 0x50],
        "str r7, [r7, r3]"
    );
    test_display(
        &[0xff, 0x90],
        "str r0, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x91],
        "str r1, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x92],
        "str r2, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x93],
        "str r3, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x94],
        "str r4, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x95],
        "str r5, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x96],
        "str r6, [sp, 0x3fc]"
    );
    test_display(
        &[0xff, 0x97],
        "str r7, [sp, 0x3fc]"
    );
}
#[test]
fn test_decode_sub_cases() {
    test_display(
        &[0xd8, 0xb0],
        "sub sp, sp, 0x160"
    );
}
#[test]
fn test_decode_svc_cases() {
    test_display(
        &[0x27, 0xdf],
        "svc 0x27"
    );
    test_display(
        &[0xad, 0xdf],
        "svc 0xad"
    );
}
#[test]
fn test_decode_udf_cases() {
    test_display(
        &[0x27, 0xde],
        "udf 0x27"
    );
    test_display(
        &[0xad, 0xde],
        "udf 0xad"
    );
}
#[test]
fn test_decode_ux_sx_cases() {
    test_display(
        &[0x0a, 0xb2],
        "sxth r2, r1"
    );
    test_display(
        &[0x4a, 0xb2],
        "sxtb r2, r1"
    );
    test_display(
        &[0x8a, 0xb2],
        "uxth r2, r1"
    );
    test_display(
        &[0xca, 0xb2],
        "uxtb r2, r1"
    );
}

#[test]
fn test_decode_ldr_32b_cases() {
    test_display(
        &[0x73, 0xe8, 0x7e, 0x5a],
        "ldrd r5, r10, [r3], -0x1f8"
    );
    test_display(
        &[0x93, 0xf8, 0x7e, 0x5a],
        "ldrb.w r5, [r3, 0xa7e]"
    );
    test_display(
        &[0xb3, 0xf8, 0x7e, 0x5a],
        "ldrh.w r5, [r3, 0xa7e]"
    );
    test_display(
        &[0xd3, 0xf8, 0x7e, 0x5a],
        "ldr.w r5, [r3, 0xa7e]"
    );
    test_display(
        &[0xf3, 0xe8, 0x7e, 0x5a],
        "ldrd r5, r10, [r3], 0x1f8"
    );
    test_display(
        &[0x53, 0xe9, 0x7e, 0x5a],
        "ldrd r5, r10, [r3, -0x1f8]"
    );
    test_display(
        &[0x73, 0xe9, 0x7e, 0x5a],
        "ldrd r5, r10, [r3, -0x1f8]!"
    );
    test_display(
        &[0x93, 0xf9, 0x7e, 0x5a],
        "ldrsb.w r5, [r3, 0xa7e]"
    );
    test_display(
        &[0xb3, 0xf9, 0x7e, 0x5a],
        "ldrsh.w r5, [r3, 0xa7e]"
    );
    test_display(
        &[0xd3, 0xe9, 0x7e, 0x5a],
        "ldrd r5, r10, [r3, 0x1f8]"
    );
    test_display(
        &[0xf3, 0xe9, 0x7e, 0x5a],
        "ldrd r5, r10, [r3, 0x1f8]!"
    );
}

#[test]
fn test_decode_coproc_ld_32b_cases() {
    test_display(
        &[0x33, 0xfc, 0x7e, 0x54],
        "ldc2 p4, c5, [r3], -0x1f8"
    );
    test_display(
        &[0x73, 0xfc, 0x7e, 0x54],
        "ldc2l p4, c5, [r3], -0x1f8"
    );
    test_display(
        &[0x93, 0xfc, 0x7e, 0x54],
        "ldc2 p4, c5, [r3], {0x7e}"
    );
    test_display(
        &[0xb3, 0xfc, 0x7e, 0x54],
        "ldc2 p4, c5, [r3], 0x1f8"
    );
    test_display(
        &[0xd3, 0xfc, 0x7e, 0x54],
        "ldc2l p4, c5, [r3], {0x7e}"
    );
    test_display(
        &[0xf3, 0xfc, 0x7e, 0x54],
        "ldc2l p4, c5, [r3], 0x1f8"
    );
    test_display(
        &[0x13, 0xfd, 0x7e, 0x54],
        "ldc2 p4, c5, [r3, -0x1f8]"
    );
    test_display(
        &[0x33, 0xfd, 0x7e, 0x54],
        "ldc2 p4, c5, [r3, -0x1f8]!"
    );
    test_display(
        &[0x53, 0xfd, 0x7e, 0x54],
        "ldc2l p4, c5, [r3, -0x1f8]"
    );
    test_display(
        &[0x73, 0xfd, 0x7e, 0x54],
        "ldc2l p4, c5, [r3, -0x1f8]!"
    );
    test_display(
        &[0x93, 0xfd, 0x7e, 0x54],
        "ldc2 p4, c5, [r3, 0x1f8]"
    );
    test_display(
        &[0xb3, 0xfd, 0x7e, 0x54],
        "ldc2 p4, c5, [r3, 0x1f8]!"
    );
    test_display(
        &[0xd3, 0xfd, 0x7e, 0x54],
        "ldc2l p4, c5, [r3, 0x1f8]"
    );
    test_display(
        &[0xf3, 0xfd, 0x7e, 0x54],
        "ldc2l p4, c5, [r3, 0x1f8]!"
    );
}
#[test]
fn test_decode_str_32b_cases() {
    test_display(
        &[0x43, 0xe8, 0x7e, 0x5a],
        "strex r10, r5, [r3, 0x1f8]"
    );
    test_display(
        &[0x63, 0xe8, 0x7e, 0x5a],
        "strd r5, r10, [r3], -0x1f8"
    );
    test_display(
        &[0x83, 0xf8, 0x7e, 0x5a],
        "strb.w r5, [r3, 0xa7e]"
    );
    test_display(
        &[0xa3, 0xf8, 0x7e, 0x5a],
        "strh.w r5, [r3, 0xa7e]"
    );
    test_display(
        &[0xc3, 0xe8, 0x7e, 0x5a],
        "strexd lr, r5, r10, [r3]"
    );
    test_display(
        &[0xc3, 0xf8, 0x7e, 0x5a],
        "str.w r5, [r3, 0xa7e]"
    );
    test_display(
        &[0xe3, 0xe8, 0x7e, 0x5a],
        "strd r5, r10, [r3], 0x1f8"
    );
    test_display(
        &[0x43, 0xe9, 0x7e, 0x5a],
        "strd r5, r10, [r3, -0x1f8]"
    );
    test_display(
        &[0x63, 0xe9, 0x7e, 0x5a],
        "strd r5, r10, [r3, -0x1f8]!"
    );
    test_display(
        &[0xc3, 0xe9, 0x7e, 0x5a],
        "strd r5, r10, [r3, 0x1f8]"
    );
    test_display(
        &[0xe3, 0xe9, 0x7e, 0x5a],
        "strd r5, r10, [r3, 0x1f8]!"
    );
    test_display(
        &[0x41, 0xf8, 0x04, 0x2b],
        "str.w r2, [r1], 0x4"
    );
    test_display(
        &[0x41, 0xf8, 0x00, 0x2b],
        "str.w r2, [r1]"
    );
}

#[test]
fn test_decode_coproc_st_32b_cases() {
    test_display(
        &[0x23, 0xfc, 0x7e, 0x54],
        "stc2 p4, c5, [r3], -0x1f8"
    );
    test_display(
        &[0x63, 0xfc, 0x7e, 0x54],
        "stc2l p4, c5, [r3], -0x1f8"
    );
    test_display(
        &[0x83, 0xfc, 0x7e, 0x54],
        "stc2 p4, c5, [r3], {0x7e}"
    );
    test_display(
        &[0xa3, 0xfc, 0x7e, 0x54],
        "stc2 p4, c5, [r3], 0x1f8"
    );
    test_display(
        &[0xc3, 0xfc, 0x7e, 0x54],
        "stc2l p4, c5, [r3], {0x7e}"
    );
    test_display(
        &[0xe3, 0xfc, 0x7e, 0x54],
        "stc2l p4, c5, [r3], 0x1f8"
    );
    test_display(
        &[0x03, 0xfd, 0x7e, 0x54],
        "stc2 p4, c5, [r3, -0x1f8]"
    );
    test_display(
        &[0x23, 0xfd, 0x7e, 0x54],
        "stc2 p4, c5, [r3, -0x1f8]!"
    );
    test_display(
        &[0x43, 0xfd, 0x7e, 0x54],
        "stc2l p4, c5, [r3, -0x1f8]"
    );
    test_display(
        &[0x63, 0xfd, 0x7e, 0x54],
        "stc2l p4, c5, [r3, -0x1f8]!"
    );
    test_display(
        &[0x83, 0xfd, 0x7e, 0x54],
        "stc2 p4, c5, [r3, 0x1f8]"
    );
    test_display(
        &[0xa3, 0xfd, 0x7e, 0x54],
        "stc2 p4, c5, [r3, 0x1f8]!"
    );
    test_display(
        &[0xc3, 0xfd, 0x7e, 0x54],
        "stc2l p4, c5, [r3, 0x1f8]"
    );
    test_display(
        &[0xe3, 0xfd, 0x7e, 0x54],
        "stc2l p4, c5, [r3, 0x1f8]!"
    );
}
#[test]
fn test_decode_stm_ldm_32b_cases() {
    test_display(
        &[0x83, 0xe8, 0x7e, 0x5a],
        "stm.w r3, {r1, r2, r3, r4, r5, r6, sb, fp, ip, lr}"
    );
    test_display(
        &[0xa3, 0xe8, 0x7e, 0x5a],
        "stm.w r3!, {r1, r2, r3, r4, r5, r6, sb, fp, ip, lr}"
    );
    test_display(
        &[0x03, 0xe9, 0x7e, 0x5a],
        "stmdb r3, {r1, r2, r3, r4, r5, r6, sb, fp, ip, lr}"
    );
    test_display(
        &[0x23, 0xe9, 0x7e, 0x5a],
        "stmdb r3!, {r1, r2, r3, r4, r5, r6, sb, fp, ip, lr}"
    );

    test_display(
        &[0x93, 0xe8, 0x7e, 0x5a],
        "ldm.w r3, {r1, r2, r3, r4, r5, r6, sb, fp, ip, lr}"
    );
    test_display(
        &[0xb3, 0xe8, 0x7e, 0x5a],
        "ldm.w r3!, {r1, r2, r3, r4, r5, r6, sb, fp, ip, lr}"
    );
    test_display(
        &[0x13, 0xe9, 0x7e, 0x5a],
        "ldmdb r3, {r1, r2, r3, r4, r5, r6, sb, fp, ip, lr}"
    );
    test_display(
        &[0x33, 0xe9, 0x7e, 0x5a],
        "ldmdb r3!, {r1, r2, r3, r4, r5, r6, sb, fp, ip, lr}"
    );
}
#[test]
fn test_decode_sat_ext_32b_cases() {
    test_display(
        &[0x43, 0xf3, 0x7e, 0x5a],
        "sbfx r10, r3, 0x15, 0x1f"
    );
    test_display(
        &[0xc3, 0xf3, 0x7e, 0x5a],
        "ubfx r10, r3, 0x15, 0x1f"
    );
    test_display(
        &[0x43, 0xf7, 0x7e, 0x5a],
        "sbfx r10, r3, 0x15, 0x1f"
    );
    test_display(
        &[0xc3, 0xf7, 0x7e, 0x5a],
        "ubfx r10, r3, 0x15, 0x1f"
    );

    test_display(
        &[0x83, 0xf3, 0x7e, 0x5a],
        "usat r10, 0x1e, r3, lsl 21"
    );
    test_display(
        &[0xa3, 0xf3, 0x7e, 0x5a],
        "usat r10, 0x1e, r3, asr 21"
    );
    test_display(
        &[0x83, 0xf7, 0x7e, 0x5a],
        "usat r10, 0x1e, r3, lsl 21"
    );
    test_display(
        &[0xa3, 0xf7, 0x7e, 0x5a],
        "usat r10, 0x1e, r3, asr 21"
    );
}
#[test]
fn test_decode_bitwise_32b_cases() {
    test_display(
        &[0x23, 0xf0, 0x7e, 0x5a],
        "bic r10, r3, 0x3f800000"
    );
    test_display(
        &[0x33, 0xf0, 0x7e, 0x5a],
        "bics r10, r3, 0x3f800000"
    );
    test_display(
        &[0x23, 0xea, 0x7e, 0x5a],
        "bic.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x33, 0xea, 0x7e, 0x5a],
        "bics.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x23, 0xf4, 0x7e, 0x5a],
        "bic r10, r3, 0x3f80"
    );
    test_display(
        &[0x33, 0xf4, 0x7e, 0x5a],
        "bics r10, r3, 0x3f80"
    );

    test_display(
        &[0x03, 0xf0, 0x7e, 0x5a],
        "and r10, r3, 0x3f800000"
    );
    test_display(
        &[0x13, 0xf0, 0x7e, 0x5a],
        "ands r10, r3, 0x3f800000"
    );
    test_display(
        &[0x03, 0xea, 0x7e, 0x5a],
        "and.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x13, 0xea, 0x7e, 0x5a],
        "ands.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x03, 0xf4, 0x7e, 0x5a],
        "and r10, r3, 0x3f80"
    );
    test_display(
        &[0x13, 0xf4, 0x7e, 0x5a],
        "ands r10, r3, 0x3f80"
    );

    test_display(
        &[0x43, 0xf0, 0x7e, 0x5a],
        "orr r10, r3, 0x3f800000"
    );
    test_display(
        &[0x53, 0xf0, 0x7e, 0x5a],
        "orrs r10, r3, 0x3f800000"
    );
    test_display(
        &[0x43, 0xea, 0x7e, 0x5a],
        "orr.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x53, 0xea, 0x7e, 0x5a],
        "orrs.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x43, 0xf4, 0x7e, 0x5a],
        "orr r10, r3, 0x3f80"
    );
    test_display(
        &[0x53, 0xf4, 0x7e, 0x5a],
        "orrs r10, r3, 0x3f80"
    );

    test_display(
        &[0x83, 0xf0, 0x7e, 0x5a],
        "eor r10, r3, 0x3f800000"
    );
    test_display(
        &[0x93, 0xf0, 0x7e, 0x5a],
        "eors r10, r3, 0x3f800000"
    );
    test_display(
        &[0x83, 0xea, 0x7e, 0x5a],
        "eor.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x93, 0xea, 0x7e, 0x5a],
        "eors.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x83, 0xf4, 0x7e, 0x5a],
        "eor r10, r3, 0x3f80"
    );
    test_display(
        &[0x93, 0xf4, 0x7e, 0x5a],
        "eors r10, r3, 0x3f80"
    );

    test_display(
        &[0x63, 0xf0, 0x7e, 0x5a],
        "orn r10, r3, 0x3f800000"
    );
    test_display(
        &[0x73, 0xf0, 0x7e, 0x5a],
        "orns r10, r3, 0x3f800000"
    );
    test_display(
        &[0x63, 0xea, 0x7e, 0x5a],
        "orn r10, r3, lr, ror 21"
    );
    test_display(
        &[0x73, 0xea, 0x7e, 0x5a],
        "orns r10, r3, lr, ror 21"
    );
    test_display(
        &[0x63, 0xf4, 0x7e, 0x5a],
        "orn r10, r3, 0x3f80"
    );
    test_display(
        &[0x73, 0xf4, 0x7e, 0x5a],
        "orns r10, r3, 0x3f80"
    );
}
#[test]
fn test_decode_arithmetic_32b_cases() {
    test_display(
        &[0xa3, 0xf1, 0x7e, 0x5a],
        "sub.w r10, r3, 0x3f800000"
    );
    test_display(
        &[0xb3, 0xf1, 0x7e, 0x5a],
        "subs.w r10, r3, 0x3f800000"
    );
    test_display(
        &[0xa3, 0xf2, 0x7e, 0x5a],
        "sub.w r10, r3, 0x57e"
    );
    test_display(
        &[0xa3, 0xeb, 0x7e, 0x5a],
        "sub.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0xb3, 0xeb, 0x7e, 0x5a],
        "subs.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0xa3, 0xf6, 0x7e, 0x5a],
        "sub.w r10, r3, 0xd7e"
    );
    test_display(
        &[0xa3, 0xf5, 0x7e, 0x5a],
        "sub.w r10, r3, 0x3f80"
    );
    test_display(
        &[0xb3, 0xf5, 0x7e, 0x5a],
        "subs.w r10, r3, 0x3f80"
    );

    test_display(
        &[0x03, 0xf1, 0x7e, 0x5a],
        "add.w r10, r3, 0x3f800000"
    );
    test_display(
        &[0x13, 0xf1, 0x7e, 0x5a],
        "adds.w r10, r3, 0x3f800000"
    );
    test_display(
        &[0x03, 0xf2, 0x7e, 0x5a],
        "add.w r10, r3, 0x57e"
    );
    test_display(
        &[0x03, 0xeb, 0x7e, 0x5a],
        "add.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x13, 0xeb, 0x7e, 0x5a],
        "adds.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x03, 0xf5, 0x7e, 0x5a],
        "add.w r10, r3, 0x3f80"
    );
    test_display(
        &[0x13, 0xf5, 0x7e, 0x5a],
        "adds.w r10, r3, 0x3f80"
    );
    test_display(
        &[0x03, 0xf6, 0x7e, 0x5a],
        "add.w r10, r3, 0xd7e"
    );

    test_display(
        &[0x43, 0xf1, 0x7e, 0x5a],
        "adc r10, r3, 0x3f800000"
    );
    test_display(
        &[0x53, 0xf1, 0x7e, 0x5a],
        "adcs r10, r3, 0x3f800000"
    );
    test_display(
        &[0x43, 0xeb, 0x7e, 0x5a],
        "adc.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x53, 0xeb, 0x7e, 0x5a],
        "adcs.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x43, 0xf5, 0x7e, 0x5a],
        "adc r10, r3, 0x3f80"
    );
    test_display(
        &[0x53, 0xf5, 0x7e, 0x5a],
        "adcs r10, r3, 0x3f80"
    );

    test_display(
        &[0x63, 0xf1, 0x7e, 0x5a],
        "sbc r10, r3, 0x3f800000"
    );
    test_display(
        &[0x73, 0xf1, 0x7e, 0x5a],
        "sbcs r10, r3, 0x3f800000"
    );
    test_display(
        &[0x63, 0xeb, 0x7e, 0x5a],
        "sbc.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x73, 0xeb, 0x7e, 0x5a],
        "sbcs.w r10, r3, lr, ror 21"
    );
    test_display(
        &[0x63, 0xf5, 0x7e, 0x5a],
        "sbc r10, r3, 0x3f80"
    );
    test_display(
        &[0x73, 0xf5, 0x7e, 0x5a],
        "sbcs r10, r3, 0x3f80"
    );

    test_display(
        &[0xc3, 0xf1, 0x7e, 0x5a],
        "rsb.w r10, r3, 0x3f800000"
    );
    test_display(
        &[0xd3, 0xf1, 0x7e, 0x5a],
        "rsbs.w r10, r3, 0x3f800000"
    );
    test_display(
        &[0xc3, 0xeb, 0x7e, 0x5a],
        "rsb r10, r3, lr, ror 21"
    );
    test_display(
        &[0xd3, 0xeb, 0x7e, 0x5a],
        "rsbs r10, r3, lr, ror 21"
    );
    test_display(
        &[0xc3, 0xf5, 0x7e, 0x5a],
        "rsb.w r10, r3, 0x3f80"
    );
    test_display(
        &[0xd3, 0xf5, 0x7e, 0x5a],
        "rsbs.w r10, r3, 0x3f80"
    );
}
#[ignore]
#[test]
fn test_decode_simd_32b_cases() {
    test_display(
        &[0x13, 0xed, 0x7e, 0x5a],
        "vldr s10, [r3, -0x1f8]"
    );
    test_display(
        &[0x53, 0xed, 0x7e, 0x5a],
        "vldr s11, [r3, -0x1f8]"
    );
    test_display(
        &[0x93, 0xed, 0x7e, 0x5a],
        "vldr s10, [r3, 0x1f8]"
    );
    test_display(
        &[0xd3, 0xed, 0x7e, 0x5a],
        "vldr s11, [r3, 0x1f8]"
    );

    test_display(
        &[0x03, 0xed, 0x7e, 0x5a],
        "vstr s10, [r3, -0x1f8]"
    );
    test_display(
        &[0x43, 0xed, 0x7e, 0x5a],
        "vstr s11, [r3, -0x1f8]"
    );
    test_display(
        &[0x83, 0xed, 0x7e, 0x5a],
        "vstr s10, [r3, 0x1f8]"
    );
    test_display(
        &[0xc3, 0xed, 0x7e, 0x5a],
        "vstr s11, [r3, 0x1f8]"
    );

    test_display(
        &[0x93, 0xec, 0x7e, 0x5a],
        "vldmia r3, {s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
    test_display(
        &[0xb3, 0xec, 0x7e, 0x5a],
        "vldmia r3!, {s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
    test_display(
        &[0xd3, 0xec, 0x7e, 0x5a],
        "vldmia r3, {s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
    test_display(
        &[0xf3, 0xec, 0x7e, 0x5a],
        "vldmia r3!, {s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
    test_display(
        &[0x33, 0xed, 0x7e, 0x5a],
        "vldmdb r3!, {s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
    test_display(
        &[0x73, 0xed, 0x7e, 0x5a],
        "vldmdb r3!, {s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );

    test_display(
        &[0x83, 0xec, 0x7e, 0x5a],
        "vstmia r3, {s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
    test_display(
        &[0xa3, 0xec, 0x7e, 0x5a],
        "vstmia r3!, {s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
    test_display(
        &[0xc3, 0xec, 0x7e, 0x5a],
        "vstmia r3, {s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
    test_display(
        &[0xe3, 0xec, 0x7e, 0x5a],
        "vstmia r3!, {s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
    test_display(
        &[0x23, 0xed, 0x7e, 0x5a],
        "vstmdb r3!, {s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
    test_display(
        &[0x63, 0xed, 0x7e, 0x5a],
        "vstmdb r3!, {s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31}"
    );
}
