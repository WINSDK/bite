use crate::armv7::{
    ConditionCode, CReg, Reg, RegShift,
    Decoder,
    Operand, Opcode, Instruction, StatusRegMask
};

use decoder::{ErrorKind, Reader};

use bitvec::prelude::*;

#[allow(non_snake_case)]
fn ROR_C(x: u32, shift: u16) -> (u32, bool) {
//    let m = shift % 32; // `shift` is known to be 31 or lower
    let m = shift;
    let result = (x >> m) | (x << (32 - m));
    let carry_out = (result >> 31) & 1 != 0;
    (result, carry_out)
}

#[allow(non_snake_case)]
fn ThumbExpandImm_C(imm: u16) -> Result<u32, ErrorKind> {
    if imm & 0b1100_0000_0000 == 0 {
        let ty = (imm >> 8) & 0b11;
        let imm_low = (imm & 0b11111111) as u32;
        match ty {
            0b00 => {
                Ok(imm_low)
            }
            0b01 => {
                if imm_low == 0 {
                    return Err(ErrorKind::Unpredictable);
                }
                Ok((imm_low << 16) | imm_low)
            }
            0b10 => {
                if imm_low == 0 {
                    return Err(ErrorKind::Unpredictable);
                }
                Ok((imm_low << 24) | (imm_low << 8))
            }
            0b11 => {
                if imm_low == 0 {
                    return Err(ErrorKind::Unpredictable);
                }
                Ok((imm_low << 24) | (imm_low << 16) | (imm_low << 8) | imm_low)
            }
            _ => {
                unreachable!("impossible bit pattern");
            }
        }
    } else {
        let unrotated_value = ((1 << 7) | (imm & 0b1111111)) as u32;
        let rot = (imm >> 7) & 0b11111;
        // TODO: figure out what to do with carry_out
        let (imm32, _carry_out) = ROR_C(unrotated_value, rot);
        Ok(imm32)
    }
}

#[allow(non_snake_case)]
fn DecodeImmShift(reg: u8, ty: u8, imm5: u8) -> RegShift {
    let imm = match ty {
        0b00 => { imm5 },
        0b01 => {
            if imm5 == 0 {
                32
            } else {
                imm5
            }
        },
        0b10 => {
            if imm5 == 0 {
                32
            } else {
                imm5
            }
        },
        0b11 => {
            // ignores the `if ty == 11 && imm5 = 00000 then shift_t = RRX && shift_n = 1`
            imm5
        }
        _ => {
            unreachable!("impossible bit pattern");
        }
    };
    RegShift::from_raw(
        0b00000 |    // `RegImm`
        reg as u16 |
        ((ty as u16) << 5)|
        ((imm as u16) << 7)
    )
}

pub fn read(
    decoder: &Decoder,
    words: &mut Reader,
    inst: &mut Instruction,
) -> Result<(), ErrorKind> {
    // these are cleared in `armv7::read`.
    // they must be reset when switching out of thumb decoding or decoding a new thumb instruction,
    // which that `decode_into` is the entrypoint for in all cases.
    // inst.set_w(false);
    // inst.set_wide(false);
    inst.set_thumb(true);
    let mut word_bytes = [0u8; 2];
    words.next_n(&mut word_bytes).ok_or(ErrorKind::ExhaustedInput)?;
    let word = u16::from_le_bytes(word_bytes);
    let instr = word;

    let mut instr2 = bitarr![Lsb0, u16; 0u16; 16];
    instr2[0..16].store(word);

    let opword = instr2[11..].load::<u16>();

    // `A6.1 Thumb instruction set encoding`
    if opword >= 0b11101 {
        inst.set_w(true);
        inst.set_wide(true);

        // 32b instruction - `A6-228, 32-bit Thumb instruction encoding`
        // opword low bits 01, 10, and 11 correspond to `op1` in table `A6-9`

        let mut word_bytes = [0u8; 2];
        words.next_n(&mut word_bytes).ok_or(ErrorKind::ExhaustedInput)?;
        let lower = u16::from_le_bytes(word_bytes);

        let mut lower2 = bitarr![Lsb0, u16; 0u16; 16];
        lower2[0..16].store(lower);

        let op2 = &instr2[4..11];

        if opword == 0b11101 {
            // op1 == 0b01
            // interpret `op1 == 0b01` lines of table `A6-9`
            if !op2[6] {
                // `op2` is `0b00.. ` or `0b01..`
                if !op2[5] {
                    // `op2` is `00xxxxx`, and is `Load/store`, either `multiple` or `dual`
                    let rn = instr2[..4].load::<u8>();
                    // TODO: double check
                    if op2[2] {
                        // `Load/store dual, load/store exclusive, table branch` (`A6-236`)
                        let op1op2 = (instr2[7..9].load::<u8>() << 2) | instr2[4..6].load::<u8>();
                        let imm8 = lower2[..8].load::<u16>();
                        let rd = lower2[8..12].load::<u8>();
                        let rt = lower2[12..16].load::<u8>();

                        // all only-wide, no w suffix
                        inst.set_w(false);

                        match op1op2 {
                            0b0000 => {
                                // `STREX` (`A8-691`)
                                // v6T2
                                if rd == 13 || rd == 15 || rt == 13 || rt == 15 || rn == 15 {
                                    decoder.unpredictable()?;
                                }
                                if rd == rn || rd == rt {
                                    decoder.unpredictable()?;
                                }
                                inst.opcode = Opcode::STREX;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rt)),
                                    // preindex for `[<Rn>, #<imm>]`, no writeback. imm is zero
                                    // extended, so not signed; always add.
                                    Operand::RegDerefPreindexOffset(
                                        Reg::from_u8(rn),
                                        imm8 << 2,
                                        true,
                                        false,
                                    ),
                                    Operand::Nothing,
                                ];
                            }
                            0b0001 => {
                                // `LDREX` (`A8-433`)
                                // v6T2
                                if rt == 13 || rt == 15 || rn == 15 {
                                    decoder.unpredictable()?;
                                }
                                // TODO: should_is_must()
                                // rd == 0b1111
                                inst.opcode = Opcode::LDREX;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rt)),
                                    // preindex for `[<Rn>, #<imm>]`, no writeback. imm is zero
                                    // extended, so not signed; always add.
                                    Operand::RegDerefPreindexOffset(
                                        Reg::from_u8(rn),
                                        imm8 << 2,
                                        true,
                                        false,
                                    ),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            0b0010 |
                            0b0110 => {
                                // `STRD (immediate)` (`A8-687`)
                                // v6T2
                                // bit 5 (w) == 0
                                let w = false;
                                let u = instr2[7];
                                let p = instr2[8];
                                // `if P == '0' && W == '0' then SEE "Related encodings"` -> this
                                // would imply tbb/tbh, should be unreachable
                                if rn == 15 || rt == 13 || rt == 15 || rd == 13 || rd == 15 {
                                    decoder.unpredictable()?;
                                }
                                inst.opcode = Opcode::STRD;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rt)),
                                    Operand::Reg(Reg::from_u8(rd)),
                                    if p {
                                        Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8 << 2, u, w)
                                    } else {
                                        // p == 0 and w == 0 is impossible, would be tbb/tbh
                                        Operand::RegDerefPostindexOffset(Reg::from_u8(rn), imm8 << 2, u, false)
                                    },
                                    Operand::Nothing,
                                ];
                            }
                            0b0011 |
                            0b0111 => {
                                // `LDRD (immediate)`/`(literal)` (`A8-687`)
                                // bit 5 (w) == 0
                                let w = false;
                                let u = instr2[7];
                                let p = instr2[8];
                                // `if P == '0' && W == '0' then SEE "Related encodings"` -> this
                                // would imply tbb/tbh, should be unreachable
                                if rt == 13 || rt == 15 || rd == 13 || rd == 15 || rd == rt {
                                    decoder.unpredictable()?;
                                }
                                if w && (rn == rt || rn == rd) {
                                    decoder.unpredictable()?;
                                }
                                if rn != 0b1111 {
                                    // `LDRD (immediate)` (`A8-427`)
                                    // v6T2
                                    inst.opcode = Opcode::LDRD;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        Operand::Reg(Reg::from_u8(rd)),
                                        if p {
                                            Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8 << 2, u, w)
                                        } else {
                                            // p == 0 and w == 0 is impossible, would be tbb/tbh
                                            Operand::RegDerefPostindexOffset(Reg::from_u8(rn), imm8 << 2, u, false)
                                        },
                                        Operand::Nothing,
                                    ];
                                } else {
                                    // `LDRD (literal)` (`A8-429`)
                                    // v6T2
                                    if w {
                                        decoder.unpredictable()?;
                                    }
                                    // which because !(p == 0 && w == 0), we know p is true
                                    inst.opcode = Opcode::LDRD;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8 << 2, u, false),
                                        Operand::Nothing,
                                    ];
                                }
                            }
                            0b0100 => {
                                // `STREX_`
                                // v7
                                let op3 = lower2[4..8].load::<u8>();
                                let rt2 = rd;
                                let rd = (imm8 & 0b1111) as u8;
                                match op3 {
                                    0b0100 => {
                                        // `STREXB` (`A8-693`)
                                        if rd == 13 || rd == 15 || rt == 13 || rt == 15 || rn == 15 {
                                            decoder.unpredictable()?;
                                        }
                                        if rd == rn || rd == rt {
                                            decoder.unpredictable()?;
                                        }
                                        // TODO: should_is_must()
                                        // rt2 == 0b1111
                                        inst.opcode = Opcode::STREXB;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::RegDeref(Reg::from_u8(rn)),
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b0101 => {
                                        // `STREXH` (`A8-693`)
                                        if rd == 13 || rd == 15 || rt == 13 || rt == 15 || rn == 15 {
                                            decoder.unpredictable()?;
                                        }
                                        if rd == rn || rd == rt {
                                            decoder.unpredictable()?;
                                        }
                                        // TODO: should_is_must()
                                        // rt2 == 0b1111
                                        inst.opcode = Opcode::STREXH;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::RegDeref(Reg::from_u8(rn)),
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b0111 => {
                                        // `STREXD` (`A8-693`)
                                        if rd == 13 || rd == 15 || rt == 13 || rt == 15 || rt2 == 13 || rt2 == 15 || rn == 15 {
                                            decoder.unpredictable()?;
                                        }
                                        if rd == rn || rd == rt || rd == rt2 {
                                            decoder.unpredictable()?;
                                        }
                                        inst.opcode = Opcode::STREXD;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::Reg(Reg::from_u8(rt2)),
                                            Operand::RegDeref(Reg::from_u8(rn)),
                                        ];
                                    }
                                    _ => {
                                        return Err(ErrorKind::Undefined);
                                    }
                                }
                            }
                            0b0101 => {
                                // `TBB`/`TBH`/`LDREX_`
                                let op3 = lower2[4..8].load::<u8>();
                                let rt2 = rd;
                                let rd = (imm8 & 0b1111) as u8;
                                match op3 {
                                    0b0000 => {
                                        // `TBB`
                                        // TODO: should_is_must()
                                        // rt == 0b1111
                                        // rd == 0b0000
                                        inst.opcode = Opcode::TBB;
                                        inst.operands = [
                                            Operand::RegDerefPreindexReg(
                                                Reg::from_u8(rn),
                                                Reg::from_u8(rd),
                                                true, // add
                                                false, // no wback
                                            ),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b0001 => {
                                        // `TBH`
                                        // TODO: should_is_must()
                                        // rt == 0b1111
                                        // rd == 0b0000
                                        inst.opcode = Opcode::TBB;
                                        inst.operands = [
                                            Operand::RegDerefPreindexRegShift(
                                                Reg::from_u8(rn),
                                                // want `<Rm>, LSL #1`, construct a raw shift
                                                // ourselves
                                                RegShift::from_raw(
                                                    0b10000 |        // `RegImm`
                                                    rd as u16 |            // reg == rd
                                                    (0b00 << 5) |   // LSL
                                                    (1 << 7)        // shift == #1
                                                ),
                                                true, // add
                                                false, // no wback
                                            ),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b0100 => {
                                        // `LDREXB`
                                        if rt == 13 || rt == 15 || rn == 15 {
                                            decoder.unpredictable()?;
                                        }
                                        // TODO: should_is_must()
                                        // rt2 == 0b1111
                                        // rd == 0b1111
                                        inst.opcode = Opcode::LDREXB;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::RegDeref(Reg::from_u8(rn)),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b0101 => {
                                        // `LDREXH`
                                        if rt == 13 || rt == 15 || rn == 15 {
                                            decoder.unpredictable()?;
                                        }
                                        // TODO: should_is_must()
                                        // rt2 == 0b1111
                                        // rd == 0b1111
                                        inst.opcode = Opcode::LDREXH;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::RegDeref(Reg::from_u8(rn)),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b0110 => {
                                        // `LDREXD`
                                        if rt == 13 || rt == 15 || rt2 == 13 || rt2 == 15 || rn == 15 {
                                            decoder.unpredictable()?;
                                        }
                                        // TODO: should_is_must()
                                        // rd == 0b1111
                                        inst.opcode = Opcode::LDREXD;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::Reg(Reg::from_u8(rt2)),
                                            Operand::RegDeref(Reg::from_u8(rn)),
                                            Operand::Nothing,
                                        ];
                                    }
                                    _ => {
                                        return Err(ErrorKind::Undefined);
                                    }
                                }
                            }
                            0b1000 |
                            0b1010 |
                            0b1100 |
                            0b1110 => {
                                // `STRD (immediate)` (`A8-687`)
                                // v6T2
                                let w = instr2[5];
                                let u = instr2[7];
                                let p = instr2[8];
                                // `if P == '0' && W == '0' then SEE "Related encodings"` -> this
                                // would imply tbb/tbh, should be unreachable
                                if rn == 15 || rt == 13 || rt == 15 || rd == 13 || rd == 15 {
                                    decoder.unpredictable()?;
                                }
                                inst.opcode = Opcode::STRD;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rt)),
                                    Operand::Reg(Reg::from_u8(rd)),
                                    if p {
                                        Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8 << 2, u, w)
                                    } else {
                                        // p == 0 and w == 0 is impossible, would be tbb/tbh
                                        Operand::RegDerefPostindexOffset(Reg::from_u8(rn), imm8 << 2, u, false)
                                    },
                                    Operand::Nothing,
                                ];
                            }
                            0b1001 |
                            0b1011 |
                            0b1101 |
                            0b1111 => {
                                // `LDRD (immediate)` (`A8-687`)
                                // v6T2
                                let w = instr2[5];
                                let u = instr2[7];
                                let p = instr2[8];
                                // `if P == '0' && W == '0' then SEE "Related encodings"` -> this
                                // would imply tbb/tbh, should be unreachable
                                if rt == 13 || rt == 15 || rd == 13 || rd == 15 || rd == rt {
                                    decoder.unpredictable()?;
                                }
                                if w && (rn == rt || rn == rd) {
                                    decoder.unpredictable()?;
                                }
                                if rn != 0b1111 {
                                    // `LDRD (immediate)` (`A8-427`)
                                    // v6T2
                                    inst.opcode = Opcode::LDRD;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        Operand::Reg(Reg::from_u8(rd)),
                                        if p {
                                            Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8 << 2, u, w)
                                        } else {
                                            // p == 0 and w == 0 is impossible, would be tbb/tbh
                                            Operand::RegDerefPostindexOffset(Reg::from_u8(rn), imm8 << 2, u, false)
                                        },
                                        Operand::Nothing,
                                    ];
                                } else {
                                    // `LDRD (literal)` (`A8-429`)
                                    // v6T2
                                    if w {
                                        decoder.unpredictable()?;
                                    }
                                    // which because !(p == 0 && w == 0), we know p is true
                                    inst.opcode = Opcode::LDRD;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8 << 2, u, false),
                                        Operand::Nothing,
                                    ];
                                }
                            }
                            _ => {
                                unreachable!("impossible bit pattern");
                            }
                        }
                    } else {
                        let w = instr2[5];
                        // `Load/store multiple` (`A6-235`)
                        if !instr2[4] {
                            // `L == 0`
                            match instr2[7..9].load::<u8>() {
                                0b00 => {
                                    // `SRS (Thumb)` (`B9-1990`)
                                    // v6T2
                                    inst.opcode = Opcode::SRS(false, true); // `srsdb`
                                    inst.operands = [
                                        Operand::RegWBack(Reg::from_u8(13), w),
                                        Operand::Imm12(lower2[0..4].load::<u16>()), // #<mode> ? what's the syntax here? #<the literal>?
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b01 => {
                                    // `STM (STMIA, STMEA)` (`A8-665`)
                                    // v6T2
                                    if rn == 15 || lower.count_ones() < 2 {
                                        decoder.unpredictable()?;
                                    }
                                    if w && (lower & (1 << rn)) != 0 {
                                        decoder.unpredictable()?;
                                    }
                                    inst.opcode = Opcode::STM(
                                        true, // add
                                        false, // preincrement
                                        w, // wback
                                        true, // usermode
                                    );
                                    inst.operands = [
                                        Operand::RegWBack(Reg::from_u8(rn), w),
                                        Operand::RegList(lower),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b10 => {
                                    // `STMDB`/`STMFD` (`A8-669`)
                                    // or `PUSH` (`A8-539`)

                                    // implied by instruction?
                                    inst.set_w(false);

                                    if w && rn == 0b1101 {
                                        // `PUSH`
                                        // v6T2
                                        if lower.count_ones() < 2 {
                                            decoder.unpredictable()?;
                                        }
                                        inst.opcode = Opcode::PUSH;
                                        inst.operands = [
                                            Operand::RegList(lower),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        // `STMDB`
                                        // v6T2
                                        if rn == 15 || lower.count_ones() < 2 {
                                            decoder.unpredictable()?;
                                        }
                                        if w && (lower & (1 << rn)) != 0 {
                                            decoder.unpredictable()?;
                                        }
                                        inst.opcode = Opcode::STM(
                                            false, // decrement
                                            true, // preincrement
                                            w, // wback
                                            true, // usermode?
                                        );
                                        inst.operands = [
                                            Operand::RegWBack(Reg::from_u8(rn), w),
                                            Operand::RegList(lower),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                }
                                0b11 => {
                                    // `SRS (Thumb)` (`B9-1990`)
                                    // v6T2
                                    inst.opcode = Opcode::SRS(false, true); // `srsia`
                                    inst.operands = [
                                        Operand::RegWBack(Reg::from_u8(13), w),
                                        Operand::Imm12(lower & 0b1111), // #<mode> ? what's the syntax here? #<the literal>?
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                _ => {
                                    unreachable!();
                                }
                            }
                        } else {
                            // `L == 1`
                            match instr2[7..9].load::<u8>() {
                                0b00 => {
                                    // `RFE` (`B9-1986`)
                                    // v6T2
                                    if rn == 15 {
                                        decoder.unpredictable()?;
                                    }
                                    inst.opcode = Opcode::RFE(false, true);
                                    inst.operands = [
                                        Operand::RegWBack(Reg::from_u8(rn), w),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b01 => {
                                    // `LDM/LDMIA/LDMFD (Thumb)` (`A8-397`)
                                    if w && rn == 0b1101 {
                                        // `POP` (`A8-535`)
                                        if lower.count_ones() < 2 || (lower & 0xc000) == 0xc000 {
                                            decoder.unpredictable()?;
                                        }
                                        inst.opcode = Opcode::POP;
                                        inst.operands = [
                                            Operand::RegList(lower),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        // `LDM/LDMIA/LDMFD`
                                        if rn == 15 || lower.count_ones() < 2 {
                                            decoder.unpredictable()?;
                                        }
                                        if w && (lower & (1 << rn)) != 0 {
                                            decoder.unpredictable()?;
                                        }
                                        inst.opcode = Opcode::LDM(true, false, w, true);
                                        inst.operands = [
                                            Operand::RegWBack(Reg::from_u8(rn), w),
                                            Operand::RegList(lower),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                }
                                0b10 => {
                                    // `LDMDB/LDMEA` (`A8-403`)

                                    // wide is implied
                                    inst.set_w(false);

                                    if rn == 15 || lower.count_ones() < 2 {
                                        decoder.unpredictable()?;
                                    }
                                    if w && (lower & (1 << rn)) != 0 {
                                        decoder.unpredictable()?;
                                    }
                                    inst.opcode = Opcode::LDM(false, true, w, true);
                                    inst.operands = [
                                        Operand::RegWBack(Reg::from_u8(rn), w),
                                        Operand::RegList(lower),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                0b11 => {
                                    // `RFE` (`B9-1986`)
                                    // v6T2
                                    if rn == 15 {
                                        decoder.unpredictable()?;
                                    }
                                    inst.opcode = Opcode::RFE(true, false);
                                    inst.operands = [
                                        Operand::RegWBack(Reg::from_u8(rn), w),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                _ => {
                                    unreachable!();
                                }
                            }
                        }
                    }
                } else {
                    // op2 is `01xxxxx` and is:
                    // `Data-processing (shfited register)` (`A6-241`)
                    // v6T2
                    let op = op2[1..5].load::<u8>();
                    let s = instr2[4];
                    let rn = instr2[0..4].load::<u8>();
                    let rd = lower2[8..12].load::<u8>();

                    let imm3 = lower2[12..15].load::<u16>();
                    let imm2 = lower2[6..8].load::<u16>();
                    let tp = lower2[4..6].load::<u16>();
                    let rm = lower2[0..4].load::<u16>();

                    let shift = RegShift::from_raw(
                        0b00000 | // reg-imm shift
                        rm as u16 |
                        (imm2 << 7) | (imm3 << 9) |
                        tp << 5
                    );
                    let shift = Operand::RegShift(shift);

                    inst.s = s;

                    match op {
                        0b0000 => {
                            if rd == 0b1111 && s {
                                // `TST` (`A8-747`)
                                // v6T2
                                inst.s = false;
                                inst.opcode = Opcode::TST;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rn)),
                                    shift,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `AND` (`A8-324`)
                                // v6T2
                                inst.opcode = Opcode::AND;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    shift,
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b0001 => {
                            // `BIC` (`A8-340`)
                            // v6T2
                            inst.opcode = Opcode::BIC;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                shift,
                                Operand::Nothing,
                            ];
                        }
                        0b0010 => {
                            if rn == 0b1111 {
                                // `Move register and immediate shifts`, also `A6-241`
                                let tp = (lower >> 4) & 0b11;
                                let imm2 = (lower >> 6) & 0b11;
                                let imm3 = (lower >> 12) & 0b111;
                                let imm5 = (imm3 << 2) | imm2;
                                match tp {
                                    0b00 => {
                                        if imm5 == 0 {
                                            // `MOV (register, Thumb)` (`A8-487`)
                                            // encoding T3
                                            inst.set_w(true);
                                            let rm = lower2[..4].load::<u8>();
                                            let rd = lower2[8..12].load::<u8>();
                                            inst.opcode = Opcode::MOV;
                                            inst.operands = [
                                                Operand::Reg(Reg::from_u8(rd)),
                                                Operand::Reg(Reg::from_u8(rm)),
                                                Operand::Nothing,
                                                Operand::Nothing,
                                            ];
                                        } else {
                                            // `LSL (immediate)` (`A8-469`)
                                            // encoding T2
                                            inst.set_w(true);
                                            let rm = lower2[..4].load::<u8>();
                                            let rd = lower2[8..12].load::<u8>();
                                            inst.opcode = Opcode::LSL;
                                            inst.operands = [
                                                Operand::Reg(Reg::from_u8(rd)),
                                                Operand::Reg(Reg::from_u8(rm)),
                                                Operand::Imm12(imm5),
                                                Operand::Nothing,
                                            ];
                                        }
                                    },
                                    0b01 => {
                                        // `LSR (immediate)` (`A8-473`)
                                        // encoding T2
                                        inst.set_w(true);
                                        let rm = lower2[..4].load::<u8>();
                                        let rd = lower2[8..12].load::<u8>();
                                        inst.opcode = Opcode::LSR;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                            Operand::Imm12(imm5),
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b10 => {
                                        // `ASR (immediate)` (`A8-328`)
                                        // encoding T2
                                        inst.set_w(true);
                                        let rm = lower2[..4].load::<u8>();
                                        let rd = lower2[8..12].load::<u8>();
                                        inst.opcode = Opcode::ASR;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                            Operand::Imm12(imm5),
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b11 => {
                                        if imm5 == 0 {
                                            // `RRX` (`A8-573`)
                                            // encoding T1
                                            inst.set_w(false);
                                            let rm = lower2[..4].load::<u8>();
                                            let rd = lower2[8..12].load::<u8>();
                                            inst.opcode = Opcode::RRX;
                                            inst.operands = [
                                                Operand::Reg(Reg::from_u8(rd)),
                                                Operand::Reg(Reg::from_u8(rm)),
                                                Operand::Nothing,
                                                Operand::Nothing,
                                            ];
                                        } else {
                                            // `ROR (immediate)` (`A8-569`)
                                            // encoding T1
                                            inst.set_w(false);
                                            let rm = lower2[..4].load::<u8>();
                                            let rd = lower2[8..12].load::<u8>();
                                            inst.opcode = Opcode::ASR;
                                            inst.operands = [
                                                Operand::Reg(Reg::from_u8(rd)),
                                                Operand::Reg(Reg::from_u8(rm)),
                                                Operand::Imm12(imm5),
                                                Operand::Nothing,
                                            ];
                                        }
                                    }
                                    _ => {
                                        unreachable!("impossible bit pattern for `tp`");
                                    }
                                }
                            } else {
                                // `ORR` (`A8-519`)
                                // v6T2
                                inst.opcode = Opcode::ORR;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    shift,
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b0011 => {
                            if rn == 0b1111 {
                                // `MVN` (`A8-507`)
                                // v6T2
                                inst.opcode = Opcode::MVN;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    shift,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `ORN` (`A8-515`)
                                // v6T2
                                inst.set_w(false);
                                inst.opcode = Opcode::ORN;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    shift,
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b0100 => {
                            if rd == 0b1111 && s {
                                // `TEQ` (`A8-741`)
                                // v6T2
                                inst.s = false;
                                inst.opcode = Opcode::TEQ;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rn)),
                                    shift,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `EOR` (`A8-385`)
                                // v6T2
                                inst.opcode = Opcode::EOR;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    shift,
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b0110 => {
                            // `PKH` (`A8-523`)
                            // v6T2
                            // TODO: fix shift
                            // TODO: check opcode
                            inst.s = false;
                            inst.opcode = if lower & 0b10000 != 0 {
                                Opcode::PKHTB
                            } else {
                                Opcode::PKHBT
                            };
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                shift,
                                Operand::Nothing,
                            ];
                        }
                        0b1000 => {
                            if rd == 0b1111 && s {
                                // `CMN` (`A8-364`)
                                // v6T2
                                inst.s = false;
                                inst.opcode = Opcode::CMN;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rn)),
                                    shift,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `ADD` (`A8-308`)
                                inst.opcode = Opcode::ADD;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    shift,
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b1010 => {
                            // `ADC` (`A8-300`)
                            // v6T2
                            inst.opcode = Opcode::ADC;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                shift,
                                Operand::Nothing,
                            ];
                        }
                        0b1011 => {
                            // `SBC` (`A8-595`)
                            // v6T2
                            inst.opcode = Opcode::SBC;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                shift,
                                Operand::Nothing,
                            ];
                        }
                        0b1101 => {
                            if rd == 0b1111 && s {
                                // `CMP` (`A8-370`)
                                // v6T2
                                inst.s = false;
                                inst.opcode = Opcode::CMP;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rn)),
                                    shift,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `SUB` (`A8-713`)
                                // v6T2
                                inst.opcode = Opcode::SUB;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    shift,
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b1110 => {
                            // `RSB` (`A8-577`)
                            inst.set_w(false);
                            inst.opcode = Opcode::RSB;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                shift,
                                Operand::Nothing,
                            ];
                        }
                        _ => {
                            // undefined encoding
                            return Err(ErrorKind::Undefined);
                        }
                    }
                }
            } else {
                // `Coprocessor, Advanced SIMD, and Floating-point instructions` (`A6-249`)
                // v6T2
                // op1 == 01, op2 == 1xxxxxx
                // this means `assert!(instr2[10])`
                return decode_table_a6_30(decoder, inst, instr2, lower2);
            }
        } else if opword == 0b11110 {
            // op1 == 0b10
            // interpret `op1 == 0b10` lines in table `A6-9` on `A6-228`:
            if !lower2[15] {
                // op == 0
                if !op2[5] {
                    // `A6.3.1` `Data-processing (modified immediate)` (`A6-229`)
                    // see `A6.3.2` for `Modified immediate constants in Thumb instructions` on how
                    // to decode immediates
                    // v6T2
                    let op = op2[1..5].load::<u8>();
                    let i = instr2[10..11].load::<u16>();
                    let s = instr2[4];
                    let rn = instr2[0..4].load::<u8>();
                    let imm3 = lower2[12..15].load::<u16>();
                    let rd = lower2[8..12].load::<u8>();
                    let imm8 = lower2[0..8].load::<u16>();
                    let imm = (i << 11) | (imm3 << 8) | imm8;

                    inst.s = s;

                    let imm = ThumbExpandImm_C(imm)?;

                    match op {
                        0b0000 => {
                            if rd == 0b1111 && s {
                                // `TST` (`A8-745`)
                                // v6T2
                                inst.s = false;
                                inst.opcode = Opcode::TST;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `AND` (`A8-322`)
                                // v6T2
                                inst.set_w(false);
                                inst.opcode = Opcode::AND;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b0001 => {
                            // `BIC` (`A8-338`)
                            // v6T2
                            inst.set_w(false);
                            inst.opcode = Opcode::BIC;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                Operand::Imm32(imm as u32),
                                Operand::Nothing,
                            ];
                        }
                        0b0010 => {
                            if rn == 0b1111 {
                                // `MOV` (`A8-485`)
                                // v6T2
                                inst.opcode = Opcode::MOV;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `ORR` (`A8-517`)
                                // v6T2
                                inst.set_w(false);
                                inst.opcode = Opcode::ORR;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b0011 => {
                            if rn == 0b1111 {
                                // `MVN` (`A8-505`)
                                // v6T2
                                inst.opcode = Opcode::MOV;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `ORN` (`A8-513`)
                                // v6T2
                                inst.set_w(false);
                                inst.opcode = Opcode::ORN;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b0100 => {
                            if rd == 0b1111 && s {
                                // `TEQ` (`A8-739`)
                                // v6T2
                                inst.s = false;
                                inst.opcode = Opcode::TEQ;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `EOR` (`A8-383`)
                                // v6T2
                                inst.set_w(false);
                                inst.opcode = Opcode::EOR;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b1000 => {
                            if rd == 0b1111 && s {
                                // `CMN` (`A8-362`)
                                // v6T2
                                inst.s = false;
                                inst.opcode = Opcode::CMN;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `ADD` (`A8-304`)
                                // v6T2
                                inst.opcode = Opcode::ADD;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b1010 => {
                            // `ADC` (`A8-298`)
                            // v6T2
                            inst.set_w(false);
                            inst.opcode = Opcode::ADC;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                Operand::Imm32(imm as u32),
                                Operand::Nothing,
                            ];
                        }
                        0b1011 => {
                            // `SBC` (`A8-593`)
                            // v6T2
                            inst.set_w(false);
                            inst.opcode = Opcode::SBC;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                Operand::Imm32(imm as u32),
                                Operand::Nothing,
                            ];
                        }
                        0b1101 => {
                            if rd == 0b1111 && s {
                                // `CMP` (`A8-368`)
                                // v6T2
                                inst.s = false;
                                inst.opcode = Opcode::CMP;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `SUB` (`A8-709`)
                                // v6T2
                                inst.opcode = Opcode::SUB;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b1110 => {
                            // `RSB` (`A8-575`)
                            // v6T2
                            inst.opcode = Opcode::RSB;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                Operand::Imm32(imm as u32),
                                Operand::Nothing,
                            ];
                        }
                        _ => {
                            // undefined encoding
                            return Err(ErrorKind::Undefined);
                        }
                    }
                } else {
                    // `Data-processing (plain binary immediate)` (`A6-232`)
                    // v6T2
                    // aka table `A6-12`
                    let op = instr2[4..9].load::<u8>();
                    let i = instr2[10..11].load::<u16>();
                    inst.s = false;
                    inst.set_w(false);
                    let rn = instr2[0..4].load::<u8>();
                    let imm3 = lower2[12..15].load::<u16>();
                    let rd = lower2[8..12].load::<u8>();
                    let imm8 = lower2[0..8].load::<u16>();
                    let imm = (i << 11) | (imm3 << 8) | imm8;

                    match op {
                        0b00000 => {
                            if rn != 0b1111 {
                                // `ADD` (`A8-304`)
                                // v6T2
                                // encoding T4
                                inst.set_w(true);
                                inst.opcode = Opcode::ADD;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                ];
                            } else {
                                // `ADR` (`A8-320`)
                                // v6T2
                                // encoding T3
                                // handle "add = TRUE" and "add = FALSE" by calling this add/sub
                                inst.opcode = Opcode::ADD;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(15)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b00100 => {
                            // `MOV` (`A8-485`)
                            // encoding T3
                            inst.opcode = Opcode::MOV;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Imm32(imm as u32 | ((rn as u32) << 16)),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b01010 => {
                            if rn != 0b1111 {
                                // `SUB` (`A8-709`)
                                // v6T2
                                // encoding T4
                                inst.set_w(true);
                                inst.opcode = Opcode::SUB;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                ];
                            } else {
                                // `ADR` (`A8-320`)
                                // v6T2
                                // encoding T2
                                // handle "add = TRUE" and "add = FALSE" by calling this add/sub
                                inst.opcode = Opcode::SUB;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(15)),
                                    Operand::Imm32(imm as u32),
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b01100 => {
                            // `MOVT` (`A8-492`)
                            // v6T2
                            inst.opcode = Opcode::MOVT;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Imm32(imm as u32 | ((rn as u32) << 16)),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b10000 => {
                            // `SSAT` (`A8-653`)
                            // v6T2
                            let imm3_2 = ((lower >> 10) & 0b11100) | ((lower >> 6) & 0b11);
                            let sh = 0; // from the opcode
                            let shift = DecodeImmShift(rn, sh << 1, imm3_2 as u8);
                            inst.opcode = Opcode::SSAT;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Imm32((lower & 0b11111) as u32),
                                Operand::RegShift(shift),
                                Operand::Nothing,
                            ];
                        }
                        0b10010 => {
                            let imm3_2 = ((lower >> 10) & 0b11100) | ((lower >> 6) & 0b11);
                            if imm3_2 != 0 {
                                let shift = DecodeImmShift(rn, instr2[5..6].load::<u8>() << 1, imm3_2 as u8);
                                // `SSAT`
                                // v6T2
                                inst.opcode = Opcode::SSAT;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Imm32((lower & 0b11111) as u32),
                                    Operand::RegShift(shift),
                                    Operand::Nothing,
                                ];
                            } else {
                                // `SSAT16`
                                // v6T2
                                inst.opcode = Opcode::SSAT16;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Imm32((lower & 0b11111) as u32),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b10100 => {
                            // `SBFX` (`A8-599`)
                            // v6T2

                            inst.opcode = Opcode::SBFX;
                            let imm3_2 = ((lower >> 10) & 0b11100) | ((lower >> 6) & 0b11);
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                Operand::Imm12(imm3_2),
                                Operand::Imm12((lower & 0b11111) + 1),
                            ];
                        }
                        0b10110 => {
                            if rn != 0b1111 {
                                // `BFI` (`A8-336`)
                                // v6T2
                                inst.opcode = Opcode::BFI;
                                let imm3_2 = ((lower >> 10) & 0b11100) | ((lower >> 6) & 0b11);
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm12(imm3_2),
                                    // TODO: this is `msb` but the operand here should be `width`
                                    Operand::Imm12(lower & 0b11111),
                                ];
                            } else {
                                // `BFC` (`A8-334`)
                                // v6T2
                                inst.opcode = Opcode::BFC;
                                let imm3_2 = ((lower >> 10) & 0b11100) | ((lower >> 6) & 0b11);
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Imm12(imm3_2),
                                    // TODO: this is `msb` but the operand here should be `width`
                                    Operand::Imm32((lower & 0b1111) as u32),
                                ];
                            }
                        }
                        0b11000 => {
                            // `USAT` (`A8-797`)
                            // v6T2
                            let imm3_2 = ((lower >> 10) & 0b11100) | ((lower >> 6) & 0b11);
                            let sh = 0; // from the opcode
                            let shift = DecodeImmShift(rn, sh << 1, imm3_2 as u8);
                            inst.opcode = Opcode::USAT;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Imm32((lower & 0b11111) as u32),
                                Operand::RegShift(shift),
                                Operand::Nothing,
                            ];
                        }
                        0b11010 => {
                            let imm3_2 = ((lower >> 10) & 0b11100) | ((lower >> 6) & 0b11);
                            if imm3_2 != 0 {
                                let sh = 1; // from the opcode
                                let shift = DecodeImmShift(rn, sh << 1, imm3_2 as u8);
                                // `USAT`
                                // v6T2
                                inst.opcode = Opcode::USAT;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Imm32((lower & 0b11111) as u32),
                                    Operand::RegShift(shift),
                                    Operand::Nothing,
                                ];
                            } else {
                                // `USAT16`
                                // v6T2
                                inst.opcode = Opcode::USAT16;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Imm32((lower & 0b11111) as u32),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Nothing,
                                ];
                            }
                        }
                        0b11100 => {
                            // `UBFX` (`A8-757`)
                            // v6T2

                            inst.opcode = Opcode::UBFX;
                            let imm3_2 = ((lower >> 10) & 0b11100) | ((lower >> 6) & 0b11);
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                Operand::Imm12(imm3_2),
                                Operand::Imm12((lower & 0b11111) + 1),
                            ];
                        }
                        _ => {
                            return Err(ErrorKind::Undefined);
                        }
                    }
                }
            } else {
                // A6.3 op == 1
                // `Branches and miscellaneous control` (`A6-233`)
                let imm8 = lower2[0..8].load::<u16>();
                let op2 = lower2[8..12].load::<u8>();
                let op1 = lower2[12..15].load::<u8>();
                let op = instr2[4..11].load::<u8>();
                if op1 & 0b101 == 0b000 {
                    // TODO: This entire section appears wrong? what encoding is the conditional
                    // branch, none of those line up with the above components, or provided
                    // operands.
                    //
                    // the high bit of op is a sign bit, if a conditional branch. otherwise, it is
                    // 0 for valid instructiosn other than `udf`, `hvc`, and `smc`. `Branch` is
                    //   ruled out as `op1` is `0x1`, so see if this is any of the misc
                    //   instructions:
                    if op & 0b0111000 != 0b0111000 {
                        // `Conditional branch` (`A8-332`)
                        // v6T2
                        let imm11 = lower2[0..11].load::<u32>();
                        let imm6 = instr2[0..6].load::<u32>();
                        let s = instr2[10..11].load::<u32>();
                        let j1 = lower2[13..14].load::<u32>();
                        let j2 = lower2[11..12].load::<u32>();
                        let imm =
                            (imm11 as i32) |
                            ((imm6 as i32) << 11) |
                            ((j1 as i32) << 17) |
                            ((j2 as i32) << 18) |
                            ((s as i32) << 19);
                        let imm = (imm << 12) >> 12;
                        inst.condition = ConditionCode::build(((instr >> 6) & 0b1111) as u8);
                        inst.opcode = Opcode::B;
                        inst.operands = [
                            Operand::BranchThumbOffset(imm),
                            Operand::Nothing,
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    } else {
                        // some misc instruction, rule out `udf`, `hvc`, `smc`:
                        if op < 0b1000000 {
                            // misc instruction
                            if op < 0b0111010 {
                                // `MSR` in some form, slightly more work to figure this out
                                let rn = instr2[0..4].load::<u8>();
                                if imm8 & 0b00100000 != 0 {
                                    // `MSR` (`B9-1980`)
                                    // v7VE
                                    let sysm = (((lower >> 4) & 1) << 4) | ((lower >> 8) & 0b1111);
                                    let R = instr2[4];
                                    inst.opcode = Opcode::MSR;
                                    inst.operands = [
                                        // TODO: is this the appropriate banked reg?
                                        if let Some(op) = Reg::from_sysm(R, sysm as u8) {
                                            // TODO: from_sysm should succeed?
                                            op
                                        } else {
                                            return Err(ErrorKind::InvalidOperand);
                                        },
                                        Operand::Reg(Reg::from_u8(rn)),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else {
                                    if op == 0b0111000 {
                                        if op2 & 0b0011 == 0b00 {
                                            // `Move to Special register, Application level` (`A8-501`)
                                            let mask = (lower >> 10) & 0b11;
                                            let spec_reg = match mask {
                                                0b00 => {
                                                    // TODO: generally "unpredictable" is
                                                    // overridden by DecodeMode::Any but there's
                                                    // nothing to salvage here?
                                                    return Err(ErrorKind::Unpredictable);
                                                }
                                                0b01 => {
                                                    StatusRegMask::APSR_G
                                                }
                                                0b10 => {
                                                    StatusRegMask::APSR_NZCVQ
                                                }
                                                0b11 => {
                                                    StatusRegMask::APSR_NZCVQG
                                                }
                                                _ => {
                                                    unreachable!("impossible mask bits");
                                                }
                                            };
                                            inst.opcode = Opcode::MSR;
                                            inst.operands = [
                                                Operand::StatusRegMask(spec_reg),
                                                Operand::Reg(Reg::from_u8(rn)),
                                                Operand::Nothing,
                                                Operand::Nothing,
                                            ];
                                        } else {
                                            // `Move to Special register, System level` (`B9-1984`)
                                            let mask = lower2[8..12].load::<u8>();
                                            let R = instr2[4];
                                            inst.opcode = Opcode::MSR;
                                            inst.operands = [
                                                // TODO: is this the appropriate?
                                                if let Some(op) = Reg::from_sysm(R, mask) {
                                                    // TODO: from_sysm should succeed?
                                                    op
                                                } else {
                                                    return Err(ErrorKind::InvalidOperand);
                                                },
                                                Operand::Reg(Reg::from_u8(rn)),
                                                Operand::Nothing,
                                                Operand::Nothing,
                                            ];
                                        }
                                    } else {
                                        // `Move to Special register, System level` (`B9-1984`)
                                        let mask = lower2[8..12].load::<u8>();
                                        let R = instr2[4];
                                        inst.opcode = Opcode::MSR;
                                        inst.operands = [
                                            // TODO: is this the appropriate?
                                            if let Some(op) = Reg::from_sysm(R, mask) {
                                                // TODO: from_sysm should succeed?
                                                op
                                            } else {
                                                return Err(ErrorKind::InvalidOperand);
                                            },
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                }
                            } else if op < 0b0111011 {
                                // `Change Processor State, and hints` (`A6-234`)
                                let op1 = lower2[8..11].load::<u8>();
                                let op2 = lower2[0..8].load::<u8>();
                                if op1 != 0b000 {
                                    // `CPS (Thumb)` (`B9-1964`)
                                    // v6T2
                                    // encoding T2
                                    let mode = lower2[0..5].load::<u16>();
                                    let m = lower2[8];
                                    let aif = lower2[5..8].load::<u16>();
                                    let imod = lower2[9..11].load::<u8>();

                                    if !m && imod == 0b00 {
                                        // unreachable; would be a hint
                                    }
                                    if !m && mode != 0 {
                                        decoder.unpredictable()?;
                                    }
                                    if imod < 0b10 {
                                        if imod == 0b01 {
                                            decoder.unpredictable()?;
                                        }
                                        inst.opcode = Opcode::CPS_modeonly;
                                        inst.operands = [
                                            Operand::Imm12(mode),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        inst.opcode = Opcode::CPS(imod == 0b11);
                                        inst.operands = [
                                            Operand::Imm12(aif),
                                            if m {
                                                Operand::Imm12(mode)
                                            } else {
                                                Operand::Nothing
                                            },
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                } else {
                                    if op2 >= 0b11110000 {
                                        // `DBG` (`A8-378`)
                                        let option = lower2[0..4].load::<u16>();
                                        inst.opcode = Opcode::DBG;
                                        inst.operands = [
                                            Operand::Imm12(option),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        match op2 {
                                            0b00000000 => {
                                                // `NOP` (`A8-511`)
                                                // v6T2
                                                // TODO: should_is_must
                                                inst.opcode = Opcode::NOP;
                                                inst.operands = [
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                ];
                                            }
                                            0b00000001 => {
                                                // `YIELD` (`A8-1109`)
                                                // v7
                                                inst.opcode = Opcode::YIELD;
                                                inst.operands = [
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                ];
                                            }
                                            0b00000010 => {
                                                // `WFE` (`A8-1105`)
                                                // v7
                                                inst.opcode = Opcode::WFE;
                                                inst.operands = [
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                ];
                                            }
                                            0b00000011 => {
                                                // `WFI` (`A8-1107`)
                                                // v7
                                                inst.opcode = Opcode::WFI;
                                                inst.operands = [
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                ];
                                            }
                                            0b00000100 => {
                                                // `SEV` (`A8-607`)
                                                // v7
                                                inst.opcode = Opcode::SEV;
                                                inst.operands = [
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                ];
                                            }
                                            0b00010100 => {
                                                // `CSDB` (`A8-376`)
                                                // v6T2
                                                inst.opcode = Opcode::CSDB;
                                                inst.operands = [
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                    Operand::Nothing,
                                                ];
                                            }
                                            _ => {
                                                return Err(ErrorKind::Undefined);
                                            }
                                        }
                                    }
                                }
                            } else if op < 0b0111100 {
                                // `Miscellaneous control instructions` (`A6-235`)
                                let op = (lower >> 4) & 0b1111;
                                match op {
                                    0b0000 => {
                                        // `ENTERX` (`A9-1116`)
                                        // ThumbEE
                                        inst.opcode = Opcode::LEAVEX;
                                        inst.operands = [
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    },
                                    0b0001 => {
                                        // `ENTERX` (`A9-1116`)
                                        // ThumbEE
                                        inst.opcode = Opcode::ENTERX;
                                        inst.operands = [
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    },
                                    0b0010 => {
                                        // `CLREX` (`A8-358`)
                                        // v7
                                        inst.opcode = Opcode::CLREX;
                                        inst.operands = [
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    },
                                    0b0100 => {
                                        // `DSB` (`A8-381`)
                                        // v7
                                        let option = lower & 0b1111;
                                        inst.opcode = Opcode::DSB;
                                        inst.operands = [
                                            Operand::Imm12(option),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    },
                                    0b0101 => {
                                        // `DMB` (`A8-379`)
                                        // v7
                                        let option = lower & 0b1111;
                                        inst.opcode = Opcode::DMB;
                                        inst.operands = [
                                            Operand::Imm12(option),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    },
                                    0b0110 => {
                                        // `ISB` (`A8-390`)
                                        // v7
                                        let option = lower & 0b1111;
                                        inst.opcode = Opcode::ISB;
                                        inst.operands = [
                                            Operand::Imm12(option),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    },
                                    _ => {
                                        return Err(ErrorKind::Undefined);
                                    }
                                }
                            } else if op < 0b0111101 {
                                // `BXJ` (`A8-352`)
                                // v6T2
                                let rm = instr2[0..4].load::<u8>();
                                inst.opcode = Opcode::BXJ;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rm)),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else if op < 0b0111110 {
                                // `ERET` or `SUBS PC, LR`
                                // v6T2
                                // `v7VE` defines `ERET` here, identical to `subs pc, lr` with
                                // `imm8 == 0`. `v7VE` does not change the behavior of this
                                // instruction at `PL1`.
                                let imm8 = lower2[0..8].load::<u16>();
                                if imm8 == 0 {
                                    // `ERET` (`B9-1968`)
                                    // v6T2
                                    // if <v7VE, `subs pc, lr, #0`
                                    inst.opcode = Opcode::ERET;
                                    inst.operands = [
                                        Operand::Nothing,
                                        Operand::Nothing,
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else {
                                    // `SUBS PC, LR (Thumb)` (`B9-1996`)
                                    // v6T2
                                    inst.opcode = Opcode::SUB;
                                    inst.s = true;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(15)), // pc
                                        Operand::Reg(Reg::from_u8(14)), // lr
                                        Operand::Imm12(imm8),
                                        Operand::Nothing,
                                    ];
                                }
                            } else {
                                // `op` is `0b0111110` or `0b0111111`, both are `MRS` but there's
                                // some discerning to do still.
                                let imm8 = lower & 0b11111111;
                                if imm8 & 0b00100000 != 0 {
                                    // `MRS (Banked register)` (`B9-1978`)
                                    // v7VE
                                    let r = instr & 0b10000;
                                    let sysm = (lower & 0b10000) | (instr & 0b1111);
                                    let rd = ((lower >> 8) & 0b1111) as u8;
                                    inst.opcode = Opcode::MRS;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rd)),
                                        if let Some(op) = Reg::from_sysm(r != 0, sysm as u8) {
                                            // TODO: from_sysm should succeed?
                                            op
                                        } else {
                                            return Err(ErrorKind::InvalidOperand);
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else {
                                    if op == 0b0111110 {
                                        // `MRS` (`A8-497`)
                                        // v6T2
                                        inst.opcode = Opcode::MRS;
                                        let rd = ((lower >> 8) & 0b1111) as u8;
                                        inst.opcode = Opcode::MRS;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            // TODO: "<spec_reg>"?
                                            if let Some(op) = Reg::from_sysm(false, 0) {
                                                // TODO: from_sysm should succeed?
                                                op
                                            } else {
                                                return Err(ErrorKind::InvalidOperand);
                                            },
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        // `MRS` (`B9-1976`)
                                        // v6T2
                                        inst.opcode = Opcode::MRS;
                                        let rd = ((lower >> 8) & 0b1111) as u8;
                                        let r = (instr >> 4) & 1;
                                        inst.opcode = Opcode::MRS;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            // TODO: "<spec_reg>"?
                                            if let Some(op) = Reg::from_sysm(r != 0, 0) {
                                                // TODO: from_sysm should succeed?
                                                op
                                            } else {
                                                return Err(ErrorKind::InvalidOperand);
                                            },
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                }
                            }
                        } else if op == 0b1111111{
                            if op1 == 0b000 {
                                // `SMC` (aka `SMI`) (`B9-1988`)
                                // "Security Extensions"
                                let imm = instr & 0b1111;
                                inst.opcode = Opcode::SMC;
                                inst.operands = [
                                    Operand::Imm12(imm),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // `UDF` (`A8-759`)
                                // All (first defined in issue `C.a`)
                                // TODO: should this decode to an intentional `UDF`
                                // instruction?
                                return Err(ErrorKind::Undefined);
                            }
                        } else if op == 0b1111110 {
                            if op1 == 0b000 {
                                // `HVC` (`B8-1970`)
                                // v7VE
                                let imm = lower & 0b1111_1111_1111;
                                inst.opcode = Opcode::HVC;
                                inst.operands = [
                                    Operand::Imm12(imm),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // undefined, but by not being mentioned in the manual
                                return Err(ErrorKind::Undefined);
                            }
                        } else {
                            // undefined, but by not being mentioned in the manual
                            return Err(ErrorKind::Undefined);
                        }
                    }
                } else {
                    let imm11 = lower2[0..11].load::<u32>();
                    let imm10 = instr2[0..10].load::<u32>();
                    let j1 = lower2[13..14].load::<u32>();
                    let j2 = lower2[11..12].load::<u32>();
                    let s = instr2[10..11].load::<u32>();
                    let i1 = 0x1 ^ s ^ j1;
                    let i2 = 0x1 ^ s ^ j2;
                    let imm =
                        (imm11 as i32) |
                        ((imm10 as i32) << 11) |
                        ((i2 as i32) << 21) |
                        ((i1 as i32) << 22) |
                        ((s as i32) << 23);
                    let imm = (imm << 8) >> 8;
                    inst.operands = [
                        Operand::BranchThumbOffset(imm),
                        Operand::Nothing,
                        Operand::Nothing,
                        Operand::Nothing,
                    ];

                    if op1 & 0b101 == 0b001 {
                        // `Branch` (`A8-332`)
                        // T4 encoding
                        // v6T2
                        inst.opcode = Opcode::B;
                    } else if op1 & 0b101 == 0b100 {
                        // `Branch with Link and Exchange` (`A8-346`)
                        // `UNDEFINED` in v4T
                        // v5T
                        // Undefined if low bit of imm10 is set ("H")
                        if imm11 & 0x1 != 0 {
                            return Err(ErrorKind::Undefined);
                        }
                        inst.opcode = Opcode::BLX;
                    } else if op1 & 0b101 == 0b101 {
                        // `Brach with Link` (`A8-346`)
                        // v4T
                        inst.opcode = Opcode::BL;
                    } else {
                        // Permanently undefined by A6-13
                        return Err(ErrorKind::Undefined);
                    }
                }
            }
        } else {
            // working through table `A6-9 32-bit Thumb instruction encoding`

            // op1 == 0b11
            if !op2[6] {
                // not coprocessor, advanced simd, or floating point instructions
                if !op2[5] {
                    // loads, stores
                    if !op2[0] {
                        // store single item, or advanced simd load/store
                        if !op2[4] {
                            // `Store single data item` (`A6-240`)
                            let rn = instr2[0..4].load::<u8>();
                            let op1 = instr2[5..8].load::<u8>();
                            let size_bits = op1 & 0b011;
                            let has_imm12 = op1 & 0b100 != 0;
                            let op2 = (lower >> 6) & 0b111111;
                            match size_bits {
                                0b00 => {
                                    // `STRB_`
                                    if op2 == 0 {
                                        // `STRB (register)` (`A8-683`)
                                        // encoding T2
                                        // v6T2
                                        let rm = (lower & 0b1111) as u8;
                                        let imm2 = (lower >> 4) & 0b11;
                                        let rt = ((lower >> 12) & 0b1111) as u8;
                                        inst.opcode = Opcode::STRB;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::RegDerefPreindexRegShift(
                                                Reg::from_u8(rn),
                                                RegShift::from_raw(
                                                    // do things
                                                    0b00000 |       // imm shift
                                                    (imm2 << 7) |   // imm
                                                    rm as u16 |            // shiftee
                                                    (0b00 << 5) // shift style (lsl)
                                                ),
                                                true,   // add
                                                false,  // wback
                                            ),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else if (op2 & 0b111100) == 0b111000 {
                                        // `STRBT` (`A8-685`)
                                        // v6T2
                                        let imm8 = lower & 0b1111_1111;
                                        let rt = ((lower >> 12) & 0b1111) as u8;
                                        inst.opcode = Opcode::STRBT;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::RegDerefPreindexOffset(
                                                Reg::from_u8(rn),
                                                imm8,
                                                true,   // add
                                                false,  // wback
                                            ),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        // `STRB (immediate, Thumb)` (`A8-679`)
                                        // encoding T2/T3
                                        // v6T2
                                        let (imm, p, u, w) = if has_imm12 {
                                            let imm12 = lower & 0b1111_1111_1111;
                                            let p = true;
                                            let u = true;
                                            let w = false;
                                            (imm12, p, u, w)
                                        } else {
                                            let imm8 = lower & 0b1111_1111;
                                            let puw = (lower >> 8) & 0b111;
                                            let p = puw & 0b100 != 0;
                                            let u = puw & 0b010 != 0;
                                            let w = puw & 0b001 != 0;
                                            (imm8, p, u, w)
                                        };
                                        // assert!(puw != 0b110) // would be `strbt`
                                        let rt = ((lower >> 12) & 0b1111) as u8;
                                        inst.opcode = Opcode::STRB;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            // do the puw
                                            if p {
                                                Operand::RegDerefPreindexOffset(
                                                    Reg::from_u8(rn),
                                                    imm,
                                                    u, // add
                                                    w, // wback
                                                )
                                            } else {
                                                Operand::RegDerefPostindexOffset(
                                                    Reg::from_u8(rn),
                                                    imm,
                                                    u, // add
                                                    w, // wback
                                                )
                                            },
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                }
                                0b01 => {
                                    // `STRH_`
                                    // v6T2
                                    if op2 == 0 {
                                        // `STRH (register)` (`A8-703`)
                                        let rm = (lower & 0b1111) as u8;
                                        let imm2 = (lower >> 4) & 0b11;
                                        let rt = ((lower >> 12) & 0b1111) as u8;
                                        inst.opcode = Opcode::STRH;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::RegDerefPreindexRegShift(
                                                Reg::from_u8(rn),
                                                RegShift::from_raw(
                                                    // do things
                                                    0b00000 |       // imm shift
                                                    (imm2 << 7) |   // imm
                                                    rm as u16 |            // shiftee
                                                    (0b00 << 5) // shift style (lsl)
                                                ),
                                                true,   // add
                                                false,  // wback
                                            ),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else if (op2 & 0b111100) == 0b111000 {
                                        // `STRHT` (`A8-705`)
                                        let imm8 = lower & 0b1111_1111;
                                        let rt = ((lower >> 12) & 0b1111) as u8;
                                        inst.opcode = Opcode::STRHT;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::RegDerefPreindexOffset(
                                                Reg::from_u8(rn),
                                                imm8,
                                                true,   // add
                                                false,  // wback
                                            ),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        // `STRH (immediate, Thumb)` (`A8-699`)
                                        // encoding T2/T3
                                        // v6T2
                                        let (imm, p, u, w) = if has_imm12 {
                                            let imm12 = lower & 0b1111_1111_1111;
                                            let p = true;
                                            let u = true;
                                            let w = false;
                                            (imm12, p, u, w)
                                        } else {
                                            let imm8 = lower & 0b1111_1111;
                                            let puw = (lower >> 8) & 0b111;
                                            let p = puw & 0b100 != 0;
                                            let u = puw & 0b010 != 0;
                                            let w = puw & 0b001 != 0;
                                            (imm8, p, u, w)
                                        };
                                        // assert!(puw != 0b110) // would be `strbt`
                                        let rt = ((lower >> 12) & 0b1111) as u8;
                                        inst.opcode = Opcode::STRH;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            // do the puw
                                            if p {
                                                Operand::RegDerefPreindexOffset(
                                                    Reg::from_u8(rn),
                                                    imm,
                                                    u, // add
                                                    w, // wback
                                                )
                                            } else {
                                                Operand::RegDerefPostindexOffset(
                                                    Reg::from_u8(rn),
                                                    imm,
                                                    u, // add
                                                    w, // wback
                                                )
                                            },
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                }
                                0b10 => {
                                    // `STR_`
                                    if op2 == 0 {
                                        // `STR (register)` (`A8-677`)
                                        // v6T2
                                        let rm = (lower & 0b1111) as u8;
                                        let imm2 = (lower >> 4) & 0b11;
                                        let rt = ((lower >> 12) & 0b1111) as u8;
                                        inst.opcode = Opcode::STR;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::RegDerefPreindexRegShift(
                                                Reg::from_u8(rn),
                                                RegShift::from_raw(
                                                    // do things
                                                    0b00000 |       // imm shift
                                                    (imm2 << 7) |   // imm
                                                    rm as u16 |            // shiftee
                                                    (0b00 << 5) // shift style (lsl)
                                                ),
                                                true,   // add
                                                false,  // wback
                                            ),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else if (op2 & 0b111100) == 0b111000 {
                                        // `STRT` (`A8-707`)
                                        let imm8 = lower & 0b1111_1111;
                                        let rt = ((lower >> 12) & 0b1111) as u8;
                                        inst.opcode = Opcode::STRT;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            Operand::RegDerefPreindexOffset(
                                                Reg::from_u8(rn),
                                                imm8,
                                                true,   // add
                                                false,  // wback
                                            ),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        // `STR (immediate, Thumb)` (`A8-673`)
                                        // encoding T3/T4
                                        // v6T2
                                        let (imm, p, u, w) = if has_imm12 {
                                            let imm12 = lower & 0b1111_1111_1111;
                                            let p = true;
                                            let u = true;
                                            let w = false;
                                            (imm12, p, u, w)
                                        } else {
                                            let imm8 = lower & 0b1111_1111;
                                            let puw = (lower >> 8) & 0b111;
                                            let p = puw & 0b100 != 0;
                                            let u = puw & 0b010 != 0;
                                            let w = puw & 0b001 != 0;
                                            (imm8, p, u, w)
                                        };
                                        // assert!(puw != 0b110) // would be `strbt`
                                        let rt = ((lower >> 12) & 0b1111) as u8;
                                        inst.opcode = Opcode::STR;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rt)),
                                            // do the puw
                                            if p {
                                                Operand::RegDerefPreindexOffset(
                                                    Reg::from_u8(rn),
                                                    imm,
                                                    u, // add
                                                    w, // wback
                                                )
                                            } else {
                                                Operand::RegDerefPostindexOffset(
                                                    Reg::from_u8(rn),
                                                    imm,
                                                    u, // add
                                                    w, // wback
                                                )
                                            },
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                }
                                0b11 => {
                                    return Err(ErrorKind::Undefined);
                                }
                                _ => {
                                    unreachable!("impossible bit pattern");
                                }
                            }
                        } else {
                            // `Advanced SIMD element or structure load/store instructions`
                            // (`A7-273`)
                        }
                    } else {
                        // this section is a merger of three tables:
                        // `A6.3.9 Load byte, memory hints`
                        // `A6.3.8 Load halfword, memory hints`
                        // `A6.3.7 Load word`
                        // reached by the `00xx001`, `00xx011`, `00xx101` rows of table `A6-9`.

                        let op2 = lower2[6..12].load::<u16>();
                        let rt = lower2[12..16].load::<u8>();
                        let rn = instr2[0..4].load::<u8>();
                        let op1 = instr2[7..9].load::<u16>();

                        // load {byte, halfword, word} in table `A6-9`
                        let size = instr2[5..7].load::<usize>();
                        if size == 0b11 {
                            // `UNDEFINED`
                            return Err(ErrorKind::Undefined);
                        }

                        /*
                         * trying to reorder tables a6-20, a6-19, and a6-18 to factor out operand
                         * sizes where possible...
                         *
                         *  op1 |  op2 |  rn  |  rt  | size| `see`
                         *   00 |000000|!=1111|!=1111|  00 |`LDRB (register) A8-423`
                         *   00 |000000|!=1111|==1111|  00 |`PLD, PLDW (register) A8-529`
                         *   00 |000000|!=1111|!=1111|  01 |`LDRH (register) A8-447`
                         *   00 |000000|!=1111|==1111|  01 |`PLD, PLDW (register) A8-529`
                         *   00 |000000|!=1111|------|  10 |`LDR (register, Thumb) A8-413`
                         *
                         *   00 |1100xx|!=1111|!=1111|  00 |`LDRB (immediate, Thumb) A8-417`
                         *   00 |1100xx|!=1111|==1111|  00 |`PLD, PLDW (immediate) A8-525`
                         *   00 |1100xx|!=1111|!=1111|  01 |`LDRH (immediate, Thumb) A8-441`
                         *   00 |1100xx|!=1111|==1111|  01 |`PLD, PLDW (immediate) A8-525`
                         *   00 |1100xx|!=1111|------|  10 |`LDR (immediate, Thumb) A8-407`
                         *
                         *   00 |1110xx|!=1111|------|  00 |`LDRBT A8-425`
                         *   00 |1110xx|!=1111|------|  01 |`LDRHT A8-449`
                         *   00 |1110xx|!=1111|------|  10 |`LDRT A8-467`
                         *
                         *   00 |1xx1xx|!=1111|------|  00 |`LDRB (immediate, Thumb) A8-417`
                         *   00 |1xx1xx|!=1111|------|  01 |`LDRH (immediate, Thumb) A8-441`
                         *   00 |1xx1xx|!=1111|------|  10 |`LDR (immediate, Thumb) A8-407`
                         *
                         *   01 |------|!=1111|!=1111|  00 |`LDRB (immediate, Thumb) A8-417`
                         *   01 |------|!=1111|==1111|  00 |`PLD, PLDW (immediate) A8-525`
                         *   01 |------|!=1111|!=1111|  01 |`LDRH (immediate, Thumb) A8-441`
                         *   01 |------|!=1111|==1111|  01 |`PLD, PLDW (immediate) A8-525`
                         *   01 |------|!=1111|------|  10 |`LDR (immediate, Thumb) A8-407`
                         *
                         *   0x |------|==1111|!=1111|  00 |`LDRB (literal) A8-421`
                         *   0x |------|==1111|==1111|  00 |`PLD (literal) A8-527`
                         *   0x |------|==1111|!=1111|  01 |`LDRH (literal) A8-445`
                         *   0x |------|==1111|==1111|  01 |`PLD (literal) A8-527`
                         *   0x |------|==1111|------|  10 |`LDR (literal) A8-411`
                         *
                         *   1x |------|------|------|  10 |`UNDEFINED (cite: A6.3.7)`
                         *
                         *   10 |000000|!=1111|!=1111|  00 |`LDRSB (register) A8-455`
                         *   10 |000000|!=1111|!=1111|  01 |`LDRSH (register) A8-463`
                         *   10 |000000|!=1111|==1111|  00 |`PLI (register) A8-553`
                         *   10 |000000|!=1111|==1111|  01 |`Unallocated memory hint (treat as NOP)`
                         *
                         *   10 |1100xx|!=1111|!=1111|  00 |`LDRSB (immediate) A8-451`
                         *   10 |1100xx|!=1111|==1111|  00 |`PLI (immediate, literal) A8-531`
                         *   10 |1100xx|!=1111|!=1111|  01 |`LDRSH (immediate) A8-459`
                         *   10 |1100xx|!=1111|==1111|  01 |`Unallocated memory hint (treat as NOP)`
                         *
                         *   10 |1110xx|!=1111|------|  00 |`LDRSBT A8-457`
                         *   10 |1110xx|!=1111|------|  01 |`LDRSHT A8-465`
                         *
                         *   10 |1xx1xx|!=1111|------|  00 |`LDRSB (immediate) A8-451`
                         *   10 |1xx1xx|!=1111|------|  01 |`LDRSH (immediate) A8-459`
                         *
                         *   11 |------|!=1111|!=1111|  00 |`LDRSB (immediate) A8-451`
                         *   11 |------|!=1111|==1111|  00 |`PLI (immediate, literal) A8-531`
                         *   11 |------|!=1111|!=1111|  01 |`LDRSH (immediate) A8-459`
                         *   11 |------|!=1111|==1111|  01 |`Unallocated memory hint (treat as NOP)`
                         *   1x |------|==1111|!=1111|  00 |`LDRSB (literal) A8-453`
                         *   1x |------|==1111|==1111|  00 |`PLI (immediate, literal) A8-531`
                         *   1x |------|==1111|!=1111|  01 |`LDRSH (literal) A8-461`
                         *   1x |------|==1111|==1111|  01 |`Unallocated memory hint (treat as NOP)`
                         */
                        if op1 == 0b00 {
                            // op1 == bits 7:8
                            /*
                             *  op1 |  op2 |  rn  |  rt  | size| `see`
                             *   0x |------|==1111|!=1111|  00 |`LDRB (literal) A8-421`
                             *   0x |------|==1111|==1111|  00 |`PLD (literal) A8-527`
                             *   0x |------|==1111|!=1111|  01 |`LDRH (literal) A8-445`
                             *   0x |------|==1111|==1111|  01 |`PLD (literal) A8-527`
                             *   0x |------|==1111|------|  10 |`LDR (literal) A8-411`
                             *
                             *   00 |000000|!=1111|!=1111|  00 |`LDRB (register) A8-423`
                             *   00 |000000|!=1111|==1111|  00 |`PLD, PLDW (register) A8-529`
                             *   00 |000000|!=1111|!=1111|  01 |`LDRH (register) A8-447`
                             *   00 |000000|!=1111|==1111|  01 |`PLD, PLDW (register) A8-529`
                             *   00 |000000|!=1111|------|  10 |`LDR (register, Thumb) A8-413`
                             *
                             *   00 |1100xx|!=1111|!=1111|  00 |`LDRB (immediate, Thumb) A8-417`
                             *   00 |1100xx|!=1111|==1111|  00 |`PLD, PLDW (immediate) A8-525`
                             *   00 |1100xx|!=1111|!=1111|  01 |`LDRH (immediate, Thumb) A8-441`
                             *   00 |1100xx|!=1111|==1111|  01 |`PLD, PLDW (immediate) A8-525`
                             *   00 |1100xx|!=1111|------|  10 |`LDR (immediate, Thumb) A8-407`
                             *
                             *   00 |1110xx|!=1111|------|  00 |`LDRBT A8-425`
                             *   00 |1110xx|!=1111|------|  01 |`LDRHT A8-449`
                             *   00 |1110xx|!=1111|------|  10 |`LDRT A8-467`
                             *
                             *   00 |1xx1xx|!=1111|------|  00 |`LDRB (immediate, Thumb) A8-417`
                             *   00 |1xx1xx|!=1111|------|  01 |`LDRH (immediate, Thumb) A8-441`
                             *   00 |1xx1xx|!=1111|------|  10 |`LDR (immediate, Thumb) A8-407`
                             */
                            if rn == 0b1111 {
                                // `(literal)`
                                let opcode = if rt == 0b1111 {
                                    [
                                        Opcode::PLD,
                                        Opcode::PLD,
                                        Opcode::LDR,
                                    ][size]
                                } else {
                                    [
                                        Opcode::LDRB,
                                        Opcode::LDRH,
                                        Opcode::LDR,
                                    ][size]
                                };
                                let u = false; // instr2[7], but known 0 here
                                let imm12 = lower2[..12].load::<u16>();
                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rt)),
                                    Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm12, u, false), // no add, no wback
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                if op2 == 0b000000 {
                                    // `(register, Thumb)`
                                    let opcode = if rt == 0b1111 {
                                        [
                                            Opcode::PLD,
                                            Opcode::PLD,
                                            Opcode::LDR,
                                        ][size]
                                    } else {
                                        [
                                            Opcode::LDRB,
                                            Opcode::LDRH,
                                            Opcode::LDR,
                                        ][size]
                                    };
                                    let rm = lower2[0..4].load::<u8>();
                                    let imm2 = lower2[4..6].load::<u8>();
                                    inst.opcode = opcode;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        Operand::RegDerefPreindexRegShift(
                                            Reg::from_u8(rn),
                                            RegShift::from_raw(
                                                0b00000 |    // `RegImm`
                                                rm as u16 |
                                                ((0 /* lsl */) << 5)|
                                                ((imm2 as u16) << 7)
                                            ),
                                            true,   // add
                                            false,  // wback
                                        ),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else if op2 & 0b111100 == 0b110000 {
                                    // `(immediate, Thumb)`
                                    let opcode = if rt == 0b1111 {
                                        [
                                            Opcode::PLD,
                                            Opcode::PLD,
                                            Opcode::LDR,
                                        ][size]
                                    } else {
                                        [
                                            Opcode::LDRB,
                                            Opcode::LDRH,
                                            Opcode::LDR,
                                        ][size]
                                    };
                                    let w = lower2[8];
                                    let u = lower2[9];
                                    let p = lower2[10];
                                    let imm8 = lower2[..8].load::<u16>();
                                    inst.opcode = opcode;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        if p {
                                            Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8, u, w)
                                        } else {
                                            Operand::RegDerefPostindexOffset(Reg::from_u8(rn), imm8, u, false)
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];

                                } else if op2 & 0b111100 == 0b111000 {
                                    // `(immediate, Thumb)`
                                    let opcode = if rt == 0b1111 {
                                        [
                                            Opcode::PLD,
                                            Opcode::PLD,
                                            Opcode::LDRT,
                                        ][size]
                                    } else {
                                        [
                                            Opcode::LDRBT,
                                            Opcode::LDRHT,
                                            Opcode::LDRT,
                                        ][size]
                                    };
                                    let w = lower2[8];
                                    let u = lower2[9];
                                    let p = lower2[10];
                                    let imm8 = lower2[..8].load::<u16>();
                                    inst.opcode = opcode;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        if p {
                                            Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8, u, w)
                                        } else {
                                            Operand::RegDerefPostindexOffset(Reg::from_u8(rn), imm8, u, false)
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else if op2 & 0b100100 == 0b100100 {
                                    // `(immediate, Thumb)`
                                    let opcode = if rt == 0b1111 {
                                        [
                                            Opcode::PLD,
                                            Opcode::PLD,
                                            Opcode::LDR,
                                        ][size]
                                    } else {
                                        [
                                            Opcode::LDRB,
                                            Opcode::LDRH,
                                            Opcode::LDR,
                                        ][size]
                                    };
                                    let w = lower2[8];
                                    let u = lower2[9];
                                    let p = lower2[10];
                                    let imm8 = lower2[..8].load::<u16>();
                                    inst.opcode = opcode;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        if p {
                                            Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8, u, w)
                                        } else {
                                            Operand::RegDerefPostindexOffset(Reg::from_u8(rn), imm8, u, false)
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else {
                                    // op2 =~ 0b1010xx or something?
                                    // nothing to try decoding as for a `decoder.undefined()?`, so
                                    // just error.
                                    return Err(ErrorKind::Undefined);
                                }
                            }
                        } else if op1 == 0b01 {
                            // op1 == bits 7:8
                            /*
                             *  op1 |  op2 |  rn  |  rt  | size| `see`
                             *   0x |------|==1111|!=1111|  00 |`LDRB (literal) A8-421`
                             *   0x |------|==1111|==1111|  00 |`PLD (literal) A8-527`
                             *   0x |------|==1111|!=1111|  01 |`LDRH (literal) A8-445`
                             *   0x |------|==1111|==1111|  01 |`PLD (literal) A8-527`
                             *   0x |------|==1111|------|  10 |`LDR (literal) A8-411`
                             *
                             *   01 |------|!=1111|!=1111|  00 |`LDRB (immediate, Thumb) A8-417`
                             *   01 |------|!=1111|==1111|  00 |`PLD, PLDW (immediate) A8-525`
                             *   01 |------|!=1111|!=1111|  01 |`LDRH (immediate, Thumb) A8-441`
                             *   01 |------|!=1111|==1111|  01 |`PLD, PLDW (immediate) A8-525`
                             *   01 |------|!=1111|------|  10 |`LDR (immediate, Thumb) A8-407`
                             */
                            if rn == 0b1111 {
                                let opcode = if rt == 0b1111 {
                                    [
                                        Opcode::PLD,
                                        Opcode::PLD,
                                        Opcode::LDR,
                                    ][size]
                                } else {
                                    [
                                        Opcode::LDRB,
                                        Opcode::LDRH,
                                        Opcode::LDR,
                                    ][size]
                                };
                                let u = true; // instr2[7], but known 1 here
                                let imm12 = lower2[..12].load::<u16>();
                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rt)),
                                    Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm12, u, false), // add, no wback
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                let opcode = if rt == 0b1111 {
                                    [
                                        Opcode::PLD,
                                        Opcode::PLD,
                                        Opcode::LDR,
                                    ][size]
                                } else {
                                    [
                                        Opcode::LDRB,   // encoding T2
                                        Opcode::LDRH,   // encoding T2
                                        Opcode::LDR,    // encoding T3
                                    ][size]
                                };
                                let imm12 = lower2[..12].load::<u16>();
                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rt)),
                                    Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm12, true, false), // add, no wback
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                        } else if op1 == 0b10 {
                            // op1 == bits 7:8
                            if size == 0b10 {
                                return Err(ErrorKind::Undefined);
                            }
                            /*
                             *  op1 |  op2 |  rn  |  rt  | size| `see`
                             *   1x |------|------|------|  10 |`UNDEFINED (cite: A6.3.7)`
                             *   1x |------|==1111|!=1111|  00 |`LDRSB (literal) A8-453`
                             *   1x |------|==1111|==1111|  00 |`PLI (immediate, literal) A8-531`
                             *   1x |------|==1111|!=1111|  01 |`LDRSH (literal) A8-461`
                             *   1x |------|==1111|==1111|  01 |`Unallocated memory hint (treat as NOP)`
                             *
                             *   10 |000000|!=1111|!=1111|  00 |`LDRSB (register) A8-455`
                             *   10 |000000|!=1111|!=1111|  01 |`LDRSH (register) A8-463`
                             *   10 |000000|!=1111|==1111|  00 |`PLI (register) A8-553`
                             *   10 |000000|!=1111|==1111|  01 |`Unallocated memory hint (treat as NOP)`
                             *
                             *   10 |1100xx|!=1111|!=1111|  00 |`LDRSB (immediate) A8-451`
                             *   10 |1100xx|!=1111|==1111|  00 |`PLI (immediate, literal) A8-531`
                             *   10 |1100xx|!=1111|!=1111|  01 |`LDRSH (immediate) A8-459`
                             *   10 |1100xx|!=1111|==1111|  01 |`Unallocated memory hint (treat as NOP)`
                             *
                             *   10 |1110xx|!=1111|------|  00 |`LDRSBT A8-457`
                             *   10 |1110xx|!=1111|------|  01 |`LDRSHT A8-465`
                             *
                             *   10 |1xx1xx|!=1111|------|  00 |`LDRSB (immediate) A8-451`
                             *   10 |1xx1xx|!=1111|------|  01 |`LDRSH (immediate) A8-459`
                             */
                            if rn == 0b1111 {
                                // (literal)
                                let opcode = if rt == 0b1111 {
                                    [
                                        Opcode::PLI,
                                        Opcode::NOP,
                                    ][size]
                                } else {
                                    [
                                        Opcode::LDRSB,
                                        Opcode::LDRSH,
                                    ][size]
                                };
                                let u = false; // instr[7] known false here
                                let imm12 = lower2[..12].load::<u16>();
                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rt)),
                                    Operand::RegDerefPreindexOffset(Reg::from_u8(15), imm12, u, false), // add, no wback
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                if op2 == 0b000000 {
                                    let opcode = if rt == 0b1111 {
                                        [
                                            Opcode::PLI,
                                            Opcode::NOP,
                                        ][size]
                                    } else {
                                        [
                                            Opcode::LDRSB,  // encoding T2
                                            Opcode::LDRSH,  // encoding T2
                                        ][size]
                                    };
                                    let rm = lower2[0..4].load::<u8>();
                                    let imm2 = lower2[4..6].load::<u8>();
                                    inst.opcode = opcode;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        Operand::RegDerefPreindexRegShift(
                                            Reg::from_u8(rn),
                                            RegShift::from_raw(
                                                0b00000 |    // `RegImm`
                                                rm as u16 |
                                                ((0 /* lsl */) << 5)|
                                                ((imm2 as u16) << 7)
                                            ),
                                            true,   // add
                                            false,  // wback
                                        ),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else if op2 & 0b111100 == 0b110000 {
                                    let opcode = if rt == 0b1111 {
                                        [
                                            Opcode::PLI,
                                            Opcode::NOP,
                                        ][size]
                                    } else {
                                        [
                                            Opcode::LDRSB,  // encoding T2
                                            Opcode::LDRSH,  // encoding T2
                                        ][size]
                                    };
                                    let w = lower2[8];
                                    let u = lower2[9];
                                    let p = lower2[10];
                                    let imm8 = lower2[..8].load::<u16>();
                                    inst.opcode = opcode;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        if p {
                                            Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8, u, w)
                                        } else {
                                            Operand::RegDerefPostindexOffset(Reg::from_u8(rn), imm8, u, false)
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else if op2 & 0b111100 == 0b111000 {
                                    let opcode = [
                                        Opcode::LDRSBT, // encoding T1
                                        Opcode::LDRSHT, // encoding T1
                                    ][size];
                                    let imm8 = lower2[..8].load::<u16>();
                                    inst.opcode = opcode;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8, true, false), // add, no wback
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else if op2 & 0b100100 == 0b100100 {
                                    let opcode = [
                                        Opcode::LDRSB,  // encoding T2
                                        Opcode::LDRSH,  // encoding T2
                                    ][size];
                                    let w = lower2[8];
                                    let u = lower2[9];
                                    let p = lower2[10];
                                    let imm8 = lower2[..8].load::<u16>();
                                    inst.opcode = opcode;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rt)),
                                        if p {
                                            Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm8, u, w)
                                        } else {
                                            Operand::RegDerefPostindexOffset(Reg::from_u8(rn), imm8, u, false)
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else {
                                    // op2 =~ 0b1010xx or something?
                                    // nothing to try decoding as for a `decoder.undefined()?`, so
                                    // just error.
                                    return Err(ErrorKind::Undefined);
                                }
                            }
                        } else {
                            // op1 == bits 7:8
                            if size == 0b10 {
                                return Err(ErrorKind::Undefined);
                            }
                            // op1 == 0b11
                            /*
                             *   1x |------|------|------|  10 |`UNDEFINED (cite: A6.3.7)`
                             *   1x |------|==1111|!=1111|  00 |`LDRSB (literal) A8-453`
                             *   1x |------|==1111|==1111|  00 |`PLI (immediate, literal) A8-531`
                             *   1x |------|==1111|!=1111|  01 |`LDRSH (literal) A8-461`
                             *   1x |------|==1111|==1111|  01 |`Unallocated memory hint (treat as NOP)`
                             *
                             *   11 |------|!=1111|!=1111|  00 |`LDRSB (immediate) A8-451`
                             *   11 |------|!=1111|==1111|  00 |`PLI (immediate, literal) A8-531`
                             *   11 |------|!=1111|!=1111|  01 |`LDRSH (immediate) A8-459`
                             *   11 |------|!=1111|==1111|  01 |`Unallocated memory hint (treat as NOP)`
                             */
                            if rn == 0b1111 {
                                // (literal)
                                let opcode = if rt == 0b1111 {
                                    [
                                        Opcode::PLI,
                                        Opcode::NOP,
                                    ][size]
                                } else {
                                    [
                                        Opcode::LDRSB,
                                        Opcode::LDRSH,
                                    ][size]
                                };
                                let u = true; // instr[7] known true here
                                let imm12 = lower2[..12].load::<u16>();
                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rt)),
                                    Operand::RegDerefPreindexOffset(Reg::from_u8(15), imm12, u, false), // add, no wback
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                // (immediate)
                                let opcode = if rt == 0b1111 {
                                    [
                                        Opcode::PLI,
                                        Opcode::NOP,
                                    ][size]
                                } else {
                                    [
                                        Opcode::LDRSB,  // encoding T1
                                        Opcode::LDRSH,  // encoding T1
                                    ][size]
                                };
                                let imm12 = lower2[..12].load::<u16>();
                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rt)),
                                    Operand::RegDerefPreindexOffset(Reg::from_u8(rn), imm12, true, false), // add, no wback
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                        }
                    }
                } else {
                    if !op2[4] {
                        // `Data-processing (register)` (`A6-243`)
                        let op1 = &instr2[4..8];
                        let op2 = &lower2[4..8];
                        let rn = instr2[0..4].load::<u8>();
                        if !op1[3] {
                            // `LSL`, `LSR`, `ASR`, `ROR`, `SXTAH`, .... out of table `A6-24`
                            if !op2[3] {
                                // `LSL`, `LSR`, `ASR`, `ROR`
                                // v6T2
                                let op = [
                                    Opcode::LSL,
                                    Opcode::LSR,
                                    Opcode::ASR,
                                    Opcode::ROR,
                                ][op2[1..3].load::<usize>()];
                                let rd = lower2[8..12].load::<u8>();
                                let rm = lower2[0..4].load::<u8>();
                                inst.opcode = op;
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Reg(Reg::from_u8(rm)),
                                    Operand::Nothing,
                                ];
                            } else {
                                let op1 = op1[0..3].load::<usize>();
                                // `SXTAH` and friends
                                if op1 > 0b101 {
                                    return Err(ErrorKind::Undefined);
                                }

                                if rn == 0b1111 {
                                    let op = [
                                        Opcode::SXTH,
                                        Opcode::UXTH,
                                        Opcode::SXTB16,
                                        Opcode::UXTB16,
                                        Opcode::SXTB,
                                        Opcode::UXTB,
                                    ][op1];

                                    let rm = lower2[..4].load::<u8>();
                                    let rotate = lower2[1..3].load::<u8>() << 2;
                                    let rd = lower2[8..12].load::<u8>();

                                    inst.opcode = op;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::Reg(Reg::from_u8(rm)),
                                        Operand::Imm32(rotate as u32),
                                        Operand::Nothing,
                                    ];
                                } else {
                                    let op = [
                                        Opcode::SXTAH,
                                        Opcode::UXTAH,
                                        Opcode::SXTAB16,
                                        Opcode::UXTAB16,
                                        Opcode::SXTAH,
                                        Opcode::UXTAB,
                                    ][op1];

                                    let rm = lower2[..4].load::<u8>();
                                    let rotate = lower2[1..3].load::<u8>() << 2;
                                    let rd = lower2[8..12].load::<u8>();

                                    inst.opcode = op;
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::Reg(Reg::from_u8(rn)),
                                        Operand::Reg(Reg::from_u8(rm)),
                                        Operand::Imm32(rotate as u32),
                                    ];
                                };
                            }
                        } else {
                            let op2 = op2.load::<u8>();
                            if op2 < 0b0100 {
                                // `Parallel addition and subtraction, signed`
                                let op1 = instr2[4..7].load::<usize>();
                                let op2 = lower2[4..6].load::<usize>();
                                if op1 == 0 || op1 > 0b100 || op2 == 0b11 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                let opcode_idx = (op1 - 1) * 3 + op2;

                                let rn = instr2[0..4].load::<u8>();
                                let rd = lower2[8..12].load::<u8>();
                                let rm = lower2[0..4].load::<u8>();

                                inst.opcode = [
                                    Opcode::SADD16,
                                    Opcode::QADD16,
                                    Opcode::SHADD16,
                                    Opcode::SASX,
                                    Opcode::QASX,
                                    Opcode::SHASX,
                                    Opcode::SSAX,
                                    Opcode::QSAX,
                                    Opcode::SHSAX,
                                    Opcode::SSUB16,
                                    Opcode::QSUB16,
                                    Opcode::SHSUB16,
                                    Opcode::SADD8,
                                    Opcode::QADD8,
                                    Opcode::SHADD8,
                                    Opcode::SSUB8,
                                    Opcode::QSUB8,
                                    Opcode::SHSUB8,
                                ][opcode_idx];
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Reg(Reg::from_u8(rm)),
                                    Operand::Nothing,
                                ];
                            } else if op2 < 0b1000 {
                                // `Parallel addition and subtraction, unsigned` (`A6-244`)
                                let op1 = instr2[4..7].load::<usize>();
                                let op2 = lower2[4..6].load::<usize>();
                                if op1 > 0b100 || op2 == 0b11 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                if op1 == 0 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                                let opcode_idx = (op1 - 1) * 3 + op2;

                                let rn = instr2[0..4].load::<u8>();
                                let rd = lower2[8..12].load::<u8>();
                                let rm = lower2[0..4].load::<u8>();

                                inst.opcode = [
                                    Opcode::UADD16,
                                    Opcode::UQADD16,
                                    Opcode::UHADD16,
                                    Opcode::UASX,
                                    Opcode::UQASX,
                                    Opcode::UHASX,
                                    Opcode::USAX,
                                    Opcode::UQSAX,
                                    Opcode::UHSAX,
                                    Opcode::USUB16,
                                    Opcode::UQSUB16,
                                    Opcode::UHSUB16,
                                    Opcode::UADD8,
                                    Opcode::UQADD8,
                                    Opcode::UHADD8,
                                    Opcode::USUB8,
                                    Opcode::UQSUB8,
                                    Opcode::UHSUB8,
                                ][opcode_idx];
                                inst.operands = [
                                    Operand::Reg(Reg::from_u8(rd)),
                                    Operand::Reg(Reg::from_u8(rn)),
                                    Operand::Reg(Reg::from_u8(rm)),
                                    Operand::Nothing,
                                ];
                            } else if op2 < 0b1100 {
                                // `Miscellaneous operations` (`A6-246`)
                                let rn = instr2[0..4].load::<u8>();
                                let rd = lower2[8..12].load::<u8>();
                                let rm = lower2[0..4].load::<u8>();
                                let op1 = instr2[4..6].load::<u8>();
                                let op2 = lower2[4..6].load::<usize>();
                                match op1 {
                                    0b00 => {
                                        inst.opcode = [
                                            Opcode::QADD,
                                            Opcode::QDADD,
                                            Opcode::QSUB,
                                            Opcode::QDSUB,
                                        ][op2];
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b01 => {
                                        if rn != rm {
                                            decoder.unpredictable()?;
                                        }
                                        inst.opcode = [
                                            Opcode::REV,
                                            Opcode::REV16,
                                            Opcode::RBIT,
                                            Opcode::REVSH,
                                        ][op2];
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b10 => {
                                        if op2 != 0 {
                                            return Err(ErrorKind::InvalidOpcode);
                                        }
                                        inst.opcode = Opcode::SEL;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                            Operand::Nothing,
                                        ];
                                    }
                                    0b11 => {
                                        if op2 != 0 {
                                            return Err(ErrorKind::InvalidOpcode);
                                        }
                                        inst.opcode = Opcode::CLZ;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                    _ => {
                                        unreachable!("impossible bit pattern for op1");
                                    }
                                }
                            } else {
                                return Err(ErrorKind::Undefined);
                            }
                        }
                    } else {
                        if op2[3] {
                            // `Long multiply, long multiply accumulate, and divide` (`A6-248`)
                            let op1 = instr2[4..7].load::<usize>();
                            let op2 = lower2[4..8].load::<usize>();

                            let rn = instr2[0..4].load::<u8>();
                            let rdlo = lower2[12..16].load::<u8>();
                            let rd = lower2[8..12].load::<u8>();
                            let rm = lower2[0..4].load::<u8>();

                            match op1 {
                                0b000 => {
                                    if op2 == 0b0000 {
                                        inst.opcode = Opcode::SMULL;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rdlo)),
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                        ];
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                                0b001 => {
                                    if op2 == 0b1111 {
                                        inst.opcode = Opcode::SDIV;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                                0b010 => {
                                    if op2 == 0b0000 {
                                        inst.opcode = Opcode::UMULL;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rdlo)),
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                        ];
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                                0b011 => {
                                    if op2 == 0b1111 {
                                        inst.opcode = Opcode::UDIV;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                                0b100 => {
                                    if op2 == 0b0000 {
                                        inst.opcode = Opcode::SMLAL;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rdlo)),
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                        ];
                                    } else if op2 & 0b1100 == 0b1000 {
                                        inst.opcode = Opcode::SMLAL_halfword(lower2[5], lower2[4]);
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rdlo)),
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                        ];
                                    } else if op2 & 0b1110 == 0b1100 {
                                        inst.opcode = Opcode::SMLALD(lower2[4]);
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rdlo)),
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                        ];
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                                0b101 => {
                                    if op2 == 0b1100 || op2 == 0b1101 {
                                        inst.opcode = Opcode::SMLSLD(lower2[4]);
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rdlo)),
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                        ];
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                                0b110 => {
                                    if op2 == 0b0000 {
                                        inst.opcode = Opcode::UMLAL;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rdlo)),
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                        ];
                                    } else if op2 == 0b0110 {
                                        inst.opcode = Opcode::UMAAL;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rdlo)),
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                        ];
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                                _ => {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            }
                        } else {
                            // `Multiply, multiply accumulate, and absolute difference` (`A6-247`)
                            let op1 = instr2[4..7].load::<usize>();
                            let op2 = lower2[4..6].load::<usize>();
                            let rm = lower2[0..4].load::<u8>();
                            let rd = lower2[8..12].load::<u8>();
                            let ra = lower2[12..16].load::<u8>();
                            let rn = instr2[0..4].load::<u8>();

                            if ra == 0b1111 {
                                if op1 == 0b000 {
                                    if op2 == 0b00 {
                                        inst.opcode = Opcode::MUL;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                            Operand::Nothing,
                                        ];
                                    } else if op2 == 0b01{
                                        inst.opcode = Opcode::MLS;
                                        // a == 15, unpredictable
                                        decoder.unpredictable()?;
                                        inst.operands = [
                                            Operand::Reg(Reg::from_u8(rd)),
                                            Operand::Reg(Reg::from_u8(rn)),
                                            Operand::Reg(Reg::from_u8(rm)),
                                            Operand::Reg(Reg::from_u8(ra)),
                                        ];
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }

                                } else if op1 == 0b001 {
                                    // `SMULBB, SMULBT, SMULTB, SMULTT on page A8-645`
                                    inst.opcode = Opcode::SMUL(lower2[5], lower2[4]);
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::Reg(Reg::from_u8(rn)),
                                        Operand::Reg(Reg::from_u8(rm)),
                                        Operand::Nothing,
                                    ];
                                } else if op1 == 0b011 {
                                    if op2 >= 0b10 {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                    inst.opcode = Opcode::SMULW(lower2[4]);
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::Reg(Reg::from_u8(rn)),
                                        Operand::Reg(Reg::from_u8(rm)),
                                        Operand::Nothing,
                                    ];
                                } else {
                                    if op2 >= 0b10 {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                    if op1 == 0b111 && op2 == 0b00 {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                    if op1 == 0b110 {
                                        decoder.unpredictable()?;
                                    }
                                    inst.opcode = [
                                        Opcode::MUL, // already handled
                                        Opcode::UDF, // already handled
                                        Opcode::SMUAD,
                                        Opcode::UDF, // already handled
                                        Opcode::SMUSD,
                                        Opcode::SMMUL,
                                        Opcode::SMMLS,
                                        Opcode::USAD8,
                                    ][op1];
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::Reg(Reg::from_u8(rn)),
                                        Operand::Reg(Reg::from_u8(rm)),
                                        Operand::Nothing,
                                    ];
                                }
                            } else {
                                if op1 == 0b000 {
                                    if op2 == 0b00 {
                                        inst.opcode = Opcode::MLA;
                                    } else if op2 == 0b01{
                                        inst.opcode = Opcode::MLS;
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }

                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::Reg(Reg::from_u8(rn)),
                                        Operand::Reg(Reg::from_u8(rm)),
                                        Operand::Reg(Reg::from_u8(ra)),
                                    ];
                                } else if op1 == 0b001 {
                                    // `SMULBB, SMULBT, SMULTB, SMULTT on page A8-645`
                                    inst.opcode = Opcode::SMLA(lower2[5], lower2[4]);
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::Reg(Reg::from_u8(rn)),
                                        Operand::Reg(Reg::from_u8(rm)),
                                        Operand::Reg(Reg::from_u8(ra)),
                                    ];
                                } else if op1 == 0b011 {
                                    if op2 >= 0b10 {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                    inst.opcode = Opcode::SMLAW(lower2[4]);
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::Reg(Reg::from_u8(rn)),
                                        Operand::Reg(Reg::from_u8(rm)),
                                        Operand::Reg(Reg::from_u8(ra)),
                                    ];
                                } else {
                                    if op2 >= 0b10 {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                    if op1 == 0b111 && op2 == 0b00 {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                    if op1 == 0b110 {
                                        decoder.unpredictable()?;
                                    }
                                    inst.opcode = [
                                        Opcode::MUL, // already handled
                                        Opcode::UDF, // already handled
                                        Opcode::SMLAD,
                                        Opcode::UDF, // already handled
                                        Opcode::SMLSD,
                                        Opcode::SMMLA,
                                        Opcode::SMMLS,
                                        Opcode::USADA8,
                                    ][op1];
                                    inst.operands = [
                                        Operand::Reg(Reg::from_u8(rd)),
                                        Operand::Reg(Reg::from_u8(rn)),
                                        Operand::Reg(Reg::from_u8(rm)),
                                        Operand::Reg(Reg::from_u8(ra)),
                                    ];
                                }

                            }
                        }
                    }
                }
            } else {
                // `Coprocessor, Advanced SIMD, and Floating-point instructions` (`A6-249`)
                // v6T2
                // op1 == 11, op2 == 1xxxxxx
                // this means `assert!(instr2[10])`
                return decode_table_a6_30(decoder, inst, instr2, lower2);
            }
        }
    } else {
        // 16b instruction - `A6-221, 16-bit Thumb instruction encoding`
        // `Table A6-1`
        if opword < 0b01000 {
            // `Shift (immediate), add, subtract, move, and compare` page `A6-222`
            // v4T
            let opcode = opword & 0b111;
            // TODO: `S` iff outside `IT` block
            inst.s = true;

            match opcode {
                0b000 => {
                    // LSL (immediate)
                    // footnote: when opcode is 0, bits 8:6 are 0, encoding is `MOV`. see `A8-487`.
                    let rd = instr2[0..3].load::<u8>();
                    let rm = instr2[3..6].load::<u8>();
                    let imm5 = instr2[6..11].load::<u16>();
                    inst.opcode = Opcode::LSL;
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rd)),
                        Operand::Reg(Reg::from_u8(rm)),
                        Operand::Imm12(imm5),
                        Operand::Nothing,
                    ];
                }
                0b001 => {
                    /* LSR on page A8-473 */
                    let rd = instr2[0..3].load::<u8>();
                    let rm = instr2[3..6].load::<u8>();
                    let imm5 = instr2[6..11].load::<u16>();
                    let imm = if imm5 == 0 {
                        0x20
                    } else {
                        imm5
                    };
                    inst.opcode = Opcode::LSR;
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rd)),
                        Operand::Reg(Reg::from_u8(rm)),
                        Operand::Imm12(imm),
                        Operand::Nothing,
                    ];
                }
                0b010 => {
                    /* ASR on page A8-328 */
                    let rd = instr2[0..3].load::<u8>();
                    let rm = instr2[3..6].load::<u8>();
                    let imm5 = instr2[6..11].load::<u16>();
                    let imm = if imm5 == 0 {
                        0x20
                    } else {
                        imm5
                    };
                    inst.opcode = Opcode::ASR;
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rd)),
                        Operand::Reg(Reg::from_u8(rm)),
                        Operand::Imm12(imm),
                        Operand::Nothing,
                    ];
                }
                0b011 => {
                    /* ADD, SUB (register/immediate) */
                    let oplower = instr2[9..11].load::<u32>();
                    let rm = instr2[6..9].load::<u8>();
                    let rd = instr2[0..3].load::<u8>();
                    let rn = instr2[3..6].load::<u8>();

                    match oplower {
                        0b00 => {
                            inst.opcode = Opcode::ADD;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                Operand::Reg(Reg::from_u8(rm)),
                                Operand::Nothing,
                            ];
                        }
                        0b01 => {
                            inst.opcode = Opcode::SUB;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                Operand::Reg(Reg::from_u8(rm)),
                                Operand::Nothing,
                            ];
                        }
                        0b10 => {
                            inst.opcode = Opcode::ADD;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                Operand::Imm32(rm as u32),
                                Operand::Nothing,
                            ];
                        }
                        0b11 => {
                            inst.opcode = Opcode::SUB;
                            inst.operands = [
                                Operand::Reg(Reg::from_u8(rd)),
                                Operand::Reg(Reg::from_u8(rn)),
                                Operand::Imm32(rm as u32),
                                Operand::Nothing,
                            ];
                        }
                        _ => {
                            unreachable!("impossible bit pattern");
                        }
                    }
                }
                0b100 => {
                    /* MOV on page A8-485 */
                    let imm8 = instr2[0..8].load::<u32>();
                    let rd = instr2[8..11].load::<u8>();
                    inst.opcode = Opcode::MOV;
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rd)),
                        Operand::Imm32(imm8 as u32),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                }
                0b101 => {
                    /* CMP on page A8-368 */
                    inst.s = false;
                    let imm8 = instr2[0..8].load::<u32>();
                    let rd = instr2[8..11].load::<u8>();
                    inst.opcode = Opcode::CMP;
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rd)),
                        Operand::Imm32(imm8 as u32),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                }
                0b110 => {
                    /* ADD (immediate, Thumb) on page A8-304 */
                    let imm8 = instr2[0..8].load::<u32>();
                    let rdn = instr2[8..11].load::<u8>();
                    inst.opcode = Opcode::ADD;
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rdn)),
                        Operand::Imm32(imm8 as u32),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                }
                0b111 => {
                    /* SUB (immediate, Thumb) on page A8-709 */
                    let imm8 = instr2[0..8].load::<u32>();
                    let rdn = instr2[8..11].load::<u8>();
                    inst.opcode = Opcode::SUB;
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rdn)),
                        Operand::Imm32(imm8 as u32),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                }
                _ => {
                    unreachable!("impossible bit pattern");
                }
            }
        } else if opword < 0b01001 {
            let opcode_bits = instr2[6..10].load::<u8>();
            // `Data-processing` on page `A6-223` or `Special data instructions and branch and
            // exchange` on page `A6-224`
            if (instr >> 10) < 0b010001 {
                // `Data-processing` on page `A6-223`
                // v4T
                // TODO: condition inside IT block, no S
                inst.s = true;
                let rdn = instr2[0..3].load::<u8>();
                let rm = instr2[3..6].load::<u8>();
                if opcode_bits == 0b1101 {
                    inst.opcode = Opcode::MUL;
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rdn)),
                        Operand::Reg(Reg::from_u8(rm)),
                        Operand::Reg(Reg::from_u8(rdn)),
                        Operand::Nothing,
                    ];
                } else if opcode_bits == 0b1001 {
                    inst.opcode = Opcode::RSB;
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rdn)),
                        Operand::Reg(Reg::from_u8(rm)),
                        Operand::Imm12(0),
                        Operand::Nothing,
                    ];
                } else {
                    let opcode = [
                        Opcode::AND,
                        Opcode::EOR,
                        Opcode::LSL,
                        Opcode::LSR,
                        Opcode::ASR,
                        Opcode::ADC,
                        Opcode::SBC,
                        Opcode::ROR,
                        Opcode::TST,
                        Opcode::RSB,
                        Opcode::CMP,
                        Opcode::CMN,
                        Opcode::ORR,
                        Opcode::MUL,
                        Opcode::BIC,
                        Opcode::MVN,
                    ][opcode_bits as usize];
                    inst.opcode = opcode;
                    if opcode_bits == 8 || opcode_bits == 10 || opcode_bits == 11 {
                        inst.s = false;
                    }
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rdn)),
                        Operand::Reg(Reg::from_u8(rm)),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                }
            } else {
                // `Special data instructions and branch and exchange` on page `A6-224`
                match opcode_bits {
                    0b0000 => {
                        // `Add Low Registers` (`A8-308`)
                        // v6T2, `UNPREDICTABLE` in earlier versions
                        let rdn = instr2[0..3].load::<u8>() | (instr2[7..8].load::<u8>() << 3);
                        let rm = instr2[3..7].load::<u8>();
                        inst.opcode = Opcode::ADD;
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(rdn)),
                            Operand::Reg(Reg::from_u8(rm)),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    },
                    0b0001 |
                    0b0010 |
                    0b0011 => {
                        // `Add High Registers` (`A8-308`)
                        // v4T
                        let rdn = instr2[0..3].load::<u8>() | (instr2[7..8].load::<u8>() << 3);
                        let rm = instr2[3..7].load::<u8>();
                        inst.opcode = Opcode::ADD;
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(rdn)),
                            Operand::Reg(Reg::from_u8(rm)),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    },
                    0b0100 |
                    0b0101 |
                    0b0110 |
                    0b0111 => {
                        // `Compare High Registers` (`A8-307`)
                        // v4T
                        let rn = instr2[0..3].load::<u8>() | (instr2[7..8].load::<u8>() << 3);
                        let rm = instr2[3..7].load::<u8>();
                        inst.opcode = Opcode::CMP;
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(rn)),
                            Operand::Reg(Reg::from_u8(rm)),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    }
                    0b1000 => {
                        // `Move Low Registers` (`A8-487`)
                        // v6, `UNPREDICTABLE` in earlier versions
                        // (encoding T1)
                        let rd = instr2[0..3].load::<u8>() | (instr2[7..8].load::<u8>() << 3);
                        let rm = instr2[3..7].load::<u8>();
                        inst.opcode = Opcode::MOV;
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(rd)),
                            Operand::Reg(Reg::from_u8(rm)),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    }
                    0b1001 |
                    0b1010 |
                    0b1011 => {
                        // `Move High Registers` (`A8-487`)
                        // v4T
                        let rd = instr2[0..3].load::<u8>() | (instr2[7..8].load::<u8>() << 3);
                        let rm = instr2[3..7].load::<u8>();
                        inst.opcode = Opcode::MOV;
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(rd)),
                            Operand::Reg(Reg::from_u8(rm)),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    },
                    0b1100 |
                    0b1101 => {
                        // `Branch and Exchange` (`A8-350`)
                        // v4T
                        let rm = instr2[3..7].load::<u8>();
                        inst.opcode = Opcode::BX;
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(rm)),
                            Operand::Nothing,
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    }
                    0b1110 |
                    0b1111 => {
                        // `Branch and Link with Exchange` (`A8-348`)
                        // v5T, `UNPREDICTABLE` in earlier versions
                        let rm = instr2[3..7].load::<u8>();
                        inst.opcode = Opcode::BLX;
                        inst.operands = [
                            Operand::Reg(Reg::from_u8(rm)),
                            Operand::Nothing,
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    }
                    _ => {
                        unreachable!("bad bit pattern");
                    }
                }
            }
        } else if opword < 0b01010 {
            // `LDR (literal)` on page `A8-411` -- v4T
            let imm8 = instr2[0..8].load::<u16>();
            let rt = instr2[8..11].load::<u8>();
            inst.opcode = Opcode::LDR;
            inst.operands = [
                Operand::Reg(Reg::from_u8(rt)),
                Operand::RegDerefPreindexOffset(
                    Reg::from_u8(0b1111),
                    imm8 << 2,
                    true,  // add
                    false, // no wback
                ),
                Operand::Nothing,
                Operand::Nothing,
            ];
        } else if opword < 0b10100 {
            let op_b = instr2[9..12].load::<usize>();
            let op_a = instr2[12..].load::<usize>();
            // `Load/store single data item` on page `A6-225`
            // v4T
            let rt = instr2[0..3].load::<u8>();
            let rn = instr2[3..6].load::<u8>();
            let rm = instr2[6..9].load::<u8>();
            let imm5 = instr2[6..11].load::<u16>();
            if op_a == 0b0101 {
                let op = [
                    Opcode::STR,
                    Opcode::STRH,
                    Opcode::STRB,
                    Opcode::LDRSB,
                    Opcode::LDR,
                    Opcode::LDRH,
                    Opcode::LDRB,
                    Opcode::LDRSH,
                ][op_b];
                inst.opcode = op;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rt)),
                    Operand::RegDerefPreindexReg(
                        Reg::from_u8(rn),
                        Reg::from_u8(rm),
                        true,   // add
                        false,  // wback
                    ),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else {
                // opword is 0b0110, 0b0111, 0b1000, or 0b1001. opb bit 2 can be used to form a
                // three-bit index and select an opcode. operands are shared. except the last two.
                // those are sp-relative.
                let upper = op_a - 0b0110;
                let idx = (upper << 1) | (op_b >> 2);
                let op = [
                    Opcode::STR,
                    Opcode::LDR,
                    Opcode::STRB,
                    Opcode::LDRB,
                    Opcode::STRH,
                    Opcode::LDRH,
                    Opcode::STR,
                    Opcode::LDR,
                ][idx];
                inst.opcode = op;
                if idx < 6 {
                    let shift = match idx >> 1 {
                        0b00 => 2,
                        0b01 => 0,
                        0b10 => 1,
                        _ => { unreachable!("impossible bit pattern"); }
                    };
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rt)),
                        Operand::RegDerefPreindexOffset(
                            Reg::from_u8(rn),
                            imm5 << shift,
                            true,   // add
                            false,  // wback
                        ),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                } else {
                    let rt = instr2[8..11].load::<u8>();
                    let imm8 = instr2[..8].load::<u16>();
                    inst.operands = [
                        Operand::Reg(Reg::from_u8(rt)),
                        Operand::RegDerefPreindexOffset(
                            Reg::from_u8(13), // sp
                            imm8 << 2,
                            true,   // add
                            false,  // wback
                        ),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                }
            }
        } else if opword < 0b10101 {
            // `ADR` on page `A8-320` -- v4T
            let rd = instr2[8..11].load::<u8>();
            let imm8 = instr2[..8].load::<u16>();
            inst.opcode = Opcode::ADR;
            inst.operands = [
                Operand::Reg(Reg::from_u8(rd)),
                Operand::Imm32(imm8 as u32 * 4),
                Operand::Nothing,
                Operand::Nothing,
            ];
        } else if opword < 0b10110 {
            // `ADD (SP plus immediate)` on `A8-314` -- v4T
            let rd = instr2[8..11].load::<u8>();
            let imm8 = instr2[..8].load::<u16>();
            inst.opcode = Opcode::ADD;
            inst.operands = [
                Operand::Reg(Reg::from_u8(rd)),
                Operand::Reg(Reg::from_u8(13)), // sp
                Operand::Imm32(imm8 as u32 * 4),
                Operand::Nothing,
            ];
        } else if opword < 0b11000 {
            // `Miscellaneous 16-bit instructions` on page `A6-226`
            let opcode_bits = instr2[5..12].load::<u16>();
            if opcode_bits < 0b0000100 {
                // `Add Immediate to SP` (`A8-314`)
                // v4T
                // encoding T2
                let imm7 = instr2[..7].load::<u32>();
                inst.s = false;
                inst.opcode = Opcode::ADD;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(13)),
                    Operand::Reg(Reg::from_u8(13)),
                    Operand::Imm32(imm7 << 2),
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b0001000 {
                // `Subtract Immediate to SP` (`A8-717`)
                // v4T
                let imm7 = instr2[..7].load::<u32>();
                inst.s = false;
                inst.opcode = Opcode::SUB;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(13)),
                    Operand::Reg(Reg::from_u8(13)),
                    Operand::Imm32(imm7 << 2),
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b0010000 {
                // `Compare and Branch on Zero` (`A8-354`)
                // v6T2
                let op = instr2[11];
                let rn = instr2[..3].load::<u8>();
                let imm5 = instr2[3..8].load::<u16>();
                let imm = (((instr >> 9) & 1) << 5) | imm5;
                inst.opcode = if op {
                    Opcode::CBNZ
                } else {
                    Opcode::CBZ
                };
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rn)),
                    Operand::BranchThumbOffset(imm as i32 + 1),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b0010010 {
                // `Signed Extend Halfword` (`A8-735`)
                // v6
                let rd = instr2[..3].load::<u8>();
                let rm = instr2[3..6].load::<u8>();
                inst.opcode = Opcode::SXTH;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rd)),
                    Operand::Reg(Reg::from_u8(rm)),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b0010100 {
                // `Signed Extend Byte` (`A8-731`)
                // v6
                let rd = instr2[..3].load::<u8>();
                let rm = instr2[3..6].load::<u8>();
                inst.opcode = Opcode::SXTB;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rd)),
                    Operand::Reg(Reg::from_u8(rm)),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b0010110 {
                // `Unsigned Extend Halfword` (`A8-817`)
                // v6
                let rd = instr2[..3].load::<u8>();
                let rm = instr2[3..6].load::<u8>();
                inst.opcode = Opcode::UXTH;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rd)),
                    Operand::Reg(Reg::from_u8(rm)),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b0011000 {
                // `Unsigned Extend Byte` (`A8-813`)
                // v6
                let rd = instr2[..3].load::<u8>();
                let rm = instr2[3..6].load::<u8>();
                inst.opcode = Opcode::UXTB;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rd)),
                    Operand::Reg(Reg::from_u8(rm)),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b0100000 {
                // `Compare and Branch on Zero` (`A8-354`)
                // v6T2
                let op = instr2[11];
                let rn = instr2[..3].load::<u8>();
                let imm5 = instr2[3..8].load::<u16>();
                let imm = (((instr >> 9) & 1) << 5) | imm5;
                inst.opcode = if op {
                    Opcode::CBNZ
                } else {
                    Opcode::CBZ
                };
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rn)),
                    Operand::BranchThumbOffset(imm as i32 + 1),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b0110000 {
                // `Push multiple registers` (`A8-539`)
                // v4T
                let m = instr2[8..9].load::<u16>();
                let reglist = instr2[0..8].load::<u16>() | (m << (6 + 8));
                inst.opcode = Opcode::PUSH;
                inst.operands = [
                    Operand::RegList(reglist),
                    Operand::Nothing,
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b0110010 {
                // undefined encoding between `PUSH` and `SETEND`
                return Err(ErrorKind::Undefined);
            } else if opcode_bits < 0b0110011 {
                // opword == 0b0110010
                // `Set Endianness` (`A8-605`)
                // v6
                let e = instr2[3..4].load::<u16>();
                inst.opcode = Opcode::SETEND;
                inst.operands = [
                    Operand::Imm12(e),
                    Operand::Nothing,
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b0110100 {
                // opword == 0b0110011
                // `Change Processor State` (`B9-1964`)
                // v6
                let aif = instr2[0..3].load::<u16>();
                let im = instr2[4];
                inst.opcode = Opcode::CPS(im);
                inst.operands = [
                    Operand::Imm12(aif),
                    Operand::Nothing,
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b1001000 {
                // undefined encoding between `CPS` and `CBNZ/CBZ`
                return Err(ErrorKind::Undefined);
            } else if opcode_bits < 0b1010000 {
                // `Compare and Branch on Nonzero` (`A8-354`)
                // v6T2
                let op = instr2[11];
                let rn = instr2[0..3].load::<u8>();
                let imm5 = instr2[3..8].load::<u16>();
                let imm = (((instr >> 9) & 1) << 5) | imm5;
                inst.opcode = if op {
                    Opcode::CBNZ
                } else {
                    Opcode::CBZ
                };
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rn)),
                    Operand::BranchThumbOffset(imm as i32 + 1),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b1010010 {
                // `Byte-Reverse Word` (`A8-563`)
                // v6
                let rd = instr2[0..3].load::<u8>();
                let rm = instr2[3..6].load::<u8>();
                inst.opcode = Opcode::REV;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rd)),
                    Operand::Reg(Reg::from_u8(rm)),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b1010100 {
                // `Byte-Reverse Packed Halfword` (`A8-565`)
                // v6
                let rd = instr2[0..3].load::<u8>();
                let rm = instr2[3..6].load::<u8>();
                inst.opcode = Opcode::REV16;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rd)),
                    Operand::Reg(Reg::from_u8(rm)),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b1010110 {
                // undefined encoding where `Byte-Reverse Signed Word` might go
                return Err(ErrorKind::Undefined);
            } else if opcode_bits < 0b1011000 {
                // `Byte-Reverse Signed Halfword` (`A8-567`)
                // v6
                let rd = instr2[0..3].load::<u8>();
                let rm = instr2[3..6].load::<u8>();
                inst.opcode = Opcode::REVSH;
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rd)),
                    Operand::Reg(Reg::from_u8(rm)),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b1100000 {
                // `Compare and Branch on Nonzero` (`A8-354`)
                // v6T2
                let op = instr2[11];
                let rn = instr2[0..3].load::<u8>();
                let imm5 = instr2[3..8].load::<u16>();
                let imm = (((instr >> 9) & 1) << 5) | imm5;
                inst.opcode = if op {
                    Opcode::CBNZ
                } else {
                    Opcode::CBZ
                };
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rn)),
                    Operand::BranchThumbOffset(imm as i32 + 1),
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b1110000 {
                // `Pop Multiple Registers` (`A8-535`)
                // v4T
                let p = instr2[8..9].load::<u16>();
                let reglist = instr2[0..8].load::<u16>() | (p << (7 + 8));
                inst.opcode = Opcode::POP;
                inst.operands = [
                    Operand::RegList(reglist),
                    Operand::Nothing,
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode_bits < 0b1111000 {
                // `Breakpoint` (`A8-344`)
                // v5
                let imm8 = instr2[0..8].load::<u32>();
                inst.opcode = Opcode::BKPT;
                inst.operands = [
                    Operand::Imm32(imm8),
                    Operand::Nothing,
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else {
                // `If-Then, and hints` (`A6-227`)
                let opb = instr2[0..4].load::<u32>();
                let opa = instr2[4..8].load::<u32>();

                if opb != 0 {
                    // `IT` (`A8-391`)
                    // v6T2
                    let firstcond = opa;
                    let mask = opb;
                    inst.opcode = Opcode::IT;
                    if firstcond == 0b1111 {
                        return Err(ErrorKind::InvalidOperand);
                    }
                    inst.operands = [
                        Operand::Imm32(firstcond),
                        Operand::Imm32(mask),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                } else {
                    match opa {
                        0b0000 => {
                            // `NOP` (`A8-511`)
                            // v6T2
                            inst.opcode = Opcode::NOP;
                            inst.operands = [
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0001 => {
                            // `YIELD` (`A8-1109`)
                            // v7
                            inst.opcode = Opcode::YIELD;
                            inst.operands = [
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0010 => {
                            // `WFE` (`A8-1105`)
                            // v7
                            inst.opcode = Opcode::WFE;
                            inst.operands = [
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0011 => {
                            // `WFI` (`A8-1107`)
                            // v7
                            inst.opcode = Opcode::WFI;
                            inst.operands = [
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0100 => {
                            // `SEV` (`A8-607`)
                            // v7
                            inst.opcode = Opcode::SEV;
                            inst.operands = [
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        hint => {
                            // `Other encodings in this space are unallocated hints. They execute
                            // as NOPs, but software must not use them.`
                            inst.opcode = Opcode::HINT;
                            inst.operands = [
                                Operand::Imm32(hint),
                                Operand::Nothing,
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                    }
                }
            }
        } else if opword < 0b11001 {
            // `STM (STMIA, STMEA)` on page `A8-665` -- v4T
            let rn = instr2[8..11].load::<u8>();
            let reglist = instr2[0..8].load::<u16>();
            inst.opcode = Opcode::STM(true, true, false, true); // stmia, no wback, yes usermode
            inst.operands = [
                Operand::RegWBack(Reg::from_u8(rn), true), // always wback
                Operand::RegList(reglist as u16),
                Operand::Nothing,
                Operand::Nothing,
            ];
        } else if opword < 0b11010 {
            // `LDM/LDMIA/LDMFD (Thumb)` on page `A8-397` -- v4T
            let rn = instr2[8..11].load::<u8>();
            let reglist = instr2[0..8].load::<u16>();
            let w = (reglist & (1 << rn)) == 0;
            inst.opcode = Opcode::LDM(true, false, false, true); // ldmia, no wback, yes usermode
            inst.operands = [
                Operand::RegWBack(Reg::from_u8(rn), w),
                Operand::RegList(reglist as u16),
                Operand::Nothing,
                Operand::Nothing,
            ];
        } else if opword < 0b11100 {
            // `Conditional branch, and Supervisor Call` on page `A6-227`
            let opcode = instr2[8..12].load::<u8>();
            if opcode < 0b1110 {
                // `B` (`A8-332`)
                // v4T
                inst.opcode = Opcode::B;
                let imm = instr2[0..8].load::<u8>() as i8 as i32;
                inst.condition = ConditionCode::build(opcode);
                inst.operands = [
                    Operand::BranchThumbOffset(imm + 1),
                    Operand::Nothing,
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else if opcode < 0b1111 {
                // `UDF` (`A8-759`)
                // v4T
                // first described in revision `C.a`
                inst.opcode = Opcode::UDF;
                inst.operands = [
                    Operand::Imm32(instr2[..8].load::<u32>()),
                    Operand::Nothing,
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            } else {
                // `SVC` (`A8-721`)
                // v4T
                inst.opcode = Opcode::SVC;
                inst.operands = [
                    Operand::Imm32(instr2[..8].load::<u32>()),
                    Operand::Nothing,
                    Operand::Nothing,
                    Operand::Nothing,
                ];
            }
        } else {
            // `B` on page `A8-332` -- v4T
            // encoding T2
            // v4T
            inst.opcode = Opcode::B;
            let imm = instr2[0..11].load::<u32>();
            let imm = ((imm as i32) << 21) >> 21;
            inst.operands = [
                Operand::BranchThumbOffset(imm),
                Operand::Nothing,
                Operand::Nothing,
                Operand::Nothing,
            ];
        }
    }
    Ok(())
}

fn decode_table_a6_30(decoder: &Decoder, inst: &mut Instruction, instr2: BitArray<Lsb0, [u16; 1]>, lower2: BitArray<Lsb0, [u16; 1]>) -> Result<(), ErrorKind> {
    // implementation of table `A6-30 Coprocessor, Advanced SIMD, and Floating-point instructions`
    let op1 = instr2[4..10].load::<usize>();
    if op1 & 0b11_1110 == 0b00_0000 {
        inst.opcode = Opcode::UDF;
        inst.operands = [
            Operand::Nothing,
            Operand::Nothing,
            Operand::Nothing,
            Operand::Nothing,
        ];
        return Err(ErrorKind::InvalidOpcode);
    } else if op1 & 0b110000 == 0b110000 {
        // TODO: `Advanced SIMD data-processing instructions on A7-259`
        return Err(ErrorKind::Incomplete);
    } else {
        let coproc = lower2[8..12].load::<u8>();
        if coproc & 0b1110 != 0b1010 {
            // `not 101x` rows
            if op1 == 0b000100 {
                // `MCRR, MCRR2 on page A8-479`
                let crm = lower2[0..4].load::<u8>();
                let opc1 = lower2[4..8].load::<u8>();
                let rt = lower2[12..16].load::<u8>();
                let rt2 = instr2[0..4].load::<u8>();
                if instr2[12] {
                    inst.opcode = Opcode::MCRR2(coproc, opc1);
                } else {
                    inst.opcode = Opcode::MCRR(coproc, opc1);
                }
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rt)),
                    Operand::Reg(Reg::from_u8(rt2)),
                    Operand::CReg(CReg::from_u8(crm)),
                    Operand::Nothing,
                ];
            } else if op1 == 0b000101 {
                // `MRRC, MRRC2 on page A8-495`
                let crm = lower2[0..4].load::<u8>();
                let opc1 = lower2[4..8].load::<u8>();
                let rt = lower2[12..16].load::<u8>();
                let rt2 = instr2[0..4].load::<u8>();
                // manual typo!! the manual spells the operands
                // `<coproc>, <opc>, <Rt>, <Rt2>, <CRm>`
                // but the operand name is `opc1`!
                //
                // this is a very uninteresting typo, but fun to spot nonetheless
                if instr2[12] {
                    inst.opcode = Opcode::MRRC2(coproc, opc1);
                } else {
                    inst.opcode = Opcode::MRRC(coproc, opc1);
                }
                inst.operands = [
                    Operand::Reg(Reg::from_u8(rt)),
                    Operand::Reg(Reg::from_u8(rt2)),
                    Operand::CReg(CReg::from_u8(crm)),
                    Operand::Nothing,
                ];
            } else {
                if op1 & 1 == 0 {
                    // `STC, STC2 on page A8-663`
                    let p = instr2[8];
                    let u = instr2[7];
                    let w = instr2[5];
                    let rn = instr2[0..4].load::<u8>();
                    let crd = lower2[12..16].load::<u8>();
                    let imm8 = lower2[0..8].load::<u16>();

                    if instr2[12] {
                        if instr2[6] {
                            inst.opcode = Opcode::STC2L(coproc);
                        } else {
                            inst.opcode = Opcode::STC2(coproc);
                        }
                    } else {
                        if instr2[6] {
                            inst.opcode = Opcode::STCL(coproc);
                        } else {
                            inst.opcode = Opcode::STC(coproc);
                        }
                    }
                    inst.operands = [
                        Operand::CReg(CReg::from_u8(crd)),
                        if p {
                            Operand::RegDerefPreindexOffset(
                                Reg::from_u8(rn),
                                imm8 << 2,
                                u,
                                w,
                            )
                        } else {
                            if w {
                                Operand::RegDerefPostindexOffset(
                                    Reg::from_u8(rn),
                                    imm8 << 2,
                                    u,
                                    false, // TODO: wback? this is true? not true?
                                )
                            } else {
                                Operand::RegDeref(Reg::from_u8(rn))
                            }
                        },
                        if !p && !w {
                            Operand::CoprocOption(imm8 as u8)
                        } else {
                            Operand::Nothing
                        },
                        Operand::Nothing,
                    ];
                } else {
                    // `LDC, LDC2 (immediate or literal) on A8-393 or A8-395`
                    let p = instr2[8];
                    let u = instr2[7];
                    let w = instr2[5];
                    let rn = instr2[0..4].load::<u8>();
                    let crd = lower2[12..16].load::<u8>();
                    let imm8 = lower2[0..8].load::<u16>();

                    if rn == 0b1111 {
                        // `LDC, LDC2 (literal) on A8-395`
                        // notable for rejecting writeback
                        if w {
                            decoder.unpredictable()?;
                        }
                    } else {
                        // `LDC, LDC2 (immediate) on A8-393`
                    }

                    if instr2[12] {
                        if instr2[6] {
                            inst.opcode = Opcode::LDC2L(coproc);
                        } else {
                            inst.opcode = Opcode::LDC2(coproc);
                        }
                    } else {
                        if instr2[6] {
                            inst.opcode = Opcode::LDCL(coproc);
                        } else {
                            inst.opcode = Opcode::LDC(coproc);
                        }
                    }
                    inst.operands = [
                        Operand::CReg(CReg::from_u8(crd)),
                        if p {
                            Operand::RegDerefPreindexOffset(
                                Reg::from_u8(rn),
                                imm8 << 2,
                                u,
                                w,
                            )
                        } else {
                            if w {
                                Operand::RegDerefPostindexOffset(
                                    Reg::from_u8(rn),
                                    imm8 << 2,
                                    u,
                                    false, // TODO: wback? this is true? not true?
                                )
                            } else {
                                Operand::RegDeref(Reg::from_u8(rn))
                            }
                        },
                        if !p && !w {
                            Operand::CoprocOption(imm8 as u8)
                        } else {
                            Operand::Nothing
                        },
                        Operand::Nothing,
                    ];
                }
            }
        } else {
            // `101x` rows
            return Err(ErrorKind::Incomplete);
        }
    }
    Ok(())
}
