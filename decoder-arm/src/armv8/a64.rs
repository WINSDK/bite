//#[cfg(feature="use-serde")]
//use serde::{Serialize, Deserialize};
//
// many variables, among other things, will use the manual's spelling (e.g. fields named `Rm`
// rather than `rm` or `reg_m`). rust doesn't like this, but it's how we're gonna be.
#![allow(non_snake_case)]

use core::fmt::{self, Display, Formatter};

use decoder::{Decodable, Decoded, Error, ErrorKind, Reader, ToTokens};
use debugvault::Index;
use tokenizing::{colors, ColorScheme, Colors, TokenStream};

#[allow(non_snake_case)]
mod docs {
    use crate::armv8::a64::ErrorKind;

    #[test]
    fn test_ones() {
        assert_eq!(Ones(0), 0x00);
        assert_eq!(Ones(1), 0x01);
        assert_eq!(Ones(2), 0x03);
        assert_eq!(Ones(3), 0x07);
        assert_eq!(Ones(4), 0x0f);
        assert_eq!(Ones(5), 0x1f);
        assert_eq!(Ones(6), 0x3f);
        assert_eq!(Ones(7), 0x7f);
        assert_eq!(Ones(8), 0xff);
    }

    fn Ones(len: u8) -> u64 {
        assert!(len <= 64);

        if len == 0 {
            return 0;
        }
        if len == 64 {
            return 0xffffffff_ffffffffu64;
        }

        let mask = ((0x8000_0000_0000_0000u64 as i64) >> ((64 - 1) - len)) as u64;
        !mask
    }

    #[test]
    fn test_highest_set_bit() {
        assert_eq!(HighestSetBit(1, 0x11), 0);
        assert_eq!(HighestSetBit(5, 0x11), 4);
        assert_eq!(HighestSetBit(8, 0x08), 3);
    }

    #[test]
    fn test_vfpexpandimm() {
        assert_eq!(VFPExpandImm(0x34), 20.0f64);
        assert_eq!(VFPExpandImm(0x70), 1f64);
        assert_eq!(VFPExpandImm(0xdc), -0.4375f64);
        assert_eq!(VFPExpandImm(0xe0), -0.5f64);
        assert_eq!(VFPExpandImm(0xf0), -1f64);
    }

    fn HighestSetBit(N: u8, bits: u64) -> u8 {
        let mut probe = 1u64 << (N - 1);
        let mut i = N - 1;
        loop {
            if bits & probe != 0 {
                return i;
            }

            if i == 0 {
                break;
            } else {
                probe = probe >> 1;
                i -= 1;
            }
        }

        return 0xff;
    }

    fn Replicate(bitsM: u64, M_size: u8, N: u8) -> u64 {
        let count = N / M_size;
        let mut res = bitsM;
        for i in 1..count {
            res |= bitsM << M_size * i;
        }
        // since this produces a u64, we might have a few extra non-zero bits set.
        let res_mask = Ones(N);
        res & res_mask
    }

    fn ROR(bits: u64, bitsN: u8, shift: u8) -> u64 {
        if shift == 0 {
            bits
        } else {
            let m = shift % bitsN;
            (bits >> m) | (bits << (bitsN - m))
        }
    }

    // helper functions from the ARMv8 Architecture Reference Manual
    pub fn DecodeBitMasks_32(immN: u8, imms: u8, immr: u8) -> Result<(u32, u32), ErrorKind> {
        // should the !imms be ~imms
        let len = HighestSetBit(7, ((immN << 6) | ((!imms) & 0x3f)) as u64);
        if len == 0xff {
            return Err(ErrorKind::InvalidOperand);
        }

        let levels = (Ones(len) & 0x3f) as u8; // should ZeroExtend to at least 6 bits, but this is u8.

        let S = imms & levels;
        let R = immr & levels;
        let diff = S.wrapping_sub(R);

        let esize = 1 << len;
        let d = diff & !(0xff << len);
        let welem = Ones(S + 1);
        let telem = Ones(d + 1);
        let wmask = Replicate(ROR(welem, esize, R), esize, 32) as u32;
        let tmask = Replicate(telem, esize, 32) as u32;
        Ok((wmask, tmask))
    }

    pub fn DecodeBitMasks_64(immN: u8, imms: u8, immr: u8) -> Result<(u64, u64), ErrorKind> {
        // should the !imms be ~imms
        let len = HighestSetBit(7, ((immN << 6) | ((!imms) & 0x3f)) as u64);
        if len == 0xff {
            return Err(ErrorKind::InvalidOperand);
        }

        let levels = (Ones(len) & 0x3f) as u8; // should ZeroExtend to at least 6 bits, but this is u8.

        let S = imms & levels;
        let R = immr & levels;
        let diff = S.wrapping_sub(R);

        let esize = 1 << len;
        let d = diff & !(0xff << len);
        let welem = Ones(S + 1);
        let telem = Ones(d + 1);
        let wmask = Replicate(ROR(welem, esize, R), esize, 64);
        let tmask = Replicate(telem, esize, 64);
        Ok((wmask, tmask))
    }

    pub fn DecodeShift(op: u8) -> super::ShiftStyle {
        assert!(op <= 0b11);
        [
            super::ShiftStyle::LSL,
            super::ShiftStyle::LSR,
            super::ShiftStyle::ASR,
            super::ShiftStyle::ROR,
        ][op as usize]
    }

    pub fn VFPExpandImm(imm8: u8) -> f64 {
        let sign = imm8 >> 7;
        let exp = imm8 >> 4;
        let exp = (exp as i16) << 13 >> 13;
        let exp = exp ^ 0b1_00_0000_0000;
        let frac = imm8 << 4;

        let bits = ((sign as u64) << 63)
            | (((exp as u64) & 0b1_11_1111_1111) << 52)
            | ((frac as u64) << 44);

        f64::from_bits(bits)
    }
}

/// a size marker for a register.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum SizeCode {
    /// an x-size (64-bits) register.
    X,
    /// a w-size (32-bits) register.
    W,
}

/// a size marker for a SIMD register, a full scalar size, or vector element size.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum SIMDSizeCode {
    /// a byte (8-bits) scalar or element.
    B,
    /// a halfword (16-bits) scalar or element.
    H,
    /// a single(?) word (32-bits) scalar or element.
    S,
    /// a dword (64-bits) scalar or element.
    D,
    /// a qword (128-bits) scalar or element.
    Q,
}

impl SIMDSizeCode {
    fn width(&self) -> u16 {
        match self {
            SIMDSizeCode::B => 1,
            SIMDSizeCode::H => 2,
            SIMDSizeCode::S => 4,
            SIMDSizeCode::D => 8,
            SIMDSizeCode::Q => 16,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            SIMDSizeCode::B => "b",
            SIMDSizeCode::H => "h",
            SIMDSizeCode::S => "s",
            SIMDSizeCode::D => "d",
            SIMDSizeCode::Q => "q",
        }
    }
}

/// an `aarch64` instruction.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(C)]
pub struct Instruction {
    /// the operation of the decoded instruction.
    pub opcode: Opcode,
    /// operands for the decoded instruction. operands are populated from index 0, to 1, 2, and 3.
    /// operands from the instruction are non-`Operand::Nothing`.
    pub operands: [Operand; 4],
}

impl Decoded for Instruction {
    #[inline]
    fn width(&self) -> usize {
        4
    }

    fn update_rel_addrs(&mut self, addr: usize) {
        for operand in self.operands.iter_mut() {
            match operand {
                Operand::PCOffset(offs) => {
                    let addr = (addr as u64).saturating_add_signed(*offs);
                    *operand = Operand::Imm64(addr);
                }
                // stack pointer
                // Operand::RegisterOrSP(_, 31) => {
                //     let addr = (addr as u64).saturating_add_signed(*offs);
                //     *operand = Operand::Imm64(addr);
                // }
                _ => {}
            }
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut stream = TokenStream::new();
        let symbols = Index::default();
        self.tokenize(&mut stream, &symbols);
        f.write_str(&stream.to_string())?;
        Ok(())
    }
}

impl ToTokens for Instruction {
    fn tokenize(&self, stream: &mut TokenStream, symbols: &Index) {
        match self.opcode {
            Opcode::ISB => {
                stream.push("isb", Colors::opcode());

                // the default/reserved/expected value for the immediate in `isb` is `0b1111`.
                if let Operand::Imm16(15) = self.operands[0] {
                    return;
                }
            }
            Opcode::SBC => {
                if let Operand::Register(_, 31) = self.operands[1] {
                    stream.push("ngc ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                } else {
                    stream.push("sbc", Colors::opcode());
                }
            }
            Opcode::MOVN => {
                let imm = if let Operand::ImmShift(imm, shift) = self.operands[1] {
                    !((imm as u64) << shift)
                } else {
                    unreachable!("movn operand 1 is always ImmShift");
                };
                let imm = if let Operand::Register(size, _) = self.operands[0] {
                    if size == SizeCode::W {
                        imm as u32 as u64
                    } else {
                        imm
                    }
                } else {
                    unreachable!("movn operand 0 is always Register");
                };
                stream.push("mov ", Colors::opcode());
                self.operands[0].tokenize(stream, symbols);
                stream.push(", #", Colors::expr());
                stream.push_owned(decoder::encode_uhex(imm), Colors::immediate());
                return;
            }
            Opcode::MOVZ => {
                let imm = if let Operand::ImmShift(imm, shift) = self.operands[1] {
                    (imm as u64) << shift
                } else {
                    unreachable!("movz operand is always ImmShift");
                };
                let imm = if let Operand::Register(size, _) = self.operands[0] {
                    if size == SizeCode::W {
                        imm as u32 as u64
                    } else {
                        imm
                    }
                } else {
                    unreachable!("movn operand 0 is always Register");
                };
                stream.push("mov ", Colors::opcode());
                self.operands[0].tokenize(stream, symbols);
                stream.push(", #", Colors::expr());
                stream.push_owned(decoder::encode_uhex(imm), Colors::immediate());
                return;
            }
            Opcode::ORR => {
                if let Operand::Register(_, 31) = self.operands[1] {
                    if let Operand::Immediate(0) = self.operands[2] {
                        stream.push("mov ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[1].tokenize(stream, symbols);
                        return;
                    } else if let Operand::RegShift(style, amt, size, r) = self.operands[2] {
                        if style == ShiftStyle::LSL && amt == 0 {
                            stream.push("mov ", Colors::opcode());
                            self.operands[0].tokenize(stream, symbols);
                            stream.push(", ", Colors::expr());
                            Operand::Register(size, r).tokenize(stream, symbols);
                            return;
                        }

                    } else {
                        stream.push("mov ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                } else if self.operands[1] == self.operands[2] {
                    stream.push("mov ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    return;
                }
                stream.push("orr", Colors::opcode());
            }
            Opcode::ORN => {
                if let Operand::Register(_, 31) = self.operands[1] {
                    stream.push("mvn ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                }
                stream.push("orn", Colors::opcode());
            }
            Opcode::ANDS => {
                if let Operand::Register(_, 31) = self.operands[0] {
                    stream.push("tst ", Colors::opcode());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                }
                stream.push("ands", Colors::opcode());
            }
            Opcode::NOT => {
                // `This instruction is used by the alias MVN. The alias is always the preferred
                // disassembly.`
                stream.push("mvn", Colors::opcode());
            }
            Opcode::ADDS => {
                if let Operand::Register(_, 31) = self.operands[0] {
                    stream.push("cmn ", Colors::opcode());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                } else if let Operand::RegShift(ShiftStyle::LSL, 0, size, reg) = self.operands[2] {
                    stream.push("adds ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    Operand::Register(size, reg).tokenize(stream, symbols);
                    return;
                }
                stream.push("adds", Colors::opcode());
            }
            Opcode::ADD => {
                if let Operand::Immediate(0) = self.operands[2] {
                    if let Operand::RegisterOrSP(_, 31) = self.operands[0] {
                        stream.push("mov ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[1].tokenize(stream, symbols);
                        return;
                    }
                    if let Operand::RegisterOrSP(_, 31) = self.operands[1] {
                        stream.push("mov ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[1].tokenize(stream, symbols);
                        return;
                    }
                // oh. add-with-zr does not alias mov
                //                } else if let Operand::Register(_, 31) = self.operands[1] {
                //                    return write!(fmt, "mov {}, {}", self.operands[0], self.operands[2]);
                } else if let Operand::RegShift(ShiftStyle::LSL, 0, size, reg) = self.operands[2] {
                    stream.push("add ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    Operand::Register(size, reg).tokenize(stream, symbols);
                    return;
                }
                stream.push("add", Colors::opcode());
            }
            Opcode::SUBS => {
                if let Operand::Register(_, 31) = self.operands[0] {
                    stream.push("cmp ", Colors::opcode());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                } else if let Operand::Register(_, 31) = self.operands[1] {
                    stream.push("negs ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                } else if let Operand::RegShift(ShiftStyle::LSL, 0, size, reg) = self.operands[2] {
                    stream.push("subs ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    Operand::Register(size, reg).tokenize(stream, symbols);
                    return;
                }
                stream.push("subs", Colors::opcode());
            }
            Opcode::SUB => {
                if let Operand::Register(_, 31) = self.operands[1] {
                    stream.push("neg ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                } else if let Operand::RegShift(ShiftStyle::LSL, 0, size, reg) = self.operands[2] {
                    stream.push("sub ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    Operand::Register(size, reg).tokenize(stream, symbols);
                    return;
                }
                stream.push("sub", Colors::opcode());
            }
            Opcode::SBCS => {
                if let Operand::Register(_, 31) = self.operands[1] {
                    stream.push("ngcs ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                }
                stream.push("sbcs", Colors::opcode());
            }
            Opcode::UBFM => {
                // TODO: handle ubfx alias
                if let (
                    Operand::Register(SizeCode::W, _),
                    Operand::Register(SizeCode::W, _),
                    Operand::Immediate(0),
                ) = (self.operands[0], self.operands[1], self.operands[2])
                {
                    if let Operand::Immediate(7) = self.operands[3] {
                        stream.push("uxtb ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[1].tokenize(stream, symbols);
                        return;
                    } else if let Operand::Immediate(15) = self.operands[3] {
                        stream.push("uxth ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[1].tokenize(stream, symbols);
                        return;
                    }
                }
                if let Operand::Immediate(imms) = self.operands[3] {
                    let size = if let Operand::Register(size, _) = self.operands[0] {
                        size
                    } else {
                        unreachable!("operand 0 is a register");
                    };
                    match (imms, size) {
                        (63, SizeCode::X) | (31, SizeCode::W) => {
                            stream.push("lsr ", Colors::opcode());
                            self.operands[0].tokenize(stream, symbols);
                            stream.push(", ", Colors::expr());
                            self.operands[1].tokenize(stream, symbols);
                            stream.push(", ", Colors::expr());
                            self.operands[2].tokenize(stream, symbols);
                            return;
                        }
                        _ => {
                            let size = if size == SizeCode::X { 64 } else { 32 };
                            let immr = if let Operand::Immediate(immr) = self.operands[2] {
                                immr
                            } else {
                                unreachable!("operand 3 is a register");
                            };
                            if imms + 1 == immr {
                                stream.push("lsl ", Colors::opcode());
                                self.operands[0].tokenize(stream, symbols);
                                stream.push(", ", Colors::expr());
                                self.operands[1].tokenize(stream, symbols);
                                stream.push(", #", Colors::expr());
                                stream.push_owned(decoder::encode_uhex((size - imms - 1) as u64), Colors::immediate());
                                return;
                            }
                            if imms < immr {
                                stream.push("ubfiz ", Colors::opcode());
                                self.operands[0].tokenize(stream, symbols);
                                stream.push(", ", Colors::expr());
                                self.operands[1].tokenize(stream, symbols);
                                stream.push(", #", Colors::expr());
                                stream.push_owned(decoder::encode_uhex((size - immr) as u64), Colors::immediate());
                                stream.push(", #", Colors::expr());
                                stream.push_owned(decoder::encode_uhex((imms + 1) as u64), Colors::immediate());
                                return;
                            }
                        }
                    }
                }
                // `ubfm` is never actually displayed: in the remaining case, it is always aliased
                // to `ubfx`
                let width = if let (Operand::Immediate(lsb), Operand::Immediate(width)) =
                    (self.operands[2], self.operands[3])
                {
                    Operand::Immediate(width - lsb + 1)
                } else {
                    unreachable!("last two operands of ubfm are always immediates");
                };

                stream.push("ubfx ", Colors::opcode());
                self.operands[0].tokenize(stream, symbols);
                stream.push(", ", Colors::expr());
                self.operands[1].tokenize(stream, symbols);
                stream.push(", ", Colors::expr());
                self.operands[2].tokenize(stream, symbols);
                stream.push(", ", Colors::expr());
                width.tokenize(stream, symbols);
                return;
            }
            Opcode::BFM => {
                if let (Operand::Immediate(immr), Operand::Immediate(imms)) =
                    (self.operands[2], self.operands[3])
                {
                    if imms < immr {
                        if let Operand::Register(sz, rn) = self.operands[1] {
                            let width = imms + 1;
                            let lsb = if sz == SizeCode::W {
                                ((-(immr as i8)) as u8) & 0x1f
                            } else {
                                ((-(immr as i8)) as u8) & 0x3f
                            };
                            if rn == 31 {
                                stream.push("bfc ", Colors::opcode());
                                self.operands[0].tokenize(stream, symbols);
                                stream.push(", #", Colors::expr());
                                stream.push_owned(decoder::encode_uhex(lsb as u64), Colors::immediate());
                                stream.push(", #", Colors::expr());
                                stream.push_owned(decoder::encode_uhex(width as u64), Colors::immediate());
                                return;
                            } else {
                                stream.push("bfi ", Colors::opcode());
                                self.operands[0].tokenize(stream, symbols);
                                stream.push(", ", Colors::expr());
                                self.operands[1].tokenize(stream, symbols);
                                stream.push(", #", Colors::expr());
                                stream.push_owned(decoder::encode_uhex(lsb as u64), Colors::immediate());
                                stream.push(", #", Colors::expr());
                                stream.push_owned(decoder::encode_uhex(width as u64), Colors::immediate());
                                return;
                            }
                        }
                    } else {
                        // bfxil
                        let lsb = immr;
                        let width = imms + 1 - lsb;

                        stream.push("bfxil ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[1].tokenize(stream, symbols);
                        stream.push(", #", Colors::expr());
                        stream.push_owned(decoder::encode_uhex(lsb as u64), Colors::immediate());
                        stream.push(", #", Colors::expr());
                        stream.push_owned(decoder::encode_uhex(width as u64), Colors::immediate());
                        return;
                    }
                }
            }
            Opcode::SBFM => {
                if let Operand::Immediate(63) = self.operands[3] {
                    if let Operand::Register(SizeCode::X, _) = self.operands[0] {
                        stream.push("asr ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[1].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                if let Operand::Immediate(31) = self.operands[3] {
                    if let Operand::Register(SizeCode::W, _) = self.operands[0] {
                        stream.push("asr ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[1].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                if let Operand::Immediate(0) = self.operands[2] {
                    let newsrc = if let Operand::Register(_size, srcnum) = self.operands[1] {
                        Operand::Register(SizeCode::W, srcnum)
                    } else {
                        unreachable!("operand 1 is always a register");
                    };
                    if let Operand::Immediate(7) = self.operands[3] {
                        stream.push("sxtb ", Colors::opcode());
                    } else if let Operand::Immediate(15) = self.operands[3] {
                        stream.push("sxth ", Colors::opcode());
                    } else if let Operand::Immediate(31) = self.operands[3] {
                        stream.push("sxtw ", Colors::opcode());
                    }
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    newsrc.tokenize(stream, symbols);
                    return;
                }
                if let (Operand::Immediate(imms), Operand::Immediate(immr)) =
                    (self.operands[2], self.operands[3])
                {
                    if immr < imms {
                        let size = if let Operand::Register(size, _) = self.operands[0] {
                            if size == SizeCode::W {
                                32
                            } else {
                                64
                            }
                        } else {
                            unreachable!("operand 0 is always a register");
                        };
                        stream.push("sbfiz ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[1].tokenize(stream, symbols);
                        stream.push(", #", Colors::expr());
                        stream.push_owned(decoder::encode_uhex((size - imms) as u64), Colors::immediate());
                        stream.push(", #", Colors::expr());
                        stream.push_owned(decoder::encode_uhex((immr + 1) as u64), Colors::immediate());
                        return;
                    }
                }
                // `sbfm` is never actually displayed: in the remaining case, it is always aliased
                // to `sbfx`
                let width = if let (Operand::Immediate(lsb), Operand::Immediate(width)) =
                    (self.operands[2], self.operands[3])
                {
                    Operand::Immediate(width - lsb + 1)
                } else {
                    unreachable!("last two operands of sbfm are always immediates");
                };
                stream.push("sbfx ", Colors::opcode());
                self.operands[0].tokenize(stream, symbols);
                stream.push(", ", Colors::expr());
                self.operands[1].tokenize(stream, symbols);
                stream.push(", ", Colors::expr());
                self.operands[2].tokenize(stream, symbols);
                stream.push(", ", Colors::expr());
                width.tokenize(stream, symbols);
                return;
            }
            Opcode::EXTR => {
                if let (Operand::Register(_, Rn), Operand::Register(_, Rm)) =
                    (self.operands[1], self.operands[2])
                {
                    if Rn == Rm {
                        stream.push("ror ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[3].tokenize(stream, symbols);
                        return;
                    }
                }
                stream.push("extr", Colors::opcode());
            }
            Opcode::RET => {
                stream.push("ret", Colors::opcode());
                if let Operand::Register(SizeCode::X, 30) = self.operands[0] {
                    // C5.6.148:  Defaults to X30 if absent.
                    // so ret x30 is probably expected to be read as just `ret`
                    return;
                }
            }
            Opcode::SYS(ops) => {
                stream.push("sys #", Colors::opcode());
                stream.push_owned(decoder::encode_uhex(ops.op1() as u64), Colors::immediate());
                stream.push(", ", Colors::expr());
                self.operands[1].tokenize(stream, symbols);
                stream.push(", ", Colors::expr());
                self.operands[2].tokenize(stream, symbols);
                stream.push(", #", Colors::expr());
                stream.push_owned(decoder::encode_uhex(ops.op2() as u64), Colors::immediate());
                stream.push(", ", Colors::expr());
                self.operands[0].tokenize(stream, symbols);
                return;
            }
            Opcode::SYSL(ops) => {
                stream.push("sysl ", Colors::opcode());
                self.operands[2].tokenize(stream, symbols);
                stream.push(", #", Colors::expr());
                stream.push_owned(decoder::encode_uhex(ops.op1() as u64), Colors::immediate());
                stream.push(", ", Colors::expr());
                self.operands[0].tokenize(stream, symbols);
                stream.push(", ", Colors::expr());
                self.operands[1].tokenize(stream, symbols);
                stream.push(", #", Colors::expr());
                stream.push_owned(decoder::encode_uhex(ops.op2() as u64), Colors::immediate());
                return;
            }
            Opcode::HINT => {
                if let (Operand::ControlReg(CRn), Operand::Immediate(op2)) =
                    (self.operands[0], self.operands[1])
                {
                    let hint_num = (CRn << 3) | op2 as u16;
                    match hint_num & 0b111111 {
                        0x0 => stream.push("nop", Colors::opcode()),
                        0x1 => stream.push("yield", Colors::opcode()),
                        0x2 => stream.push("wfe", Colors::opcode()),
                        0x3 => stream.push("wfi", Colors::opcode()),
                        0x4 => stream.push("sev", Colors::opcode()),
                        0x10 => stream.push("esb", Colors::opcode()),
                        0x11 => stream.push("psb csync", Colors::opcode()),
                        0x12 => stream.push("tsb csync", Colors::opcode()),
                        0x14 => stream.push("csdb", Colors::opcode()),
                        0x15 => stream.push("sevl", Colors::opcode()),
                        _ => {
                            stream.push("hint #", Colors::opcode());
                            stream.push_owned(decoder::encode_uhex(hint_num as u64), Colors::immediate());
                        }
                    };
                    return;
                }
            }
            Opcode::CSNEG => {
                if let (
                    Operand::Register(_size, rn),
                    Operand::Register(_, rm),
                    Operand::ConditionCode(cond),
                ) = (self.operands[1], self.operands[2], self.operands[3])
                {
                    if cond < 0b1110 && rn == rm {
                        stream.push("cneg ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        Operand::ConditionCode(cond ^ 0x01).tokenize(stream, symbols);
                        return;
                    }
                } else {
                    unreachable!("operands 2 and 3 are always registers");
                }
                stream.push("csneg", Colors::opcode());
            }
            Opcode::CSINC => {
                match (self.operands[1], self.operands[2], self.operands[3]) {
                    (
                        Operand::Register(_, n),
                        Operand::Register(_, m),
                        Operand::ConditionCode(cond),
                    ) => {
                        if n == m && cond < 0b1110 {
                            if n == 31 {
                                stream.push("cset ", Colors::opcode());
                                self.operands[0].tokenize(stream, symbols);
                                stream.push(", ", Colors::expr());
                                Operand::ConditionCode(cond ^ 0x01).tokenize(stream, symbols);
                                return;
                            } else {
                                stream.push("cinc ", Colors::opcode());
                                self.operands[0].tokenize(stream, symbols);
                                stream.push(", ", Colors::expr());
                                self.operands[1].tokenize(stream, symbols);
                                stream.push(", ", Colors::expr());
                                Operand::ConditionCode(cond ^ 0x01).tokenize(stream, symbols);
                                return;
                            }
                        }
                    }
                    _ => {}
                }
                stream.push("csinc", Colors::opcode());
            }
            Opcode::CSINV => {
                match (self.operands[1], self.operands[2], self.operands[3]) {
                    (
                        Operand::Register(_, n),
                        Operand::Register(_, m),
                        Operand::ConditionCode(cond),
                    ) => {
                        if n == m && n != 31 && cond < 0b1110 {
                            stream.push("cinv ", Colors::opcode());
                            self.operands[0].tokenize(stream, symbols);
                            stream.push(", ", Colors::expr());
                            self.operands[1].tokenize(stream, symbols);
                            stream.push(", ", Colors::expr());
                            Operand::ConditionCode(cond ^ 0x01).tokenize(stream, symbols);
                            return;
                        } else if n == m && n == 31 && cond < 0b1110 {
                            stream.push("csetm ", Colors::opcode());
                            self.operands[0].tokenize(stream, symbols);
                            stream.push(", ", Colors::expr());
                            Operand::ConditionCode(cond ^ 0x01).tokenize(stream, symbols);
                            return;
                        }
                    }
                    _ => {}
                }
                stream.push("csinv", Colors::opcode());
            }
            Opcode::MADD => {
                if let Operand::Register(_, 31) = self.operands[3] {
                    stream.push("mul ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                }
                stream.push("madd", Colors::opcode());
            }
            Opcode::MSUB => {
                if let Operand::Register(_, 31) = self.operands[3] {
                    stream.push("mneg ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                }
                stream.push("msub", Colors::opcode());
            }
            Opcode::SMADDL => {
                if let Operand::Register(_, 31) = self.operands[3] {
                    stream.push("smull ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                }
                stream.push("smaddl", Colors::opcode());
            }
            Opcode::SMSUBL => {
                if let Operand::Register(_, 31) = self.operands[3] {
                    stream.push("smnegl ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                }
                stream.push("smsubl", Colors::opcode());
            }
            Opcode::UMADDL => {
                if let Operand::Register(_, 31) = self.operands[3] {
                    stream.push("umull ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                }
                stream.push("umaddl", Colors::opcode());
            }
            Opcode::UMSUBL => {
                if let Operand::Register(_, 31) = self.operands[3] {
                    stream.push("umnegl ", Colors::opcode());
                    self.operands[0].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[1].tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    self.operands[2].tokenize(stream, symbols);
                    return;
                }
                stream.push("umsubl", Colors::opcode());
            }
            Opcode::LSLV => {
                // lslv == lsl (register) and, quoth the manual, `lsl is always the preferred
                // disassembly`.
                stream.push("lsl", Colors::opcode());
            }
            Opcode::LSRV => {
                // lsrv == lsr (register) and, quoth the manual, `lsr is always the preferred
                // disassembly`.
                stream.push("lsr", Colors::opcode());
            }
            Opcode::ASRV => {
                // asrv == asr (register) and, quoth the manual, `asr is always the preferred
                // disassembly`.
                stream.push("asr", Colors::opcode());
            }
            Opcode::RORV => {
                // rorv == ror (register) and, quoth the manual, `ror is always the preferred
                // disassembly`.
                stream.push("ror", Colors::opcode());
            }
            Opcode::INS => {
                // `ins (element)` and `ins (general)` both have `mov` as an alias. manual reports
                // that `mov` is the preferred disassembly.
                stream.push("mov", Colors::opcode());
            }
            Opcode::DUP => {
                if let Operand::Register(_, _) = self.operands[1] {
                    // `dup (general)`
                    stream.push("dup", Colors::opcode());
                } else {
                    // `dup (element)`
                    // manual says `mov` is the preferred disassembly here? but capstone uses
                    // `dup`.
                    stream.push("mov", Colors::opcode());
                }
            }
            Opcode::UMOV => {
                if let (
                    Operand::Register(reg_sz, _),
                    Operand::SIMDRegisterElementsLane(_, _, elem_sz, _),
                ) = (self.operands[0], self.operands[1])
                {
                    if (reg_sz == SizeCode::W && elem_sz == SIMDSizeCode::S)
                        || (reg_sz == SizeCode::X && elem_sz == SIMDSizeCode::D)
                    {
                        stream.push("mov ", Colors::opcode());
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[1].tokenize(stream, symbols);
                        return;
                    } else {
                        stream.push("umov", Colors::opcode());
                    }
                }
            }
            Opcode::LDADD(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stadd ", Colors::opcode());
                        } else {
                            stream.push("staddl ", Colors::opcode());
                        }
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDCLR(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stclr ", Colors::opcode());
                        } else {
                            stream.push("stclrl ", Colors::opcode());
                        }
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDSET(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stset ", Colors::opcode());
                        } else {
                            stream.push("stsetl ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDSMAX(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stsmax ", Colors::opcode());
                        } else {
                            stream.push("stsmaxl ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDSMIN(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stsmin ", Colors::opcode());
                        } else {
                            stream.push("stsminl ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDUMAX(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stumax ", Colors::opcode());
                        } else {
                            stream.push("stumaxl ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDUMIN(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stumin ", Colors::opcode());
                        } else {
                            stream.push("stuminl ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDEOR(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("steor ", Colors::opcode());
                        } else {
                            stream.push("steorl ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDADDH(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("staddh ", Colors::opcode());
                        } else {
                            stream.push("staddlh ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDCLRH(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stclrh ", Colors::opcode());
                        } else {
                            stream.push("stclrlh ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDSETH(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stseth ", Colors::opcode());
                        } else {
                            stream.push("stsetlh ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDSMAXH(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stsmaxh ", Colors::opcode());
                        } else {
                            stream.push("stsmaxlh ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDSMINH(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stsminh ", Colors::opcode());
                        } else {
                            stream.push("stsminlh ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDUMAXH(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stumaxh ", Colors::opcode());
                        } else {
                            stream.push("stumaxlh ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDUMINH(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stuminh ", Colors::opcode());
                        } else {
                            stream.push("stuminlh ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDEORH(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("steorh ", Colors::opcode());
                        } else {
                            stream.push("steorlh ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDADDB(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("staddb ", Colors::opcode());
                        } else {
                            stream.push("staddlb ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDCLRB(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stclrb ", Colors::opcode());
                        } else {
                            stream.push("stclrlb ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDSETB(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stsetb ", Colors::opcode());
                        } else {
                            stream.push("stsetlb ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDSMAXB(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stsmaxb ", Colors::opcode());
                        } else {
                            stream.push("stsmaxlb ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDSMINB(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stsminb ", Colors::opcode());
                        } else {
                            stream.push("stsminlb ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDUMAXB(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stumaxb ", Colors::opcode());
                        } else {
                            stream.push("stumaxlb ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDUMINB(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("stuminb ", Colors::opcode());
                        } else {
                            stream.push("stuminlb ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            Opcode::LDEORB(ar) => {
                if let Operand::Register(_, rt) = self.operands[1] {
                    if rt == 31 && ar & 0b10 == 0b00 {
                        if ar & 0b01 == 0b00 {
                            stream.push("steorb ", Colors::opcode());
                        } else {
                            stream.push("steorlb ", Colors::opcode());
                        };
                        self.operands[0].tokenize(stream, symbols);
                        stream.push(", ", Colors::expr());
                        self.operands[2].tokenize(stream, symbols);
                        return;
                    }
                }
                self.opcode.tokenize(stream, symbols);
            }
            other => other.tokenize(stream, symbols),
        };

        if self.operands[0] != Operand::Nothing {
            stream.push(" ", Colors::expr());
            self.operands[0].tokenize(stream, symbols);
        } else {
            return;
        }

        if self.operands[1] != Operand::Nothing {
            stream.push(", ", Colors::expr());
            self.operands[1].tokenize(stream, symbols);
        } else {
            return;
        }

        if self.operands[2] != Operand::Nothing {
            stream.push(", ", Colors::expr());
            self.operands[2].tokenize(stream, symbols);
        } else {
            return;
        }

        if self.operands[3] != Operand::Nothing {
            stream.push(", ", Colors::expr());
            self.operands[3].tokenize(stream, symbols);
        } else {
            return;
        }
    }
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction {
            opcode: Opcode::Invalid,
            operands: [
                Operand::Nothing,
                Operand::Nothing,
                Operand::Nothing,
                Operand::Nothing,
            ],
        }
    }
}

/// a descriptor for the operation in a `sys` or `sysl` instruction.
///
/// there are two fields of interest, `op1` and `op2`. for a description of how to interpret these
/// fields of the `sys{,l}` opcodes, consult the `ARM Reference Manual`.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(transparent)]
pub struct SysOps {
    data: u8,
}

impl SysOps {
    fn new(op1: u8, op2: u8) -> Self {
        SysOps {
            data: op1 | (op2 << 4),
        }
    }

    /// retrieve the `op1` field encoded in this `SysOps`.
    #[inline]
    pub fn op1(&self) -> u8 {
        self.data & 0b1111
    }
    /// retrieve the `op2` field encoded in this `SysOps`.
    #[inline]
    pub fn op2(&self) -> u8 {
        (self.data >> 4) & 0b1111
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u16)]
#[allow(missing_docs)]
pub enum Opcode {
    Invalid,
    MOVN,
    MOVK,
    MOVZ,
    ADC,
    ADCS,
    SBC,
    SBCS,
    AND,
    ORR,
    ORN,
    EOR,
    EON,
    BIC,
    BICS,
    ANDS,
    ADDS,
    ADD,
    SUBS,
    SUB,
    BFM,
    UBFM,
    SBFM,
    ADR,
    ADRP,
    EXTR,
    LDAR,
    LDLAR,
    LDARB,
    LDLARB,
    LDAXRB,
    LDARH,
    LDLARH,
    LDAXP,
    LDAXR,
    LDAXRH,
    LDP,
    LDPSW,
    LDR,
    LDRB,
    LDRSB,
    LDRSW,
    LDRSH,
    LDRH,
    LDTR,
    LDTRB,
    LDTRH,
    LDTRSB,
    LDTRSH,
    LDTRSW,
    LDUR,
    LDURB,
    LDURSB,
    LDURSW,
    LDURSH,
    LDURH,
    LDXP,
    LDXR,
    LDXRB,
    LDXRH,
    STLR,
    STLLR,
    STLRB,
    STLLRB,
    STLRH,
    STLLRH,
    STLXP,
    STLXR,
    STLXRB,
    STLXRH,
    STP,
    STR,
    STTR,
    STTRB,
    STTRH,
    STRB,
    STRH,
    STRW,
    STUR,
    STURB,
    STURH,
    STXP,
    STXR,
    STXRB,
    STXRH,
    TBZ,
    TBNZ,
    CBZ,
    CBNZ,
    B,
    BR,
    Bcc(u8),
    BL,
    BLR,
    SVC,
    HVC,
    SMC,
    BRK,
    HLT,
    DCPS1,
    DCPS2,
    DCPS3,
    RET,
    ERET,
    DRPS,
    MSR, // TODO: handle aliases (like cfinv)
    MRS,
    SYS(SysOps),
    SYSL(SysOps),
    ISB,
    DSB(u8),
    DMB(u8),
    SB,
    SSSB,
    HINT,
    CLREX,
    CSEL,
    CSNEG,
    CSINC,
    CSINV,
    CCMN,
    CCMP,
    RBIT,
    REV16,
    REV,
    REV32,
    CLZ,
    CLS,
    MADD,
    MSUB,
    SMADDL,
    SMSUBL,
    SMULH,
    UMADDL,
    UMSUBL,
    UMULH,
    UDIV,
    SDIV,
    LSLV,
    LSRV,
    ASRV,
    RORV,
    CRC32B,
    CRC32H,
    CRC32W,
    CRC32X,
    CRC32CB,
    CRC32CH,
    CRC32CW,
    CRC32CX,
    STNP,
    LDNP,
    ST1,
    ST2,
    ST3,
    ST4,
    LD1,
    LD2,
    LD3,
    LD4,
    LD1R,
    LD2R,
    LD3R,
    LD4R,
    FMADD,
    FMSUB,
    FNMADD,
    FNMSUB,
    SCVTF,
    UCVTF,
    FCVTZS,
    FCVTZU,
    FMOV,
    FABS,
    FNEG,
    FSQRT,
    FRINTN,
    FRINTP,
    FRINTM,
    FRINTZ,
    FRINTA,
    FRINTX,
    FRINTI,
    FRINT32Z,
    FRINT32X,
    FRINT64Z,
    FRINT64X,
    BFCVT,
    FCVT,
    FCMP,
    FCMPE,
    FMUL,
    FDIV,
    FADD,
    FSUB,
    FMAX,
    FMIN,
    FMAXNM,
    FMINNM,
    FNMUL,
    FCSEL,
    FCCMP,
    FCCMPE,
    FMULX,
    FMLSL,
    FMLAL,
    SQRDMLSH,
    UDOT,
    SQRDMLAH,
    UMULL,
    UMULL2,
    UMLSL,
    UMLSL2,
    MLS,
    UMLAL,
    UMLAL2,
    MLA,
    SDOT,
    SQDMULH,
    SQDMULL,
    SQDMULL2,
    SMULL,
    SMULL2,
    MUL,
    SQDMLSL,
    SQDMLSL2,
    SMLSL,
    SMLSL2,
    SQDMLAL,
    SQDMLAL2,
    SMLAL,
    SMLAL2,
    SQRDMULH,
    FCMLA,
    SSHR,
    SSRA,
    SRSHR,
    SRSRA,
    SHL,
    SQSHL,
    SHRN,
    RSHRN,
    SQSHRN,
    SQRSHRN,
    SSHLL,
    USHR,
    USRA,
    URSHR,
    URSRA,
    SRI,
    SLI,
    SQSHLU,
    UQSHL,
    SQSHRUN,
    SQRSHRUN,
    UQSHRN,
    UQRSHRN,
    USHLL,
    MOVI,
    MVNI,
    SHADD,
    SQADD,
    SRHADD,
    SHSUB,
    SQSUB,
    CMGT,
    CMGE,
    SSHL,
    SRSHL,
    SQRSHL,
    SMAX,
    SMIN,
    SABD,
    SABA,
    CMTST,
    SMAXP,
    SMINP,
    ADDP,
    UHADD,
    UQADD,
    URHADD,
    UHSUB,
    UQSUB,
    CMHI,
    CMHS,
    USHL,
    URSHL,
    UQRSHL,
    UMAX,
    UMIN,
    UABD,
    UABA,
    CMEQ,
    PMUL,
    UMAXP,
    UMINP,
    FMLA,
    FCMEQ,
    FRECPS,
    BSL,
    BIT,
    BIF,
    FMAXNMP,
    FMINMNP,
    FADDP,
    FCMGE,
    FACGE,
    FMAXP,
    SADDL,
    SADDL2,
    SADDW,
    SADDW2,
    SSUBL,
    SSUBL2,
    SSUBW,
    SSUBW2,
    ADDHN,
    ADDHN2,
    SABAL,
    SABAL2,
    SUBHN,
    SUBHN2,
    SABDL,
    SABDL2,
    PMULL,
    PMULL2,
    UADDL,
    UADDL2,
    UADDW,
    UADDW2,
    USUBL,
    USUBL2,
    USUBW,
    USUBW2,
    RADDHN,
    RADDHN2,
    RSUBHN,
    RSUBHN2,
    UABAL,
    UABAL2,
    UABDL,
    UABDL2,

    REV64,
    SADDLP,
    SUQADD,
    CNT,
    SADALP,
    SQABS,
    CMLT,
    ABS,
    XTN,
    XTN2,
    SQXTN,
    SQXTN2,
    FCVTN,
    FCVTN2,
    FCMGT,
    FCVTL,
    FCVTL2,
    FCVTNS,
    FCVTPS,
    FCVTMS,
    FCVTAS,
    URECPE,
    FRECPE,

    UADDLP,
    USQADD,
    UADALP,
    SQNEG,
    CMLE,
    NEG,
    SQXTUN,
    SQXTUN2,
    SHLL,
    SHLL2,
    UQXTN,
    UQXTN2,
    FCVTXN,
    FCVTXN2,
    FCVTNU,
    FCVTMU,
    FCVTAU,

    INS,
    EXT,
    DUP,
    UZP1,
    TRN1,
    ZIP1,
    UZP2,
    TRN2,
    ZIP2,

    SMOV,
    UMOV,

    SQSHRN2,
    SQRSHRN2,
    SQSHRUN2,
    UQSHRN2,
    UQRSHRN2,

    FMLS,

    FRECPX,
    FRSQRTE,
    FCVTPU,
    FCMLT,
    FCMLE,

    FMAXNMV,
    FMINNMV,
    FMAXV,
    FMINV,
    UADDLV,
    SADDLV,
    UMAXV,
    SMAXV,
    UMINV,
    SMINV,
    ADDV,

    FRSQRTS,
    FMINNMP,
    FMLAL2,
    FMLSL2,
    FABD,
    FACGT,
    FMINP,

    FJCVTZS,

    URSQRTE,

    PRFM,
    PRFUM,

    AESE,
    AESD,
    AESMC,
    AESIMC,

    SHA1H,
    SHA1SU1,
    SHA256SU0,

    SM3TT1A,
    SM3TT1B,
    SM3TT2A,
    SM3TT2B,
    SHA512H,
    SHA512H2,
    SHA512SU1,
    RAX1,
    SM3PARTW1,
    SM3PARTW2,
    SM4EKEY,
    BCAX,
    SM3SS1,
    SHA512SU0,
    SM4E,
    EOR3,
    XAR,

    LDRAA,
    LDRAB,

    LDAPR,
    LDAPRH,
    LDAPRB,
    SWP(u8),
    SWPB(u8),
    SWPH(u8),
    LDADDB(u8),
    LDCLRB(u8),
    LDEORB(u8),
    LDSETB(u8),
    LDSMAXB(u8),
    LDSMINB(u8),
    LDUMAXB(u8),
    LDUMINB(u8),
    LDADDH(u8),
    LDCLRH(u8),
    LDEORH(u8),
    LDSETH(u8),
    LDSMAXH(u8),
    LDSMINH(u8),
    LDUMAXH(u8),
    LDUMINH(u8),
    LDADD(u8),
    LDCLR(u8),
    LDEOR(u8),
    LDSET(u8),
    LDSMAX(u8),
    LDSMIN(u8),
    LDUMAX(u8),
    LDUMIN(u8),

    CAS(u8),
    CASH(u8),
    CASB(u8),
    CASP(u8),

    TBL,
    TBX,

    FCADD,

    LDGM,
    LDG,
    STGM,
    STZGM,
    STG,
    STZG,
    ST2G,
    STZ2G,

    LDAPUR,
    LDAPURB,
    LDAPURH,
    LDAPURSB,
    LDAPURSH,
    LDAPURSW,
    STLUR,
    STLURB,
    STLURH,

    SETF8,
    SETF16,
    RMIF,

    NOT,

    RSHRN2,
    SQRSHRUN2,
    USHLL2,
    SSHLL2,

    SHA1C,
    SHA1P,
    SHA1M,
    SHA1SU0,
    SHA256H,
    SHA256H2,
    SHA256SU1,

    SHRN2,

    BLRAA,
    BLRAAZ,
    BLRAB,
    BLRABZ,
    BRAA,
    BRAAZ,
    BRAB,
    BRABZ,
    RETAA,
    RETAB,
    ERETAA,
    ERETAB,

    PACIA,
    PACIB,
    PACDA,
    PACDB,
    AUTIA,
    AUTIB,
    AUTDA,
    AUTDB,
    PACIZA,
    PACIZB,
    PACDZA,
    PACDZB,
    AUTIZA,
    AUTIZB,
    AUTDZA,
    AUTDZB,
    XPACI,
    XPACD,

    PACGA,
    GMI,
    IRG,
    SUBP,
    SUBPS,
}

impl ToTokens for Opcode {
    fn tokenize(&self, stream: &mut TokenStream, symbols: &Index) {
        let text = match *self {
            Opcode::Invalid => "invalid",
            Opcode::MOVK => "movk",
            Opcode::ADC => "adc",
            Opcode::ADCS => "adcs",
            Opcode::SBC => "sbc",
            Opcode::SBCS => "sbcs",
            Opcode::AND => "and",
            Opcode::BIC => "bic",
            Opcode::BICS => "bics",
            Opcode::EOR => "eor",
            Opcode::EON => "eon",
            Opcode::BFM => "bfm",
            Opcode::ADR => "adr",
            Opcode::ADRP => "adrp",
            Opcode::LDP => "ldp",
            Opcode::LDPSW => "ldpsw",
            Opcode::LDR => "ldr",
            Opcode::LDRB => "ldrb",
            Opcode::LDRSB => "ldrsb",
            Opcode::LDRSH => "ldrsh",
            Opcode::LDRSW => "ldrsw",
            Opcode::LDRH => "ldrh",
            Opcode::LDTR => "ldtr",
            Opcode::LDTRB => "ldtrb",
            Opcode::LDTRSB => "ldtrsb",
            Opcode::LDTRSH => "ldtrsh",
            Opcode::LDTRSW => "ldtrsw",
            Opcode::LDTRH => "ldtrh",
            Opcode::LDUR => "ldur",
            Opcode::LDURB => "ldurb",
            Opcode::LDURSB => "ldursb",
            Opcode::LDURSW => "ldursw",
            Opcode::LDURSH => "ldursh",
            Opcode::LDURH => "ldurh",
            Opcode::LDAR => "ldar",
            Opcode::LDARB => "ldarb",
            Opcode::LDAXRB => "ldaxrb",
            Opcode::LDARH => "ldarh",
            Opcode::LDAXP => "ldaxp",
            Opcode::LDAXR => "ldaxr",
            Opcode::LDAXRH => "ldaxrh",
            Opcode::LDXP => "ldxp",
            Opcode::LDXR => "ldxr",
            Opcode::LDXRB => "ldxrb",
            Opcode::LDXRH => "ldxrh",
            Opcode::STP => "stp",
            Opcode::STR => "str",
            Opcode::STRB => "strb",
            Opcode::STRH => "strh",
            Opcode::STRW => "strw",
            Opcode::STTR => "sttr",
            Opcode::STTRB => "sttrb",
            Opcode::STTRH => "sttrh",
            Opcode::STUR => "stur",
            Opcode::STURB => "sturb",
            Opcode::STURH => "sturh",
            Opcode::STLR => "stlr",
            Opcode::STLRB => "stlrb",
            Opcode::STLRH => "stlrh",
            Opcode::STLXP => "stlxp",
            Opcode::STLXR => "stlxr",
            Opcode::STLXRB => "stlxrb",
            Opcode::STLXRH => "stlxrh",
            Opcode::STXP => "stxp",
            Opcode::STXR => "stxr",
            Opcode::STXRB => "stxrb",
            Opcode::STXRH => "stxrh",
            Opcode::TBZ => "tbz",
            Opcode::TBNZ => "tbnz",
            Opcode::CBZ => "cbz",
            Opcode::CBNZ => "cbnz",
            Opcode::B => "b",
            Opcode::BR => "br",
            Opcode::BL => "bl",
            Opcode::BLR => "blr",
            Opcode::SVC => "svc",
            Opcode::HVC => "hvc",
            Opcode::SMC => "smc",
            Opcode::BRK => "brk",
            Opcode::HLT => "hlt",
            Opcode::DCPS1 => "dcps1",
            Opcode::DCPS2 => "dcps2",
            Opcode::DCPS3 => "dcps3",
            Opcode::ERET => "eret",
            Opcode::DRPS => "drps",
            Opcode::MSR => "msr",
            Opcode::MRS => "mrs",
            Opcode::ISB => "isb",
            Opcode::SB => "sb",
            Opcode::SSSB => "sssb",
            Opcode::CLREX => "clrex",
            Opcode::CSEL => "csel",
            Opcode::CCMN => "ccmn",
            Opcode::CCMP => "ccmp",
            Opcode::RBIT => "rbit",
            Opcode::REV16 => "rev16",
            Opcode::REV => "rev",
            Opcode::REV32 => "rev32",
            Opcode::CLZ => "clz",
            Opcode::CLS => "cls",
            Opcode::CRC32B => "crc32b",
            Opcode::CRC32H => "crc32h",
            Opcode::CRC32W => "crc32w",
            Opcode::CRC32X => "crc32x",
            Opcode::CRC32CB => "crc32cb",
            Opcode::CRC32CH => "crc32ch",
            Opcode::CRC32CW => "crc32cw",
            Opcode::CRC32CX => "crc32cx",
            Opcode::STNP => "stnp",
            Opcode::LDNP => "ldnp",
            Opcode::ST1 => "st1",
            Opcode::ST2 => "st2",
            Opcode::ST3 => "st3",
            Opcode::ST4 => "st4",
            Opcode::LD1R => "ld1r",
            Opcode::LD2R => "ld2r",
            Opcode::LD3R => "ld3r",
            Opcode::LD4R => "ld4r",
            Opcode::LD1 => "ld1",
            Opcode::LD2 => "ld2",
            Opcode::LD3 => "ld3",
            Opcode::LD4 => "ld4",
            Opcode::FMADD => "fmadd",
            Opcode::FMSUB => "fmsub",
            Opcode::FNMADD => "fnmadd",
            Opcode::FNMSUB => "fnmsub",
            Opcode::SCVTF => "scvtf",
            Opcode::UCVTF => "ucvtf",
            Opcode::FCVTZS => "fcvtzs",
            Opcode::FCVTZU => "fcvtzu",
            Opcode::FMOV => "fmov",
            Opcode::FABS => "fabs",
            Opcode::FNEG => "fneg",
            Opcode::FSQRT => "fsqrt",
            Opcode::FRINTN => "frintn",
            Opcode::FRINTP => "frintp",
            Opcode::FRINTM => "frintm",
            Opcode::FRINTZ => "frintz",
            Opcode::FRINTA => "frinta",
            Opcode::FRINTX => "frintx",
            Opcode::FRINTI => "frinti",
            Opcode::FRINT32Z => "frint32z",
            Opcode::FRINT32X => "frint32x",
            Opcode::FRINT64Z => "frint64z",
            Opcode::FRINT64X => "frint64x",
            Opcode::BFCVT => "bfcvt",
            Opcode::FCVT => "fcvt",
            Opcode::FCMP => "fcmp",
            Opcode::FCMPE => "fcmpe",
            Opcode::FMUL => "fmul",
            Opcode::FDIV => "fdiv",
            Opcode::FADD => "fadd",
            Opcode::FSUB => "fsub",
            Opcode::FMAX => "fmax",
            Opcode::FMIN => "fmin",
            Opcode::FMAXNM => "fmaxnm",
            Opcode::FMINNM => "fminnm",
            Opcode::FNMUL => "fnmul",
            Opcode::FCSEL => "fcsel",
            Opcode::FCCMP => "fccmp",
            Opcode::FCCMPE => "fccmpe",
            Opcode::FMULX => "fmulx",
            Opcode::FMLSL => "fmlsl",
            Opcode::FMLAL => "fmlal",
            Opcode::SQRDMLSH => "sqrdmlsh",
            Opcode::UDOT => "udot",
            Opcode::SQRDMLAH => "sqrdmlah",
            Opcode::UMULL => "umull",
            Opcode::UMULL2 => "umull2",
            Opcode::UMLSL => "umlsl",
            Opcode::UMLSL2 => "umlsl2",
            Opcode::MLS => "mls",
            Opcode::UMLAL => "umlal",
            Opcode::UMLAL2 => "umlal2",
            Opcode::MLA => "mla",
            Opcode::SDOT => "sdot",
            Opcode::SQDMULH => "sqdmulh",
            Opcode::SQDMULL => "sqdmull",
            Opcode::SQDMULL2 => "sqdmull2",
            Opcode::SMULL => "smull",
            Opcode::SMULL2 => "smull2",
            Opcode::MUL => "mul",
            Opcode::SQDMLSL => "sqdmlsl",
            Opcode::SQDMLSL2 => "sqdmlsl2",
            Opcode::SMLSL => "smlsl",
            Opcode::SMLSL2 => "smlsl2",
            Opcode::SQDMLAL => "sqdmlal",
            Opcode::SQDMLAL2 => "sqdmlal2",
            Opcode::SMLAL => "smlal",
            Opcode::SMLAL2 => "smlal2",
            Opcode::SQRDMULH => "sqrdmulh",
            Opcode::FCMLA => "fcmla",
            Opcode::SSHR => "sshr",
            Opcode::SSRA => "ssra",
            Opcode::SRSHR => "srshr",
            Opcode::SRSRA => "srsra",
            Opcode::SHL => "shl",
            Opcode::SQSHL => "sqshl",
            Opcode::SHRN => "shrn",
            Opcode::SHRN2 => "shrn2",
            Opcode::RSHRN => "rshrn",
            Opcode::SQSHRN => "sqshrn",
            Opcode::SQRSHRN => "sqrshrn",
            Opcode::SSHLL => "sshll",
            Opcode::USHLL => "ushll",
            Opcode::USHR => "ushr",
            Opcode::USRA => "usra",
            Opcode::URSHR => "urshr",
            Opcode::URSRA => "ursra",
            Opcode::SRI => "sri",
            Opcode::SLI => "sli",
            Opcode::SQSHLU => "sqshlu",
            Opcode::UQSHL => "uqshl",
            Opcode::SQSHRUN => "sqshrun",
            Opcode::SQRSHRUN => "sqrshrun",
            Opcode::UQSHRN => "uqshrn",
            Opcode::UQRSHRN => "uqrshrn",
            Opcode::MOVI => "movi",
            Opcode::MVNI => "mvni",
            Opcode::NOT => "not",
            Opcode::RSHRN2 => "rshrn2",
            Opcode::SQRSHRUN2 => "sqrshrun2",
            Opcode::USHLL2 => "ushll2",
            Opcode::SSHLL2 => "sshll2",
            Opcode::SHADD => "shadd",
            Opcode::SQADD => "sqadd",
            Opcode::SRHADD => "srhadd",
            Opcode::SHSUB => "shsub",
            Opcode::SQSUB => "sqsub",
            Opcode::CMGT => "cmgt",
            Opcode::CMGE => "cmge",
            Opcode::SSHL => "sshl",
            Opcode::SRSHL => "srshl",
            Opcode::SQRSHL => "sqrshl",
            Opcode::SMAX => "smax",
            Opcode::SMIN => "smin",
            Opcode::SABD => "sabd",
            Opcode::SABA => "saba",
            Opcode::CMTST => "cmtst",
            Opcode::SMAXP => "smaxp",
            Opcode::SMINP => "sminp",
            Opcode::ADDP => "addp",
            Opcode::UHADD => "uhadd",
            Opcode::UQADD => "uqadd",
            Opcode::URHADD => "urhadd",
            Opcode::UHSUB => "uhsub",
            Opcode::UQSUB => "uqsub",
            Opcode::CMHI => "cmhi",
            Opcode::CMHS => "cmhs",
            Opcode::USHL => "ushl",
            Opcode::URSHL => "urshl",
            Opcode::UQRSHL => "uqrshl",
            Opcode::UMAX => "umax",
            Opcode::UMIN => "umin",
            Opcode::UABD => "uabd",
            Opcode::UABA => "uaba",
            Opcode::CMEQ => "cmeq",
            Opcode::PMUL => "pmul",
            Opcode::UMAXP => "umaxp",
            Opcode::UMINP => "uminp",
            Opcode::FMLA => "fmla",
            Opcode::FCMEQ => "fcmeq",
            Opcode::FRECPS => "frecps",
            Opcode::BSL => "bsl",
            Opcode::BIT => "bit",
            Opcode::BIF => "bif",
            Opcode::FMAXNMP => "fmaxnmp",
            Opcode::FMINMNP => "fminmnp",
            Opcode::FADDP => "faddp",
            Opcode::FCMGE => "fcmge",
            Opcode::FACGE => "facge",
            Opcode::FMAXP => "fmaxp",
            Opcode::SADDL => "saddl",
            Opcode::SADDL2 => "saddl2",
            Opcode::SADDW => "saddw",
            Opcode::SADDW2 => "saddw2",
            Opcode::SSUBL => "ssubl",
            Opcode::SSUBL2 => "ssubl2",
            Opcode::SSUBW => "ssubw",
            Opcode::SSUBW2 => "ssubw2",
            Opcode::ADDHN => "addhn",
            Opcode::ADDHN2 => "addhn2",
            Opcode::SABAL => "sabal",
            Opcode::SABAL2 => "sabal2",
            Opcode::SUBHN => "subhn",
            Opcode::SUBHN2 => "subhn2",
            Opcode::SABDL => "sabdl",
            Opcode::SABDL2 => "sabdl2",
            Opcode::PMULL => "pmull",
            Opcode::PMULL2 => "pmull2",
            Opcode::UADDL => "uaddl",
            Opcode::UADDL2 => "uaddl2",
            Opcode::UADDW => "uaddw",
            Opcode::UADDW2 => "uaddw2",
            Opcode::USUBL => "usubl",
            Opcode::USUBL2 => "usubl2",
            Opcode::USUBW => "usubw",
            Opcode::USUBW2 => "usubw2",
            Opcode::RADDHN => "raddhn",
            Opcode::RADDHN2 => "raddhn2",
            Opcode::RSUBHN => "rsubhn",
            Opcode::RSUBHN2 => "rsubhn2",
            Opcode::UABAL => "uabal",
            Opcode::UABAL2 => "uabal2",
            Opcode::UABDL => "uabdl",
            Opcode::UABDL2 => "uabdl2",
            Opcode::REV64 => "rev64",
            Opcode::SADDLP => "saddlp",
            Opcode::SUQADD => "suqadd",
            Opcode::CNT => "cnt",
            Opcode::SADALP => "sadalp",
            Opcode::SQABS => "sqabs",
            Opcode::CMLT => "cmlt",
            Opcode::ABS => "abs",
            Opcode::XTN => "xtn",
            Opcode::XTN2 => "xtn2",
            Opcode::SQXTN => "sqxtn",
            Opcode::SQXTN2 => "sqxtn2",
            Opcode::FCVTN => "fcvtn",
            Opcode::FCVTN2 => "fcvtn2",
            Opcode::FCMGT => "fcmgt",
            Opcode::FCVTL => "fcvtl",
            Opcode::FCVTL2 => "fcvtl2",
            Opcode::FCVTNS => "fcvtns",
            Opcode::FCVTPS => "fcvtps",
            Opcode::FCVTMS => "fcvtms",
            Opcode::FCVTAS => "fcvtas",
            Opcode::URECPE => "urecpe",
            Opcode::FRECPE => "frecpe",
            Opcode::UADDLP => "uaddlp",
            Opcode::USQADD => "usqadd",
            Opcode::UADALP => "uadalp",
            Opcode::SQNEG => "sqneg",
            Opcode::CMLE => "cmle",
            Opcode::NEG => "neg",
            Opcode::SQXTUN => "sqxtun",
            Opcode::SQXTUN2 => "sqxtun2",
            Opcode::SHLL => "shll",
            Opcode::SHLL2 => "shll2",
            Opcode::UQXTN => "uqxtn",
            Opcode::UQXTN2 => "uqxtn2",
            Opcode::FCVTXN => "fcvtxn",
            Opcode::FCVTXN2 => "fcvtxn2",
            Opcode::FCVTNU => "fcvtnu",
            Opcode::FCVTMU => "fcvtmu",
            Opcode::FCVTAU => "fcvtau",
            Opcode::EXT => "ext",
            Opcode::UZP1 => "uzp1",
            Opcode::TRN1 => "trn1",
            Opcode::ZIP1 => "zip1",
            Opcode::UZP2 => "uzp2",
            Opcode::TRN2 => "trn2",
            Opcode::ZIP2 => "zip2",
            Opcode::SMOV => "smov",
            Opcode::UMOV => "umov",
            Opcode::SQSHRN2 => "sqshrn2",
            Opcode::SQRSHRN2 => "sqrshrn2",
            Opcode::SQSHRUN2 => "sqshrun2",
            Opcode::UQSHRN2 => "uqshrn2",
            Opcode::UQRSHRN2 => "uqrshrn2",
            Opcode::FMLS => "fmls",
            Opcode::FRECPX => "frecpx",
            Opcode::FRSQRTE => "frsqrte",
            Opcode::FCVTPU => "fcvtpu",
            Opcode::FCMLT => "fcmlt",
            Opcode::FCMLE => "fcmle",
            Opcode::FMAXNMV => "fmaxnmv",
            Opcode::FMINNMV => "fminnmv",
            Opcode::FMAXV => "fmaxv",
            Opcode::FMINV => "fminv",
            Opcode::UADDLV => "uaddlv",
            Opcode::SADDLV => "saddlv",
            Opcode::UMAXV => "umaxv",
            Opcode::SMAXV => "smaxv",
            Opcode::UMINV => "uminv",
            Opcode::SMINV => "sminv",
            Opcode::ADDV => "addv",
            Opcode::FRSQRTS => "frsqrts",
            Opcode::FMINNMP => "fminnmp",
            Opcode::FMLAL2 => "fmlal2",
            Opcode::FMLSL2 => "fmlsl2",
            Opcode::FABD => "fabd",
            Opcode::FACGT => "facgt",
            Opcode::FMINP => "fminp",
            Opcode::FJCVTZS => "fjcvtzs",
            Opcode::URSQRTE => "ursqrte",
            Opcode::PRFM => "prfm",
            Opcode::PRFUM => "prfum",
            Opcode::AESE => "aese",
            Opcode::AESD => "aesd",
            Opcode::AESMC => "aesmc",
            Opcode::AESIMC => "aesimc",
            Opcode::SHA1H => "sha1h",
            Opcode::SHA1SU1 => "sha1su1",
            Opcode::SHA256SU0 => "sha256su0",
            Opcode::SM3TT1A => "sm3tt1a",
            Opcode::SM3TT1B => "sm3tt1b",
            Opcode::SM3TT2A => "sm3tt2a",
            Opcode::SM3TT2B => "sm3tt2b",
            Opcode::SHA512H => "sha512h",
            Opcode::SHA512H2 => "sha512h2",
            Opcode::SHA512SU1 => "sha512su1",
            Opcode::RAX1 => "rax1",
            Opcode::SM3PARTW1 => "sm3partw1",
            Opcode::SM3PARTW2 => "sm3partw2",
            Opcode::SM4EKEY => "sm4ekey",
            Opcode::BCAX => "bcax",
            Opcode::SM3SS1 => "sm3ss1",
            Opcode::SHA512SU0 => "sha512su0",
            Opcode::SM4E => "sm4e",
            Opcode::EOR3 => "eor3",
            Opcode::XAR => "xar",
            Opcode::LDRAA => "ldraa",
            Opcode::LDRAB => "ldrab",
            Opcode::LDAPR => "ldapr",
            Opcode::LDAPRH => "ldaprh",
            Opcode::LDAPRB => "ldaprb",
            Opcode::SWP(ar) => {
                if ar == 0 {
                    "swp"
                } else if ar == 0b01 {
                    "swpl"
                } else if ar == 0b10 {
                    "swpa"
                } else {
                    "swpal"
                }
            }
            Opcode::SWPB(ar) => {
                if ar == 0 {
                    "swpb"
                } else if ar == 0b01 {
                    "swplb"
                } else if ar == 0b10 {
                    "swpab"
                } else {
                    "swpalb"
                }
            }
            Opcode::SWPH(ar) => {
                if ar == 0 {
                    "swph"
                } else if ar == 0b01 {
                    "swplh"
                } else if ar == 0b10 {
                    "swpah"
                } else {
                    "swpalh"
                }
            }
            Opcode::LDADDB(ar) => {
                if ar == 0 {
                    "ldaddb"
                } else if ar == 0b01 {
                    "ldaddlb"
                } else if ar == 0b10 {
                    "ldaddab"
                } else {
                    "ldaddalb"
                }
            }
            Opcode::LDCLRB(ar) => {
                if ar == 0 {
                    "ldclrb"
                } else if ar == 0b01 {
                    "ldclrlb"
                } else if ar == 0b10 {
                    "ldclrab"
                } else {
                    "ldclralb"
                }
            }
            Opcode::LDEORB(ar) => {
                if ar == 0 {
                    "ldeorb"
                } else if ar == 0b01 {
                    "ldeorlb"
                } else if ar == 0b10 {
                    "ldeorab"
                } else {
                    "ldeoralb"
                }
            }
            Opcode::LDSETB(ar) => {
                if ar == 0 {
                    "ldsetb"
                } else if ar == 0b01 {
                    "ldsetlb"
                } else if ar == 0b10 {
                    "ldsetab"
                } else {
                    "ldsetalb"
                }
            }
            Opcode::LDSMAXB(ar) => {
                if ar == 0 {
                    "ldsmaxb"
                } else if ar == 0b01 {
                    "ldsmaxlb"
                } else if ar == 0b10 {
                    "ldsmaxab"
                } else {
                    "ldsmaxalb"
                }
            }
            Opcode::LDSMINB(ar) => {
                if ar == 0 {
                    "ldsminb"
                } else if ar == 0b01 {
                    "ldsminlb"
                } else if ar == 0b10 {
                    "ldsminab"
                } else {
                    "ldsminalb"
                }
            }
            Opcode::LDUMAXB(ar) => {
                if ar == 0 {
                    "ldumaxb"
                } else if ar == 0b01 {
                    "ldumaxlb"
                } else if ar == 0b10 {
                    "ldumaxab"
                } else {
                    "ldumaxalb"
                }
            }
            Opcode::LDUMINB(ar) => {
                if ar == 0 {
                    "lduminb"
                } else if ar == 0b01 {
                    "lduminlb"
                } else if ar == 0b10 {
                    "lduminab"
                } else {
                    "lduminalb"
                }
            }
            Opcode::LDADDH(ar) => {
                if ar == 0 {
                    "ldaddh"
                } else if ar == 0b01 {
                    "ldaddlh"
                } else if ar == 0b10 {
                    "ldaddah"
                } else {
                    "ldaddalh"
                }
            }
            Opcode::LDCLRH(ar) => {
                if ar == 0 {
                    "ldclrh"
                } else if ar == 0b01 {
                    "ldclrlh"
                } else if ar == 0b10 {
                    "ldclrah"
                } else {
                    "ldclralh"
                }
            }
            Opcode::LDEORH(ar) => {
                if ar == 0 {
                    "ldeorh"
                } else if ar == 0b01 {
                    "ldeorlh"
                } else if ar == 0b10 {
                    "ldeorah"
                } else {
                    "ldeoralh"
                }
            }
            Opcode::LDSETH(ar) => {
                if ar == 0 {
                    "ldseth"
                } else if ar == 0b01 {
                    "ldsetlh"
                } else if ar == 0b10 {
                    "ldsetah"
                } else {
                    "ldsetalh"
                }
            }
            Opcode::LDSMAXH(ar) => {
                if ar == 0 {
                    "ldsmaxh"
                } else if ar == 0b01 {
                    "ldsmaxlh"
                } else if ar == 0b10 {
                    "ldsmaxah"
                } else {
                    "ldsmaxalh"
                }
            }
            Opcode::LDSMINH(ar) => {
                if ar == 0 {
                    "ldsminh"
                } else if ar == 0b01 {
                    "ldsminlh"
                } else if ar == 0b10 {
                    "ldsminah"
                } else {
                    "ldsminalh"
                }
            }
            Opcode::LDUMAXH(ar) => {
                if ar == 0 {
                    "ldumaxh"
                } else if ar == 0b01 {
                    "ldumaxlh"
                } else if ar == 0b10 {
                    "ldumaxah"
                } else {
                    "ldumaxalh"
                }
            }
            Opcode::LDUMINH(ar) => {
                if ar == 0 {
                    "lduminh"
                } else if ar == 0b01 {
                    "lduminlh"
                } else if ar == 0b10 {
                    "lduminah"
                } else {
                    "lduminalh"
                }
            }
            Opcode::LDADD(ar) => {
                if ar == 0 {
                    "ldadd"
                } else if ar == 0b01 {
                    "ldaddl"
                } else if ar == 0b10 {
                    "ldadda"
                } else {
                    "ldaddal"
                }
            }
            Opcode::LDCLR(ar) => {
                if ar == 0 {
                    "ldclr"
                } else if ar == 0b01 {
                    "ldclrl"
                } else if ar == 0b10 {
                    "ldclra"
                } else {
                    "ldclral"
                }
            }
            Opcode::LDEOR(ar) => {
                if ar == 0 {
                    "ldeor"
                } else if ar == 0b01 {
                    "ldeorl"
                } else if ar == 0b10 {
                    "ldeora"
                } else {
                    "ldeoral"
                }
            }
            Opcode::LDSET(ar) => {
                if ar == 0 {
                    "ldset"
                } else if ar == 0b01 {
                    "ldsetl"
                } else if ar == 0b10 {
                    "ldseta"
                } else {
                    "ldsetal"
                }
            }
            Opcode::LDSMAX(ar) => {
                if ar == 0 {
                    "ldsmax"
                } else if ar == 0b01 {
                    "ldsmaxl"
                } else if ar == 0b10 {
                    "ldsmaxa"
                } else {
                    "ldsmaxal"
                }
            }
            Opcode::LDSMIN(ar) => {
                if ar == 0 {
                    "ldsmin"
                } else if ar == 0b01 {
                    "ldsminl"
                } else if ar == 0b10 {
                    "ldsmina"
                } else {
                    "ldsminal"
                }
            }
            Opcode::LDUMAX(ar) => {
                if ar == 0 {
                    "ldumax"
                } else if ar == 0b01 {
                    "ldumaxl"
                } else if ar == 0b10 {
                    "ldumaxa"
                } else {
                    "ldumaxal"
                }
            }
            Opcode::LDUMIN(ar) => {
                if ar == 0 {
                    "ldumin"
                } else if ar == 0b01 {
                    "lduminl"
                } else if ar == 0b10 {
                    "ldumina"
                } else {
                    "lduminal"
                }
            }
            Opcode::MOVN => "movn",
            Opcode::MOVZ => "movz",
            Opcode::ORR => "orr",
            Opcode::ORN => "orn",
            Opcode::ANDS => "ands",
            Opcode::ADDS => "adds",
            Opcode::ADD => "add",
            Opcode::SUBS => "subs",
            Opcode::SUB => "sub",
            Opcode::UBFM => "ubfm",
            Opcode::SBFM => "sbfm",
            Opcode::EXTR => "extr",
            Opcode::RET => "ret",
            Opcode::SYS(_) => "sys",
            Opcode::SYSL(_) => "sysl",
            Opcode::CSNEG => "csneg",
            Opcode::CSINC => "csinc",
            Opcode::CSINV => "csinv",
            Opcode::MADD => "madd",
            Opcode::MSUB => "msub",
            Opcode::SMADDL => "smaddl",
            Opcode::SMSUBL => "smsubl",
            Opcode::SMULH => "smulh",
            Opcode::UMADDL => "umaddl",
            Opcode::UMSUBL => "umsubl",
            Opcode::UMULH => "umulh",
            Opcode::UDIV => "udiv",
            Opcode::SDIV => "sdiv",
            Opcode::LSLV => "lslv",
            Opcode::LSRV => "lsrv",
            Opcode::ASRV => "asrv",
            Opcode::RORV => "rorv",
            Opcode::INS => "ins",
            Opcode::DUP => "dup",
            Opcode::LDLARB => "ldlarb",
            Opcode::LDLARH => "ldlarh",
            Opcode::LDLAR => "ldlar",
            Opcode::STLLRB => "stllrb",
            Opcode::STLLRH => "stllrh",
            Opcode::STLLR => "stllr",
            Opcode::TBL => "tbl",
            Opcode::TBX => "tbx",
            Opcode::FCADD => "fcadd",
            Opcode::LDGM => "ldgm",
            Opcode::LDG => "ldm",
            Opcode::STGM => "stgm",
            Opcode::STZGM => "stzgm",
            Opcode::STG => "stg",
            Opcode::STZG => "stzg",
            Opcode::ST2G => "st2g",
            Opcode::STZ2G => "stz2g",
            Opcode::LDAPUR => "ldapur",
            Opcode::LDAPURB => "ldapurb",
            Opcode::LDAPURH => "ldapurh",
            Opcode::LDAPURSB => "ldapursb",
            Opcode::LDAPURSH => "ldapursh",
            Opcode::LDAPURSW => "ldapursw",
            Opcode::STLUR => "stlur",
            Opcode::STLURB => "stlurb",
            Opcode::STLURH => "stlurh",

            Opcode::SETF8 => "setf8",
            Opcode::SETF16 => "setf16",
            Opcode::RMIF => "rmif",

            Opcode::SHA1C => "sha1c",
            Opcode::SHA1P => "sha1p",
            Opcode::SHA1M => "sha1m",
            Opcode::SHA1SU0 => "sha1su0",
            Opcode::SHA256H => "sha256h",
            Opcode::SHA256H2 => "sha256h2",
            Opcode::SHA256SU1 => "sha256su1",

            Opcode::BLRAA => "blraa",
            Opcode::BLRAAZ => "blraaz",
            Opcode::BLRAB => "blrab",
            Opcode::BLRABZ => "blrabz",
            Opcode::BRAA => "braa",
            Opcode::BRAAZ => "braaz",
            Opcode::BRAB => "brab",
            Opcode::BRABZ => "brabz",
            Opcode::ERETAA => "eretaa",
            Opcode::ERETAB => "eretab",
            Opcode::RETAA => "retaa",
            Opcode::RETAB => "retab",

            Opcode::PACIA => "pacia",
            Opcode::PACIB => "pacib",
            Opcode::PACDA => "pacda",
            Opcode::PACDB => "pacdb",
            Opcode::AUTIA => "autia",
            Opcode::AUTIB => "autib",
            Opcode::AUTDA => "autda",
            Opcode::AUTDB => "autdb",
            Opcode::PACIZA => "paciza",
            Opcode::PACIZB => "pacizb",
            Opcode::PACDZA => "pacdza",
            Opcode::PACDZB => "pacdzb",
            Opcode::AUTIZA => "autiza",
            Opcode::AUTIZB => "autizb",
            Opcode::AUTDZA => "autdza",
            Opcode::AUTDZB => "autdzb",
            Opcode::XPACI => "xpaci",
            Opcode::XPACD => "xpacd",

            Opcode::PACGA => "pacga",
            Opcode::GMI => "gmi",
            Opcode::IRG => "irg",
            Opcode::SUBP => "subp",
            Opcode::SUBPS => "subps",

            Opcode::Bcc(cond) => {
                stream.push("b.", Colors::opcode());
                Operand::ConditionCode(cond).tokenize(stream, symbols);
                return;
            }
            Opcode::DMB(option) => {
                return match option {
                    0b0001 => stream.push("dmb oshld", Colors::opcode()),
                    0b0010 => stream.push("dmb oshst", Colors::opcode()),
                    0b0011 => stream.push("dmb osh", Colors::opcode()),
                    0b0101 => stream.push("dmb nshld", Colors::opcode()),
                    0b0110 => stream.push("dmb nshst", Colors::opcode()),
                    0b0111 => stream.push("dmb nsh", Colors::opcode()),
                    0b1001 => stream.push("dmb ishld", Colors::opcode()),
                    0b1010 => stream.push("dmb ishst", Colors::opcode()),
                    0b1011 => stream.push("dmb ish", Colors::opcode()),
                    0b1101 => stream.push("dmb ld", Colors::opcode()),
                    0b1110 => stream.push("dmb st", Colors::opcode()),
                    0b1111 => stream.push("dmb sy", Colors::opcode()),
                    _ => stream.push_owned(format!("dmb #{option}"), Colors::opcode()),
                };
            }
            Opcode::DSB(option) => {
                return match option {
                    0b0001 => stream.push("dsb oshld", Colors::opcode()),
                    0b0010 => stream.push("dsb oshst", Colors::opcode()),
                    0b0011 => stream.push("dsb osh", Colors::opcode()),
                    0b0101 => stream.push("dsb nshld", Colors::opcode()),
                    0b0110 => stream.push("dsb nshst", Colors::opcode()),
                    0b0111 => stream.push("dsb nsh", Colors::opcode()),
                    0b1001 => stream.push("dsb ishld", Colors::opcode()),
                    0b1010 => stream.push("dsb ishst", Colors::opcode()),
                    0b1011 => stream.push("dsb ish", Colors::opcode()),
                    0b1101 => stream.push("dsb ld", Colors::opcode()),
                    0b1110 => stream.push("dsb st", Colors::opcode()),
                    0b1111 => stream.push("dsb sy", Colors::opcode()),
                    _ => stream.push_owned(format!("dsb #{option}"), Colors::opcode()),
                };
            }
            Opcode::HINT => "hint",
            Opcode::CASB(ar) => {
                if ar == 0 {
                    "casb"
                } else if ar == 0b01 {
                    "caslb"
                } else if ar == 0b10 {
                    "casab"
                } else {
                    "casalb"
                }
            }
            Opcode::CASH(ar) => {
                if ar == 0 {
                    "cash"
                } else if ar == 0b01 {
                    "caslh"
                } else if ar == 0b10 {
                    "casah"
                } else {
                    "casalh"
                }
            }
            Opcode::CAS(ar) => {
                if ar == 0 {
                    "cas"
                } else if ar == 0b01 {
                    "casl"
                } else if ar == 0b10 {
                    "casa"
                } else {
                    "casal"
                }
            }
            Opcode::CASP(ar) => {
                if ar == 0 {
                    "casp"
                } else if ar == 0b01 {
                    "caspl"
                } else if ar == 0b10 {
                    "caspa"
                } else {
                    "caspal"
                }
            }
        };

        stream.push(text, Colors::opcode());
    }
}

fn format_register_32(stream: &mut TokenStream, reg: u16) {
    const LOOKUP: &[&str] = &[
        "w0", "w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9", "w10", "w11", "w12", "w13",
        "w14", "w15", "w16", "w17", "w18", "w19", "w20", "w21", "w22", "w23", "w24", "w25", "w26",
        "w27", "w28", "w29", "w30", "w31"
    ];

    stream.push(LOOKUP[reg as usize], Colors::register());
}

fn format_register_64(stream: &mut TokenStream, reg: u16) {
    const LOOKUP: &[&str] = &[
        "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13",
        "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26",
        "x27", "x28", "x29", "x30", "x31"
    ];

    stream.push(LOOKUP[reg as usize], Colors::register());
}

fn format_register_b(stream: &mut TokenStream, reg: u16) {
    const LOOKUP: &[&str] = &[
        "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "b13",
        "b14", "b15", "b16", "b17", "b18", "b19", "b20", "b21", "b22", "b23", "b24", "b25", "b26",
        "b27", "b28", "b29", "b30", "b31"
    ];

    stream.push(LOOKUP[reg as usize], Colors::register());
}

fn format_register_h(stream: &mut TokenStream, reg: u16) {
    const LOOKUP: &[&str] = &[
        "h0", "h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h10", "h11", "h12", "h13",
        "h14", "h15", "h16", "h17", "h18", "h19", "h20", "h21", "h22", "h23", "h24", "h25", "h26",
        "h27", "h28", "h29", "h30", "h31"
    ];

    stream.push(LOOKUP[reg as usize], Colors::register());
}

fn format_register_s(stream: &mut TokenStream, reg: u16) {
    const LOOKUP: &[&str] = &[
        "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "s12", "s13",
        "s14", "s15", "s16", "s17", "s18", "s19", "s20", "s21", "s22", "s23", "s24", "s25", "s26",
        "s27", "s28", "s29", "s30", "s31"
    ];

    stream.push(LOOKUP[reg as usize], Colors::register());
}

fn format_register_d(stream: &mut TokenStream, reg: u16) {
    const LOOKUP: &[&str] = &[
        "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13",
        "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23", "d24", "d25", "d26",
        "d27", "d28", "d29", "d30", "d31"
    ];

    stream.push(LOOKUP[reg as usize], Colors::register());
}

fn format_register_q(stream: &mut TokenStream, reg: u16) {
    const LOOKUP: &[&str] = &[
        "q0", "q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13",
        "q14", "q15", "q16", "q17", "q18", "q19", "q20", "q21", "q22", "q23", "q24", "q25", "q26",
        "q27", "q28", "q29", "q30", "q31"
    ];

    stream.push(LOOKUP[reg as usize], Colors::register());
}

fn format_register_ctrl(stream: &mut TokenStream, reg: u16) {
    const LOOKUP: &[&str] = &[
        "c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c11", "c12", "c13",
        "c14", "c15"
    ];

    stream.push(LOOKUP[reg as usize], Colors::register());
}

/// the way a shift operation is carried out.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ShiftStyle {
    /// left-shift the value, filling in zeroes.
    LSL,
    /// right-shift the value, filling in zeroes.
    LSR,
    /// arithmetic shift right, filling with the top bit of the value (sign-extending).
    ASR,
    /// rotate-right, filling with bits shifted out of the value.
    ROR,
    /// unsigned extend byte to 64-bit word.
    UXTB,
    /// unsigned extend half-word (16-bit) to 64-bit word.
    UXTH,
    /// unsigned extend word (32-bit) to 64-bit word.
    UXTW,
    /// unsigned extend 64-bit word to 64-bit word (functionally equivalent to left-shift).
    UXTX,
    /// signed extend byte to 64-bit word.
    SXTB,
    /// signed extend half-word (16-bit) to 64-bit word.
    SXTH,
    /// signed extend word (32-bit) to 64-bit word.
    SXTW,
    /// signed extend 64-bit word to 64-bit word (functionally equivalent to left-shift).
    SXTX,
}

impl ShiftStyle {
    fn as_str(&self) -> &'static str {
        match self {
            ShiftStyle::LSL => {
                "lsl"
            }
            ShiftStyle::LSR => {
                "lsr"
            }
            ShiftStyle::ASR => {
                "asr"
            }
            ShiftStyle::ROR => { 
               "ror" 
            }
            ShiftStyle::UXTB => {
                "uxtb"
            }
            ShiftStyle::UXTH => {
                "uxth"
            }
            ShiftStyle::UXTW => {
                "uxtw"
            }
            ShiftStyle::UXTX => {
                "uxtx"
            }
            ShiftStyle::SXTB => {
                "sxtb"
            }
            ShiftStyle::SXTH => {
                "sxth"
            }
            ShiftStyle::SXTW => {
                "sxtw"
            }
            ShiftStyle::SXTX => { "sxtx" },
        }
    }
}

/// an operand for an `aarch64` instruction.
///
/// an instruction's `operands` array allows many more combination of `Operand` than are possible
/// in practice; no `aarch64` instruction has multiple `Operand::PCOffset` entries, for example.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(C)]
pub enum Operand {
    /// "no operand". since an instruction's `operands` array is always four entries, this is used
    /// to fill space, if any, after recording an instruction's extant operands.
    Nothing,
    /// a general-purpose register.
    ///
    /// for example, `x5` would be represented as `Operand::Register(SizeCode::X, 5)`. the register
    /// number currently does not extend beyond 31. `Operand::Register(_, 31)` is the zero
    /// register, either `wzr` or `xzr`.
    Register(SizeCode, u16),
    /// a general-purpose register.
    ///
    /// for example, `x5` would be represented as `Operand::Register(SizeCode::X, 5)`. the register
    /// number ranges between 0 and 31. unlike `Operand::Register`, `Operand::Register(_, 31)` is
    /// the stack pointer, either `wsp` or `xsp`.
    RegisterPair(SizeCode, u16),
    /// a SIMD register, used as a scalar element.
    ///
    /// for example, `d3` would be represented as `Operand::SIMDRegister(SIMDSizeCode::D, 3)`. the
    /// register number ranges between 0 and 31.
    SIMDRegister(SIMDSizeCode, u16),
    /// a SIMD register, used as a vector of elements.
    ///
    /// for example, `v10.4h` would be represented as
    /// `Operand::SIMDRegisterElements(SIMDSizeCode::D, 10, SIMDSizeCode::H)`.
    ///
    /// SIMD registers are either `SIMDSizeCode::Q` or `SIMDSizeCode::D` wide, and can have
    /// elements of width `SIMDSizeCode::{B, H, S, D}`. the register number ranges between 0 and 31.
    SIMDRegisterElements(SIMDSizeCode, u16, SIMDSizeCode),
    /// one lane in a SIMD vector of elements.
    ///
    /// for example, `v10.h[3]` would be represented as
    /// `Operand::SIMDRegisterElementsLane(SIMDSizeCode::D, 10, SIMDSizeCode::H, 3)`.
    ///
    /// SIMD registers are either `SIMDSizeCode::Q` or `SIMDSizeCode::D` wide, and can have
    /// elements of width `SIMDSizeCode::{B, H, S, D}`. the register number ranges between 0 and 31.
    /// the lane number can range between 0 and `element.width() / vector.width()`.
    SIMDRegisterElementsLane(SIMDSizeCode, u16, SIMDSizeCode, u8),
    /// multiple lanes in a SIMD vector of elements.
    ///
    /// for example, `v10.4h[3]` would be represented as
    /// `Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, 10, SIMDSizeCode::H, 3, 4)`.
    ///
    /// SIMD registers are either `SIMDSizeCode::Q` or `SIMDSizeCode::D` wide, and can have
    /// elements of width `SIMDSizeCode::{B, H, S, D}`. the register number ranges between 0 and 31.
    /// the lane number can range between 0 and `element.width() / vector.width()`. the number of
    /// elements is, in practice, always 4.
    SIMDRegisterElementsMultipleLane(SIMDSizeCode, u16, SIMDSizeCode, u8, u8),
    /// multiple full SIMD vectors.
    ///
    /// for example, `{v2.2d, v3.2d, v4.2d}` would be represented as
    /// `Operand::SIMDRegisterGroup(SIMDSizeCode::Q, 2, SIMDSizeCode::D, 3)`.
    ///
    /// SIMD registers are either `SIMDSizeCode::Q` or `SIMDSizeCode::D` wide, and can have
    /// elements of width `SIMDSizeCode::{B, H, S, D}`. the register number ranges between 0 and 31.
    /// the number of SIMD registers ranges from 1 to 4.
    SIMDRegisterGroup(SIMDSizeCode, u16, SIMDSizeCode, u8),
    /// a lane in multiple SIMD vectors.
    ///
    /// for example, `{v2.2d, v3.2d, v4.2d}[1]` would be represented as
    /// `Operand::SIMDRegisterGroup(SIMDSizeCode::Q, 2, SIMDSizeCode::D, 3, 1)`.
    ///
    /// SIMD registers are either `SIMDSizeCode::Q` or `SIMDSizeCode::D` wide, and can have
    /// elements of width `SIMDSizeCode::{B, H, S, D}`. the register number ranges between 0 and 31.
    /// the number of SIMD registers ranges from 1 to 4. the lane number can range between 0 and
    /// `element.width() / vector.width()`.
    SIMDRegisterGroupLane(u16, SIMDSizeCode, u8, u8),
    /// a general-purpose register, where register 31 is `sp`, rather than `zr`.
    ///
    /// for example, `x5` would be represented as `Operand::Register(SizeCode::X, 5)`. the register
    /// number currently does not extend beyond 31. `Operand::Register(_, 31)` is the stack pointer
    /// register, either `wsp` or `xsp`.
    RegisterOrSP(SizeCode, u16),
    /// a condition code describing some comparison.
    ///
    /// there are 16 condition codes with number 0 to 15: `eq, hs, mi, vs, hi, ge, gt, al, ne, lo,
    /// pl, vc, ls, lt, le, nv`. the `nv` condition code is identical to `al`, and only has a
    /// different condition code number in encoded instructions.
    ConditionCode(u8),
    /// a pc-relative offset.
    ///
    /// this is the addressing mode for instructions like, `bl $+0x1234`, or `adrp $-0x123000`.
    PCOffset(i64),
    /// a 32-bit immediate.
    ///
    /// this immediate may be signed or unsigned, depending on the instruction in question. if the
    /// immediate is signed, it was sign-extended to 32-bits and that extended value is reported
    /// here.
    Immediate(u32),
    /// a 64-bit immediate.
    ///
    /// this immediate may be signed or unsigned, depending on the instruction in question. if the
    /// immediate is signed, it was sign-extended to 64-bits and that extended value is reported
    /// here.
    Imm64(u64),
    /// a 16-bit immediate.
    ///
    /// this immediate may be signed or unsigned, depending on the instruction in question. if the
    /// immediate is signed, it was sign-extended to 16-bits and that extended value is reported
    /// here.
    Imm16(u16),
    /// a 64-bit floating-point immediate.
    ///
    /// the immediate as recorded in the instruction may be `f16`, `f32`, or `f64`, but is
    /// converted to an `f64` for this operand.
    ImmediateDouble(f64),
    /// an immediate, a base number left-shifted by some amount.
    ///
    /// this is displayed as, for example, `#0x1234, lsl #12`. the base number may be any 16-bit
    /// value, and can be shifted by up to 64 bits.
    ImmShift(u16, u8),
    /// an immediate, a base number left-shifted by some amount, filling with 1's.
    ///
    /// this is displayed as, for example, `#0x1234, msl #12`. the base number may be any 16-bit
    /// value, and can be shifted by up to 64 bits.
    ImmShiftMSL(u16, u8),
    /// a register, shifted by some constant number of bits.
    ///
    /// this is displayed as, for example, `x15, lsl #3`. the operand would be represented by
    /// `Operand::RegShift(ShiftStyle::LSL, 3, Sizecode::X, 15)`.
    RegShift(ShiftStyle, u8, SizeCode, u16),
    /// a memory access, with base register offset by another register shifted by some amount.
    ///
    /// this is displayed as, for example, `[x6, w9, lsl #5]`, and would be represented by
    /// `Operand::RegRegOffset(6, 9, SizeCode::W, ShiftStyle::LSL, 5)`.
    RegRegOffset(u16, u16, SizeCode, ShiftStyle, u8),
    /// a memory access, with a register base address and constant offset, and optional writeback.
    ///
    /// this is displayed as, for example:
    /// * `[x13]` (no writeback, offset of 0)
    ///   - Offset::RegPreIndex(13, 0, false)`
    /// * `[x13, #0x80]` (no writeback, offset of 0x80)
    ///   - Offset::RegPreIndex(13, 0x80, false)`
    /// * `[x13, #0x80]!` (writeback, offset of 0x80)
    ///   - Offset::RegPreIndex(13, 0x80, true)`
    RegPreIndex(u16, i32, bool),
    /// a memory access, with register base address and constant offset, base+offset written back
    /// to the base register after access.
    ///
    /// this is displayed as, for example, `[x5], #-0x40`. the operand would be represented by
    /// `Operand::RegPostIndex(5, -0x40)`.
    RegPostIndex(u16, i32),
    /// a memory access, with register base address and register offset, base+offset written back
    /// to the base register after access.
    ///
    /// this is displayed as, for example, `[x5], x9`. the operand would be represented by
    /// `Operand::RegPostIndex(5, 9)`.
    RegPostIndexReg(u16, u16),
    /// some kind of operation for a `prfm` or `prfum` operation.
    ///
    /// if the operation has a specified name, it will be `{pld,pli,pst}{l1,l2,l3}{keep,strm}`.
    /// otherwise, the prefetch operation will be shown as the underlying integer from the encoded
    /// instruction.
    PrefetchOp(u16),
    /// a system register for an `msr` or `mrs` operation.
    ///
    /// if the register is a standard system register, and `yaxpeax-arm` knows about it, its name
    /// will be used instead. otherwise, this will display as, for example, `sysreg:a6f4`.
    SystemReg(u16),
    /// a control register for an `sys` or `sysl` operation.
    ///
    /// this operand will display as, for example, `cr5`.
    ControlReg(u16),
    /// a selector for a field of the `pstate` control registers.
    ///
    /// `yaxpeax-arm` does not name specific fields of `pstate` yet, so this operand displays as
    /// `pstate.0x50`.
    PstateField(u8),
}

impl ToTokens for Operand {
    fn tokenize(&self, stream: &mut TokenStream, symbols: &Index) {
        match self {
            Operand::Register(size, reg) => {
                if *reg == 31 {
                    match size {
                        SizeCode::X => {
                           stream.push("xzr", Colors::register());
                        },
                        SizeCode::W => {
                            stream.push("wzr", Colors::register());
                        }
                    }
                } else {
                    match size {
                        SizeCode::X => {
                            format_register_64(stream, *reg);
                        },
                        SizeCode::W => {
                            format_register_32(stream, *reg);
                        }
                    }
               }
            },
            Operand::RegisterPair(size, reg) => {
                Operand::Register(*size, *reg).tokenize(stream, symbols);
                stream.push(", ", Colors::expr());
                Operand::Register(*size, *reg + 1).tokenize(stream, symbols);
            },
            Operand::ControlReg(reg) => {
                format_register_ctrl(stream, *reg);
            },
            Operand::PrefetchOp(op) => {
                let ty = (op >> 3) & 0b11;
                let target = (op >> 1) & 0b11;
                let policy = op & 1;

                if ty == 0b11 || target == 0b11 {
                    stream.push_owned(format!("{:#02x}", op), Colors::immediate());
                } else {
                    let op = format!("{}{}{}",
                        ["pld", "pli", "pst"][ty as usize],
                        ["l1", "l2", "l3"][target as usize],
                        ["keep", "strm"][policy as usize],
                    );
                    stream.push_owned(op, Colors::opcode());
                }
            }
            Operand::SystemReg(reg) => {
                // TODO: look up system register names better
                match reg {
                    0x4000 => stream.push("midr_el1", Colors::register()),
                    0x5e82 => stream.push("tpidr_el0", Colors::register()),
                    0x5f02 => stream.push("cntvct_el0", Colors::register()),
                    _ => {
                        // syntax for otherwise-undescribed system register names is described in
                        // MRS or similar, S<op0>_<op1>_<Cn>_<Cm>_<op2>
                        // ... but that "op0" is `o0` + 2.
                        let op2 = reg & 0b111;
                        let CRm = (reg >> 3) & 0b1111;
                        let CRn = (reg >> 7) & 0b1111;
                        let op1 = (reg >> 11) & 0b111;
                        let op0 = ((reg >> 14) & 0b1) + 2;
                        
                        stream.push_owned(format!("s{op0}"), Colors::immediate());
                        stream.push("_", Colors::expr());
                        stream.push_owned(format!("{op1}"), Colors::immediate());
                        stream.push("_", Colors::expr());
                        stream.push_owned(format!("c{CRn}"), Colors::register());
                        stream.push("_", Colors::expr());
                        stream.push_owned(format!("c{CRm}"), Colors::register());
                        stream.push("_", Colors::expr());
                        stream.push_owned(format!("{op2}"), Colors::immediate());
                    },
                }
            }
            Operand::PstateField(reg) => {
                // `MSR (immediate)` writes to the `PSTATE` registers, setting a few bit patterns as
                // selected by `reg`.
                stream.push_owned(format!("pstate.{:#x}", reg), Colors::register());
            }
            Operand::SIMDRegister(size, reg) => {
                match size {
                    SIMDSizeCode::B => format_register_b(stream, *reg),
                    SIMDSizeCode::H => format_register_h(stream, *reg),
                    SIMDSizeCode::S => format_register_s(stream, *reg),
                    SIMDSizeCode::D => format_register_d(stream, *reg),
                    SIMDSizeCode::Q => format_register_q(stream, *reg),
                }
            }
            Operand::SIMDRegisterElements(vector_width, reg, lane_width) => {
                let num_items = vector_width.width() / lane_width.width();
                let op = format!("v{}.{}{}", reg, num_items, lane_width.name());
                stream.push_owned(op, Colors::register());
            }
            Operand::SIMDRegisterElementsLane(_vector_width, reg, lane_width, lane) => {
                let op = format!("v{}.{}[{}]", reg, lane_width.name(), lane);
                stream.push_owned(op, Colors::register());
            }
            Operand::SIMDRegisterElementsMultipleLane(_vector_width, reg, lane_width, lane, num_lanes) => {
                let op = format!("v{}.{}{}[{}]", reg, num_lanes, lane_width.name(), lane);
                stream.push_owned(op, Colors::register());
            }
            Operand::SIMDRegisterGroup(vector_width, reg, lane_width, group_size) => {
                let num_items = vector_width.width() / lane_width.width();
                let format_reg = |stream: &mut TokenStream, reg, elems, lane_size: SIMDSizeCode| {
                    let op = format!("v{}.{}{}", reg, elems, lane_size.name());
                    stream.push_owned(op, Colors::register());
                };

                stream.push("{", Colors::brackets());
                format_reg(stream, *reg, num_items, *lane_width);
                for i in 1..*group_size {
                    stream.push(", ", Colors::expr());
                    format_reg(stream, (*reg + i as u16) % 32, num_items, *lane_width);
                }
                stream.push("}", Colors::brackets());
            }
            Operand::SIMDRegisterGroupLane(reg, lane_width, group_size, lane) => {
                let format_reg = |stream: &mut TokenStream, reg, lane_size: SIMDSizeCode| {
                    let op = format!("v{}.{}", reg, lane_size.name());
                    stream.push_owned(op, Colors::register());
                };

                stream.push("{", Colors::brackets());
                format_reg(stream, *reg, *lane_width);
                for i in 1..*group_size {
                    stream.push(", ", Colors::expr());
                    format_reg(stream, (*reg + i as u16) % 32, *lane_width);
                }
                stream.push("}[", Colors::brackets());
                stream.push_owned(lane.to_string(), Colors::register());
                stream.push("]", Colors::brackets());
            }
            Operand::RegisterOrSP(size, reg) => {
                if *reg == 31 {
                    match size {
                        SizeCode::X => {
                            stream.push("sp", Colors::register());
                        },
                        SizeCode::W => {
                            stream.push("wsp", Colors::register());
                        }
                    }
                } else {
                    match size {
                        SizeCode::X => {
                            format_register_64(stream, *reg);
                        },
                        SizeCode::W => {
                            format_register_32(stream, *reg);
                        }
                    }
                }
            },
            Operand::ConditionCode(cond) => {
                match cond {
                    0b0000 => stream.push("eq", Colors::opcode()),
                    0b0010 => stream.push("hs", Colors::opcode()),
                    0b0100 => stream.push("mi", Colors::opcode()),
                    0b0110 => stream.push("vs", Colors::opcode()),
                    0b1000 => stream.push("hi", Colors::opcode()),
                    0b1010 => stream.push("ge", Colors::opcode()),
                    0b1100 => stream.push("gt", Colors::opcode()),
                    0b1110 => stream.push("al", Colors::opcode()),
                    0b0001 => stream.push("ne", Colors::opcode()),
                    0b0011 => stream.push("lo", Colors::opcode()),
                    0b0101 => stream.push("pl", Colors::opcode()),
                    0b0111 => stream.push("vc", Colors::opcode()),
                    0b1001 => stream.push("ls", Colors::opcode()),
                    0b1011 => stream.push("lt", Colors::opcode()),
                    0b1101 => stream.push("le", Colors::opcode()),
                    // `The Condition code NV exists only to provide a valid disassembly of the
                    // 0b1111 encoding, otherwise its behavior is identical to AL`.
                    0b1111 => stream.push("nv", Colors::opcode()),
                    _ => unreachable!(),
                }
            }
            Operand::PCOffset(offs) => {
                if *offs >= 0 {
                    stream.push("$", Colors::expr());
                    stream.push("+", Colors::immediate());
                } else {
                    stream.push("$", Colors::expr());
                }
                stream.push_owned(decoder::encode_hex(*offs), Colors::immediate());
            }
            Operand::Immediate(imm) => {
                match symbols.get_func_by_addr(*imm as usize) {
                    Some(symbol) => {
                        stream.push("<", colors::BLUE);
                        for token in symbol.name() {
                            stream.push_token(token.clone());
                        }
                        stream.push(">", colors::BLUE);
                    }
                    None => {
                        stream.push("#", Colors::expr());
                        stream.push_owned(decoder::encode_uhex(*imm as u64), Colors::immediate());
                    }
                }
            },
            Operand::ImmediateDouble(d) => {
                if *d as i64 as f64 == *d {
                    let imm = format!("#{d:0.1}");
                    stream.push_owned(imm, Colors::immediate());
                } else {
                    let imm = format!("#{d:0.}");
                    stream.push_owned(imm, Colors::immediate());
                }
            },
            Operand::Imm16(imm) => {
                match symbols.get_func_by_addr(*imm as usize) {
                    Some(symbol) => {
                        stream.push("<", colors::BLUE);
                        for token in symbol.name() {
                            stream.push_token(token.clone());
                        }
                        stream.push(">", colors::BLUE);
                    }
                    None => {
                        stream.push("#", Colors::expr());
                        stream.push_owned(decoder::encode_uhex(*imm as u64), Colors::immediate());
                    }
                }
            },
            Operand::Imm64(imm) => {
                match symbols.get_func_by_addr(*imm as usize) {
                    Some(symbol) => {
                        stream.push("<", colors::BLUE);
                        for token in symbol.name() {
                            stream.push_token(token.clone());
                        }
                        stream.push(">", colors::BLUE);
                    }
                    None => {
                        stream.push("#", Colors::expr());
                        stream.push_owned(decoder::encode_uhex(*imm as u64), Colors::immediate());
                    }
                }
            },
            Operand::ImmShift(imm, shift) => {
                stream.push("#", Colors::expr());
                stream.push_owned(decoder::encode_uhex(*imm as u64), Colors::immediate());

                if *shift != 0 {
                    stream.push(", ", Colors::expr());
                    stream.push("lsl ", Colors::opcode());
                    stream.push("#", Colors::expr());
                    stream.push_owned(shift.to_string(), Colors::immediate());
                }
            },
            Operand::ImmShiftMSL(imm, shift) => {
                stream.push("#", Colors::expr());
                stream.push_owned(decoder::encode_uhex(*imm as u64), Colors::immediate());

                if *shift != 0 {
                    stream.push(", ", Colors::expr());
                    stream.push("msl ", Colors::opcode());
                    stream.push("#", Colors::expr());
                    stream.push_owned(shift.to_string(), Colors::immediate());
                }
            },
            Operand::RegShift(shift_type, amount, size, reg) => {
                match size {
                    SizeCode::X => {
                        if (*shift_type == ShiftStyle::LSL || *shift_type == ShiftStyle::UXTX) && *amount == 0 {
                            Operand::Register(SizeCode::X, *reg).tokenize(stream, symbols);
                        } else if *amount != 0 {
                            Operand::Register(SizeCode::X, *reg).tokenize(stream, symbols);
                            stream.push(", ", Colors::expr());
                            stream.push(shift_type.as_str(), Colors::opcode());
                            stream.push(" ", Colors::expr());
                            stream.push("#", Colors::expr());
                            stream.push_owned(amount.to_string(), Colors::immediate());
                        } else {
                            Operand::Register(SizeCode::X, *reg).tokenize(stream, symbols);
                            stream.push(", ", Colors::expr());
                            stream.push(shift_type.as_str(), Colors::opcode());
                        }
                    },
                    SizeCode::W => {
                        if *shift_type == ShiftStyle::LSL && *amount == 0 {
                            Operand::Register(SizeCode::W, *reg).tokenize(stream, symbols);
                        } else if *amount != 0 {
                            Operand::Register(SizeCode::W, *reg).tokenize(stream, symbols);
                            stream.push(", ", Colors::expr());
                            stream.push(shift_type.as_str(), Colors::opcode());
                            stream.push(" ", Colors::expr());
                            stream.push("#", Colors::expr());
                            stream.push_owned(amount.to_string(), Colors::immediate());
                        } else {
                            Operand::Register(SizeCode::W, *reg).tokenize(stream, symbols);
                            stream.push(", ", Colors::expr());
                            stream.push(shift_type.as_str(), Colors::opcode());
                        }
                    }
                }
            }
            Operand::RegRegOffset(reg, index_reg, index_size, extend, amount) => {
                if extend == &ShiftStyle::LSL && *amount == 0 {
                    stream.push("[", Colors::brackets());
                    Operand::RegisterOrSP(SizeCode::X, *reg).tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    Operand::Register(*index_size, *index_reg).tokenize(stream, symbols);
                    stream.push("]", Colors::brackets());
                } else if ((extend == &ShiftStyle::UXTW && index_size == &SizeCode::W) ||
                           (extend == &ShiftStyle::UXTX && index_size == &SizeCode::X)) &&
                           *amount == 0 {
                    stream.push("[", Colors::brackets());
                    Operand::RegisterOrSP(SizeCode::X, *reg).tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    Operand::Register(*index_size, *index_reg).tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    stream.push(extend.as_str(), Colors::opcode());
                    stream.push("]", Colors::brackets());
                } else {
                    stream.push("[", Colors::brackets());
                    Operand::RegisterOrSP(SizeCode::X, *reg).tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    Operand::Register(*index_size, *index_reg).tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    stream.push(extend.as_str(), Colors::opcode());
                    stream.push(" ", Colors::expr());
                    stream.push("#", Colors::expr());
                    stream.push_owned(amount.to_string(), Colors::immediate());
                    stream.push("]", Colors::brackets());
                }
            }
            Operand::RegPreIndex(reg, offset, wback_bit) => {
                if *offset != 0 || *wback_bit {
                    stream.push("[", Colors::brackets());
                    Operand::RegisterOrSP(SizeCode::X, *reg).tokenize(stream, symbols);
                    stream.push(", ", Colors::expr());
                    stream.push("#", Colors::expr());
                    stream.push_owned(decoder::encode_hex(*offset as i64), Colors::immediate());
                    stream.push("]", Colors::brackets());

                    if *wback_bit {
                        stream.push("!", Colors::expr());
                    }
                } else {
                    stream.push("[", Colors::brackets());
                    Operand::RegisterOrSP(SizeCode::X, *reg).tokenize(stream, symbols);
                    stream.push("]", Colors::brackets());
                }
            }
            Operand::RegPostIndex(reg, offset) => {
                stream.push("[", Colors::brackets());
                Operand::RegisterOrSP(SizeCode::X, *reg).tokenize(stream, symbols);
                stream.push("], ", Colors::expr());
                stream.push("#", Colors::expr());
                stream.push_owned(decoder::encode_hex(*offset as i64), Colors::immediate());
            }
            Operand::RegPostIndexReg(reg, offset_reg) => {
                stream.push("[", Colors::brackets());
                Operand::RegisterOrSP(SizeCode::X, *reg).tokenize(stream, symbols);
                stream.push("], ", Colors::expr());
                format_register_64(stream, *offset_reg);
            }
            Operand::Nothing => panic!("Tried to format nothing opcode"),
        }
    }
}

/// an `aarch64` instruction decoder.
///
/// there are no options or levels of decoding support, yet. as a result, any
/// `armv8::a64::Decoder` will decode as much of the a64 instruction set as is implemented.
///
/// `Decoder` is currently zero-size, but users should not rely on this being the case in the
/// future.
#[derive(Default, PartialEq, Copy, Clone, Eq, Hash, PartialOrd, Ord)]
pub struct Decoder {}

impl Decodable for Decoder {
    type Instruction = Instruction;

    fn decode(&self, reader: &mut decoder::Reader) -> Result<Self::Instruction, Error> {
        let mut inst = Instruction::default();
        read(reader, &mut inst).map_err(|err| Error::new(err, 4))?;
        Ok(inst)
    }

    fn max_width(&self) -> usize {
        4
    }
}

#[inline(always)]
fn read(
    words: &mut Reader,
    inst: &mut Instruction,
) -> Result<(), ErrorKind> {
    let mut word_bytes = [0u8; 4];
    words.next_n(&mut word_bytes).ok_or(ErrorKind::ExhaustedInput)?;
    let word = u32::from_le_bytes(word_bytes);

    inst.operands = [
        Operand::Nothing,
        Operand::Nothing,
        Operand::Nothing,
        Operand::Nothing,
    ];

    #[derive(Copy, Clone, Debug)]
    enum Section {
        Unallocated,
        LoadStore,
        DataProcessingReg,
        DataProcessingSimd,
        DataProcessingSimd2,
        DataProcessingImmediate,
        BranchExceptionSystem,
        SME,
        SVE,
    }

    // from ARM architecture refrence manual for ARMv8, C3.1

    /*
     * ---00---  UNALLOCATED
     * ---100--  Data processing - immediate
     * ---101--  Branch, exception, system instructions
     * ----1-0-  Loads and stores
     * ----101-  Data processing - register
     * ---0111-  Data processing - SIMD and floating point
     * ---1111-  Data processing - SIMD and floating point
     */

    let section_bits = word >> 25;
    static SECTIONS: [Section; 16] = [
        Section::SME,                            // 0000 // SME encodings
        Section::Unallocated,                    // 0001
        Section::SVE,                            // 0010 // SVE encodings
        Section::Unallocated,                    // 0011
        Section::LoadStore,                      // 0100
        Section::DataProcessingReg,              // 0101
        Section::LoadStore,                      // 0110
        Section::DataProcessingSimd,             // 0111
        Section::DataProcessingImmediate,        // 1000
        Section::DataProcessingImmediate,        // 1001
        Section::BranchExceptionSystem,          // 1010
        Section::BranchExceptionSystem,          // 1011
        Section::LoadStore,                      // 1100
        Section::DataProcessingReg,              // 1101
        Section::LoadStore,                      // 1110
        Section::DataProcessingSimd2,            // 1111
    ];
    let section = SECTIONS[(section_bits & 0x0f) as usize];

//        crate::armv8::a64::std::eprintln!("word: {:#x}, bits: {:#b}", word, section_bits & 0xf);

    match section {
        Section::SME => {
            return Err(ErrorKind::IncompleteDecoder);
        },
        Section::SVE => {
            return Err(ErrorKind::IncompleteDecoder);
        }
        Section::DataProcessingSimd |
        Section::DataProcessingSimd2 => {
            let op3 = (word >> 10) & 0b1_1111_1111;
            let op2 = (word >> 19) & 0b1111;
            let op1 = (word >> 23) & 0b11;
            let op0 = (word >> 28) & 0b1111;
            if (op0 & 0b1001) == 0b0000 {
                let op3_low = (op3 & 1) == 1;

                if op1 >= 0b10 {
                    if op3_low {
                        if op1 == 0b11 {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                        // `Advanced SIMD {modified,shift by} immediate`
                        if op2 != 0b0000 {
                            // `shift by`
                            let Rd = word & 0b1_1111;
                            let Rn = (word >> 5) & 0b1_1111;
                            let opcode = (word >> 11) & 0b11111;
                            let immb = (word >> 16) & 0b111;
                            let immh = (word >> 19) & 0b1111;

                            let Q = (word >> 30) & 1;
                            let U = (word >> 29) & 1;

                            if immh == 0b0001 && (opcode == 0b11100 || opcode == 0b11111) {
                                return Err(ErrorKind::InvalidOperand);
                            };

                            if immh >= 0b1000 && Q == 0 {
                                return Err(ErrorKind::InvalidOperand);
                            }

                            let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                            // is shift encoded as `N - imm` or `imm - N`?
                            const OPCODES: &[Result<(Opcode, bool), ErrorKind>] = &[
                                Ok((Opcode::SSHR, true)),         Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::SSRA, true)),         Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::SRSHR, true)),        Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::SRSRA, true)),        Err(ErrorKind::InvalidOpcode),
                                // 0b01000
                                Err(ErrorKind::InvalidOpcode),  Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::SHL, false)),         Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),  Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::SQSHL, false)),       Err(ErrorKind::InvalidOpcode),
                                // 0b10000
                                Ok((Opcode::SHRN, true)),         Ok((Opcode::RSHRN, true)),
                                Ok((Opcode::SQSHRN, true)),       Ok((Opcode::SQRSHRN, true)),
                                Ok((Opcode::SSHLL, false)),       Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),  Err(ErrorKind::InvalidOpcode),
                                // 0b11000
                                Err(ErrorKind::InvalidOpcode),  Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),  Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::SCVTF, true)),        Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),  Ok((Opcode::FCVTZS, true)),
                                // U == 1
                                Ok((Opcode::USHR, true)),         Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::USRA, true)),         Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::URSHR, true)),        Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::URSRA, true)),        Err(ErrorKind::InvalidOpcode),
                                // 0b01000
                                Ok((Opcode::SRI, true)),          Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::SLI, false)),         Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::SQSHLU, false)),      Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::UQSHL, false)),       Err(ErrorKind::InvalidOpcode),
                                // 0b10000
                                Ok((Opcode::SQSHRUN, true)),      Ok((Opcode::SQRSHRUN, true)),
                                Ok((Opcode::UQSHRN, true)),       Ok((Opcode::UQRSHRN, true)),
                                Ok((Opcode::USHLL, false)),       Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),  Err(ErrorKind::InvalidOpcode),
                                // 0b11000
                                Err(ErrorKind::InvalidOpcode),  Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),  Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::UCVTF, true)),        Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),  Ok((Opcode::FCVTZU, true)),
                            ];
                            let (opcode, before) = OPCODES[((U << 5) | opcode) as usize]?;
                            inst.opcode = opcode;
                            let (T, shift) = if before {
                                if immh > 0b0111 {
                                    (SIMDSizeCode::D, 128 - ((immh << 3) | immb))
                                } else if immh > 0b0011 {
                                    (SIMDSizeCode::S, 64 - ((immh << 3) | immb))
                                } else if immh > 0b0001 {
                                    (SIMDSizeCode::H, 32 - ((immh << 3) | immb))
                                } else {
                                    (SIMDSizeCode::B, 16 - ((immh << 3) | immb))
                                }
                            } else {
                                if immh > 0b0111 {
                                    (SIMDSizeCode::D, ((immh << 3) | immb) - 64)
                                } else if immh > 0b0011 {
                                    (SIMDSizeCode::S, ((immh << 3) | immb) - 32)
                                } else if immh > 0b0001 {
                                    (SIMDSizeCode::H, ((immh << 3) | immb) - 16)
                                } else {
                                    (SIMDSizeCode::B, ((immh << 3) | immb) - 8)
                                }
                            };

                            let (Vb, Tb) = if [Opcode::SQSHRUN, Opcode::SQRSHRUN, Opcode::UQSHRN, Opcode::UQRSHRN, Opcode::SHRN, Opcode::RSHRN, Opcode::SQSHRN, Opcode::SQRSHRN].contains(&opcode) {
                                if immh > 0b0111 {
                                    return Err(ErrorKind::InvalidOperand);
                                } else if immh > 0b0011 {
                                    (SIMDSizeCode::Q, SIMDSizeCode::D)
                                } else if immh > 0b0001 {
                                    (SIMDSizeCode::Q, SIMDSizeCode::S)
                                } else {
                                    (SIMDSizeCode::Q, SIMDSizeCode::H)
                                }
                            } else {
                                (datasize, T)
                            };

                            let (datasize, T, shift) = if opcode == Opcode::SSHLL || opcode == Opcode::USHLL {
                                let new_t = match T {
                                    SIMDSizeCode::B => SIMDSizeCode::H,
                                    SIMDSizeCode::H => SIMDSizeCode::S,
                                    _ /* SIMDSizeCode::S */ => SIMDSizeCode::D,
                                };
                                (SIMDSizeCode::Q, new_t, shift)
                            } else {
                                (datasize, T, shift)
                            };

                            if Q == 1 {
                                if inst.opcode == Opcode::RSHRN {
                                    inst.opcode = Opcode::RSHRN2;
                                } else if inst.opcode == Opcode::SQSHRUN {
                                    inst.opcode = Opcode::SQSHRUN2;
                                } else if inst.opcode == Opcode::SQRSHRUN {
                                    inst.opcode = Opcode::SQRSHRUN2;
                                } else if inst.opcode == Opcode::UQSHRN {
                                    inst.opcode = Opcode::UQSHRN2;
                                } else if inst.opcode == Opcode::UQRSHRN {
                                    inst.opcode = Opcode::UQRSHRN2;
                                } else if inst.opcode == Opcode::USHLL {
                                    inst.opcode = Opcode::USHLL2;
                                } else if inst.opcode == Opcode::SSHLL {
                                    inst.opcode = Opcode::SSHLL2;
                                } else if inst.opcode == Opcode::SHRN {
                                    inst.opcode = Opcode::SHRN2;
                                } else if inst.opcode == Opcode::SQSHRN {
                                    inst.opcode = Opcode::SQSHRN2;
                                } else if inst.opcode == Opcode::SQRSHRN {
                                    inst.opcode = Opcode::SQRSHRN2;
                                }
                            };

                            inst.operands = [
                                Operand::SIMDRegisterElements(datasize, Rd as u16, T),
                                Operand::SIMDRegisterElements(Vb, Rn as u16, Tb),
                                Operand::Immediate(shift),
                                Operand::Nothing,
                            ];
                        } else {
                            // `modified`
                            let Rd = word & 0b1_1111;
                            let defgh = (word >> 5) & 0b1_1111;
                            let o2 = (word >> 11) & 0b1;
                            let cmode = (word >> 12) & 0b1111;
                            let abc = (word >> 16) & 0b111;

                            let Q = (word >> 30) & 1;
                            let op = (word >> 29) & 1;

                            let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                            const OPCODES: &[Result<(Opcode, SIMDSizeCode), ErrorKind>] = &[
                                Ok((Opcode::MOVI, SIMDSizeCode::S)), Ok((Opcode::ORR, SIMDSizeCode::S)),
                                Ok((Opcode::MOVI, SIMDSizeCode::S)), Ok((Opcode::ORR, SIMDSizeCode::S)),
                                Ok((Opcode::MOVI, SIMDSizeCode::S)), Ok((Opcode::ORR, SIMDSizeCode::S)),
                                Ok((Opcode::MOVI, SIMDSizeCode::S)), Ok((Opcode::ORR, SIMDSizeCode::S)),
                                Ok((Opcode::MOVI, SIMDSizeCode::H)), Ok((Opcode::ORR, SIMDSizeCode::H)),
                                Ok((Opcode::MOVI, SIMDSizeCode::H)), Ok((Opcode::ORR, SIMDSizeCode::H)),
                                Ok((Opcode::MOVI, SIMDSizeCode::S)), Ok((Opcode::MOVI, SIMDSizeCode::S)),
                                Ok((Opcode::MOVI, SIMDSizeCode::B)), Ok((Opcode::FMOV, SIMDSizeCode::B)),
                                // op == 1
                                Ok((Opcode::MVNI, SIMDSizeCode::S)), Ok((Opcode::BIC, SIMDSizeCode::S)),
                                Ok((Opcode::MVNI, SIMDSizeCode::S)), Ok((Opcode::BIC, SIMDSizeCode::S)),
                                Ok((Opcode::MVNI, SIMDSizeCode::S)), Ok((Opcode::BIC, SIMDSizeCode::S)),
                                Ok((Opcode::MVNI, SIMDSizeCode::S)), Ok((Opcode::BIC, SIMDSizeCode::S)),
                                Ok((Opcode::MVNI, SIMDSizeCode::H)), Ok((Opcode::BIC, SIMDSizeCode::H)),
                                Ok((Opcode::MVNI, SIMDSizeCode::H)), Ok((Opcode::BIC, SIMDSizeCode::H)),
                                Ok((Opcode::MVNI, SIMDSizeCode::S)), Ok((Opcode::MVNI, SIMDSizeCode::S)),
                                Ok((Opcode::MOVI, SIMDSizeCode::D)), Ok((Opcode::FMOV, SIMDSizeCode::B)),
                            ];

                            if cmode == 0b1111 && op == 1 && Q == 0 {
                                return Err(ErrorKind::InvalidOpcode);
                            }

                            let (opc, size) = OPCODES[((op << 4) | cmode) as usize]?;
                            let mut size = size;

                            let imm = if opc == Opcode::FMOV {
                                if op == 0 && o2 == 0 {
                                    size = SIMDSizeCode::S;
                                } else if op == 0 && o2 == 1 {
                                    size = SIMDSizeCode::H;
                                } else if op == 1 && o2 == 0 {
                                    size = SIMDSizeCode::D;
                                }
                                // simd expand
                                let a = (abc >> 2) as u64;
                                let b = ((abc >> 1) & 1) as u64;
                                let c = (abc & 1) as u64;
                                let cdefgh = (c << 5) | defgh as u64;

                                // the expansion is the same (with more exponent bits) for the
                                // various widths
                                let value = (a << 63) |
                                    (((b * 0b111_111_111) ^ 0b100_000_000) << 54) |
                                    (cdefgh << 48);
                                let value = f64::from_bits(value);

                                Operand::ImmediateDouble(value)
                            } else if opc == Opcode::ORR || opc == Opcode::BIC {
                                // abcdefgh
                                let abcdefgh = (abc << 5) | defgh;
                                    let amount = match size {
                                        SIMDSizeCode::H => {
                                            (cmode & 0b0010) << 2
                                        }
                                        SIMDSizeCode::S => {
                                            (cmode & 0b0110) << 2
                                        }
                                        _ => 0,
                                    };
                                Operand::ImmShift(abcdefgh as u16, amount as u8)
                            } else /* if opc == Opcode::MOVI || opc == Opcode::MVNI */ {
                                if cmode == 0b1110 && op == 1 {
                                    let abcdefgh = ((abc << 5) | defgh) as u64;
                                    let mut bytes = [0u8; 8];
                                    for i in 0..8 {
                                        let byte = ((abcdefgh as i8) << (7 - i)) >> 7;
                                        bytes[i] = byte as u8;
                                    }

                                    Operand::Imm64(u64::from_le_bytes(bytes))
                                } else {
                                    let abcdefgh = ((abc << 5) | defgh) as u64;
                                    let imm8 = abcdefgh;
                                    let amount = match size {
                                        SIMDSizeCode::H => {
                                            (cmode & 0b0010) << 2
                                        }
                                        SIMDSizeCode::S => {
                                            (cmode & 0b0110) << 2
                                        }
                                        _ => 0,
                                    };
                                    if cmode & 0b1110 == 0b1100 {
                                        let amount = (cmode & 1) << 3;
                                        Operand::ImmShiftMSL(imm8 as u16, amount as u8 + 8)
                                    } else {
                                        Operand::ImmShift(imm8 as u16, amount as u8)
                                    }
                                }
                            };

                            inst.opcode = opc;
                            if Q == 0 && op == 1 && cmode == 0b1110 && o2 == 0 {
                                inst.operands = [
                                    Operand::SIMDRegister(size, Rd as u16),
                                    imm,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, size),
                                    imm,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                        }
                    } else {
                        // `Advanced SIMD vector x indexed element`
                        let Rd = word & 0b1_1111;
                        let Rn = (word >> 5) & 0b1_1111;
                        let opcode = (word >> 12) & 0b1111;
                        let Rm = (word >> 16) & 0b1111;

                        let Q = (word >> 30) & 1;
                        let U = (word >> 29) & 1;
                        let size = (word >> 22) & 0b11;
                        let L = (word >> 21) & 1;
                        let M = (word >> 20) & 1;
                        let H = (word >> 11) & 1;

                        match (U << 4) | opcode {
                            0b0_0010 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::S, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::D, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = if Q == 1 {
                                    Opcode::SMLAL2
                                } else {
                                    Opcode::SMLAL
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            },
                            0b0_0011 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::S, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::D, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = if Q == 1 {
                                    Opcode::SQDMLAL2
                                } else {
                                    Opcode::SQDMLAL
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            },
                            0b0_0110 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::S, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::D, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = if Q == 1 {
                                    Opcode::SMLSL2
                                } else {
                                    Opcode::SMLSL
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            },
                            0b0_0111 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::S, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::D, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = if Q == 1 {
                                    Opcode::SQDMLSL2
                                } else {
                                    Opcode::SQDMLSL
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            },
                            0b0_1000 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let elemsize = if size == 0b01 {
                                    SIMDSizeCode::H
                                } else {
                                    SIMDSizeCode::S
                                };

                                inst.opcode = Opcode::MUL;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, elemsize),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, elemsize),
                                    Operand::SIMDRegisterElementsLane(datasize, Rm as u16, elemsize, index as u8),
                                    Operand::Nothing,
                                ];
                            },
                            0b0_1010 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::S, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::D, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = if Q == 1 {
                                    Opcode::SMULL2
                                } else {
                                    Opcode::SMULL
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            },
                            0b0_1011 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::S, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::D, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = if Q == 1 {
                                    Opcode::SQDMULL2
                                } else {
                                    Opcode::SQDMULL
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            },
                            0b0_1100 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::H, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::S, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = Opcode::SQDMULH;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            },
                            0b0_1101 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::H, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::S, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = Opcode::SQRDMULH;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            },
                            0b0_1110 => {
                                if size != 0b10 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let Rm = (M << 4) | Rm;
                                let index = (H << 1) | L;

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                                inst.opcode = Opcode::SDOT;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, SIMDSizeCode::B),
                                    Operand::SIMDRegisterElementsMultipleLane(SIMDSizeCode::Q, Rm as u16, SIMDSizeCode::B, index as u8, 4),
                                    Operand::Nothing,
                                ];
                            },
                            0b0_1111 => {
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            0b0_0000 => {
                                // FMLAL
                                if size != 0b10 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                                if (word >> 29 & 1) == 0 {
                                    inst.opcode = Opcode::FMLAL;
                                } else {
                                    inst.opcode = Opcode::FMLAL2;
                                }

                                let index = (H << 2) | (L << 1) | M;

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, SIMDSizeCode::H),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, SIMDSizeCode::H, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b0_0100 => {
                                // FMLSL
                                if size != 0b10 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                                if (word >> 29 & 1) == 0 {
                                    inst.opcode = Opcode::FMLSL;
                                } else {
                                    inst.opcode = Opcode::FMLSL2;
                                }

                                let index = (H << 2) | (L << 1) | M;

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, SIMDSizeCode::H),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, SIMDSizeCode::H, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b0_0001 => {
                                // FMLA (not size=0b01)
                                if size == 0b01 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                inst.opcode = Opcode::FMLA;

                                let sz = size & 1;

                                if sz == 1 && L == 1 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                if sz == 1 && Q == 0 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm, size) = if size == 0b00 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm, SIMDSizeCode::H)
                                } else if size == 0b10 {
                                    // `sz == 0`
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm, SIMDSizeCode::S)
                                } else {
                                    // `sz == 1`
                                    let index = H;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm, SIMDSizeCode::D)
                                };

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, size),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, size),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, size, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b0_0101 => {
                                // FMLS (not size=0b01)
                                if size == 0b01 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                inst.opcode = Opcode::FMLS;

                                let sz = size & 1;

                                if sz == 1 && L == 1 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                if sz == 1 && Q == 0 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm, size) = if size == 0b00 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm, SIMDSizeCode::H)
                                } else if size == 0b10 {
                                    // `sz == 0`
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm, SIMDSizeCode::S)
                                } else {
                                    // `sz == 1`
                                    let index = H;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm, SIMDSizeCode::D)
                                };

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, size),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, size),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, size, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b0_1001 => {
                                // FMUL (not size=0b01)
                                if size == 0b01 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                inst.opcode = Opcode::FMUL;

                                let sz = size & 1;

                                if sz == 1 && L == 1 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                if sz == 1 && Q == 0 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm, size) = if size == 0b00 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm, SIMDSizeCode::H)
                                } else if size == 0b10 {
                                    // `sz == 0`
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm, SIMDSizeCode::S)
                                } else {
                                    // `sz == 1`
                                    let index = H;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm, SIMDSizeCode::D)
                                };

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, size),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, size),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, size, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_0000 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let elemsize = if size == 0b01 {
                                    SIMDSizeCode::H
                                } else {
                                    SIMDSizeCode::S
                                };

                                inst.opcode = Opcode::MLA;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, elemsize),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, elemsize, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_0010 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::S, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::D, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = if Q == 1 {
                                    Opcode::UMLAL2
                                } else {
                                    Opcode::UMLAL
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_0100 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let elemsize = if size == 0b01 {
                                    SIMDSizeCode::H
                                } else {
                                    SIMDSizeCode::S
                                };

                                inst.opcode = Opcode::MLS;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, elemsize),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, elemsize, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_0110 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::S, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::D, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = if Q == 1 {
                                    Opcode::UMLSL2
                                } else {
                                    Opcode::UMLSL
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_1010 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::S, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::D, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = if Q == 1 {
                                    Opcode::UMULL2
                                } else {
                                    Opcode::UMULL
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_1011 => {
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            0b1_1101 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::H, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::S, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = Opcode::SQRDMLAH;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_1110 => {
                                if size != 0b10 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let Rm = (M << 4) | Rm;
                                let index = (H << 1) | L;

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                                inst.opcode = Opcode::UDOT;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, SIMDSizeCode::B),
                                    Operand::SIMDRegisterElementsMultipleLane(SIMDSizeCode::Q, Rm as u16, SIMDSizeCode::B, index as u8, 4),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_1111 => {
                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let (index, Rm) = if size == 0b01 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                };

                                let Tb_vecsize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::H, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else {
                                    (SIMDSizeCode::S, SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                inst.opcode = Opcode::SQRDMLSH;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(Tb_vecsize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_1000 => {
                                if size < 0b10 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                                let index = (H << 2) | (L << 1) | M;

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                                inst.opcode = Opcode::FMLAL;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, SIMDSizeCode::H),
                                    Operand::SIMDRegisterElementsLane(datasize, Rm as u16, SIMDSizeCode::H, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_1100 => {
                                if size < 0b10 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                                let index = (H << 2) | (L << 1) | M;

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                                inst.opcode = Opcode::FMLSL;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, SIMDSizeCode::H),
                                    Operand::SIMDRegisterElementsLane(datasize, Rm as u16, SIMDSizeCode::H, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_1001 => {
                                if size == 0b01 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                let (index, Rm) = if size == 0b00 {
                                    let index = (H << 2) | (L << 1) | M;
                                    (index, Rm)
                                } else if size == 0b10 {
                                    let index = (H << 1) | L;
                                    let Rm = (M << 4) | Rm;
                                    (index, Rm)
                                } else {
                                    if L == 1 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    let Rm = (M << 4) | Rm;
                                    (H, Rm)
                                };

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (Ta, Tb_elemsize, Ts) = if size == 0b00 {
                                    (SIMDSizeCode::H, SIMDSizeCode::H, SIMDSizeCode::H)
                                } else if size == 0b10 {
                                    (SIMDSizeCode::S, SIMDSizeCode::S, SIMDSizeCode::S)
                                } else /* if size == 0b11 */ {
                                    (SIMDSizeCode::D, SIMDSizeCode::D, SIMDSizeCode::D)
                                };

                                inst.opcode = Opcode::FMULX;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, Tb_elemsize),
                                    Operand::SIMDRegisterElementsLane(datasize, Rm as u16, Ts, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                            0b1_0001 |
                            0b1_0011 |
                            0b1_0101 |
                            0b1_0111 => {
                                // opcode == 0xx1
                                // arm v8.3

                                if size == 0b00 || size == 0b11 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                if size == 0b10 && Q == 0 {
                                    return Err(ErrorKind::InvalidOperand);
                                }

                                let index = if size == 0b01 {
                                    (H << 1) | L
                                } else {
                                    H
                                };
                                let Rm = (M << 4) | Rm;

                                let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };
                                let (T, Ts) = if size == 0b01 {
                                    (SIMDSizeCode::H, SIMDSizeCode::H)
                                } else /* if size == 0b10 */ {
                                    (SIMDSizeCode::S, SIMDSizeCode::S)
                                };

                                let rot = (word >> 13) & 0b11;
                                let rot = rot * 90;

                                inst.opcode = Opcode::FCMLA;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, T),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, T),
                                    Operand::SIMDRegisterElementsLane(datasize, Rm as u16, Ts, index as u8),
                                    Operand::Immediate(rot),
                                ];
                            }
                            _ => {
                                return Err(ErrorKind::InvalidOpcode);
                            }
                         }
                    }
                } else {
                    // `Advanced SIMD three same` and below
                    // op1 == 0x
                    if op2 & 0b0100 == 0b0100 {
                        // op2 == x1xx

                        let size = (word >> 22) & 0b11;
                        let Rn = (word >> 5) & 0b1_1111;
                        let Rd = word & 0b1_1111;

                        let Q = (word >> 30) & 1;
                        let U = (word >> 29) & 1;

                        if op3 & 1 == 1 {
                            // `Advanced SIMD three same`
                            let opcode = (word >> 11) & 0b1_1111;
                            let Rm = (word >> 16) & 0b1_1111;

                            const U0_OPCODES: &[Opcode; 0b11000] = &[
                                Opcode::SHADD, Opcode::SQADD, Opcode::SRHADD, Opcode::AND, // or BIC, ORR, ORN
                                Opcode::SHSUB, Opcode::SQSUB, Opcode::CMGT, Opcode::CMGE,
                                Opcode::SSHL, Opcode::SQSHL, Opcode::SRSHL, Opcode::SQRSHL,
                                Opcode::SMAX, Opcode::SMIN, Opcode::SABD, Opcode::SABA,
                                Opcode::ADD, Opcode::CMTST, Opcode::MLA, Opcode::MUL,
                                Opcode::SMAXP, Opcode::SMINP, Opcode::SQDMULH, Opcode::ADDP,
                            ];
                            const U1_OPCODES: &[Opcode; 0b11000] = &[
                                Opcode::UHADD, Opcode::UQADD, Opcode::URHADD, Opcode::EOR, // or BSL, BIT, BIF
                                Opcode::UHSUB, Opcode::UQSUB, Opcode::CMHI, Opcode::CMHS,
                                Opcode::USHL, Opcode::UQSHL, Opcode::URSHL, Opcode::UQRSHL,
                                Opcode::UMAX, Opcode::UMIN, Opcode::UABD, Opcode::UABA,
                                Opcode::SUB, Opcode::CMEQ, Opcode::MLS, Opcode::PMUL,
                                Opcode::UMAXP, Opcode::UMINP, Opcode::SQRDMULH, Opcode::Invalid,
                            ];

                            let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                            const SIZES: [SIMDSizeCode; 4] = [
                                SIMDSizeCode::B,
                                SIMDSizeCode::H,
                                SIMDSizeCode::S,
                                SIMDSizeCode::D,
                            ];

                            let (opc, T) = if U == 0 {
                                if opcode < 0b11000 {
                                    if opcode == 0b00011 {
                                        const OPS: [Opcode; 4] = [
                                            Opcode::AND, Opcode::BIC,
                                            Opcode::ORR, Opcode::ORN,
                                        ];
                                        (OPS[size as usize], SIMDSizeCode::B)
                                    } else {
                                        (U0_OPCODES[opcode as usize], SIZES[size as usize])
                                    }
                                } else {
                                    const U0_HIGH_OPCODES: &[Result<Opcode, ErrorKind>] = &[
                                        Ok(Opcode::FMAXNM), Ok(Opcode::FMINNM),
                                        Ok(Opcode::FMLA), Ok(Opcode::FMLS),
                                        Ok(Opcode::FADD), Ok(Opcode::FSUB),
                                        Ok(Opcode::FMULX), Err(ErrorKind::InvalidOpcode),
                                        Ok(Opcode::FCMEQ), Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                        Ok(Opcode::FMAX), Ok(Opcode::FMIN),
                                        Ok(Opcode::FRECPS), Ok(Opcode::FRSQRTS),
                                    ];
                                    (
                                        U0_HIGH_OPCODES[(((opcode - 0b11000) << 1) | (word >> 23) & 1) as usize]?,
                                        [SIMDSizeCode::S, SIMDSizeCode::D][((word >> 22) & 1) as usize]
                                    )
                                }
                            } else {
                                if opcode < 0b11000 {
                                    if opcode == 0b00011 {
                                        const OPS: [Opcode; 4] = [
                                            Opcode::EOR, Opcode::BSL,
                                            Opcode::BIT, Opcode::BIF,
                                        ];
                                        (OPS[size as usize], SIMDSizeCode::B)
                                    } else if opcode == 0b10111 {
                                        return Err(ErrorKind::InvalidOpcode);
                                    } else {
                                        (U1_OPCODES[opcode as usize], SIZES[size as usize])
                                    }
                                } else {
                                    const U1_HIGH_OPCODES: &[Result<Opcode, ErrorKind>] = &[
                                        Ok(Opcode::FMAXNMP), Ok(Opcode::FMINNMP),
                                        Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                        Ok(Opcode::FADDP), Ok(Opcode::FABD),
                                        Ok(Opcode::FMUL), Err(ErrorKind::InvalidOpcode),
                                        Ok(Opcode::FCMGE), Ok(Opcode::FCMGT),
                                        Ok(Opcode::FACGE), Ok(Opcode::FACGT),
                                        Ok(Opcode::FMAXP), Ok(Opcode::FMINP),
                                        Ok(Opcode::FDIV), Err(ErrorKind::InvalidOpcode),
                                    ];
                                    (
                                        U1_HIGH_OPCODES[(((opcode - 0b11000) << 1) | (word >> 23) & 1) as usize]?,
                                        [SIMDSizeCode::S, SIMDSizeCode::D][((word >> 22) & 1) as usize]
                                    )
                                }
                            };

                            inst.opcode = opc;
                            inst.operands = [
                                Operand::SIMDRegisterElements(datasize, Rd as u16, T),
                                Operand::SIMDRegisterElements(datasize, Rn as u16, T),
                                Operand::SIMDRegisterElements(datasize, Rm as u16, T),
                                Operand::Nothing,
                            ];
                        } else if op2 == 0b1111 && op3 & 0b110000011 == 0b000000010 {
                            // `Advanced SIMD two-register miscellaneous (FP16)`
                            let Rd = (word >> 0) & 0b11111;
                            let Rn = (word >> 5) & 0b11111;
                            let opc = (word >> 12) & 0b11111;
                            let a = (word >> 23) & 1;
                            let u = (word >> 29) & 1;
                            let q = (word >> 30) & 1;

                            let vec_size = if q == 0 {
                                SIMDSizeCode::D
                            } else {
                                SIMDSizeCode::Q
                            };

                            inst.opcode = match (u, a, opc) {
                                (0, 0, 0b11000) => Opcode::FRINTN,
                                (0, 0, 0b11001) => Opcode::FRINTM,
                                (0, 0, 0b11010) => Opcode::FCVTNS,
                                (0, 0, 0b11011) => Opcode::FCVTMS,
                                (0, 0, 0b11100) => Opcode::FCVTAS,
                                (0, 0, 0b11101) => Opcode::SCVTF,
                                (0, 1, 0b01100) => Opcode::FCMGT,
                                (0, 1, 0b01101) => Opcode::FCMEQ,
                                (0, 1, 0b01110) => Opcode::FCMLT,
                                (0, 1, 0b01111) => Opcode::FABS,
                                (0, 1, 0b11000) => Opcode::FRINTP,
                                (0, 1, 0b11001) => Opcode::FRINTZ,
                                (0, 1, 0b11010) => Opcode::FCVTPS,
                                (0, 1, 0b11011) => Opcode::FCVTZS,
                                (0, 1, 0b11101) => Opcode::FRECPE,
                                (1, 0, 0b11000) => Opcode::FRINTA,
                                (1, 0, 0b11001) => Opcode::FRINTX,
                                (1, 0, 0b11010) => Opcode::FCVTNU,
                                (1, 0, 0b11011) => Opcode::FCVTMU,
                                (1, 0, 0b11100) => Opcode::FCVTAU,
                                (1, 0, 0b11101) => Opcode::UCVTF,
                                (1, 1, 0b01100) => Opcode::FCMGE,
                                (1, 1, 0b01101) => Opcode::FCMLE,
                                (1, 1, 0b01111) => Opcode::FNEG,
                                (1, 1, 0b11001) => Opcode::FRINTI,
                                (1, 1, 0b11010) => Opcode::FCVTPU,
                                (1, 1, 0b11011) => Opcode::FCVTZU,
                                (1, 1, 0b11101) => Opcode::FRSQRTE,
                                (1, 1, 0b11111) => Opcode::FSQRT,
                                _ => { return Err(ErrorKind::InvalidOpcode); }
                            };
                            inst.operands = [
                                Operand::SIMDRegisterElements(vec_size, Rd as u16, SIMDSizeCode::H),
                                Operand::SIMDRegisterElements(vec_size, Rn as u16, SIMDSizeCode::H),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                            if [Opcode::FCMGE, Opcode::FCMLE, Opcode::FCMGT, Opcode::FCMEQ, Opcode::FCMLT].contains(&inst.opcode) {
                                inst.operands[2] = Operand::ImmediateDouble(0.0);
                            }
                        } else if op3 & 0b11 == 0b00 {
                            // `Advanced SIMD three different`
                            let opcode = (word >> 12) & 0b1111;
                            let Rm = (word >> 16) & 0b1_1111;

                            const OPCODES: &[Result<Opcode, ErrorKind>; 64] = &[
                                Ok(Opcode::SADDL), Ok(Opcode::SADDL2),
                                Ok(Opcode::SADDW), Ok(Opcode::SADDW2),
                                Ok(Opcode::SSUBL), Ok(Opcode::SSUBL2),
                                Ok(Opcode::SSUBW), Ok(Opcode::SSUBW2),
                                Ok(Opcode::ADDHN), Ok(Opcode::ADDHN2),
                                Ok(Opcode::SABAL), Ok(Opcode::SABAL2),
                                Ok(Opcode::SUBHN), Ok(Opcode::SUBHN2),
                                Ok(Opcode::SABDL), Ok(Opcode::SABDL2),
                                Ok(Opcode::SMLAL), Ok(Opcode::SMLAL2),
                                Ok(Opcode::SQDMLAL), Ok(Opcode::SQDMLAL2),
                                Ok(Opcode::SMLSL), Ok(Opcode::SMLSL2),
                                Ok(Opcode::SQDMLSL), Ok(Opcode::SQDMLSL2),
                                Ok(Opcode::SMULL), Ok(Opcode::SMULL2),
                                Ok(Opcode::SQDMULL), Ok(Opcode::SQDMULL2),
                                Ok(Opcode::PMULL), Ok(Opcode::PMULL2),
                                Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                // u == 1
                                Ok(Opcode::UADDL), Ok(Opcode::UADDL2),
                                Ok(Opcode::UADDW), Ok(Opcode::UADDW2),
                                Ok(Opcode::USUBL), Ok(Opcode::USUBL2),
                                Ok(Opcode::USUBW), Ok(Opcode::USUBW2),
                                Ok(Opcode::RADDHN), Ok(Opcode::RADDHN2),
                                Ok(Opcode::UABAL), Ok(Opcode::UABAL2),
                                Ok(Opcode::RSUBHN), Ok(Opcode::RSUBHN2),
                                Ok(Opcode::UABDL), Ok(Opcode::UABDL2),
                                Ok(Opcode::UMLAL), Ok(Opcode::UMLAL2),
                                Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                Ok(Opcode::UMLSL), Ok(Opcode::UMLSL2),
                                Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                Ok(Opcode::UMULL), Ok(Opcode::UMULL2),
                                Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                            ];

                            let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                            let (Ta, Tb) = [
                                (SIMDSizeCode::H, SIMDSizeCode::B),
                                (SIMDSizeCode::S, SIMDSizeCode::H),
                                (SIMDSizeCode::D, SIMDSizeCode::S),
                                (SIMDSizeCode::Q, SIMDSizeCode::D),
                            ][size as usize];

                            inst.opcode = OPCODES[((U << 5) | (opcode << 1) | Q) as usize]?;

                            if size == 0b11 && inst.opcode != Opcode::PMULL && inst.opcode != Opcode::PMULL2 {
                                return Err(ErrorKind::InvalidOperand);
                            }

                            if opcode == 0b001 || opcode == 0b011 {
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn as u16, Ta),
                                    Operand::SIMDRegisterElements(datasize, Rm as u16, Tb),
                                    Operand::Nothing,
                                ];
                            } else if opcode == 0b100 || opcode == 0b110 {
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize, Rd as u16, Tb),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn as u16, Ta),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm as u16, Ta),
                                    Operand::Nothing,
                                ];
                            } else if opcode == 0b1110 && U == 0 {
                                if size != 0b00 && size != 0b11 {
                                    return Err(ErrorKind::InvalidOperand);
                                }
                                let ta_size = SIMDSizeCode::Q;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(ta_size, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, Tb),
                                    Operand::SIMDRegisterElements(datasize, Rm as u16, Tb),
                                    Operand::Nothing,
                                ];
                            } else {
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, Ta),
                                    Operand::SIMDRegisterElements(datasize, Rn as u16, Tb),
                                    Operand::SIMDRegisterElements(datasize, Rm as u16, Tb),
                                    Operand::Nothing,
                                ];
                            }
                        } else if op3 & 0b11_00000_11 == 0b00_00000_10 {
                            // op3 == 00xxxx10:
                            // could be either `Advanced SIMD across lanes` or `Advanced SIMD
                            // two-register miscellaneous` or `Cryptographic AES`

                            let opcode = (word >> 12) & 0b1_1111;
                            let size = (word >> 22) & 0b11;
                            let q = (word >> 30) & 1;

                            type OperandSizeTable = [Result<(SIMDSizeCode, SIMDSizeCode, SIMDSizeCode, SIMDSizeCode), ErrorKind>; 8];

                            use crate::armv8::a64::SIMDSizeCode::*;

                            const TABLE_A: &OperandSizeTable = &[
                                Ok((D, B, D, B)), Ok((Q, B, Q, B)),
                                Ok((D, H, D, H)), Ok((Q, H, Q, H)),
                                Ok((D, S, D, S)), Ok((Q, S, Q, S)),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                            ];

                            const TABLE_B: &OperandSizeTable = &[
                                Ok((D, B, D, B)), Ok((Q, B, Q, B)),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                            ];

                            const TABLE_C: &OperandSizeTable = &[
                                Ok((D, H, D, B)), Ok((Q, H, Q, B)),
                                Ok((D, S, D, H)), Ok((Q, S, Q, H)),
                                Ok((D, D, D, S)), Ok((Q, D, Q, S)),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                            ];

                            const TABLE_E: &OperandSizeTable = &[
                                Ok((D, B, Q, H)), Ok((Q, B, Q, H)),
                                Ok((D, H, Q, S)), Ok((Q, H, Q, S)),
                                Ok((D, S, Q, D)), Ok((Q, S, Q, D)),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                            ];

                            const TABLE_F: &OperandSizeTable = &[
                                Ok((D, H, Q, S)), Ok((Q, H, Q, S)),
                                Ok((D, S, Q, D)), Ok((Q, S, Q, D)),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                            ];

                            const TABLE_G: &OperandSizeTable = &[
                                Ok((Q, S, D, H)), Ok((Q, S, Q, H)),
                                Ok((Q, D, D, S)), Ok((Q, D, Q, S)),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                            ];

                            const TABLE_H: &OperandSizeTable = &[
                                Ok((D, S, D, S)), Ok((Q, S, Q, S)),
                                Err(ErrorKind::InvalidOperand), Ok((Q, D, Q, D)),
                                Ok((D, S, D, S)), Ok((Q, S, Q, S)),
                                Err(ErrorKind::InvalidOperand), Ok((Q, D, Q, D)),
                            ];

                            const TABLE_I: &OperandSizeTable = &[
                                Ok((D, S, D, S)), Ok((Q, S, Q, S)),
                                Err(ErrorKind::InvalidOperand), Ok((Q, D, Q, D)),
                                Ok((D, S, D, S)), Ok((Q, S, Q, S)),
                                Err(ErrorKind::InvalidOperand), Ok((Q, D, Q, D)),
                            ];

                            const TABLE_J: &OperandSizeTable = &[
                                Ok((Q, H, D, B)), Ok((Q, H, Q, B)),
                                Ok((Q, S, D, H)), Ok((Q, S, Q, H)),
                                Ok((Q, D, D, S)), Ok((Q, D, Q, S)),
                                Err(ErrorKind::InvalidOperand), Err(ErrorKind::InvalidOperand),
                            ];

                            const TABLE_K: &OperandSizeTable = &[
                                Ok((D, B, D, B)), Ok((Q, B, Q, B)),
                                Ok((D, H, D, H)), Ok((Q, H, Q, H)),
                                Ok((D, S, D, S)), Ok((Q, S, Q, S)),
                                Err(ErrorKind::InvalidOperand), Ok((Q, D, Q, D)),
                            ];

                            if op2 & 0b0111 == 0b0100 {
                                // `Advanced SIMD two-register miscellaneous`
                                const OPCODES_U0_LOW: &[Result<(Opcode, &OperandSizeTable), ErrorKind>; 20] = &[
                                    Ok((Opcode::REV64, TABLE_A)),
                                    Ok((Opcode::REV16, TABLE_B)),
                                    Ok((Opcode::SADDLP, TABLE_C)),
                                    Ok((Opcode::SUQADD, TABLE_K)),
                                    Ok((Opcode::CLS, TABLE_A)),
                                    Ok((Opcode::CNT, TABLE_B)),
                                    Ok((Opcode::SADALP, TABLE_C)),
                                    Ok((Opcode::SQABS, TABLE_K)),
                                    Ok((Opcode::CMGT, TABLE_K)),
                                    Ok((Opcode::CMEQ, TABLE_K)),
                                    Ok((Opcode::CMLT, TABLE_K)),
                                    Ok((Opcode::ABS, TABLE_K)),
                                    // 0b01100, all four size=1x
                                    Ok((Opcode::FCMGT, TABLE_I)),
                                    Ok((Opcode::FCMEQ, TABLE_I)),
                                    Ok((Opcode::FCMLT, TABLE_I)),
                                    Ok((Opcode::FABS, TABLE_I)),
                                    // 0b10000
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::XTN, TABLE_E)),
                                    Err(ErrorKind::InvalidOpcode),
                                    // 0b10100
                                ];
                                // index by low 3 of opcode | upper bit of size
                                const OPCODES_U0_HIGH: &[Result<(Opcode, &OperandSizeTable), ErrorKind>] = &[
                                    Ok((Opcode::SQXTN, TABLE_E)),
                                    Ok((Opcode::SQXTN, TABLE_E)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FCVTN, TABLE_F)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FCVTL, TABLE_G)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FRINTN, TABLE_H)),
                                    Ok((Opcode::FRINTP, TABLE_H)),
                                    Ok((Opcode::FRINTM, TABLE_H)),
                                    Ok((Opcode::FRINTZ, TABLE_H)),
                                    Ok((Opcode::FCVTNS, TABLE_H)),
                                    Ok((Opcode::FCVTPS, TABLE_H)),
                                    Ok((Opcode::FCVTMS, TABLE_H)),
                                    Ok((Opcode::FCVTZS, TABLE_H)),
                                    Ok((Opcode::FCVTAS, TABLE_H)),
                                    Ok((Opcode::URECPE, TABLE_H)),
                                    Ok((Opcode::SCVTF, TABLE_H)),
                                    Ok((Opcode::FRECPE, TABLE_H)),
                                    Ok((Opcode::FRINT32Z, TABLE_H)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FRINT64Z, TABLE_H)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    // u == 1, op == 0b00000
                                ];

                                // `Advanced SIMD two-register miscellaneous`, U == 1
                                const OPCODES_U1_LOW: &[Result<(Opcode, &OperandSizeTable), ErrorKind>; 20] = &[
                                    Ok((Opcode::REV32, TABLE_A)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::UADDLP, TABLE_C)),
                                    Ok((Opcode::USQADD, TABLE_K)),
                                    Ok((Opcode::CLZ, TABLE_A)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::UADALP, TABLE_C)),
                                    Ok((Opcode::SQNEG, TABLE_K)),
                                    Ok((Opcode::CMGE, TABLE_K)),
                                    Ok((Opcode::CMLE, TABLE_K)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::NEG, TABLE_K)),
                                    // 0b01100, all four size=1x
                                    Ok((Opcode::FCMGE, TABLE_I)),
                                    Ok((Opcode::FCMLE, TABLE_I)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FNEG, TABLE_I)),
                                    // 0b10000
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::SQXTUN, TABLE_E)),
                                    Ok((Opcode::SHLL, TABLE_J)),
                                    // 0b10100
                                ];
                                // index by low 3 of opcode | upper bit of size
                                const OPCODES_U1_HIGH: &[Result<(Opcode, &OperandSizeTable), ErrorKind>] = &[
                                    Ok((Opcode::UQXTN, TABLE_E)),
                                    Ok((Opcode::UQXTN, TABLE_E)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FCVTXN, TABLE_F)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FRINTA, TABLE_H)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FRINTX, TABLE_H)),
                                    Ok((Opcode::FRINTI, TABLE_H)),
                                    Ok((Opcode::FCVTNU, TABLE_H)),
                                    Ok((Opcode::FCVTPU, TABLE_H)),
                                    Ok((Opcode::FCVTMU, TABLE_H)),
                                    Ok((Opcode::FCVTZU, TABLE_H)),
                                    Ok((Opcode::FCVTAU, TABLE_H)),
                                    Ok((Opcode::URSQRTE, TABLE_H)),
                                    Ok((Opcode::UCVTF, TABLE_H)),
                                    Ok((Opcode::FRSQRTE, TABLE_H)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FSQRT, TABLE_H)),
                                    // u == 1, op == 0b00000
                                ];

                                let (opc, (datasize_a, elemsize_a, datasize_b, elemsize_b)) = if opcode == 0b00101 && U == 1 {
                                    let vecsize = if q == 0 {
                                        SIMDSizeCode::D
                                    } else {
                                        SIMDSizeCode::Q
                                    };
                                    if size == 0b00 {
                                        (Opcode::NOT, (vecsize, SIMDSizeCode::B, vecsize, SIMDSizeCode::B))
                                    } else if size == 0b01 {
                                        (Opcode::RBIT, (vecsize, SIMDSizeCode::B, vecsize, SIMDSizeCode::B))
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                } else if opcode < 0b10100 {
                                    let (opc, table) = if U == 0 {
                                        OPCODES_U0_LOW[opcode as usize]?
                                    } else {
                                        OPCODES_U1_LOW[opcode as usize]?
                                    };
                                    (opc, table[((size << 1) | q) as usize]?)
                                } else {
                                    let (opc, table) = if U == 0 {
                                        OPCODES_U0_HIGH[(((opcode - 0b10100) << 1) | (size >> 1)) as usize]?
                                    } else {
                                        OPCODES_U1_HIGH[(((opcode - 0b10100) << 1) | (size >> 1)) as usize]?
                                    };
                                    (opc, table[((size << 1) | q) as usize]?)
                                };

                                inst.opcode = opc;

                                if q != 0 {
                                    if opc == Opcode::FCVTL {
                                        inst.opcode = Opcode::FCVTL2;
                                    } else if opc == Opcode::XTN {
                                        inst.opcode = Opcode::XTN2;
                                    } else if opc == Opcode::SQXTN {
                                        inst.opcode = Opcode::SQXTN2;
                                    } else if opc == Opcode::SQXTUN {
                                        inst.opcode = Opcode::SQXTUN2;
                                    } else if opc == Opcode::UQXTN {
                                        inst.opcode = Opcode::UQXTN2;
                                    } else if opc == Opcode::FCVTN {
                                        inst.opcode = Opcode::FCVTN2;
                                    } else if opc == Opcode::FCVTXN {
                                        inst.opcode = Opcode::FCVTXN2;
                                    }
                                }
                                inst.operands = [
                                    Operand::SIMDRegisterElements(datasize_a, Rd as u16, elemsize_a),
                                    Operand::SIMDRegisterElements(datasize_b, Rn as u16, elemsize_b),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                                if [Opcode::CMGE, Opcode::CMLE, Opcode::CMGT, Opcode::CMEQ, Opcode::CMLT].contains(&inst.opcode) {
                                    inst.operands[2] = Operand::Imm64(0);
                                } else if [Opcode::FCMGE, Opcode::FCMLE, Opcode::FCMGT, Opcode::FCMEQ, Opcode::FCMLT].contains(&inst.opcode) {
                                    inst.operands[2] = Operand::ImmediateDouble(0.0);
                                } else if inst.opcode == Opcode::SHLL {
                                    if q == 1 {
                                        inst.opcode = Opcode::SHLL2;
                                    }
                                    let sz = (word >> 22) & 0b11;
                                    if sz == 0b11 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    inst.operands[2] = Operand::Imm64((8 << sz) as u64);
                                }
                            } else if op2 & 0b0111 == 0b0110 {
                                // `Advanced SIMD across lanes`

                                let opcode = (word >> 12) & 0b1_1111;
                                let size = (word >> 22) & 0b11;
                                let u = (word >> 29) & 1 == 1;
                                let q = (word >> 30) & 1 == 1;

                                let opc_bits = opcode;
                                let size_bits = size;

                                let (size, opcode) = if opcode == 0b01100 {
                                    let opcode = if size == 0b00 {
                                        Opcode::FMAXNMV
                                    } else if size >= 0b10 {
                                        Opcode::FMINNMV
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    };

                                    let size = if !u {
                                        SIMDSizeCode::H
                                    } else if size & 1 == 0 {
                                        if !q {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                        SIMDSizeCode::S
                                    } else {
                                        // `sz=1` encodings are reserved
                                        return Err(ErrorKind::InvalidOperand);
                                    };
                                    (size, opcode)
                                } else if opcode == 0b01111 {
                                    let opcode = if size == 0b00 {
                                        Opcode::FMAXV
                                    } else if size >= 0b10 {
                                        Opcode::FMINV
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    };

                                    let size = if !u {
                                        SIMDSizeCode::H
                                    } else if size & 1 == 0 {
                                        if !q {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                        SIMDSizeCode::S
                                    } else {
                                        // `sz=1` encodings are reserved
                                        return Err(ErrorKind::InvalidOperand);
                                    };
                                    (size, opcode)
                                } else {
                                    let size = if size == 0b00 {
                                        SIMDSizeCode::B
                                    } else if size == 0b01 {
                                        SIMDSizeCode::H
                                    } else if size == 0b10 {
                                        if !q {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                        SIMDSizeCode::S
                                    } else {
                                        return Err(ErrorKind::InvalidOperand);
                                    };

                                    let opcode = if opcode == 0b00011 {
                                        if u {
                                            Opcode::UADDLV
                                        } else {
                                            Opcode::SADDLV
                                        }
                                    } else if opcode == 0b01010 {
                                        if u {
                                            Opcode::UMAXV
                                        } else {
                                            Opcode::SMAXV
                                        }
                                    } else if opcode == 0b11010 {
                                        if u {
                                            Opcode::UMINV
                                        } else {
                                            Opcode::SMINV
                                        }
                                    } else if opcode == 0b11011 {
                                        if u {
                                            return Err(ErrorKind::InvalidOpcode);
                                        } else {
                                            Opcode::ADDV
                                        }
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    };
                                    (size, opcode)
                                };

                                let scalar_size = if opc_bits == 0b00011 {
                                    if size_bits == 0b00 {
                                        SIMDSizeCode::H
                                    } else if size_bits == 0b01 {
                                        SIMDSizeCode::S
                                    } else {
                                        SIMDSizeCode::D
                                    }
                                } else {
                                    size
                                };

                                let vec_size = if !q {
                                    SIMDSizeCode::D
                                } else {
                                    SIMDSizeCode::Q
                                };

                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::SIMDRegister(scalar_size, Rd as u16),
                                    Operand::SIMDRegisterElements(vec_size, Rn as u16, size),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else if op2 & 0b0111 == 0b0101 && op0 == 0b0100 {
                                // `Cryptographic AES`
                                let Rd = word & 0b11111;
                                let Rn = (word >> 5) & 0b11111;
                                let opcode = (word >> 12) & 0b11111;
                                let size = (word >> 22) & 0b11;

                                if size != 0b00 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                let opcode = match opcode {
                                    0b00100 => Opcode::AESE,
                                    0b00101 => Opcode::AESD,
                                    0b00110 => Opcode::AESMC,
                                    0b00111 => Opcode::AESIMC,
                                    _ => {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                };
                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, SIMDSizeCode::B),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn as u16, SIMDSizeCode::B),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                        } else {
                            // op3 == 1xxxxx10 or x1xxxx10: both unallocated.
                            return Err(ErrorKind::InvalidOpcode);
                        }
                    } else {
                        // op2 == x0xx
                        if op3 & 0b000100001 == 0b000100001 {
                            // `Advanced SIMD three same extra``
                            let Rd = (word >> 0) & 0b11111;
                            let Rn = (word >> 5) & 0b11111;
                            let opcode = (word >> 11) & 0b1111;
                            let Rm = (word >> 16) & 0b11111;
                            let sz = (word >> 22) & 0b11;
                            let u = (word >> 29) & 1;
                            let q = (word >> 30) & 1;

                            if opcode == 0b0010 {
                                // sdot/udot
                                inst.opcode = if u == 0 {
                                    Opcode::SDOT
                                } else {
                                    Opcode::UDOT
                                };
                                let vec_size = if q == 0 {
                                    SIMDSizeCode::D
                                } else {
                                    SIMDSizeCode::Q
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(vec_size, Rd as u16, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(vec_size, Rn as u16, SIMDSizeCode::B),
                                    Operand::SIMDRegisterElements(vec_size, Rm as u16, SIMDSizeCode::B),
                                    Operand::Nothing,
                                ];
                            } else {
                                if u == 0 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                                if opcode & 0b1110 == 0b0000 {
                                    // sqrdml{a,s}h
                                    inst.opcode = if opcode == 0b0000 {
                                        Opcode::SQRDMLAH
                                    } else {
                                        Opcode::SQRDMLSH
                                    };
                                    let vec_size = if q == 0 {
                                        SIMDSizeCode::D
                                    } else {
                                        SIMDSizeCode::Q
                                    };
                                    let elem_size = if sz == 0b01 {
                                        SIMDSizeCode::H
                                    } else if sz == 0b10 {
                                        SIMDSizeCode::S
                                    } else {
                                        return Err(ErrorKind::InvalidOperand);
                                    };
                                    inst.operands = [
                                        Operand::SIMDRegisterElements(vec_size, Rd as u16, elem_size),
                                        Operand::SIMDRegisterElements(vec_size, Rn as u16, elem_size),
                                        Operand::SIMDRegisterElements(vec_size, Rm as u16, elem_size),
                                        Operand::Nothing,
                                    ];
                                } else if opcode & 0b1100 == 0b1000 {
                                    // fcmla
                                    let rot = (word >> 11) & 0b11;

                                    let vec_size = if q == 0 {
                                        SIMDSizeCode::D
                                    } else {
                                        SIMDSizeCode::Q
                                    };
                                    let elem_size = if sz == 0b01 {
                                        SIMDSizeCode::H
                                    } else if sz == 0b10 {
                                        SIMDSizeCode::S
                                    } else {
                                        if q == 0 {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                        SIMDSizeCode::D
                                    };
                                    inst.opcode = Opcode::FCMLA;
                                    inst.operands = [
                                        Operand::SIMDRegisterElements(vec_size, Rd as u16, elem_size),
                                        Operand::SIMDRegisterElements(vec_size, Rn as u16, elem_size),
                                        Operand::SIMDRegisterElements(vec_size, Rm as u16, elem_size),
                                        Operand::Imm16(90 * (rot as u16)),
                                    ];
                                } else if opcode & 0b1101 == 0b1100 {
                                    // fcadd
                                    let rot = (word >> 12) & 1;

                                    let vec_size = if q == 0 {
                                        SIMDSizeCode::D
                                    } else {
                                        SIMDSizeCode::Q
                                    };
                                    let elem_size = if sz == 0b01 {
                                        SIMDSizeCode::H
                                    } else if sz == 0b10 {
                                        SIMDSizeCode::S
                                    } else {
                                        if q == 0 {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                        SIMDSizeCode::D
                                    };
                                    inst.opcode = Opcode::FCADD;
                                    inst.operands = [
                                        Operand::SIMDRegisterElements(vec_size, Rd as u16, elem_size),
                                        Operand::SIMDRegisterElements(vec_size, Rn as u16, elem_size),
                                        Operand::SIMDRegisterElements(vec_size, Rm as u16, elem_size),
                                        Operand::Imm16(90 + 180 * (rot as u16)),
                                    ];
                                } else {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            }
                        } else if op2 & 0b1100 == 0b1000 && op3 & 0b000110001 == 0b000000001 {
                            // `Advanced SIMD three same (FP16)`
                            let Rd = (word >> 0) & 0b11111;
                            let Rn = (word >> 5) & 0b11111;
                            let opc = (word >> 11) & 0b111;
                            let Rm = (word >> 16) & 0b11111;
                            let a = (word >> 23) & 1;
                            let u = (word >> 29) & 1;
                            let q = (word >> 30) & 1;

                            const OPCODES: &[Result<Opcode, ErrorKind>; 32] = &[
                                Ok(Opcode::FMAXNM), Ok(Opcode::FMLA),
                                Ok(Opcode::FADD), Ok(Opcode::FMULX),
                                Ok(Opcode::FCMEQ), Err(ErrorKind::InvalidOpcode),
                                Ok(Opcode::FMAX), Ok(Opcode::FRECPS),
                                Ok(Opcode::FMINNM), Ok(Opcode::FMLS),
                                Ok(Opcode::FSUB), Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                Ok(Opcode::FMIN), Ok(Opcode::FRSQRTS),
                                Ok(Opcode::FMAXNMP), Err(ErrorKind::InvalidOpcode),
                                Ok(Opcode::FADDP), Ok(Opcode::FMUL),
                                Ok(Opcode::FCMGE), Ok(Opcode::FACGE),
                                Ok(Opcode::FMAXP), Ok(Opcode::FDIV),
                                Ok(Opcode::FMINNMP), Err(ErrorKind::InvalidOpcode),
                                Ok(Opcode::FABD), Err(ErrorKind::InvalidOpcode),
                                Ok(Opcode::FCMGT), Ok(Opcode::FACGT),
                                Ok(Opcode::FMINP), Err(ErrorKind::InvalidOpcode),
                            ];

                            let vec_size = if q == 0 {
                                SIMDSizeCode::D
                            } else {
                                SIMDSizeCode::Q
                            };

                            let uaopc = (u << 4) | (a << 3) | opc;

                            inst.opcode = OPCODES[uaopc as usize]?;
                            inst.operands = [
                                Operand::SIMDRegisterElements(vec_size, Rd as u16, SIMDSizeCode::H),
                                Operand::SIMDRegisterElements(vec_size, Rn as u16, SIMDSizeCode::H),
                                Operand::SIMDRegisterElements(vec_size, Rm as u16, SIMDSizeCode::H),
                                Operand::Nothing,
                            ];
                        } else if op3 & 0b000100001 == 0b000000001 {
                            // `Advanced SIMD copy`
                            let Rd = (word >> 0) & 0b11111;
                            let Rn = (word >> 5) & 0b11111;
                            let imm4 = (word >> 11) & 0b1111;
                            let imm5 = (word >> 16) & 0b11111;
                            let op = (word >> 29) & 1;
                            let Q = (word >> 30) & 1 == 1;

                            if imm5 & 0b01111 == 0b00000 {
                                return Err(ErrorKind::InvalidOpcode);
                            }

                            if op == 1 {
                                if Q {
                                    // INS (element)
                                    inst.opcode = Opcode::INS;

                                    let (size, index1, index2) = match imm5.trailing_zeros() {
                                        0 => {
                                            (SIMDSizeCode::B, imm5 >> 1, imm4)
                                        }
                                        1 => {
                                            (SIMDSizeCode::H, imm5 >> 2, imm4 >> 1)
                                        }
                                        2 => {
                                            (SIMDSizeCode::S, imm5 >> 3, imm4 >> 2)
                                        }
                                        _ => {
                                            (SIMDSizeCode::D, imm5 >> 4, imm4 >> 3)
                                        }
                                    };
                                    inst.operands = [
                                        Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rd as u16, size, index1 as u8),
                                        Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rn as u16, size, index2 as u8),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            } else {
                                let vector_size = if Q {
                                    SIMDSizeCode::Q
                                } else {
                                    SIMDSizeCode::D
                                };

                                if imm4 == 0b0000 {
                                    // DUP (element)
                                    inst.opcode = Opcode::DUP;

                                    let (size, index) = match imm5.trailing_zeros() {
                                        0 => {
                                            (SIMDSizeCode::B, imm5 >> 1)
                                        }
                                        1 => {
                                            (SIMDSizeCode::H, imm5 >> 2)
                                        }
                                        2 => {
                                            (SIMDSizeCode::S, imm5 >> 3)
                                        }
                                        _ => {
                                            (SIMDSizeCode::D, imm5 >> 4)
                                        }
                                    };
                                    inst.operands = [
                                        Operand::SIMDRegisterElements(vector_size, Rd as u16, size),
                                        Operand::SIMDRegisterElementsLane(vector_size, Rn as u16, size, index as u8),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else if imm4 == 0b0001 {
                                    // DUP (general)
                                    inst.opcode = Opcode::DUP;

                                    let (size, gpr_size) = match imm5.trailing_zeros() {
                                        0 => {
                                            (SIMDSizeCode::B, SizeCode::W)
                                        }
                                        1 => {
                                            (SIMDSizeCode::H, SizeCode::W)
                                        }
                                        2 => {
                                            (SIMDSizeCode::S, SizeCode::W)
                                        }
                                        3 => {
                                            if !Q {
                                                return Err(ErrorKind::InvalidOperand);
                                            }
                                            (SIMDSizeCode::D, SizeCode::X)
                                        }
                                        _ => {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                    };
                                    inst.operands = [
                                        Operand::SIMDRegisterElements(vector_size, Rd as u16, size),
                                        Operand::Register(gpr_size, Rn as u16),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else if imm4 == 0b0101 {
                                    // SMOV
                                    inst.opcode = Opcode::SMOV;

                                    let gpr_size = if Q {
                                        SizeCode::X
                                    } else {
                                        SizeCode::W
                                    };

                                    let (size, index) = match imm5.trailing_zeros() {
                                        0 => {
                                            (SIMDSizeCode::B, imm5 >> 1)
                                        }
                                        1 => {
                                            (SIMDSizeCode::H, imm5 >> 2)
                                        }
                                        2 => {
                                            if !Q {
                                                return Err(ErrorKind::InvalidOperand);
                                            }
                                            (SIMDSizeCode::S, imm5 >> 3)
                                        }
                                        _ => {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                    };

                                    inst.operands = [
                                        Operand::Register(gpr_size, Rd as u16),
                                        Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rn as u16, size, index as u8),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else if imm4 == 0b0111 {
                                    // UMOV
                                    inst.opcode = Opcode::UMOV;

                                    let gpr_size = if Q {
                                        SizeCode::X
                                    } else {
                                        SizeCode::W
                                    };

                                    let (size, index) = match imm5.trailing_zeros() {
                                        0 => {
                                            if Q {
                                                return Err(ErrorKind::InvalidOperand);
                                            }
                                            (SIMDSizeCode::B, imm5 >> 1)
                                        }
                                        1 => {
                                            if Q {
                                                return Err(ErrorKind::InvalidOperand);
                                            }
                                            (SIMDSizeCode::H, imm5 >> 2)
                                        }
                                        2 => {
                                            if Q {
                                                return Err(ErrorKind::InvalidOperand);
                                            }
                                            (SIMDSizeCode::S, imm5 >> 3)
                                        }
                                        3 => {
                                            if !Q {
                                                return Err(ErrorKind::InvalidOperand);
                                            }
                                            (SIMDSizeCode::D, imm5 >> 4)
                                        }
                                        _ => {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                    };

                                    inst.operands = [
                                        Operand::Register(gpr_size, Rd as u16),
                                        Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rn as u16, size, index as u8),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else if imm4 == 0b0011 {
                                    // INS (general)
                                    inst.opcode = Opcode::INS;

                                    let (size, gpr_size, index) = match imm5.trailing_zeros() {
                                        0 => {
                                            (SIMDSizeCode::B, SizeCode::W, imm5 >> 1)
                                        }
                                        1 => {
                                            (SIMDSizeCode::H, SizeCode::W, imm5 >> 2)
                                        }
                                        2 => {
                                            (SIMDSizeCode::S, SizeCode::W, imm5 >> 3)
                                        }
                                        3 => {
                                            (SIMDSizeCode::D, SizeCode::X, imm5 >> 4)
                                        }
                                        _ => {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                    };
                                    inst.operands = [
                                        Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rd as u16, size, index as u8),
                                        Operand::Register(gpr_size, Rn as u16),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            }
                        } else {
                            if op3 & 0b000100001 == 0b000000000 && op0 & 0b1011 == 0b0010 {
                                // `Advanced SIMD extract`
                                let Rd = (word >> 0) & 0b11111;
                                let Rn = (word >> 5) & 0b11111;
                                let imm4 = (word >> 11) & 0b1111;
                                let Rm = (word >> 16) & 0b11111;
                                let op2 = (word >> 22) & 0b11;
                                let Q = (word >> 30) & 1 == 1;

                                if op2 != 00 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                inst.opcode = Opcode::EXT;

                                let (vec_size, index) = if Q {
                                    (SIMDSizeCode::Q, imm4)
                                } else {
                                    if (imm4 & 0b1000) != 0 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                    (SIMDSizeCode::D, imm4)
                                };

                                inst.operands = [
                                    Operand::SIMDRegisterElements(vec_size, Rd as u16, SIMDSizeCode::B),
                                    Operand::SIMDRegisterElements(vec_size, Rn as u16, SIMDSizeCode::B),
                                    Operand::SIMDRegisterElements(vec_size, Rm as u16, SIMDSizeCode::B),
                                    Operand::Imm16(index as u16),
                                ];
                            } else if op3 & 0b000100011 == 0b000000010 && op0 & 0b1011 == 0b0000 {
                                // `Advanced SIMD permute`
                                let Rd = (word >> 0) & 0b11111;
                                let Rn = (word >> 5) & 0b11111;
                                let opcode = (word >> 12) & 0b111;
                                let Rm = (word >> 16) & 0b11111;
                                let size = (word >> 22) & 0b11;
                                let Q = (word >> 30) & 1 == 1;

                                const OPCODES: [Result<Opcode, ErrorKind>; 8] = [
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok(Opcode::UZP1),
                                    Ok(Opcode::TRN1),
                                    Ok(Opcode::ZIP1),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok(Opcode::UZP2),
                                    Ok(Opcode::TRN2),
                                    Ok(Opcode::ZIP2),
                                ];
                                inst.opcode = OPCODES[opcode as usize]?;

                                let vec_size = if Q {
                                    SIMDSizeCode::Q
                                } else {
                                    SIMDSizeCode::D
                                };

                                let size = match size {
                                    0b00 => SIMDSizeCode::B,
                                    0b01 => SIMDSizeCode::H,
                                    0b10 => SIMDSizeCode::S,
                                    _ => {
                                        if !Q {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                        SIMDSizeCode::D
                                    }
                                };

                                inst.operands = [
                                    Operand::SIMDRegisterElements(vec_size, Rd as u16, size),
                                    Operand::SIMDRegisterElements(vec_size, Rn as u16, size),
                                    Operand::SIMDRegisterElements(vec_size, Rm as u16, size),
                                    Operand::Nothing,
                                ];
                            } else if op3 & 0b000100011 == 0b000000000 && op0 & 0b1011 == 0b0000 {
                                // `Advanced SIMD table lookup`
                                let Rd = (word >> 0) & 0b11111;
                                let Rn = (word >> 5) & 0b11111;
                                let op = (word >> 12) & 0b1;
                                let len = ((word >> 13) & 0b11) as u8;
                                let Rm = (word >> 16) & 0b11111;
                                let op2 = (word >> 22) & 0b11;
                                let Q = (word >> 30) & 1;

                                if op2 != 0b00 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                let vec_size = if Q == 0 {
                                    SIMDSizeCode::D
                                } else {
                                    SIMDSizeCode::Q
                                };

                                inst.opcode = if op == 0 {
                                    Opcode::TBL
                                } else {
                                    Opcode::TBX
                                };
                                inst.operands = [
                                    Operand::SIMDRegisterElements(vec_size, Rd as u16, SIMDSizeCode::B),
                                    Operand::SIMDRegisterGroup(SIMDSizeCode::Q, Rn as u16, SIMDSizeCode::B, len + 1),
                                    Operand::SIMDRegisterElements(vec_size, Rm as u16, SIMDSizeCode::B),
                                    Operand::Nothing,
                                ];
                            } else {
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        }
                    }
                }
            } else if (op0 & 0b0101) == 0b0001 {
                // op0 == x0x1 (floating-point ops or unallocated - op1, op2, op3 are
                // self-contained from here)
                if op1 & 0b10 == 0b00 {
                    if op2 & 0b0100 == 0b0000 {
                        // op2 = x0xx
                        // `Conversion between floating-point and fixed-point`
                        let sf = word >> 31;
                        let S = (word >> 29) & 1;
                        let ty = (word >> 22) & 0b11;
                        let mode = (word >> 19) & 0b11;
                        let opcode = (word >> 16) & 0b111;
                        let scale = (word >> 10) & 0b11_1111;
                        let Rn = (word >> 5) & 0b1_1111;
                        let Rd = word & 0b1_1111;

                        if S == 1 {
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        if sf == 0 && scale >= 100000 {
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        if opcode >= 0b100 {
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        let size = if sf == 0 {
                            SizeCode::W
                        } else {
                            SizeCode::X
                        };

                        let precision = if ty == 0b00 {
                            SIMDSizeCode::S
                        } else if ty == 0b01 {
                            SIMDSizeCode::D
                        } else if ty == 0b11 {
                            SIMDSizeCode::H
                        } else {
                            return Err(ErrorKind::InvalidOperand);
                        };

                        const OPCODES: &[Result<Opcode, ErrorKind>] = &[
                            // type = 00
                            // mode = 00, opcode = 00
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            // mode = 00, opcode = 10
                            Ok(Opcode::SCVTF),
                            Ok(Opcode::UCVTF),
                            // mode = 11, opcode = 00
                            Ok(Opcode::FCVTZS),
                            Ok(Opcode::FCVTZU),
                            // mode = 11, opcode = 10
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                        ];

                        let opc_idx = ((mode & 0b10) << 1) | (opcode & 0b011);
                        inst.opcode = OPCODES[opc_idx as usize]?;

                        if mode == 0b00 {
                            inst.operands = [
                                Operand::SIMDRegister(precision, Rd as u16),
                                Operand::Register(size, Rn as u16),
                                Operand::Immediate(core::cmp::min(64 - scale, if size == SizeCode::X { 64 } else { 32 })),
                                Operand::Nothing,
                            ];
                        } else {
                            inst.operands = [
                                Operand::Register(size, Rd as u16),
                                Operand::SIMDRegister(precision, Rn as u16),
                                Operand::Immediate(core::cmp::min(64 - scale, if size == SizeCode::X { 64 } else { 32 })),
                                Operand::Nothing,
                            ];
                        }
                    } else if (op3 & 0b000_111111) == 0b000_000000 {
                        // op2 = x1xx, op3 = xxx000000
                        // `Conversion between floating-point and fixed-point`
                        /*
                        let op3 = (word >> 10) & 0b1_1111_1111;
                        let op2 = (word >> 19) & 0b1111;
                        let op1 = (word >> 23) & 0b11;
                        let op0 = (word >> 28) & 0b1111;
                        */

                        let sf = word >> 31;
                        let S = (word >> 29) & 1;
                        let ty = (word >> 22) & 0b11;
                        let mode = (word >> 19) & 0b11;
                        let opcode = (word >> 16) & 0b111;
                        let Rn = (word >> 5) & 0b1_1111;
                        let Rd = word & 0b1_1111;

                        if S == 1 {
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        if mode != 0b00 && opcode >= 0b100 && opcode < 0b110 {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                        let size = if sf == 0 {
                            SizeCode::W
                        } else {
                            SizeCode::X
                        };

                        let precision = if ty == 0b00 {
                            SIMDSizeCode::S
                        } else if ty == 0b01 {
                            SIMDSizeCode::D
                        } else if ty == 0b11 {
                            SIMDSizeCode::H
                        } else {
                            if (opcode == 0b110 || opcode == 0b111) && mode == 0b01 {
                                // this is fine, but it's not inferred..
                                SIMDSizeCode::B
                            } else {
                                return Err(ErrorKind::InvalidOperand);
                            }
                        };

                        #[derive(Copy, Clone, Debug)]
                        enum GpSize {
                            Infer,
                            W,
                            X,
                        }
                        #[derive(Copy, Clone, Debug)]
                        enum FpSize {
                            Infer,
                            H,
                            S,
                            D,
                            Qh,
                        }
                        #[allow(non_camel_case_types)]
                        #[derive(Copy, Clone, Debug)]
                        enum ConvertOperands {
                            fp_to_gp(FpSize, GpSize),
                            gp_to_fp(GpSize, FpSize),
                        }

                        const OPCODES: &[Result<(Opcode, ConvertOperands), ErrorKind>; 32] = &[
                            Ok((Opcode::FCVTNS, ConvertOperands::fp_to_gp(FpSize::Infer, GpSize::Infer))),
                            Ok((Opcode::FCVTNU, ConvertOperands::fp_to_gp(FpSize::Infer, GpSize::Infer))),
                            Ok((Opcode::SCVTF, ConvertOperands::gp_to_fp(GpSize::Infer, FpSize::Infer))),
                            Ok((Opcode::UCVTF, ConvertOperands::gp_to_fp(GpSize::Infer, FpSize::Infer))),
                            Ok((Opcode::FCVTAS, ConvertOperands::fp_to_gp(FpSize::Infer, GpSize::Infer))),
                            Ok((Opcode::FCVTAU, ConvertOperands::fp_to_gp(FpSize::Infer, GpSize::Infer))),
                            Err(ErrorKind::InvalidOpcode), // these are handled in the explicit check above
                            Err(ErrorKind::InvalidOpcode), // these are handled in the explicit check above
                            Ok((Opcode::FCVTPS, ConvertOperands::fp_to_gp(FpSize::Infer, GpSize::Infer))),
                            Ok((Opcode::FCVTPU, ConvertOperands::fp_to_gp(FpSize::Infer, GpSize::Infer))),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Ok((Opcode::FCVTMS, ConvertOperands::fp_to_gp(FpSize::Infer, GpSize::Infer))),
                            Ok((Opcode::FCVTMU, ConvertOperands::fp_to_gp(FpSize::Infer, GpSize::Infer))),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Ok((Opcode::FCVTZS, ConvertOperands::fp_to_gp(FpSize::Infer, GpSize::Infer))),
                            Ok((Opcode::FCVTZU, ConvertOperands::fp_to_gp(FpSize::Infer, GpSize::Infer))),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                        ];

                        let (opcode, operands_size) = if opcode == 0b110 || opcode == 0b111 {
                            match (sf, ty, mode, opcode) {
                                (0, 0b01, 0b11, 0b110) => {
                                    (Opcode::FJCVTZS, ConvertOperands::fp_to_gp(FpSize::D, GpSize::W))
                                }
                                (1, 0b10, 0b01, 0b110) => {
                                    (Opcode::FMOV, ConvertOperands::fp_to_gp(FpSize::Qh, GpSize::X))
                                }
                                (1, 0b10, 0b01, 0b111) => {
                                    (Opcode::FMOV, ConvertOperands::gp_to_fp(GpSize::X, FpSize::Qh))
                                }
                                (1, 0b11, 0b00, 0b110) => {
                                    (Opcode::FMOV, ConvertOperands::fp_to_gp(FpSize::H, GpSize::X))
                                }
                                (1, 0b11, 0b00, 0b111) => {
                                    (Opcode::FMOV, ConvertOperands::gp_to_fp(GpSize::X, FpSize::H))
                                }
                                (1, 0b01, 0b00, 0b110) => {
                                    (Opcode::FMOV, ConvertOperands::fp_to_gp(FpSize::D, GpSize::X))
                                }
                                (1, 0b01, 0b00, 0b111) => {
                                    (Opcode::FMOV, ConvertOperands::gp_to_fp(GpSize::X, FpSize::D))
                                }
                                (0, 0b11, 0b00, 0b110) => {
                                    (Opcode::FMOV, ConvertOperands::fp_to_gp(FpSize::H, GpSize::W))
                                }
                                (0, 0b11, 0b00, 0b111) => {
                                    (Opcode::FMOV, ConvertOperands::gp_to_fp(GpSize::W, FpSize::H))
                                }
                                (0, 0b00, 0b00, 0b110) => {
                                    (Opcode::FMOV, ConvertOperands::fp_to_gp(FpSize::S, GpSize::W))
                                }
                                (0, 0b00, 0b00, 0b111) => {
                                    (Opcode::FMOV, ConvertOperands::gp_to_fp(GpSize::W, FpSize::S))
                                }
                                _ => {
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            }
                        } else {
                            OPCODES[((mode << 3) | opcode) as usize]?
                        };

                        inst.opcode = opcode;

                        match operands_size {
                            ConvertOperands::gp_to_fp(gp_size, fp_size) => {
                                let fp_reg = match fp_size {
                                    FpSize::Infer => Operand::SIMDRegister(precision, Rd as u16),
                                    FpSize::H => Operand::SIMDRegister(SIMDSizeCode::H, Rd as u16),
                                    FpSize::S => Operand::SIMDRegister(SIMDSizeCode::S, Rd as u16),
                                    FpSize::D => Operand::SIMDRegister(SIMDSizeCode::D, Rd as u16),
                                    FpSize::Qh => Operand::SIMDRegisterElementsLane(
                                        SIMDSizeCode::Q, Rd as u16, SIMDSizeCode::D, 1
                                    ),
                                };
                                let gp_reg = match gp_size {
                                    GpSize::Infer => Operand::Register(size, Rn as u16),
                                    GpSize::W => Operand::Register(SizeCode::W, Rn as u16),
                                    GpSize::X => Operand::Register(SizeCode::X, Rn as u16),
                                };
                                inst.operands = [
                                    fp_reg,
                                    gp_reg,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            ConvertOperands::fp_to_gp(fp_size, gp_size) => {
                                let fp_reg = match fp_size {
                                    FpSize::Infer => Operand::SIMDRegister(precision, Rn as u16),
                                    FpSize::H => Operand::SIMDRegister(SIMDSizeCode::H, Rn as u16),
                                    FpSize::S => Operand::SIMDRegister(SIMDSizeCode::S, Rn as u16),
                                    FpSize::D => Operand::SIMDRegister(SIMDSizeCode::D, Rn as u16),
                                    FpSize::Qh => Operand::SIMDRegisterElementsLane(
                                        SIMDSizeCode::Q, Rn as u16, SIMDSizeCode::D, 1
                                    ),
                                };
                                let gp_reg = match gp_size {
                                    GpSize::Infer => Operand::Register(size, Rd as u16),
                                    GpSize::W => Operand::Register(SizeCode::W, Rd as u16),
                                    GpSize::X => Operand::Register(SizeCode::X, Rd as u16),
                                };
                                inst.operands = [
                                    gp_reg,
                                    fp_reg,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                        }
                    } else {
                        if (word >> 29) > 0 {
                            // if either `M` or `S` are set..
                            return Err(ErrorKind::InvalidOpcode);
                        }
                        match op3.trailing_zeros() {
                            0 => {
                                // op3 = xxxxxxxx1
                                // `Floating-point conditional compare` or
                                // `Floating-point conditional select`
                                let ty = (word >> 22) & 0b11;
                                let Rm = (word >> 16) & 0b1_1111;
                                let cond = (word >> 12) & 0b1111;
                                let Rn = (word >> 5) & 0b1_1111;

                                let precision = if ty == 0b00 {
                                    SIMDSizeCode::S
                                } else if ty == 0b01 {
                                    SIMDSizeCode::D
                                } else if ty == 0b11 {
                                    SIMDSizeCode::H
                                } else {
                                    return Err(ErrorKind::InvalidOperand);
                                };
                                if ((word >> 11) & 1) == 0 {
                                    // fp compare
                                    let nzcv = word & 0b1111;
                                    inst.opcode = if (word >> 4) & 1 == 0 {
                                        Opcode::FCCMP
                                    } else {
                                        Opcode::FCCMPE
                                    };
                                    inst.operands = [
                                        Operand::SIMDRegister(precision, Rn as u16),
                                        Operand::SIMDRegister(precision, Rm as u16),
                                        Operand::Immediate(nzcv),
                                        Operand::ConditionCode(cond as u8),
                                    ];
                                } else {
                                    // fp select
                                    let Rd = word & 0b1_1111;
                                    inst.opcode = Opcode::FCSEL;
                                    inst.operands = [
                                        Operand::SIMDRegister(precision, Rd as u16),
                                        Operand::SIMDRegister(precision, Rn as u16),
                                        Operand::SIMDRegister(precision, Rm as u16),
                                        Operand::ConditionCode(cond as u8),
                                    ];
                                }
                            }
                            1 => {
                                // op3 = xxxxxxx10
                                // `Floating-point data-processing (2 source)`
                                let ty = (word >> 22) & 0b11;
                                let Rm = (word >> 16) & 0b1_1111;
                                let opcode = (word >> 12) & 0b1111;
                                let Rn = (word >> 5) & 0b1_1111;
                                let Rd = word & 0b1_1111;

                                if opcode > 0b1000 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                let precision = if ty == 0b00 {
                                    SIMDSizeCode::S
                                } else if ty == 0b01 {
                                    SIMDSizeCode::D
                                } else if ty == 0b11 {
                                    SIMDSizeCode::H
                                } else {
                                    return Err(ErrorKind::InvalidOperand);
                                };

                                const OPCODES: &[Opcode] = &[
                                    Opcode::FMUL,
                                    Opcode::FDIV,
                                    Opcode::FADD,
                                    Opcode::FSUB,
                                    Opcode::FMAX,
                                    Opcode::FMIN,
                                    Opcode::FMAXNM,
                                    Opcode::FMINNM,
                                    Opcode::FNMUL,
                                ];

                                inst.opcode = OPCODES[opcode as usize];
                                inst.operands = [
                                    Operand::SIMDRegister(precision, Rd as u16),
                                    Operand::SIMDRegister(precision, Rn as u16),
                                    Operand::SIMDRegister(precision, Rm as u16),
                                    Operand::Nothing,
                                ];
                            }
                            2 => {
                                // op3 = xxxxxx100
                                // `Floating-point immediate`
                                let ty = (word >> 22) & 0b11;
                                let imm8 = (word >> 13) & 0b1111_1111;
                                let imm5 = (word >> 5) & 0b1_1111;
                                let Rd = word & 0b1_1111;

                                if imm5 != 0 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                let precision = if ty == 0b00 {
                                    SIMDSizeCode::S
                                } else if ty == 0b01 {
                                    SIMDSizeCode::D
                                } else if ty == 0b11 {
                                    SIMDSizeCode::H
                                } else {
                                    return Err(ErrorKind::InvalidOperand);
                                };

                                inst.opcode = Opcode::FMOV;
                                inst.operands = [
                                    Operand::SIMDRegister(precision, Rd as u16),
                                    Operand::ImmediateDouble(docs::VFPExpandImm(imm8 as u8)),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            3 => {
                                // op3 = xxxxx1000
                                // `Floating-point compare`
                                let ty = (word >> 22) & 0b11;
                                let Rm = (word >> 16) & 0b1_1111;
                                let opcode = (word >> 14) & 0b11;
                                let Rn = (word >> 5) & 0b1_1111;
                                let opcode2 = word & 0b1_1111;

                                if (opcode & 0b00_111) != 00_000 {
                                    // any of the low three bits in opcode being set means
                                    // unallocated
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                let precision = if ty == 0b00 {
                                    SIMDSizeCode::S
                                } else if ty == 0b01 {
                                    SIMDSizeCode::D
                                } else if ty == 0b11 {
                                    SIMDSizeCode::H
                                } else {
                                    return Err(ErrorKind::InvalidOperand);
                                };

                                inst.opcode = if opcode2 & 0b1_0000 != 0 {
                                    Opcode::FCMPE
                                } else {
                                    Opcode::FCMP
                                };
                                inst.operands = [
                                    Operand::SIMDRegister(precision, Rn as u16),
                                    if opcode2 & 0b01000 != 0 { // and, sugguested, Rm == 0
                                        Operand::ImmediateDouble(0.0)
                                    } else {
                                        Operand::SIMDRegister(precision, Rm as u16)
                                    },
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            4 => {
                                // op3 = xxxx10000
                                // `Floating-point data-processing (1 source)`
                                let ty = (word >> 22) & 0b11;
                                let opcode = (word >> 15) & 0b111_111;
                                let Rn = (word >> 5) & 0b1_1111;
                                let Rd = word & 0b1_1111;

                                if opcode >= 0b100_000 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                let precision = if ty == 0b00 {
                                    SIMDSizeCode::S
                                } else if ty == 0b01 {
                                    SIMDSizeCode::D
                                } else if ty == 0b11 {
                                    SIMDSizeCode::H
                                } else {
                                    return Err(ErrorKind::InvalidOperand);
                                };

                                const OPCODES: &[Result<Opcode, ErrorKind>] = &[
                                    Ok(Opcode::FMOV), Ok(Opcode::FABS), Ok(Opcode::FNEG), Ok(Opcode::FSQRT),
                                    Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                    Ok(Opcode::FRINTN), Ok(Opcode::FRINTP), Ok(Opcode::FRINTM), Ok(Opcode::FRINTZ),
                                    Ok(Opcode::FRINTA), Err(ErrorKind::InvalidOpcode), Ok(Opcode::FRINTX), Ok(Opcode::FRINTI),
                                    Ok(Opcode::FRINT32Z), Ok(Opcode::FRINT32X), Ok(Opcode::FRINT64Z), Ok(Opcode::FRINT64X),
                                ];

                                if opcode >= 0b000100 && opcode < 0b001000 {
                                    // precision-specific opcodes to convert between floating
                                    // point precisions
                                    let dest_precision_bits = opcode & 0b11;
                                    let dest_precision = if dest_precision_bits == 0b00 {
                                        SIMDSizeCode::S
                                    } else if dest_precision_bits == 0b01 {
                                        SIMDSizeCode::D
                                    } else if dest_precision_bits == 0b10 {
                                        return if precision == SIMDSizeCode::H {
                                            inst.opcode = Opcode::BFCVT;
                                            inst.operands = [
                                                Operand::SIMDRegister(SIMDSizeCode::H, Rd as u16),
                                                Operand::SIMDRegister(SIMDSizeCode::S, Rn as u16),
                                                Operand::Nothing,
                                                Operand::Nothing,
                                            ];
                                            Ok(())
                                        } else {
                                            Err(ErrorKind::InvalidOpcode)
                                        };
                                    } else {
                                        SIMDSizeCode::H
                                    };

                                    if precision == dest_precision {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }

                                    inst.opcode = Opcode::FCVT;
                                    inst.operands = [
                                        Operand::SIMDRegister(dest_precision, Rd as u16),
                                        Operand::SIMDRegister(precision, Rn as u16),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else {
                                    let opc = OPCODES.get(opcode as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;
                                    inst.opcode = opc;
                                    inst.operands = [
                                        Operand::SIMDRegister(precision, Rd as u16),
                                        Operand::SIMDRegister(precision, Rn as u16),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                            }
                            5 => {
                                // op3 = xxx100000
                                // unallocated
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            _ => {
                                // 6 or more zeroes
                                // op3 = xxx000000
                                // `Conversion between floating-point and integer`
                                // handled above, so this arm is actually unreachable.
                            }
                        }
                    }
                } else {
                    // op1 = 1x
                    // `Floating-point data-processing (3 source)`

                    if (word >> 29) > 0 {
                        // if either `M` or `S` are set..
                        return Err(ErrorKind::InvalidOpcode);
                    }

                    let type_o1_o0 = ((word >> 20) & 0b1110) | ((word >> 15) & 0b0001);
                    const OPCODES: [Result<(Opcode, SIMDSizeCode), ErrorKind>; 16] = [
                        Ok((Opcode::FMADD, SIMDSizeCode::S)),
                        Ok((Opcode::FMSUB, SIMDSizeCode::S)),
                        Ok((Opcode::FNMADD, SIMDSizeCode::S)),
                        Ok((Opcode::FNMSUB, SIMDSizeCode::S)),

                        Ok((Opcode::FMADD, SIMDSizeCode::D)),
                        Ok((Opcode::FMSUB, SIMDSizeCode::D)),
                        Ok((Opcode::FNMADD, SIMDSizeCode::D)),
                        Ok((Opcode::FNMSUB, SIMDSizeCode::D)),

                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),

                        Ok((Opcode::FMADD, SIMDSizeCode::H)), // TODO: half-precision added in ARMv8.2
                        Ok((Opcode::FMSUB, SIMDSizeCode::H)),
                        Ok((Opcode::FNMADD, SIMDSizeCode::H)),
                        Ok((Opcode::FNMSUB, SIMDSizeCode::H)),
                    ];

                    let (opcode, precision) = OPCODES[type_o1_o0 as usize]?;

                    let Rd = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Ra = ((word >> 10) & 0x1f) as u16;
                    let Rm = ((word >> 16) & 0x1f) as u16;

                    inst.opcode = opcode;
                    inst.operands = [
                        Operand::SIMDRegister(precision, Rd),
                        Operand::SIMDRegister(precision, Rn),
                        Operand::SIMDRegister(precision, Rm),
                        Operand::SIMDRegister(precision, Ra),
                    ];
                }
            } else if (op0 & 0b1101) == 0b0101 {
                // op0 == x1x1 `Advanced SIMD scalar *`
                if op1 < 0b10 {
                    // op1 == 0x
                    if op2 & 0b0100 == 0b0100 {
                        // op2 == x1xx
                        if op3 & 1 == 1 {
                            // `Advanced SIMD scalar three same`
                            // op3 == xxxxxxxx1
                            let Rd = word & 0b11111;
                            let Rn = (word >> 5) & 0b11111;
                            let opcode = (word >> 11) & 0b11111;
                            let Rm = (word >> 16) & 0b11111;
                            let size = (word >> 22) & 0b11;
                            let U = (word >> 29) & 1 == 1;
                            let q = (word >> 30) & 1;

                            type OperandSizeTable = [Result<(SIMDSizeCode, SIMDSizeCode, SIMDSizeCode, SIMDSizeCode), ErrorKind>; 8];
                            use crate::armv8::a64::SIMDSizeCode::*;

                            const TABLE_A: &OperandSizeTable = &[
                                Ok((D, B, D, B)), Ok((Q, B, Q, B)),
                                Ok((D, H, D, H)), Ok((Q, H, Q, H)),
                                Ok((D, S, D, S)), Ok((Q, S, Q, S)),
                                Err(ErrorKind::InvalidOperand), Ok((Q, D, Q, D)),
                            ];

                            const TABLE_C: &OperandSizeTable = &[
                                Ok((D, S, D, S)), Ok((Q, S, Q, S)),
                                Err(ErrorKind::InvalidOperand), Ok((Q, D, Q, D)),
                                Ok((D, S, D, S)), Ok((Q, S, Q, S)),
                                Err(ErrorKind::InvalidOperand), Ok((Q, D, Q, D)),
                            ];

                            if opcode < 0b11000 {
                                const OPCODES_U0_LOW: &[Result<(Opcode, &OperandSizeTable), ErrorKind>] = &[
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::SQADD, TABLE_A)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::SQSUB, TABLE_A)),
                                    Ok((Opcode::CMGT, TABLE_A)),
                                    Ok((Opcode::CMGE, TABLE_A)),
                                    Ok((Opcode::SSHL, TABLE_A)),
                                    Ok((Opcode::SQSHL, TABLE_A)),
                                    Ok((Opcode::SRSHL, TABLE_A)),
                                    Ok((Opcode::SQRSHL, TABLE_A)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::ADD, TABLE_A)),
                                    Ok((Opcode::CMTST, TABLE_A)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::SQDMULH, TABLE_A)),
                                    Err(ErrorKind::InvalidOpcode),
                                ];
                                const OPCODES_U1_LOW: &[Result<(Opcode, &OperandSizeTable), ErrorKind>] = &[
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::UQADD, TABLE_A)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::UQSUB, TABLE_A)),
                                    Ok((Opcode::CMHI, TABLE_A)),
                                    Ok((Opcode::CMHS, TABLE_A)),
                                    Ok((Opcode::USHL, TABLE_A)),
                                    Ok((Opcode::UQSHL, TABLE_A)),
                                    Ok((Opcode::URSHL, TABLE_A)),
                                    Ok((Opcode::UQRSHL, TABLE_A)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::SUB, TABLE_A)),
                                    Ok((Opcode::CMEQ, TABLE_A)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::SQRDMULH, TABLE_A)),
                                    Err(ErrorKind::InvalidOpcode),
                                ];
                                let (opcode, table) = if U {
                                    OPCODES_U1_LOW[opcode as usize]?
                                } else {
                                    OPCODES_U0_LOW[opcode as usize]?
                                };

                                let (_va_width, va_elem, _vb_width, vb_elem) = table[((size << 1) | q) as usize]?;
                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::SIMDRegister(va_elem, Rd as u16),
                                    Operand::SIMDRegister(vb_elem, Rn as u16),
                                    Operand::SIMDRegister(vb_elem, Rm as u16),
                                    Operand::Nothing,
                                ]
                            } else {
                                // indexed by ((opcode - 0b11000) << 1) | (size >> 1)
                                const OPCODES_U0_HIGH: &[Result<(Opcode, &OperandSizeTable), ErrorKind>; 16] = &[
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FMULX, TABLE_C)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FCMEQ, TABLE_C)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FRECPS, TABLE_C)),
                                    Ok((Opcode::FRSQRTS, TABLE_C)),
                                ];
                                const OPCODES_U1_HIGH: &[Result<(Opcode, &OperandSizeTable), ErrorKind>; 16] = &[
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    // 0b11010
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::FABD, TABLE_C)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    // 0b11100
                                    Ok((Opcode::FCMGE, TABLE_C)),
                                    Ok((Opcode::FCMGT, TABLE_C)),
                                    Ok((Opcode::FACGE, TABLE_C)),
                                    Ok((Opcode::FACGT, TABLE_C)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                ];
                                let index = ((opcode - 0b11000) << 1) | (size >> 1);

                                let (opcode, table) = if U {
                                    OPCODES_U1_HIGH[index as usize]?
                                } else {
                                    OPCODES_U0_HIGH[index as usize]?
                                };

                                let (_va_width, va_elem, _vb_width, vb_elem) = table[(((size & 0b01) << 1) | q) as usize]?;
                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::SIMDRegister(va_elem, Rd as u16),
                                    Operand::SIMDRegister(vb_elem, Rn as u16),
                                    Operand::SIMDRegister(vb_elem, Rm as u16),
                                    Operand::Nothing,
                                ]
                            };
                        } else {
                            if op3 & 0b10 == 0b00 {
                                // `Advanced SIMD scalar three different`
                                // op3 == xxxxxxx00
                                let Rd = word & 0b11111;
                                let Rn = (word >> 5) & 0b11111;
                                let opcode = (word >> 12) & 0b1111;
                                let Rm = (word >> 16) & 0b11111;
                                let size = (word >> 22) & 0b11;
                                let U = (word >> 29) & 1 == 1;
                                let Q = (word >> 29) & 1 == 1;

                                if U {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                let (dest_size, src_size) = if size == 0b01 {
                                    (SIMDSizeCode::S, SIMDSizeCode::H)
                                } else if size == 0b10 {
                                    (SIMDSizeCode::D, SIMDSizeCode::S)
                                } else {
                                    return Err(ErrorKind::InvalidOperand);
                                };

                                let opcode = if opcode == 0b1001 {
                                    if Q { Opcode::SQDMLAL2 } else { Opcode::SQDMLAL }
                                } else if opcode == 0b1011 {
                                    if Q { Opcode::SQDMLSL2 } else { Opcode::SQDMLSL }
                                } else if opcode == 0b1101 {
                                    if Q { Opcode::SQDMULL2 } else { Opcode::SQDMULL }
                                } else {
                                    return Err(ErrorKind::InvalidOpcode);
                                };

                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::SIMDRegister(dest_size, Rd as u16),
                                    Operand::SIMDRegister(src_size, Rn as u16),
                                    Operand::SIMDRegister(src_size, Rm as u16),
                                    Operand::Nothing,
                                ];
                            } else {
                                // op3 == xxxxxxx10
                                if op3 & 0b110000011 != 0b000000010 {
                                    // op3 == 1xxxxxx10 or
                                    // op3 == x1xxxxx10
                                    // either are unallocated
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                if op2 & 0b0111 == 0b0100 {
                                    // `Advanced SIMD scalar two-register miscellaneous`
                                    let Rd = word & 0b11111;
                                    let Rn = (word >> 5) & 0b11111;
                                    let opcode = (word >> 12) & 0b11111;
                                    let size = (word >> 22) & 0b11;
                                    let U = (word >> 29) & 1;

                                    type OperandSizeTable = [Result<(SIMDSizeCode, SIMDSizeCode, SIMDSizeCode, SIMDSizeCode), ErrorKind>; 4];

                                    use crate::armv8::a64::SIMDSizeCode::*;

                                    const TABLE_A: &OperandSizeTable = &[
                                        Ok((Q, B, Q, B)),
                                        Ok((Q, H, Q, H)),
                                        Ok((Q, S, Q, S)),
                                        Ok((Q, D, Q, D)),
                                    ];

                                    const TABLE_B: &OperandSizeTable = &[
                                        Ok((Q, B, Q, H)),
                                        Ok((Q, H, Q, S)),
                                        Ok((Q, S, Q, D)),
                                        Err(ErrorKind::InvalidOperand),
                                    ];

                                    const TABLE_C: &OperandSizeTable = &[
                                        Err(ErrorKind::InvalidOperand),
                                        Ok((Q, S, Q, D)),
                                        Err(ErrorKind::InvalidOperand),
                                        Ok((Q, S, Q, D)),
                                    ];

                                    const TABLE_D: &OperandSizeTable = &[
                                        Ok((Q, S, Q, S)),
                                        Ok((Q, D, Q, D)),
                                        Ok((Q, S, Q, S)),
                                        Ok((Q, D, Q, D)),
                                    ];

                                    // indexed by `opcode << 2 | size & 0b10 | U`
                                    // opcodes for the section `Advanced SIMD scalar
                                    // two-register miscellaneous`.
                                    const ADV_SIMD_SCALAR_TWO_REG_MISC: &[Result<(Opcode, &OperandSizeTable), ErrorKind>; 128] = &[
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b00011_00
                                        Ok((Opcode::SUQADD, TABLE_A)), // 0b0001100,
                                        Ok((Opcode::USQADD, TABLE_A)), // 0b0001101,
                                        Ok((Opcode::SUQADD, TABLE_A)), // 0b0001110,
                                        Ok((Opcode::USQADD, TABLE_A)), // 0b0001111,
                                        // 0b00100_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b00101_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b00110_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::SQABS, TABLE_A)), // 0b0011100,
                                        Ok((Opcode::SQNEG, TABLE_A)), // 0b0011101,
                                        Ok((Opcode::SQABS, TABLE_A)), // 0b0011110,
                                        Ok((Opcode::SQNEG, TABLE_A)), // 0b0011111,
                                        Ok((Opcode::CMGT, TABLE_D)), // 0b0100000,
                                        Ok((Opcode::CMGE, TABLE_D)), // 0b0100001,
                                        Ok((Opcode::CMGT, TABLE_D)), // 0b0100010,
                                        Ok((Opcode::CMGE, TABLE_D)), // 0b0100011,
                                        Ok((Opcode::CMEQ, TABLE_D)), // 0b0100100,
                                        Ok((Opcode::CMLE, TABLE_D)), // 0b0100101,
                                        Ok((Opcode::CMEQ, TABLE_D)), // 0b0100110,
                                        Ok((Opcode::CMLE, TABLE_D)), // 0b0100111,
                                        Ok((Opcode::CMLT, TABLE_D)), // 0b0101000,
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::CMLT, TABLE_D)), // 0b0101010,
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::ABS, TABLE_D)), // 0b0101100,
                                        Ok((Opcode::NEG, TABLE_D)), // 0b0101101,
                                        Ok((Opcode::ABS, TABLE_D)), // 0b0101110,
                                        Ok((Opcode::NEG, TABLE_D)), // 0b0101111,
                                        // 0b01100_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::FCMGT, TABLE_D)), // 0b0110010,
                                        Ok((Opcode::FCMGE, TABLE_D)), // 0b0110011,
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::FCMEQ, TABLE_D)), // 0b0110110,
                                        Ok((Opcode::FCMLE, TABLE_D)), // 0b0110111,
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::FCMLT, TABLE_D)), // 0b0111010,
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b01110_11
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b10000_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b10010_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::SQXTUN, TABLE_B)), // 0b1001001,
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::SQXTUN, TABLE_B)), // 0b1001011,
                                        // 0b10011_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b10100_00
                                        Ok((Opcode::SQXTN, TABLE_B)), // 0b1010000,
                                        Ok((Opcode::UQXTN, TABLE_B)), // 0b1010001,
                                        Ok((Opcode::SQXTN, TABLE_B)), // 0b1010010,
                                        Ok((Opcode::UQXTN, TABLE_B)), // 0b1010011,
                                        // 0b10101_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b10110_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::FCVTXN, TABLE_C)), // 0b1011001,
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b10111_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b11000_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b11010_00
                                        Ok((Opcode::FCVTNS, TABLE_D)), // 0b1101000,
                                        Ok((Opcode::FCVTNU, TABLE_D)), // 0b1101001,
                                        Ok((Opcode::FCVTPS, TABLE_D)), // 0b1101010,
                                        Ok((Opcode::FCVTPU, TABLE_D)), // 0b1101011,
                                        Ok((Opcode::FCVTMS, TABLE_D)), // 0b1101100,
                                        Ok((Opcode::FCVTMU, TABLE_D)), // 0b1101101,
                                        Ok((Opcode::FCVTZS, TABLE_D)), // 0b1101110,
                                        Ok((Opcode::FCVTZU, TABLE_D)), // 0b1101111,
                                        Ok((Opcode::FCVTAS, TABLE_D)), // 0b1110000,
                                        Ok((Opcode::FCVTAU, TABLE_D)), // 0b1110001,
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::SCVTF, TABLE_D)), // 0b1110100,
                                        Ok((Opcode::UCVTF, TABLE_D)), // 0b1110101,
                                        Ok((Opcode::FRECPE, TABLE_D)), // 0b1110110,
                                        Ok((Opcode::FRSQRTE, TABLE_D)), // 0b1110111,
                                        // 0b11110_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        // 0b11111_00
                                        Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode),
                                        Ok((Opcode::FRECPX, TABLE_D)), // 0b1111110,
                                        Err(ErrorKind::InvalidOpcode),
                                    ];

                                    let (opcode, table) = ADV_SIMD_SCALAR_TWO_REG_MISC[((opcode << 2) | (size & 0b10) | U) as usize]?;
                                    inst.opcode = opcode;

                                    let (_, r1_size, _, r2_size) = table[size as usize]?;

                                    inst.operands = [
                                        Operand::SIMDRegister(r1_size, Rd as u16),
                                        Operand::SIMDRegister(r2_size, Rn as u16),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                    if [Opcode::FCMGE, Opcode::FCMLE, Opcode::FCMGT, Opcode::FCMEQ, Opcode::FCMLT].contains(&inst.opcode) {
                                        inst.operands[2] = Operand::ImmediateDouble(0.0);
                                    } else if [Opcode::CMGE, Opcode::CMLE, Opcode::CMGT, Opcode::CMEQ, Opcode::CMLT].contains(&inst.opcode) {
                                        inst.operands[2] = Operand::Immediate(0);
                                    }
                                } else if op2 & 0b0111 == 0b110 {
                                    // `Advanced SIMD scalar pairwise`
                                    let Rd = word & 0b11111;
                                    let Rn = (word >> 5) & 0b11111;
                                    let opcode = (word >> 12) & 0b11111;
                                    let size = (word >> 22) & 0b11;
                                    let U = (word >> 29) & 1;

                                    let (opcode, vec_size, size) = if opcode == 0b11011 {
                                        if U == 0 && size == 0b11 {
                                            (Opcode::ADDP, SIMDSizeCode::Q, SIMDSizeCode::D)
                                        } else {
                                            return Err(ErrorKind::InvalidOpcode);
                                        }
                                    } else {
                                        let (vec_size, el_size) = if U == 0 {
                                            (SIMDSizeCode::S, SIMDSizeCode::H)
                                        } else if size & 0b01 == 0 {
                                            (SIMDSizeCode::D, SIMDSizeCode::S)
                                        } else {
                                            (SIMDSizeCode::Q, SIMDSizeCode::D)
                                        };

                                        let opcode = match opcode | ((size & 0b10) << 4) {
                                            0b001100 => Opcode::FMAXNMP,
                                            0b001101 => Opcode::FADDP,
                                            0b001111 => Opcode::FMAXP,
                                            0b101100 => Opcode::FMINNMP,
                                            0b101111 => Opcode::FMINP,
                                            _ => {
                                                return Err(ErrorKind::InvalidOpcode);
                                            }
                                        };

                                        (opcode, vec_size, el_size)
                                    };

                                    inst.opcode = opcode;

                                    inst.operands = [
                                        Operand::SIMDRegister(size, Rd as u16),
                                        Operand::SIMDRegisterElements(vec_size, Rn as u16, size),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                } else if op2 == 0b1111 {
                                    // `Advanced SIMD scalar two-register miscellaneous FP16`
                                    let Rd = word & 0b11111;
                                    let Rn = (word >> 5) & 0b11111;
                                    let opcode = (word >> 12) & 0b11111;
                                    let a = (word >> 23) & 1;
                                    let U = (word >> 29) & 1;

                                    let index = (opcode << 2) | (U << 1) | a;
                                    let res = match index {
                                        0b11010_0_0 => Ok(Opcode::FCVTNS),
                                        0b11011_0_0 => Ok(Opcode::FCVTMS),
                                        0b11100_0_0 => Ok(Opcode::FCVTAS),
                                        0b11101_0_0 => Ok(Opcode::SCVTF),
                                        0b01100_0_1 => Ok(Opcode::FCMGT),
                                        0b01101_0_1 => Ok(Opcode::FCMEQ),
                                        0b01110_0_1 => Ok(Opcode::FCMLT),
                                        0b11010_0_1 => Ok(Opcode::FCVTPS),
                                        0b11011_0_1 => Ok(Opcode::FCVTZS),
                                        0b11101_0_1 => Ok(Opcode::FRECPE),
                                        0b11111_0_1 => Ok(Opcode::FRECPX),

                                        0b11010_1_0 => Ok(Opcode::FCVTNU),
                                        0b11011_1_0 => Ok(Opcode::FCVTMU),
                                        0b11100_1_0 => Ok(Opcode::FCVTAU),
                                        0b11101_1_0 => Ok(Opcode::UCVTF),
                                        0b01100_1_1 => Ok(Opcode::FCMGE),
                                        0b01101_1_1 => Ok(Opcode::FCMLE),
                                        0b11010_1_1 => Ok(Opcode::FCVTPU),
                                        0b11011_1_1 => Ok(Opcode::FCVTZU),
                                        0b11101_1_1 => Ok(Opcode::FRSQRTE),
                                        _ => Err(ErrorKind::InvalidOpcode)
                                    };

                                    inst.opcode = res?;
                                    inst.operands = [
                                        Operand::SIMDRegister(SIMDSizeCode::H, Rd as u16),
                                        Operand::SIMDRegister(SIMDSizeCode::H, Rn as u16),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                    if [Opcode::FCMGE, Opcode::FCMLE, Opcode::FCMGT, Opcode::FCMEQ, Opcode::FCMLT].contains(&inst.opcode) {
                                        inst.operands[2] = Operand::ImmediateDouble(0.0);
                                    }
                                } else if op2 & 0b0111 == 0b0101 && op0 == 0b0101 {
                                    // `Cryptographic two-register SHA`
                                    let Rd = word & 0b11111;
                                    let Rn = (word >> 5) & 0b11111;
                                    let opcode = (word >> 12) & 0b11111;
                                    let size = (word >> 22) & 0b11;

                                    if size != 0b00 {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }

                                    let opcode = match opcode {
                                        0b00000 => Opcode::SHA1H,
                                        0b00001 => Opcode::SHA1SU1,
                                        0b00010 => Opcode::SHA256SU0,
                                        _ => {
                                            return Err(ErrorKind::InvalidOpcode);
                                        }
                                    };
                                    inst.opcode = opcode;
                                    if opcode == Opcode::SHA1H {
                                        inst.operands = [
                                            Operand::SIMDRegister(SIMDSizeCode::S, Rd as u16),
                                            Operand::SIMDRegister(SIMDSizeCode::S, Rn as u16),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        inst.operands = [
                                            Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, SIMDSizeCode::S),
                                            Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn as u16, SIMDSizeCode::S),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    }
                                } else {
                                    // unallocated
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            }
                        }
                    } else {
                        // op2 == x0xx
                        if op0 == 0b0101 && op2 & 0b0100 == 0b0000 && op3 & 0b000_100_011 == 0b000_000_000 {
                            // `Cryptographic three-register SHA`
                            let Rd = word & 0b11111;
                            let Rn = (word >> 5) & 0b11111;
                            let opcode_bits = (word >> 12) & 0b111;
                            let Rm = (word >> 16) & 0b11111;
                            let size = (word >> 22) & 0b11;

                            if size != 0b00 {
                                return Err(ErrorKind::InvalidOpcode);
                            }

                            let (opcode, scalars) = &[
                                Ok((Opcode::SHA1C, true)),
                                Ok((Opcode::SHA1P, true)),
                                Ok((Opcode::SHA1M, true)),
                                Ok((Opcode::SHA1SU0, false)),
                                Ok((Opcode::SHA256H, true)),
                                Ok((Opcode::SHA256H2, true)),
                                Ok((Opcode::SHA256SU1, false)),
                                Err(ErrorKind::InvalidOpcode),
                            ][opcode_bits as usize]?;

                            inst.opcode = *opcode;
                            if *scalars {
                                inst.operands = [
                                    Operand::SIMDRegister(SIMDSizeCode::Q, Rd as u16),
                                    if opcode_bits < 0b100 {
                                        // sha1 second reg is `Sn`
                                        Operand::SIMDRegister(SIMDSizeCode::S, Rn as u16)
                                    } else {
                                        Operand::SIMDRegister(SIMDSizeCode::Q, Rn as u16)
                                    },
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm as u16, SIMDSizeCode::S),
                                    Operand::Nothing,
                                ];
                            } else {
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd as u16, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn as u16, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm as u16, SIMDSizeCode::S),
                                    Operand::Nothing,
                                ];
                            }
                        } else if op0 & 0b1101 == 0b0101 && op2 & 0b1000 == 0b1000 && op3 & 0b000_110_001 == 0b000_000_001 {
                            // op2 == 10xx
                            // `Advanced SIMD scalar three same FP16`
                            let Rd = word & 0b11111;
                            let Rn = (word >> 5) & 0b11111;
                            let opcode = (word >> 11) & 0b111;
                            let Rm = (word >> 16) & 0b11111;
                            let a = (word >> 23) & 1;
                            let U = (word >> 29) & 1;

                            if opcode < 0b010 {
                                return Err(ErrorKind::InvalidOpcode);
                            }

                            let opcode = opcode - 0b010;
                            let index = (opcode << 2) | (U << 1) | a;

                            inst.opcode = [
                                // opcode == 0b010
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Ok(Opcode::FABD),
                                // opcode == 0b011
                                Ok(Opcode::FMULX),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode == 0b100
                                Ok(Opcode::FCMEQ),
                                Err(ErrorKind::InvalidOpcode),
                                Ok(Opcode::FCMGE),
                                Ok(Opcode::FCMGT),
                                // opcode == 0b101
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Ok(Opcode::FACGE),
                                Ok(Opcode::FACGT),
                                // opcode == 0b110
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode == 0b111
                                Ok(Opcode::FRECPS),
                                Ok(Opcode::FRSQRTS),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                            ][index as usize]?;

                            inst.operands = [
                                Operand::SIMDRegister(SIMDSizeCode::H, Rd as u16),
                                Operand::SIMDRegister(SIMDSizeCode::H, Rn as u16),
                                Operand::SIMDRegister(SIMDSizeCode::H, Rm as u16),
                                Operand::Nothing,
                            ];
                        } else if word & 0b1101_000_11_1100_000100001_0000000000 == 0b0101_000_00_0000_000000001_0000000000 {
                            // check op0, op1, op2, and op3 all at once because i am lazy
                            // op2 == 00xx
                            // `Advanced SIMD scalar copy`
                            let imm4 = (word >> 11) & 0b1111;
                            let op = (word >> 29) & 1;

                            if imm4 == 0b0000 && op == 0 {
                                inst.opcode = Opcode::DUP;

                                let imm5 = (word >> 16) & 0b11111;
                                let Rn = (word >> 5) & 0b11111;
                                let Rd = (word >> 0) & 0b11111;

                                let (size, index) = match imm5.trailing_zeros() {
                                    0 => {
                                        (SIMDSizeCode::B, imm5 >> 1)
                                    }
                                    1 => {
                                        (SIMDSizeCode::H, imm5 >> 2)
                                    }
                                    2 => {
                                        (SIMDSizeCode::S, imm5 >> 3)
                                    }
                                    3 => {
                                        (SIMDSizeCode::D, imm5 >> 4)
                                    }
                                    _ => {
                                        return Err(ErrorKind::InvalidOperand);
                                    }
                                };

                                inst.operands = [
                                    Operand::SIMDRegister(size, Rd as u16),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rn as u16, size, index as u8),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        } else if word & 0b1101_1111_0010_0000_1000_0100_0000_0000 == 0b0101_1110_0000_0000_1000_0100_0000_0000 {
                            // `Advanced SIMD scalar three same extra`
                            let Rd = (word >> 0) & 0b11111;
                            let Rn = (word >> 5) & 0b11111;
                            let Rm = (word >> 16) & 0b11111;
                            let opcode = (word >> 11) & 0b1111;
                            let size = (word >> 22) & 0b11;
                            let u = (word >> 29) & 1;

                            if u == 0 || opcode > 0b0001 {
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            let size = if size == 0b01 {
                                SIMDSizeCode::H
                            } else if size == 0b10 {
                                SIMDSizeCode::S
                            } else {
                                return Err(ErrorKind::InvalidOperand);
                            };

                            inst.opcode = if opcode == 0b0000 {
                                Opcode::SQRDMLAH
                            } else {
                                Opcode::SQRDMLSH
                            };
                            inst.operands = [
                                Operand::SIMDRegister(size, Rd as u16),
                                Operand::SIMDRegister(size, Rn as u16),
                                Operand::SIMDRegister(size, Rm as u16),
                                Operand::Nothing,
                            ];
                        } else {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                    }
                } else {
                    // op1 == 1x
                    if op3 & 1 == 0 {
                        // `Advanced SIMD scalar x indexed element`

                        let Rd = (word >> 0) & 0b11111;
                        let Rn = (word >> 5) & 0b11111;
                        let H = (word >> 11) & 1;
                        let opcode = (word >> 12) & 0b1111;
                        let Rm = (word >> 16) & 0b1111;
                        let M = (word >> 20) & 1;
                        let L = (word >> 21) & 1;
                        let size = (word >> 22) & 0b11;
                        let U = (word >> 29) & 1;

                        enum OperandKind {
                            SameSizes,
                            DifferentSizes,
                        }

                        let (opcode, operands_kind) = match (opcode << 1) | U {
                            0b0001_0 => (Opcode::FMLA, OperandKind::SameSizes),
                            0b0011_0 => (Opcode::SQDMLAL, OperandKind::DifferentSizes),
                            0b0101_0 => (Opcode::FMLS, OperandKind::SameSizes),
                            0b0111_0 => (Opcode::SQDMLSL, OperandKind::DifferentSizes),
                            0b1001_0 => (Opcode::FMUL, OperandKind::SameSizes),
                            0b1011_0 => (Opcode::SQDMULL, OperandKind::DifferentSizes),
                            0b1100_0 => (Opcode::SQDMULH, OperandKind::SameSizes),
                            0b1101_0 => {
                                (if U == 1 {
                                    Opcode::SQRDMLAH
                                } else {
                                    Opcode::SQRDMULH
                                }, OperandKind::SameSizes)
                            }
                            0b1111_0 => (Opcode::SQRDMLSH, OperandKind::DifferentSizes),
                            0b1001_1 => (Opcode::FMULX, OperandKind::SameSizes),
                            0b1101_1 => (Opcode::SQRDMLAH, OperandKind::SameSizes),
                            0b1111_1 => (Opcode::SQRDMLSH, OperandKind::SameSizes),
                            _ => {
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        };

                        inst.opcode = opcode;

                        // by coincidence, the `SameSizes` opcodes here all are invalid for
                        // size=0b01.
                        match operands_kind {
                            OperandKind::SameSizes => {
                                let size = if opcode == Opcode::SQDMULH || opcode == Opcode::SQRDMULH || opcode == Opcode::SQRDMLAH || opcode == Opcode::SQRDMLSH {
                                    match size {
                                        0b01 => SIMDSizeCode::H,
                                        0b10 => SIMDSizeCode::S,
                                        _ => SIMDSizeCode::D,
                                    }
                                } else {
                                    match size {
                                        0b00 => SIMDSizeCode::H,
                                        0b01 => { return Err(ErrorKind::InvalidOperand); }
                                        0b10 => SIMDSizeCode::S,
                                        _ => SIMDSizeCode::D,
                                    }
                                };

                                let (Rm, index) = match size {
                                    SIMDSizeCode::H => {
                                        (Rm, (H << 2) | (L << 1) | M)
                                    },
                                    SIMDSizeCode::S => {
                                        (Rm | (M << 4), (H << 1) | L)
                                    }
                                    _ => { /* SIMDSizeCode::D */
                                        if L != 0 {
                                            return Err(ErrorKind::InvalidOperand);
                                        }
                                        (Rm | (M << 4), H)
                                    }
                                };


                                inst.operands = [
                                    Operand::SIMDRegister(size, Rd as u16),
                                    Operand::SIMDRegister(size, Rn as u16),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, size, index as u8),
                                    Operand::Nothing,
                                ];
                            },
                            OperandKind::DifferentSizes => {
                                let (dest_size, src_size) = match size {
                                    0b01 => (SIMDSizeCode::S, SIMDSizeCode::H),
                                    0b10 => (SIMDSizeCode::D, SIMDSizeCode::S),
                                    _ => { return Err(ErrorKind::InvalidOperand); }
                                };

                                let (Rm, index) = if size == 0b10 {
                                    (Rm | (M << 4), (H << 1) | L)
                                } else {
                                    (Rm, (H << 2) | (L << 1) | M)
                                };

                                inst.operands = [
                                    Operand::SIMDRegister(dest_size, Rd as u16),
                                    Operand::SIMDRegister(src_size, Rn as u16),
                                    Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm as u16, src_size, index as u8),
                                    Operand::Nothing,
                                ];
                            }
                        }
                    } else {
                        if op1 == 0b11 {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                        // `Advanced SIMD scalar shift by immediate`
                        let Rn = (word >> 5) & 0b11111;
                        let Rd = (word >> 0) & 0b11111;
                        let opcode = (word >> 11) & 0b11111;
                        let immb = (word >> 16) & 0b111;
                        let immh = (word >> 19) & 0b1111;
                        let U = (word >> 29) & 1;

                        if immh == 0 {
                            return Err(ErrorKind::InvalidOperand);
                        }

                        #[allow(non_camel_case_types)]
                        #[derive(Debug)]
                        enum OperandsKind {
                            D_After,
                            D_Before,
                            BHSD_After,
                            HSD_Before,
                            BHS_HSD_Before,
                        }

                        let (opcode, operands_kind) = match (opcode << 1) + U {
                            0b00000_0 => (Opcode::SSHR, OperandsKind::D_Before),
                            0b00010_0 => (Opcode::SSRA, OperandsKind::D_Before),
                            0b00100_0 => (Opcode::SRSHR, OperandsKind::D_Before),
                            0b00110_0 => (Opcode::SRSRA, OperandsKind::D_Before),
                            0b01010_0 => (Opcode::SHL, OperandsKind::D_After),
                            0b01110_0 => (Opcode::SQSHL, OperandsKind::BHSD_After),
                            0b10010_0 => (Opcode::SQSHRN, OperandsKind::BHS_HSD_Before),
                            0b10011_0 => (Opcode::SQRSHRN, OperandsKind::BHS_HSD_Before),
                            0b11100_0 => (Opcode::SCVTF, OperandsKind::HSD_Before),
                            0b11111_0 => (Opcode::FCVTZS, OperandsKind::HSD_Before),
                            0b00000_1 => (Opcode::USHR, OperandsKind::D_Before),
                            0b00010_1 => (Opcode::USRA, OperandsKind::D_Before),
                            0b00100_1 => (Opcode::URSHR, OperandsKind::D_Before),
                            0b00110_1 => (Opcode::URSRA, OperandsKind::D_Before),
                            0b01000_1 => (Opcode::SRI, OperandsKind::D_Before),
                            0b01010_1 => (Opcode::SLI, OperandsKind::D_After),
                            0b01100_1 => (Opcode::SQSHLU, OperandsKind::BHSD_After),
                            0b01110_1 => (Opcode::UQSHL, OperandsKind::BHSD_After),
                            0b10000_1 => (Opcode::SQSHRUN, OperandsKind::BHS_HSD_Before),
                            0b10001_1 => (Opcode::SQRSHRUN, OperandsKind::BHS_HSD_Before),
                            0b10010_1 => (Opcode::UQSHRN, OperandsKind::BHS_HSD_Before),
                            0b10011_1 => (Opcode::UQRSHRN, OperandsKind::BHS_HSD_Before),
                            0b11100_1 => (Opcode::UCVTF, OperandsKind::HSD_Before),
                            0b11111_1 => (Opcode::FCVTZU, OperandsKind::HSD_Before),
                            _ => { return Err(ErrorKind::InvalidOpcode); },
                        };

                        inst.opcode = opcode;

                        let immh = immh as u8;
                        let immb = immb as u8;

                        let leading_zeros = immh.leading_zeros() - 4;

                        let shift = immh << 3 | immb;

                        let (a_sz, b_sz, shift) = match operands_kind {
                            OperandsKind::D_After => {
                                if leading_zeros != 0 { return Err(ErrorKind::InvalidOperand); }
                                (SIMDSizeCode::D, SIMDSizeCode::D, shift - 64)
                            },
                            OperandsKind::D_Before => {
                                if leading_zeros != 0 { return Err(ErrorKind::InvalidOperand); }
                                (SIMDSizeCode::D, SIMDSizeCode::D, 128 - shift)
                            },
                            OperandsKind::BHSD_After => {
                                match leading_zeros {
                                    0 => (SIMDSizeCode::D, SIMDSizeCode::D, shift - 64),
                                    1 => (SIMDSizeCode::S, SIMDSizeCode::S, shift - 32),
                                    2 => (SIMDSizeCode::H, SIMDSizeCode::H, shift - 16),
                                    3 => (SIMDSizeCode::B, SIMDSizeCode::B, shift - 8),
                                    _ => { return Err(ErrorKind::InvalidOperand); },
                                }
                            },
                            OperandsKind::HSD_Before => {
                                match leading_zeros {
                                    0 => (SIMDSizeCode::D, SIMDSizeCode::D, 128 - shift),
                                    1 => (SIMDSizeCode::S, SIMDSizeCode::S, 64 - shift),
                                    2 => (SIMDSizeCode::H, SIMDSizeCode::H, 32 - shift),
                                    _ => { return Err(ErrorKind::InvalidOperand); },
                                }
                            }
                            OperandsKind::BHS_HSD_Before => {
                                match leading_zeros {
                                    1 => (SIMDSizeCode::S, SIMDSizeCode::D, 64 - shift),
                                    2 => (SIMDSizeCode::H, SIMDSizeCode::S, 32 - shift),
                                    3 => (SIMDSizeCode::B, SIMDSizeCode::H, 16 - shift),
                                    _ => { return Err(ErrorKind::InvalidOperand); },
                                }
                            }
                        };

                        inst.operands = [
                            Operand::SIMDRegister(a_sz, Rd as u16),
                            Operand::SIMDRegister(b_sz, Rn as u16),
                            Operand::Imm16(shift as u16),
                            Operand::Nothing,
                        ];
                    }
                }
            } else if op0 == 0b1100 {
                let Rd = (word & 0b11111) as u16;
                let Rn = ((word >> 5) & 0b11111) as u16;

                if op1 == 0b00 {
                    if op2 & 0b1100 == 0b1000 && op3 & 0b000110000 == 0b000100000 {
                        // `Cryptographic three-register, imm2`
                        let Rm = ((word >> 16) & 0b11111) as u16;
                        let opcode = (word >> 10) & 0b11;
                        let imm2 = (word >> 12) & 0b11;

                        inst.opcode = [
                            Opcode::SM3TT1A,
                            Opcode::SM3TT1B,
                            Opcode::SM3TT2A,
                            Opcode::SM3TT2B,
                        ][opcode as usize];

                        inst.operands = [
                            Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::S),
                            Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::S),
                            Operand::SIMDRegisterElementsLane(SIMDSizeCode::Q, Rm, SIMDSizeCode::S, imm2 as u8),
                            Operand::Nothing,
                        ];
                    } else if op2 & 0b1100 == 0b1100 {
                        // `Cryptographic three-register SHA512`
                        let Rm = ((word >> 16) & 0b11111) as u16;
                        let opcode = (word >> 10) & 0b11;
                        let O = (word >> 14) & 0b1;
                        let Oop = (O << 2) | opcode;

                        match Oop {
                            0b000 => {
                                inst.opcode = Opcode::SHA512H;
                                inst.operands = [
                                    Operand::SIMDRegister(SIMDSizeCode::Q, Rd),
                                    Operand::SIMDRegister(SIMDSizeCode::Q, Rn),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::D),
                                    Operand::Nothing,
                                ];
                            }
                            0b001 => {
                                inst.opcode = Opcode::SHA512H2;
                                inst.operands = [
                                    Operand::SIMDRegister(SIMDSizeCode::Q, Rd),
                                    Operand::SIMDRegister(SIMDSizeCode::Q, Rn),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::D),
                                    Operand::Nothing,
                                ];
                            }
                            0b010 => {
                                inst.opcode = Opcode::SHA512SU1;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::D),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::D),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::D),
                                    Operand::Nothing,
                                ];
                            }
                            0b011 => {
                                inst.opcode = Opcode::RAX1;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::D),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::D),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::D),
                                    Operand::Nothing,
                                ];
                            }
                            0b100 => {
                                inst.opcode = Opcode::SM3PARTW1;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::S),
                                    Operand::Nothing,
                                ];
                            }
                            0b101 => {
                                inst.opcode = Opcode::SM3PARTW2;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::S),
                                    Operand::Nothing,
                                ];
                            }
                            0b110 => {
                                inst.opcode = Opcode::SM4EKEY;
                                inst.operands = [
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::S),
                                    Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::S),
                                    Operand::Nothing,
                                ];
                            }
                            _ => {
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        }
                    } else {
                        // `Cryptographic four-register`
                        let Ra = ((word >> 10) & 0b11111) as u16;
                        let Rm = ((word >> 16) & 0b11111) as u16;
                        let Op0 = (word >> 21) & 0b11;

                        if Op0 == 0b00 {
                            inst.opcode = Opcode::EOR3;
                            inst.operands = [
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::B),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::B),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::B),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Ra, SIMDSizeCode::B),
                            ];
                        } else if Op0 == 0b01 {
                            inst.opcode = Opcode::BCAX;
                            inst.operands = [
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::B),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::B),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::B),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Ra, SIMDSizeCode::B),
                            ];
                        } else if Op0 == 0b10 {
                            inst.opcode = Opcode::SM3SS1;
                            inst.operands = [
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::S),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::S),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::S),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Ra, SIMDSizeCode::S),
                            ];
                        } else {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                    }
                } else if op1 == 0b01 {
                    if op2 & 0b1100 == 0b0000 {
                        let imm6 = ((word >> 10) & 0b111111) as u16;
                        let Rm = ((word >> 16) & 0b11111) as u16;

                        inst.opcode = Opcode::XAR;
                        inst.operands = [
                            Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::D),
                            Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::D),
                            Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rm, SIMDSizeCode::D),
                            Operand::Imm16(imm6 as u16),
                        ];
                        // `XAR`
                    } else {
                        // `Cryptographic two-register sha512`
                        let opcode = (word >> 10) & 0b11;
                        if opcode == 0b00 {
                            inst.opcode = Opcode::SHA512SU0;
                            inst.operands = [
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::D),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::D),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        } else if opcode == 0b01 {
                            inst.opcode = Opcode::SM4E;
                            inst.operands = [
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rd, SIMDSizeCode::S),
                                Operand::SIMDRegisterElements(SIMDSizeCode::Q, Rn, SIMDSizeCode::S),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        } else {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                    }
                } else {
                    return Err(ErrorKind::InvalidOpcode);
                }
            } else {
                // rest of this space is unallocated
                return Err(ErrorKind::InvalidOpcode);
            }
        }
        Section::Unallocated => {
            return Err(ErrorKind::InvalidOpcode);
        }
        Section::DataProcessingReg => {
            /*
             * Section C3.5. Data Processing - Register
             *
             * instructions here have the form
             * XXXX101X_XXXXXXXX_XXXXXXXX_XXXXXXXX
             */
            if (word & 0x10000000) != 0 {
                // These are of the form
                // XXX1101X_...
                let group_bits = (word >> 22) & 0x7;
                match group_bits {
                    0b000 => {
                        // Add/subtract (with carry)
                        let Rd = (word & 0x1f) as u16;
                        let Rn = ((word >> 5) & 0x1f) as u16;
                        let opc2 = ((word >> 10) & 0x3f) as u16;
                        let Rm = ((word >> 16) & 0x1f) as u16;

                        if opc2 != 0b000000 {
                            if opc2 & 0b011111 == 0b000001 {
                                // `Rotate right into flags`
                                if word & 0xe0_00_00_10 != 0xa0_00_00_00 {
                                    return Err(ErrorKind::InvalidOperand);
                                }
                                let mask = (word >> 0) & 0xf;
                                let imm6 = (word >> 15) & 0x3f;

                                inst.opcode = Opcode::RMIF;
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rn),
                                    Operand::Imm16(imm6 as u16),
                                    Operand::Imm16(mask as u16),
                                    Operand::Nothing,
                                ];
                                return Ok(());
                            } else if opc2 & 0b001111 == 0b000010 {
                                // `Evaluate into flags`
                                let sz = (word >> 14) & 1;
                                if word & 0b1111_1111_1111_1111_1011_1100_0001_1111 !=
                                          0b0011_1010_0000_0000_0000_1000_0000_1101 {
                                    return Err(ErrorKind::InvalidOperand);
                                }
                                inst.opcode = if sz == 0 {
                                    Opcode::SETF8
                                } else {
                                    Opcode::SETF16
                                };
                                inst.operands = [
                                    Operand::Register(SizeCode::W, Rn),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                                return Ok(());
                            } else {
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOperand);
                            }
                        }

                        let size_code = match word >> 29 {
                            0b000 => {
                                inst.opcode = Opcode::ADC;
                                SizeCode::W
                            }
                            0b001 => {
                                inst.opcode = Opcode::ADCS;
                                SizeCode::W
                            }
                            0b010 => {
                                inst.opcode = Opcode::SBC;
                                SizeCode::W
                            }
                            0b011 => {
                                inst.opcode = Opcode::SBCS;
                                SizeCode::W
                            }
                            0b100 => {
                                inst.opcode = Opcode::ADC;
                                SizeCode::X
                            }
                            0b101 => {
                                inst.opcode = Opcode::ADCS;
                                SizeCode::X
                            }
                            0b110 => {
                                inst.opcode = Opcode::SBC;
                                SizeCode::X
                            }
                            0b111 => {
                                inst.opcode = Opcode::SBCS;
                                SizeCode::X
                            }
                            _ => {
                                unreachable!("opc and size flag are three bits");
                            }
                        };

                        inst.operands = [
                            Operand::Register(size_code, Rd),
                            Operand::Register(size_code, Rn),
                            Operand::Register(size_code, Rm),
                            Operand::Nothing
                        ];
                    },
                    0b001 => {
                        // Conditional compare (register/immediate)
                        let imm_or_reg = (word >> 11) & 0x01;

                        let o3 = (word >> 4) & 0x01;
                        let o2 = (word >> 10) & 0x01;
                        let S = (word >> 29) & 0x01;
                        let op = (word >> 30) & 0x01;
                        let sf = (word >> 31) & 0x01;

                        if S != 1 || o2 != 0 || o3 != 0 {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        let size = if sf == 1 {
                            SizeCode::X
                        } else {
                            SizeCode::W
                        };

                        inst.opcode = if op == 1 {
                            Opcode::CCMP
                        } else {
                            Opcode::CCMN
                        };

                        let Rn = ((word >> 5) & 0x1f) as u16;
                        let Rm = ((word >> 16) & 0x1f) as u16;
                        let cond = ((word >> 12) & 0x0f) as u8;
                        let nzcv = (word & 0x0f) as u8;

                        inst.operands = [
                            Operand::Register(size, Rn),
                            if imm_or_reg == 1 {
                                Operand::Immediate(Rm as u32)
                            } else {
                                Operand::Register(size, Rm)
                            },
                            Operand::Immediate(nzcv as u32),
                            Operand::ConditionCode(cond),
                        ]
                    },
                    0b010 => {
                        // Conditional select
                        let op2 = (word >> 10) & 0x03;
                        let sf_op = (word >> 28) & 0x0c;

                        if word & 0x20000000 != 0 {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOperand);
                        }

                        let size = match sf_op | op2 {
                            0b0000 => {
                                inst.opcode = Opcode::CSEL;
                                SizeCode::W
                            },
                            0b0001 => {
                                inst.opcode = Opcode::CSINC;
                                SizeCode::W
                            },
                            0b0100 => {
                                inst.opcode = Opcode::CSINV;
                                SizeCode::W
                            },
                            0b0101 => {
                                inst.opcode = Opcode::CSNEG;
                                SizeCode::W
                            },
                            0b1000 => {
                                inst.opcode = Opcode::CSEL;
                                SizeCode::X
                            },
                            0b1001 => {
                                inst.opcode = Opcode::CSINC;
                                SizeCode::X
                            },
                            0b1100 => {
                                inst.opcode = Opcode::CSINV;
                                SizeCode::X
                            },
                            0b1101 => {
                                inst.opcode = Opcode::CSNEG;
                                SizeCode::X
                            },
                            0b0010 |
                            0b0011 |
                            0b0110 |
                            0b0111 |
                            0b1010 |
                            0b1011 |
                            0b1110 |
                            0b1111 => {
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOpcode);
                            },
                            _ => {
                                unreachable!("sf, op, op2 are four bits total");
                            }
                        };

                        let Rd = (word & 0x1f) as u16;
                        let Rn = ((word >> 5) & 0x1f) as u16;
                        let Rm = ((word >> 16) & 0x1f) as u16;
                        let cond = ((word >> 12) & 0x0f) as u8;

                        inst.operands = [
                            Operand::Register(size, Rd),
                            Operand::Register(size, Rn),
                            Operand::Register(size, Rm),
                            Operand::ConditionCode(cond)
                        ];
                    },
                    0b011 => {
                        // Data processing (1 source, 2 source)
                        if ((word >> 30) & 1) == 0 {
                            // X0X11010_110XXXXX_XXXXXXXX_XXXXXXXX
                            // Data-processing (2 source)
                            let sf = ((word >> 31) & 0x01) as u8;
                            let S = ((word >> 29) & 0x01) as u8;

                            let Rd = (word & 0x1f) as u16;
                            let Rn = ((word >> 5) & 0x1f) as u16;
                            let opcode = ((word >> 10) & 0x3f) as u8;
                            let Rm = ((word >> 16) & 0x1f) as u16;

                            if S == 1 {
                                if sf == 1 && opcode == 0b000000 {
                                    inst.opcode = Opcode::SUBPS;
                                    inst.operands = [
                                        Operand::Register(SizeCode::X, Rd),
                                        Operand::RegisterOrSP(SizeCode::X, Rn),
                                        Operand::RegisterOrSP(SizeCode::X, Rm),
                                        Operand::Nothing,
                                    ];
                                    return Ok(());
                                } else {
                                    // unallocated
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            }

                            let combined_idx = (opcode << 1) | sf;

                            const OPCODES: &[Result<(Opcode, SizeCode, bool), ErrorKind>] = &[
                                Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::SUBP, SizeCode::X, true)),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode = 0b00_0010
                                Ok((Opcode::UDIV, SizeCode::W, false)),
                                Ok((Opcode::UDIV, SizeCode::X, false)),
                                // opcode = 0b00_0011
                                Ok((Opcode::SDIV, SizeCode::W, false)),
                                Ok((Opcode::SDIV, SizeCode::X, false)),
                                // opcode = 0b00_01xx
                                Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::IRG, SizeCode::X, true)),
                                Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::GMI, SizeCode::X, true)),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode = 0b00_1000
                                Ok((Opcode::LSLV, SizeCode::W, false)),
                                Ok((Opcode::LSLV, SizeCode::X, false)),
                                Ok((Opcode::LSRV, SizeCode::W, false)),
                                Ok((Opcode::LSRV, SizeCode::X, false)),
                                // opcode = 0b00_1010
                                Ok((Opcode::ASRV, SizeCode::W, false)),
                                Ok((Opcode::ASRV, SizeCode::X, false)),
                                // opcode = 0b00_1011
                                Ok((Opcode::RORV, SizeCode::W, false)),
                                Ok((Opcode::RORV, SizeCode::X, false)),
                                // opcode = 0b00_11xx
                                Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::PACGA, SizeCode::X, true)),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode = 0b01_0000
                                Ok((Opcode::CRC32B, SizeCode::W, false)),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode = 0b01_0001
                                Ok((Opcode::CRC32H, SizeCode::W, false)),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode = 0b01_0010
                                Ok((Opcode::CRC32W, SizeCode::W, false)),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode = 0b01_0011
                                Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::CRC32X, SizeCode::X, false)),
                                // opcode = 0b01_0100
                                Ok((Opcode::CRC32CB, SizeCode::W, false)),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode = 0b01_0101
                                Ok((Opcode::CRC32CH, SizeCode::W, false)),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode = 0b01_0110
                                Ok((Opcode::CRC32CW, SizeCode::W, false)),
                                Err(ErrorKind::InvalidOpcode),
                                // opcode = 0b01_0111
                                Err(ErrorKind::InvalidOpcode),
                                Ok((Opcode::CRC32CX, SizeCode::X, false)),
                            ];
                            let (opcode, size, special) = OPCODES.get(combined_idx as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;
                            inst.opcode = opcode;
                            if !special {
                                inst.operands = [
                                    // `crc32` instructions always have w-reg dest/source1 operands
                                    if combined_idx > 0b01_0000_0 {
                                        Operand::Register(SizeCode::W, Rd)
                                    } else {
                                        Operand::Register(size, Rd)
                                    },
                                    if combined_idx > 0b01_0000_0 {
                                        Operand::Register(SizeCode::W, Rn)
                                    } else {
                                        Operand::Register(size, Rn)
                                    },
                                    Operand::Register(size, Rm),
                                    Operand::Nothing,
                                ];
                            } else {
                                if opcode == Opcode::SUBP {
                                    inst.operands = [
                                        Operand::Register(SizeCode::X, Rd),
                                        Operand::RegisterOrSP(SizeCode::X, Rn),
                                        Operand::RegisterOrSP(SizeCode::X, Rm),
                                        Operand::Nothing,
                                    ];
                                } else if opcode == Opcode::IRG {
                                    inst.operands = [
                                        Operand::RegisterOrSP(SizeCode::X, Rd),
                                        Operand::RegisterOrSP(SizeCode::X, Rn),
                                        Operand::Register(SizeCode::X, Rm),
                                        Operand::Nothing,
                                    ];
                                } else if opcode == Opcode::GMI {
                                    inst.operands = [
                                        Operand::Register(SizeCode::X, Rd),
                                        Operand::RegisterOrSP(SizeCode::X, Rn),
                                        Operand::Register(SizeCode::X, Rm),
                                        Operand::Nothing,
                                    ];
                                } else if opcode == Opcode::PACGA {
                                    inst.operands = [
                                        Operand::Register(SizeCode::X, Rd),
                                        Operand::Register(SizeCode::X, Rn),
                                        Operand::RegisterOrSP(SizeCode::X, Rm),
                                        Operand::Nothing,
                                    ];
                                }
                            }
                        } else {
                            // X1X11010_110XXXXX_XXXXXXXX_XXXXXXXX
                            // Data-processing (1 source)
                            let sf = ((word >> 31) & 0x01) as u8;
                            let S = ((word >> 29) & 0x01) as u8;

                            let Rd = (word & 0x1f) as u16;
                            let Rn = ((word >> 5) & 0x1f) as u16;
                            let opcode = ((word >> 10) & 0x3f) as u8;
                            let opcode2 = ((word >> 16) & 0x1f) as u8;
                            // So ARMv8 ARM only says that 0b00000 has well-defined
                            // instructions
                            // however, PAC (added in v8.3) says otherwise.
                            match opcode2 {
                                0b00000 => {
                                    if S != 0 {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }

                                    let (opcode, size) = match (opcode << 1) | sf {
                                        0b000000_0 => (Opcode::RBIT, SizeCode::W),
                                        0b000000_1 => (Opcode::RBIT, SizeCode::X),
                                        0b000001_0 => (Opcode::REV16, SizeCode::W),
                                        0b000001_1 => (Opcode::REV16, SizeCode::X),
                                        0b000010_0 => (Opcode::REV, SizeCode::W),
                                        0b000010_1 => (Opcode::REV32, SizeCode::X),
                                        0b000011_0 => (Opcode::Invalid, SizeCode::W),
                                        0b000011_1 => (Opcode::REV, SizeCode::X),
                                        0b000100_0 => (Opcode::CLZ, SizeCode::W),
                                        0b000100_1 => (Opcode::CLZ, SizeCode::X),
                                        0b000101_0 => (Opcode::CLS, SizeCode::W),
                                        0b000101_1 => (Opcode::CLS, SizeCode::X),
                                        _ => (Opcode::Invalid, SizeCode::W),
                                    };

                                    inst.opcode = opcode;
                                    inst.operands = [
                                        Operand::Register(size, Rd),
                                        Operand::Register(size, Rn),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];

                                    if opcode == Opcode::Invalid {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                                0b00001 => {
                                    if S != 0 || sf != 1 {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }

                                    if opcode >= 0b100000 {
                                        return Err(ErrorKind::InvalidOperand);
                                    }

                                    let opc = &[
                                        Ok(Opcode::PACIA),  Ok(Opcode::PACIB),
                                        Ok(Opcode::PACDA),  Ok(Opcode::PACDB),
                                        Ok(Opcode::AUTIA),  Ok(Opcode::AUTIB),
                                        Ok(Opcode::AUTDA),  Ok(Opcode::AUTDB),
                                        Ok(Opcode::PACIZA), Ok(Opcode::PACIZB),
                                        Ok(Opcode::PACDZA), Ok(Opcode::PACDZB),
                                        Ok(Opcode::AUTIZA), Ok(Opcode::AUTIZB),
                                        Ok(Opcode::AUTDZA), Ok(Opcode::AUTDZB),
                                        Ok(Opcode::XPACI),  Ok(Opcode::XPACD),
                                        Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                        Err(ErrorKind::InvalidOpcode), Err(ErrorKind::InvalidOpcode),
                                    ][opcode as usize]?;
                                    inst.opcode = *opc;
                                    inst.operands = [
                                        Operand::Register(SizeCode::X, Rd),
                                        if opcode < 0b001000 {
                                            Operand::RegisterOrSP(SizeCode::X, Rn)
                                        } else {
                                            if Rn != 0b11111 {
                                                return Err(ErrorKind::InvalidOpcode);
                                            }
                                            Operand::Nothing
                                        },
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                }
                                _ => {
                                    // Data-processing (1 source), op2 > 0b00001 is (currently
                                    // as of v8.3) undefined.
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            }
                        }
                    },
                    _ => {
                        // Data processing (3 source)
                        let op0 = ((word >> 15) & 0x01) as u8;
                        let op31 = ((word >> 21) & 0x07) as u8;
                        let op54 = ((word >> 29) & 0x03) as u8;
                        let op = op0 | (op31 << 1) | (op54 << 4);
                        let sf = ((word >> 31) & 0x01) as u8;

                        let Rd = ((word >> 0) & 0x1f) as u16;
                        let Rn = ((word >> 5) & 0x1f) as u16;
                        let Ra = ((word >> 10) & 0x1f) as u16;
                        let Rm = ((word >> 16) & 0x1f) as u16;

                        const DATA_PROCESSING_3_SOURCE: &[Result<(Opcode, SizeCode, SizeCode), ErrorKind>] = &[
                            Ok((Opcode::MADD, SizeCode::W, SizeCode::W)),
                            Ok((Opcode::MADD, SizeCode::X, SizeCode::X)),
                            Ok((Opcode::MSUB, SizeCode::W, SizeCode::W)),
                            Ok((Opcode::MSUB, SizeCode::X, SizeCode::X)),
                            Err(ErrorKind::InvalidOpcode),
                            Ok((Opcode::SMADDL, SizeCode::W, SizeCode::X)),
                            Err(ErrorKind::InvalidOpcode),
                            Ok((Opcode::SMSUBL, SizeCode::W, SizeCode::X)),
                            Err(ErrorKind::InvalidOpcode),
                            Ok((Opcode::SMULH, SizeCode::X, SizeCode::X)),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Err(ErrorKind::InvalidOpcode),
                            Ok((Opcode::UMADDL, SizeCode::W, SizeCode::X)),
                            Err(ErrorKind::InvalidOpcode),
                            Ok((Opcode::UMSUBL, SizeCode::W, SizeCode::X)),
                            Err(ErrorKind::InvalidOpcode),
                            Ok((Opcode::UMULH, SizeCode::X, SizeCode::X)),
                            Err(ErrorKind::InvalidOpcode),
                        ];

                        let compound_idx = (op << 1) | sf;
                        let (opcode, source_size, dest_size) = DATA_PROCESSING_3_SOURCE.get(compound_idx as usize)
                            .cloned()
                            .unwrap_or(Err(ErrorKind::InvalidOpcode))?;

                        inst.opcode = opcode;
                        inst.operands = [
                            Operand::Register(dest_size, Rd),
                            Operand::Register(source_size, Rn),
                            Operand::Register(source_size, Rm),
                            if opcode == Opcode::SMULH {
                                Operand::Nothing
                            } else {
                                // in practice these match up with the corresponding operand.
                                Operand::Register(dest_size, Ra)
                            },
                        ];
                        if opcode == Opcode::UMULH {
                            inst.operands[3] = Operand::Nothing;
                        }
                    }
                }
            } else {
                // These are of the form
                // XXX0101X_...
                // so bits 21 and 24 fully distinguish categories here...
                // but bit 21 distinguishes between add/sub shifted/extended, which
                // we can deal with later. so use bit 24 to figure out the instruction
                // class, first.
                if (word & 0x01000000) == 0 {
                    // Logical (shifted register)
                    // XXX01010X_...
                    let sf = (word >> 31) == 1;
                    let size = if sf { SizeCode::X } else { SizeCode::W };

                    let opc = (word >> 28) & 6;
                    let n = (word >> 21) & 1;
                    inst.opcode = [
                        Opcode::AND,
                        Opcode::BIC,
                        Opcode::ORR,
                        Opcode::ORN,
                        Opcode::EOR,
                        Opcode::EON,
                        Opcode::ANDS,
                        Opcode::BICS,
                    ][(opc | n) as usize];

                    let shift = ((word >> 22) & 3) as u8;

                    let Rd = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let imm6 = ((word >> 10) & 0x3f) as u8;
                    let Rm = ((word >> 16) & 0x1f) as u16;

                    inst.operands[0] = Operand::Register(size, Rd);
                    inst.operands[1] = Operand::Register(size, Rn);
                    inst.operands[2] = Operand::RegShift(docs::DecodeShift(shift), imm6, size, Rm);
                } else {
                    // Add/subtract ({shifted,extended} register)
                    // XXX11011X_...
                    // specific instruction is picked by the first two bits..
                    inst.opcode = [
                        Opcode::ADD,
                        Opcode::ADDS,
                        Opcode::SUB,
                        Opcode::SUBS
                    ][((word >> 29) as usize) & 0x03];

                    let sf = (word >> 31) == 1;
                    let size = if sf { SizeCode::X } else { SizeCode::W };

                    // and operands are contingent on bit 21
                    if (word & 0x20_0000) != 0 {
                        // extended form
                        // opt (bits 22, 23) must be 0

                        if (word >> 22) & 0x03 != 0 {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOperand);
                        }

                        let Rd = (word & 0x1f) as u16;
                        let Rn = ((word >> 5) & 0x1f) as u16;
                        let imm3 = (word >> 10) & 0x07;
                        let option = (word >> 13) & 0x07;
                        let Rm = ((word >> 16) & 0x1f) as u16;

                        // for `extended register` instructions, sometimes the dest is Rd,
                        // sometimes it's <Rd|xSP>. this depends on if the instruction sets
                        // status flags.
                        inst.operands = [
                            if (word >> 29 & 1) == 0 {
                                Operand::RegisterOrSP(size, Rd)
                            } else {
                                Operand::Register(size, Rd)
                            },
                            Operand::RegisterOrSP(size, Rn),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];

                        let shift_size = match option {
                            0b011 |
                            0b111 => {
                                if size == SizeCode::X {
                                    SizeCode::X
                                } else {
                                    SizeCode::W
                                }
                            }
                            _ => {
                                SizeCode::W
                            }
                        };

                        const SHIFT_TYPES: &[ShiftStyle] = &[
                            ShiftStyle::UXTB, ShiftStyle::UXTH, ShiftStyle::UXTW, ShiftStyle::UXTX,
                            ShiftStyle::SXTB, ShiftStyle::SXTH, ShiftStyle::SXTW, ShiftStyle::SXTX,
                        ];

                        let shift_type = {
                            let ty = SHIFT_TYPES[option as usize];
                            if ty == ShiftStyle::UXTW && shift_size == SizeCode::W {
                                ShiftStyle::LSL
                            } else if ty == ShiftStyle::UXTX && shift_size == SizeCode::X {
                                ShiftStyle::LSL
                            } else {
                                ty
                            }
                        };

                        let shift = imm3 as u8;
                        inst.operands[2] = Operand::RegShift(shift_type, shift, shift_size, Rm);
                   } else {
                        // shifted form

                        let shift = (word >> 22) & 0x03;
                        if shift == 0b11 {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOperand);
                        }

                        let Rd = (word & 0x1f) as u16;
                        let Rn = ((word >> 5) & 0x1f) as u16;
                        let imm6 = ((word >> 10) & 0x3f) as u8;
                        let Rm = ((word >> 16) & 0x1f) as u16;

                        if size == SizeCode::W && imm6 >= 32 {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOperand);
                        }

                        inst.operands[0] = Operand::Register(size, Rd);
                        inst.operands[1] = Operand::Register(size, Rn);

                        inst.operands[2] = match shift {
                            0b00 => Operand::RegShift(ShiftStyle::LSL, imm6, size, Rm),
                            0b01 => Operand::RegShift(ShiftStyle::LSR, imm6, size, Rm),
                            0b10 => Operand::RegShift(ShiftStyle::ASR, imm6, size, Rm),
                            _ => { unreachable!("shift is two bits and 0b11 has early test"); },
                        };
                    }
                }
            }
        },
        Section::DataProcessingImmediate => {
            /*
             * Section C3.4, Data processing - immediate
             * This collects bits 23:25, which are the only ones that vary in this category
             */
            let group_bits = (word >> 23) & 0x7;
            match group_bits {
                0b000 |
                0b001 => {
                    // PC-rel addressing
                    if word >= 0x80000000 {
                        inst.opcode = Opcode::ADRP;
                        let offset = (((word >> 3) & 0x1ffffc) | ((word >> 29) & 0x3)) as i32;
                        let extended_offset = (offset << 11) >> 11;
                        inst.operands = [
                            Operand::Register(SizeCode::X, (word & 0x1f) as u16),
                            Operand::PCOffset((extended_offset as i64) << 12),
                            Operand::Nothing,
                            Operand::Nothing
                        ];
                    } else {
                        inst.opcode = Opcode::ADR;
                        let offset = (((word >> 3) & 0x1ffffc) | ((word >> 29) & 0x3)) as i32;
                        let extended_offset = (offset << 11) >> 11;
                        inst.operands = [
                            Operand::Register(SizeCode::X, (word & 0x1f) as u16),
                            Operand::PCOffset(extended_offset as i64),
                            Operand::Nothing,
                            Operand::Nothing
                        ];
                    };
                }
                0b010 |
                0b011 => {
                    // add/sub imm
                    let Rd = word & 0x1f;
                    let Rn = (word >> 5) & 0x1f;
                    let imm12 = (word >> 10) & 0xfff;
                    let shift = (word >> 22) & 0x3;
                    let size = match word >> 29 {
                        0b000 => {
                            inst.opcode = Opcode::ADD;
                            SizeCode::W
                        },
                        0b001 => {
                            inst.opcode = Opcode::ADDS;
                            SizeCode::W
                        },
                        0b010 => {
                            inst.opcode = Opcode::SUB;
                            SizeCode::W
                        },
                        0b011 => {
                            inst.opcode = Opcode::SUBS;
                            SizeCode::W
                        },
                        0b100 => {
                            inst.opcode = Opcode::ADD;
                            SizeCode::X
                        },
                        0b101 => {
                            inst.opcode = Opcode::ADDS;
                            SizeCode::X
                        },
                        0b110 => {
                            inst.opcode = Opcode::SUB;
                            SizeCode::X
                        },
                        0b111 => {
                            inst.opcode = Opcode::SUBS;
                            SizeCode::X
                        },
                        _ => {
                            unreachable!("size and opc are three bits");
                        }
                    };
                    if inst.opcode == Opcode::ADD || inst.opcode == Opcode::SUB {
                        inst.operands[0] = Operand::RegisterOrSP(size, Rd as u16);
                    } else {
                        inst.operands[0] = Operand::Register(size, Rd as u16);
                    }
                    inst.operands[1] = Operand::RegisterOrSP(size, Rn as u16);
                    inst.operands[2] = match shift {
                        0b00 => {
                            Operand::Immediate(imm12 as u32)
                        },
                        0b01 => {
                            Operand::ImmShift(imm12 as u16, 12)
                        },
                        0b10 |
                        0b11 => {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOperand);
                        }
                        _ => { unreachable!("shift is two bits"); }
                    };
                    inst.operands[3] = Operand::Nothing;
                }
                0b100 => {
                    // logical (imm)
                    let Rd = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let imms = (word >> 10) & 0x3f;
                    let immr = (word >> 16) & 0x3f;
                    let N = (word >> 22) & 1;
                    let size = match word >> 29 {
                        0b000 => {
                            inst.opcode = Opcode::AND;
                            SizeCode::W
                        }
                        0b001 => {
                            inst.opcode = Opcode::ORR;
                            SizeCode::W
                        }
                        0b010 => {
                            inst.opcode = Opcode::EOR;
                            SizeCode::W
                        }
                        0b011 => {
                            inst.opcode = Opcode::ANDS;
                            SizeCode::W
                        }
                        0b100 => {
                            inst.opcode = Opcode::AND;
                            SizeCode::X
                        }
                        0b101 => {
                            inst.opcode = Opcode::ORR;
                            SizeCode::X
                        }
                        0b110 => {
                            inst.opcode = Opcode::EOR;
                            SizeCode::X
                        }
                        0b111 => {
                            inst.opcode = Opcode::ANDS;
                            SizeCode::X
                        }
                        _ => {
                            unreachable!("size and opc are three bits");
                        }
                    };

                    inst.operands = [
                        if (word >> 29 & 0b11) == 0b11 {
                            Operand::Register(size, Rd)
                        } else {
                            Operand::RegisterOrSP(size, Rd)
                        },
                        Operand::Register(size, Rn),
                        match size {
                            SizeCode::W => Operand::Immediate(docs::DecodeBitMasks_32(N as u8, imms as u8, immr as u8)?.0),
                            SizeCode::X => Operand::Imm64(docs::DecodeBitMasks_64(N as u8, imms as u8, immr as u8)?.0)
                        },
                        Operand::Nothing
                    ];
                },
                0b101 => {
                    // move wide (imm)
                    let Rd = word & 0x1f;
                    let imm16 = (word >> 5) & 0xffff;
                    let hw = (word >> 21) & 0x3;
                    let size = match word >> 29 {
                        0b000 => {
                            if hw >= 0x10 {
                                inst.opcode = Opcode::Invalid;
                            } else {
                                inst.opcode = Opcode::MOVN;
                            }
                            SizeCode::W
                        },
                        0b001 => {
                            inst.opcode = Opcode::Invalid;
                            SizeCode::W
                        }
                        0b010 => {
                            if hw >= 0x10 {
                                inst.opcode = Opcode::Invalid;
                            } else {
                                inst.opcode = Opcode::MOVZ;
                            }
                            SizeCode::W
                        },
                        0b011 => {
                            if hw >= 0x10 {
                                inst.opcode = Opcode::Invalid;
                            } else {
                                inst.opcode = Opcode::MOVK;
                            }
                            SizeCode::W
                        },
                        0b100 => {
                            inst.opcode = Opcode::MOVN;
                            SizeCode::X
                        },
                        0b101 => {
                            inst.opcode = Opcode::Invalid;
                            SizeCode::X
                        }
                        0b110 => {
                            inst.opcode = Opcode::MOVZ;
                            SizeCode::X
                        },
                        0b111 => {
                            inst.opcode = Opcode::MOVK;
                            SizeCode::X
                        },
                        _ => {
                            unreachable!("size and opc are three bits");
                        }
                    };

                    inst.operands = [
                        Operand::Register(size, Rd as u16),
                        Operand::ImmShift(imm16 as u16, hw as u8 * 16),
                        Operand::Nothing,
                        Operand::Nothing
                    ];
                },
                0b110 => {
                    // bitfield
                    let Rd = word & 0x1f;
                    let Rn = (word >> 5) & 0x1f;
                    let imms = (word >> 10) & 0x3f;
                    let immr = (word >> 16) & 0x3f;
                    let N = (word >> 22) & 0x1;

                    let opc = word >> 29 & 0b11;
                    let sf = word >> 31 & 1;

                    let opc = &[
                        Ok(Opcode::SBFM),
                        Ok(Opcode::BFM),
                        Ok(Opcode::UBFM),
                        Err(ErrorKind::InvalidOpcode)
                    ][opc as usize]?;

                    inst.opcode = *opc;

                    let size = if sf == 0 {
                        SizeCode::W
                    } else {
                        SizeCode::X
                    };

                    if sf != N {
                        return Err(ErrorKind::InvalidOpcode);
                    }

                    if sf == 0 && (immr >= 32 || imms >= 32) {
                        return Err(ErrorKind::InvalidOpcode);
                    }

                    inst.operands = [
                        Operand::Register(size, Rd as u16),
                        Operand::Register(size, Rn as u16),
                        Operand::Immediate(immr as u32),
                        Operand::Immediate(imms as u32)
                    ];
                },
                0b111 => {
                    // extract
                    // let Rd = word & 0x1f;
                    // let Rn = (word >> 5) & 0x1f;
                    let imms = (word >> 10) & 0x3f;
                    // let Rm = (word >> 16) & 0x1f;
                    let No0 = (word >> 21) & 0x3;

                    let sf_op21 = word >> 29;

                    let size = if sf_op21 == 0b000 {
                        if No0 != 0b00 || imms >= 0x20 {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOpcode);
                        } else {
                            inst.opcode = Opcode::EXTR;
                            SizeCode::W
                        }
                    } else if sf_op21 == 0b100 {
                        if No0 != 0b10 {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOpcode);
                        } else {
                            inst.opcode = Opcode::EXTR;
                            SizeCode::X
                        }
                    } else {
                        inst.opcode = Opcode::Invalid;
                        return Err(ErrorKind::InvalidOpcode);
                    };

                    let Rd = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rm = ((word >> 16) & 0x1f) as u16;

                    inst.operands = [
                        Operand::Register(size, Rd),
                        Operand::Register(size, Rn),
                        Operand::Register(size, Rm),
                        Operand::Immediate(imms),
                    ];
                }
                _ => { unreachable!("group is three bits") }
            }
        },
        Section::LoadStore => {
            /*
             * This corresponds to section C3.3, Loads and stores.
             * Specifically, instructions in this category are all of the form
             *
             * v _ G G 1 G 0 G G _ v v v v v v _ _ _ _ v v __________
             *
             * where G+v variants indicate which instruction class the word is in.
             *
             * however! the G bits are sufficient to distinguish most class of instruction.
             */

            let group_byte = word >> 23;
            let group_bits = (group_byte & 0x03) | ((group_byte >> 1) & 0x04) | ((group_byte >> 2) & 0x18);

            // println!("Group byte: {:#b}, bits: {:#b}", group_byte, group_bits);
            match group_bits {
                0b00000 => {
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rt2 = ((word >> 10) & 0x1f) as u16;
                    let o0 = (word & 0x8000) >> 15;
                    let Rs = ((word >> 16) & 0x1f) as u16;
                    let Lo1 = (word & 0x600000) >> 21;
                    let size = (word >> 30) & 0x3;
                    // load/store exclusive
                    // o2 == 0
                    if Lo1 & 1 == 1 && size < 0b10 {
                        // o1 == 1
                        if Rt2 != 0b11111 {
                            return Err(ErrorKind::InvalidOperand);
                        }

                        let ar = ((Lo1 & 0b10) | o0) as u8;

                        let size_code = if size & 0b01 == 0 {
                            SizeCode::W
                        } else {
                            SizeCode::X
                        };

                        if Rt & 1 != 0 || Rs & 1 != 0 {
                            return Err(ErrorKind::InvalidOperand);
                        }

                        inst.opcode = Opcode::CASP(ar);
                        inst.operands = [
                            Operand::RegisterPair(size_code, Rs),
                            Operand::RegisterPair(size_code, Rt),
                            Operand::RegPreIndex(Rn, 0, false),
                            Operand::Nothing,
                        ];
                        return Ok(());
                    }
                    inst.opcode = match size {
                        size @ 0b00 |
                        size @ 0b01 => {
                            if Lo1 == 0b00 {
                                // store ops
                                inst.operands = [
                                    Operand::Register(SizeCode::W, Rs),
                                    Operand::Register(SizeCode::W, Rt),
                                    Operand::RegPreIndex(Rn, 0, false),
                                    Operand::Nothing,
                                ];
                            } else if Lo1 == 0b10 {
                                // load ops
                                inst.operands = [
                                    Operand::Register(SizeCode::W, Rt),
                                    Operand::RegPreIndex(Rn, 0, false),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                return Err(ErrorKind::InvalidOpcode);
                            };
                            if size == 0b00 {
                                match (Lo1, o0) {
                                    (0b00, 0b0) => Opcode::STXRB,
                                    (0b00, 0b1) => Opcode::STLXRB,
                                    (0b10, 0b0) => Opcode::LDXRB,
                                    (0b10, 0b1) => Opcode::LDAXRB,
                                    _ => {
                                        inst.opcode = Opcode::Invalid;
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                            } else if size == 0b01 {
                                match (Lo1, o0) {
                                    (0b00, 0b0) => Opcode::STXRH,
                                    (0b00, 0b1) => Opcode::STLXRH,
                                    (0b10, 0b0) => Opcode::LDXRH,
                                    (0b10, 0b1) => Opcode::LDAXRH,
                                    _ => {
                                        inst.opcode = Opcode::Invalid;
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                            } else {
                                unreachable!("size was checked to be 0 or 1");
                            }
                        }
                        size @ 0b10 |
                        size @ 0b11 => {
                            let size_code = match size {
                                0b10 => SizeCode::W,
                                0b11 => SizeCode::X,
                                _ => { unreachable!("size is already known to be 0b10 or 0b11"); }
                            };

                            match Lo1 {
                                0b00 => {
                                    inst.operands = [
                                        Operand::Register(SizeCode::W, Rs),
                                        Operand::Register(size_code, Rt),
                                        Operand::RegPreIndex(Rn, 0, false),
                                        Operand::Nothing,
                                    ];
                                    match o0 {
                                        0b0 => Opcode::STXR,
                                        0b1 => Opcode::STLXR,
                                        _ => { unreachable!("o0 is one bit"); }
                                    }
                                }
                                0b01 => {
                                    inst.operands = [
                                        Operand::Register(SizeCode::W, Rs),
                                        Operand::Register(size_code, Rt),
                                        Operand::Register(size_code, Rt2),
                                        Operand::RegPreIndex(Rn, 0, false),
                                    ];
                                    match o0 {
                                        0b0 => Opcode::STXP,
                                        0b1 => Opcode::STLXP,
                                        _ => { unreachable!("o0 is one bit"); }
                                    }
                                }
                                0b10 => {
                                    inst.operands = [
                                        Operand::Register(size_code, Rt),
                                        Operand::RegPreIndex(Rn, 0, false),
                                        Operand::Nothing,
                                        Operand::Nothing,
                                    ];
                                    match o0 {
                                        0b0 => Opcode::LDXR,
                                        0b1 => Opcode::LDAXR,
                                        _ => { unreachable!("o0 is one bit"); }
                                    }
                                }
                                0b11 => {
                                    inst.operands = [
                                        Operand::Register(size_code, Rt),
                                        Operand::Register(size_code, Rt2),
                                        Operand::RegPreIndex(Rn, 0, false),
                                        Operand::Nothing,
                                    ];
                                    match o0 {
                                        0b0 => Opcode::LDXP,
                                        0b1 => Opcode::LDAXP,
                                        _ => { unreachable!("o0 is one bit"); }
                                    }
                                }
                                _ => { unreachable!("Lo1 is two bits"); }
                            }
                        }
                        _ => {
                            unreachable!("size is two bits");
                        }
                    };
                },
                0b00001 => {
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rt2 = ((word >> 10) & 0x1f) as u16;
                    let o0 = (word >> 15) & 1;
                    let Rs = ((word >> 16) & 0x1f) as u16;
                    let Lo1 = (word >> 21) & 0x3;
                    let size = (word >> 30) & 0x3;
                    // load/store exclusive
                    // o2 == 1
                    if Lo1 & 1 == 1 {
                        // o1 == 1
                        if Rt2 != 0b11111 {
                            return Err(ErrorKind::InvalidOperand);
                        }

                        let ar = ((Lo1 & 0b10) | o0) as u8;

                        let (opcode, size_code) = if size == 0b00 {
                            (Opcode::CASB(ar), SizeCode::W)
                        } else if size == 0b01 {
                            (Opcode::CASH(ar), SizeCode::W)
                        } else if size == 0b10 {
                            (Opcode::CAS(ar), SizeCode::W)
                        } else {
                            (Opcode::CAS(ar), SizeCode::X)
                        };

                        inst.opcode = opcode;
                        inst.operands = [
                            Operand::Register(size_code, Rs),
                            Operand::Register(size_code, Rt),
                            Operand::RegPreIndex(Rn, 0, false),
                            Operand::Nothing,
                        ];
                        return Ok(());
                    }
                    // STLRB -> Wt (Rt) Xn|SP (Rn)
                    // LDARB -> Wt (Rt) Xn|SP (Rn)
                    // STLRH -> Wt (Rt) Xn|SP (Rn)
                    // LDARH -> Wt (Rt) Xn|SP (Rn)
                    // STLR -> Wt (Rt) Xn|SP (Rn)
                    // LDAR -> Wt (Rt) Xn|SP (Rn)
                    // STLR -> Wt (Rt) Xn|SP (Rn)
                    // LDAR -> Wt (Rt) Xn|SP (Rn)
                    inst.opcode = match (size, Lo1, o0) {
                        (0b00, 0b00, 0b0) => Opcode::STLLRB,
                        (0b00, 0b00, 0b1) => Opcode::STLRB,
                        (0b00, 0b10, 0b0) => Opcode::LDLARB,
                        (0b00, 0b10, 0b1) => Opcode::LDARB,
                        (0b01, 0b00, 0b0) => Opcode::STLLRH,
                        (0b01, 0b00, 0b1) => Opcode::STLRH,
                        (0b01, 0b10, 0b0) => Opcode::LDLARH,
                        (0b01, 0b10, 0b1) => Opcode::LDARH,
                        (0b10, 0b00, 0b0) => Opcode::STLLR, // 32-bit
                        (0b10, 0b00, 0b1) => Opcode::STLR, // 32-bit
                        (0b10, 0b10, 0b0) => Opcode::LDLAR, // 32-bit
                        (0b10, 0b10, 0b1) => Opcode::LDAR, // 32-bit
                        (0b11, 0b00, 0b0) => Opcode::STLLR, // 64-bit
                        (0b11, 0b00, 0b1) => Opcode::STLR, // 64-bit
                        (0b11, 0b10, 0b0) => Opcode::LDLAR, // 64-bit
                        (0b11, 0b10, 0b1) => Opcode::LDAR, // 64-bit
                        _ => {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOpcode);
                        }
                    };
                    let size_code = if size == 0b11 {
                        SizeCode::X
                    } else {
                        SizeCode::W
                    };

                    inst.operands = [
                        Operand::Register(size_code, Rt),
                        Operand::RegPreIndex(Rn, 0, false),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                },
                0b01000 |
                0b01001 => {
                    // load register (literal)
                    // V == 0
                    let opc = (word >> 30) & 0x3;
                    let Rt = (word & 0x1f) as u16;
                    let imm19 = ((((word >> 5) & 0x7ffff) as i32) << 13) >> 13;

                    let size = match opc {
                        0b00 => {
                            inst.opcode = Opcode::LDR;
                            SizeCode::W
                        },
                        0b01 => {
                            inst.opcode = Opcode::LDR;
                            SizeCode::X
                        }
                        0b10 => {
                            inst.opcode = Opcode::LDRSW;
                            SizeCode::X
                        }
                        0b11 => {
                            inst.opcode = Opcode::PRFM;
                            SizeCode::X
                        }
                        _ => {
                            unreachable!("opc is two bits");
                        }
                    };

                    inst.operands = [
                        if opc == 0b11 {
                            Operand::PrefetchOp(Rt)
                        } else {
                            Operand::Register(size, Rt)
                        },
                        Operand::PCOffset((imm19 << 2) as i64),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                },
                0b01010 |
                0b01011 => {
                    // `Load/store memory tags`, `Load/store exclusive`
                    let op0 = (word >> 28) & 0b11111;
                    let op3 = (word >> 16) & 0b111111;
                    let op4 = (word >> 10) & 0b11;

                    if op3 & 0b100000 == 0 {
                        if op4 != 0b00 {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                        // else, `LDAPR/STLR (unscaled immediate)`
                        let size = (word >> 30) & 0b11;
                        let opc = (word >> 22) & 0b11;
                        let imm9 = (word >> 12) & 0b111_111_111;
                        let Rn = ((word >> 5) & 0b11111) as u16;
                        let Rt = ((word >> 0) & 0b11111) as u16;

                        let simm = (((imm9 as i16) << 7) >> 7) as i32;
                        let index = (size << 2) | opc;

                        if index == 0b1011 || index >= 0b1110 {
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        let opcode = &[
                            Opcode::STLURB, Opcode::LDAPURB, Opcode::LDAPURSB, Opcode::LDAPURSB,
                            Opcode::STLURH, Opcode::LDAPURH, Opcode::LDAPURSH, Opcode::LDAPURSH,
                            Opcode::STLUR, Opcode::LDAPUR, Opcode::LDAPURSW, Opcode::Invalid,
                            Opcode::STLUR, Opcode::LDAPUR, Opcode::Invalid, Opcode::Invalid,
                        ][index as usize];

                        let size = if size == 0b11 || opc == 0b10 {
                            SizeCode::X
                        } else {
                            SizeCode::W
                        };

                        inst.opcode = *opcode;
                        inst.operands = [
                            Operand::Register(size, Rt),
                            Operand::RegPreIndex(Rn, simm, false),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    } else {
                        if op0 != 0b1101 {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                        // else, `Load/store memory tags`
                        let opc = (word >> 22) & 0b11;
                        let imm9 = (word >> 12) & 0b111_111_111;
                        let op2 = (word >> 10) & 0b11;
                        let Rn = ((word >> 5) & 0b11111) as u16;
                        let Rt = ((word >> 0) & 0b11111) as u16;

                        let simm = (((imm9 as i16) << 7) >> 7) as i32;

                        let opcode = &[
                            Opcode::STZGM, Opcode::STG, Opcode::STG, Opcode::STG,
                            Opcode::LDG, Opcode::STZG, Opcode::STZG, Opcode::STZG,
                            Opcode::STGM, Opcode::ST2G, Opcode::ST2G, Opcode::ST2G,
                            Opcode::LDGM, Opcode::STZ2G, Opcode::STZ2G, Opcode::STZ2G,
                        ][((opc << 2) | op2) as usize];

                        inst.opcode = *opcode;

                        if op2 == 0 && imm9 != 0 {
                            if opc == 0b01 {
                                // ldg
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rt),
                                    Operand::RegPreIndex(Rn, simm, false),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                                return Ok(());
                            } else {
                                return Err(ErrorKind::InvalidOperand);
                            }
                        }

                        inst.operands = [
                            if op2 == 0b00 {
                                Operand::Register(SizeCode::X, Rt)
                            } else {
                                Operand::RegisterOrSP(SizeCode::X, Rt)
                            },
                            if op2 == 0b00 {
                                Operand::RegPreIndex(Rn, 0, false)
                            } else if op2 == 0b01 {
                                Operand::RegPostIndex(Rn, simm)
                            } else if op2 == 0b10 {
                                Operand::RegPreIndex(Rn, simm, false)
                            } else {
                                Operand::RegPreIndex(Rn, simm, true)
                            },
                            Operand::Nothing,
                            Operand::Nothing,
                        ];
                    }
                },
                0b01100 |
                0b01101 => {
                    // load register (literal)
                    // V == 1
                    let opc = (word >> 30) & 0x3;
                    let Rt = (word & 0x1f) as u16;
                    let imm19 = ((((word >> 5) & 0x7ffff) as i32) << 13) >> 13;

                    let size_code = match opc {
                        0b00 => SIMDSizeCode::S,
                        0b01 => SIMDSizeCode::D,
                        0b10 => SIMDSizeCode::Q,
                        0b11 => {
                            // 11011100_XXXXXXXX_XXXXXXXX_XXXXXXXX
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOpcode);
                        }
                        _ => {
                            unreachable!("opc is two bits");
                        }
                    };

                    inst.opcode = Opcode::LDR;
                    inst.operands = [
                        Operand::SIMDRegister(size_code, Rt),
                        Operand::PCOffset((imm19 << 2) as i64),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                },
                0b10000 => {
                    // load/store no-allocate pair (offset)
                    // V == 0
                    let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    const ENCODINGS: &[Result<(Opcode, SizeCode), ErrorKind>] = &[
                        Ok((Opcode::STNP, SizeCode::W)),
                        Ok((Opcode::LDNP, SizeCode::W)),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        Ok((Opcode::STNP, SizeCode::X)),
                        Ok((Opcode::LDNP, SizeCode::X)),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                    ];

                    let (opcode, size) = ENCODINGS[opc_L as usize]?;
                    inst.opcode = opcode;

                    let scale = if size == SizeCode::W {
                        2
                    } else {
                        3
                    };

                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rt2 = ((word >> 10) & 0x1f) as u16;
                    let imm7 = ((word >> 15) & 0x7f) as i16;
                    let imm7 = (imm7 << 9) >> 9;

                    inst.operands = [
                        Operand::Register(size, Rt),
                        Operand::Register(size, Rt2),
                        Operand::RegPreIndex(Rn, (imm7 as i32) << scale, false),
                        Operand::Nothing,
                    ];
                },
                0b10100 => {
                    // load/store no-allocate pair (offset)
                    // V == 1
                    let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    const ENCODINGS: &[Result<(Opcode, SIMDSizeCode), ErrorKind>] = &[
                        Ok((Opcode::STNP, SIMDSizeCode::S)),
                        Ok((Opcode::LDNP, SIMDSizeCode::S)),
                        Ok((Opcode::STNP, SIMDSizeCode::D)),
                        Ok((Opcode::LDNP, SIMDSizeCode::D)),
                        Ok((Opcode::STNP, SIMDSizeCode::Q)),
                        Ok((Opcode::LDNP, SIMDSizeCode::Q)),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                    ];

                    let (opcode, size) = ENCODINGS[opc_L as usize]?;
                    inst.opcode = opcode;

                    let scale = if size == SIMDSizeCode::S {
                        2
                    } else if size == SIMDSizeCode::D {
                        3
                    } else {
                        4
                    };

                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rt2 = ((word >> 10) & 0x1f) as u16;
                    let imm7 = ((word >> 15) & 0x7f) as i16;
                    let imm7 = (imm7 << 9) >> 9;

                    inst.operands = [
                        Operand::SIMDRegister(size, Rt),
                        Operand::SIMDRegister(size, Rt2),
                        Operand::RegPreIndex(Rn, (imm7 as i32) << scale, false),
                        Operand::Nothing,
                    ];
                },
                0b10001 => {
                    // load/store register pair (post-indexed)
                    // V == 0
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rt2 = ((word >> 10) & 0x1f) as u16;
                    let mut imm7 = ((((word >> 15) & 0x7f) as i16) << 9) >> 9;
                    let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    let size = match opc_L {
                        0b000 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 2;
                            SizeCode::W
                        },
                        0b001 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 2;
                            SizeCode::W
                        },
                        0b010 => {
                            inst.opcode = Opcode::Invalid;
                            SizeCode::W
                        },
                        0b011 => {
                            inst.opcode = Opcode::LDPSW;
                            imm7 <<= 2;
                            SizeCode::X
                        },
                        0b100 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 3;
                            SizeCode::X
                        },
                        0b101 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 3;
                            SizeCode::X
                        },
                        0b110 |
                        0b111 => {
                            inst.opcode = Opcode::Invalid;
                            SizeCode::X
                        }
                        _ => { unreachable!("opc and L are three bits"); }
                    };
                    inst.operands = [
                        Operand::Register(size, Rt),
                        Operand::Register(size, Rt2),
                        Operand::RegPostIndex(Rn, imm7 as i32),
                        Operand::Nothing
                    ];
                },
                0b10101 => {
                    // load/store register pair (post-indexed)
                    // V == 1
                    // let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rt2 = ((word >> 10) & 0x1f) as u16;
                    let mut imm7 = ((((word >> 15) & 0x7f) as i16) << 9) >> 9;
                    let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    let size = match opc_L {
                        0b000 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 2;
                            SIMDSizeCode::S
                        },
                        0b001 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 2;
                            SIMDSizeCode::S
                        },
                        0b010 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 3;
                            SIMDSizeCode::D
                        },
                        0b011 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 3;
                            SIMDSizeCode::D
                        },
                        0b100 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 4;
                            SIMDSizeCode::Q
                        },
                        0b101 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 4;
                            SIMDSizeCode::Q
                        },
                        0b110 |
                        0b111 => {
                            inst.opcode = Opcode::Invalid;
                            SIMDSizeCode::Q
                        }
                        _ => { unreachable!("opc and L are three bits"); }
                    };
                    inst.operands = [
                        Operand::SIMDRegister(size, Rt),
                        Operand::SIMDRegister(size, Rt2),
                        Operand::RegPostIndex(Rn, imm7 as i32),
                        Operand::Nothing
                    ];
                },
                0b10010 => {
                    // load/store register pair (offset)
                    // V == 0
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rt2 = ((word >> 10) & 0x1f) as u16;
                    let mut imm7 = (((((word >> 15) & 0x7f) as i16) << 9) >> 9) as i32;
                    let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    let size = match opc_L {
                        0b000 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 2;
                            SizeCode::W
                        },
                        0b001 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 2;
                            SizeCode::W
                        },
                        0b010 => {
                            inst.opcode = Opcode::Invalid;
                            SizeCode::W
                        },
                        0b011 => {
                            inst.opcode = Opcode::LDPSW;
                            imm7 <<= 2;
                            SizeCode::X
                        },
                        0b100 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 3;
                            SizeCode::X
                        },
                        0b101 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 3;
                            SizeCode::X
                        },
                        0b110 |
                        0b111 => {
                            inst.opcode = Opcode::Invalid;
                            SizeCode::X
                        }
                        _ => { unreachable!("opc and L are three bits"); }
                    };
                    inst.operands = [
                        Operand::Register(size, Rt),
                        Operand::Register(size, Rt2),
                        Operand::RegPreIndex(Rn, imm7, false),
                        Operand::Nothing
                    ];
                },
                0b10110 => {
                    // load/store register pair (offset)
                    // V == 1
                    // let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rt2 = ((word >> 10) & 0x1f) as u16;
                    let mut imm7 = ((((word >> 15) & 0x7f) as i16) << 9) >> 9;
                    let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    let size = match opc_L {
                        0b000 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 2;
                            SIMDSizeCode::S
                        },
                        0b001 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 2;
                            SIMDSizeCode::S
                        },
                        0b010 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 3;
                            SIMDSizeCode::D
                        },
                        0b011 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 3;
                            SIMDSizeCode::D
                        },
                        0b100 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 4;
                            SIMDSizeCode::Q
                        },
                        0b101 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 4;
                            SIMDSizeCode::Q
                        },
                        0b110 |
                        0b111 => {
                            inst.opcode = Opcode::Invalid;
                            SIMDSizeCode::Q
                        }
                        _ => { unreachable!("opc and L are three bits"); }
                    };
                    inst.operands = [
                        Operand::SIMDRegister(size, Rt),
                        Operand::SIMDRegister(size, Rt2),
                        Operand::RegPreIndex(Rn, imm7 as i32, false),
                        Operand::Nothing
                    ];
                },
                0b10011 => {
                    // load/store register pair (pre-indexed)
                    // V == 0
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rt2 = ((word >> 10) & 0x1f) as u16;
                    let mut imm7 = ((((word >> 15) & 0x7f) as i16) << 9) >> 9;
                    let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    let size = match opc_L {
                        0b000 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 2;
                            SizeCode::W
                        },
                        0b001 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 2;
                            SizeCode::W
                        },
                        0b010 => {
                            inst.opcode = Opcode::Invalid;
                            SizeCode::W
                        },
                        0b011 => {
                            inst.opcode = Opcode::LDPSW;
                            imm7 <<= 2;
                            SizeCode::X
                        },
                        0b100 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 3;
                            SizeCode::X
                        },
                        0b101 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 3;
                            SizeCode::X
                        },
                        0b110 |
                        0b111 => {
                            inst.opcode = Opcode::Invalid;
                            SizeCode::X
                        }
                        _ => { unreachable!("opc and L are three bits"); }
                    };
                    inst.operands = [
                        Operand::Register(size, Rt),
                        Operand::Register(size, Rt2),
                        Operand::RegPreIndex(Rn, imm7 as i32, true),
                        Operand::Nothing
                    ];
                },
                0b10111 => {
                    // load/store register pair (pre-indexed)
                    // V == 1
                    // let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let Rt2 = ((word >> 10) & 0x1f) as u16;
                    let mut imm7 = ((((word >> 15) & 0x7f) as i16) << 9) >> 9;
                    let opc_L = ((word >> 22) & 1) | ((word >> 29) & 0x6);
                    let size = match opc_L {
                        0b000 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 2;
                            SIMDSizeCode::S
                        },
                        0b001 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 2;
                            SIMDSizeCode::S
                        },
                        0b010 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 3;
                            SIMDSizeCode::D
                        },
                        0b011 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 3;
                            SIMDSizeCode::D
                        },
                        0b100 => {
                            inst.opcode = Opcode::STP;
                            imm7 <<= 4;
                            SIMDSizeCode::Q
                        },
                        0b101 => {
                            inst.opcode = Opcode::LDP;
                            imm7 <<= 4;
                            SIMDSizeCode::Q
                        },
                        0b110 |
                        0b111 => {
                            inst.opcode = Opcode::Invalid;
                            SIMDSizeCode::Q
                        }
                        _ => { unreachable!("opc and L are three bits"); }
                    };
                    inst.operands = [
                        Operand::SIMDRegister(size, Rt),
                        Operand::SIMDRegister(size, Rt2),
                        Operand::RegPreIndex(Rn, imm7 as i32, true),
                        Operand::Nothing
                    ];
                },
                0b11000 |
                0b11001 => {
                    /*
                     * load/store register {unscaled immediate, immediate post-indexed,
                     * unprivileged, immediate pre-indexd, register offset, pac}
                     * atomic memory operations
                     * V == 0
                     */
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let size_opc = ((word >> 22) & 0x03) | ((word >> 28) & 0x0c);
                    let category = (word >> 10) & 0x03;
                    // println!("load/store: size_opc: {:#b}, category: {:#b}", size_opc, category);
                    if word & 0x200000 != 0 {
                        match category {
                            0b00 => {
                                // Atomic memory operations
                                // V==0

                                // A and R bits mean... "acquire" and "release"
                                let sz = (word >> 30) & 0b11;
                                let ar = ((word >> 22) & 0b11) as u8;
                                let o3 = (word >> 15) & 1;
                                let opcode = (word >> 12) & 0b111;
                                let Rs = (word >> 16) & 0b11111;

                                let size = if sz == 0b11 {
                                    SizeCode::X
                                } else {
                                    SizeCode::W
                                };

                                if o3 == 0 {
                                    let opc = match (sz << 3) | opcode {
                                        0b00_000 => Opcode::LDADDB(ar),
                                        0b00_001 => Opcode::LDCLRB(ar),
                                        0b00_010 => Opcode::LDEORB(ar),
                                        0b00_011 => Opcode::LDSETB(ar),
                                        0b00_100 => Opcode::LDSMAXB(ar),
                                        0b00_101 => Opcode::LDSMINB(ar),
                                        0b00_110 => Opcode::LDUMAXB(ar),
                                        0b00_111 => Opcode::LDUMINB(ar),
                                        0b01_000 => Opcode::LDADDH(ar),
                                        0b01_001 => Opcode::LDCLRH(ar),
                                        0b01_010 => Opcode::LDEORH(ar),
                                        0b01_011 => Opcode::LDSETH(ar),
                                        0b01_100 => Opcode::LDSMAXH(ar),
                                        0b01_101 => Opcode::LDSMINH(ar),
                                        0b01_110 => Opcode::LDUMAXH(ar),
                                        0b01_111 => Opcode::LDUMINH(ar),
                                        0b10_000 => Opcode::LDADD(ar),
                                        0b10_001 => Opcode::LDCLR(ar),
                                        0b10_010 => Opcode::LDEOR(ar),
                                        0b10_011 => Opcode::LDSET(ar),
                                        0b10_100 => Opcode::LDSMAX(ar),
                                        0b10_101 => Opcode::LDSMIN(ar),
                                        0b10_110 => Opcode::LDUMAX(ar),
                                        0b10_111 => Opcode::LDUMIN(ar),
                                        0b11_000 => Opcode::LDADD(ar),
                                        0b11_001 => Opcode::LDCLR(ar),
                                        0b11_010 => Opcode::LDEOR(ar),
                                        0b11_011 => Opcode::LDSET(ar),
                                        0b11_100 => Opcode::LDSMAX(ar),
                                        0b11_101 => Opcode::LDSMIN(ar),
                                        0b11_110 => Opcode::LDUMAX(ar),
                                        0b11_111 => Opcode::LDUMIN(ar),
                                        _ => { unreachable!("sz|opc is 5 bits") }
                                    };
                                    inst.opcode = opc;
                                    inst.operands = [
                                        Operand::Register(size, Rs as u16),
                                        Operand::Register(size, Rt),
                                        Operand::RegPreIndex(Rn, 0, false),
                                        Operand::Nothing,
                                    ];
                                } else {
                                    if opcode == 0b000 {
                                        inst.opcode = if sz == 0b00 {
                                            Opcode::SWPB(ar)
                                        } else if sz == 0b01 {
                                            Opcode::SWPH(ar)
                                        } else if sz == 0b10 {
                                            Opcode::SWP(ar)
                                        } else {
                                            Opcode::SWP(ar)
                                        };

                                        inst.operands = [
                                            Operand::Register(size, Rs as u16),
                                            Operand::Register(size, Rt),
                                            Operand::RegPreIndex(Rn, 0, false),
                                            Operand::Nothing,
                                        ];
                                    } else if opcode == 0b100 {
                                        if ar != 0b10 {
                                            return Err(ErrorKind::InvalidOpcode);
                                        }
                                        inst.opcode = if sz == 0b01 {
                                            Opcode::LDAPRH
                                        } else if sz == 0b00 {
                                            Opcode::LDAPRB
                                        } else {
                                            // sz = 1x
                                            Opcode::LDAPR
                                        };

                                        // TOOD: should_is_must, Rs = 11111
                                        inst.operands = [
                                            Operand::Register(size, Rt),
                                            Operand::RegPreIndex(Rn, 0, false),
                                            Operand::Nothing,
                                            Operand::Nothing,
                                        ];
                                    } else {
                                        return Err(ErrorKind::InvalidOpcode);
                                    }
                                }
                            }
                            0b01 |
                            0b11 => {
                                // Load/store register (pac)
                                // V==0
                                let w = (word >> 11) & 1;
                                let imm9 = ((word >> 12) & 0b1_1111_1111) as i16;
                                let S = ((word >> 22) & 1) as i16;
                                let imm10 = imm9 | (S << 9);
                                let imm10 = imm10 << 3; // `simm` is stored as a multiple of 8
                                let imm10 = ((imm10 << 3) >> 3) as i32;
                                let m = (word >> 23) & 1;
                                let size = (word >> 30) & 0b11;
                                if size != 0b11 {
                                    return Err(ErrorKind::InvalidOpcode);
                                }

                                if m == 0 {
                                    inst.opcode = Opcode::LDRAA;
                                } else {
                                    inst.opcode = Opcode::LDRAB;
                                }

                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rt),
                                    if w == 0 {
                                        Operand::RegPreIndex(Rn, imm10, false)
                                    } else {
                                        Operand::RegPreIndex(Rn, imm10, true)
                                    },
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            0b10 => {
                                // Load/store register (register offset)
                                // C3.3.10
                                const OPCODES: &[Result<(Opcode, SizeCode, u8), ErrorKind>] = &[
                                    Ok((Opcode::STRB, SizeCode::W, 0)),
                                    Ok((Opcode::LDRB, SizeCode::W, 0)),
                                    Ok((Opcode::LDRSB, SizeCode::X, 0)),
                                    Ok((Opcode::LDRSB, SizeCode::W, 0)),
                                    Ok((Opcode::STRH, SizeCode::W, 1)),
                                    Ok((Opcode::LDRH, SizeCode::W, 1)),
                                    Ok((Opcode::LDRSH, SizeCode::X, 1)),
                                    Ok((Opcode::LDRSH, SizeCode::W, 1)),
                                    Ok((Opcode::STR, SizeCode::W, 2)),
                                    Ok((Opcode::LDR, SizeCode::W, 2)),
                                    Ok((Opcode::LDRSW, SizeCode::X, 2)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::STR, SizeCode::X, 3)),
                                    Ok((Opcode::LDR, SizeCode::X, 3)),
                                    Ok((Opcode::PRFM, SizeCode::X, 3)),
                                    Err(ErrorKind::InvalidOpcode),
                                ];

                                let (opcode, size, shamt) = OPCODES.get(size_opc as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;
                                inst.opcode = opcode;

                                let S = ((word >> 12) & 0x1) as u8;
                                let option = ((word >> 13) & 0x07) as u8;
                                let Rm = ((word >> 16) & 0x1f) as u16;

                                let index_size = match option & 0x3 {
                                    0b00 |
                                    0b01 => {
                                        inst.opcode = Opcode::Invalid;
                                        return Err(ErrorKind::InvalidOpcode);
                                    },
                                    0b10 => { SizeCode::W }
                                    0b11 => { SizeCode::X }
                                    _ => { unreachable!("option is two bits"); }
                                };

                                const SHIFT_STYLES: &[Result<ShiftStyle, ErrorKind>] = &[
                                    Err(ErrorKind::InvalidOperand),
                                    Err(ErrorKind::InvalidOperand),
                                    Ok(ShiftStyle::UXTW),
                                    Ok(ShiftStyle::LSL),
                                    Err(ErrorKind::InvalidOperand),
                                    Err(ErrorKind::InvalidOperand),
                                    Ok(ShiftStyle::SXTW),
                                    Ok(ShiftStyle::SXTX),
                                ];
                                let shift_style = SHIFT_STYLES[option as usize]?;

                                inst.operands = [
                                    if size_opc == 0b1110 {
                                        Operand::PrefetchOp(Rt)
                                    } else {
                                        Operand::Register(size, Rt)
                                    },
                                    Operand::RegRegOffset(Rn, Rm, index_size, shift_style, S * shamt),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            _ => {
                                unreachable!("category is two bits");
                            }
                        }
                    } else {
                        let imm9 = (((((word >> 12) & 0x1ff) as i16) << 7) >> 7) as i32;
                        match category {
                            0b00 => {
                                // Load/store register (unscaled immediate)
                                const OPCODES: &[Result<(Opcode, SizeCode), ErrorKind>] = &[
                                    Ok((Opcode::STURB, SizeCode::W)),
                                    Ok((Opcode::LDURB, SizeCode::W)),
                                    Ok((Opcode::LDURSB, SizeCode::X)),
                                    Ok((Opcode::LDURSB, SizeCode::W)),
                                    Ok((Opcode::STURH, SizeCode::W)),
                                    Ok((Opcode::LDURH, SizeCode::W)),
                                    Ok((Opcode::LDURSH, SizeCode::X)),
                                    Ok((Opcode::LDURSH, SizeCode::W)),
                                    Ok((Opcode::STUR, SizeCode::W)),
                                    Ok((Opcode::LDUR, SizeCode::W)),
                                    Ok((Opcode::LDURSW, SizeCode::X)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::STUR, SizeCode::X)),
                                    Ok((Opcode::LDUR, SizeCode::X)),
                                    Ok((Opcode::PRFUM, SizeCode::X)),
                                    Err(ErrorKind::InvalidOpcode),
                                ];

                                let (opcode, size) = OPCODES.get(size_opc as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;
                                inst.opcode = opcode;

                                inst.operands = [
                                    if size_opc == 0b1110 {
                                        Operand::PrefetchOp(Rt)
                                    } else {
                                        Operand::Register(size, Rt)
                                    },
                                    Operand::RegPreIndex(Rn, imm9, false),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            0b10 => {
                                // Load/store register (unprivileged)
                                const OPCODES: &[Result<(Opcode, SizeCode), ErrorKind>] = &[
                                    Ok((Opcode::STTRB, SizeCode::W)),
                                    Ok((Opcode::LDTRB, SizeCode::W)),
                                    Ok((Opcode::LDTRSB, SizeCode::X)),
                                    Ok((Opcode::LDTRSB, SizeCode::W)),
                                    Ok((Opcode::STTRH, SizeCode::W)),
                                    Ok((Opcode::LDTRH, SizeCode::W)),
                                    Ok((Opcode::LDTRSH, SizeCode::X)),
                                    Ok((Opcode::LDTRSH, SizeCode::W)),
                                    Ok((Opcode::STTR, SizeCode::W)),
                                    Ok((Opcode::LDTR, SizeCode::W)),
                                    Ok((Opcode::LDTRSW, SizeCode::X)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::STTR, SizeCode::X)),
                                    Ok((Opcode::LDTR, SizeCode::X)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                ];

                                let (opcode, size) = OPCODES.get(size_opc as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;
                                inst.opcode = opcode;

                                inst.operands = [
                                    Operand::Register(size, Rt),
                                    Operand::RegPreIndex(Rn, imm9, false),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            0b01 |
                            0b11 => {
                                const OPCODES: &[Result<(Opcode, SizeCode), ErrorKind>] = &[
                                    Ok((Opcode::STRB, SizeCode::W)),
                                    Ok((Opcode::LDRB, SizeCode::W)),
                                    Ok((Opcode::LDRSB, SizeCode::X)),
                                    Ok((Opcode::LDRSB, SizeCode::W)),
                                    Ok((Opcode::STRH, SizeCode::W)),
                                    Ok((Opcode::LDRH, SizeCode::W)),
                                    Ok((Opcode::LDRSH, SizeCode::X)),
                                    Ok((Opcode::LDRSH, SizeCode::W)),
                                    Ok((Opcode::STR, SizeCode::W)),
                                    Ok((Opcode::LDR, SizeCode::W)),
                                    Ok((Opcode::LDRSW, SizeCode::X)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Ok((Opcode::STR, SizeCode::X)),
                                    Ok((Opcode::LDR, SizeCode::X)),
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                ];

                                let (opcode, size) = OPCODES.get(size_opc as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;
                                inst.opcode = opcode;

                                inst.operands = [
                                    Operand::Register(size, Rt),
                                    if category == 0b01 {
                                        Operand::RegPostIndex(Rn, imm9)
                                    } else {
                                        Operand::RegPreIndex(Rn, imm9, true)
                                    },
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            },
                            _ => {
                                unreachable!("category is two bits");
                            }
                        }
                    }
                }
                0b11100 |
                0b11101 => {
                    /*
                     * load/store register {unscaled immediate, immediate post-indexed,
                     * unprivileged, immediate pre-indexed, register offset, pac}
                     * also atomic memory operations
                     * V == 1
                     */
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let size_opc = ((word >> 22) & 0x03) | ((word >> 28) & 0x0c);

                    let op4 = ((word >> 16) & 0x3f) as u8;
                    let op5 = ((word >> 10) & 0x03) as u8;
                    if op4 < 0b10_0000 {
                        let imm9 = ((((word >> 12) & 0x1ff) as i16) << 7) >> 7;
                        match op5 {
                            0b00 => {
                                // Load/store register (unscaled immediate)
                                // V == 1
                                const OPCODES: &[Result<(Opcode, SIMDSizeCode), ErrorKind>] = &[
                                    Ok((Opcode::STUR, SIMDSizeCode::B)),
                                    Ok((Opcode::LDUR, SIMDSizeCode::B)),
                                    Ok((Opcode::STUR, SIMDSizeCode::Q)),
                                    Ok((Opcode::LDUR, SIMDSizeCode::Q)),
                                    // 01 1 0x
                                    Ok((Opcode::STUR, SIMDSizeCode::H)),
                                    Ok((Opcode::LDUR, SIMDSizeCode::H)),
                                    // 01 1 1x
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    // 10 1 0x
                                    Ok((Opcode::STUR, SIMDSizeCode::S)),
                                    Ok((Opcode::LDUR, SIMDSizeCode::S)),
                                    // 10 1 1x
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    // 11 1 0x
                                    Ok((Opcode::STUR, SIMDSizeCode::D)),
                                    Ok((Opcode::LDUR, SIMDSizeCode::D)),
                                    // 11 1 1x
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                ];

                                let (opcode, size) = OPCODES.get(size_opc as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;
                                inst.opcode = opcode;

                                inst.operands = [
                                    Operand::SIMDRegister(size, Rt),
                                    Operand::RegPreIndex(Rn, imm9 as i32, false),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            0b10 => {
                                // Load/store register (unprivileged)
                                // V == 1
                                // this entire section in the manual is currently `Unallocated`
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            0b01 |
                            0b11 => {
                                // Load/store register (immediate post-indexed)
                                // and
                                // Load/store register (immediate pre-indexed)
                                // V == 1
                                let pre_or_post = (word >> 11) & 0x01;
                                const OPCODES: &[Result<(Opcode, SIMDSizeCode), ErrorKind>] = &[
                                    Ok((Opcode::STR, SIMDSizeCode::B)),
                                    Ok((Opcode::LDR, SIMDSizeCode::B)),
                                    Ok((Opcode::STR, SIMDSizeCode::Q)),
                                    Ok((Opcode::LDR, SIMDSizeCode::Q)),
                                    // 01 1 0x
                                    Ok((Opcode::STR, SIMDSizeCode::H)),
                                    Ok((Opcode::LDR, SIMDSizeCode::H)),
                                    // 01 1 1x
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    // 10 1 0x
                                    Ok((Opcode::STR, SIMDSizeCode::S)),
                                    Ok((Opcode::LDR, SIMDSizeCode::S)),
                                    // 10 1 1x
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    // 11 1 0x
                                    Ok((Opcode::STR, SIMDSizeCode::D)),
                                    Ok((Opcode::LDR, SIMDSizeCode::D)),
                                    // 11 1 1x
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                ];

                                let (opcode, size) = OPCODES.get(size_opc as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;
                                inst.opcode = opcode;

                                inst.operands = [
                                    Operand::SIMDRegister(size, Rt),
                                    if pre_or_post == 1 {
                                        Operand::RegPreIndex(Rn, imm9 as i32, true)
                                    } else {
                                        Operand::RegPostIndex(Rn, imm9 as i32)
                                    },
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            _ => {
                                unreachable!("op5 is two bits");
                            }
                        }
                    } else {
                        match op5 {
                            0b00 => {
                                // Atomic memory operations
                                // V == 1
                                // all `V == 1` atomic operations are unallocated.
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            0b10 => {
                                // Load/store register (register offset)
                                // V == 1
                                const OPCODES: &[Result<(Opcode, SIMDSizeCode), ErrorKind>] = &[
                                    // 00 1 00
                                    Ok((Opcode::STR, SIMDSizeCode::B)),
                                    // 00 1 01
                                    Ok((Opcode::LDR, SIMDSizeCode::B)),
                                    // 00 1 10
                                    Ok((Opcode::STR, SIMDSizeCode::Q)),
                                    // 00 1 11
                                    Ok((Opcode::LDR, SIMDSizeCode::Q)),
                                    // 01 1 00
                                    Ok((Opcode::STR, SIMDSizeCode::H)),
                                    Ok((Opcode::LDR, SIMDSizeCode::H)),
                                    // 01 1 1x
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    // 10 1 0x
                                    Ok((Opcode::STR, SIMDSizeCode::S)),
                                    Ok((Opcode::LDR, SIMDSizeCode::S)),
                                    // 10 1 1x
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                    // 11 1 0x
                                    Ok((Opcode::STR, SIMDSizeCode::D)),
                                    Ok((Opcode::LDR, SIMDSizeCode::D)),
                                    // 11 1 1x
                                    Err(ErrorKind::InvalidOpcode),
                                    Err(ErrorKind::InvalidOpcode),
                                ];

                                let option = (word >> 13) & 0x07;
                                let Rt = word & 0x1f;
                                let Rn = (word >> 5) & 0x1f;
                                let Rm = (word >> 16) & 0x1f;
                                let S = (word >> 12) & 0x01;

                                let (opcode, size) = OPCODES.get(size_opc as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;
                                let shift_amount = match size {
                                    SIMDSizeCode::B => 0,
                                    SIMDSizeCode::H => 1,
                                    SIMDSizeCode::S => 2,
                                    SIMDSizeCode::D => 3,
                                    SIMDSizeCode::Q => 4,
                                };

                                let extend = if size == SIMDSizeCode::B && option == 0b011 {
                                    ShiftStyle::LSL
                                } else {
                                    const SHIFT_TYPES: &[ShiftStyle] = &[
                                        ShiftStyle::UXTB, ShiftStyle::UXTH, ShiftStyle::UXTW, ShiftStyle::LSL,
                                        ShiftStyle::SXTB, ShiftStyle::SXTH, ShiftStyle::SXTW, ShiftStyle::SXTX,
                                    ];
                                    SHIFT_TYPES[option as usize]
                                };

                                inst.opcode = opcode;
                                inst.operands = [
                                    Operand::SIMDRegister(size, Rt as u16),
                                    Operand::RegRegOffset(Rn as u16, Rm as u16, if option & 0x01 == 0 {
                                        SizeCode::W
                                    } else {
                                        SizeCode::X
                                    }, extend, if S != 0 { shift_amount } else { 0 }),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            _ => {
                                // Load/store register (pac)
                                // not clearly named in a table but looks like all `V == 1` are
                                // unallocated.
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        }
                    }
                }
                0b11010 |
                0b11011 => {
                    // load/store register (unsigned immediate)
                    // V == 0
                    let Rt = (word & 0x1f) as u16;
                    let Rn = ((word >> 5) & 0x1f) as u16;
                    let imm12 = ((word >> 10) & 0x0fff) as i16 as i32;
                    let size_opc = ((word >> 22) & 0x3) | ((word >> 28) & 0xc);
                    match size_opc {
                        0b0000 => {
                            inst.opcode = Opcode::STRB;
                            inst.operands = [
                                Operand::Register(SizeCode::W, Rt),
                                Operand::RegPreIndex(Rn, imm12, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0001 => {
                            inst.opcode = Opcode::LDRB;
                            inst.operands = [
                                Operand::Register(SizeCode::W, Rt),
                                Operand::RegPreIndex(Rn, imm12, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0010 => {
                            inst.opcode = Opcode::LDRSB;
                            inst.operands = [
                                Operand::Register(SizeCode::X, Rt),
                                Operand::RegPreIndex(Rn, imm12, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0011 => {
                            inst.opcode = Opcode::LDRSB;
                            inst.operands = [
                                Operand::Register(SizeCode::W, Rt),
                                Operand::RegPreIndex(Rn, imm12, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0100 => {
                            inst.opcode = Opcode::STRH;
                            inst.operands = [
                                Operand::Register(SizeCode::W, Rt),
                                Operand::RegPreIndex(Rn, imm12 << 1, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0101 => {
                            inst.opcode = Opcode::LDRH;
                            inst.operands = [
                                Operand::Register(SizeCode::W, Rt),
                                Operand::RegPreIndex(Rn, imm12 << 1, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0110 => {
                            inst.opcode = Opcode::LDRSH;
                            inst.operands = [
                                Operand::Register(SizeCode::X, Rt),
                                Operand::RegPreIndex(Rn, imm12 << 1, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b0111 => {
                            inst.opcode = Opcode::LDRSH;
                            inst.operands = [
                                Operand::Register(SizeCode::W, Rt),
                                Operand::RegPreIndex(Rn, imm12 << 1, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b1000 => {
                            inst.opcode = Opcode::STR;
                            inst.operands = [
                                Operand::Register(SizeCode::W, Rt),
                                Operand::RegPreIndex(Rn, imm12 << 2, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b1001 => {
                            inst.opcode = Opcode::LDR;
                            inst.operands = [
                                Operand::Register(SizeCode::W, Rt),
                                Operand::RegPreIndex(Rn, imm12 << 2, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b1010 => {
                            inst.opcode = Opcode::LDRSW;
                            inst.operands = [
                                Operand::Register(SizeCode::X, Rt),
                                Operand::RegPreIndex(Rn, imm12 << 2, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b1011 => {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOpcode);
                        }
                        0b1100 => {
                            inst.opcode = Opcode::STR;
                            inst.operands = [
                                Operand::Register(SizeCode::X, Rt),
                                Operand::RegPreIndex(Rn, imm12 << 3, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b1101 => {
                            inst.opcode = Opcode::LDR;
                            inst.operands = [
                                Operand::Register(SizeCode::X, Rt),
                                Operand::RegPreIndex(Rn, imm12 << 3, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b1110 => {
                            inst.opcode = Opcode::PRFM;
                            inst.operands = [
                                Operand::PrefetchOp(Rt),
                                Operand::RegPreIndex(Rn, imm12 << 3, false),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        }
                        0b1111 => {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOpcode);
                        }
                        _ => { unreachable!("size and opc are four bits"); }
                    }
                },
                0b11110 |
                0b11111 => {
                    // load/store register (unsigned immediate)
                    // V == 1
                    let Rt = word & 0x1f;
                    let Rn = (word >> 5) & 0x1f;
                    let imm12 = (word >> 10) & 0xfff;
                    let opc = (word >> 22) & 0x3;

                    let size_opc = ((word >> 28) & 0x0c) | opc;

                    const OPCODES: &[Result<(Opcode, SIMDSizeCode), ErrorKind>] = &[
                        // 00 1 00
                        Ok((Opcode::STR, SIMDSizeCode::B)),
                        // 00 1 01
                        Ok((Opcode::LDR, SIMDSizeCode::B)),
                        // 00 1 10
                        Ok((Opcode::STR, SIMDSizeCode::Q)),
                        // 00 1 11
                        Ok((Opcode::LDR, SIMDSizeCode::Q)),
                        // 01 1 00
                        Ok((Opcode::STR, SIMDSizeCode::H)),
                        Ok((Opcode::LDR, SIMDSizeCode::H)),
                        // 01 1 1x
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        // 10 1 0x
                        Ok((Opcode::STR, SIMDSizeCode::S)),
                        Ok((Opcode::LDR, SIMDSizeCode::S)),
                        // 10 1 1x
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        // 11 1 0x
                        Ok((Opcode::STR, SIMDSizeCode::D)),
                        Ok((Opcode::LDR, SIMDSizeCode::D)),
                        // 11 1 1x
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                    ];

                    let (opc, size) = OPCODES.get(size_opc as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;

                    let offset_scale = match size {
                        SIMDSizeCode::B => 1,
                        SIMDSizeCode::H => 2,
                        SIMDSizeCode::S => 4,
                        SIMDSizeCode::D => 8,
                        SIMDSizeCode::Q => 16,
                    };

                    inst.opcode = opc;
                    inst.operands = [
                        Operand::SIMDRegister(size, Rt as u16),
                        Operand::RegPreIndex(Rn as u16, (imm12 * offset_scale) as i32, false),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                },
                0b00100 => {
                    // AdvSIMD load/store multiple structures
                    let Rt = word & 0x1f;
                    let Rn = (word >> 5) & 0x1f;
                    let size = (word >> 10) & 0x03;
                    let opcode_bits = (word >> 12) & 0x0f;
                    let Rm = (word >> 16) & 0x1f;
                    if Rm != 0 {
                        return Err(ErrorKind::InvalidOperand);
                    }
                    let L = (word >> 22) & 0x01;
                    let Q = (word >> 30) & 0x01;
                    let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                    const OPCODES: &[Result<(Opcode, u8), ErrorKind>] = &[
                        // opcode == 0b0000
                        Ok((Opcode::ST4, 4)),
                        Err(ErrorKind::InvalidOpcode),
                        Ok((Opcode::ST1, 4)),
                        Err(ErrorKind::InvalidOpcode),
                        // opcode == 0b0100
                        Ok((Opcode::ST3, 3)),
                        Err(ErrorKind::InvalidOpcode),
                        Ok((Opcode::ST1, 3)),
                        Ok((Opcode::ST1, 1)),
                        // opcode == 0b1000
                        Ok((Opcode::ST2, 2)),
                        Err(ErrorKind::InvalidOpcode),
                        Ok((Opcode::ST1, 2)),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                    ];

                    let (opcode, num_regs) = OPCODES[opcode_bits as usize]?;

                    inst.opcode = if L == 0 {
                        opcode
                    } else {
                        if opcode == Opcode::ST1 {
                            Opcode::LD1
                        } else if opcode == Opcode::ST2 {
                            Opcode::LD2
                        } else if opcode == Opcode::ST3 {
                            Opcode::LD3
                        } else {
                            Opcode::LD4
                        }
                    };
                    const SIZES: [SIMDSizeCode; 4] = [
                        SIMDSizeCode::B,
                        SIMDSizeCode::H,
                        SIMDSizeCode::S,
                        SIMDSizeCode::D,
                    ];
                    inst.operands = [
                        Operand::SIMDRegisterGroup(datasize, Rt as u16, SIZES[size as usize], num_regs),
                        Operand::RegPreIndex(Rn as u16, 0, false),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                },
                0b00101 => {
                    // AdvSIMD load/store multiple structures (post-indexed)
                    let Rt = word & 0x1f;
                    let Rn = (word >> 5) & 0x1f;
                    let size = (word >> 10) & 0x03;
                    let opcode_bits = (word >> 12) & 0x0f;
                    let Rm = (word >> 16) & 0x1f;
                    let L = (word >> 22) & 0x01;
                    let Q = (word >> 30) & 0x01;
                    let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                    const OPCODES: &[Result<(Opcode, u8), ErrorKind>] = &[
                        // opcode == 0b0000
                        Ok((Opcode::ST4, 4)),
                        Err(ErrorKind::InvalidOpcode),
                        Ok((Opcode::ST1, 4)),
                        Err(ErrorKind::InvalidOpcode),
                        // opcode == 0b0100
                        Ok((Opcode::ST3, 3)),
                        Err(ErrorKind::InvalidOpcode),
                        Ok((Opcode::ST1, 3)),
                        Ok((Opcode::ST1, 1)),
                        // opcode == 0b1000
                        Ok((Opcode::ST2, 2)),
                        Err(ErrorKind::InvalidOpcode),
                        Ok((Opcode::ST1, 2)),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                        Err(ErrorKind::InvalidOpcode),
                    ];

                    let (opcode, num_regs) = OPCODES[opcode_bits as usize]?;

                    inst.opcode = if L == 0 {
                        opcode
                    } else {
                        if opcode == Opcode::ST1 {
                            Opcode::LD1
                        } else if opcode == Opcode::ST2 {
                            Opcode::LD2
                        } else if opcode == Opcode::ST3 {
                            Opcode::LD3
                        } else {
                            Opcode::LD4
                        }
                    };
                    const SIZES: [SIMDSizeCode; 4] = [
                        SIMDSizeCode::B,
                        SIMDSizeCode::H,
                        SIMDSizeCode::S,
                        SIMDSizeCode::D,
                    ];
                    inst.operands = [
                        Operand::SIMDRegisterGroup(datasize, Rt as u16, SIZES[size as usize], num_regs),
                        if Rm == 31 {
                            Operand::RegPostIndex(Rn as u16, (datasize.width() * (num_regs as u16)) as i32)
                        } else {
                            Operand::RegPostIndexReg(Rn as u16, Rm as u16)
                        },
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                },
                0b00110 => {
                    // AdvSIMD load/store single structure
                    let Rt = word & 0x1f;
                    let Rn = (word >> 5) & 0x1f;
                    let size = (word >> 10) & 0x03;
                    let S = (word >> 12) & 1;
                    let opcode_bits = (word >> 13) & 0x07;
                    let Rm = (word >> 16) & 0x1f;
                    if Rm != 0 {
                        return Err(ErrorKind::InvalidOperand);
                    }
                    let R = (word >> 21) & 0x01;
                    let L = (word >> 22) & 0x01;
                    let Q = (word >> 30) & 0x01;
                    let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                    // interleave R==0, R==1
                    const OPCODES: &[Result<(Opcode, u8, SIMDSizeCode), ErrorKind>] = &[
                        Ok((Opcode::ST1, 1, SIMDSizeCode::B)),
                        Ok((Opcode::ST2, 2, SIMDSizeCode::B)),
                        Ok((Opcode::ST3, 3, SIMDSizeCode::B)),
                        Ok((Opcode::ST4, 4, SIMDSizeCode::B)),
                        // opcode = 0b010
                        Ok((Opcode::ST1, 1, SIMDSizeCode::H)),
                        Ok((Opcode::ST2, 2, SIMDSizeCode::H)),
                        Ok((Opcode::ST3, 3, SIMDSizeCode::H)),
                        Ok((Opcode::ST4, 4, SIMDSizeCode::H)),
                        // opcode = 0b100
                        Ok((Opcode::ST1, 1, SIMDSizeCode::S)), // note these can be 64-bit if `size` says so.
                        Ok((Opcode::ST2, 2, SIMDSizeCode::S)),
                        Ok((Opcode::ST3, 3, SIMDSizeCode::S)),
                        Ok((Opcode::ST4, 4, SIMDSizeCode::S)),
                        // opcode = 0b110
                        // unallocated, is. if L==1, these are LD*R
                    ];

                    if opcode_bits > 0b101 {
                        if S != 0 {
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        if L == 0 {
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        const OPCODES: [Opcode; 4] = [
                            Opcode::LD1R,
                            Opcode::LD2R,
                            Opcode::LD3R,
                            Opcode::LD4R,
                        ];
                        let opc_idx = (opcode_bits & 0x01) * 2 + R;
                        inst.opcode = OPCODES[opc_idx as usize];
                        const SIZES: [SIMDSizeCode; 4] = [
                            SIMDSizeCode::B,
                            SIMDSizeCode::H,
                            SIMDSizeCode::S,
                            SIMDSizeCode::D,
                        ];
                        inst.operands = [
                            Operand::SIMDRegisterGroup(datasize, Rt as u16, SIZES[size as usize], opc_idx as u8 + 1),
                            Operand::RegPreIndex(Rn as u16, 0, false),
                            Operand::Nothing,
                            Operand::Nothing,
                        ];

                        return Ok(());
                    }

                    let mut scale = opcode_bits >> 1;
                    // let selem = (((opcode_bits & 1) << 1) | R) + 1;
                    // let mut replicate = false;
                    let opc_idx = (opcode_bits << 1) | R;

                    let (opcode, group_size, item_size) = OPCODES.get(opc_idx as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;

                    let item_size = match item_size {
                        SIMDSizeCode::B => SIMDSizeCode::B,
                        SIMDSizeCode::H => {
                            if (size & 1) == 1 {
                                return Err(ErrorKind::InvalidOperand);
                            }
                            SIMDSizeCode::H
                        }
                        SIMDSizeCode::S => {
                            if size >= 0b10 {
                                return Err(ErrorKind::InvalidOperand);
                            }
                            if size == 0b01 {
                                if S == 1 {
                                    return Err(ErrorKind::InvalidOperand);
                                }
                                scale = 3;
                                SIMDSizeCode::D
                            } else {
                                SIMDSizeCode::S
                            }
                        }
                        SIMDSizeCode::D => {
                            if L == 0 || S == 1 {
                                return Err(ErrorKind::InvalidOperand);
                            }
                            // replicate = true;
                            SIMDSizeCode::D
                        }
                        other => other
                    };

                    let index = ((Q << 3) | (S << 2) | size) >> scale;

                    inst.opcode = if L == 0 {
                        opcode
                    } else {
                        if opcode == Opcode::ST1 {
                            Opcode::LD1
                        } else if opcode == Opcode::ST2 {
                            Opcode::LD2
                        } else if opcode == Opcode::ST3 {
                            Opcode::LD3
                        } else {
                            Opcode::LD4
                        }
                    };
                    inst.operands = [
                        Operand::SIMDRegisterGroupLane(Rt as u16, item_size, group_size, index as u8),
                        Operand::RegPreIndex(Rn as u16, 0, false),
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                },
                0b00111 => {
                    // AdvSIMD load/store single structure (post-indexed)
                    let Rt = word & 0x1f;
                    let Rn = (word >> 5) & 0x1f;
                    let size = (word >> 10) & 0x03;
                    let S = (word >> 12) & 1;
                    let opcode_bits = (word >> 13) & 0x07;
                    let Rm = (word >> 16) & 0x1f;
                    let R = (word >> 21) & 0x01;
                    let L = (word >> 22) & 0x01;
                    let Q = (word >> 30) & 0x01;
                    let datasize = if Q == 1 { SIMDSizeCode::Q } else { SIMDSizeCode::D };

                    // interleave R==0, R==1
                    const OPCODES: &[Result<(Opcode, u8, SIMDSizeCode), ErrorKind>] = &[
                        Ok((Opcode::ST1, 1, SIMDSizeCode::B)),
                        Ok((Opcode::ST2, 2, SIMDSizeCode::B)),
                        Ok((Opcode::ST3, 3, SIMDSizeCode::B)),
                        Ok((Opcode::ST4, 4, SIMDSizeCode::B)),
                        // opcode = 0b010
                        Ok((Opcode::ST1, 1, SIMDSizeCode::H)),
                        Ok((Opcode::ST2, 2, SIMDSizeCode::H)),
                        Ok((Opcode::ST3, 3, SIMDSizeCode::H)),
                        Ok((Opcode::ST4, 4, SIMDSizeCode::H)),
                        // opcode = 0b100
                        Ok((Opcode::ST1, 1, SIMDSizeCode::S)), // note these can be 64-bit if `size` says so.
                        Ok((Opcode::ST2, 2, SIMDSizeCode::S)),
                        Ok((Opcode::ST3, 3, SIMDSizeCode::S)),
                        Ok((Opcode::ST4, 4, SIMDSizeCode::S)),
                        // opcode = 0b110
                        // unallocated, is. if L==1, these are LD*R
                    ];

                    if opcode_bits >= 0b110 {
                        if S != 0 {
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        if L == 0 {
                            return Err(ErrorKind::InvalidOpcode);
                        }

                        const OPCODES: [Opcode; 4] = [
                            Opcode::LD1R,
                            Opcode::LD2R,
                            Opcode::LD3R,
                            Opcode::LD4R,
                        ];
                        let opc_idx = (opcode_bits & 0x01) * 2 + R;
                        inst.opcode = OPCODES[opc_idx as usize];
                        const SIZES: [SIMDSizeCode; 4] = [
                            SIMDSizeCode::B,
                            SIMDSizeCode::H,
                            SIMDSizeCode::S,
                            SIMDSizeCode::D,
                        ];
                        inst.operands = [
                            Operand::SIMDRegisterGroup(datasize, Rt as u16, SIZES[size as usize], opc_idx as u8 + 1),
                            if Rm == 31 {
                                Operand::RegPostIndex(Rn as u16, ((opc_idx + 1) * (1 << size)) as i32)
                            } else {
                                Operand::RegPostIndexReg(Rn as u16, Rm as u16)
                            },
                            Operand::Nothing,
                            Operand::Nothing,
                        ];

                        return Ok(());
                    }

                    let mut scale = opcode_bits >> 1;
                    // let selem = (((opcode_bits & 1) << 1) | R) + 1;
                    // let mut replicate = false;
                    let opc_idx = (opcode_bits << 1) | R;

                    let (opcode, group_size, item_size) = OPCODES.get(opc_idx as usize).cloned().unwrap_or(Err(ErrorKind::InvalidOpcode))?;

                    let item_size = match item_size {
                        SIMDSizeCode::B => SIMDSizeCode::B,
                        SIMDSizeCode::H => {
                            if (size & 1) == 1 {
                                return Err(ErrorKind::InvalidOperand);
                            }
                            SIMDSizeCode::H
                        }
                        SIMDSizeCode::S => {
                            if size >= 0b10 {
                                return Err(ErrorKind::InvalidOperand);
                            }
                            if size == 0b01 {
                                if S == 1 {
                                    return Err(ErrorKind::InvalidOperand);
                                }
                                scale = 3;
                                SIMDSizeCode::D
                            } else {
                                SIMDSizeCode::S
                            }
                        }
                        SIMDSizeCode::D => {
                            if L == 0 || S == 1 {
                                return Err(ErrorKind::InvalidOperand);
                            }
                            // replicate = true;
                            SIMDSizeCode::D
                        }
                        other => other
                    };

                    let index = ((Q << 3) | (S << 2) | size) >> scale;

                    inst.opcode = if L == 0 {
                        opcode
                    } else {
                        if opcode == Opcode::ST1 {
                            Opcode::LD1
                        } else if opcode == Opcode::ST2 {
                            Opcode::LD2
                        } else if opcode == Opcode::ST3 {
                            Opcode::LD3
                        } else {
                            Opcode::LD4
                        }
                    };
                    inst.operands = [
                        Operand::SIMDRegisterGroupLane(Rt as u16, item_size, group_size, index as u8),
                        if Rm == 31 {
                            Operand::RegPostIndex(Rn as u16, (group_size as u16 * item_size.width()) as i32)
                        } else {
                            Operand::RegPostIndexReg(Rn as u16, Rm as u16)
                        },
                        Operand::Nothing,
                        Operand::Nothing,
                    ];
                }
                _ => {
                    return Err(ErrorKind::InvalidOpcode);
                }
            }
        },
        Section::BranchExceptionSystem => {
            let group_bits = ((word >> 29) << 2) | ((word >> 24) & 0x03);
            match group_bits {
                0b00000..=0b00011 => { // unconditional branch (imm)
                    let offset = ((word & 0x03ff_ffff) << 2) as i32;
                    let extended_offset = (offset << 4) >> 4;
                    inst.opcode = Opcode::B;
                    inst.operands = [
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing,
                        Operand::Nothing,
                        Operand::Nothing
                    ];
                },
                0b00100 => { // compare branch (imm)
                    inst.opcode = Opcode::CBZ;
                    let offset = (word as i32 & 0x00ff_ffe0) >> 3;
                    let extended_offset = (offset << 11) >> 11;
                    let Rt = word & 0x1f;
                    inst.operands = [
                        Operand::Register(SizeCode::W, Rt as u16),
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing,
                        Operand::Nothing
                    ];
                },
                0b00101 => { // compare branch (imm)
                    inst.opcode = Opcode::CBNZ;
                    let offset = (word as i32 & 0x00ff_ffe0) >> 3;
                    let extended_offset = (offset << 11) >> 11;
                    let Rt = word & 0x1f;
                    inst.operands = [
                        Operand::Register(SizeCode::W, Rt as u16),
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing,
                        Operand::Nothing
                    ];
                },
                0b00110 => { // test branch (imm)
                    let offset = (word as i32 & 0x0007_ffe0) >> 3;
                    let extended_offset = (offset << 16) >> 16;
                    let b = (word >> 19) & 0x1f;
                    let Rt = word & 0x1f;
                    inst.opcode = Opcode::TBZ;
                    inst.operands = [
                        Operand::Register(SizeCode::W, Rt as u16),
                        Operand::Imm16(b as u16),
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing
                    ];
                },
                0b00111 => { // test branch (imm)
                    let offset = (word as i32 & 0x0007_ffe0) >> 3;
                    let extended_offset = (offset << 16) >> 16;
                    let b = (word >> 19) & 0x1f;
                    let Rt = word & 0x1f;
                    inst.opcode = Opcode::TBNZ;
                    inst.operands = [
                        Operand::Register(SizeCode::W, Rt as u16),
                        Operand::Imm16(b as u16),
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing
                    ];
                },
                0b01000 => { // conditional branch (imm)
                    let offset = (word as i32 & 0x00ff_ffe0) >> 3;
                    let extended_offset = (offset << 11) >> 11;
                    let cond = word & 0x0f;
                    inst.opcode = Opcode::Bcc(cond as u8);
                    inst.operands = [
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing,
                        Operand::Nothing,
                        Operand::Nothing
                    ];
                }
                0b01001 => { // conditional branch (imm)
                    // o1 -> unallocated, reserved
                    return Err(ErrorKind::InvalidOpcode);
                }
                /* 0b01010 to 0b01111 seem all invalid? */
                0b10000..=0b10011 => { // unconditional branch (imm)
                    let offset = ((word & 0x03ff_ffff) << 2) as i32;
                    let extended_offset = (offset << 4) >> 4;
                    inst.opcode = Opcode::BL;
                    inst.operands = [
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing,
                        Operand::Nothing,
                        Operand::Nothing
                    ];
                },
                0b10100 => { // compare branch (imm)
                    inst.opcode = Opcode::CBZ;
                    let offset = (word as i32 & 0x00ff_ffe0) >> 3;
                    let extended_offset = (offset << 11) >> 11;
                    let Rt = word & 0x1f;
                    inst.operands = [
                        Operand::Register(SizeCode::X, Rt as u16),
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing,
                        Operand::Nothing
                    ];
                },
                0b10101 => { // compare branch (imm)
                    inst.opcode = Opcode::CBNZ;
                    let offset = (word as i32 & 0x00ff_ffe0) >> 3;
                    let extended_offset = (offset << 11) >> 11;
                    let Rt = word & 0x1f;
                    inst.operands = [
                        Operand::Register(SizeCode::X, Rt as u16),
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing,
                        Operand::Nothing
                    ];
                },
                0b10110 => { // test branch (imm)
                    let offset = (word as i32 & 0x0007_ffe0) >> 3;
                    let extended_offset = (offset << 16) >> 16;
                    let b = (word >> 19) & 0x1f;
                    let Rt = word & 0x1f;
                    inst.opcode = Opcode::TBZ;
                    inst.operands = [
                        Operand::Register(SizeCode::X, Rt as u16),
                        Operand::Imm16((b as u16) | 0x20),
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing
                    ];
                },
                0b10111 => { // test branch (imm)
                    let offset = (word as i32 & 0x0007_ffe0) >> 3;
                    let extended_offset = (offset << 16) >> 16;
                    let b = (word >> 19) & 0x1f;
                    let Rt = word & 0x1f;
                    inst.opcode = Opcode::TBNZ;
                    inst.operands = [
                        Operand::Register(SizeCode::X, Rt as u16),
                        Operand::Imm16((b as u16) | 0x20),
                        Operand::PCOffset(extended_offset as i64),
                        Operand::Nothing
                    ];
                },
                0b11000 => { // exception generation
                    let ll = word & 0x3;
                    let op2 = (word >> 2) & 0x7;
                    let opc = (word >> 21) & 0x7;
                    match (opc, op2, ll) {
                        (0b000, 0b000, 0b01) => {
                            inst.opcode = Opcode::SVC;
                        }
                        (0b000, 0b000, 0b10) => {
                            inst.opcode = Opcode::HVC;
                        }
                        (0b000, 0b000, 0b11) => {
                            inst.opcode = Opcode::SMC;
                        }
                        (0b001, 0b000, 0b00) => {
                            inst.opcode = Opcode::BRK;
                        }
                        (0b010, 0b000, 0b00) => {
                            inst.opcode = Opcode::HLT;
                        }
                        (0b101, 0b000, 0b01) => {
                            inst.opcode = Opcode::DCPS1;
                        }
                        (0b101, 0b000, 0b10) => {
                            inst.opcode = Opcode::DCPS2;
                        }
                        (0b101, 0b000, 0b11) => {
                            inst.opcode = Opcode::DCPS3;
                        }
                        _ => {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOpcode);
                        }
                    }
                    let imm = (word >> 5) & 0xffff;
                    inst.operands = [
                        Operand::Imm16(imm as u16),
                        Operand::Nothing,
                        Operand::Nothing,
                        Operand::Nothing
                    ];
                },
                0b11001 => { // system
                    let remainder = word & 0xffffff;
                    if remainder >= 0x400000 {
                        return Err(ErrorKind::InvalidOperand);
                    } else {
                        let Rt = word & 0x1f;
                        let Lop0 = ((word >> 19) & 0x7) as u8;
                        let op1 = (word >> 16) & 0b111;
                        let CRn = (word >> 12) & 0b1111;
                        let CRm = (word >> 8) & 0b1111;
                        let op2 = (word >> 5) & 0b111;
                        match Lop0 {
                            0b000 => {
                                // MSR, HINT, CLREX, DSB, DMB, ISB
                                if Rt == 0b11111 {
//                                        panic!("crn: {:2x}, op1: {:2x}", CRn, op1);

                                    // the ARM ARM does not, as far as i can see, explicitly
                                    // talk about these system instructiosn where op1 is a
                                    // value other than 0b011. capstone disassembles
                                    // instructions in here as `msr` and things, for example,
                                    // [1f, 20, 00, d5] which is reported as
                                    // `msr s0_0_c2_c0_0, xzr`. it seems these are more,
                                    // invalid? in which case, return invalid.
                                    if CRn != 0b0100 && op1 != 0b011 {
                                        inst.opcode = Opcode::Invalid;
                                        return Err(ErrorKind::InvalidOpcode);
                                    }

                                    match CRn {
                                        0b0010 => {
                                            inst.opcode = Opcode::HINT;
                                            inst.operands = [
                                                Operand::ControlReg(CRm as u16),
                                                Operand::Immediate(op2),
                                                Operand::Nothing,
                                                Operand::Nothing,
                                            ];
                                        },
                                        0b0011 => {
                                            match op2 {
                                                0b010 => {
                                                    inst.opcode = Opcode::CLREX;
                                                    inst.operands = [
                                                        Operand::Imm16(CRm as u16),
                                                        Operand::Nothing,
                                                        Operand::Nothing,
                                                        Operand::Nothing,
                                                    ];
                                                },
                                                0b100 => {
                                                    if CRm == 0b0000 {
                                                        inst.opcode = Opcode::SSSB;
                                                        inst.operands = [
                                                            Operand::Nothing,
                                                            Operand::Nothing,
                                                            Operand::Nothing,
                                                            Operand::Nothing,
                                                        ];
                                                    } else {
                                                        inst.opcode = Opcode::DSB(CRm as u8);
                                                        inst.operands = [
                                                            Operand::Nothing,
                                                            Operand::Nothing,
                                                            Operand::Nothing,
                                                            Operand::Nothing,
                                                        ];
                                                    }
                                                },
                                                0b101 => {
                                                    inst.opcode = Opcode::DMB(CRm as u8);
                                                    inst.operands = [
                                                        Operand::Nothing,
                                                        Operand::Nothing,
                                                        Operand::Nothing,
                                                        Operand::Nothing,
                                                    ];
                                                },
                                                0b110 => {
                                                    inst.opcode = Opcode::ISB;
                                                    // other values of CRm are reserved, but
                                                    // execute as full barriers. "must not be
                                                    // relied upon by software".
                                                    inst.operands = [
                                                        Operand::Imm16(CRm as u16),
                                                        Operand::Nothing,
                                                        Operand::Nothing,
                                                        Operand::Nothing,
                                                    ];
                                                }
                                                0b111 => {
                                                    inst.opcode = Opcode::SB;
                                                    inst.operands = [
                                                        Operand::Nothing,
                                                        Operand::Nothing,
                                                        Operand::Nothing,
                                                        Operand::Nothing,
                                                    ];
                                                }
                                                _ => {
                                                    inst.opcode = Opcode::Invalid;
                                                    return Err(ErrorKind::InvalidOpcode);
                                                }
                                            };
                                        },
                                        0b0100 => {
                                            inst.opcode = Opcode::MSR;

                                            /*
                                            if op1 == 0b001 || op1 == 0b010 || op1 >= 0b100 {
                                                return Err(ErrorKind::InvalidOperand);
                                            } else if op1 == 0b000 && op2 >= 0b110 {
                                                return Err(ErrorKind::InvalidOperand);
                                            } else if op1 == 0b011 && op2 == 0b000 {
                                                return Err(ErrorKind::InvalidOperand);
                                            } else if op1 == 0b011 && op2 == 0b011 {
                                                return Err(ErrorKind::InvalidOperand);
                                            } else if op1 == 0b011 && op2 == 0b101 {
                                                return Err(ErrorKind::InvalidOperand);
                                            }
                                            */

                                            inst.operands = [
                                                Operand::PstateField(((op1 << 3) | op2) as u8),
                                                Operand::Imm16(
                                                    ((word >> 8) & 0xf) as u16
                                                ),
                                                Operand::Nothing,
                                                Operand::Nothing,
                                            ];
                                        }
                                        _ => {
                                            inst.opcode = Opcode::Invalid;
                                            return Err(ErrorKind::InvalidOpcode);
                                        }
                                    }
                                } else {
                                    inst.opcode = Opcode::Invalid;
                                    return Err(ErrorKind::InvalidOpcode);
                                }
                            }
                            0b001 => {
                                inst.opcode = Opcode::SYS(SysOps::new(op1 as u8, op2 as u8));
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rt as u16),
                                    Operand::ControlReg(CRn as u16),
                                    Operand::ControlReg(CRm as u16),
                                    Operand::Nothing,
                                ];
                            }
                            0b010 |
                            0b011 => {
                                inst.opcode = Opcode::MSR;
                                let Rt = word & 0b1111;
                                let systemreg = (word >> 5) & 0b1_111_1111_1111_111;
                                inst.operands = [
                                    Operand::SystemReg(systemreg as u16),
                                    Operand::Register(SizeCode::X, Rt as u16),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            0b100 => {
                                // quote C5.1.2:
                                // All encodings with L==1 and op0==0b0x are UNDEFINED, except
                                // for encodings in the area reserved for IMPLEMENTATION
                                // DEFINED use
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOpcode);
                            }
                            0b101 => {
                                let Rt = word & 0b11111;
                                let op2 = (word >> 5) & 0b111;
                                let CRm = (word >> 8) & 0b1111;
                                let CRn = (word >> 12) & 0b1111;
                                let op1 = (word >> 16) & 0b111;
                                inst.opcode = Opcode::SYSL(SysOps::new(op1 as u8, op2 as u8));
                                inst.operands = [
                                    Operand::ControlReg(CRn as u16),
                                    Operand::ControlReg(CRm as u16),
                                    Operand::Register(SizeCode::X, Rt as u16),
                                    Operand::Nothing,
                                ];
                            }
                            0b110 |
                            0b111 => {
                                let Rt = word & 0b11111;
                                let systemreg = (word >> 5) & 0b1_111_1111_1111_111;
                                inst.opcode = Opcode::MRS;
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rt as u16),
                                    Operand::SystemReg(systemreg as u16),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            }
                            _ => {
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        }
                    }
                },
                0b11010 => { // unconditional branch (reg)
                    // actually the low 3 bits of opc
                    let opc = (word >> 21) & 0x7;
                    match opc {
                        0b000 => {
                            let Rn = (word >> 5) & 0x1f;
                            if (word & 0x1ffc1f) == 0x1f0000 {
                                inst.opcode = Opcode::BR;
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rn as u16),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing
                                ];
                            } else if (word & 0x1ffc1f) == 0x1f081f {
                                inst.opcode = Opcode::BRAAZ;
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rn as u16),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else if (word & 0x1ffc1f) == 0x1f0c1f {
                                inst.opcode = Opcode::BRABZ;
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rn as u16),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        },
                        0b001 => {
                            let Rn = (word >> 5) & 0x1f;
                            if (word & 0x1ffc1f) == 0x1f0000 {
                                inst.opcode = Opcode::BLR;
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rn as u16),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing
                                ];
                            } else if (word & 0x1ffc1f) == 0x1f081f {
                                inst.opcode = Opcode::BLRAAZ;
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rn as u16),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else if (word & 0x1ffc1f) == 0x1f0c1f {
                                inst.opcode = Opcode::BLRABZ;
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rn as u16),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        },
                        0b010 => {
                            let Rn = (word >> 5) & 0x1f;
                            if (word & 0x1ffc1f) == 0x1f0000 {
                                inst.opcode = Opcode::RET;
                                inst.operands = [
                                    Operand::Register(SizeCode::X, Rn as u16),
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing
                                ];
                            } else if (word & 0x1fffff) == 0x1f0bff {
                                inst.opcode = Opcode::RETAA;
                                inst.operands = [
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else if (word & 0x1fffff) == 0x1f0fff {
                                inst.opcode = Opcode::RETAB;
                                inst.operands = [
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                    Operand::Nothing,
                                ];
                            } else {
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        },
                        0b100 => {
                            if (word & 0x1fffff) == 0x1f03e0 {
                                inst.opcode = Opcode::ERET;
                            } else if (word & 0x1fffff) == 0x1f0bff {
                                inst.opcode = Opcode::ERETAA;
                            } else if (word & 0x1fffff) == 0x1f0fff {
                                inst.opcode = Opcode::ERETAB;
                            } else {
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        },
                        0b101 => {
                            if (word & 0x1fffff) == 0x1f03e0 {
                                inst.opcode = Opcode::DRPS;
                            } else {
                                inst.opcode = Opcode::Invalid;
                                return Err(ErrorKind::InvalidOpcode);
                            }
                        },
                        _ => {
                            inst.opcode = Opcode::Invalid;
                            return Err(ErrorKind::InvalidOpcode);
                        }
                    }
                }
                0b11011 => { // unconditional branch (reg)
                    // the last 1 is bit 24, which C3.2.7 indicates are
                    // all invalid encodings (opc is b0101 or lower)
                    let opc = (word >> 21) & 0x7;

                    if opc == 0b000 {
                        // implied leading bit means opc = 0b1000
                        if word & 0xff_ff_fc_00 == 0xd7_1f_08_00 {
                            // op3 = 000011
                            // register modifier variant
                            inst.opcode = Opcode::BRAA;

                            inst.operands = [
                                Operand::Register(SizeCode::X, ((word >> 5) & 0b11111) as u16),
                                Operand::RegisterOrSP(SizeCode::X, (word & 0b11111) as u16),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        } else if word & 0xff_ff_fc_00 == 0xd7_1f_0c_00 {
                            // op3 = 000011
                            inst.opcode = Opcode::BRAB;

                            inst.operands = [
                                Operand::Register(SizeCode::X, ((word >> 5) & 0b11111) as u16),
                                Operand::RegisterOrSP(SizeCode::X, (word & 0b11111) as u16),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        } else {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                    } else if opc == 0b001 {
                        // implied leading bit means opc = 0b1001
                        if word & 0xff_ff_fc_00 == 0xd7_3f_08_00 {
                            // op3 = 000011
                            // register modifier variant
                            inst.opcode = Opcode::BLRAA;

                            inst.operands = [
                                Operand::Register(SizeCode::X, ((word >> 5) & 0b11111) as u16),
                                Operand::RegisterOrSP(SizeCode::X, (word & 0b11111) as u16),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        } else if word & 0xff_ff_fc_00 == 0xd7_3f_0c_00 {
                            // op3 = 000011
                            inst.opcode = Opcode::BLRAB;

                            inst.operands = [
                                Operand::Register(SizeCode::X, ((word >> 5) & 0b11111) as u16),
                                Operand::RegisterOrSP(SizeCode::X, (word & 0b11111) as u16),
                                Operand::Nothing,
                                Operand::Nothing,
                            ];
                        } else {
                            return Err(ErrorKind::InvalidOpcode);
                        }
                    } else {
                        inst.opcode = Opcode::Invalid;
                        return Err(ErrorKind::InvalidOpcode);
                    }
                }
                _ => {
                    return Err(ErrorKind::InvalidOpcode);
                }
            }
        },
    };

    Ok(())
}
