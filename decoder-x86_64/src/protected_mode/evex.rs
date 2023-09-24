use crate::protected_mode::{read_E_vex, read_imm_unsigned, read_modrm};
use crate::protected_mode::{Instruction, Opcode, OperandSpec, RegSpec, RegisterBank};
use decoder::{ErrorKind, Reader};

const DEFAULT_EVEX_REGISTER_SIZE: RegisterBank = RegisterBank::D;
const DEFAULT_EVEX_REGISTER_WIDTH: u8 = 4;

fn isa_has_qwords() -> bool {
    false
}

mod generated {
    #[allow(non_camel_case_types)]
    #[derive(Debug, PartialEq, Eq, Copy, Clone)]
    pub(crate) enum EVEXOperandCode {
        Mvector_Mask_G_LL,
        E_G_LL_W0,
        Ebd_G_xmm_imm8,
        Edd_G_xmm_imm8,
        Edm_xmm_G_xmm_W0,
        Edm_xmm_G_ymm_W0,
        Em_G_LL,
        Em_G_LL_W0,
        Em_G_LL_W1,
        Em_xmm_G_LL_imm8,
        Em_xmm_G_ymm_W0,
        Em_xmm_G_ymm_imm8_sae_W0,
        Em_xmm_G_zmm_W0,
        Em_ymm_G_zmm_W0,
        Em_ymm_G_zmm_imm8,
        Em_ymm_G_zmm_imm8_sae_W0,
        Eqm_G_xmm_imm8_sae_W0,
        Eqm_xmm_G_xmm_W0,
        Eqm_xmm_G_ymm_W0,
        Eqm_xmm_G_zmm_W0,
        Ewd_G_xmm_imm8,
        Ewm_xmm_G_xmm_W0,
        G_E_LL_W0,
        G_Ed_xmm_sae_W0,
        G_LL_Mask,
        G_LL_Mask_W0,
        G_LL_Mask_W1,
        G_V_E_LL,
        G_V_E_LL_imm8,
        G_V_Ed_xmm_imm8_W0,
        G_V_Mq_xmm_W1,
        G_V_xmm_Ebd_imm8,
        G_V_xmm_Edq_imm8,
        G_V_xmm_Edq_sae,
        Gd_Ed_xmm_sae,
        Gm_E_LL,
        Gm_E_LL_W0,
        Gm_E_LL_W1,
        Gm_E_LL_bcast,
        Gm_E_LL_bcast_W0,
        Gm_E_LL_bcast_W1,
        Gm_E_LL_imm8,
        Gm_E_LL_imm8_bcast_W0,
        Gm_E_LL_imm8_bcast_W1,
        Gm_E_LL_imm8_sae,
        Gm_E_LL_imm8_sae_W0,
        Gm_E_LL_imm8_sae_W1,
        Gm_E_LL_sae_bcast,
        Gm_E_LL_sae_bcast_W0,
        Gm_E_LL_sae_bcast_W1,
        Gm_E_zmm_sae_bcast,
        Gm_Ed_LL_imm8_sae_noround_bcast,
        Gm_Ed_LL_sae_noround_bcast_W0,
        Gm_Eq_xmm_sae_W1,
        Gm_LL_Eb_xmm_W0,
        Gm_LL_Ed_xmm_W0,
        Gm_LL_Eq_xmm,
        Gm_LL_Ew_xmm_W0,
        Gm_LL_Ud,
        Gm_LL_Ud_W0,
        Gm_V_E_LL,
        Gm_V_E_LL_W0,
        Gm_V_E_LL_W1,
        Gm_V_E_LL_bcast,
        Gm_V_E_LL_bcast_W0,
        Gm_V_E_LL_bcast_W1,
        Gm_V_E_LL_imm8,
        Gm_V_E_LL_imm8_W0,
        Gm_V_E_LL_imm8_W1,
        Gm_V_E_LL_imm8_bcast,
        Gm_V_E_LL_imm8_bcast_W0,
        Gm_V_E_LL_imm8_bcast_W1,
        Gm_V_E_LL_imm8_sae_bcast,
        Gm_V_E_LL_sae_bcast,
        Gm_V_E_LL_sae_bcast_W0,
        Gm_V_E_LL_sae_bcast_W1,
        Gm_V_E_xmm_imm8_sae_W1,
        Gm_V_E_xmm_sae,
        Gm_V_E_xmm_sae_W1,
        Gm_V_Ed_LL_bcast,
        Gm_V_Ed_LL_bcast_W0,
        Gm_V_Ed_LL_imm8_bcast,
        Gm_V_Ed_LL_sae,
        Gm_V_Ed_xmm,
        Gm_V_Ed_xmm_imm8_sae,
        Gm_V_Ed_xmm_sae,
        Gm_V_Ed_xmm_sae_W0,
        Gm_V_Ed_xmm_sae_bcast,
        Gm_V_Ed_xmm_sae_noround_W0,
        Gm_V_Eq_xmm_sae_W1,
        Gm_V_LL_E_xmm,
        Gm_V_LL_E_xmm_W0,
        Gm_V_LL_E_xmm_W1,
        Gm_V_LL_E_xmm_imm8,
        Gm_V_M_xmm,
        Gm_V_ymm_E_xmm_imm8,
        Gm_V_zmm_E_xmm_imm8,
        Gm_V_zmm_E_ymm_imm8,
        Gm_V_zmm_M_xmm_W0,
        Gm_xmm_E_xmm_sae_bcast_W1,
        Gm_xmm_E_ymm_sae_bcast_W1,
        Gm_xmm_Ed_xmm,
        Gm_xmm_Ed_xmm_W0,
        Gm_xmm_Eq_xmm,
        Gm_xmm_Eq_xmm_W0,
        Gm_xmm_Ew_xmm,
        Gm_ymm_E_xmm,
        Gm_ymm_E_xmm_W0,
        Gm_ymm_E_zmm_sae_bcast_W1,
        Gm_ymm_Ed_xmm,
        Gm_ymm_Ed_xmm_W0,
        Gm_ymm_Eq_xmm,
        Gm_ymm_M_xmm,
        Gm_ymm_U_zmm_sae_W1,
        Gm_zmm_E_xmm,
        Gm_zmm_E_ymm,
        Gm_zmm_E_ymm_W0,
        Gm_zmm_Ed_xmm,
        Gm_zmm_Ed_xmm_W0,
        Gm_zmm_Eq_xmm,
        Gm_zmm_M_xmm,
        Gm_zmm_M_ymm,
        M_G_LL_W0,
        M_G_LL_W1,
        Mask_E_LL_imm8_bcast,
        Mask_Ed_xmm_imm8,
        Mask_U_LL,
        Mask_V_E_LL,
        Mask_V_E_LL_W0,
        Mask_V_E_LL_bcast,
        Mask_V_E_LL_bcast_W0,
        Mask_V_E_LL_bcast_W1,
        Mask_V_E_LL_imm8,
        Mask_V_E_LL_imm8_bcast,
        Mask_V_E_LL_imm8_sae_bcast_W0,
        Maskm_V_E_LL_imm8_sae_bcast_W1,
        Maskm_V_Ed_xmm_imm8_sae_W0,
        Maskm_V_Eq_xmm_imm8_sae_W1,
        Mq_G_W0,
        Mq_G_xmm_W1,
        Nothing,
        Opcode_72_Gm_E_LL_imm8_bcast,
        Operands_12_W0,
        Operands_16_W0,
        Operands_72_W0,
        VBROADCASTF32X2_Gm_ymm_Ed_xmm,
        VCVTDQ2PS,
        VCVTPH2PS,
        VCVTSI2SS,
        VCVTSS2SI,
        VCVTTPD2DQ,
        VCVTTPS2UDQ,
        VCVTTPS2UQQ,
        VCVTTSS2SI,
        VCVTUDQ2PD,
        VCVTUSI2SD,
        VEXTRACTPS,
        VMOVD_6e,
        VMOVD_7e,
        VMOVQ_7e,
        VMOVQ_Ed_G_xmm,
        VMOVQ_G_Ed_xmm,
        VMOVSD_10,
        VMOVSD_11,
        VMOVSS_10,
        VMOVSS_11,
        VPEXTRW,
        VPINSRW,
    }

    type OpcodeTables = [&'static [(u8, [(super::Opcode, EVEXOperandCode); 4])]; 12];

    pub(crate) const TABLES: OpcodeTables = [
        &EVEX_NONE_0F,
        &EVEX_66_0F,
        &EVEX_F2_0F,
        &EVEX_F3_0F,
        &DUMMY,
        &EVEX_66_0F38,
        &EVEX_F2_0F38,
        &EVEX_F3_0F38,
        &DUMMY,
        &EVEX_66_0F3A,
        &DUMMY,
        &DUMMY,
    ];

    pub(crate) const DUMMY: [(u8, [(super::Opcode, EVEXOperandCode); 4]); 0] = [];

    const EVEX_NONE_0F: [(u8, [(super::Opcode, EVEXOperandCode); 4]); 30] = [
        (
            0x10,
            [
                (super::Opcode::VMOVUPS, EVEXOperandCode::Gm_E_LL_W0),
                (super::Opcode::VMOVUPS, EVEXOperandCode::Gm_E_LL_W0),
                (super::Opcode::VMOVUPS, EVEXOperandCode::Gm_E_LL_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x11,
            [
                (super::Opcode::VMOVUPS, EVEXOperandCode::Em_G_LL_W0),
                (super::Opcode::VMOVUPS, EVEXOperandCode::Em_G_LL_W0),
                (super::Opcode::VMOVUPS, EVEXOperandCode::Em_G_LL_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x12,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Operands_12_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x13,
            [
                (super::Opcode::VMOVLPS, EVEXOperandCode::Mq_G_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x14,
            [
                (
                    super::Opcode::VUNPCKLPS,
                    EVEXOperandCode::Gm_V_Ed_LL_bcast_W0,
                ),
                (
                    super::Opcode::VUNPCKLPS,
                    EVEXOperandCode::Gm_V_Ed_LL_bcast_W0,
                ),
                (
                    super::Opcode::VUNPCKLPS,
                    EVEXOperandCode::Gm_V_Ed_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x15,
            [
                (
                    super::Opcode::VUNPCKHPS,
                    EVEXOperandCode::Gm_V_Ed_LL_bcast_W0,
                ),
                (
                    super::Opcode::VUNPCKHPS,
                    EVEXOperandCode::Gm_V_Ed_LL_bcast_W0,
                ),
                (
                    super::Opcode::VUNPCKHPS,
                    EVEXOperandCode::Gm_V_Ed_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x16,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Operands_16_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x17,
            [
                (super::Opcode::VMOVHPS, EVEXOperandCode::Mq_G_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x28,
            [
                (
                    super::Opcode::VMOVAPS,
                    EVEXOperandCode::Gm_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMOVAPS,
                    EVEXOperandCode::Gm_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMOVAPS,
                    EVEXOperandCode::Gm_E_LL_sae_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x29,
            [
                (super::Opcode::VMOVAPS, EVEXOperandCode::Em_G_LL_W0),
                (super::Opcode::VMOVAPS, EVEXOperandCode::Em_G_LL_W0),
                (super::Opcode::VMOVAPS, EVEXOperandCode::Em_G_LL_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x2b,
            [
                (super::Opcode::VMOVNTPS, EVEXOperandCode::M_G_LL_W0),
                (super::Opcode::VMOVNTPS, EVEXOperandCode::M_G_LL_W0),
                (super::Opcode::VMOVNTPS, EVEXOperandCode::M_G_LL_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x2e,
            [
                (super::Opcode::VUCOMISS, EVEXOperandCode::G_Ed_xmm_sae_W0),
                (super::Opcode::VUCOMISS, EVEXOperandCode::G_Ed_xmm_sae_W0),
                (super::Opcode::VUCOMISS, EVEXOperandCode::G_Ed_xmm_sae_W0),
                (super::Opcode::VUCOMISS, EVEXOperandCode::G_Ed_xmm_sae_W0),
            ],
        ),
        (
            0x2f,
            [
                (super::Opcode::VCOMISS, EVEXOperandCode::G_Ed_xmm_sae_W0),
                (super::Opcode::VCOMISS, EVEXOperandCode::G_Ed_xmm_sae_W0),
                (super::Opcode::VCOMISS, EVEXOperandCode::G_Ed_xmm_sae_W0),
                (super::Opcode::VCOMISS, EVEXOperandCode::G_Ed_xmm_sae_W0),
            ],
        ),
        (
            0x51,
            [
                (super::Opcode::VSQRTPS, EVEXOperandCode::Gm_E_LL_sae_bcast),
                (super::Opcode::VSQRTPS, EVEXOperandCode::Gm_E_LL_sae_bcast),
                (super::Opcode::VSQRTPS, EVEXOperandCode::Gm_E_LL_sae_bcast),
                (super::Opcode::VSQRTPS, EVEXOperandCode::Gm_E_LL_sae_bcast),
            ],
        ),
        (
            0x54,
            [
                (super::Opcode::VANDPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VANDPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VANDPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x55,
            [
                (super::Opcode::VANDNPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VANDNPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VANDNPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x56,
            [
                (super::Opcode::VORPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VORPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VORPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x57,
            [
                (super::Opcode::VXORPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VXORPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VXORPS, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x58,
            [
                (
                    super::Opcode::VADDPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VADDPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VADDPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VADDPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
            ],
        ),
        (
            0x59,
            [
                (
                    super::Opcode::VMULPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMULPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMULPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMULPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
            ],
        ),
        (
            0x5a,
            [
                (super::Opcode::VCVTPS2PD, EVEXOperandCode::VCVTPH2PS),
                (super::Opcode::VCVTPS2PD, EVEXOperandCode::VCVTPH2PS),
                (super::Opcode::VCVTPS2PD, EVEXOperandCode::VCVTPH2PS),
                (super::Opcode::VCVTPS2PD, EVEXOperandCode::VCVTPH2PS),
            ],
        ),
        (
            0x5b,
            [
                (super::Opcode::VCVTDQ2PS, EVEXOperandCode::VCVTDQ2PS),
                (super::Opcode::VCVTDQ2PS, EVEXOperandCode::VCVTDQ2PS),
                (super::Opcode::VCVTDQ2PS, EVEXOperandCode::VCVTDQ2PS),
                (super::Opcode::VCVTDQ2PS, EVEXOperandCode::VCVTDQ2PS),
            ],
        ),
        (
            0x5c,
            [
                (
                    super::Opcode::VSUBPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VSUBPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VSUBPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VSUBPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
            ],
        ),
        (
            0x5d,
            [
                (
                    super::Opcode::VMINPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMINPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMINPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMINPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
            ],
        ),
        (
            0x5e,
            [
                (
                    super::Opcode::VDIVPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VDIVPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VDIVPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VDIVPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
            ],
        ),
        (
            0x5f,
            [
                (
                    super::Opcode::VMAXPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMAXPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMAXPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
                (
                    super::Opcode::VMAXPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0,
                ),
            ],
        ),
        (
            0x78,
            [
                (super::Opcode::VCVTTPS2UDQ, EVEXOperandCode::VCVTTPS2UDQ),
                (super::Opcode::VCVTTPS2UDQ, EVEXOperandCode::VCVTTPS2UDQ),
                (super::Opcode::VCVTTPS2UDQ, EVEXOperandCode::VCVTTPS2UDQ),
                (super::Opcode::VCVTTPS2UDQ, EVEXOperandCode::VCVTTPS2UDQ),
            ],
        ),
        (
            0x79,
            [
                (super::Opcode::VCVTPS2UDQ, EVEXOperandCode::VCVTTPS2UDQ), // operands=['VCVTPS2UDQ_ZMMu32_MASKmskw_ZMMf32_AVX512_sae', 'VCVTPS2UDQ_XMMu32_MASKmskw_MEMf32_AVX512_sae', 'VCVTPS2UDQ_XMMu32_MASKmskw_XMMf32_AVX512_sae']
                (super::Opcode::VCVTPS2UDQ, EVEXOperandCode::VCVTTPS2UDQ), // operands=['VCVTPS2UDQ_ZMMu32_MASKmskw_ZMMf32_AVX512_sae', 'VCVTPS2UDQ_YMMu32_MASKmskw_MEMf32_AVX512_sae', 'VCVTPS2UDQ_YMMu32_MASKmskw_YMMf32_AVX512_sae']
                (super::Opcode::VCVTPS2UDQ, EVEXOperandCode::VCVTTPS2UDQ),
                (super::Opcode::VCVTPS2UDQ, EVEXOperandCode::VCVTTPS2UDQ),
            ],
        ),
        (
            0xc2,
            [
                (
                    super::Opcode::VCMPPS,
                    EVEXOperandCode::Mask_V_E_LL_imm8_sae_bcast_W0,
                ),
                (
                    super::Opcode::VCMPPS,
                    EVEXOperandCode::Mask_V_E_LL_imm8_sae_bcast_W0,
                ),
                (
                    super::Opcode::VCMPPS,
                    EVEXOperandCode::Mask_V_E_LL_imm8_sae_bcast_W0,
                ),
                (
                    super::Opcode::VCMPPS,
                    EVEXOperandCode::Mask_V_E_LL_imm8_sae_bcast_W0,
                ),
            ],
        ),
        (
            0xc6,
            [
                (
                    super::Opcode::VSHUFPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W0,
                ),
                (
                    super::Opcode::VSHUFPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W0,
                ),
                (
                    super::Opcode::VSHUFPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
    ];

    const EVEX_66_0F: [(u8, [(super::Opcode, EVEXOperandCode); 4]); 100] = [
        (
            0x10,
            [
                (super::Opcode::VMOVUPD, EVEXOperandCode::Gm_E_LL_W1),
                (super::Opcode::VMOVUPD, EVEXOperandCode::Gm_E_LL_W1),
                (super::Opcode::VMOVUPD, EVEXOperandCode::Gm_E_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x11,
            [
                (super::Opcode::VMOVUPD, EVEXOperandCode::Em_G_LL_W1),
                (super::Opcode::VMOVUPD, EVEXOperandCode::Em_G_LL_W1),
                (super::Opcode::VMOVUPD, EVEXOperandCode::Em_G_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x12,
            [
                (super::Opcode::VMOVLPD, EVEXOperandCode::G_V_Mq_xmm_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x13,
            [
                (super::Opcode::VMOVLPD, EVEXOperandCode::Mq_G_xmm_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x14,
            [
                (
                    super::Opcode::VUNPCKLPD,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VUNPCKLPD,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VUNPCKLPD,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x15,
            [
                (
                    super::Opcode::VUNPCKHPD,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VUNPCKHPD,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VUNPCKHPD,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x16,
            [
                (super::Opcode::VMOVHPD, EVEXOperandCode::G_V_Mq_xmm_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x17,
            [
                (super::Opcode::VMOVHPD, EVEXOperandCode::Mq_G_xmm_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x28,
            [
                (super::Opcode::VMOVAPD, EVEXOperandCode::Gm_E_LL_W1),
                (super::Opcode::VMOVAPD, EVEXOperandCode::Gm_E_LL_W1),
                (super::Opcode::VMOVAPD, EVEXOperandCode::Gm_E_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x29,
            [
                (super::Opcode::VMOVAPD, EVEXOperandCode::Em_G_LL_W1),
                (super::Opcode::VMOVAPD, EVEXOperandCode::Em_G_LL_W1),
                (super::Opcode::VMOVAPD, EVEXOperandCode::Em_G_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x2b,
            [
                (super::Opcode::VMOVNTPD, EVEXOperandCode::M_G_LL_W1),
                (super::Opcode::VMOVNTPD, EVEXOperandCode::M_G_LL_W1),
                (super::Opcode::VMOVNTPD, EVEXOperandCode::M_G_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x2e,
            [
                (super::Opcode::VUCOMISD, EVEXOperandCode::Gm_Eq_xmm_sae_W1),
                (super::Opcode::VUCOMISD, EVEXOperandCode::Gm_Eq_xmm_sae_W1),
                (super::Opcode::VUCOMISD, EVEXOperandCode::Gm_Eq_xmm_sae_W1),
                (super::Opcode::VUCOMISD, EVEXOperandCode::Gm_Eq_xmm_sae_W1),
            ],
        ),
        (
            0x2f,
            [
                (super::Opcode::VCOMISD, EVEXOperandCode::Gm_Eq_xmm_sae_W1),
                (super::Opcode::VCOMISD, EVEXOperandCode::Gm_Eq_xmm_sae_W1),
                (super::Opcode::VCOMISD, EVEXOperandCode::Gm_Eq_xmm_sae_W1),
                (super::Opcode::VCOMISD, EVEXOperandCode::Gm_Eq_xmm_sae_W1),
            ],
        ),
        (
            0x51,
            [
                (
                    super::Opcode::VSQRTPD,
                    EVEXOperandCode::Gm_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VSQRTPD,
                    EVEXOperandCode::Gm_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VSQRTPD,
                    EVEXOperandCode::Gm_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VSQRTPD,
                    EVEXOperandCode::Gm_E_LL_sae_bcast_W1,
                ),
            ],
        ),
        (
            0x54,
            [
                (super::Opcode::VANDPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VANDPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VANDPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x55,
            [
                (super::Opcode::VANDNPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VANDNPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VANDNPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x56,
            [
                (super::Opcode::VORPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VORPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VORPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x57,
            [
                (super::Opcode::VXORPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VXORPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VXORPD, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x58,
            [
                (
                    super::Opcode::VADDPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VADDPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VADDPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VADDPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
            ],
        ),
        (
            0x59,
            [
                (
                    super::Opcode::VMULPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VMULPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VMULPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VMULPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
            ],
        ),
        (
            0x5a,
            [
                (
                    super::Opcode::VCVTPD2PS,
                    EVEXOperandCode::Gm_xmm_E_xmm_sae_bcast_W1,
                ),
                (
                    super::Opcode::VCVTPD2PS,
                    EVEXOperandCode::Gm_xmm_E_ymm_sae_bcast_W1,
                ),
                (
                    super::Opcode::VCVTPD2PS,
                    EVEXOperandCode::Gm_ymm_E_zmm_sae_bcast_W1,
                ),
                (
                    super::Opcode::VCVTPD2PS,
                    EVEXOperandCode::Gm_ymm_U_zmm_sae_W1,
                ),
            ],
        ),
        (
            0x5b,
            [
                (super::Opcode::VCVTPS2DQ, EVEXOperandCode::Gm_E_LL_sae_bcast),
                (super::Opcode::VCVTPS2DQ, EVEXOperandCode::Gm_E_LL_sae_bcast),
                (super::Opcode::VCVTPS2DQ, EVEXOperandCode::Gm_E_LL_sae_bcast),
                (super::Opcode::VCVTPS2DQ, EVEXOperandCode::Gm_E_LL_sae_bcast),
            ],
        ),
        (
            0x5c,
            [
                (
                    super::Opcode::VSUBPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VSUBPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VSUBPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VSUBPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
            ],
        ),
        (
            0x5d,
            [
                (
                    super::Opcode::VMINPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VMINPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VMINPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VMINPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
            ],
        ),
        (
            0x5e,
            [
                (
                    super::Opcode::VDIVPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VDIVPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VDIVPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VDIVPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
            ],
        ),
        (
            0x5f,
            [
                (
                    super::Opcode::VMAXPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VMAXPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VMAXPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
                (
                    super::Opcode::VMAXPD,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1,
                ),
            ],
        ),
        (
            0x60,
            [
                (super::Opcode::VPUNPCKLBW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPUNPCKLBW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPUNPCKLBW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x61,
            [
                (super::Opcode::VPUNPCKLWD, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPUNPCKLWD, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPUNPCKLWD, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x62,
            [
                (
                    super::Opcode::VPUNPCKLDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPUNPCKLDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPUNPCKLDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x63,
            [
                (super::Opcode::VPACKSSWB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPACKSSWB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPACKSSWB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x64,
            [
                (super::Opcode::VPCMPGTB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPCMPGTB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPCMPGTB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x65,
            [
                (super::Opcode::VPCMPGTW, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPCMPGTW, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPCMPGTW, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x66,
            [
                (
                    super::Opcode::VPCMPGTD,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPCMPGTD,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPCMPGTD,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x67,
            [
                (super::Opcode::VPACKUSWB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPACKUSWB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPACKUSWB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x68,
            [
                (super::Opcode::VPUNPCKHBW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPUNPCKHBW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPUNPCKHBW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x69,
            [
                (super::Opcode::VPUNPCKHWD, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPUNPCKHWD, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPUNPCKHWD, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x6a,
            [
                (
                    super::Opcode::VPUNPCKHDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPUNPCKHDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPUNPCKHDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x6b,
            [
                (
                    super::Opcode::VPACKSSDW,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPACKSSDW,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPACKSSDW,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x6c,
            [
                (
                    super::Opcode::VPUNPCKLQDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPUNPCKLQDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPUNPCKLQDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x6d,
            [
                (
                    super::Opcode::VPUNPCKHQDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPUNPCKHQDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPUNPCKHQDQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x6e,
            [
                (super::Opcode::VMOVD, EVEXOperandCode::VMOVD_6e),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x6f,
            [
                (super::Opcode::VMOVDQA32, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VMOVDQA32, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VMOVDQA32, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x70,
            [
                (
                    super::Opcode::VPSHUFD,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W0,
                ),
                (
                    super::Opcode::VPSHUFD,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W0,
                ),
                (
                    super::Opcode::VPSHUFD,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x72,
            [
                (
                    super::Opcode::VPROLD,
                    EVEXOperandCode::Opcode_72_Gm_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPROLD,
                    EVEXOperandCode::Opcode_72_Gm_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPROLD,
                    EVEXOperandCode::Opcode_72_Gm_E_LL_imm8_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x74,
            [
                (super::Opcode::VPCMPEQB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPCMPEQB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPCMPEQB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x75,
            [
                (super::Opcode::VPCMPEQW, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPCMPEQW, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPCMPEQW, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x76,
            [
                (
                    super::Opcode::VPCMPEQD,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPCMPEQD,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPCMPEQD,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x78,
            [
                (super::Opcode::VCVTTPS2UQQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTTPS2UQQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTTPS2UQQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTTPS2UQQ, EVEXOperandCode::VCVTTPS2UQQ),
            ],
        ),
        (
            0x79,
            [
                (super::Opcode::VCVTPS2UQQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTPS2UQQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTPS2UQQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTPS2UQQ, EVEXOperandCode::VCVTTPS2UQQ),
            ],
        ),
        (
            0x7a,
            [
                (super::Opcode::VCVTTPS2QQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTTPS2QQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTTPS2QQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTTPS2QQ, EVEXOperandCode::VCVTTPS2UQQ),
            ],
        ),
        (
            0x7b,
            [
                (super::Opcode::VCVTPS2QQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTPS2QQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTPS2QQ, EVEXOperandCode::VCVTTPS2UQQ),
                (super::Opcode::VCVTPS2QQ, EVEXOperandCode::VCVTTPS2UQQ),
            ],
        ),
        (
            0x7e,
            [
                (super::Opcode::VMOVD, EVEXOperandCode::VMOVD_7e),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x7f,
            [
                (super::Opcode::VMOVDQA32, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VMOVDQA32, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VMOVDQA32, EVEXOperandCode::Em_G_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xc2,
            [
                (
                    super::Opcode::VCMPPD,
                    EVEXOperandCode::Maskm_V_E_LL_imm8_sae_bcast_W1,
                ),
                (
                    super::Opcode::VCMPPD,
                    EVEXOperandCode::Maskm_V_E_LL_imm8_sae_bcast_W1,
                ),
                (
                    super::Opcode::VCMPPD,
                    EVEXOperandCode::Maskm_V_E_LL_imm8_sae_bcast_W1,
                ),
                (
                    super::Opcode::VCMPPD,
                    EVEXOperandCode::Maskm_V_E_LL_imm8_sae_bcast_W1,
                ),
            ],
        ),
        (
            0xc4,
            [
                (super::Opcode::VPINSRW, EVEXOperandCode::VPINSRW),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xc5,
            [
                (super::Opcode::VPEXTRW, EVEXOperandCode::VPEXTRW),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xc6,
            [
                (
                    super::Opcode::VSHUFPD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W1,
                ),
                (
                    super::Opcode::VSHUFPD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W1,
                ),
                (
                    super::Opcode::VSHUFPD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xd1,
            [
                (super::Opcode::VPSRLW, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::VPSRLW, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::VPSRLW, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xd2,
            [
                (super::Opcode::VPSRLD, EVEXOperandCode::Gm_V_LL_E_xmm_W0),
                (super::Opcode::VPSRLD, EVEXOperandCode::Gm_V_LL_E_xmm_W0),
                (super::Opcode::VPSRLD, EVEXOperandCode::Gm_V_LL_E_xmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xd3,
            [
                (super::Opcode::VPSRLQ, EVEXOperandCode::Gm_V_LL_E_xmm_W1),
                (super::Opcode::VPSRLQ, EVEXOperandCode::Gm_V_LL_E_xmm_W1),
                (super::Opcode::VPSRLQ, EVEXOperandCode::Gm_V_LL_E_xmm_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xd4,
            [
                (super::Opcode::VPADDQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VPADDQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VPADDQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xd5,
            [
                (super::Opcode::VPMULLW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMULLW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMULLW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xd6,
            [
                (super::Opcode::VMOVQ, EVEXOperandCode::VMOVQ_Ed_G_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xd8,
            [
                (super::Opcode::VPSUBUSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBUSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBUSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xd9,
            [
                (super::Opcode::VPSUBUSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBUSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBUSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xda,
            [
                (super::Opcode::VPMINUB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMINUB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMINUB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xdb,
            [
                (super::Opcode::VPANDD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPANDD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPANDD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xdc,
            [
                (super::Opcode::VPADDUSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDUSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDUSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xdd,
            [
                (super::Opcode::VPADDUSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDUSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDUSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xde,
            [
                (super::Opcode::VPMAXUB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMAXUB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMAXUB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xdf,
            [
                (super::Opcode::VPANDND, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPANDND, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPANDND, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xe0,
            [
                (super::Opcode::VPAVGB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPAVGB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPAVGB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xe1,
            [
                (super::Opcode::VPSRAW, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::VPSRAW, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::VPSRAW, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xe2,
            [
                (super::Opcode::VPSRAD, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::VPSRAD, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::VPSRAD, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xe3,
            [
                (super::Opcode::VPAVGW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPAVGW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPAVGW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xe4,
            [
                (super::Opcode::VPMULHUW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMULHUW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMULHUW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xe5,
            [
                (super::Opcode::VPMULHW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMULHW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMULHW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xe6,
            [
                (super::Opcode::VCVTTPD2DQ, EVEXOperandCode::VCVTTPD2DQ),
                (super::Opcode::VCVTTPD2DQ, EVEXOperandCode::VCVTTPD2DQ),
                (super::Opcode::VCVTTPD2DQ, EVEXOperandCode::VCVTTPD2DQ),
                (super::Opcode::VCVTTPD2DQ, EVEXOperandCode::VCVTTPD2DQ),
            ],
        ),
        (
            0xe7,
            [
                (super::Opcode::VMOVNTDQ, EVEXOperandCode::E_G_LL_W0),
                (super::Opcode::VMOVNTDQ, EVEXOperandCode::E_G_LL_W0),
                (super::Opcode::VMOVNTDQ, EVEXOperandCode::E_G_LL_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xe8,
            [
                (super::Opcode::VPSUBSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xe9,
            [
                (super::Opcode::VPSUBSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xea,
            [
                (super::Opcode::VPMINSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMINSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMINSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xeb,
            [
                (super::Opcode::VPORD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPORD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPORD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xec,
            [
                (super::Opcode::VPADDSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xed,
            [
                (super::Opcode::VPADDSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xee,
            [
                (super::Opcode::VPMAXSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMAXSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMAXSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xef,
            [
                (super::Opcode::VPXORD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPXORD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPXORD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xf1,
            [
                (super::Opcode::VPSLLW, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::VPSLLW, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::VPSLLW, EVEXOperandCode::Gm_V_LL_E_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xf2,
            [
                (super::Opcode::VPSLLD, EVEXOperandCode::Gm_V_LL_E_xmm_W0),
                (super::Opcode::VPSLLD, EVEXOperandCode::Gm_V_LL_E_xmm_W0),
                (super::Opcode::VPSLLD, EVEXOperandCode::Gm_V_LL_E_xmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xf3,
            [
                (super::Opcode::VPSLLQ, EVEXOperandCode::Gm_V_LL_E_xmm_W1),
                (super::Opcode::VPSLLQ, EVEXOperandCode::Gm_V_LL_E_xmm_W1),
                (super::Opcode::VPSLLQ, EVEXOperandCode::Gm_V_LL_E_xmm_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xf4,
            [
                (super::Opcode::VPMULUDQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VPMULUDQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VPMULUDQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xf5,
            [
                (super::Opcode::VPMADDWD, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMADDWD, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMADDWD, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xf6,
            [
                (super::Opcode::VPSADBW, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::VPSADBW, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::VPSADBW, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xf8,
            [
                (super::Opcode::VPSUBB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xf9,
            [
                (super::Opcode::VPSUBW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSUBW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xfa,
            [
                (super::Opcode::VPSUBD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VPSUBD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VPSUBD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xfb,
            [
                (super::Opcode::VPSUBQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VPSUBQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VPSUBQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xfc,
            [
                (super::Opcode::VPADDB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xfd,
            [
                (super::Opcode::VPADDW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPADDW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xfe,
            [
                (super::Opcode::VPADDD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VPADDD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VPADDD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
    ];

    const EVEX_66_0F38: [(u8, [(super::Opcode, EVEXOperandCode); 4]); 143] = [
        (
            0x00,
            [
                (super::Opcode::VPSHUFB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSHUFB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPSHUFB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x04,
            [
                (super::Opcode::VPMADDUBSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMADDUBSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMADDUBSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x0b,
            [
                (super::Opcode::VPMULHRSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMULHRSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMULHRSW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x0c,
            [
                (
                    super::Opcode::VPERMILPS,
                    EVEXOperandCode::Gm_V_Ed_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPERMILPS,
                    EVEXOperandCode::Gm_V_Ed_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPERMILPS,
                    EVEXOperandCode::Gm_V_Ed_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x0d,
            [
                (
                    super::Opcode::VPERMILPD,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPERMILPD,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPERMILPD,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x10,
            [
                (super::Opcode::VPSRLVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::VPSRLVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::VPSRLVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x11,
            [
                (super::Opcode::VPSRAVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::VPSRAVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::VPSRAVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x12,
            [
                (super::Opcode::VPSLLVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::VPSLLVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::VPSLLVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x13,
            [
                (super::Opcode::VCVTPH2PS, EVEXOperandCode::VCVTPH2PS),
                (super::Opcode::VCVTPH2PS, EVEXOperandCode::VCVTPH2PS),
                (super::Opcode::VCVTPH2PS, EVEXOperandCode::VCVTPH2PS),
                (super::Opcode::VCVTPH2PS, EVEXOperandCode::VCVTPH2PS),
            ],
        ),
        (
            0x14,
            [
                (super::Opcode::VPRORVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPRORVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPRORVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x15,
            [
                (super::Opcode::VPROLVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPROLVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPROLVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x16,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::VPERMPS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::VPERMPS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x18,
            [
                (
                    super::Opcode::VBROADCASTSS,
                    EVEXOperandCode::Gm_xmm_Ed_xmm_W0,
                ),
                (
                    super::Opcode::VBROADCASTSS,
                    EVEXOperandCode::Gm_ymm_Ed_xmm_W0,
                ),
                (
                    super::Opcode::VBROADCASTSS,
                    EVEXOperandCode::Gm_zmm_Ed_xmm_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x19,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VBROADCASTF32X2,
                    EVEXOperandCode::VBROADCASTF32X2_Gm_ymm_Ed_xmm,
                ),
                (
                    super::Opcode::VBROADCASTF32X2,
                    EVEXOperandCode::Gm_zmm_Ed_xmm,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x1a,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VBROADCASTF32X4,
                    EVEXOperandCode::Gm_ymm_M_xmm,
                ),
                (
                    super::Opcode::VBROADCASTF32X4,
                    EVEXOperandCode::Gm_zmm_M_xmm,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x1b,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VBROADCASTF32X8,
                    EVEXOperandCode::Gm_zmm_M_ymm,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x1c,
            [
                (super::Opcode::VPABSB, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VPABSB, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VPABSB, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x1d,
            [
                (super::Opcode::VPABSW, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VPABSW, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VPABSW, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x1e,
            [
                (super::Opcode::VPABSD, EVEXOperandCode::Gm_E_LL_bcast_W0),
                (super::Opcode::VPABSD, EVEXOperandCode::Gm_E_LL_bcast_W0),
                (super::Opcode::VPABSD, EVEXOperandCode::Gm_E_LL_bcast_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x1f,
            [
                (super::Opcode::VPABSQ, EVEXOperandCode::Gm_E_LL_bcast_W1),
                (super::Opcode::VPABSQ, EVEXOperandCode::Gm_E_LL_bcast_W1),
                (super::Opcode::VPABSQ, EVEXOperandCode::Gm_E_LL_bcast_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x20,
            [
                (super::Opcode::VPMOVSXBW, EVEXOperandCode::Gm_xmm_Eq_xmm),
                (super::Opcode::VPMOVSXBW, EVEXOperandCode::Gm_ymm_E_xmm),
                (super::Opcode::VPMOVSXBW, EVEXOperandCode::Gm_zmm_E_ymm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x21,
            [
                (super::Opcode::VPMOVSXBD, EVEXOperandCode::Gm_xmm_Ed_xmm),
                (super::Opcode::VPMOVSXBD, EVEXOperandCode::Gm_ymm_Eq_xmm),
                (super::Opcode::VPMOVSXBD, EVEXOperandCode::Gm_zmm_E_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x22,
            [
                (super::Opcode::VPMOVSXBQ, EVEXOperandCode::Gm_xmm_Ew_xmm),
                (super::Opcode::VPMOVSXBQ, EVEXOperandCode::Gm_ymm_Ed_xmm),
                (super::Opcode::VPMOVSXBQ, EVEXOperandCode::Gm_zmm_Eq_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x23,
            [
                (super::Opcode::VPMOVSXWD, EVEXOperandCode::Gm_xmm_Eq_xmm),
                (super::Opcode::VPMOVSXWD, EVEXOperandCode::Gm_ymm_E_xmm),
                (super::Opcode::VPMOVSXWD, EVEXOperandCode::Gm_zmm_E_ymm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x24,
            [
                (super::Opcode::VPMOVSXWQ, EVEXOperandCode::Gm_xmm_Ed_xmm),
                (super::Opcode::VPMOVSXWQ, EVEXOperandCode::Gm_ymm_Eq_xmm),
                (super::Opcode::VPMOVSXWQ, EVEXOperandCode::Gm_zmm_E_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x25,
            [
                (super::Opcode::VPMOVSXDQ, EVEXOperandCode::Gm_xmm_Eq_xmm_W0),
                (super::Opcode::VPMOVSXDQ, EVEXOperandCode::Gm_ymm_E_xmm_W0),
                (super::Opcode::VPMOVSXDQ, EVEXOperandCode::Gm_zmm_E_ymm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x26,
            [
                (super::Opcode::VPTESTMB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPTESTMB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPTESTMB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x27,
            [
                (super::Opcode::VPTESTMD, EVEXOperandCode::Mask_V_E_LL_bcast),
                (super::Opcode::VPTESTMD, EVEXOperandCode::Mask_V_E_LL_bcast),
                (super::Opcode::VPTESTMD, EVEXOperandCode::Mask_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x28,
            [
                (super::Opcode::VPMULDQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VPMULDQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::VPMULDQ, EVEXOperandCode::Gm_V_E_LL_bcast_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x29,
            [
                (
                    super::Opcode::VPCMPEQQ,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPCMPEQQ,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPCMPEQQ,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x2a,
            [
                (super::Opcode::VMOVNTDQA, EVEXOperandCode::G_E_LL_W0),
                (super::Opcode::VMOVNTDQA, EVEXOperandCode::G_E_LL_W0),
                (super::Opcode::VMOVNTDQA, EVEXOperandCode::G_E_LL_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x2b,
            [
                (
                    super::Opcode::VPACKUSDW,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPACKUSDW,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPACKUSDW,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x2c,
            [
                (
                    super::Opcode::VSCALEFPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VSCALEFPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VSCALEFPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VSCALEFPS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0x2d,
            [
                (super::Opcode::VSCALEFSS, EVEXOperandCode::Gm_V_Ed_xmm_sae),
                (super::Opcode::VSCALEFSS, EVEXOperandCode::Gm_V_Ed_xmm_sae),
                (super::Opcode::VSCALEFSS, EVEXOperandCode::Gm_V_Ed_xmm_sae),
                (super::Opcode::VSCALEFSS, EVEXOperandCode::Gm_V_Ed_xmm_sae),
            ],
        ),
        (
            0x30,
            [
                (super::Opcode::VPMOVZXBW, EVEXOperandCode::Gm_xmm_Eq_xmm),
                (super::Opcode::VPMOVZXBW, EVEXOperandCode::Gm_ymm_E_xmm),
                (super::Opcode::VPMOVZXBW, EVEXOperandCode::Gm_zmm_E_ymm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x31,
            [
                (super::Opcode::VPMOVZXBD, EVEXOperandCode::Gm_xmm_Ed_xmm),
                (super::Opcode::VPMOVZXBD, EVEXOperandCode::Gm_ymm_Eq_xmm),
                (super::Opcode::VPMOVZXBD, EVEXOperandCode::Gm_zmm_E_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x32,
            [
                (super::Opcode::VPMOVZXBQ, EVEXOperandCode::Gm_xmm_Ew_xmm),
                (super::Opcode::VPMOVZXBQ, EVEXOperandCode::Gm_ymm_Ed_xmm),
                (super::Opcode::VPMOVZXBQ, EVEXOperandCode::Gm_zmm_Eq_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x33,
            [
                (super::Opcode::VPMOVZXWD, EVEXOperandCode::Gm_xmm_Eq_xmm),
                (super::Opcode::VPMOVZXWD, EVEXOperandCode::Gm_ymm_E_xmm),
                (super::Opcode::VPMOVZXWD, EVEXOperandCode::Gm_zmm_E_ymm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x34,
            [
                (super::Opcode::VPMOVZXWQ, EVEXOperandCode::Gm_xmm_Ed_xmm),
                (super::Opcode::VPMOVZXWQ, EVEXOperandCode::Gm_ymm_Eq_xmm),
                (super::Opcode::VPMOVZXWQ, EVEXOperandCode::Gm_zmm_E_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x35,
            [
                (super::Opcode::VPMOVZXDQ, EVEXOperandCode::Gm_xmm_Eq_xmm_W0),
                (super::Opcode::VPMOVZXDQ, EVEXOperandCode::Gm_ymm_E_xmm_W0),
                (super::Opcode::VPMOVZXDQ, EVEXOperandCode::Gm_zmm_E_ymm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x36,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::VPERMD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPERMD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x37,
            [
                (
                    super::Opcode::VPCMPGTQ,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPCMPGTQ,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPCMPGTQ,
                    EVEXOperandCode::Mask_V_E_LL_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x38,
            [
                (super::Opcode::VPMINSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMINSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMINSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x39,
            [
                (super::Opcode::VPMINSD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPMINSD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPMINSD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3a,
            [
                (super::Opcode::VPMINUW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMINUW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMINUW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3b,
            [
                (super::Opcode::VPMINUD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPMINUD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPMINUD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3c,
            [
                (super::Opcode::VPMAXSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMAXSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMAXSB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3d,
            [
                (super::Opcode::VPMAXSD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPMAXSD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPMAXSD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3e,
            [
                (super::Opcode::VPMAXUW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMAXUW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPMAXUW, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3f,
            [
                (super::Opcode::VPMAXUD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPMAXUD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPMAXUD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x40,
            [
                (super::Opcode::VPMULLD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPMULLD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPMULLD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x42,
            [
                (super::Opcode::VGETEXPPS, EVEXOperandCode::Gm_E_LL_sae_bcast),
                (super::Opcode::VGETEXPPS, EVEXOperandCode::Gm_E_LL_sae_bcast),
                (super::Opcode::VGETEXPPS, EVEXOperandCode::Gm_E_LL_sae_bcast),
                (super::Opcode::VGETEXPPS, EVEXOperandCode::Gm_E_LL_sae_bcast),
            ],
        ),
        (
            0x43,
            [
                (
                    super::Opcode::VGETEXPSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_sae_bcast,
                ),
                (
                    super::Opcode::VGETEXPSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_sae_bcast,
                ),
                (
                    super::Opcode::VGETEXPSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_sae_bcast,
                ),
                (
                    super::Opcode::VGETEXPSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_sae_bcast,
                ),
            ],
        ),
        (
            0x44,
            [
                (super::Opcode::VPLZCNTD, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::VPLZCNTD, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::VPLZCNTD, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x45,
            [
                (super::Opcode::VPSRLVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPSRLVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPSRLVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x46,
            [
                (super::Opcode::VPSRAVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPSRAVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPSRAVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x47,
            [
                (super::Opcode::VPSLLVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPSLLVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPSLLVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x4c,
            [
                (super::Opcode::VRCP14PS, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::VRCP14PS, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::VRCP14PS, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x4d,
            [
                (super::Opcode::VRCP14SS, EVEXOperandCode::Gm_V_Ed_xmm_sae),
                (super::Opcode::VRCP14SS, EVEXOperandCode::Gm_V_Ed_xmm_sae),
                (super::Opcode::VRCP14SS, EVEXOperandCode::Gm_V_Ed_xmm_sae),
                (super::Opcode::VRCP14SS, EVEXOperandCode::Gm_V_Ed_xmm_sae),
            ],
        ),
        (
            0x4e,
            [
                (
                    super::Opcode::VRSQRT14PS,
                    EVEXOperandCode::Gm_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VRSQRT14PS,
                    EVEXOperandCode::Gm_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VRSQRT14PS,
                    EVEXOperandCode::Gm_E_LL_sae_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x4f,
            [
                (super::Opcode::VRSQRT14SS, EVEXOperandCode::Gm_V_Ed_xmm),
                (super::Opcode::VRSQRT14SS, EVEXOperandCode::Gm_V_Ed_xmm),
                (super::Opcode::VRSQRT14SS, EVEXOperandCode::Gm_V_Ed_xmm),
                (super::Opcode::VRSQRT14SS, EVEXOperandCode::Gm_V_Ed_xmm),
            ],
        ),
        (
            0x50,
            [
                (super::Opcode::VPDPBUSD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VPDPBUSD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VPDPBUSD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x51,
            [
                (
                    super::Opcode::VPDPBUSDS,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPDPBUSDS,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPDPBUSDS,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x52,
            [
                (super::Opcode::VPDPWSSD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VPDPWSSD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::VPDPWSSD, EVEXOperandCode::Gm_V_E_LL_bcast_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x53,
            [
                (
                    super::Opcode::VPDPWSSDS,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPDPWSSDS,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VPDPWSSDS,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x54,
            [
                (super::Opcode::VPOPCNTB, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VPOPCNTB, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VPOPCNTB, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x55,
            [
                (super::Opcode::VPOPCNTD, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::VPOPCNTD, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::VPOPCNTD, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x58,
            [
                (
                    super::Opcode::VPBROADCASTD,
                    EVEXOperandCode::Gm_LL_Ed_xmm_W0,
                ),
                (
                    super::Opcode::VPBROADCASTD,
                    EVEXOperandCode::Gm_LL_Ed_xmm_W0,
                ),
                (
                    super::Opcode::VPBROADCASTD,
                    EVEXOperandCode::Gm_LL_Ed_xmm_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x59,
            [
                (
                    super::Opcode::VBROADCASTI32X2,
                    EVEXOperandCode::Gm_LL_Eq_xmm,
                ),
                (
                    super::Opcode::VBROADCASTI32X2,
                    EVEXOperandCode::Gm_LL_Eq_xmm,
                ),
                (
                    super::Opcode::VBROADCASTI32X2,
                    EVEXOperandCode::Gm_LL_Eq_xmm,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x5a,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VBROADCASTI32X4,
                    EVEXOperandCode::Gm_ymm_M_xmm,
                ),
                (
                    super::Opcode::VBROADCASTI32X4,
                    EVEXOperandCode::Gm_zmm_M_xmm,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x5b,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VBROADCASTI32X8,
                    EVEXOperandCode::Gm_zmm_M_ymm,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x62,
            [
                (super::Opcode::VPEXPANDB, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VPEXPANDB, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VPEXPANDB, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x63,
            [
                (super::Opcode::VPCOMPRESSB, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VPCOMPRESSB, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VPCOMPRESSB, EVEXOperandCode::Em_G_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x64,
            [
                (super::Opcode::VPBLENDMD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPBLENDMD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPBLENDMD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x65,
            [
                (super::Opcode::VBLENDMPS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::VBLENDMPS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::VBLENDMPS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x66,
            [
                (super::Opcode::VPBLENDMB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPBLENDMB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPBLENDMB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x70,
            [
                (super::Opcode::VPSHLDVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::VPSHLDVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::VPSHLDVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x71,
            [
                (super::Opcode::VPSHLDVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPSHLDVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPSHLDVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x72,
            [
                (super::Opcode::VPSHRDVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::VPSHRDVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::VPSHRDVW, EVEXOperandCode::Gm_V_E_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x73,
            [
                (super::Opcode::VPSHRDVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPSHRDVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPSHRDVD, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x75,
            [
                (super::Opcode::VPERMI2B, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPERMI2B, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPERMI2B, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x76,
            [
                (super::Opcode::VPERMI2D, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPERMI2D, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPERMI2D, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x77,
            [
                (super::Opcode::VPERMI2PS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::VPERMI2PS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::VPERMI2PS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x78,
            [
                (
                    super::Opcode::VPBROADCASTB,
                    EVEXOperandCode::Gm_LL_Eb_xmm_W0,
                ),
                (
                    super::Opcode::VPBROADCASTB,
                    EVEXOperandCode::Gm_LL_Eb_xmm_W0,
                ),
                (
                    super::Opcode::VPBROADCASTB,
                    EVEXOperandCode::Gm_LL_Eb_xmm_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x79,
            [
                (
                    super::Opcode::VPBROADCASTW,
                    EVEXOperandCode::Gm_LL_Ew_xmm_W0,
                ),
                (
                    super::Opcode::VPBROADCASTW,
                    EVEXOperandCode::Gm_LL_Ew_xmm_W0,
                ),
                (
                    super::Opcode::VPBROADCASTW,
                    EVEXOperandCode::Gm_LL_Ew_xmm_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x7a,
            [
                (super::Opcode::VPBROADCASTB, EVEXOperandCode::Gm_LL_Ud_W0),
                (super::Opcode::VPBROADCASTB, EVEXOperandCode::Gm_LL_Ud_W0),
                (super::Opcode::VPBROADCASTB, EVEXOperandCode::Gm_LL_Ud_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x7b,
            [
                (super::Opcode::VPBROADCASTW, EVEXOperandCode::Gm_LL_Ud_W0),
                (super::Opcode::VPBROADCASTW, EVEXOperandCode::Gm_LL_Ud_W0),
                (super::Opcode::VPBROADCASTW, EVEXOperandCode::Gm_LL_Ud_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x7c,
            [
                (super::Opcode::VPBROADCASTD, EVEXOperandCode::Gm_LL_Ud),
                (super::Opcode::VPBROADCASTD, EVEXOperandCode::Gm_LL_Ud),
                (super::Opcode::VPBROADCASTD, EVEXOperandCode::Gm_LL_Ud),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x7d,
            [
                (super::Opcode::VPERMT2B, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPERMT2B, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPERMT2B, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x7e,
            [
                (super::Opcode::VPERMT2D, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPERMT2D, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::VPERMT2D, EVEXOperandCode::Gm_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x7f,
            [
                (super::Opcode::VPERMT2PS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::VPERMT2PS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::VPERMT2PS, EVEXOperandCode::Gm_V_Ed_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x83,
            [
                (
                    super::Opcode::VPMULTISHIFTQB,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPMULTISHIFTQB,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPMULTISHIFTQB,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x88,
            [
                (super::Opcode::VEXPANDPS, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VEXPANDPS, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VEXPANDPS, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x89,
            [
                (super::Opcode::VPEXPANDD, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VPEXPANDD, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VPEXPANDD, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x8a,
            [
                (super::Opcode::VCOMPRESSPS, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VCOMPRESSPS, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VCOMPRESSPS, EVEXOperandCode::Em_G_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x8b,
            [
                (super::Opcode::VPCOMPRESSD, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VPCOMPRESSD, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VPCOMPRESSD, EVEXOperandCode::Em_G_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x8d,
            [
                (super::Opcode::VPERMB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPERMB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::VPERMB, EVEXOperandCode::Gm_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x8f,
            [
                (super::Opcode::VPSHUFBITQMB, EVEXOperandCode::Mask_V_E_LL_W0),
                (super::Opcode::VPSHUFBITQMB, EVEXOperandCode::Mask_V_E_LL_W0),
                (super::Opcode::VPSHUFBITQMB, EVEXOperandCode::Mask_V_E_LL_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x96,
            [
                (
                    super::Opcode::VFMADDSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADDSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADDSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADDSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0x97,
            [
                (
                    super::Opcode::VFMSUBADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUBADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUBADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUBADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0x98,
            [
                (
                    super::Opcode::VFMADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0x99,
            [
                (super::Opcode::VFMADD132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMADD132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMADD132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMADD132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0x9a,
            [
                (
                    super::Opcode::VFMSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0x9b,
            [
                (super::Opcode::VFMSUB132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMSUB132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMSUB132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMSUB132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0x9c,
            [
                (
                    super::Opcode::VFNMADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMADD132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0x9d,
            [
                (super::Opcode::VFNMADD132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMADD132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMADD132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMADD132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0x9e,
            [
                (
                    super::Opcode::VFNMSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMSUB132PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0x9f,
            [
                (super::Opcode::VFNMSUB132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMSUB132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMSUB132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMSUB132SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0xa0,
            [
                (
                    super::Opcode::VPSCATTERDD,
                    EVEXOperandCode::Mvector_Mask_G_LL,
                ),
                (
                    super::Opcode::VPSCATTERDD,
                    EVEXOperandCode::Mvector_Mask_G_LL,
                ),
                (
                    super::Opcode::VPSCATTERDD,
                    EVEXOperandCode::Mvector_Mask_G_LL,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xa1,
            [
                (
                    super::Opcode::VPSCATTERQD,
                    EVEXOperandCode::Mvector_Mask_G_LL,
                ),
                (
                    super::Opcode::VPSCATTERQD,
                    EVEXOperandCode::Mvector_Mask_G_LL,
                ),
                (
                    super::Opcode::VPSCATTERQD,
                    EVEXOperandCode::Mvector_Mask_G_LL,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xa6,
            [
                (
                    super::Opcode::VFMADDSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADDSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADDSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADDSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xa7,
            [
                (
                    super::Opcode::VFMSUBADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUBADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUBADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUBADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xa8,
            [
                (
                    super::Opcode::VFMADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xa9,
            [
                (super::Opcode::VFMADD213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMADD213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMADD213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMADD213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0xaa,
            [
                (
                    super::Opcode::VFMSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xab,
            [
                (super::Opcode::VFMSUB213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMSUB213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMSUB213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMSUB213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0xac,
            [
                (
                    super::Opcode::VFNMADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMADD213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xad,
            [
                (super::Opcode::VFNMADD213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMADD213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMADD213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMADD213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0xae,
            [
                (
                    super::Opcode::VFNMSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMSUB213PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xaf,
            [
                (super::Opcode::VFNMSUB213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMSUB213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMSUB213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMSUB213SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0xb4,
            [
                (
                    super::Opcode::VPMADD52LUQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPMADD52LUQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPMADD52LUQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xb5,
            [
                (
                    super::Opcode::VPMADD52HUQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPMADD52HUQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (
                    super::Opcode::VPMADD52HUQ,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xb6,
            [
                (
                    super::Opcode::VFMADDSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADDSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADDSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADDSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xb7,
            [
                (
                    super::Opcode::VFMSUBADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUBADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUBADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUBADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xb8,
            [
                (
                    super::Opcode::VFMADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xb9,
            [
                (super::Opcode::VFMADD231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMADD231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMADD231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMADD231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0xba,
            [
                (
                    super::Opcode::VFMSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFMSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xbb,
            [
                (super::Opcode::VFMSUB231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMSUB231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMSUB231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFMSUB231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0xbc,
            [
                (
                    super::Opcode::VFNMADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMADD231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xbd,
            [
                (super::Opcode::VFNMADD231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMADD231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMADD231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMADD231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0xbe,
            [
                (
                    super::Opcode::VFNMSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
                (
                    super::Opcode::VFNMSUB231PS,
                    EVEXOperandCode::Gm_V_E_LL_sae_bcast,
                ),
            ],
        ),
        (
            0xbf,
            [
                (super::Opcode::VFNMSUB231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMSUB231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMSUB231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
                (super::Opcode::VFNMSUB231SS, EVEXOperandCode::Gm_V_Ed_LL_sae),
            ],
        ),
        (
            0xc4,
            [
                (super::Opcode::VPCONFLICTD, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::VPCONFLICTD, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::VPCONFLICTD, EVEXOperandCode::Gm_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xc8,
            [
                (super::Opcode::VEXP2PS, EVEXOperandCode::Gm_E_zmm_sae_bcast),
                (super::Opcode::VEXP2PS, EVEXOperandCode::Gm_E_zmm_sae_bcast),
                (super::Opcode::VEXP2PS, EVEXOperandCode::Gm_E_zmm_sae_bcast),
                (super::Opcode::VEXP2PS, EVEXOperandCode::Gm_E_zmm_sae_bcast),
            ],
        ),
        (
            0xca,
            [
                (super::Opcode::VRCP28PS, EVEXOperandCode::Gm_E_zmm_sae_bcast),
                (super::Opcode::VRCP28PS, EVEXOperandCode::Gm_E_zmm_sae_bcast),
                (super::Opcode::VRCP28PS, EVEXOperandCode::Gm_E_zmm_sae_bcast),
                (super::Opcode::VRCP28PS, EVEXOperandCode::Gm_E_zmm_sae_bcast),
            ],
        ),
        (
            0xcb,
            [
                (super::Opcode::VRCP28SS, EVEXOperandCode::Gm_V_E_xmm_sae),
                (super::Opcode::VRCP28SS, EVEXOperandCode::Gm_V_E_xmm_sae),
                (super::Opcode::VRCP28SS, EVEXOperandCode::Gm_V_E_xmm_sae),
                (super::Opcode::VRCP28SS, EVEXOperandCode::Gm_V_E_xmm_sae),
            ],
        ),
        (
            0xcc,
            [
                (
                    super::Opcode::VRSQRT28PS,
                    EVEXOperandCode::Gm_E_zmm_sae_bcast,
                ),
                (
                    super::Opcode::VRSQRT28PS,
                    EVEXOperandCode::Gm_E_zmm_sae_bcast,
                ),
                (
                    super::Opcode::VRSQRT28PS,
                    EVEXOperandCode::Gm_E_zmm_sae_bcast,
                ),
                (
                    super::Opcode::VRSQRT28PS,
                    EVEXOperandCode::Gm_E_zmm_sae_bcast,
                ),
            ],
        ),
        (
            0xcd,
            [
                (super::Opcode::VRSQRT28SS, EVEXOperandCode::Gm_V_E_xmm_sae),
                (super::Opcode::VRSQRT28SS, EVEXOperandCode::Gm_V_E_xmm_sae),
                (super::Opcode::VRSQRT28SS, EVEXOperandCode::Gm_V_E_xmm_sae),
                (super::Opcode::VRSQRT28SS, EVEXOperandCode::Gm_V_E_xmm_sae),
            ],
        ),
        (
            0xcf,
            [
                (super::Opcode::VGF2P8MULB, EVEXOperandCode::Gm_V_E_LL_W0),
                (super::Opcode::VGF2P8MULB, EVEXOperandCode::Gm_V_E_LL_W0),
                (super::Opcode::VGF2P8MULB, EVEXOperandCode::Gm_V_E_LL_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xdc,
            [
                (super::Opcode::VAESENC, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::VAESENC, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::VAESENC, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xdd,
            [
                (super::Opcode::VAESENCLAST, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::VAESENCLAST, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::VAESENCLAST, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xde,
            [
                (super::Opcode::VAESDEC, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::VAESDEC, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::VAESDEC, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xdf,
            [
                (super::Opcode::VAESDECLAST, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::VAESDECLAST, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::VAESDECLAST, EVEXOperandCode::G_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
    ];

    const EVEX_66_0F3A: [(u8, [(super::Opcode, EVEXOperandCode); 4]); 51] = [
        (
            0x00,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VPERMQ,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W1,
                ),
                (
                    super::Opcode::VPERMQ,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x01,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VPERMPD,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W1,
                ),
                (
                    super::Opcode::VPERMPD,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x03,
            [
                (
                    super::Opcode::VALIGND,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VALIGND,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VALIGND,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x04,
            [
                (
                    super::Opcode::VPERMILPS,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W0,
                ),
                (
                    super::Opcode::VPERMILPS,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W0,
                ),
                (
                    super::Opcode::VPERMILPS,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x05,
            [
                (
                    super::Opcode::VPERMILPD,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W1,
                ),
                (
                    super::Opcode::VPERMILPD,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W1,
                ),
                (
                    super::Opcode::VPERMILPD,
                    EVEXOperandCode::Gm_E_LL_imm8_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x08,
            [
                (
                    super::Opcode::VRNDSCALEPS,
                    EVEXOperandCode::Gm_E_LL_imm8_sae_W0,
                ),
                (
                    super::Opcode::VRNDSCALEPS,
                    EVEXOperandCode::Gm_E_LL_imm8_sae_W0,
                ),
                (
                    super::Opcode::VRNDSCALEPS,
                    EVEXOperandCode::Gm_E_LL_imm8_sae_W0,
                ),
                (
                    super::Opcode::VRNDSCALEPS,
                    EVEXOperandCode::Gm_E_LL_imm8_sae_W0,
                ),
            ],
        ),
        (
            0x09,
            [
                (
                    super::Opcode::VRNDSCALEPD,
                    EVEXOperandCode::Gm_E_LL_imm8_sae_W1,
                ),
                (
                    super::Opcode::VRNDSCALEPD,
                    EVEXOperandCode::Gm_E_LL_imm8_sae_W1,
                ),
                (
                    super::Opcode::VRNDSCALEPD,
                    EVEXOperandCode::Gm_E_LL_imm8_sae_W1,
                ),
                (
                    super::Opcode::VRNDSCALEPD,
                    EVEXOperandCode::Gm_E_LL_imm8_sae_W1,
                ),
            ],
        ),
        (
            0x0a,
            [
                (
                    super::Opcode::VRNDSCALESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VRNDSCALESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VRNDSCALESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VRNDSCALESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
            ],
        ), // W0
        (
            0x0b,
            [
                (
                    super::Opcode::VRNDSCALESD,
                    EVEXOperandCode::Gm_V_E_xmm_imm8_sae_W1,
                ),
                (
                    super::Opcode::VRNDSCALESD,
                    EVEXOperandCode::Gm_V_E_xmm_imm8_sae_W1,
                ),
                (
                    super::Opcode::VRNDSCALESD,
                    EVEXOperandCode::Gm_V_E_xmm_imm8_sae_W1,
                ),
                (
                    super::Opcode::VRNDSCALESD,
                    EVEXOperandCode::Gm_V_E_xmm_imm8_sae_W1,
                ),
            ],
        ), // W1
        (
            0x0f,
            [
                (super::Opcode::VPALIGNR, EVEXOperandCode::Gm_V_E_LL_imm8),
                (super::Opcode::VPALIGNR, EVEXOperandCode::Gm_V_E_LL_imm8),
                (super::Opcode::VPALIGNR, EVEXOperandCode::Gm_V_E_LL_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x14,
            [
                (super::Opcode::VPEXTRB, EVEXOperandCode::Ebd_G_xmm_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x15,
            [
                (super::Opcode::VPEXTRW, EVEXOperandCode::Ewd_G_xmm_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x16,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Edd_G_xmm_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x17,
            [
                (super::Opcode::VEXTRACTPS, EVEXOperandCode::VEXTRACTPS),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x18,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VINSERTF32X4,
                    EVEXOperandCode::Gm_V_LL_E_xmm_imm8,
                ),
                (
                    super::Opcode::VINSERTF32X4,
                    EVEXOperandCode::Gm_V_LL_E_xmm_imm8,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x19,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VEXTRACTF32X4,
                    EVEXOperandCode::Em_xmm_G_LL_imm8,
                ),
                (
                    super::Opcode::VEXTRACTF32X4,
                    EVEXOperandCode::Em_xmm_G_LL_imm8,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x1a,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VINSERTF32X8,
                    EVEXOperandCode::Gm_V_zmm_E_ymm_imm8,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x1b,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VEXTRACTF32X8,
                    EVEXOperandCode::Em_ymm_G_zmm_imm8,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x1d,
            [
                (
                    super::Opcode::VCVTPS2PH,
                    EVEXOperandCode::Eqm_G_xmm_imm8_sae_W0,
                ),
                (
                    super::Opcode::VCVTPS2PH,
                    EVEXOperandCode::Em_xmm_G_ymm_imm8_sae_W0,
                ),
                (
                    super::Opcode::VCVTPS2PH,
                    EVEXOperandCode::Em_ymm_G_zmm_imm8_sae_W0,
                ),
                (
                    super::Opcode::VCVTPS2PH,
                    EVEXOperandCode::Em_ymm_G_zmm_imm8_sae_W0,
                ),
            ],
        ),
        (
            0x1e,
            [
                (
                    super::Opcode::VPCMPUD,
                    EVEXOperandCode::Mask_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPCMPUD,
                    EVEXOperandCode::Mask_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPCMPUD,
                    EVEXOperandCode::Mask_V_E_LL_imm8_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x1f,
            [
                (
                    super::Opcode::VPCMPD,
                    EVEXOperandCode::Mask_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPCMPD,
                    EVEXOperandCode::Mask_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPCMPD,
                    EVEXOperandCode::Mask_V_E_LL_imm8_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x20,
            [
                (super::Opcode::VPINSRB, EVEXOperandCode::G_V_xmm_Ebd_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x21,
            [
                (
                    super::Opcode::VINSERTPS,
                    EVEXOperandCode::G_V_Ed_xmm_imm8_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x22,
            [
                (super::Opcode::VPINSRD, EVEXOperandCode::G_V_xmm_Edq_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x23,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VSHUFF32X4,
                    EVEXOperandCode::Gm_V_Ed_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VSHUFF32X4,
                    EVEXOperandCode::Gm_V_Ed_LL_imm8_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x25,
            [
                (
                    super::Opcode::VPTERNLOGD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPTERNLOGD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPTERNLOGD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x26,
            [
                (
                    super::Opcode::VGETMANTPS,
                    EVEXOperandCode::Gm_Ed_LL_imm8_sae_noround_bcast,
                ),
                (
                    super::Opcode::VGETMANTPS,
                    EVEXOperandCode::Gm_Ed_LL_imm8_sae_noround_bcast,
                ),
                (
                    super::Opcode::VGETMANTPS,
                    EVEXOperandCode::Gm_Ed_LL_imm8_sae_noround_bcast,
                ),
                (
                    super::Opcode::VGETMANTPS,
                    EVEXOperandCode::Gm_Ed_LL_imm8_sae_noround_bcast,
                ),
            ],
        ),
        (
            0x27,
            [
                (
                    super::Opcode::VGETMANTSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VGETMANTSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VGETMANTSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VGETMANTSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
            ],
        ),
        (
            0x38,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VINSERTI32X4,
                    EVEXOperandCode::Gm_V_ymm_E_xmm_imm8,
                ),
                (
                    super::Opcode::VINSERTI32X4,
                    EVEXOperandCode::Gm_V_zmm_E_xmm_imm8,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x39,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VEXTRACTI32X4,
                    EVEXOperandCode::Em_xmm_G_LL_imm8,
                ),
                (
                    super::Opcode::VEXTRACTI32X4,
                    EVEXOperandCode::Em_xmm_G_LL_imm8,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3a,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VINSERTI32X8,
                    EVEXOperandCode::Gm_V_zmm_E_ymm_imm8,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3b,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VEXTRACTI32X8,
                    EVEXOperandCode::Em_ymm_G_zmm_imm8,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3e,
            [
                (super::Opcode::VPCMPUB, EVEXOperandCode::Mask_V_E_LL_imm8),
                (super::Opcode::VPCMPUB, EVEXOperandCode::Mask_V_E_LL_imm8),
                (super::Opcode::VPCMPUB, EVEXOperandCode::Mask_V_E_LL_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3f,
            [
                (super::Opcode::VPCMPB, EVEXOperandCode::Mask_V_E_LL_imm8),
                (super::Opcode::VPCMPB, EVEXOperandCode::Mask_V_E_LL_imm8),
                (super::Opcode::VPCMPB, EVEXOperandCode::Mask_V_E_LL_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x42,
            [
                (super::Opcode::VDBPSADBW, EVEXOperandCode::Gm_V_E_LL_imm8_W0),
                (super::Opcode::VDBPSADBW, EVEXOperandCode::Gm_V_E_LL_imm8_W0),
                (super::Opcode::VDBPSADBW, EVEXOperandCode::Gm_V_E_LL_imm8_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x43,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VSHUFI32X4,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VSHUFI32X4,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x44,
            [
                (super::Opcode::VPCLMULQDQ, EVEXOperandCode::G_V_E_LL_imm8),
                (super::Opcode::VPCLMULQDQ, EVEXOperandCode::G_V_E_LL_imm8),
                (super::Opcode::VPCLMULQDQ, EVEXOperandCode::G_V_E_LL_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x50,
            [
                (
                    super::Opcode::VRANGEPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_sae_bcast,
                ),
                (
                    super::Opcode::VRANGEPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_sae_bcast,
                ),
                (
                    super::Opcode::VRANGEPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_sae_bcast,
                ),
                (
                    super::Opcode::VRANGEPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_sae_bcast,
                ),
            ],
        ),
        (
            0x51,
            [
                (
                    super::Opcode::VRANGESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VRANGESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VRANGESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VRANGESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
            ],
        ),
        (
            0x54,
            [
                (
                    super::Opcode::VFIXUPIMMPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_sae_bcast,
                ),
                (
                    super::Opcode::VFIXUPIMMPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_sae_bcast,
                ),
                (
                    super::Opcode::VFIXUPIMMPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_sae_bcast,
                ),
                (
                    super::Opcode::VFIXUPIMMPS,
                    EVEXOperandCode::Gm_V_E_LL_imm8_sae_bcast,
                ),
            ],
        ),
        (
            0x55,
            [
                (
                    super::Opcode::VFIXUPIMMSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VFIXUPIMMSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VFIXUPIMMSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VFIXUPIMMSS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
            ],
        ),
        (
            0x56,
            [
                (super::Opcode::VREDUCEPS, EVEXOperandCode::Gm_E_LL_imm8_sae),
                (super::Opcode::VREDUCEPS, EVEXOperandCode::Gm_E_LL_imm8_sae),
                (super::Opcode::VREDUCEPS, EVEXOperandCode::Gm_E_LL_imm8_sae),
                (super::Opcode::VREDUCEPS, EVEXOperandCode::Gm_E_LL_imm8_sae),
            ],
        ),
        (
            0x57,
            [
                (
                    super::Opcode::VREDUCESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VREDUCESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VREDUCESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
                (
                    super::Opcode::VREDUCESS,
                    EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae,
                ),
            ],
        ),
        (
            0x66,
            [
                (
                    super::Opcode::VFPCLASSPS,
                    EVEXOperandCode::Mask_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VFPCLASSPS,
                    EVEXOperandCode::Mask_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VFPCLASSPS,
                    EVEXOperandCode::Mask_E_LL_imm8_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x67,
            [
                (super::Opcode::VFPCLASSSS, EVEXOperandCode::Mask_Ed_xmm_imm8),
                (super::Opcode::VFPCLASSSS, EVEXOperandCode::Mask_Ed_xmm_imm8),
                (super::Opcode::VFPCLASSSS, EVEXOperandCode::Mask_Ed_xmm_imm8),
                (super::Opcode::VFPCLASSSS, EVEXOperandCode::Mask_Ed_xmm_imm8),
            ],
        ),
        (
            0x70,
            [
                (super::Opcode::VPSHLDW, EVEXOperandCode::Gm_V_E_LL_imm8_W1),
                (super::Opcode::VPSHLDW, EVEXOperandCode::Gm_V_E_LL_imm8_W1),
                (super::Opcode::VPSHLDW, EVEXOperandCode::Gm_V_E_LL_imm8_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x71,
            [
                (
                    super::Opcode::VPSHLDD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPSHLDD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPSHLDD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x72,
            [
                (super::Opcode::VPSHRDW, EVEXOperandCode::Gm_V_E_LL_imm8_W1),
                (super::Opcode::VPSHRDW, EVEXOperandCode::Gm_V_E_LL_imm8_W1),
                (super::Opcode::VPSHRDW, EVEXOperandCode::Gm_V_E_LL_imm8_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x73,
            [
                (
                    super::Opcode::VPSHRDD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPSHRDD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (
                    super::Opcode::VPSHRDD,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xce,
            [
                (
                    super::Opcode::VGF2P8AFFINEQB,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W1,
                ),
                (
                    super::Opcode::VGF2P8AFFINEQB,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W1,
                ),
                (
                    super::Opcode::VGF2P8AFFINEQB,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xcf,
            [
                (
                    super::Opcode::VGF2P8AFFINEINVQB,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W1,
                ),
                (
                    super::Opcode::VGF2P8AFFINEINVQB,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W1,
                ),
                (
                    super::Opcode::VGF2P8AFFINEINVQB,
                    EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
    ];

    const EVEX_F2_0F: [(u8, [(super::Opcode, EVEXOperandCode); 4]); 26] = [
        (
            0x10,
            [
                (super::Opcode::VMOVSS, EVEXOperandCode::VMOVSS_10),
                (super::Opcode::VMOVSS, EVEXOperandCode::VMOVSS_10),
                (super::Opcode::VMOVSS, EVEXOperandCode::VMOVSS_10),
                (super::Opcode::VMOVSS, EVEXOperandCode::VMOVSS_10),
            ],
        ), // W0
        (
            0x11,
            [
                (super::Opcode::VMOVSS, EVEXOperandCode::VMOVSS_11),
                (super::Opcode::VMOVSS, EVEXOperandCode::VMOVSS_11),
                (super::Opcode::VMOVSS, EVEXOperandCode::VMOVSS_11),
                (super::Opcode::VMOVSS, EVEXOperandCode::VMOVSS_11),
            ],
        ), // W0
        (
            0x12,
            [
                (super::Opcode::VMOVSLDUP, EVEXOperandCode::Gm_E_LL_W0),
                (super::Opcode::VMOVSLDUP, EVEXOperandCode::Gm_E_LL_W0),
                (super::Opcode::VMOVSLDUP, EVEXOperandCode::Gm_E_LL_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x16,
            [
                (super::Opcode::VMOVSHDUP, EVEXOperandCode::Gm_E_LL_W0),
                (super::Opcode::VMOVSHDUP, EVEXOperandCode::Gm_E_LL_W0),
                (super::Opcode::VMOVSHDUP, EVEXOperandCode::Gm_E_LL_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x2a,
            [
                (super::Opcode::VCVTSI2SS, EVEXOperandCode::VCVTSI2SS),
                (super::Opcode::VCVTSI2SS, EVEXOperandCode::VCVTSI2SS),
                (super::Opcode::VCVTSI2SS, EVEXOperandCode::VCVTSI2SS),
                (super::Opcode::VCVTSI2SS, EVEXOperandCode::VCVTSI2SS),
            ],
        ),
        (
            0x2c,
            [
                (super::Opcode::VCVTTSS2SI, EVEXOperandCode::VCVTTSS2SI),
                (super::Opcode::VCVTTSS2SI, EVEXOperandCode::VCVTTSS2SI),
                (super::Opcode::VCVTTSS2SI, EVEXOperandCode::VCVTTSS2SI),
                (super::Opcode::VCVTTSS2SI, EVEXOperandCode::VCVTTSS2SI),
            ],
        ),
        (
            0x2d,
            [
                (super::Opcode::VCVTSS2SI, EVEXOperandCode::VCVTSS2SI),
                (super::Opcode::VCVTSS2SI, EVEXOperandCode::VCVTSS2SI),
                (super::Opcode::VCVTSS2SI, EVEXOperandCode::VCVTSS2SI),
                (super::Opcode::VCVTSS2SI, EVEXOperandCode::VCVTSS2SI),
            ],
        ),
        (
            0x51,
            [
                (super::Opcode::VSQRTSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VSQRTSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VSQRTSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VSQRTSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
            ],
        ),
        (
            0x58,
            [
                (super::Opcode::VADDSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VADDSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VADDSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VADDSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
            ],
        ),
        (
            0x59,
            [
                (super::Opcode::VMULSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VMULSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VMULSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VMULSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
            ],
        ),
        (
            0x5a,
            [
                (
                    super::Opcode::VCVTSS2SD,
                    EVEXOperandCode::Gm_V_Ed_xmm_sae_noround_W0,
                ),
                (
                    super::Opcode::VCVTSS2SD,
                    EVEXOperandCode::Gm_V_Ed_xmm_sae_noround_W0,
                ),
                (
                    super::Opcode::VCVTSS2SD,
                    EVEXOperandCode::Gm_V_Ed_xmm_sae_noround_W0,
                ),
                (
                    super::Opcode::VCVTSS2SD,
                    EVEXOperandCode::Gm_V_Ed_xmm_sae_noround_W0,
                ),
            ],
        ), // W0
        (
            0x5b,
            [
                (
                    super::Opcode::VCVTTPS2DQ,
                    EVEXOperandCode::Gm_Ed_LL_sae_noround_bcast_W0,
                ),
                (
                    super::Opcode::VCVTTPS2DQ,
                    EVEXOperandCode::Gm_Ed_LL_sae_noround_bcast_W0,
                ),
                (
                    super::Opcode::VCVTTPS2DQ,
                    EVEXOperandCode::Gm_Ed_LL_sae_noround_bcast_W0,
                ),
                (
                    super::Opcode::VCVTTPS2DQ,
                    EVEXOperandCode::Gm_Ed_LL_sae_noround_bcast_W0,
                ),
            ],
        ),
        (
            0x5c,
            [
                (super::Opcode::VSUBSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VSUBSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VSUBSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VSUBSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
            ],
        ),
        (
            0x5d,
            [
                (super::Opcode::VMINSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VMINSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VMINSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VMINSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
            ],
        ), // W0
        (
            0x5e,
            [
                (super::Opcode::VDIVSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VDIVSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VDIVSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VDIVSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
            ],
        ),
        (
            0x5f,
            [
                (super::Opcode::VMAXSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VMAXSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VMAXSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
                (super::Opcode::VMAXSS, EVEXOperandCode::Gm_V_Ed_xmm_sae_W0),
            ],
        ), // W0
        (
            0x6f,
            [
                (super::Opcode::VMOVDQU32, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VMOVDQU32, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VMOVDQU32, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x70,
            [
                (super::Opcode::VPSHUFHW, EVEXOperandCode::Gm_E_LL_imm8),
                (super::Opcode::VPSHUFHW, EVEXOperandCode::Gm_E_LL_imm8),
                (super::Opcode::VPSHUFHW, EVEXOperandCode::Gm_E_LL_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x78,
            [
                (super::Opcode::VCVTTSS2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTTSS2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTTSS2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTTSS2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
            ],
        ),
        (
            0x79,
            [
                (super::Opcode::VCVTSS2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTSS2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTSS2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTSS2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
            ],
        ),
        (
            0x7a,
            [
                (super::Opcode::VCVTUDQ2PD, EVEXOperandCode::VCVTUDQ2PD),
                (super::Opcode::VCVTUDQ2PD, EVEXOperandCode::VCVTUDQ2PD),
                (super::Opcode::VCVTUDQ2PD, EVEXOperandCode::VCVTUDQ2PD),
                (super::Opcode::VCVTUDQ2PD, EVEXOperandCode::VCVTUDQ2PD),
            ],
        ),
        (
            0x7b,
            [
                (super::Opcode::VCVTUSI2SS, EVEXOperandCode::G_V_xmm_Edq_sae),
                (super::Opcode::VCVTUSI2SS, EVEXOperandCode::G_V_xmm_Edq_sae),
                (super::Opcode::VCVTUSI2SS, EVEXOperandCode::G_V_xmm_Edq_sae),
                (super::Opcode::VCVTUSI2SS, EVEXOperandCode::G_V_xmm_Edq_sae),
            ],
        ),
        (
            0x7e,
            [
                (super::Opcode::VMOVQ, EVEXOperandCode::VMOVQ_7e),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x7f,
            [
                (super::Opcode::VMOVDQU32, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VMOVDQU32, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VMOVDQU32, EVEXOperandCode::Em_G_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xc2,
            [
                (
                    super::Opcode::VCMPSS,
                    EVEXOperandCode::Maskm_V_Ed_xmm_imm8_sae_W0,
                ),
                (
                    super::Opcode::VCMPSS,
                    EVEXOperandCode::Maskm_V_Ed_xmm_imm8_sae_W0,
                ),
                (
                    super::Opcode::VCMPSS,
                    EVEXOperandCode::Maskm_V_Ed_xmm_imm8_sae_W0,
                ),
                (
                    super::Opcode::VCMPSS,
                    EVEXOperandCode::Maskm_V_Ed_xmm_imm8_sae_W0,
                ),
            ],
        ),
        (
            0xe6,
            [
                (super::Opcode::VCVTDQ2PD, EVEXOperandCode::VCVTUDQ2PD),
                (super::Opcode::VCVTDQ2PD, EVEXOperandCode::VCVTUDQ2PD),
                (super::Opcode::VCVTDQ2PD, EVEXOperandCode::VCVTUDQ2PD),
                (super::Opcode::VCVTDQ2PD, EVEXOperandCode::VCVTUDQ2PD),
            ],
        ),
    ];

    const EVEX_F2_0F38: [(u8, [(super::Opcode, EVEXOperandCode); 4]); 28] = [
        (
            0x10,
            [
                (super::Opcode::VPMOVUSWB, EVEXOperandCode::Eqm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVUSWB, EVEXOperandCode::Em_xmm_G_ymm_W0),
                (super::Opcode::VPMOVUSWB, EVEXOperandCode::Em_ymm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x11,
            [
                (super::Opcode::VPMOVUSDB, EVEXOperandCode::Edm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVUSDB, EVEXOperandCode::Eqm_xmm_G_ymm_W0),
                (super::Opcode::VPMOVUSDB, EVEXOperandCode::Em_xmm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x12,
            [
                (super::Opcode::VPMOVUSQB, EVEXOperandCode::Ewm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVUSQB, EVEXOperandCode::Edm_xmm_G_ymm_W0),
                (super::Opcode::VPMOVUSQB, EVEXOperandCode::Eqm_xmm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x13,
            [
                (super::Opcode::VPMOVUSDW, EVEXOperandCode::Eqm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVUSDW, EVEXOperandCode::Em_xmm_G_ymm_W0),
                (super::Opcode::VPMOVUSDW, EVEXOperandCode::Em_ymm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x14,
            [
                (super::Opcode::VPMOVUSQW, EVEXOperandCode::Edm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVUSQW, EVEXOperandCode::Eqm_xmm_G_ymm_W0),
                (super::Opcode::VPMOVUSQW, EVEXOperandCode::Em_xmm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x15,
            [
                (super::Opcode::VPMOVUSQD, EVEXOperandCode::Eqm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVUSQD, EVEXOperandCode::Em_xmm_G_ymm_W0),
                (super::Opcode::VPMOVUSQD, EVEXOperandCode::Em_ymm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x20,
            [
                (super::Opcode::VPMOVSWB, EVEXOperandCode::Eqm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVSWB, EVEXOperandCode::Em_xmm_G_ymm_W0),
                (super::Opcode::VPMOVSWB, EVEXOperandCode::Em_ymm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x21,
            [
                (super::Opcode::VPMOVSDB, EVEXOperandCode::Edm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVSDB, EVEXOperandCode::Eqm_xmm_G_ymm_W0),
                (super::Opcode::VPMOVSDB, EVEXOperandCode::Em_xmm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x22,
            [
                (super::Opcode::VPMOVSQB, EVEXOperandCode::Ewm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVSQB, EVEXOperandCode::Edm_xmm_G_ymm_W0),
                (super::Opcode::VPMOVSQB, EVEXOperandCode::Eqm_xmm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x23,
            [
                (super::Opcode::VPMOVSDW, EVEXOperandCode::Eqm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVSDW, EVEXOperandCode::Em_xmm_G_ymm_W0),
                (super::Opcode::VPMOVSDW, EVEXOperandCode::Em_ymm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x24,
            [
                (super::Opcode::VPMOVSQW, EVEXOperandCode::Edm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVSQW, EVEXOperandCode::Eqm_xmm_G_ymm_W0),
                (super::Opcode::VPMOVSQW, EVEXOperandCode::Em_xmm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x25,
            [
                (super::Opcode::VPMOVSQD, EVEXOperandCode::Eqm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVSQD, EVEXOperandCode::Em_xmm_G_ymm_W0),
                (super::Opcode::VPMOVSQD, EVEXOperandCode::Em_ymm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x26,
            [
                (super::Opcode::VPTESTNMB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPTESTNMB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::VPTESTNMB, EVEXOperandCode::Mask_V_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x27,
            [
                (super::Opcode::VPTESTNMD, EVEXOperandCode::Mask_V_E_LL_bcast),
                (super::Opcode::VPTESTNMD, EVEXOperandCode::Mask_V_E_LL_bcast),
                (super::Opcode::VPTESTNMD, EVEXOperandCode::Mask_V_E_LL_bcast),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x28,
            [
                (super::Opcode::VPMOVM2B, EVEXOperandCode::G_LL_Mask),
                (super::Opcode::VPMOVM2B, EVEXOperandCode::G_LL_Mask),
                (super::Opcode::VPMOVM2B, EVEXOperandCode::G_LL_Mask),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x29,
            [
                (super::Opcode::VPMOVB2M, EVEXOperandCode::Mask_U_LL),
                (super::Opcode::VPMOVB2M, EVEXOperandCode::Mask_U_LL),
                (super::Opcode::VPMOVB2M, EVEXOperandCode::Mask_U_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x2a,
            [
                (
                    super::Opcode::VPBROADCASTMB2Q,
                    EVEXOperandCode::G_LL_Mask_W1,
                ),
                (
                    super::Opcode::VPBROADCASTMB2Q,
                    EVEXOperandCode::G_LL_Mask_W1,
                ),
                (
                    super::Opcode::VPBROADCASTMB2Q,
                    EVEXOperandCode::G_LL_Mask_W1,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x30,
            [
                (super::Opcode::VPMOVWB, EVEXOperandCode::Eqm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVWB, EVEXOperandCode::Em_xmm_G_ymm_W0),
                (super::Opcode::VPMOVWB, EVEXOperandCode::Em_ymm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x31,
            [
                (super::Opcode::VPMOVDB, EVEXOperandCode::Edm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVDB, EVEXOperandCode::Eqm_xmm_G_ymm_W0),
                (super::Opcode::VPMOVDB, EVEXOperandCode::Em_xmm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x32,
            [
                (super::Opcode::VPMOVQB, EVEXOperandCode::Ewm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVQB, EVEXOperandCode::Edm_xmm_G_ymm_W0),
                (super::Opcode::VPMOVQB, EVEXOperandCode::Eqm_xmm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x33,
            [
                (super::Opcode::VPMOVDW, EVEXOperandCode::Eqm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVDW, EVEXOperandCode::Em_xmm_G_ymm_W0),
                (super::Opcode::VPMOVDW, EVEXOperandCode::Em_ymm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x34,
            [
                (super::Opcode::VPMOVQW, EVEXOperandCode::Edm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVQW, EVEXOperandCode::Eqm_xmm_G_ymm_W0),
                (super::Opcode::VPMOVQW, EVEXOperandCode::Em_xmm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x35,
            [
                (super::Opcode::VPMOVQD, EVEXOperandCode::Eqm_xmm_G_xmm_W0),
                (super::Opcode::VPMOVQD, EVEXOperandCode::Em_xmm_G_ymm_W0),
                (super::Opcode::VPMOVQD, EVEXOperandCode::Em_ymm_G_zmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x38,
            [
                (super::Opcode::VPMOVM2D, EVEXOperandCode::G_LL_Mask),
                (super::Opcode::VPMOVM2D, EVEXOperandCode::G_LL_Mask),
                (super::Opcode::VPMOVM2D, EVEXOperandCode::G_LL_Mask),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x39,
            [
                (super::Opcode::VPMOVD2M, EVEXOperandCode::Mask_U_LL),
                (super::Opcode::VPMOVD2M, EVEXOperandCode::Mask_U_LL),
                (super::Opcode::VPMOVD2M, EVEXOperandCode::Mask_U_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x3a,
            [
                (
                    super::Opcode::VPBROADCASTMW2D,
                    EVEXOperandCode::G_LL_Mask_W0,
                ),
                (
                    super::Opcode::VPBROADCASTMW2D,
                    EVEXOperandCode::G_LL_Mask_W0,
                ),
                (
                    super::Opcode::VPBROADCASTMW2D,
                    EVEXOperandCode::G_LL_Mask_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x52,
            [
                (
                    super::Opcode::VDPBF16PS,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VDPBF16PS,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VDPBF16PS,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x72,
            [
                (
                    super::Opcode::VCVTNEPS2BF16,
                    EVEXOperandCode::Operands_72_W0,
                ),
                (
                    super::Opcode::VCVTNEPS2BF16,
                    EVEXOperandCode::Operands_72_W0,
                ),
                (
                    super::Opcode::VCVTNEPS2BF16,
                    EVEXOperandCode::Operands_72_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
    ];

    const EVEX_F3_0F: [(u8, [(super::Opcode, EVEXOperandCode); 4]); 24] = [
        (
            0x10,
            [
                (super::Opcode::VMOVSD, EVEXOperandCode::VMOVSD_10),
                (super::Opcode::VMOVSD, EVEXOperandCode::VMOVSD_10),
                (super::Opcode::VMOVSD, EVEXOperandCode::VMOVSD_10),
                (super::Opcode::VMOVSD, EVEXOperandCode::VMOVSD_10),
            ],
        ), // W1
        (
            0x11,
            [
                (super::Opcode::VMOVSD, EVEXOperandCode::VMOVSD_11),
                (super::Opcode::VMOVSD, EVEXOperandCode::VMOVSD_11),
                (super::Opcode::VMOVSD, EVEXOperandCode::VMOVSD_11),
                (super::Opcode::VMOVSD, EVEXOperandCode::VMOVSD_11),
            ],
        ), // W1
        (
            0x12,
            [
                (super::Opcode::VMOVDDUP, EVEXOperandCode::Gm_E_LL_W1),
                (super::Opcode::VMOVDDUP, EVEXOperandCode::Gm_E_LL_W1),
                (super::Opcode::VMOVDDUP, EVEXOperandCode::Gm_E_LL_W1),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x2a,
            [
                (super::Opcode::VCVTSI2SD, EVEXOperandCode::VCVTSI2SS),
                (super::Opcode::VCVTSI2SD, EVEXOperandCode::VCVTSI2SS),
                (super::Opcode::VCVTSI2SD, EVEXOperandCode::VCVTSI2SS),
                (super::Opcode::VCVTSI2SD, EVEXOperandCode::VCVTSI2SS),
            ],
        ),
        (
            0x2c,
            [
                (super::Opcode::VCVTTSD2SI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTTSD2SI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTTSD2SI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTTSD2SI, EVEXOperandCode::Gd_Ed_xmm_sae),
            ],
        ),
        (
            0x2d,
            [
                (super::Opcode::VCVTSD2SI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTSD2SI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTSD2SI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTSD2SI, EVEXOperandCode::Gd_Ed_xmm_sae),
            ],
        ),
        (
            0x51,
            [
                (super::Opcode::VSQRTSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VSQRTSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VSQRTSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VSQRTSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
            ],
        ),
        (
            0x58,
            [
                (super::Opcode::VADDSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VADDSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VADDSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VADDSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
            ],
        ),
        (
            0x59,
            [
                (super::Opcode::VMULSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VMULSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VMULSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VMULSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
            ],
        ),
        (
            0x5a,
            [
                (
                    super::Opcode::VCVTSD2SS,
                    EVEXOperandCode::Gm_V_Eq_xmm_sae_W1,
                ),
                (
                    super::Opcode::VCVTSD2SS,
                    EVEXOperandCode::Gm_V_Eq_xmm_sae_W1,
                ),
                (
                    super::Opcode::VCVTSD2SS,
                    EVEXOperandCode::Gm_V_Eq_xmm_sae_W1,
                ),
                (
                    super::Opcode::VCVTSD2SS,
                    EVEXOperandCode::Gm_V_Eq_xmm_sae_W1,
                ),
            ],
        ),
        (
            0x5c,
            [
                (super::Opcode::VSUBSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VSUBSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VSUBSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VSUBSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
            ],
        ),
        (
            0x5d,
            [
                (super::Opcode::VMINSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VMINSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VMINSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VMINSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
            ],
        ), // W1
        (
            0x5e,
            [
                (super::Opcode::VDIVSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VDIVSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VDIVSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
                (super::Opcode::VDIVSD, EVEXOperandCode::Gm_V_E_xmm_sae_W1),
            ],
        ),
        (
            0x5f,
            [
                (super::Opcode::VMAXSD, EVEXOperandCode::Gm_V_E_xmm_sae),
                (super::Opcode::VMAXSD, EVEXOperandCode::Gm_V_E_xmm_sae),
                (super::Opcode::VMAXSD, EVEXOperandCode::Gm_V_E_xmm_sae),
                (super::Opcode::VMAXSD, EVEXOperandCode::Gm_V_E_xmm_sae),
            ],
        ), // W1
        (
            0x6f,
            [
                (super::Opcode::VMOVDQU8, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VMOVDQU8, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::VMOVDQU8, EVEXOperandCode::Gm_E_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x70,
            [
                (super::Opcode::VPSHUFLW, EVEXOperandCode::Gm_E_LL_imm8),
                (super::Opcode::VPSHUFLW, EVEXOperandCode::Gm_E_LL_imm8),
                (super::Opcode::VPSHUFLW, EVEXOperandCode::Gm_E_LL_imm8),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x78,
            [
                (super::Opcode::VCVTTSD2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTTSD2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTTSD2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTTSD2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
            ],
        ),
        (
            0x79,
            [
                (super::Opcode::VCVTSD2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTSD2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTSD2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
                (super::Opcode::VCVTSD2USI, EVEXOperandCode::Gd_Ed_xmm_sae),
            ],
        ),
        (
            0x7a,
            [
                (super::Opcode::VCVTUDQ2PS, EVEXOperandCode::VCVTDQ2PS),
                (super::Opcode::VCVTUDQ2PS, EVEXOperandCode::VCVTDQ2PS),
                (super::Opcode::VCVTUDQ2PS, EVEXOperandCode::VCVTDQ2PS),
                (super::Opcode::VCVTUDQ2PS, EVEXOperandCode::VCVTDQ2PS),
            ],
        ),
        (
            0x7b,
            [
                (super::Opcode::VCVTUSI2SD, EVEXOperandCode::VCVTUSI2SD),
                (super::Opcode::VCVTUSI2SD, EVEXOperandCode::VCVTUSI2SD),
                (super::Opcode::VCVTUSI2SD, EVEXOperandCode::VCVTUSI2SD),
                (super::Opcode::VCVTUSI2SD, EVEXOperandCode::VCVTUSI2SD),
            ],
        ),
        (
            0x7e,
            [
                (super::Opcode::VMOVQ, EVEXOperandCode::VMOVQ_G_Ed_xmm),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x7f,
            [
                (super::Opcode::VMOVDQU8, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VMOVDQU8, EVEXOperandCode::Em_G_LL),
                (super::Opcode::VMOVDQU8, EVEXOperandCode::Em_G_LL),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xc2,
            [
                (
                    super::Opcode::VCMPSD,
                    EVEXOperandCode::Maskm_V_Eq_xmm_imm8_sae_W1,
                ),
                (
                    super::Opcode::VCMPSD,
                    EVEXOperandCode::Maskm_V_Eq_xmm_imm8_sae_W1,
                ),
                (
                    super::Opcode::VCMPSD,
                    EVEXOperandCode::Maskm_V_Eq_xmm_imm8_sae_W1,
                ),
                (
                    super::Opcode::VCMPSD,
                    EVEXOperandCode::Maskm_V_Eq_xmm_imm8_sae_W1,
                ),
            ],
        ),
        (
            0xe6,
            [
                (super::Opcode::VCVTPD2DQ, EVEXOperandCode::VCVTTPD2DQ),
                (super::Opcode::VCVTPD2DQ, EVEXOperandCode::VCVTTPD2DQ),
                (super::Opcode::VCVTPD2DQ, EVEXOperandCode::VCVTTPD2DQ),
                (super::Opcode::VCVTPD2DQ, EVEXOperandCode::VCVTTPD2DQ),
            ],
        ),
    ];

    const EVEX_F3_0F38: [(u8, [(super::Opcode, EVEXOperandCode); 4]); 8] = [
        (
            0x52,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::VP4DPWSSD, EVEXOperandCode::Gm_V_zmm_M_xmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x53,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::VP4DPWSSDS,
                    EVEXOperandCode::Gm_V_zmm_M_xmm_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x68,
            [
                (
                    super::Opcode::VP2INTERSECTD,
                    EVEXOperandCode::Mask_V_E_LL_bcast,
                ),
                (
                    super::Opcode::VP2INTERSECTD,
                    EVEXOperandCode::Mask_V_E_LL_bcast,
                ),
                (
                    super::Opcode::VP2INTERSECTD,
                    EVEXOperandCode::Mask_V_E_LL_bcast,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x72,
            [
                (
                    super::Opcode::VCVTNE2PS2BF16,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VCVTNE2PS2BF16,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (
                    super::Opcode::VCVTNE2PS2BF16,
                    EVEXOperandCode::Gm_V_E_LL_bcast_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x9a,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::V4FMADDPS, EVEXOperandCode::Gm_V_zmm_M_xmm_W0),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0x9b,
            [
                (super::Opcode::V4FMADDSS, EVEXOperandCode::Gm_V_M_xmm),
                (super::Opcode::V4FMADDSS, EVEXOperandCode::Gm_V_M_xmm),
                (super::Opcode::V4FMADDSS, EVEXOperandCode::Gm_V_M_xmm),
                (super::Opcode::V4FMADDSS, EVEXOperandCode::Gm_V_M_xmm),
            ],
        ), // W0
        (
            0xaa,
            [
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
                (
                    super::Opcode::V4FNMADDPS,
                    EVEXOperandCode::Gm_V_zmm_M_xmm_W0,
                ),
                (super::Opcode::Invalid, EVEXOperandCode::Nothing),
            ],
        ),
        (
            0xab,
            [
                (super::Opcode::V4FNMADDSS, EVEXOperandCode::Gm_V_M_xmm),
                (super::Opcode::V4FNMADDSS, EVEXOperandCode::Gm_V_M_xmm),
                (super::Opcode::V4FNMADDSS, EVEXOperandCode::Gm_V_M_xmm),
                (super::Opcode::V4FNMADDSS, EVEXOperandCode::Gm_V_M_xmm),
            ],
        ), // W0
    ];
}

// `evex_byte_one` is an option because the caller *may* have already read it,
// but may have not. `long_mode` can decide immediately that `0x62` should be read
// as an `EVEX` instruction, but for other modes we can only make this
// determination when reading a `bound`'s `modrm` byte.
#[inline(never)]
pub(crate) fn read_evex(
    words: &mut Reader,
    instruction: &mut Instruction,
    evex_byte_one: Option<u8>,
) -> Result<(), ErrorKind> {
    let evex_byte_one = if let Some(b) = evex_byte_one {
        b
    } else {
        words.next().ok_or(ErrorKind::ExhaustedInput)?
    };
    let evex_byte_two = words.next().ok_or(ErrorKind::ExhaustedInput)?;
    let evex_byte_three = words.next().ok_or(ErrorKind::ExhaustedInput)?;
    let p = evex_byte_two & 0x03;
    if evex_byte_one & 0x0c != 0 {
        // the two bits above `m` are reserved and must be 0
        return Err(ErrorKind::InvalidOpcode);
    }
    if evex_byte_two & 0x04 == 0 {
        // the one bit above `p` is reserved and must be 1
        return Err(ErrorKind::InvalidOpcode);
    }
    let m = evex_byte_one & 0x03;
    if m == 0 {
        return Err(ErrorKind::InvalidOpcode);
    }
    let m = m - 1;
    // instead of enums for the lookup bits, these are used to select a TABLES entry in the first
    // place
    /*
    let p = [
        EVEXOpcodePrefix::None,
        EVEXOpcodePrefix::Prefix66,
        EVEXOpcodePrefix::PrefixF3,
        EVEXOpcodePrefix::PrefixF2,
    ][p];
    let m = [
        Ok(EVEXOpcodeMap::Map0F),
        Ok(EVEXOpcodeMap::Map0F38),
        Ok(EVEXOpcodeMap::Map0F3A),
        Err(Error::InvalidOpcode),
    ][m - 1]?;
    */

    let vp = ((evex_byte_three >> 3) & 1) << 4;
    let vvvvv = ((evex_byte_two >> 3) & 0b1111) | vp;

    instruction.regs[3] = RegSpec {
        bank: RegisterBank::X,
        num: vvvvv ^ 0b11111, // `vvvvv` is provided in inverted form
    };

    instruction.prefixes.evex_from(evex_byte_one, evex_byte_two, evex_byte_three);

    let opc = words.next().ok_or(ErrorKind::ExhaustedInput)?;
    let table_idx = ((m << 2) | p) as usize;
    let table = generated::TABLES[table_idx];
    if std::ptr::eq(table, &generated::DUMMY[..]) {
        return Err(ErrorKind::InvalidOpcode);
    }
    let mut index_lower = 0;
    if instruction.prefixes.evex_unchecked().vex().l() {
        index_lower |= 1;
    }
    if instruction.prefixes.evex_unchecked().lp() {
        index_lower |= 2;
    }
    if let Ok(entry) = table.binary_search_by_key(&opc, |x| x.0) {
        let (opcode, operand_code) = table[entry].1[index_lower];
        instruction.opcode = opcode;
        read_evex_operands(words, instruction, operand_code)?;
        if instruction.prefixes.evex_unchecked().rp() {
            instruction.regs[0].num |= 0b10000;
            let banks = [RegisterBank::X, RegisterBank::Y, RegisterBank::Z];
            if !banks.contains(&instruction.regs[0].bank) {
                return Err(ErrorKind::InvalidOperand);
            }
        }
        if instruction.prefixes.evex_unchecked().vex().x()
            && instruction.operands.contains(&OperandSpec::RegMMM)
        {
            instruction.regs[1].num |= 0b10000;
            let banks = [RegisterBank::X, RegisterBank::Y, RegisterBank::Z];
            if !banks.contains(&instruction.regs[1].bank) {
                return Err(ErrorKind::InvalidOperand);
            }
        }
        if instruction.prefixes.evex_unchecked().vex().compressed_disp() {
            let overridden_size = match instruction.opcode {
                Opcode::VPEXPANDB => Some(1),
                Opcode::VPEXPANDW => Some(2),
                Opcode::VPEXPANDD => Some(4),
                Opcode::VPEXPANDQ => Some(8),
                Opcode::VPCOMPRESSB => Some(1),
                Opcode::VPCOMPRESSW => Some(2),
                Opcode::VPCOMPRESSD => Some(4),
                Opcode::VPCOMPRESSQ => Some(8),
                Opcode::VEXPANDPS => Some(4),
                Opcode::VEXPANDPD => Some(8),
                Opcode::VCOMPRESSPS => Some(4),
                Opcode::VCOMPRESSPD => Some(8),
                _ => None,
            };
            if let Some(size) = overridden_size {
                instruction.disp = instruction.disp.wrapping_mul(size);
            } else {
                instruction.disp = instruction.disp.wrapping_mul(instruction.mem_size.into());
            }
            instruction.prefixes.apply_compressed_disp(false);
        }
        if instruction.opcode == Opcode::Invalid {
            return Err(ErrorKind::InvalidOpcode);
        }
        // TODO: apply rp and bp?
    } else {
        return Err(ErrorKind::InvalidOpcode);
    }
    Ok(())
}

fn deny_broadcast(inst: &Instruction) -> Result<(), ErrorKind> {
    if inst.prefixes.evex_unchecked().broadcast() {
        Err(ErrorKind::InvalidOperand)
    } else {
        Ok(())
    }
}

fn deny_z(inst: &Instruction) -> Result<(), ErrorKind> {
    if inst.prefixes.evex_unchecked().merge() {
        Err(ErrorKind::InvalidOperand)
    } else {
        Ok(())
    }
}

fn deny_vex_reg(inst: &Instruction) -> Result<(), ErrorKind> {
    if inst.regs[3].num != 0 {
        Err(ErrorKind::InvalidOperand)
    } else {
        Ok(())
    }
}

#[allow(non_snake_case)]
fn ensure_W(inst: &Instruction, w: u8) -> Result<(), ErrorKind> {
    if inst.prefixes.evex_unchecked().vex().w() ^ (w != 0) {
        Err(ErrorKind::InvalidOpcode)
    } else {
        Ok(())
    }
}

fn deny_mask_reg(inst: &Instruction) -> Result<(), ErrorKind> {
    if inst.prefixes.evex_unchecked().mask_reg() != 0 {
        Err(ErrorKind::InvalidOperand)
    } else {
        Ok(())
    }
}

fn check_mask_reg(inst: &Instruction) -> Result<(), ErrorKind> {
    // if an operand is to be zeroed on mask bits but mask register 0 is
    // selected, this instruction is nonsense and will #UD
    if inst.prefixes.evex_unchecked().merge() && inst.prefixes.evex_unchecked().mask_reg() == 0 {
        Err(ErrorKind::InvalidOperand)
    } else {
        Ok(())
    }
}

fn apply_broadcast(inst: &mut Instruction, item_size: u8, reg_size: u8) {
    if inst.prefixes.evex_unchecked().broadcast() {
        inst.mem_size = item_size;
    } else {
        inst.mem_size = reg_size;
    }
}

fn set_rrr(inst: &mut Instruction, modrm: u8) {
    inst.regs[0].num = (modrm >> 3) & 7;
    if inst.prefixes.evex_unchecked().vex().r() {
        inst.regs[0].num |= 8;
    }
    if inst.prefixes.evex_unchecked().rp() {
        inst.regs[0].num |= 16;
    }
}

fn set_reg_sizes(inst: &mut Instruction, size: RegisterBank) {
    inst.regs[0].bank = size;
    inst.regs[3].bank = size;
    for i in 0..inst.operand_count {
        if [
            OperandSpec::RegMMM,
            OperandSpec::RegMMM_maskmerge,
            OperandSpec::RegMMM_maskmerge_sae_noround,
        ]
        .contains(&inst.operands[i as usize])
        {
            inst.regs[1].bank = size;
        }
    }
}

fn regs_size(inst: &Instruction) -> u8 {
    if inst.prefixes.evex_unchecked().lp() {
        64
    } else if inst.prefixes.evex_unchecked().vex().l() {
        32
    } else {
        16
    }
}

fn set_reg_sizes_from_ll(inst: &mut Instruction) -> Result<(), ErrorKind> {
    if inst.prefixes.evex_unchecked().lp() {
        if inst.prefixes.evex_unchecked().vex().l() {
            return Err(ErrorKind::InvalidOperand);
        }
        set_reg_sizes(inst, RegisterBank::Z);
    } else if inst.prefixes.evex_unchecked().vex().l() {
        set_reg_sizes(inst, RegisterBank::Y);
    } else {
        set_reg_sizes(inst, RegisterBank::X);
    }
    Ok(())
}

pub(crate) fn read_evex_operands(
    words: &mut Reader,
    instruction: &mut Instruction,
    operand_code: generated::EVEXOperandCode,
) -> Result<(), ErrorKind> {
    match operand_code {
        generated::EVEXOperandCode::Gm_V_E_LL_imm8_sae_bcast => {
            check_mask_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;

            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VRANGEPS {
                    instruction.opcode = Opcode::VRANGEPD;
                } else if instruction.opcode == Opcode::VFIXUPIMMPS {
                    instruction.opcode = Opcode::VFIXUPIMMPD;
                }
            }

            if let OperandSpec::RegMMM = mem_oper {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    if [
                        Opcode::VRANGEPS,
                        Opcode::VRANGEPD,
                        Opcode::VFIXUPIMMPS,
                        Opcode::VFIXUPIMMPD,
                    ]
                    .contains(&instruction.opcode)
                    {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    } else {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    }
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                let sz = regs_size(instruction);

                if instruction.prefixes.evex_unchecked().vex().w() {
                    apply_broadcast(instruction, 8, sz);
                } else {
                    apply_broadcast(instruction, 4, sz);
                }

                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Gm_V_Ed_xmm => {
            check_mask_reg(instruction)?;
            if instruction.prefixes.evex_unchecked().broadcast() {
                return Err(ErrorKind::InvalidOpcode);
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;

            instruction.operand_count = 3;

            if instruction.prefixes.evex_unchecked().vex().w()
                && instruction.opcode == Opcode::VRSQRT14SS
            {
                instruction.opcode = Opcode::VRSQRT14SD;
            }

            if let OperandSpec::RegMMM = mem_oper {
                instruction.mem_size = 0;
            } else if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.mem_size = 8;
            } else {
                instruction.mem_size = 4;
            }
            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::Gm_V_Eq_xmm_sae_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;

            instruction.operand_count = 3;

            if let OperandSpec::RegMMM = mem_oper {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                }
                instruction.mem_size = 0;
            } else {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    return Err(ErrorKind::InvalidOpcode);
                }
                instruction.mem_size = 8;
            }
            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::Gm_V_Ed_xmm_sae_bcast => {
            check_mask_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;

            instruction.operand_count = 3;

            if instruction.prefixes.evex_unchecked().vex().w()
                && instruction.opcode == Opcode::VGETEXPSS
            {
                instruction.opcode = Opcode::VGETEXPSD;
            }

            if let OperandSpec::RegMMM = mem_oper {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                }
            } else if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.mem_size = 8;
            } else {
                instruction.mem_size = 4;
            }
            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::Gm_V_Ed_LL_sae => {
            check_mask_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;

            instruction.operand_count = 3;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VFMADD132SS {
                    instruction.opcode = Opcode::VFMADD132SD;
                } else if instruction.opcode == Opcode::VFMSUB132SS {
                    instruction.opcode = Opcode::VFMSUB132SD;
                } else if instruction.opcode == Opcode::VFNMADD132SS {
                    instruction.opcode = Opcode::VFNMADD132SD;
                } else if instruction.opcode == Opcode::VFNMSUB132SS {
                    instruction.opcode = Opcode::VFNMSUB132SD;
                } else if instruction.opcode == Opcode::VFMADD213SS {
                    instruction.opcode = Opcode::VFMADD213SD;
                } else if instruction.opcode == Opcode::VFMSUB213SS {
                    instruction.opcode = Opcode::VFMSUB213SD;
                } else if instruction.opcode == Opcode::VFNMADD213SS {
                    instruction.opcode = Opcode::VFNMADD213SD;
                } else if instruction.opcode == Opcode::VFNMSUB213SS {
                    instruction.opcode = Opcode::VFNMSUB213SD;
                } else if instruction.opcode == Opcode::VFMADD231SS {
                    instruction.opcode = Opcode::VFMADD231SD;
                } else if instruction.opcode == Opcode::VFMSUB231SS {
                    instruction.opcode = Opcode::VFMSUB231SD;
                } else if instruction.opcode == Opcode::VFNMADD231SS {
                    instruction.opcode = Opcode::VFNMADD231SD;
                } else if instruction.opcode == Opcode::VFNMSUB231SS {
                    instruction.opcode = Opcode::VFNMSUB231SD;
                }
            }

            set_reg_sizes(instruction, RegisterBank::X);

            if let OperandSpec::RegMMM = mem_oper {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                }
            } else {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    return Err(ErrorKind::InvalidOpcode);
                }

                if instruction.prefixes.evex_unchecked().vex().w() {
                    instruction.mem_size = 8;
                } else {
                    instruction.mem_size = 4;
                }
            }
        }
        generated::EVEXOperandCode::Gm_V_E_LL_sae_bcast => {
            check_mask_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;

            instruction.operand_count = 3;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VSCALEFPS {
                    instruction.opcode = Opcode::VSCALEFPD;
                } else if instruction.opcode == Opcode::VFNMADD132PS {
                    instruction.opcode = Opcode::VFNMADD132PD;
                } else if instruction.opcode == Opcode::VFNMSUB132PS {
                    instruction.opcode = Opcode::VFNMSUB132PD;
                } else if instruction.opcode == Opcode::VFMADDSUB132PS {
                    instruction.opcode = Opcode::VFMADDSUB132PD;
                } else if instruction.opcode == Opcode::VFMSUBADD132PS {
                    instruction.opcode = Opcode::VFMSUBADD132PD;
                } else if instruction.opcode == Opcode::VFMADD132PS {
                    instruction.opcode = Opcode::VFMADD132PD;
                } else if instruction.opcode == Opcode::VFMSUB132PS {
                    instruction.opcode = Opcode::VFMSUB132PD;
                } else if instruction.opcode == Opcode::VFMADDSUB213PS {
                    instruction.opcode = Opcode::VFMADDSUB213PD;
                } else if instruction.opcode == Opcode::VFMSUBADD213PS {
                    instruction.opcode = Opcode::VFMSUBADD213PD;
                } else if instruction.opcode == Opcode::VFMADD213PS {
                    instruction.opcode = Opcode::VFMADD213PD;
                } else if instruction.opcode == Opcode::VFMSUB213PS {
                    instruction.opcode = Opcode::VFMSUB213PD;
                } else if instruction.opcode == Opcode::VFNMADD213PS {
                    instruction.opcode = Opcode::VFNMADD213PD;
                } else if instruction.opcode == Opcode::VFNMSUB213PS {
                    instruction.opcode = Opcode::VFNMSUB213PD;
                } else if instruction.opcode == Opcode::VFMADDSUB231PS {
                    instruction.opcode = Opcode::VFMADDSUB231PD;
                } else if instruction.opcode == Opcode::VFMSUBADD231PS {
                    instruction.opcode = Opcode::VFMSUBADD231PD;
                } else if instruction.opcode == Opcode::VFMADD231PS {
                    instruction.opcode = Opcode::VFMADD231PD;
                } else if instruction.opcode == Opcode::VFMSUB231PS {
                    instruction.opcode = Opcode::VFMSUB231PD;
                } else if instruction.opcode == Opcode::VFNMADD231PS {
                    instruction.opcode = Opcode::VFNMADD231PD;
                } else if instruction.opcode == Opcode::VFNMSUB231PS {
                    instruction.opcode = Opcode::VFNMSUB231PD;
                }
            }

            if let OperandSpec::RegMMM = mem_oper {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                let sz = regs_size(instruction);

                if instruction.prefixes.evex_unchecked().vex().w() {
                    apply_broadcast(instruction, 8, sz);
                } else {
                    apply_broadcast(instruction, 4, sz);
                }

                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Gm_E_LL_imm8_sae => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.opcode = Opcode::VREDUCEPD;
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;

            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            if let OperandSpec::RegMMM = mem_oper {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.mem_size = 0;
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;

                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                let sz = regs_size(instruction);

                if instruction.prefixes.evex_unchecked().vex().w() {
                    apply_broadcast(instruction, 8, sz);
                } else {
                    apply_broadcast(instruction, 4, sz);
                }

                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Gm_E_LL_imm8_sae_W0 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;

            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            if let OperandSpec::RegMMM = mem_oper {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    // this mode is only used for `vcvtps2ph` and `vrndscaleps`, neither use sae rounding
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;

                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                let sz = regs_size(instruction);

                apply_broadcast(instruction, 4, sz);

                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Gm_E_LL_imm8_sae_W1 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;

            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            if let OperandSpec::RegMMM = mem_oper {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    // this mode is only used for `vrndscalepd`, does not use sae rounding
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;

                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                let sz = regs_size(instruction);

                apply_broadcast(instruction, 8, sz);

                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Mq_G_xmm_W1 => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 1)?;

            instruction.mem_size = 8;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            }
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::G_V_Ed_xmm_imm8_W0 => {
            deny_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            instruction.regs[3].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 4;
            }
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;
        }
        generated::EVEXOperandCode::G_V_xmm_Edq_sae => {
            deny_mask_reg(instruction)?;
            deny_z(instruction)?;

            let (sz, bank) = if instruction.prefixes.evex_unchecked().vex().w() {
                (DEFAULT_EVEX_REGISTER_WIDTH, DEFAULT_EVEX_REGISTER_SIZE)
            } else {
                (4, RegisterBank::D)
            };

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            instruction.regs[3].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.regs[1].bank = bank;
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = sz;
            }
            if instruction.prefixes.evex_unchecked().broadcast() {
                if mem_oper == OperandSpec::RegMMM {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                } else {
                    return Err(ErrorKind::InvalidOperand);
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR;
            }
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::G_V_xmm_Edq_imm8 => {
            deny_mask_reg(instruction)?;

            let (sz, bank) = if instruction.prefixes.evex_unchecked().vex().w() {
                if isa_has_qwords() {
                    instruction.opcode = Opcode::VPINSRQ;
                }
                (DEFAULT_EVEX_REGISTER_WIDTH, DEFAULT_EVEX_REGISTER_SIZE)
            } else {
                (4, RegisterBank::D)
            };

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            instruction.regs[3].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.regs[1].bank = bank;
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = sz;
            }
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;
        }
        generated::EVEXOperandCode::G_V_xmm_Ebd_imm8 => {
            deny_mask_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            instruction.regs[3].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.regs[1].bank = RegisterBank::D;
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 1;
            }
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;
        }
        generated::EVEXOperandCode::G_V_Mq_xmm_W1 => {
            deny_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            instruction.mem_size = 8;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            }
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::Gm_V_E_LL_bcast_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let sz = regs_size(instruction);

            // specifically for vunpcklpd!!! probably need to reconsider.
            apply_broadcast(instruction, 8, sz);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::M_G_LL_W0 => {
            deny_vex_reg(instruction)?;
            deny_mask_reg(instruction)?;

            instruction.mem_size = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            if mem_oper == OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            }
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::M_G_LL_W1 => {
            deny_vex_reg(instruction)?;
            deny_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            instruction.mem_size = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            if mem_oper == OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            }
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Em_G_LL_W1 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            instruction.mem_size = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::G_Ed_xmm_sae_W0 => {
            deny_vex_reg(instruction)?;
            deny_mask_reg(instruction)?;
            // vucomiss and vcomiss both are W=0
            ensure_W(instruction, 0)?;

            instruction.mem_size = 4;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;

            // in specific support of vcomisd/vucomisd
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
            } else {
                instruction.operands[0] = OperandSpec::RegRRR;
            }
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_Eq_xmm_sae_W1 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            // vucomisd and vcomisd both are W=1
            ensure_W(instruction, 1)?;

            instruction.mem_size = 8;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;

            // in specific support of vcomisd/vucomisd
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
            } else {
                instruction.operands[0] = OperandSpec::RegRRR;
            }
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_E_LL_W1 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            instruction.mem_size = regs_size(instruction);

            if instruction.opcode == Opcode::VMOVDDUP && instruction.mem_size == 16 {
                instruction.mem_size = 8;
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::VCVTUDQ2PD => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;

            let sz = regs_size(instruction);

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VCVTUDQ2PD {
                    instruction.opcode = Opcode::VCVTUQQ2PD;
                } else if instruction.opcode == Opcode::VCVTDQ2PD {
                    instruction.opcode = Opcode::VCVTQQ2PD;
                }
            } else if instruction.prefixes.evex_unchecked().lp()
                && instruction.prefixes.evex_unchecked().vex().l()
            {
                return Err(ErrorKind::InvalidOperand);
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    set_reg_sizes_from_ll(instruction)?;
                    if !instruction.prefixes.evex_unchecked().vex().w() {
                        if instruction.regs[0].bank == RegisterBank::Z {
                            instruction.regs[1].bank = RegisterBank::Y;
                        } else if instruction.regs[0].bank == RegisterBank::Y {
                            instruction.regs[1].bank = RegisterBank::X;
                        }
                    }
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                set_reg_sizes_from_ll(instruction)?;
                if instruction.prefixes.evex_unchecked().vex().w() {
                    apply_broadcast(instruction, 8, sz);
                } else {
                    apply_broadcast(instruction, 4, sz / 2);
                }
            }
        }
        generated::EVEXOperandCode::Maskm_V_E_LL_imm8_sae_bcast_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                apply_broadcast(instruction, 8, sz);
                set_reg_sizes_from_ll(instruction)?;
            }
            instruction.regs[0].bank = RegisterBank::K;
            if instruction.regs[0].num > 7 {
                return Err(ErrorKind::InvalidOperand);
            }
        }
        generated::EVEXOperandCode::Gm_E_LL_sae_bcast_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                apply_broadcast(instruction, 8, sz);
                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Gm_E_LL_sae_bcast_W1 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                apply_broadcast(instruction, 8, sz);
                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Gm_V_Ed_LL_bcast => {
            check_mask_reg(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VPERMPS {
                    instruction.opcode = Opcode::VPERMPD;
                } else if instruction.opcode == Opcode::VBLENDMPS {
                    instruction.opcode = Opcode::VBLENDMPD;
                } else if instruction.opcode == Opcode::VPERMI2PS {
                    instruction.opcode = Opcode::VPERMI2PD;
                } else if instruction.opcode == Opcode::VPERMT2PS {
                    instruction.opcode = Opcode::VPERMT2PD
                }
            }

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    return Err(ErrorKind::InvalidOpcode);
                } else {
                    instruction.mem_size = 0;
                }
            } else if instruction.prefixes.evex_unchecked().vex().w() {
                apply_broadcast(instruction, 8, sz);
            } else {
                apply_broadcast(instruction, 4, sz);
            }
            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_Ed_LL_bcast_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    return Err(ErrorKind::InvalidOpcode);
                } else {
                    instruction.mem_size = 0;
                }
            } else {
                apply_broadcast(instruction, 4, sz);
            }
            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            if instruction.prefixes.evex_unchecked().broadcast() {
                return Err(ErrorKind::InvalidOpcode);
            }

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = sz;
            }

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            if instruction.prefixes.evex_unchecked().broadcast() {
                return Err(ErrorKind::InvalidOpcode);
            }

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = sz;
            }

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_sae_bcast_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    if instruction.opcode == Opcode::VMINPS || instruction.opcode == Opcode::VMAXPS
                    {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    } else {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    }
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                apply_broadcast(instruction, 4, sz);
                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Gm_V_E_LL_sae_bcast_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    if instruction.opcode == Opcode::VMINPD || instruction.opcode == Opcode::VMAXPD
                    {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    } else {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    }
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                apply_broadcast(instruction, 8, sz);
                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::G_V_E_LL => {
            deny_mask_reg(instruction)?;
            if [
                Opcode::VAESDECLAST,
                Opcode::VAESDEC,
                Opcode::VAESENC,
                Opcode::VAESENCLAST,
            ]
            .contains(&instruction.opcode)
            {
                deny_z(instruction)?;
            }

            let sz = regs_size(instruction);

            if instruction.prefixes.evex_unchecked().vex().w() {
                apply_broadcast(instruction, 8, sz);
            } else {
                apply_broadcast(instruction, 4, sz);
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_bcast => {
            check_mask_reg(instruction)?;

            let sz = regs_size(instruction);

            if instruction.prefixes.evex_unchecked().vex().w() {
                apply_broadcast(instruction, 8, sz);
                instruction.opcode = if instruction.opcode == Opcode::VPANDD {
                    Opcode::VPANDQ
                } else if instruction.opcode == Opcode::VPANDND {
                    Opcode::VPANDNQ
                } else if instruction.opcode == Opcode::VPORD {
                    Opcode::VPORQ
                } else if instruction.opcode == Opcode::VPXORD {
                    Opcode::VPXORQ
                } else if instruction.opcode == Opcode::VPRORVD {
                    Opcode::VPRORVQ
                } else if instruction.opcode == Opcode::VPROLVD {
                    Opcode::VPROLVQ
                } else if instruction.opcode == Opcode::VPERMD {
                    Opcode::VPERMQ
                } else if instruction.opcode == Opcode::VPMINSD {
                    Opcode::VPMINSQ
                } else if instruction.opcode == Opcode::VPMINUD {
                    Opcode::VPMINUQ
                } else if instruction.opcode == Opcode::VPMAXSD {
                    Opcode::VPMAXSQ
                } else if instruction.opcode == Opcode::VPMAXUD {
                    Opcode::VPMAXUQ
                } else if instruction.opcode == Opcode::VPSRLVD {
                    Opcode::VPSRLVQ
                } else if instruction.opcode == Opcode::VPSRAVD {
                    Opcode::VPSRAVQ
                } else if instruction.opcode == Opcode::VPSLLVD {
                    Opcode::VPSLLVQ
                } else if instruction.opcode == Opcode::VPMULLD {
                    Opcode::VPMULLQ
                } else if instruction.opcode == Opcode::VPBLENDMD {
                    Opcode::VPBLENDMQ
                } else if instruction.opcode == Opcode::VPSHLDVD {
                    Opcode::VPSHLDVQ
                } else if instruction.opcode == Opcode::VPSHRDVD {
                    Opcode::VPSHRDVQ
                } else if instruction.opcode == Opcode::VPERMI2D {
                    Opcode::VPERMI2Q
                } else if instruction.opcode == Opcode::VPERMT2D {
                    Opcode::VPERMT2Q
                } else {
                    instruction.opcode
                };
            } else {
                apply_broadcast(instruction, 4, sz);
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_E_LL_imm8_bcast_W0 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            apply_broadcast(instruction, 4, sz);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_E_LL_imm8_bcast_W1 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let sz = regs_size(instruction);

            apply_broadcast(instruction, 8, sz);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_Ed_LL_imm8_sae_noround_bcast => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;

            let sz = regs_size(instruction);

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VGETMANTPS {
                    instruction.opcode = Opcode::VGETMANTPD;
                }
                apply_broadcast(instruction, 8, sz);
            } else {
                apply_broadcast(instruction, 4, sz);
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[1] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            if instruction.prefixes.evex_unchecked().broadcast() {
                if mem_oper == OperandSpec::RegMMM {
                    instruction.mem_size = 0;
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                if instruction.prefixes.evex_unchecked().lp()
                    && instruction.prefixes.evex_unchecked().vex().l()
                {
                    return Err(ErrorKind::InvalidOperand);
                }
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Gm_Ed_LL_sae_noround_bcast_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            apply_broadcast(instruction, 4, sz);

            if instruction.prefixes.evex_unchecked().broadcast() {
                if mem_oper == OperandSpec::RegMMM {
                    instruction.mem_size = 0;
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                if instruction.prefixes.evex_unchecked().lp()
                    && instruction.prefixes.evex_unchecked().vex().l()
                {
                    return Err(ErrorKind::InvalidOperand);
                }
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Gm_V_Ed_xmm_sae_noround_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
            } else {
                if instruction.prefixes.evex_unchecked().lp()
                    && instruction.prefixes.evex_unchecked().vex().l()
                {
                    return Err(ErrorKind::InvalidOperand);
                }
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            }
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 4;
            }
            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::Gm_V_Ed_xmm_sae => {
            check_mask_reg(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VSCALEFSS {
                    instruction.opcode = Opcode::VSCALEFSD;
                } else if instruction.opcode == Opcode::VRCP14SS {
                    instruction.opcode = Opcode::VRCP14SD;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
            } else {
                if instruction.prefixes.evex_unchecked().lp()
                    && instruction.prefixes.evex_unchecked().vex().l()
                {
                    return Err(ErrorKind::InvalidOperand);
                }
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            }
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.mem_size = 8;
            } else {
                instruction.mem_size = 4;
            }
            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::Gm_V_Ed_xmm_sae_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if instruction.prefixes.evex_unchecked().broadcast() {
                if instruction.opcode == Opcode::VMINSS || instruction.opcode == Opcode::VMAXSS {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                }
            } else {
                if instruction.prefixes.evex_unchecked().lp()
                    && instruction.prefixes.evex_unchecked().vex().l()
                {
                    return Err(ErrorKind::InvalidOperand);
                }
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            }
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                deny_broadcast(instruction)?;
                instruction.mem_size = 4;
            }
            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::Eqm_G_xmm_imm8_sae_W0 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    // sae sets this to `vcvtps2ph ymm, zmm, imm8`
                    instruction.regs[1].bank = RegisterBank::Y;
                    instruction.regs[0].bank = RegisterBank::Z;
                    instruction.operands[0] = OperandSpec::RegMMM_maskmerge_sae_noround;
                } else {
                    instruction.regs[1].bank = RegisterBank::X;
                    instruction.regs[0].bank = RegisterBank::X;
                    instruction.operands[0] = OperandSpec::RegMMM_maskmerge;
                }
            } else if instruction.prefixes.evex_unchecked().broadcast() {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.mem_size = 8;
                instruction.regs[0].bank = RegisterBank::X;
                instruction.operands[0] = mem_oper.masked();
            }
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::Em_xmm_G_ymm_imm8_sae_W0 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    // sae sets this to `vcvtps2ph ymm, zmm, imm8`
                    instruction.regs[1].bank = RegisterBank::Y;
                    instruction.regs[0].bank = RegisterBank::Z;
                    instruction.operands[0] = OperandSpec::RegMMM_maskmerge_sae_noround;
                } else {
                    instruction.regs[1].bank = RegisterBank::X;
                    instruction.regs[0].bank = RegisterBank::Y;
                    instruction.operands[0] = OperandSpec::RegMMM_maskmerge;
                }
            } else if instruction.prefixes.evex_unchecked().broadcast() {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::Y;
                instruction.operands[0] = mem_oper.masked();
            }
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.mem_size = 16;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::Em_ymm_G_zmm_imm8_sae_W0 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::Z;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegMMM_maskmerge_sae_noround;
                } else {
                    instruction.operands[0] = OperandSpec::RegMMM_maskmerge;
                }
            } else if instruction.prefixes.evex_unchecked().broadcast() {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.operands[0] = mem_oper.masked();
            }
            instruction.mem_size = 32;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::Gm_V_zmm_E_xmm_imm8 => {
            check_mask_reg(instruction)?;
            deny_broadcast(instruction)?;

            instruction.opcode = if instruction.prefixes.evex_unchecked().vex().w() {
                Opcode::VINSERTI64X2
            } else {
                Opcode::VINSERTI32X4
            };

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::Z;
            instruction.regs[3].bank = RegisterBank::Z;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 16;
            }
            instruction.operands[0] = OperandSpec::RegRRR.masked();
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;
        }
        generated::EVEXOperandCode::Gm_V_ymm_E_xmm_imm8 => {
            check_mask_reg(instruction)?;
            deny_broadcast(instruction)?;

            instruction.opcode = if instruction.prefixes.evex_unchecked().vex().w() {
                Opcode::VINSERTI64X2
            } else {
                Opcode::VINSERTI32X4
            };

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::Y;
            instruction.regs[3].bank = RegisterBank::Y;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 16;
            }
            instruction.operands[0] = OperandSpec::RegRRR.masked();
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;
        }
        generated::EVEXOperandCode::Gm_V_zmm_E_ymm_imm8 => {
            check_mask_reg(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VINSERTF32X8 {
                    instruction.opcode = Opcode::VINSERTF64X4;
                } else if instruction.opcode == Opcode::VINSERTI32X8 {
                    instruction.opcode = Opcode::VINSERTI64X4;
                }
            };

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::Z;
            instruction.regs[3].bank = RegisterBank::Z;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.mem_size = 32;
            instruction.operands[0] = OperandSpec::RegRRR.masked();
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;
        }
        generated::EVEXOperandCode::Em_ymm_G_zmm_imm8 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VEXTRACTF32X8 {
                    instruction.opcode = Opcode::VEXTRACTF64X4;
                } else if instruction.opcode == Opcode::VEXTRACTI32X8 {
                    instruction.opcode = Opcode::VEXTRACTI64X4;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::Z;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.mem_size = 32;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::Gm_zmm_Eq_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Z;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 8;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_ymm_Ed_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Y;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 4;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_xmm_Ew_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 2;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_zmm_E_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Z;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 16;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_ymm_Eq_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Y;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 8;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_xmm_Ed_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 4;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_zmm_E_ymm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.regs[0].bank = RegisterBank::Z;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 32;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_zmm_E_ymm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.regs[0].bank = RegisterBank::Z;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 32;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_ymm_E_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Y;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 16;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_ymm_E_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Y;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 16;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_xmm_Eq_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 8;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_xmm_Eq_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 8;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Em_ymm_G_zmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.regs[0].bank = RegisterBank::Z;
            instruction.mem_size = 32;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Em_xmm_G_zmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Z;
            instruction.mem_size = 16;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Em_xmm_G_ymm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Y;
            instruction.mem_size = 16;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Eqm_xmm_G_zmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Z;
            instruction.mem_size = 8;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Eqm_xmm_G_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;
            instruction.mem_size = 8;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Edm_xmm_G_ymm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Y;
            instruction.mem_size = 4;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Edm_xmm_G_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;
            instruction.mem_size = 4;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Ewm_xmm_G_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;
            instruction.mem_size = 2;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Eqm_xmm_G_ymm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Y;
            instruction.mem_size = 8;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_xmm_Ed_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 4;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_ymm_Ed_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Y;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 4;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_zmm_M_ymm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VBROADCASTF32X8 {
                    instruction.opcode = Opcode::VBROADCASTF64X4;
                } else if instruction.opcode == Opcode::VBROADCASTI32X8 {
                    instruction.opcode = Opcode::VBROADCASTI64X4;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Z;
            if mem_oper == OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.mem_size = 32;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_zmm_M_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VBROADCASTF32X4 {
                    instruction.opcode = Opcode::VBROADCASTF64X2;
                } else if instruction.opcode == Opcode::VBROADCASTI32X4 {
                    instruction.opcode = Opcode::VBROADCASTI64X2;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Z;
            if mem_oper == OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.mem_size = 16;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_ymm_M_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VBROADCASTF32X4 {
                    instruction.opcode = Opcode::VBROADCASTF64X2;
                } else if instruction.opcode == Opcode::VBROADCASTI32X4 {
                    instruction.opcode = Opcode::VBROADCASTI64X2;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Y;
            if mem_oper == OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.mem_size = 16;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_zmm_Ed_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.opcode = Opcode::VBROADCASTSD;
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Z;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 8;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::VBROADCASTF32X2_Gm_ymm_Ed_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.opcode = Opcode::VBROADCASTSD;
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Y;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 8;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_zmm_Ed_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::Z;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 4;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Em_xmm_G_LL_imm8 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.opcode = if instruction.opcode == Opcode::VEXTRACTF32X4 {
                    Opcode::VEXTRACTF64X2
                } else if instruction.opcode == Opcode::VEXTRACTI32X4 {
                    Opcode::VEXTRACTI64X2
                } else {
                    instruction.opcode
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            if instruction.prefixes.evex_unchecked().lp() {
                instruction.regs[0].bank = RegisterBank::Z;
            } else {
                instruction.regs[0].bank = RegisterBank::Y;
            }
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.mem_size = 16;
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::Gm_V_LL_E_xmm_imm8 => {
            check_mask_reg(instruction)?;
            deny_broadcast(instruction)?;

            instruction.opcode = if instruction.prefixes.evex_unchecked().vex().w() {
                Opcode::VINSERTF64X2
            } else {
                Opcode::VINSERTF32X4
            };

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            set_reg_sizes_from_ll(instruction)?;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.mem_size = 16;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;
        }
        generated::EVEXOperandCode::Gm_V_LL_E_xmm_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            set_reg_sizes_from_ll(instruction)?;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.mem_size = 16;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::Gm_V_LL_E_xmm_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            set_reg_sizes_from_ll(instruction)?;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.mem_size = 16;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::Gm_V_LL_E_xmm => {
            check_mask_reg(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.opcode = if instruction.opcode == Opcode::VPSRAD {
                    Opcode::VPSRAQ
                } else {
                    instruction.opcode
                };
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            set_reg_sizes_from_ll(instruction)?;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.mem_size = 16;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::VPEXTRW => {
            deny_mask_reg(instruction)?;
            deny_z(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::D;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;

            if mem_oper == OperandSpec::RegMMM {
                instruction.regs[1].bank = RegisterBank::X;
            } else {
                return Err(ErrorKind::InvalidOperand);
            }

            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::VPINSRW => {
            deny_mask_reg(instruction)?;
            deny_z(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;

            if mem_oper == OperandSpec::RegMMM {
                instruction.regs[1].bank = RegisterBank::D;
            } else {
                instruction.mem_size = 2;
            }

            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;
        }
        generated::EVEXOperandCode::VMOVQ_G_Ed_xmm => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 1)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 8;
            }

            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::VMOVQ_Ed_G_xmm => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 1)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 8;
            }

            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::VMOVQ_7e => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 1)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;

            if mem_oper != OperandSpec::RegMMM {
                instruction.mem_size = 8;
            }

            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::VMOVD_7e => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if isa_has_qwords() {
                    instruction.opcode = Opcode::VMOVQ;
                }
                if mem_oper == OperandSpec::RegMMM {
                    instruction.regs[1].bank = DEFAULT_EVEX_REGISTER_SIZE;
                } else {
                    instruction.mem_size = DEFAULT_EVEX_REGISTER_WIDTH;
                }
            } else if mem_oper == OperandSpec::RegMMM {
                instruction.regs[1].bank = RegisterBank::D;
            } else {
                instruction.mem_size = 4;
            }

            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR_maskmerge;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::VMOVD_6e => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if isa_has_qwords() {
                    instruction.opcode = Opcode::VMOVQ;
                }
                if mem_oper == OperandSpec::RegMMM {
                    instruction.regs[1].bank = DEFAULT_EVEX_REGISTER_SIZE;
                } else {
                    instruction.mem_size = DEFAULT_EVEX_REGISTER_WIDTH;
                }
            } else if mem_oper == OperandSpec::RegMMM {
                instruction.regs[1].bank = RegisterBank::D;
            } else {
                instruction.mem_size = 4;
            }

            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Mask_V_E_LL_bcast => {
            check_mask_reg(instruction)?;

            let sz = regs_size(instruction);

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VP2INTERSECTD {
                    instruction.opcode = Opcode::VP2INTERSECTQ;
                } else if instruction.opcode == Opcode::VPTESTNMD {
                    instruction.opcode = Opcode::VPTESTNMQ;
                } else if instruction.opcode == Opcode::VPTESTMD {
                    instruction.opcode = Opcode::VPTESTMQ;
                }
                apply_broadcast(instruction, 8, sz);
            } else {
                apply_broadcast(instruction, 4, sz);
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Mask_V_E_LL_bcast_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let sz = regs_size(instruction);

            apply_broadcast(instruction, 8, sz);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Mask_V_E_LL_bcast_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            apply_broadcast(instruction, 4, sz);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Em_G_LL => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VMOVDQA32 {
                    instruction.opcode = Opcode::VMOVDQA64;
                } else if instruction.opcode == Opcode::VMOVDQU32 {
                    instruction.opcode = Opcode::VMOVDQU64;
                } else if instruction.opcode == Opcode::VMOVDQU8 {
                    instruction.opcode = Opcode::VMOVDQU16;
                } else if instruction.opcode == Opcode::VPCOMPRESSB {
                    instruction.opcode = Opcode::VPCOMPRESSW;
                } else if instruction.opcode == Opcode::VPCOMPRESSD {
                    instruction.opcode = Opcode::VPCOMPRESSQ;
                } else if instruction.opcode == Opcode::VCOMPRESSPS {
                    instruction.opcode = Opcode::VCOMPRESSPD;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Mask_U_LL => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VPMOVB2M {
                    instruction.opcode = Opcode::VPMOVW2M;
                } else {
                    instruction.opcode = Opcode::VPMOVQ2M;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                instruction.regs[0].bank = RegisterBank::K;
            } else {
                return Err(ErrorKind::InvalidOperand);
            }
        }
        generated::EVEXOperandCode::G_LL_Mask => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VPMOVM2B {
                    instruction.opcode = Opcode::VPMOVM2W;
                } else {
                    instruction.opcode = Opcode::VPMOVM2Q;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                instruction.regs[1].bank = RegisterBank::K;
            } else {
                return Err(ErrorKind::InvalidOperand);
            }
        }
        generated::EVEXOperandCode::G_LL_Mask_W1 => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 1)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                instruction.regs[1].bank = RegisterBank::K;
            } else {
                return Err(ErrorKind::InvalidOperand);
            }
        }
        generated::EVEXOperandCode::G_LL_Mask_W0 => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                instruction.regs[1].bank = RegisterBank::K;
            } else {
                return Err(ErrorKind::InvalidOperand);
            }
        }
        generated::EVEXOperandCode::G_E_LL_W0 => {
            deny_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::E_G_LL_W0 => {
            deny_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Em_G_LL_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = mem_oper.masked();
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Operands_72_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            let (r_sz, m_sz, m_data_sz) = match (
                instruction.prefixes.evex_unchecked().lp(),
                instruction.prefixes.evex_unchecked().vex().l(),
            ) {
                (true, true) => {
                    return Err(ErrorKind::InvalidOpcode);
                }
                (true, false) => (RegisterBank::Y, RegisterBank::Z, 64),
                (false, true) => (RegisterBank::X, RegisterBank::Y, 32),
                (false, false) => (RegisterBank::X, RegisterBank::X, 16),
            };
            instruction.regs[0].bank = r_sz;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                instruction.regs[1].bank = m_sz;
            } else {
                apply_broadcast(instruction, 4, m_data_sz);
            }
        }
        generated::EVEXOperandCode::Gm_E_LL_bcast => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;

            let sz = regs_size(instruction);

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VPLZCNTD {
                    instruction.opcode = Opcode::VPLZCNTQ;
                } else if instruction.opcode == Opcode::VRCP14PS {
                    instruction.opcode = Opcode::VRCP14PD;
                } else if instruction.opcode == Opcode::VPOPCNTD {
                    instruction.opcode = Opcode::VPOPCNTQ;
                } else if instruction.opcode == Opcode::VPCONFLICTD {
                    instruction.opcode = Opcode::VPCONFLICTQ;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    return Err(ErrorKind::InvalidOpcode);
                }
                instruction.mem_size = 0;
            } else if instruction.prefixes.evex_unchecked().vex().w() {
                apply_broadcast(instruction, 8, sz);
            } else {
                apply_broadcast(instruction, 4, sz);
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_E_LL_bcast_W1 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    return Err(ErrorKind::InvalidOpcode);
                }
                instruction.mem_size = 0;
            } else {
                apply_broadcast(instruction, 8, sz);
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_E_LL_bcast_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    return Err(ErrorKind::InvalidOpcode);
                }
                instruction.mem_size = 0;
            } else {
                apply_broadcast(instruction, 4, sz);
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_LL_Ud => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w()
                && isa_has_qwords()
                && instruction.opcode == Opcode::VPBROADCASTD
            {
                instruction.opcode = Opcode::VPBROADCASTQ;
            }

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                if instruction.prefixes.evex_unchecked().vex().w() {
                    instruction.regs[1].bank = DEFAULT_EVEX_REGISTER_SIZE;
                } else {
                    instruction.regs[1].bank = RegisterBank::D;
                }
            } else {
                return Err(ErrorKind::InvalidOperand);
            }
        }
        generated::EVEXOperandCode::Gm_LL_Ud_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                instruction.regs[1].bank = RegisterBank::D;
            } else {
                return Err(ErrorKind::InvalidOperand);
            }
        }
        generated::EVEXOperandCode::Gm_LL_Eq_xmm => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.opcode = Opcode::VPBROADCASTQ;
            } else {
                instruction.opcode = Opcode::VBROADCASTI32X2;
            }

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                instruction.regs[1].bank = RegisterBank::X;
            } else {
                instruction.mem_size = 8;
            }
        }
        generated::EVEXOperandCode::Gm_LL_Ed_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                instruction.regs[1].bank = RegisterBank::X;
            } else {
                instruction.mem_size = 4;
            }
        }
        generated::EVEXOperandCode::Gm_LL_Ew_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                instruction.regs[1].bank = RegisterBank::X;
            } else {
                instruction.mem_size = 2;
            }
        }
        generated::EVEXOperandCode::Gm_LL_Eb_xmm_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                instruction.regs[1].bank = RegisterBank::X;
            } else {
                instruction.mem_size = 1;
            }
        }
        generated::EVEXOperandCode::Gm_E_LL_W0 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_E_LL_imm8 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VMOVDQA32 {
                    instruction.opcode = Opcode::VMOVDQA64;
                } else if instruction.opcode == Opcode::VMOVDQU32 {
                    instruction.opcode = Opcode::VMOVDQU64;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gd_Ed_xmm_sae => {
            deny_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_z(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);

            if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.regs[0].bank = DEFAULT_EVEX_REGISTER_SIZE;
            } else {
                instruction.regs[0].bank = RegisterBank::D;
            }

            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR;

            if instruction.prefixes.evex_unchecked().broadcast() {
                if mem_oper == OperandSpec::RegMMM {
                    if instruction.opcode == Opcode::VCVTSS2USI
                        || instruction.opcode == Opcode::VCVTSD2SI
                        || instruction.opcode == Opcode::VCVTSD2USI
                    {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    } else {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    }
                } else {
                    return Err(ErrorKind::InvalidOperand);
                }
            } else if instruction.prefixes.evex_unchecked().lp()
                && instruction.prefixes.evex_unchecked().vex().l()
            {
                return Err(ErrorKind::InvalidOperand);
            }

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                deny_broadcast(instruction)?;
                if instruction.opcode == Opcode::VCVTTSD2SI
                    || instruction.opcode == Opcode::VCVTSD2SI
                    || instruction.opcode == Opcode::VCVTTSD2USI
                    || instruction.opcode == Opcode::VCVTSD2USI
                {
                    instruction.mem_size = 8;
                } else {
                    instruction.mem_size = 4;
                }
            }
            instruction.operands[1] = mem_oper;

            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_E_LL_sae_bcast => {
            check_mask_reg(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VGETEXPPS {
                    instruction.opcode = Opcode::VGETEXPPD;
                } else if instruction.opcode == Opcode::VRSQRT14PS {
                    instruction.opcode = Opcode::VRSQRT14PD;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }

            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            if instruction.prefixes.evex_unchecked().broadcast() {
                if mem_oper != OperandSpec::RegMMM {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    set_reg_sizes_from_ll(instruction)?;
                    apply_broadcast(
                        instruction,
                        if instruction.prefixes.evex_unchecked().vex().w() {
                            8
                        } else {
                            4
                        },
                        sz,
                    );
                } else {
                    if instruction.opcode == Opcode::VSQRTPS
                        || instruction.opcode == Opcode::VCVTPS2DQ
                    {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    } else {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    }
                    set_reg_sizes(instruction, RegisterBank::Z);
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Gm_E_LL => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VMOVDQA32 {
                    instruction.opcode = Opcode::VMOVDQA64;
                } else if instruction.opcode == Opcode::VMOVDQU32 {
                    instruction.opcode = Opcode::VMOVDQU64;
                } else if instruction.opcode == Opcode::VMOVDQU8 {
                    instruction.opcode = Opcode::VMOVDQU16;
                } else if instruction.opcode == Opcode::VPOPCNTB {
                    instruction.opcode = Opcode::VPOPCNTW;
                } else if instruction.opcode == Opcode::VPEXPANDB {
                    instruction.opcode = Opcode::VPEXPANDW;
                } else if instruction.opcode == Opcode::VEXPANDPS {
                    instruction.opcode = Opcode::VEXPANDPD;
                } else if instruction.opcode == Opcode::VPEXPANDD {
                    instruction.opcode = Opcode::VPEXPANDQ;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_Ed_LL_imm8_bcast => {
            check_mask_reg(instruction)?;

            let sz = regs_size(instruction);

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VSHUFF32X4 {
                    instruction.opcode = Opcode::VSHUFF64X2;
                }
                apply_broadcast(instruction, 8, sz);
            } else {
                apply_broadcast(instruction, 4, sz);
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                deny_broadcast(instruction)?;
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_bcast_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            apply_broadcast(instruction, 4, sz);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                deny_broadcast(instruction)?;
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL => {
            check_mask_reg(instruction)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VPBLENDMB {
                    instruction.opcode = Opcode::VPBLENDMW;
                } else if instruction.opcode == Opcode::VPERMI2B {
                    instruction.opcode = Opcode::VPERMI2W;
                } else if instruction.opcode == Opcode::VPERMT2B {
                    instruction.opcode = Opcode::VPERMT2W;
                } else if instruction.opcode == Opcode::VPERMB {
                    instruction.opcode = Opcode::VPERMW;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Mask_V_E_LL_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Mask_V_E_LL => {
            check_mask_reg(instruction)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);

            instruction.mem_size = sz;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VPTESTNMB {
                    instruction.opcode = Opcode::VPTESTNMW;
                } else if instruction.opcode == Opcode::VPTESTMB {
                    instruction.opcode = Opcode::VPTESTMW;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Maskm_V_Eq_xmm_imm8_sae_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;
            deny_z(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                deny_broadcast(instruction)?;
                instruction.mem_size = 8;
            }
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            }
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes(instruction, RegisterBank::X);
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Maskm_V_Ed_xmm_imm8_sae_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;
            deny_z(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 4;
                deny_broadcast(instruction)?;
            }
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            }
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes(instruction, RegisterBank::X);
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Mask_V_E_LL_imm8 => {
            check_mask_reg(instruction)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);
            instruction.mem_size = sz;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VPCMPUB {
                    instruction.opcode = Opcode::VPCMPUW;
                } else if instruction.opcode == Opcode::VPCMPB {
                    instruction.opcode = Opcode::VPCMPW;
                }
            };

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes_from_ll(instruction)?;
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Mask_Ed_xmm_imm8 => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;
            deny_z(instruction)?;
            deny_broadcast(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.opcode = if instruction.opcode == Opcode::VFPCLASSSS {
                    Opcode::VFPCLASSSD
                } else {
                    instruction.opcode
                };
            };

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.mem_size = 8;
            } else {
                instruction.mem_size = 4;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            set_reg_sizes(instruction, RegisterBank::X);
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Mask_E_LL_imm8_bcast => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;

            let sz = regs_size(instruction);

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    apply_broadcast(instruction, 8, sz);
                } else {
                    instruction.mem_size = sz;
                }
                instruction.opcode = if instruction.opcode == Opcode::VFPCLASSPS {
                    Opcode::VFPCLASSPD
                } else {
                    instruction.opcode
                };
            } else if instruction.prefixes.evex_unchecked().broadcast() {
                apply_broadcast(instruction, 4, sz);
            } else {
                instruction.mem_size = sz;
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                deny_broadcast(instruction)?;
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Mask_V_E_LL_imm8_sae_bcast_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    apply_broadcast(instruction, 8, sz);
                } else {
                    instruction.mem_size = sz;
                }
            } else if instruction.prefixes.evex_unchecked().broadcast() {
                apply_broadcast(instruction, 4, sz);
            } else {
                instruction.mem_size = sz;
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    set_reg_sizes_from_ll(instruction)?;
                }
                instruction.mem_size = 0;
            } else {
                set_reg_sizes_from_ll(instruction)?;
            }
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Mask_V_E_LL_imm8_bcast => {
            check_mask_reg(instruction)?;

            let sz = regs_size(instruction);

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    apply_broadcast(instruction, 8, sz);
                } else {
                    instruction.mem_size = sz;
                }
                // this operand code is used in a few places, apply `w` as appropriate
                instruction.opcode = if instruction.opcode == Opcode::VPCMPUD {
                    Opcode::VPCMPUQ
                } else {
                    Opcode::VPCMPQ
                };
            } else if instruction.prefixes.evex_unchecked().broadcast() {
                apply_broadcast(instruction, 4, sz);
            } else {
                instruction.mem_size = sz;
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                deny_broadcast(instruction)?;
                instruction.mem_size = 0;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes_from_ll(instruction)?;
            if instruction.regs[0].num >= 8 {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.regs[0].bank = RegisterBank::K;
            }
        }
        generated::EVEXOperandCode::Opcode_72_Gm_E_LL_imm8_bcast => {
            check_mask_reg(instruction)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            let rrr = (modrm >> 3) & 7;

            let item_size = if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.opcode = [
                    Ok(Opcode::VPRORQ),
                    Ok(Opcode::VPROLQ),
                    Err(ErrorKind::InvalidOpcode),
                    Err(ErrorKind::InvalidOpcode),
                    Ok(Opcode::VPSRAQ),
                    Err(ErrorKind::InvalidOpcode),
                    Err(ErrorKind::InvalidOpcode),
                    Err(ErrorKind::InvalidOpcode),
                ][rrr as usize]?;
                8
            } else {
                instruction.opcode = [
                    Ok(Opcode::VPRORD),
                    Ok(Opcode::VPROLD),
                    Ok(Opcode::VPSRLD),
                    Err(ErrorKind::InvalidOpcode),
                    Ok(Opcode::VPSRAD),
                    Ok(Opcode::VPSLLD),
                    Err(ErrorKind::InvalidOpcode),
                    Err(ErrorKind::InvalidOpcode),
                ][rrr as usize]?;
                4
            };

            apply_broadcast(instruction, item_size, sz);

            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegVex_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_imm8_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;
            deny_broadcast(instruction)?;

            let sz = regs_size(instruction);
            instruction.mem_size = sz;
            /*
                  instruction.opcode = if instruction.prefixes.evex_unchecked().vex().w() {
                    apply_broadcast(instruction, 8, sz);
                    if instruction.opcode == Opcode::VALIGND {
                      Opcode::VALIGNQ
                    } else if instruction.opcode == Opcode::VPTERNLOGD {
                      Opcode::VPTERNLOGQ
                    } else if instruction.opcode == Opcode::VSHUFI32X4 {
                      Opcode::VSHUFI64X2
                    } else {
                      instruction.opcode
                    }
                  } else {
                    apply_broadcast(instruction, 4, sz);
                    instruction.opcode
                  };
            */

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_imm8_bcast => {
            check_mask_reg(instruction)?;

            let sz = regs_size(instruction);

            instruction.opcode = if instruction.prefixes.evex_unchecked().vex().w() {
                apply_broadcast(instruction, 8, sz);
                if instruction.opcode == Opcode::VALIGND {
                    Opcode::VALIGNQ
                } else if instruction.opcode == Opcode::VPTERNLOGD {
                    Opcode::VPTERNLOGQ
                } else if instruction.opcode == Opcode::VSHUFI32X4 {
                    Opcode::VSHUFI64X2
                } else if instruction.opcode == Opcode::VPSHLDD {
                    Opcode::VPSHLDQ
                } else if instruction.opcode == Opcode::VPSHRDD {
                    Opcode::VPSHRDQ
                } else {
                    instruction.opcode
                }
            } else {
                apply_broadcast(instruction, 4, sz);
                instruction.opcode
            };

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                deny_broadcast(instruction)?;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_imm8_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                deny_broadcast(instruction)?;
            } else {
                instruction.mem_size = sz;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W0 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let sz = regs_size(instruction);

            if instruction.opcode == Opcode::VSHUFPS {
                apply_broadcast(instruction, 4, sz);
            } else {
                apply_broadcast(instruction, 8, sz);
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::G_V_E_LL_imm8 => {
            check_mask_reg(instruction)?;

            instruction.mem_size = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_imm8 => {
            check_mask_reg(instruction)?;

            instruction.mem_size = regs_size(instruction);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_V_E_LL_imm8_bcast_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let sz = regs_size(instruction);

            apply_broadcast(instruction, 8, sz);

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes_from_ll(instruction)?;
        }
        generated::EVEXOperandCode::Gm_ymm_E_zmm_sae_bcast_W1 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Z)?;
            instruction.regs[0].bank = RegisterBank::Y;
            instruction.operands[1] = mem_oper;
            if instruction.prefixes.evex_unchecked().broadcast() {
                if mem_oper != OperandSpec::RegMMM {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    apply_broadcast(instruction, 8, 64);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                instruction.mem_size = 64;
            }
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_xmm_E_ymm_sae_bcast_W1 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Y)?;
            instruction.regs[0].bank = RegisterBank::X;
            instruction.operands[1] = mem_oper;
            if instruction.prefixes.evex_unchecked().broadcast() {
                if mem_oper != OperandSpec::RegMMM {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    apply_broadcast(instruction, 8, 32);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    instruction.regs[0].bank = RegisterBank::Y;
                    instruction.regs[1].bank = RegisterBank::Z;
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                instruction.mem_size = 32;
            }
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_xmm_E_xmm_sae_bcast_W1 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[0].bank = RegisterBank::X;
            instruction.operands[1] = mem_oper;
            if instruction.prefixes.evex_unchecked().broadcast() {
                if mem_oper != OperandSpec::RegMMM {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    apply_broadcast(instruction, 8, 16);
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    instruction.regs[0].bank = RegisterBank::Y;
                    instruction.regs[1].bank = RegisterBank::Z;
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                instruction.mem_size = 16;
            }
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::VCVTTPS2UQQ => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VCVTTPS2UQQ {
                    instruction.opcode = Opcode::VCVTTPD2UQQ;
                } else if instruction.opcode == Opcode::VCVTPS2UQQ {
                    instruction.opcode = Opcode::VCVTPD2UQQ;
                } else if instruction.opcode == Opcode::VCVTTPS2QQ {
                    instruction.opcode = Opcode::VCVTTPD2QQ;
                } else if instruction.opcode == Opcode::VCVTPS2QQ {
                    instruction.opcode = Opcode::VCVTPD2QQ;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
            instruction.mem_size = 0;

            let mut lp = 0;
            if instruction.prefixes.evex_unchecked().lp() {
                lp |= 2;
            }
            if instruction.prefixes.evex_unchecked().vex().l() {
                lp |= 1;
            }

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    if instruction.opcode == Opcode::VCVTPD2UQQ
                        || instruction.opcode == Opcode::VCVTPS2UQQ
                        || instruction.opcode == Opcode::VCVTPD2QQ
                        || instruction.opcode == Opcode::VCVTPS2QQ
                    {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    } else {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    }
                    instruction.regs[0].bank = RegisterBank::Z;
                    if instruction.prefixes.evex_unchecked().vex().w() {
                        instruction.regs[1].bank = RegisterBank::Z;
                    } else {
                        instruction.regs[1].bank = RegisterBank::Y;
                    }
                } else {
                    let (r_sz, m_sz) = if instruction.prefixes.evex_unchecked().vex().w() {
                        [
                            Ok((RegisterBank::X, RegisterBank::X)),
                            Ok((RegisterBank::Y, RegisterBank::Y)),
                            Ok((RegisterBank::Z, RegisterBank::Z)),
                            Err(ErrorKind::InvalidOperand),
                        ][lp]?
                    } else {
                        [
                            Ok((RegisterBank::X, RegisterBank::X)),
                            Ok((RegisterBank::Y, RegisterBank::X)),
                            Ok((RegisterBank::Z, RegisterBank::Y)),
                            Err(ErrorKind::InvalidOperand),
                        ][lp]?
                    };
                    instruction.regs[0].bank = r_sz;
                    instruction.regs[1].bank = m_sz;
                }
            } else {
                let (r_sz, m_sz) = if instruction.prefixes.evex_unchecked().vex().w() {
                    [
                        Ok((RegisterBank::X, 16)),
                        Ok((RegisterBank::Y, 32)),
                        Ok((RegisterBank::Z, 64)),
                        Err(ErrorKind::InvalidOperand),
                    ][lp]?
                } else {
                    [
                        Ok((RegisterBank::X, 8)),
                        Ok((RegisterBank::Y, 16)),
                        Ok((RegisterBank::Z, 32)),
                        Err(ErrorKind::InvalidOperand),
                    ][lp]?
                };
                instruction.regs[0].bank = r_sz;
                if instruction.prefixes.evex_unchecked().vex().w() {
                    apply_broadcast(instruction, 8, m_sz);
                } else {
                    apply_broadcast(instruction, 4, m_sz);
                }
            }
        }
        generated::EVEXOperandCode::VCVTPH2PS => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;

            if instruction.opcode == Opcode::VCVTPS2PD {
                if instruction.prefixes.evex_unchecked().vex().w() {
                    return Err(ErrorKind::InvalidOpcode);
                }
            } else if instruction.opcode == Opcode::VCVTTPS2UQQ {
                instruction.opcode = Opcode::VCVTTPD2UQQ;
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
            instruction.mem_size = 0;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    instruction.regs[0].bank = RegisterBank::Z;
                    if instruction.opcode == Opcode::VCVTTPD2UQQ {
                        instruction.regs[1].bank = RegisterBank::Z;
                    } else {
                        instruction.regs[1].bank = RegisterBank::Y;
                    }
                } else {
                    let (r_sz, m_sz) = match (
                        instruction.prefixes.evex_unchecked().vex().l(),
                        instruction.prefixes.evex_unchecked().lp(),
                    ) {
                        (true, true) => {
                            return Err(ErrorKind::InvalidOperand);
                        }
                        (false, true) => (RegisterBank::Z, RegisterBank::Y),
                        (true, false) => (RegisterBank::Y, RegisterBank::X),
                        (false, false) => (RegisterBank::X, RegisterBank::X),
                    };
                    instruction.regs[0].bank = r_sz;
                    instruction.regs[1].bank = m_sz;
                }
            } else {
                let (r_sz, m_sz) = match (
                    instruction.prefixes.evex_unchecked().vex().l(),
                    instruction.prefixes.evex_unchecked().lp(),
                ) {
                    (true, true) => {
                        return Err(ErrorKind::InvalidOperand);
                    }
                    (true, false) => (RegisterBank::Y, 16),
                    (false, true) => (RegisterBank::Z, 32),
                    (false, false) => (RegisterBank::X, 8),
                };
                instruction.regs[0].bank = r_sz;
                if instruction.opcode == Opcode::VCVTPS2PD {
                    apply_broadcast(instruction, 4, m_sz);
                } else {
                    apply_broadcast(instruction, 8, m_sz);
                }
            }
        }
        generated::EVEXOperandCode::VCVTDQ2PS => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VCVTDQ2PS {
                    instruction.opcode = Opcode::VCVTQQ2PS;
                } else if instruction.opcode == Opcode::VCVTUDQ2PS {
                    instruction.opcode = Opcode::VCVTUQQ2PS;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
            instruction.mem_size = 0;

            let mut lp = 0;
            if instruction.prefixes.evex_unchecked().lp() {
                lp |= 2;
            }
            if instruction.prefixes.evex_unchecked().vex().l() {
                lp |= 1;
            }

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    if instruction.prefixes.evex_unchecked().vex().w() {
                        instruction.regs[0].bank = RegisterBank::Y;
                    } else {
                        instruction.regs[0].bank = RegisterBank::Z;
                    }
                    instruction.regs[1].bank = RegisterBank::Z;
                } else {
                    let (r_sz, m_sz) = if instruction.prefixes.evex_unchecked().vex().w() {
                        [
                            Ok((RegisterBank::X, RegisterBank::X)),
                            Ok((RegisterBank::X, RegisterBank::Y)),
                            Ok((RegisterBank::Y, RegisterBank::Z)),
                            Err(ErrorKind::InvalidOperand),
                        ][lp]?
                    } else {
                        [
                            Ok((RegisterBank::X, RegisterBank::X)),
                            Ok((RegisterBank::Y, RegisterBank::Y)),
                            Ok((RegisterBank::Z, RegisterBank::Z)),
                            Err(ErrorKind::InvalidOperand),
                        ][lp]?
                    };
                    instruction.regs[0].bank = r_sz;
                    instruction.regs[1].bank = m_sz;
                }
            } else {
                let (r_sz, m_sz, item_sz) = if instruction.prefixes.evex_unchecked().vex().w() {
                    [
                        Ok((RegisterBank::X, 16, 8)),
                        Ok((RegisterBank::X, 32, 8)),
                        Ok((RegisterBank::Y, 64, 8)),
                        Err(ErrorKind::InvalidOperand),
                    ][lp]?
                } else {
                    [
                        Ok((RegisterBank::X, 16, 4)),
                        Ok((RegisterBank::Y, 32, 4)),
                        Ok((RegisterBank::Z, 64, 4)),
                        Err(ErrorKind::InvalidOperand),
                    ][lp]?
                };
                instruction.regs[0].bank = r_sz;
                apply_broadcast(instruction, item_sz, m_sz);
            }
        }
        generated::EVEXOperandCode::VCVTTPS2UDQ => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
            instruction.mem_size = 0;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VCVTTPS2UDQ {
                    instruction.opcode = Opcode::VCVTTPD2UDQ;
                } else if instruction.opcode == Opcode::VCVTPS2UDQ {
                    instruction.opcode = Opcode::VCVTPD2UDQ;
                }
            }

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    if instruction.opcode == Opcode::VCVTTPD2UDQ
                        || instruction.opcode == Opcode::VCVTTPS2UDQ
                    {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    } else {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    }
                    if instruction.prefixes.evex_unchecked().vex().w() {
                        instruction.regs[0].bank = RegisterBank::Y;
                    } else {
                        instruction.regs[0].bank = RegisterBank::Z;
                    }
                    instruction.regs[1].bank = RegisterBank::Z;
                } else {
                    let (r_sz, m_sz) = match (
                        instruction.prefixes.evex_unchecked().vex().l(),
                        instruction.prefixes.evex_unchecked().lp(),
                    ) {
                        (true, true) => {
                            return Err(ErrorKind::InvalidOperand);
                        }
                        (false, true) => (
                            if instruction.prefixes.evex_unchecked().vex().w() {
                                RegisterBank::Y
                            } else {
                                RegisterBank::Z
                            },
                            RegisterBank::Z,
                        ),
                        (true, false) => (
                            if instruction.prefixes.evex_unchecked().vex().w() {
                                RegisterBank::X
                            } else {
                                RegisterBank::Y
                            },
                            RegisterBank::Y,
                        ),
                        (false, false) => (RegisterBank::X, RegisterBank::X),
                    };
                    instruction.regs[0].bank = r_sz;
                    instruction.regs[1].bank = m_sz;
                }
            } else {
                let (r_sz, m_sz) = match (
                    instruction.prefixes.evex_unchecked().vex().l(),
                    instruction.prefixes.evex_unchecked().lp(),
                ) {
                    (true, true) => {
                        return Err(ErrorKind::InvalidOperand);
                    }
                    //          (true, false) => (RegisterBank::Y, 32),
                    (true, false) => (
                        if instruction.prefixes.evex_unchecked().vex().w() {
                            RegisterBank::X
                        } else {
                            RegisterBank::Y
                        },
                        32,
                    ),
                    (false, true) => (
                        if instruction.prefixes.evex_unchecked().vex().w() {
                            RegisterBank::Y
                        } else {
                            RegisterBank::Z
                        },
                        64,
                    ),
                    (false, false) => (RegisterBank::X, 16),
                };
                instruction.regs[0].bank = r_sz;
                if instruction.prefixes.evex_unchecked().vex().w() {
                    apply_broadcast(instruction, 8, m_sz);
                } else {
                    apply_broadcast(instruction, 4, m_sz);
                }
            }

            if instruction.prefixes.evex_unchecked().vex().w()
                && instruction.opcode == Opcode::VCVTDQ2PS
            {
                instruction.opcode = Opcode::VCVTQQ2PS;
            }
        }
        generated::EVEXOperandCode::VCVTTPD2DQ => {
            check_mask_reg(instruction)?;
            deny_vex_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
            instruction.mem_size = 0;

            if instruction.prefixes.evex_unchecked().vex().w()
                && instruction.opcode == Opcode::VCVTTPS2UDQ
            {
                instruction.opcode = Opcode::VCVTTPD2UDQ;
            }

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    if instruction.opcode == Opcode::VCVTDQ2PS
                        || instruction.opcode == Opcode::VCVTPD2DQ
                    {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    } else {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    }
                    if instruction.opcode == Opcode::VCVTDQ2PS
                        && !instruction.prefixes.evex_unchecked().vex().w()
                    {
                        instruction.regs[0].bank = RegisterBank::Z;
                    } else {
                        instruction.regs[0].bank = RegisterBank::Y;
                    }
                    instruction.regs[1].bank = RegisterBank::Z;
                } else {
                    let (r_sz, m_sz) = match (
                        instruction.prefixes.evex_unchecked().vex().l(),
                        instruction.prefixes.evex_unchecked().lp(),
                    ) {
                        (true, true) => {
                            return Err(ErrorKind::InvalidOperand);
                        }
                        (false, true) => (RegisterBank::Y, RegisterBank::Z),
                        (true, false) => (RegisterBank::X, RegisterBank::Y),
                        (false, false) => (RegisterBank::X, RegisterBank::X),
                    };
                    instruction.regs[0].bank = r_sz;
                    instruction.regs[1].bank = m_sz;
                }
            } else {
                let (r_sz, m_sz) = match (
                    instruction.prefixes.evex_unchecked().vex().l(),
                    instruction.prefixes.evex_unchecked().lp(),
                ) {
                    (true, true) => {
                        return Err(ErrorKind::InvalidOperand);
                    }
                    (true, false) => (RegisterBank::X, 32),
                    (false, true) => (RegisterBank::Y, 64),
                    (false, false) => (RegisterBank::X, 16),
                };
                instruction.regs[0].bank = r_sz;
                apply_broadcast(instruction, 8, m_sz);
            }

            if instruction.prefixes.evex_unchecked().vex().w()
                && instruction.opcode == Opcode::VCVTDQ2PS
            {
                instruction.opcode = Opcode::VCVTQQ2PS;
            }
        }
        generated::EVEXOperandCode::Gm_ymm_U_zmm_sae_W1 => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Z)?;
            instruction.regs[0].bank = RegisterBank::Y;
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            }
            instruction.operands[1] = mem_oper;
            if mem_oper != OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            }
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Gm_V_E_xmm_sae_W1 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;

            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;
            set_reg_sizes(instruction, RegisterBank::X);

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    if instruction.opcode == Opcode::VMINSD || instruction.opcode == Opcode::VMAXSD
                    {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    } else {
                        instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                    }
                }
            } else {
                instruction.mem_size = 8;
            }
        }
        generated::EVEXOperandCode::Gm_V_E_xmm_sae => {
            check_mask_reg(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VEXP2SS {
                    instruction.opcode = Opcode::VEXP2SD;
                } else if instruction.opcode == Opcode::VRCP28SS {
                    instruction.opcode = Opcode::VRCP28SD;
                } else if instruction.opcode == Opcode::VRSQRT28SS {
                    instruction.opcode = Opcode::VRSQRT28SD;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;

            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;
            set_reg_sizes(instruction, RegisterBank::X);

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                }
            } else if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.mem_size = 8;
            } else {
                instruction.mem_size = 4;
            }
        }
        generated::EVEXOperandCode::Gm_E_zmm_sae_bcast => {
            deny_vex_reg(instruction)?;
            check_mask_reg(instruction)?;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VEXP2PS {
                    instruction.opcode = Opcode::VEXP2PD;
                } else if instruction.opcode == Opcode::VRCP28PS {
                    instruction.opcode = Opcode::VRCP28PD;
                } else if instruction.opcode == Opcode::VRSQRT28PS {
                    instruction.opcode = Opcode::VRSQRT28PD;
                }
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::Z)?;

            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;

            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
                    set_reg_sizes(instruction, RegisterBank::Z);
                } else {
                    let sz = regs_size(instruction);
                    if sz < 64 {
                        return Err(ErrorKind::InvalidOperand);
                    }
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                    set_reg_sizes_from_ll(instruction)?;
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
                let sz = regs_size(instruction);
                if sz < 64 {
                    return Err(ErrorKind::InvalidOperand);
                }

                if instruction.prefixes.evex_unchecked().vex().w() {
                    apply_broadcast(instruction, 8, sz);
                } else {
                    apply_broadcast(instruction, 4, sz);
                }
                set_reg_sizes_from_ll(instruction)?;
            }
        }
        generated::EVEXOperandCode::Edd_G_xmm_imm8 => {
            deny_vex_reg(instruction)?;
            deny_mask_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            if instruction.prefixes.evex_unchecked().vex().w() {
                if isa_has_qwords() {
                    instruction.opcode = Opcode::VPEXTRQ;
                } else {
                    instruction.opcode = Opcode::VPEXTRD;
                }
                if let OperandSpec::RegMMM = mem_oper {
                    instruction.regs[1].bank = DEFAULT_EVEX_REGISTER_SIZE;
                } else {
                    instruction.mem_size = DEFAULT_EVEX_REGISTER_WIDTH;
                }
            } else {
                instruction.opcode = Opcode::VPEXTRD;
                if let OperandSpec::RegMMM = mem_oper {
                    instruction.regs[1].bank = RegisterBank::D;
                } else {
                    instruction.mem_size = 4;
                }
            }
        }
        generated::EVEXOperandCode::VCVTUSI2SD => {
            deny_mask_reg(instruction)?;
            deny_z(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            instruction.regs[3].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            if mem_oper == OperandSpec::RegMMM {
                if instruction.prefixes.evex_unchecked().vex().w() {
                    instruction.regs[1].bank = DEFAULT_EVEX_REGISTER_SIZE;
                } else {
                    instruction.regs[1].bank = RegisterBank::D;
                }
                if instruction.prefixes.evex_unchecked().vex().w() {
                    if instruction.prefixes.evex_unchecked().broadcast() {
                        if isa_has_qwords() {
                            instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                        }
                    } else if instruction.prefixes.evex_unchecked().lp()
                        || !instruction.prefixes.evex_unchecked().vex().l()
                    {
                        return Err(ErrorKind::InvalidOpcode);
                    }
                }
            } else {
                if instruction.prefixes.evex_unchecked().broadcast() {
                    return Err(ErrorKind::InvalidOpcode);
                }
                if instruction.prefixes.evex_unchecked().vex().w() {
                    instruction.mem_size = DEFAULT_EVEX_REGISTER_WIDTH;
                } else {
                    instruction.mem_size = 4;
                }
            }
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::VEXTRACTPS => {
            deny_vex_reg(instruction)?;
            deny_mask_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            if let OperandSpec::RegMMM = mem_oper {
                instruction.regs[1].bank = RegisterBank::D;
            } else {
                instruction.mem_size = 4;
            }
        }
        generated::EVEXOperandCode::Ewd_G_xmm_imm8 => {
            deny_vex_reg(instruction)?;
            deny_mask_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            if let OperandSpec::RegMMM = mem_oper {
                instruction.regs[1].bank = RegisterBank::D;
            } else {
                instruction.mem_size = 2;
            }
        }
        generated::EVEXOperandCode::Ebd_G_xmm_imm8 => {
            deny_vex_reg(instruction)?;
            deny_mask_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::X;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[2] = OperandSpec::ImmU8;
            instruction.operand_count = 3;

            if let OperandSpec::RegMMM = mem_oper {
                instruction.regs[1].bank = RegisterBank::D;
            } else {
                instruction.mem_size = 1;
            }
        }
        generated::EVEXOperandCode::Gm_V_Ed_xmm_imm8_sae => {
            check_mask_reg(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;

            let item_size = if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VRNDSCALESS {
                    return Err(ErrorKind::InvalidOpcode);
                } else if instruction.opcode == Opcode::VRANGESS {
                    instruction.opcode = Opcode::VRANGESD;
                    8
                } else if instruction.opcode == Opcode::VFPCLASSSS {
                    instruction.opcode = Opcode::VFPCLASSSD;
                    8
                } else if instruction.opcode == Opcode::VREDUCESS {
                    instruction.opcode = Opcode::VREDUCESD;
                    8
                } else if instruction.opcode == Opcode::VFIXUPIMMSS {
                    instruction.opcode = Opcode::VFIXUPIMMSD;
                    8
                } else if instruction.opcode == Opcode::VGETMANTSS {
                    instruction.opcode = Opcode::VGETMANTSD;
                    8
                } else {
                    4
                }
            } else {
                4
            };

            if let OperandSpec::RegMMM = mem_oper {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = item_size;
            }
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            }
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::Gm_V_E_xmm_imm8_sae_W1 => {
            ensure_W(instruction, 1)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if let OperandSpec::RegMMM = mem_oper {
                /* no mem size */
            } else {
                instruction.mem_size = 8;
            }
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
            } else {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            }
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.imm = read_imm_unsigned(words, 1)?;
            instruction.operands[3] = OperandSpec::ImmU8;
            instruction.operand_count = 4;

            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::Gm_V_zmm_M_xmm_W0 => {
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            instruction.regs[0].bank = RegisterBank::Z;
            instruction.regs[3].bank = RegisterBank::Z;
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if let OperandSpec::RegMMM = mem_oper {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.mem_size = 16;
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::Gm_V_M_xmm => {
            check_mask_reg(instruction)?;

            instruction.mem_size = 16;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if mem_oper == OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            }
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::VMOVSD_10 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            if mem_oper == OperandSpec::RegMMM {
                instruction.operands[1] = OperandSpec::RegVex;
                instruction.operands[2] = mem_oper;
                instruction.operand_count = 3;

                instruction.mem_size = 0;
            } else {
                instruction.operands[1] = mem_oper;
                instruction.operand_count = 2;

                instruction.mem_size = 8;
            }
            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::VMOVSD_11 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 1)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = mem_oper.masked();
            if mem_oper == OperandSpec::RegMMM {
                instruction.operands[1] = OperandSpec::RegVex;
                instruction.operands[2] = OperandSpec::RegRRR;
                instruction.operand_count = 3;

                instruction.mem_size = 0;
            } else {
                instruction.operands[1] = OperandSpec::RegRRR;
                instruction.operand_count = 2;

                instruction.mem_size = 8;
            }
            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::VMOVSS_10 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR_maskmerge;
            if mem_oper == OperandSpec::RegMMM {
                instruction.operands[1] = OperandSpec::RegVex;
                instruction.operands[2] = mem_oper;
                instruction.operand_count = 3;

                instruction.mem_size = 0;
            } else {
                instruction.operands[1] = mem_oper;
                instruction.operand_count = 2;

                instruction.mem_size = 4;
            }
            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::VMOVSS_11 => {
            check_mask_reg(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = mem_oper.masked();
            if mem_oper == OperandSpec::RegMMM {
                instruction.operands[1] = OperandSpec::RegVex;
                instruction.operands[2] = OperandSpec::RegRRR;
                instruction.operand_count = 3;

                instruction.mem_size = 0;
            } else {
                instruction.operands[1] = OperandSpec::RegRRR;
                instruction.operand_count = 2;

                instruction.mem_size = 4;
            }
            set_reg_sizes(instruction, RegisterBank::X);
        }
        generated::EVEXOperandCode::VCVTSI2SS => {
            check_mask_reg(instruction)?;
            deny_z(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if instruction.prefixes.evex_unchecked().broadcast() && mem_oper == OperandSpec::RegMMM
            {
                if (!instruction.prefixes.evex_unchecked().vex().w() || !isa_has_qwords())
                    && instruction.opcode == Opcode::VCVTSI2SD
                {
                    instruction.operands[0] = OperandSpec::RegRRR;
                } else {
                    instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
                }
            } else {
                instruction.operands[0] = OperandSpec::RegRRR;
            }
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes(instruction, RegisterBank::X);

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
                if instruction.prefixes.evex_unchecked().vex().w() {
                    instruction.regs[1].bank = DEFAULT_EVEX_REGISTER_SIZE;
                } else {
                    instruction.regs[1].bank = RegisterBank::D;
                }
            } else if instruction.prefixes.evex_unchecked().vex().w() {
                if isa_has_qwords() {
                    instruction.mem_size = 8;
                } else if [Opcode::VCVTSI2SS, Opcode::VCVTSI2SD].contains(&instruction.opcode) {
                    instruction.mem_size = 4;
                } else {
                    instruction.mem_size = 8;
                }
            } else {
                instruction.mem_size = 4;
            }
        }
        generated::EVEXOperandCode::VCVTTSS2SI => {
            check_mask_reg(instruction)?;
            deny_z(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae_noround;
            } else {
                instruction.operands[0] = OperandSpec::RegRRR;
            }
            if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.regs[0].bank = DEFAULT_EVEX_REGISTER_SIZE;
            } else {
                instruction.regs[0].bank = RegisterBank::D;
            }

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 4;
            }

            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::VCVTSS2SI => {
            check_mask_reg(instruction)?;
            deny_z(instruction)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            if instruction.prefixes.evex_unchecked().broadcast() {
                instruction.operands[0] = OperandSpec::RegRRR_maskmerge_sae;
            } else {
                instruction.operands[0] = OperandSpec::RegRRR;
            }
            if instruction.prefixes.evex_unchecked().vex().w() {
                instruction.regs[0].bank = DEFAULT_EVEX_REGISTER_SIZE;
            } else {
                instruction.regs[0].bank = RegisterBank::D;
            }

            if mem_oper == OperandSpec::RegMMM {
                instruction.mem_size = 0;
            } else {
                instruction.mem_size = 4;
            }

            instruction.operands[1] = mem_oper;
            instruction.operand_count = 2;
        }
        generated::EVEXOperandCode::Operands_12_W0 => {
            deny_mask_reg(instruction)?;
            deny_z(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes(instruction, RegisterBank::X);

            if mem_oper == OperandSpec::RegMMM {
                instruction.opcode = Opcode::VMOVHLPS;

                instruction.mem_size = 0;
            } else {
                instruction.opcode = Opcode::VMOVLPS;

                instruction.mem_size = 8;
            }
        }
        generated::EVEXOperandCode::Operands_16_W0 => {
            deny_mask_reg(instruction)?;
            deny_z(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = OperandSpec::RegRRR;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.operands[2] = mem_oper;
            instruction.operand_count = 3;

            set_reg_sizes(instruction, RegisterBank::X);

            if mem_oper == OperandSpec::RegMMM {
                instruction.opcode = Opcode::VMOVLHPS;

                instruction.mem_size = 0;
            } else {
                instruction.opcode = Opcode::VMOVHPS;

                instruction.mem_size = 8;
            }
        }
        generated::EVEXOperandCode::Mq_G_W0 => {
            deny_mask_reg(instruction)?;
            deny_z(instruction)?;
            ensure_W(instruction, 0)?;

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            set_reg_sizes(instruction, RegisterBank::X);

            if mem_oper == OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            } else {
                instruction.mem_size = 8;
            }
        }
        generated::EVEXOperandCode::Mvector_Mask_G_LL => {
            check_mask_reg(instruction)?;
            deny_z(instruction)?;

            // we can't just `deny_vex_reg` because vp is used as bit 5 for the index register
            if instruction.regs[3].num & 0b1111 != 0 {
                return Err(ErrorKind::InvalidOperand);
            }

            let modrm = read_modrm(words)?;
            set_rrr(instruction, modrm);
            let mem_oper = read_E_vex(words, instruction, modrm, RegisterBank::X)?;
            instruction.regs[2].num |= instruction.regs[3].num & 0b10000;
            instruction.operands[0] = mem_oper;
            instruction.operands[1] = OperandSpec::RegVex;
            instruction.regs[3].bank = RegisterBank::K;
            instruction.regs[3].num = instruction.prefixes.evex_unchecked().mask_reg();
            instruction.operands[2] = OperandSpec::RegRRR;
            instruction.operand_count = 2;

            if mem_oper == OperandSpec::RegMMM {
                return Err(ErrorKind::InvalidOperand);
            }

            if instruction.prefixes.evex_unchecked().lp() {
                if instruction.prefixes.evex_unchecked().vex().l() {
                    return Err(ErrorKind::InvalidOperand);
                }
                instruction.regs[0].bank = RegisterBank::Z;
                instruction.regs[2].bank = RegisterBank::Z;
            } else if instruction.prefixes.evex_unchecked().vex().l() {
                instruction.regs[0].bank = RegisterBank::Y;
                instruction.regs[2].bank = RegisterBank::Y;
            } else {
                instruction.regs[0].bank = RegisterBank::X;
                instruction.regs[2].bank = RegisterBank::X;
            }

            if instruction.prefixes.evex_unchecked().vex().w() {
                if instruction.opcode == Opcode::VPSCATTERDD {
                    instruction.opcode = Opcode::VPSCATTERDQ;
                } else if instruction.opcode == Opcode::VPSCATTERQD {
                    instruction.opcode = Opcode::VPSCATTERQQ;
                }
                instruction.mem_size = 8;
            } else {
                instruction.mem_size = 4;
            }
            instruction.operand_count = 3;
        }
        generated::EVEXOperandCode::Nothing => {}
    }
    Ok(())
}
