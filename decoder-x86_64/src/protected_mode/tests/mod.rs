#![cfg(test)]

mod evex_generated;
mod opcode;
mod operand;
mod regspec;

use std::fmt::Write;

use crate::protected_mode::Decoder;
use decoder::{Decodable, Decoded, Reader, ToTokens};

fn test_invalid(data: &[u8]) {
    test_invalid_under(&Decoder::default(), data);
}

fn test_invalid_under(decoder: &Decoder, data: &[u8]) {
    let mut reader = Reader::new(data);
    if let Ok(inst) = decoder.decode(&mut reader) {
        // realistically, the chances an error only shows up under non-fmt builds seems unlikely,
        // but try to report *something* in such cases.
        panic!(
            "decoded {:?} from {:02x?} under decoder {}",
            inst.opcode(),
            data,
            decoder
        );
    }
}

fn test_display(data: &[u8], expected: &'static str) {
    test_display_under(&Decoder::default(), data, expected);
}

fn test_display_under(dekoder: &Decoder, data: &[u8], expected: &'static str) {
    let mut stream = decoder::TokenStream::new();
    let mut hex = String::new();
    for b in data {
        write!(hex, "{:02x}", b).unwrap();
    }

    let mut reader = Reader::new(data);
    let symbols = debugvault::Index::default();
    match dekoder.decode(&mut reader) {
        Ok(instr) => {
            instr.tokenize(&mut stream, &symbols);
            let text = stream.to_string();

            assert!(
                text == expected,
                "display error for {}:\n  decoded: {:?} under decoder {}\n displayed: {}\n expected: {}\n",
                hex,
                instr,
                dekoder,
                text,
                expected
            );
            // while we're at it, test that the instruction is as long, and no longer, than its
            // input
            assert_eq!(
                (0u32.wrapping_add(instr.width() as u32)) as usize,
                data.len(),
                "instruction length is incorrect, wanted instruction {}",
                expected
            );
        }
        Err(e) => {
            assert!(
                false,
                "decode error ({:?}) for {} under decoder {}:\n  expected: {}\n",
                e, hex, dekoder, expected
            );
        }
    }
}

#[test]
fn modrm_decode() {
    // just modrm
    test_display(&[0x33, 0x08], "xor ecx, dword [eax]");
    test_display(&[0x33, 0x20], "xor esp, dword [eax]");
    test_display(
        &[0x33, 0x05, 0x78, 0x56, 0x34, 0x12],
        "xor eax, dword [0x12345678]",
    );
    test_display(&[0x33, 0x41, 0x23], "xor eax, dword [ecx + 0x23]");
    test_display(
        &[0x33, 0x81, 0x23, 0x01, 0x65, 0x43],
        "xor eax, dword [ecx + 0x43650123]",
    );
    test_display(&[0x33, 0xc1], "xor eax, ecx");

    // sib
    test_display(&[0x33, 0x04, 0x0a], "xor eax, dword [edx + ecx * 1]");
    test_display(&[0x33, 0x04, 0x4a], "xor eax, dword [edx + ecx * 2]");
    test_display(&[0x33, 0x04, 0x8a], "xor eax, dword [edx + ecx * 4]");
    test_display(&[0x33, 0x04, 0xca], "xor eax, dword [edx + ecx * 8]");
    test_display(&[0x33, 0x04, 0x20], "xor eax, dword [eax]");
    test_display(&[0x33, 0x04, 0x60], "xor eax, dword [eax]");
    test_display(&[0x33, 0x04, 0xa0], "xor eax, dword [eax]");
    test_display(&[0x33, 0x04, 0xe0], "xor eax, dword [eax]");
    test_display(
        &[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44],
        "xor eax, dword [0x44332211]",
    );

    test_display(&[0x33, 0x44, 0x65, 0x11], "xor eax, dword [ebp + 0x11]");
    test_display(
        &[0x33, 0x84, 0xa5, 0x11, 0x22, 0x33, 0x44],
        "xor eax, dword [ebp + 0x44332211]",
    );
    test_display(
        &[0x33, 0x04, 0xe5, 0x11, 0x22, 0x33, 0x44],
        "xor eax, dword [0x44332211]",
    );

    // specifically sib with base == 0b101
    // mod bits 00
    test_display(
        &[0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50],
        "xor esi, dword [0x50403020]",
    );
    // mod bits 01
    test_display(&[0x33, 0x74, 0x25, 0x20], "xor esi, dword [ebp + 0x20]");
    // mod bits 10
    test_display(
        &[0x33, 0xb4, 0x25, 0x20, 0x30, 0x40, 0x50],
        "xor esi, dword [ebp + 0x50403020]",
    );
}

#[test]
fn mmx() {
    test_display(&[0x0f, 0xf7, 0xc1], "maskmovq mm0, mm1");
    test_invalid(&[0x0f, 0xf7, 0x01]);

    test_display(&[0x0f, 0xe7, 0x03], "movntq qword [ebx], mm0");
    test_invalid(&[0x0f, 0xe7, 0xc3]);

    test_invalid(&[0x66, 0x0f, 0xc3, 0x03]);
    test_display(&[0x0f, 0xc3, 0x03], "movnti dword [ebx], eax");
    test_invalid(&[0x0f, 0xc3, 0xc3]);

    test_display(&[0x0f, 0x7e, 0xcf], "movd edi, mm1");
    test_display(&[0x0f, 0x7f, 0xcf], "movq mm7, mm1");
    test_display(&[0x0f, 0x7f, 0x0f], "movq qword [edi], mm1");
    test_display(&[0x0f, 0xc4, 0xc0, 0x14], "pinsrw mm0, eax, 0x14");
    test_display(&[0x0f, 0xc4, 0x00, 0x14], "pinsrw mm0, word [eax], 0x14");
    test_display(&[0x0f, 0xd1, 0xcf], "psrlw mm1, mm7");
    test_display(&[0x0f, 0xd1, 0x00], "psrlw mm0, qword [eax]");
    test_invalid(&[0x0f, 0xd7, 0x00]);
    test_display(&[0x0f, 0xd7, 0xcf], "pmovmskb ecx, mm7");
    test_display(&[0x0f, 0x3a, 0x0f, 0xc1, 0x23], "palignr mm0, mm1, 0x23");
    test_display(&[0x0f, 0xf9, 0xc2], "psubw mm0, mm2");
    test_display(&[0x0f, 0xfd, 0xd2], "paddw mm2, mm2");
    test_display(&[0x0f, 0x6f, 0xe9], "movq mm5, mm1");
    test_display(
        &[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77],
        "pmulhw mm7, qword [0x77ccbbaa]",
    );

    test_display(&[0x0f, 0x38, 0x00, 0xda], "pshufb mm3, mm2");

    test_display(&[0x0f, 0x74, 0xc2], "pcmpeqb mm0, mm2");
    test_display(&[0x0f, 0x75, 0xc2], "pcmpeqw mm0, mm2");
    test_display(&[0x0f, 0x76, 0xc2], "pcmpeqd mm0, mm2");

    test_display(&[0x66, 0x0f, 0xc5, 0xd8, 0xff], "pextrw ebx, xmm0, 0xff");
    test_invalid(&[0x66, 0x0f, 0xc5, 0x08, 0xff]);

    test_display(&[0x0f, 0xc5, 0xd1, 0x00], "pextrw edx, mm1, 0x0");
    test_invalid(&[0x0f, 0xc5, 0x01, 0x00]);

    test_display(&[0x0f, 0xd8, 0xc2], "psubusb mm0, mm2");
    test_display(&[0x0f, 0xd9, 0xc2], "psubusw mm0, mm2");
    test_display(&[0x0f, 0xda, 0xc2], "pminub mm0, mm2");
    test_display(&[0x0f, 0xdb, 0xc2], "pand mm0, mm2");
    test_display(&[0x0f, 0xdc, 0xc2], "paddusb mm0, mm2");
    test_display(&[0x0f, 0xdd, 0xc2], "paddusw mm0, mm2");
    test_display(&[0x0f, 0xde, 0xc2], "pmaxub mm0, mm2");
    test_display(&[0x0f, 0xdf, 0xc2], "pandn mm0, mm2");

    test_display(&[0x0f, 0xe8, 0xc2], "psubsb mm0, mm2");
    test_display(&[0x0f, 0xe9, 0xc2], "psubsw mm0, mm2");
    test_display(&[0x0f, 0xea, 0xc2], "pminsw mm0, mm2");
    test_display(&[0x0f, 0xeb, 0xc2], "por mm0, mm2");
    test_display(&[0x0f, 0xec, 0xc2], "paddsb mm0, mm2");
    test_display(&[0x0f, 0xed, 0xc2], "paddsw mm0, mm2");
    test_display(&[0x0f, 0xee, 0xc2], "pmaxsw mm0, mm2");
    test_display(&[0x0f, 0xef, 0xc2], "pxor mm0, mm2");

    test_invalid(&[0x0f, 0xf0, 0xc2]);
    test_display(&[0x0f, 0xf1, 0xc2], "psllw mm0, mm2");
    test_display(&[0x0f, 0xf2, 0xc2], "pslld mm0, mm2");
    test_display(&[0x0f, 0xf3, 0xc2], "psllq mm0, mm2");
    test_display(&[0x0f, 0xf4, 0xc2], "pmuludq mm0, mm2");
    test_display(&[0x0f, 0xf5, 0xc2], "pmaddwd mm0, mm2");
    test_display(&[0x0f, 0xf6, 0xc2], "psadbw mm0, mm2");
    test_display(&[0x0f, 0xf8, 0xc2], "psubb mm0, mm2");
    test_display(&[0x0f, 0xf9, 0xc2], "psubw mm0, mm2");
    test_display(&[0x0f, 0xfa, 0xc2], "psubd mm0, mm2");
    test_display(&[0x0f, 0xfb, 0xc2], "psubq mm0, mm2");
    test_display(&[0x0f, 0xfc, 0xc2], "paddb mm0, mm2");
    test_display(&[0x0f, 0xfc, 0x02], "paddb mm0, qword [edx]");
    test_display(&[0x0f, 0xfd, 0xc2], "paddw mm0, mm2");
    test_display(&[0x0f, 0xfe, 0xc2], "paddd mm0, mm2");

    test_display(&[0x0f, 0xf1, 0x02], "psllw mm0, qword [edx]");
    test_display(&[0x0f, 0xf2, 0x02], "pslld mm0, qword [edx]");
    test_display(&[0x0f, 0xf3, 0x02], "psllq mm0, qword [edx]");
    test_display(&[0x0f, 0xf4, 0x02], "pmuludq mm0, qword [edx]");
    test_display(&[0x0f, 0xf5, 0x02], "pmaddwd mm0, qword [edx]");
    test_display(&[0x0f, 0xf6, 0x02], "psadbw mm0, qword [edx]");
    test_display(&[0x0f, 0xf8, 0x02], "psubb mm0, qword [edx]");
    test_display(&[0x0f, 0xf9, 0x02], "psubw mm0, qword [edx]");
    test_display(&[0x0f, 0xfa, 0x02], "psubd mm0, qword [edx]");
    test_display(&[0x0f, 0xfb, 0x02], "psubq mm0, qword [edx]");
    test_display(&[0x0f, 0xfc, 0x02], "paddb mm0, qword [edx]");
    test_display(&[0x0f, 0xfd, 0x02], "paddw mm0, qword [edx]");
    test_display(&[0x0f, 0xfe, 0x02], "paddd mm0, qword [edx]");
}

#[test]
fn cvt() {
    test_display(&[0x0f, 0x2c, 0xcf], "cvttps2pi mm1, xmm7");
    test_display(&[0x0f, 0x2a, 0xcf], "cvtpi2ps xmm1, mm7");
    test_display(&[0x0f, 0x2a, 0x00], "cvtpi2ps xmm0, qword [eax]");
    test_display(&[0x66, 0x0f, 0x2a, 0x00], "cvtpi2pd xmm0, qword [eax]");
    test_display(&[0x66, 0x0f, 0x2a, 0xcf], "cvtpi2pd xmm1, mm7");
    test_display(&[0xf2, 0x0f, 0x2a, 0x00], "cvtsi2sd xmm0, dword [eax]");
    test_display(&[0xf2, 0x0f, 0x2a, 0xcf], "cvtsi2sd xmm1, edi");
    test_display(&[0xf3, 0x0f, 0x2a, 0x00], "cvtsi2ss xmm0, dword [eax]");
    test_display(&[0xf3, 0x0f, 0x2a, 0xcf], "cvtsi2ss xmm1, edi");
}

#[test]
fn aesni() {
    fn test_instr(bytes: &[u8], text: &'static str) {
        test_display_under(&Decoder::minimal().with_aesni(), bytes, text);
        test_display_under(&Decoder::default(), bytes, text);
        test_invalid_under(&Decoder::minimal(), bytes);
    }

    test_instr(
        &[0x66, 0x0f, 0x38, 0xdb, 0x0f],
        "aesimc xmm1, xmmword [edi]",
    );
    test_instr(
        &[0x67, 0x66, 0x0f, 0x38, 0xdb, 0x0f],
        "aesimc xmm1, xmmword [bx]",
    );

    test_instr(
        &[0x66, 0x0f, 0x38, 0xdc, 0x0f],
        "aesenc xmm1, xmmword [edi]",
    );
    test_instr(
        &[0x67, 0x66, 0x0f, 0x38, 0xdc, 0x0f],
        "aesenc xmm1, xmmword [bx]",
    );

    test_instr(
        &[0x66, 0x0f, 0x38, 0xdd, 0x0f],
        "aesenclast xmm1, xmmword [edi]",
    );
    test_instr(
        &[0x67, 0x66, 0x0f, 0x38, 0xdd, 0x0f],
        "aesenclast xmm1, xmmword [bx]",
    );

    test_instr(
        &[0x66, 0x0f, 0x38, 0xde, 0x0f],
        "aesdec xmm1, xmmword [edi]",
    );
    test_instr(
        &[0x67, 0x66, 0x0f, 0x38, 0xde, 0x0f],
        "aesdec xmm1, xmmword [bx]",
    );

    test_instr(
        &[0x66, 0x0f, 0x38, 0xdf, 0x0f],
        "aesdeclast xmm1, xmmword [edi]",
    );
    test_instr(
        &[0x67, 0x66, 0x0f, 0x38, 0xdf, 0x0f],
        "aesdeclast xmm1, xmmword [bx]",
    );

    test_instr(
        &[0x66, 0x0f, 0x3a, 0xdf, 0x0f, 0xaa],
        "aeskeygenassist xmm1, xmmword [edi], 0xaa",
    );
}

#[test]
fn sse2() {
    fn test_instr(bytes: &[u8], text: &'static str) {
        // sse and sse2 are part of amd64, so x86_64, meaning even the minimal decoder must support
        // them.
        test_display_under(&Decoder::minimal(), bytes, text);
    }

    test_instr(
        &[0xf2, 0x0f, 0x10, 0x0c, 0xc7],
        "movsd xmm1, qword [edi + eax * 8]",
    );
    test_instr(
        &[0xf2, 0x0f, 0x11, 0x0c, 0xc7],
        "movsd qword [edi + eax * 8], xmm1",
    );
    test_instr(
        &[0x66, 0x0f, 0x11, 0x0c, 0xc7],
        "movupd xmmword [edi + eax * 8], xmm1",
    );
    test_instr(&[0x66, 0x0f, 0x12, 0x03], "movlpd xmm0, qword [ebx]"); // reg-mem is movlpd
    test_instr(&[0x66, 0x0f, 0x13, 0x03], "movlpd qword [ebx], xmm0");
    test_invalid(&[0x66, 0x0f, 0x13, 0xc3]);
    test_instr(&[0x66, 0x0f, 0x14, 0x03], "unpcklpd xmm0, xmmword [ebx]");
    test_instr(&[0x66, 0x0f, 0x14, 0xc3], "unpcklpd xmm0, xmm3");
    test_instr(&[0x66, 0x0f, 0x15, 0x03], "unpckhpd xmm0, xmmword [ebx]");
    test_instr(&[0x66, 0x0f, 0x15, 0xc3], "unpckhpd xmm0, xmm3");
    test_instr(&[0x66, 0x0f, 0x16, 0x03], "movhpd xmm0, qword [ebx]");
    test_invalid(&[0x66, 0x0f, 0x16, 0xc3]);
    test_instr(&[0x66, 0x0f, 0x17, 0x03], "movhpd qword [ebx], xmm0");
    test_invalid(&[0x66, 0x0f, 0x17, 0xc3]);

    test_instr(&[0x66, 0x0f, 0x28, 0xd0], "movapd xmm2, xmm0");
    test_instr(&[0x66, 0x0f, 0x28, 0x00], "movapd xmm0, xmmword [eax]");

    test_instr(&[0x66, 0x0f, 0x2a, 0xcf], "cvtpi2pd xmm1, mm7");
    test_instr(&[0x66, 0x0f, 0x2a, 0x0f], "cvtpi2pd xmm1, qword [edi]");
    test_instr(&[0xf2, 0x0f, 0x2a, 0xcf], "cvtsi2sd xmm1, edi");
    test_instr(&[0xf2, 0x0f, 0x2a, 0x0f], "cvtsi2sd xmm1, dword [edi]");
    test_instr(&[0x66, 0x0f, 0x2b, 0x0f], "movntpd xmmword [edi], xmm1");
    test_instr(&[0x66, 0x0f, 0x2c, 0xcf], "cvttpd2pi mm1, xmm7");
    test_instr(&[0x66, 0x0f, 0x2c, 0x0f], "cvttpd2pi mm1, xmmword [edi]");
    test_instr(&[0xf2, 0x0f, 0x2c, 0xcf], "cvttsd2si ecx, xmm7");
    test_instr(&[0xf2, 0x0f, 0x2c, 0x0f], "cvttsd2si ecx, qword [edi]");
    test_instr(&[0x66, 0x0f, 0x2d, 0xcf], "cvtpd2pi mm1, xmm7");
    test_instr(&[0x66, 0x0f, 0x2d, 0x0f], "cvtpd2pi mm1, xmmword [edi]");
    test_instr(&[0xf2, 0x0f, 0x2d, 0xcf], "cvtsd2si ecx, xmm7");
    test_instr(&[0xf2, 0x0f, 0x2d, 0x0f], "cvtsd2si ecx, qword [edi]");
    test_instr(&[0x66, 0x0f, 0x2e, 0xcf], "ucomisd xmm1, xmm7");
    test_instr(&[0x66, 0x0f, 0x2e, 0x0f], "ucomisd xmm1, qword [edi]");
    test_instr(&[0x66, 0x0f, 0x2f, 0xcf], "comisd xmm1, xmm7");
    test_instr(&[0x66, 0x0f, 0x2f, 0x0f], "comisd xmm1, qword [edi]");

    /*
     * .... 660f38
     * .... 660f7f
     */

    test_invalid(&[0x66, 0x0f, 0x50, 0x01]);
    test_instr(&[0x66, 0x0f, 0x50, 0xc1], "movmskpd eax, xmm1");
    test_instr(&[0x66, 0x0f, 0x51, 0x01], "sqrtpd xmm0, xmmword [ecx]");
    test_instr(&[0xf2, 0x0f, 0x51, 0x01], "sqrtsd xmm0, qword [ecx]");
    test_invalid(&[0x66, 0x0f, 0x52, 0x01]);
    test_invalid(&[0x66, 0x0f, 0x53, 0x01]);
    test_instr(&[0x66, 0x0f, 0x54, 0x01], "andpd xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0x55, 0x01], "andnpd xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0x56, 0x01], "orpd xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0x57, 0x01], "xorpd xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0x58, 0x01], "addpd xmm0, xmmword [ecx]");
    test_instr(&[0xf2, 0x0f, 0x58, 0x01], "addsd xmm0, qword [ecx]");
    test_instr(&[0x66, 0x0f, 0x59, 0x01], "mulpd xmm0, xmmword [ecx]");
    test_instr(&[0xf2, 0x0f, 0x59, 0x01], "mulsd xmm0, qword [ecx]");
    test_instr(&[0x66, 0x0f, 0x5a, 0x01], "cvtpd2ps xmm0, xmmword [ecx]");
    test_instr(&[0xf2, 0x0f, 0x5a, 0x01], "cvtsd2ss xmm0, qword [ecx]");
    test_instr(&[0x66, 0x0f, 0x5b, 0x01], "cvtps2dq xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0x5c, 0x01], "subpd xmm0, xmmword [ecx]");
    test_instr(&[0xf2, 0x0f, 0x5c, 0x01], "subsd xmm0, qword [ecx]");
    test_instr(&[0x66, 0x0f, 0x5d, 0x01], "minpd xmm0, xmmword [ecx]");
    test_instr(&[0xf2, 0x0f, 0x5d, 0x01], "minsd xmm0, qword [ecx]");
    test_instr(&[0x66, 0x0f, 0x5e, 0x01], "divpd xmm0, xmmword [ecx]");
    test_instr(&[0xf2, 0x0f, 0x5e, 0x01], "divsd xmm0, qword [ecx]");
    test_instr(&[0x66, 0x0f, 0x5f, 0x01], "maxpd xmm0, xmmword [ecx]");
    test_instr(&[0xf2, 0x0f, 0x5f, 0x01], "maxsd xmm0, qword [ecx]");
    test_instr(
        &[0x66, 0x0f, 0x60, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "punpcklbw xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x61, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "punpcklwd xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x62, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "punpckldq xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x63, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "packsswb xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x64, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "pcmpgtb xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x65, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "pcmpgtw xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x66, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "pcmpgtd xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x67, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "packuswb xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x68, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "punpckhbw xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x69, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "punpckhwd xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x6a, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "punpckhdq xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x6b, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "packssdw xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x6c, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "punpcklqdq xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x6d, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "punpckhqdq xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x6e, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "movd xmm3, dword [esp + ebx * 4 - 0x334455cc]",
    );
    test_instr(
        &[0x66, 0x0f, 0x6f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "movdqa xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );

    test_instr(&[0x66, 0x0f, 0x6e, 0xc0], "movd xmm0, eax");
    test_instr(&[0x66, 0x0f, 0x70, 0xc0, 0x4e], "pshufd xmm0, xmm0, 0x4e");
    test_instr(&[0xf2, 0x0f, 0x70, 0xc0, 0x4e], "pshuflw xmm0, xmm0, 0x4e");
    test_instr(&[0xf3, 0x0f, 0x70, 0xc0, 0x4e], "pshufhw xmm0, xmm0, 0x4e");
    test_invalid(&[0x66, 0x0f, 0x71, 0x10, 0x8f]);
    test_instr(&[0x66, 0x0f, 0x71, 0xd0, 0x8f], "psrlw xmm0, 0x8f");
    test_invalid(&[0x66, 0x0f, 0x71, 0x20, 0x8f]);
    test_instr(&[0x66, 0x0f, 0x71, 0xe0, 0x8f], "psraw xmm0, 0x8f");
    test_invalid(&[0x66, 0x0f, 0x71, 0x30, 0x8f]);
    test_instr(&[0x66, 0x0f, 0x71, 0xf0, 0x8f], "psllw xmm0, 0x8f");
    test_invalid(&[0x66, 0x0f, 0x72, 0x10, 0x8f]);
    test_instr(&[0x66, 0x0f, 0x72, 0xd0, 0x8f], "psrld xmm0, 0x8f");
    test_invalid(&[0x66, 0x0f, 0x72, 0x20, 0x8f]);
    test_instr(&[0x66, 0x0f, 0x72, 0xe0, 0x8f], "psrad xmm0, 0x8f");
    test_invalid(&[0x66, 0x0f, 0x72, 0x30, 0x8f]);
    test_instr(&[0x66, 0x0f, 0x72, 0xf0, 0x8f], "pslld xmm0, 0x8f");
    test_invalid(&[0x66, 0x0f, 0x73, 0x10, 0x8f]);
    test_invalid(&[0x66, 0x0f, 0x73, 0x18, 0x8f]);
    test_instr(&[0x66, 0x0f, 0x73, 0xd0, 0x8f], "psrlq xmm0, 0x8f");
    test_instr(&[0x66, 0x0f, 0x73, 0xd8, 0x8f], "psrldq xmm0, 0x8f");
    test_invalid(&[0x66, 0x0f, 0x73, 0x30, 0x8f]);
    test_invalid(&[0x66, 0x0f, 0x73, 0x38, 0x8f]);
    test_instr(&[0x66, 0x0f, 0x73, 0xf0, 0x8f], "psllq xmm0, 0x8f");
    test_instr(&[0x66, 0x0f, 0x73, 0xf8, 0x8f], "pslldq xmm0, 0x8f");
    test_instr(&[0x66, 0x0f, 0x7e, 0xc1], "movd ecx, xmm0");
    test_instr(&[0x66, 0x0f, 0x7e, 0x01], "movd dword [ecx], xmm0");
    test_instr(
        &[0x66, 0x0f, 0x7f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "movdqa xmmword [esp + ebx * 4 - 0x334455cc], xmm3",
    );

    test_instr(&[0x66, 0x0f, 0xc2, 0xc3, 0x08], "cmppd xmm0, xmm3, 0x8");
    test_instr(
        &[0x66, 0x0f, 0xc2, 0x03, 0x08],
        "cmppd xmm0, xmmword [ebx], 0x8",
    );
    test_instr(&[0xf2, 0x0f, 0xc2, 0xc3, 0x08], "cmpsd xmm0, xmm3, 0x8");
    test_instr(
        &[0xf2, 0x0f, 0xc2, 0x03, 0x08],
        "cmpsd xmm0, qword [ebx], 0x8",
    );

    test_instr(&[0x66, 0x0f, 0xc4, 0xc3, 0x08], "pinsrw xmm0, ebx, 0x8");
    test_instr(
        &[0x66, 0x0f, 0xc4, 0x03, 0x08],
        "pinsrw xmm0, word [ebx], 0x8",
    );

    //    test_instr(&[0x66, 0x0f, 0xc5, 0xc3, 0x08], "pextrw eax, xmm3, 0x8");
    //    test_instr_invalid(&[0x66, 0x0f, 0xc5, 0x03, 0x08]);
    //    test_instr_invalid(&[0x66, 0x0f, 0xc5, 0x40, 0x08]);
    //    test_instr_invalid(&[0x66, 0x0f, 0xc5, 0x80, 0x08]);

    test_instr(
        &[0x66, 0x0f, 0xc6, 0x03, 0x08],
        "shufpd xmm0, xmmword [ebx], 0x8",
    );
    test_instr(&[0x66, 0x0f, 0xc6, 0xc3, 0x08], "shufpd xmm0, xmm3, 0x8");
    test_instr(&[0x66, 0x0f, 0xd1, 0xc1], "psrlw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xd1, 0x01], "psrlw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xd2, 0xc1], "psrld xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xd2, 0x01], "psrld xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xd3, 0xc1], "psrlq xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xd3, 0x01], "psrlq xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xd4, 0xc1], "paddq xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xd4, 0x01], "paddq xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xd5, 0xc1], "pmullw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xd5, 0x01], "pmullw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xd6, 0xc1], "movq xmm1, xmm0");
    test_instr(&[0x66, 0x0f, 0xd6, 0x01], "movq qword [ecx], xmm0");
    test_invalid(&[0xf3, 0x0f, 0xd6, 0x03]);
    test_instr(&[0xf3, 0x0f, 0xd6, 0xc3], "movq2dq xmm0, mm3");
    test_instr(&[0xf2, 0x0f, 0xd6, 0xc3], "movdq2q mm0, xmm3");
    test_instr(&[0x66, 0x0f, 0xd7, 0xc1], "pmovmskb eax, xmm1");
    test_invalid(&[0x66, 0x0f, 0xd7, 0x01]);
    test_instr(&[0x66, 0x0f, 0xd8, 0xc1], "psubusb xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xd8, 0x01], "psubusb xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xd9, 0xc1], "psubusw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xd9, 0x01], "psubusw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xda, 0xc1], "pminub xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xda, 0x01], "pminub xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xdb, 0xc1], "pand xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xdb, 0x01], "pand xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xdc, 0xc1], "paddusb xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xdc, 0x01], "paddusb xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xdd, 0xc1], "paddusw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xdd, 0x01], "paddusw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xde, 0xc1], "pmaxub xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xde, 0x01], "pmaxub xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xdf, 0xc1], "pandn xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xdf, 0x01], "pandn xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xe0, 0xc1], "pavgb xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xe0, 0x01], "pavgb xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xe1, 0xc1], "psraw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xe1, 0x01], "psraw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xe2, 0xc1], "psrad xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xe2, 0x01], "psrad xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xe3, 0xc1], "pavgw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xe3, 0x01], "pavgw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xe4, 0xc1], "pmulhuw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xe4, 0x01], "pmulhuw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xe5, 0xc1], "pmulhw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xe5, 0x01], "pmulhw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xe6, 0xc1], "cvttpd2dq xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xe6, 0x01], "cvttpd2dq xmm0, xmmword [ecx]");
    test_invalid(&[0x66, 0x0f, 0xe7, 0xc1]);
    test_instr(&[0x66, 0x0f, 0xe7, 0x01], "movntdq xmmword [ecx], xmm0");
    test_instr(&[0x66, 0x0f, 0xe8, 0xc1], "psubsb xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xe8, 0x01], "psubsb xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xe9, 0xc1], "psubsw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xe9, 0x01], "psubsw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xea, 0xc1], "pminsw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xea, 0x01], "pminsw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xeb, 0xc3], "por xmm0, xmm3");
    test_instr(&[0x66, 0x0f, 0xeb, 0xc4], "por xmm0, xmm4");
    test_instr(&[0x66, 0x0f, 0xeb, 0xd3], "por xmm2, xmm3");
    test_instr(&[0x66, 0x0f, 0xeb, 0x12], "por xmm2, xmmword [edx]");
    test_instr(&[0x66, 0x0f, 0xeb, 0xc1], "por xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xeb, 0x01], "por xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xec, 0xc1], "paddsb xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xec, 0x01], "paddsb xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xed, 0xc1], "paddsw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xed, 0x01], "paddsw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xee, 0xc1], "pmaxsw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xee, 0x01], "pmaxsw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xef, 0xc1], "pxor xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xef, 0x01], "pxor xmm0, xmmword [ecx]");
    test_invalid(&[0x66, 0x0f, 0xf0, 0xc1]);
    test_invalid(&[0x66, 0x0f, 0xf0, 0x01]);
    test_instr(&[0x66, 0x0f, 0xf1, 0xc1], "psllw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xf1, 0x01], "psllw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xf2, 0xc1], "pslld xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xf2, 0x01], "pslld xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xf3, 0xc1], "psllq xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xf3, 0x01], "psllq xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xf4, 0xc1], "pmuludq xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xf4, 0x01], "pmuludq xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xf5, 0xc1], "pmaddwd xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xf5, 0x01], "pmaddwd xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xf6, 0xc1], "psadbw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xf6, 0x01], "psadbw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xf7, 0xc1], "maskmovdqu xmm0, xmm1");
    test_invalid(&[0x66, 0x0f, 0xf7, 0x01]);
    test_instr(&[0x66, 0x0f, 0xf8, 0xc1], "psubb xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xf8, 0x01], "psubb xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xf9, 0xc1], "psubw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xf9, 0x01], "psubw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xfa, 0xc1], "psubd xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xfa, 0x01], "psubd xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xfb, 0xc1], "psubq xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xfb, 0x01], "psubq xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xfc, 0xc1], "paddb xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xfc, 0x01], "paddb xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xfd, 0xc1], "paddw xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xfd, 0x01], "paddw xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xfe, 0xc1], "paddd xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0xfe, 0x01], "paddd xmm0, xmmword [ecx]");
    test_instr(&[0x66, 0x0f, 0xff, 0xc1], "ud0 eax, ecx");
    test_instr(&[0xf2, 0x0f, 0xff, 0xc1], "ud0 eax, ecx");
    test_instr(&[0xf3, 0x0f, 0xff, 0xc1], "ud0 eax, ecx");
    test_instr(&[0x66, 0x0f, 0xff, 0x01], "ud0 eax, word [ecx]");

    test_instr(&[0x66, 0x0f, 0x74, 0xc1], "pcmpeqb xmm0, xmm1");
    test_instr(&[0x66, 0x0f, 0x74, 0x12], "pcmpeqb xmm2, xmmword [edx]");
    test_instr(&[0x66, 0x0f, 0xf8, 0xc8], "psubb xmm1, xmm0");
    test_instr(&[0x66, 0x0f, 0xf8, 0xd0], "psubb xmm2, xmm0");
    test_instr(&[0x66, 0x0f, 0xf8, 0x12], "psubb xmm2, xmmword [edx]");
}

#[test]
fn sse3() {
    fn test_instr(bytes: &[u8], text: &'static str) {
        test_display_under(&Decoder::minimal().with_sse3(), bytes, text);
        test_invalid_under(&Decoder::minimal(), bytes);
        // avx doesn't imply older instructions are necessarily valid
        test_invalid_under(&Decoder::minimal().with_avx(), bytes);
        // sse4 doesn't imply older instructions are necessarily valid
        test_invalid_under(&Decoder::minimal().with_sse4_1(), bytes);
        test_invalid_under(&Decoder::minimal().with_sse4_2(), bytes);
    }

    fn test_instr_invalid(bytes: &[u8]) {
        test_invalid_under(&Decoder::minimal().with_sse3(), bytes);
        test_invalid_under(&Decoder::default(), bytes);
    }
    test_instr(&[0xf2, 0x0f, 0xf0, 0x0f], "lddqu xmm1, xmmword [edi]");
    test_instr_invalid(&[0xf2, 0x0f, 0xf0, 0xcf]);
    test_instr(&[0xf2, 0x0f, 0xd0, 0x0f], "addsubps xmm1, xmmword [edi]");
    test_instr(&[0xf2, 0x0f, 0xd0, 0xcf], "addsubps xmm1, xmm7");
    test_invalid(&[0xf3, 0x0f, 0xd0, 0x0f]);
    test_instr(&[0x66, 0x0f, 0xd0, 0x0f], "addsubpd xmm1, xmmword [edi]");
    test_instr(&[0x66, 0x0f, 0xd0, 0xcf], "addsubpd xmm1, xmm7");

    test_instr(&[0xf2, 0x0f, 0x7c, 0x0f], "haddps xmm1, xmmword [edi]");
    test_instr(&[0xf2, 0x0f, 0x7c, 0xcf], "haddps xmm1, xmm7");
    test_instr(&[0x66, 0x0f, 0x7c, 0x0f], "haddpd xmm1, xmmword [edi]");
    test_instr(&[0x66, 0x0f, 0x7c, 0xcf], "haddpd xmm1, xmm7");

    test_instr(&[0xf2, 0x0f, 0x7d, 0x0f], "hsubps xmm1, xmmword [edi]");
    test_instr(&[0xf2, 0x0f, 0x7d, 0xcf], "hsubps xmm1, xmm7");
    test_instr(&[0x66, 0x0f, 0x7d, 0x0f], "hsubpd xmm1, xmmword [edi]");
    test_instr(&[0x66, 0x0f, 0x7d, 0xcf], "hsubpd xmm1, xmm7");

    test_instr(&[0xf3, 0x0f, 0x12, 0x0f], "movsldup xmm1, xmmword [edi]");
    test_instr(&[0xf3, 0x0f, 0x12, 0xcf], "movsldup xmm1, xmm7");
    test_instr(&[0xf3, 0x0f, 0x16, 0x0f], "movshdup xmm1, xmmword [edi]");
    test_instr(&[0xf3, 0x0f, 0x16, 0xcf], "movshdup xmm1, xmm7");

    test_instr(&[0xf2, 0x0f, 0x12, 0x0f], "movddup xmm1, qword [edi]");
    test_instr(&[0xf2, 0x0f, 0x12, 0xcf], "movddup xmm1, xmm7");

    test_instr(&[0x0f, 0x01, 0xc8], "monitor");
    test_invalid(&[0x66, 0x0f, 0x01, 0xc8]);
    test_invalid(&[0xf3, 0x0f, 0x01, 0xc8]);
    test_invalid(&[0xf2, 0x0f, 0x01, 0xc8]);

    test_instr(&[0x0f, 0x01, 0xc9], "mwait");
    test_invalid(&[0x66, 0x0f, 0x01, 0xc9]);
    test_invalid(&[0xf2, 0x0f, 0x01, 0xc9]);
    test_invalid(&[0xf3, 0x0f, 0x01, 0xc9]);
}

#[test]
fn sse4_2() {
    fn test_instr(bytes: &[u8], text: &'static str) {
        test_display_under(&Decoder::minimal().with_sse4_2(), bytes, text);
        test_invalid_under(&Decoder::minimal(), bytes);
        // avx doesn't imply older instructions are necessarily valid
        test_invalid_under(&Decoder::minimal().with_avx(), bytes);
    }

    #[allow(unused)]
    fn test_instr_invalid(bytes: &[u8]) {
        test_invalid_under(&Decoder::minimal().with_sse4_2(), bytes);
        test_invalid_under(&Decoder::default(), bytes);
    }

    test_instr(
        &[0x66, 0x0f, 0x38, 0x37, 0x03],
        "pcmpgtq xmm0, xmmword [ebx]",
    );
    test_instr(&[0x66, 0x0f, 0x38, 0x37, 0xc3], "pcmpgtq xmm0, xmm3");

    test_instr(&[0xf2, 0x0f, 0x38, 0xf0, 0x06], "crc32 eax, byte [esi]");
    test_instr(&[0xf2, 0x0f, 0x38, 0xf0, 0xc6], "crc32 eax, dh");
    test_instr(&[0xf2, 0x0f, 0x38, 0xf1, 0x06], "crc32 eax, dword [esi]");
    test_instr(&[0xf2, 0x0f, 0x38, 0xf1, 0xc6], "crc32 eax, esi");
    test_instr(&[0x66, 0xf2, 0x0f, 0x38, 0xf1, 0xc6], "crc32 eax, si");

    test_instr(
        &[0x66, 0x0f, 0x3a, 0x60, 0xc6, 0x54],
        "pcmpestrm xmm0, xmm6, 0x54",
    );
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x60, 0x06, 0x54],
        "pcmpestrm xmm0, xmmword [esi], 0x54",
    );
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x61, 0xc6, 0x54],
        "pcmpestri xmm0, xmm6, 0x54",
    );
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x61, 0x06, 0x54],
        "pcmpestri xmm0, xmmword [esi], 0x54",
    );
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x62, 0xc6, 0x54],
        "pcmpistrm xmm0, xmm6, 0x54",
    );
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x62, 0x06, 0x54],
        "pcmpistrm xmm0, xmmword [esi], 0x54",
    );
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x63, 0xc6, 0x54],
        "pcmpistri xmm0, xmm6, 0x54",
    );
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x63, 0x06, 0x54],
        "pcmpistri xmm0, xmmword [esi], 0x54",
    );
}

#[test]
fn sse4_1() {
    fn test_instr(bytes: &[u8], text: &'static str) {
        test_display_under(&Decoder::minimal().with_sse4_1(), bytes, text);
        test_invalid_under(&Decoder::minimal(), bytes);
        // avx doesn't imply older instructions are necessarily valid
        test_invalid_under(&Decoder::minimal().with_avx(), bytes);
        // sse4_2 doesn't imply older instructions are necessarily valid
        test_invalid_under(&Decoder::minimal().with_sse4_2(), bytes);
    }

    #[allow(unused)]
    fn test_instr_invalid(bytes: &[u8]) {
        test_invalid_under(&Decoder::minimal().with_sse4_1(), bytes);
        test_invalid_under(&Decoder::default(), bytes);
    }

    test_instr(
        &[0x66, 0x0f, 0x3a, 0x0c, 0x11, 0x22],
        "blendps xmm2, xmmword [ecx], 0x22",
    );
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x0c, 0xc1, 0x22],
        "blendps xmm0, xmm1, 0x22",
    );
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x0d, 0x11, 0x22],
        "blendpd xmm2, xmmword [ecx], 0x22",
    );
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x0d, 0xc1, 0x22],
        "blendpd xmm0, xmm1, 0x22",
    );

    test_instr(
        &[0x66, 0x0f, 0x38, 0x10, 0x06],
        "pblendvb xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x10, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x38, 0x14, 0x06],
        "blendvps xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x14, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x15, 0x06],
        "blendvpd xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x15, 0x06]);

    test_instr(&[0x66, 0x0f, 0x38, 0x17, 0x06], "ptest xmm0, xmmword [esi]");
    test_invalid(&[0x0f, 0x38, 0x17, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x38, 0x20, 0x06],
        "pmovsxbw xmm0, qword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x20, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x21, 0x06],
        "pmovsxbd xmm0, qword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x21, 0x06]);
    test_instr(&[0x66, 0x0f, 0x38, 0x22, 0x06], "pmovsxbq xmm0, word [esi]");
    test_invalid(&[0x0f, 0x38, 0x22, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x23, 0x06],
        "pmovsxwd xmm0, qword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x23, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x24, 0x06],
        "pmovsxwq xmm0, qword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x24, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x25, 0x06],
        "pmovsxdq xmm0, qword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x25, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x38, 0x28, 0x06],
        "pmuldq xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x28, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x29, 0x06],
        "pcmpeqq xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x29, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x2a, 0x06],
        "movntdqa xmm0, xmmword [esi]",
    );
    test_invalid(&[0x66, 0x0f, 0x38, 0x2a, 0xc6]);
    test_invalid(&[0x0f, 0x38, 0x2a, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x2b, 0x06],
        "packusdw xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x2b, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x38, 0x30, 0x06],
        "pmovzxbw xmm0, qword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x30, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x31, 0x06],
        "pmovzxbd xmm0, dword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x31, 0x06]);
    test_instr(&[0x66, 0x0f, 0x38, 0x32, 0x06], "pmovzxbq xmm0, word [esi]");
    test_invalid(&[0x0f, 0x38, 0x32, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x33, 0x06],
        "pmovzxwd xmm0, qword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x33, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x34, 0x06],
        "pmovzxwq xmm0, qword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x34, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x35, 0x06],
        "pmovzxdq xmm0, qword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x35, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x38, 0x38, 0x06],
        "pminsb xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x38, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x39, 0x06],
        "pminsd xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x39, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x3a, 0x06],
        "pminuw xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x3a, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x3b, 0x06],
        "pminud xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x3b, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x3c, 0x06],
        "pmaxsb xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x3c, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x3d, 0x06],
        "pmaxsd xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x3d, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x3e, 0x06],
        "pmaxuw xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x3e, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x3f, 0x06],
        "pmaxud xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x3f, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x38, 0x40, 0x06],
        "pmulld xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x40, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x38, 0x41, 0x06],
        "phminposuw xmm0, xmmword [esi]",
    );
    test_invalid(&[0x0f, 0x38, 0x41, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x3a, 0x08, 0x06, 0x31],
        "roundps xmm0, xmmword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x08, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x09, 0x06, 0x31],
        "roundpd xmm0, xmmword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x09, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x0a, 0x06, 0x31],
        "roundss xmm0, xmmword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x0a, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x0b, 0x06, 0x31],
        "roundsd xmm0, xmmword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x0b, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x3a, 0x0e, 0x06, 0x31],
        "pblendw xmm0, xmmword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x0e, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x3a, 0x14, 0x06, 0x31],
        "pextrb xmm0, byte [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x14, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x15, 0x06, 0x31],
        "pextrw xmm0, word [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x15, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x16, 0x06, 0x31],
        "pextrd xmm0, dword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x16, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x17, 0x06, 0x31],
        "extractps xmm0, dword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x17, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x3a, 0x20, 0x06, 0x31],
        "pinsrb xmm0, byte [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x20, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x21, 0x06, 0x31],
        "insertps xmm0, dword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x21, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x22, 0x06, 0x31],
        "pinsrd xmm0, dword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x22, 0x06]);

    test_instr(
        &[0x66, 0x0f, 0x3a, 0x40, 0x06, 0x31],
        "dpps xmm0, xmmword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x40, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x41, 0x06, 0x31],
        "dppd xmm0, xmmword [esi], 0x31",
    );
    test_invalid(&[0x0f, 0x3a, 0x41, 0x06]);
    test_instr(
        &[0x66, 0x0f, 0x3a, 0x42, 0x06, 0x44],
        "mpsadbw xmm0, xmmword [esi], 0x44",
    );
    test_invalid(&[0x0f, 0x3a, 0x42, 0x06]);
}

#[test]
fn ssse3() {
    fn test_instr(bytes: &[u8], text: &'static str) {
        test_display_under(&Decoder::minimal().with_ssse3(), bytes, text);
        test_invalid_under(&Decoder::minimal(), bytes);
        // avx doesn't imply older instructions are necessarily valid
        test_invalid_under(&Decoder::minimal().with_avx(), bytes);
        // sse4 doesn't imply older instructions are necessarily valid
        test_invalid_under(&Decoder::minimal().with_sse4_1(), bytes);
        test_invalid_under(&Decoder::minimal().with_sse4_2(), bytes);
    }

    #[allow(unused)]
    fn test_instr_invalid(bytes: &[u8]) {
        test_invalid_under(&Decoder::minimal().with_ssse3(), bytes);
        test_invalid_under(&Decoder::default(), bytes);
    }
    test_instr(&[0x66, 0x0f, 0x38, 0x00, 0xda], "pshufb xmm3, xmm2");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x00, 0x06],
        "pshufb xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x00, 0x06], "pshufb mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x01, 0x06],
        "phaddw xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x01, 0x06], "phaddw mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x02, 0x06],
        "phaddd xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x02, 0x06], "phaddd mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x03, 0x06],
        "phaddsw xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x03, 0x06], "phaddsw mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x04, 0x06],
        "pmaddubsw xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x04, 0x06], "pmaddubsw mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x05, 0x06],
        "phsubw xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x05, 0x06], "phsubw mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x06, 0x06],
        "phsubd xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x06, 0x06], "phsubd mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x07, 0x06],
        "phsubsw xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x07, 0x06], "phsubsw mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x08, 0x06],
        "psignb xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x08, 0x06], "psignb mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x09, 0x06],
        "psignw xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x09, 0x06], "psignw mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x0a, 0x06],
        "psignd xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x0a, 0x06], "psignd mm0, qword [esi]");
    test_instr(
        &[0x66, 0x0f, 0x38, 0x0b, 0x06],
        "pmulhrsw xmm0, xmmword [esi]",
    );
    test_instr(&[0x0f, 0x38, 0x0b, 0x06], "pmulhrsw mm0, qword [esi]");

    test_instr(&[0x66, 0x0f, 0x38, 0x1c, 0x06], "pabsb xmm0, xmmword [esi]");
    test_instr(&[0x0f, 0x38, 0x1c, 0x06], "pabsb mm0, qword [esi]");
    test_instr(&[0x66, 0x0f, 0x38, 0x1d, 0x06], "pabsw xmm0, xmmword [esi]");
    test_instr(&[0x0f, 0x38, 0x1d, 0x06], "pabsw mm0, qword [esi]");
    test_instr(&[0x66, 0x0f, 0x38, 0x1e, 0x06], "pabsd xmm0, xmmword [esi]");
    test_instr(&[0x0f, 0x38, 0x1e, 0x06], "pabsd mm0, qword [esi]");

    test_instr(
        &[0x66, 0x0f, 0x3a, 0x0f, 0x06, 0x30],
        "palignr xmm0, xmmword [esi], 0x30",
    );
    test_instr(
        &[0x0f, 0x3a, 0x0f, 0x06, 0x30],
        "palignr mm0, qword [esi], 0x30",
    );
}

#[test]
fn zerof01() {
    // drawn heavily from "Table A-6.  Opcode Extensions for One- and Two-byte Opcodes by Group
    // Number"
    test_display(&[0x0f, 0x01, 0x38], "invlpg byte [eax]");
    test_display(&[0x0f, 0x01, 0x3f], "invlpg byte [edi]");
    test_display(&[0x0f, 0x01, 0x40, 0xff], "sgdt ptr [eax - 0x1]");
    test_display(&[0x0f, 0x01, 0x41, 0xff], "sgdt ptr [ecx - 0x1]");
    test_display(&[0x0f, 0x01, 0x49, 0xff], "sidt ptr [ecx - 0x1]");
    test_display(&[0x0f, 0x01, 0x51, 0xff], "lgdt ptr [ecx - 0x1]");
    test_display(&[0x0f, 0x01, 0x59, 0xff], "lidt ptr [ecx - 0x1]");
    test_display(&[0x0f, 0x01, 0x61, 0xff], "smsw word [ecx - 0x1]");
    test_invalid(&[0x0f, 0x01, 0x69, 0xff]);
    test_display(&[0x0f, 0x01, 0x71, 0xff], "lmsw word [ecx - 0x1]");
    test_display(&[0x0f, 0x01, 0x79, 0xff], "invlpg byte [ecx - 0x1]");
    test_display(&[0x0f, 0x01, 0xc0], "enclv");
    test_display(&[0x0f, 0x01, 0xc1], "vmcall");
    test_display(&[0x0f, 0x01, 0xc2], "vmlaunch");
    test_display(&[0x0f, 0x01, 0xc3], "vmresume");
    test_display(&[0x0f, 0x01, 0xc4], "vmxoff");
    test_invalid(&[0x0f, 0x01, 0xc5]); // TODO: TME would make this `pconfig`
    test_invalid(&[0x0f, 0x01, 0xc6]);
    test_invalid(&[0x0f, 0x01, 0xc7]);
    test_display(&[0x0f, 0x01, 0xc8], "monitor");
    test_display(&[0x0f, 0x01, 0xc9], "mwait");
    test_display(&[0x0f, 0x01, 0xca], "clac");
    test_display(&[0x0f, 0x01, 0xcb], "stac");
    test_invalid(&[0x0f, 0x01, 0xcc]);
    test_invalid(&[0x0f, 0x01, 0xcd]);
    test_invalid(&[0x0f, 0x01, 0xce]);
    test_display(&[0x0f, 0x01, 0xcf], "encls");
    test_display(&[0x0f, 0x01, 0xd0], "xgetbv");
    test_display(&[0x0f, 0x01, 0xd1], "xsetbv");
    test_invalid(&[0x0f, 0x01, 0xd2]);
    test_invalid(&[0x0f, 0x01, 0xd3]);
    test_display(&[0x0f, 0x01, 0xd4], "vmfunc");
    test_display(&[0x0f, 0x01, 0xd5], "xend");
    test_display(&[0x0f, 0x01, 0xd6], "xtest");
    test_display(&[0x0f, 0x01, 0xd7], "enclu");
    test_display(&[0x0f, 0x01, 0xd8], "vmrun eax");
    test_display(&[0x0f, 0x01, 0xd9], "vmmcall");
    test_display(&[0x0f, 0x01, 0xda], "vmload eax");
    test_display(&[0x0f, 0x01, 0xdb], "vmsave eax");
    test_display(&[0x0f, 0x01, 0xdc], "stgi");
    test_display(&[0x0f, 0x01, 0xdd], "clgi");
    test_display(&[0x0f, 0x01, 0xde], "skinit eax");
    test_display(&[0x0f, 0x01, 0xdf], "invlpga eax, ecx");
    // TODO: not clear what SHOULD be reported for invlpgb. certainly not a `rax` operand. xed claims
    // that this is UD in protected mode. the AMD manual explicitly says this does not #UD in protected
    // mode. same for tlbsync.
    //    test_display(&[0x0f, 0x01, 0xfe], "invlpgb rax, edx, ecx");
    //    test_display(&[0x0f, 0x01, 0xff], "tlbsync");
    //    test_display(&[0x2e, 0x67, 0x65, 0x2e, 0x46, 0x0f, 0x01, 0xff], "tlbsync");
    test_display(&[0x0f, 0x01, 0xe0], "smsw ax");
    test_display(&[0x0f, 0x01, 0xe1], "smsw cx");
    test_display(&[0x0f, 0x01, 0xe2], "smsw dx");
    test_display(&[0x0f, 0x01, 0xe3], "smsw bx");
    test_display(&[0x0f, 0x01, 0xe4], "smsw sp");
    test_display(&[0x0f, 0x01, 0xe5], "smsw bp");
    test_display(&[0x0f, 0x01, 0xe6], "smsw si");
    test_display(&[0x0f, 0x01, 0xe7], "smsw di");
    test_invalid(&[0x0f, 0x01, 0xe8]);
    test_invalid(&[0x0f, 0x01, 0xe8]);
    test_invalid(&[0x0f, 0x01, 0xe9]);
    test_invalid(&[0x0f, 0x01, 0xea]);
    test_invalid(&[0x0f, 0x01, 0xeb]);
    test_display(&[0x0f, 0x01, 0xee], "rdpkru");
    test_display(&[0x0f, 0x01, 0xef], "wrpkru");
    test_invalid(&[0xf2, 0x0f, 0x01, 0xee]);
    test_invalid(&[0xf2, 0x0f, 0x01, 0xef]);
    test_display(&[0x0f, 0x01, 0xf0], "lmsw ax");
    test_display(&[0x0f, 0x01, 0xf1], "lmsw cx");
    test_display(&[0x0f, 0x01, 0xf2], "lmsw dx");
    test_display(&[0x0f, 0x01, 0xf3], "lmsw bx");
    test_display(&[0x0f, 0x01, 0xf4], "lmsw sp");
    test_display(&[0x0f, 0x01, 0xf5], "lmsw bp");
    test_display(&[0x0f, 0x01, 0xf6], "lmsw si");
    test_display(&[0x0f, 0x01, 0xf7], "lmsw di");
    test_invalid(&[0x0f, 0x01, 0xf8]);
    test_display(&[0x0f, 0x01, 0xf9], "rdtscp");
    test_display(&[0x0f, 0x01, 0xfa], "monitorx");
    test_display(&[0x0f, 0x01, 0xfb], "mwaitx");
    test_display(&[0x0f, 0x01, 0xfc], "clzero");
    test_display(&[0x0f, 0x01, 0xfd], "rdpru ecx");
}

#[test]
fn zerofae() {
    let intel = Decoder::minimal().with_intel_quirks();
    let amd = Decoder::minimal().with_amd_quirks();
    let default = Decoder::default();
    let minimal = Decoder::minimal();
    // drawn heavily from "Table A-6.  Opcode Extensions for One- and Two-byte Opcodes by Group
    // Number"
    test_invalid(&[0xf3, 0x0f, 0xae, 0x87]);
    test_invalid(&[0xf3, 0x0f, 0xae, 0x04, 0x4f]);
    test_display(&[0x0f, 0xae, 0x04, 0x4f], "fxsave ptr [edi + ecx * 2]");
    test_display(&[0x0f, 0xae, 0x0c, 0x4f], "fxrstor ptr [edi + ecx * 2]");
    test_display(&[0x0f, 0xae, 0x14, 0x4f], "ldmxcsr dword [edi + ecx * 2]");
    test_display(&[0x0f, 0xae, 0x1c, 0x4f], "stmxcsr dword [edi + ecx * 2]");
    test_display(&[0x0f, 0xae, 0x24, 0x4f], "xsave ptr [edi + ecx * 2]");
    test_display(&[0x0f, 0xc7, 0x5c, 0x24, 0x40], "xrstors ptr [esp + 0x40]");
    test_display(&[0x0f, 0xc7, 0x64, 0x24, 0x40], "xsavec ptr [esp + 0x40]");
    test_display(&[0x0f, 0xc7, 0x6c, 0x24, 0x40], "xsaves ptr [esp + 0x40]");
    test_display(
        &[0x0f, 0xc7, 0x74, 0x24, 0x40],
        "vmptrld qword [esp + 0x40]",
    );
    test_display(
        &[0x0f, 0xc7, 0x7c, 0x24, 0x40],
        "vmptrst qword [esp + 0x40]",
    );
    test_display(&[0x0f, 0xae, 0x2c, 0x4f], "xrstor ptr [edi + ecx * 2]");
    test_display(&[0x0f, 0xae, 0x34, 0x4f], "xsaveopt ptr [edi + ecx * 2]");
    test_display(&[0x0f, 0xae, 0x3c, 0x4f], "clflush zmmword [edi + ecx * 2]");

    for (modrm, text) in &[(0xe8u8, "lfence"), (0xf0u8, "mfence"), (0xf8u8, "sfence")] {
        test_display_under(&intel, &[0x0f, 0xae, *modrm], text);
        test_display_under(&amd, &[0x0f, 0xae, *modrm], text);
        test_display_under(&default, &[0x0f, 0xae, *modrm], text);
        test_display_under(&minimal, &[0x0f, 0xae, *modrm], text);
        // it turns out intel and amd accept m != 0 for {l,m,s}fence:
        // from intel:
        // ```
        // Specification of the instruction's opcode above indicates a ModR/M byte of F0. For this
        // instruction, the processor ignores the r/m field of the ModR/M byte. Thus, MFENCE is encoded
        // by any opcode of the form 0F AE Fx, where x is in the range 0-7.
        // ```
        // whereas amd does not discuss the r/m field at all. at least as of zen, amd also accepts
        // these encodings.
        for m in 1u8..8u8 {
            test_display_under(&intel, &[0x0f, 0xae, modrm | m], text);
            test_display_under(&amd, &[0x0f, 0xae, modrm | m], text);
            test_display_under(&default, &[0x0f, 0xae, modrm | m], text);
            test_invalid_under(&minimal, &[0x0f, 0xae, modrm | m]);
        }
    }
}

#[test]
fn system() {
    test_display(&[0x63, 0xc1], "arpl cx, ax");
    test_display(&[0x63, 0x04, 0xba], "arpl word [edx + edi * 4], ax");
    test_display(&[0x0f, 0x06], "clts");
}

#[test]
fn arithmetic() {
    test_display(&[0x81, 0xec, 0x10, 0x03, 0x00, 0x00], "sub esp, 0x310");
    test_display(&[0x0f, 0xaf, 0xc2], "imul eax, edx");
    test_display(
        &[0x69, 0x43, 0x6f, 0x6d, 0x70, 0x6c, 0x65],
        "imul eax, dword [ebx + 0x6f], 0x656c706d",
    );
    test_display(&[0x66, 0x0f, 0xaf, 0xd1], "imul dx, cx");
    test_display(&[0xf6, 0xe8], "imul al");
    test_display(&[0xf6, 0x28], "imul byte [eax]");
    test_display(
        &[0x6b, 0x43, 0x6f, 0x6d],
        "imul eax, dword [ebx + 0x6f], 0x6d",
    );
    test_display(&[0x00, 0xcc], "add ah, cl");
}

#[test]
#[allow(non_snake_case)]
fn test_E_decode() {
    test_display(&[0xff, 0x75, 0xb8], "push dword [ebp - 0x48]");
    test_display(&[0xff, 0x75, 0x08], "push dword [ebp + 0x8]");
}

#[test]
fn sse() {
    test_display(
        &[0xf3, 0x0f, 0x10, 0x0c, 0xc7],
        "movss xmm1, dword [edi + eax * 8]",
    );
    test_display(
        &[0xf3, 0x0f, 0x11, 0x0c, 0xc7],
        "movss dword [edi + eax * 8], xmm1",
    );
    test_display(&[0x0f, 0x28, 0x00], "movaps xmm0, xmmword [eax]");
    test_display(&[0x0f, 0x29, 0x00], "movaps xmmword [eax], xmm0");
    test_display(&[0xf3, 0x0f, 0x2a, 0xc1], "cvtsi2ss xmm0, ecx");
    test_display(&[0xf3, 0x0f, 0x2a, 0x01], "cvtsi2ss xmm0, dword [ecx]");
    test_display(&[0x0f, 0x2b, 0x00], "movntps xmmword [eax], xmm0");
    test_display(&[0xf3, 0x0f, 0x2c, 0xc1], "cvttss2si eax, xmm1");
    test_display(&[0xf3, 0x0f, 0x2c, 0x01], "cvttss2si eax, dword [ecx]");
    test_display(&[0xf3, 0x0f, 0x2d, 0xc1], "cvtss2si eax, xmm1");
    test_display(&[0xf3, 0x0f, 0x2d, 0x01], "cvtss2si eax, dword [ecx]");
    test_display(&[0x0f, 0x2e, 0x00], "ucomiss xmm0, dword [eax]");
    test_display(&[0x0f, 0x2f, 0x00], "comiss xmm0, dword [eax]");
    test_display(&[0x0f, 0x28, 0xd0], "movaps xmm2, xmm0");
    test_display(&[0x66, 0x0f, 0x28, 0xd0], "movapd xmm2, xmm0");
    test_display(&[0x66, 0x0f, 0x28, 0x00], "movapd xmm0, xmmword [eax]");
    test_display(
        &[0x67, 0x66, 0x0f, 0x28, 0x00],
        "movapd xmm0, xmmword [bx + si]",
    );
    test_display(&[0x66, 0x0f, 0x29, 0x00], "movapd xmmword [eax], xmm0");
    test_invalid(&[0x0f, 0x50, 0x00]);
    test_display(&[0x0f, 0x50, 0xc1], "movmskps eax, xmm1");
    test_display(&[0x0f, 0x51, 0x01], "sqrtps xmm0, xmmword [ecx]");
    test_display(&[0xf3, 0x0f, 0x51, 0x01], "sqrtss xmm0, dword [ecx]");
    test_display(&[0x0f, 0x52, 0x01], "rsqrtps xmm0, xmmword [ecx]");
    test_display(&[0xf3, 0x0f, 0x52, 0x01], "rsqrtss xmm0, dword [ecx]");
    test_display(&[0x0f, 0x53, 0x01], "rcpps xmm0, xmmword [ecx]");
    test_display(&[0xf3, 0x0f, 0x53, 0x01], "rcpss xmm0, dword [ecx]");
    test_display(&[0xf3, 0x0f, 0x53, 0xc1], "rcpss xmm0, xmm1");
    test_display(&[0x0f, 0x54, 0x01], "andps xmm0, xmmword [ecx]");
    test_display(&[0x0f, 0x55, 0x01], "andnps xmm0, xmmword [ecx]");
    test_display(&[0x0f, 0x56, 0x01], "orps xmm0, xmmword [ecx]");
    test_display(&[0x0f, 0x57, 0x01], "xorps xmm0, xmmword [ecx]");
    test_display(&[0x0f, 0x58, 0x01], "addps xmm0, xmmword [ecx]");
    test_display(&[0xf3, 0x0f, 0x58, 0x01], "addss xmm0, dword [ecx]");
    test_display(&[0x0f, 0x59, 0x01], "mulps xmm0, xmmword [ecx]");
    test_display(&[0xf3, 0x0f, 0x59, 0x01], "mulss xmm0, dword [ecx]");
    test_display(&[0x0f, 0x5a, 0x01], "cvtps2pd xmm0, qword [ecx]");
    test_display(&[0xf3, 0x0f, 0x5a, 0x01], "cvtss2sd xmm0, dword [ecx]");
    test_display(&[0x0f, 0x5b, 0x01], "cvtdq2ps xmm0, xmmword [ecx]");
    test_display(&[0xf3, 0x0f, 0x5b, 0x01], "cvttps2dq xmm0, xmmword [ecx]");
    test_display(
        &[0x67, 0x0f, 0x5b, 0x01],
        "cvtdq2ps xmm0, xmmword [bx + di]",
    );
    test_display(&[0x0f, 0x5c, 0x01], "subps xmm0, xmmword [ecx]");
    test_display(&[0xf3, 0x0f, 0x5c, 0x01], "subss xmm0, dword [ecx]");
    test_display(&[0x0f, 0x5d, 0x01], "minps xmm0, xmmword [ecx]");
    test_display(&[0xf3, 0x0f, 0x5d, 0x01], "minss xmm0, dword [ecx]");
    test_display(&[0x0f, 0x5e, 0x01], "divps xmm0, xmmword [ecx]");
    test_display(&[0xf3, 0x0f, 0x5e, 0x01], "divss xmm0, dword [ecx]");
    test_display(&[0x0f, 0x5f, 0x01], "maxps xmm0, xmmword [ecx]");
    test_display(&[0xf3, 0x0f, 0x5f, 0x01], "maxss xmm0, dword [ecx]");

    test_display(&[0x0f, 0x70, 0x00, 0x7f], "pshufw mm0, qword [eax], 0x7f");

    test_display(&[0x66, 0x0f, 0xef, 0xc0], "pxor xmm0, xmm0");
    test_display(&[0x66, 0x0f, 0xef, 0xc0], "pxor xmm0, xmm0");
    test_display(
        &[0xf2, 0x0f, 0x10, 0x0c, 0xc6],
        "movsd xmm1, qword [esi + eax * 8]",
    );
    test_display(
        &[0xf3, 0x0f, 0x10, 0x04, 0x86],
        "movss xmm0, dword [esi + eax * 4]",
    );
    test_display(&[0xf2, 0x0f, 0x59, 0xc8], "mulsd xmm1, xmm0");
    test_display(&[0xf3, 0x0f, 0x59, 0xc8], "mulss xmm1, xmm0");

    test_display(
        &[0xf3, 0x0f, 0x6f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "movdqu xmm3, xmmword [esp + ebx * 4 - 0x334455cc]",
    );
    test_display(&[0xf3, 0x0f, 0x70, 0xc0, 0x4e], "pshufhw xmm0, xmm0, 0x4e");
    test_display(&[0xf3, 0x0f, 0x7e, 0xc1], "movq xmm0, xmm1");
    test_display(
        &[0xf3, 0x0f, 0x7f, 0x9c, 0x9c, 0x34, 0xaa, 0xbb, 0xcc],
        "movdqu xmmword [esp + ebx * 4 - 0x334455cc], xmm3",
    );

    test_display(&[0xf3, 0x0f, 0xc2, 0xc3, 0x08], "cmpss xmm0, xmm3, 0x8");
    test_display(
        &[0xf3, 0x0f, 0xc2, 0x03, 0x08],
        "cmpss xmm0, dword [ebx], 0x8",
    );
}

// SETLE, SETNG, ...

#[test]
fn mov() {
    test_display(&[0xa0, 0x93, 0x62, 0xc4, 0x00], "mov al, byte [0xc46293]");
    test_display(&[0x67, 0xa0, 0x93, 0x62], "mov al, byte [0x6293]");
    test_display(&[0xa1, 0x93, 0x62, 0xc4, 0x00], "mov eax, dword [0xc46293]");
    test_display(&[0x67, 0xa1, 0x93, 0x62], "mov eax, dword [0x6293]");
    test_display(&[0xa2, 0x93, 0x62, 0xc4, 0x00], "mov byte [0xc46293], al");
    test_display(&[0x67, 0xa2, 0x93, 0x62], "mov byte [0x6293], al");
    test_display(&[0xa3, 0x93, 0x62, 0xc4, 0x00], "mov dword [0xc46293], eax");
    test_display(&[0x67, 0xa3, 0x93, 0x62], "mov dword [0x6293], eax");
    test_display(&[0xba, 0x01, 0x00, 0x00, 0x00], "mov edx, 0x1");
    test_display(
        &[0xc7, 0x04, 0x24, 0x00, 0x00, 0x00, 0x00],
        "mov dword [esp], 0x0",
    );
    test_display(&[0x89, 0x44, 0x24, 0x08], "mov dword [esp + 0x8], eax");
    test_display(&[0x89, 0x43, 0x18], "mov dword [ebx + 0x18], eax");
    test_display(
        &[0xc7, 0x43, 0x10, 0x00, 0x00, 0x00, 0x00],
        "mov dword [ebx + 0x10], 0x0",
    );
    test_display(&[0x89, 0x4e, 0x08], "mov dword [esi + 0x8], ecx");
    test_display(&[0x8b, 0x32], "mov esi, dword [edx]");
    test_display(
        &[0x8b, 0x4c, 0x10, 0xf8],
        "mov ecx, dword [eax + edx * 1 - 0x8]",
    );
    test_display(&[0x89, 0x46, 0x10], "mov dword [esi + 0x10], eax");
    test_display(&[0x0f, 0x43, 0xec], "cmovnb ebp, esp");
    test_display(&[0x0f, 0xb6, 0x06], "movzx eax, byte [esi]");
    test_display(&[0x0f, 0xb7, 0x06], "movzx eax, word [esi]");
    test_display(&[0x89, 0x55, 0x94], "mov dword [ebp - 0x6c], edx");
    test_display(
        &[0x65, 0x89, 0x04, 0x25, 0xa8, 0x01, 0x00, 0x00],
        "mov dword gs:[0x1a8], eax",
    );
    test_display(
        &[0x0f, 0xbe, 0x83, 0xb4, 0x00, 0x00, 0x00],
        "movsx eax, byte [ebx + 0xb4]",
    );
    test_display(&[0xf3, 0x0f, 0x6f, 0x07], "movdqu xmm0, xmmword [edi]");
    test_display(
        &[0xf3, 0x0f, 0x7f, 0x45, 0x00],
        "movdqu xmmword [ebp], xmm0",
    );

    test_display(&[0x0f, 0x97, 0xc0], "seta al");
    test_display(&[0x0f, 0x97, 0xc8], "seta al");
    test_display(&[0x0f, 0x97, 0x00], "seta byte [eax]");
    test_display(&[0x0f, 0x97, 0x08], "seta byte [eax]");
    //    test_display(&[0xd6], "salc");
    test_display(&[0x8e, 0x00], "mov es, word [eax]");
    // cs is not an allowed destination
    test_invalid(&[0x8e, 0x08]);
    test_display(&[0x8e, 0x10], "mov ss, word [eax]");
    test_display(&[0x8e, 0x18], "mov ds, word [eax]");
    test_display(&[0x8e, 0x20], "mov fs, word [eax]");
    test_display(&[0x8e, 0x28], "mov gs, word [eax]");
    test_invalid(&[0x8e, 0x30]);
    test_invalid(&[0x8e, 0x38]);
}

#[test]
fn xchg() {
    test_display(&[0x90], "nop");
    test_display(&[0x91], "xchg eax, ecx");
    test_display(&[0x66, 0x91], "xchg ax, cx");
}

#[test]
fn stack() {
    test_display(&[0x66, 0x50], "push ax");
}

#[test]
fn prefixes() {
    test_display(&[0x66, 0x31, 0xc0], "xor ax, ax");
    test_display(&[0x66, 0x32, 0xc0], "xor al, al");
    test_display(&[0x66, 0x32, 0xc5], "xor al, ch");
    test_invalid(&[0xf0, 0x33, 0xc0]);
    test_display(&[0xf0, 0x31, 0x00], "lock xor dword [eax], eax");
    test_display(&[0xf0, 0x80, 0x30, 0x00], "lock xor byte [eax], 0x0");
    test_display(&[0xf0, 0x0f, 0xbb, 0x17], "lock btc dword [edi], edx");
    test_display(
        &[0x66, 0x2e, 0xf2, 0xf0, 0x0f, 0xbb, 0x13],
        "xacquire lock btc word cs:[ebx], dx",
    );
    test_invalid(&[0xf0, 0xc7, 0x00, 0x00, 0x00, 0x00]);
    test_display(&[0x0f, 0xc1, 0xcc], "xadd esp, ecx");
    test_display(&[0x66, 0x0f, 0xc1, 0xcc], "xadd sp, cx");
    test_display(&[0xf2, 0x0f, 0xc1, 0xcc], "xadd esp, ecx");
    test_display(&[0xf3, 0x0f, 0xc1, 0xcc], "xadd esp, ecx");
    test_display(&[0x0f, 0xc0, 0xcc], "xadd ah, cl");
    test_display(&[0x66, 0x0f, 0xc0, 0xcc], "xadd ah, cl");
    test_display(&[0xf2, 0x0f, 0xc0, 0xcc], "xadd ah, cl");
    test_display(&[0xf3, 0x0f, 0xc0, 0xcc], "xadd ah, cl");
}

#[test]
fn control_flow() {
    test_display(&[0x73, 0x31], "jnb 0x31");
    test_display(&[0x72, 0x5a], "jb 0x5a");
    test_display(&[0x72, 0xf0], "jb -0x10");
    test_display(&[0x0f, 0x86, 0x8b, 0x01, 0x00, 0x00], "jna 0x18b");
    test_display(&[0x74, 0x47], "jz 0x47");
    test_display(
        &[0xff, 0x15, 0x7e, 0x72, 0x24, 0x00],
        "call dword [0x24727e]",
    );
    test_display(
        &[0xff, 0x24, 0xcd, 0x70, 0xa0, 0xbc, 0x01],
        "jmp dword [ecx * 8 + 0x1bca070]",
    );
    test_display(&[0xff, 0xe0], "jmp eax");
    test_display(&[0x66, 0xff, 0xe0], "jmp eax");
    test_display(&[0x67, 0xff, 0xe0], "jmp eax");
    test_invalid(&[0xff, 0xd8]);
    test_display(&[0xff, 0x18], "callf far [eax]");
    test_display(&[0xe0, 0x12], "loopnz 0x12");
    test_display(&[0xe1, 0x12], "loopz 0x12");
    test_display(&[0xe2, 0x12], "loop 0x12");
    test_display(&[0xe3, 0x12], "jecxz 0x12");
    test_display(&[0xe3, 0xf0], "jecxz -0x10");
    test_display(&[0xc3], "ret");
}

#[test]
fn bad_instructions() {
    // too long
    test_invalid(&[
        0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e, 0x2e,
        0x2e, 0x33, 0xc0,
    ]);
}

#[test]
fn test_cmp() {
    test_display(
        &[0xf6, 0x05, 0x2c, 0x9b, 0xff, 0xff, 0x01],
        "test byte [0xffff9b2c], 0x1",
    );
    test_display(&[0x3d, 0x01, 0xf0, 0xff, 0xff], "cmp eax, -0xfff");
    test_display(&[0x83, 0xf8, 0xff], "cmp eax, -0x1");
    test_display(&[0x39, 0xc6], "cmp esi, eax");
}

#[test]
fn push_pop() {
    test_display(&[0x5b], "pop ebx");
    test_display(&[0x5e], "pop esi");
    test_display(&[0x68, 0x7f, 0x63, 0xc4, 0x00], "push 0xc4637f");
    test_display(&[0x66, 0x8f, 0x00], "pop word [eax]");
    test_display(&[0x8f, 0x00], "pop dword [eax]");
}

#[test]
fn bmi1() {
    let bmi1 = Decoder::minimal().with_bmi1();
    let no_bmi1 = Decoder::minimal();
    test_display_under(&bmi1, &[0xf3, 0x0f, 0xbc, 0xd3], "tzcnt edx, ebx");
    test_display_under(&bmi1, &[0xf2, 0x0f, 0xbc, 0xd3], "bsf edx, ebx");
    test_display_under(&bmi1, &[0x0f, 0xbc, 0xd3], "bsf edx, ebx");
    test_display_under(&no_bmi1, &[0xf3, 0x0f, 0xbc, 0xd3], "bsf edx, ebx");

    // from the intel manual [`ANDN`, though this is true for `BMI1` generally]:
    // ```
    // VEX.W1 is ignored in non-64-bit modes.
    // ```

    // just 0f38
    test_display_under(
        &bmi1,
        &[0xc4, 0xe2, 0x60, 0xf2, 0x01],
        "andn eax, ebx, dword [ecx]",
    );
    test_display_under(
        &bmi1,
        &[0xc4, 0xe2, 0xe0, 0xf2, 0x01],
        "andn eax, ebx, dword [ecx]",
    );
    test_display_under(
        &bmi1,
        &[0xc4, 0xe2, 0x78, 0xf3, 0x09],
        "blsr eax, dword [ecx]",
    );
    test_display_under(
        &bmi1,
        &[0xc4, 0xe2, 0xf8, 0xf3, 0x09],
        "blsr eax, dword [ecx]",
    );
    test_display_under(
        &bmi1,
        &[0xc4, 0xe2, 0x78, 0xf3, 0x11],
        "blsmsk eax, dword [ecx]",
    );
    test_display_under(
        &bmi1,
        &[0xc4, 0xe2, 0xf8, 0xf3, 0x11],
        "blsmsk eax, dword [ecx]",
    );
    test_display_under(
        &bmi1,
        &[0xc4, 0xe2, 0x78, 0xf3, 0x19],
        "blsi eax, dword [ecx]",
    );
    test_display_under(
        &bmi1,
        &[0xc4, 0xe2, 0xf8, 0xf3, 0x19],
        "blsi eax, dword [ecx]",
    );
    test_display_under(
        &bmi1,
        &[0xc4, 0xe2, 0x60, 0xf7, 0x01],
        "bextr eax, dword [ecx], ebx",
    );
    test_display_under(
        &bmi1,
        &[0xc4, 0xe2, 0xe0, 0xf7, 0x01],
        "bextr eax, dword [ecx], ebx",
    );
}

#[test]
fn bmi2() {
    let bmi2 = Decoder::minimal().with_bmi2();

    // from the intel manual [`PDEP`, though this is true for `BMI2` generally]:
    // ```
    // VEX.W1 is ignored in non-64-bit modes.
    // ```

    // f2 0f3a
    test_display_under(
        &bmi2,
        &[0xc4, 0xe3, 0x7b, 0xf0, 0x01, 0x05],
        "rorx eax, dword [ecx], 0x5",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe3, 0xfb, 0xf0, 0x01, 0x05],
        "rorx eax, dword [ecx], 0x5",
    );

    // f2 0f38 map
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0x63, 0xf5, 0x07],
        "pdep eax, ebx, dword [edi]",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0xe3, 0xf5, 0x07],
        "pdep eax, ebx, dword [edi]",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0x63, 0xf6, 0x07],
        "mulx eax, ebx, dword [edi]",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0xe3, 0xf6, 0x07],
        "mulx eax, ebx, dword [edi]",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0x63, 0xf7, 0x01],
        "shrx eax, dword [ecx], ebx",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0xe3, 0xf7, 0x01],
        "shrx eax, dword [ecx], ebx",
    );

    // f3 0f38 map
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0x62, 0xf5, 0x07],
        "pext eax, ebx, dword [edi]",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0xe2, 0xf5, 0x07],
        "pext eax, ebx, dword [edi]",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0x62, 0xf7, 0x01],
        "sarx eax, dword [ecx], ebx",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0xe2, 0xf7, 0x01],
        "sarx eax, dword [ecx], ebx",
    );

    // just 0f38
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0x60, 0xf5, 0x07],
        "bzhi eax, dword [edi], ebx",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0xe0, 0xf5, 0x07],
        "bzhi eax, dword [edi], ebx",
    );

    // 66 0f38
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0x61, 0xf7, 0x01],
        "shlx eax, dword [ecx], ebx",
    );
    test_display_under(
        &bmi2,
        &[0xc4, 0xe2, 0xe1, 0xf7, 0x01],
        "shlx eax, dword [ecx], ebx",
    );
}

#[test]
fn popcnt() {
    let popcnt = Decoder::minimal().with_popcnt();
    let intel_popcnt = Decoder::minimal().with_intel_quirks().with_sse4_2();
    let no_popcnt = Decoder::minimal();
    test_display_under(&popcnt, &[0xf3, 0x0f, 0xb8, 0xc1], "popcnt eax, ecx");
    test_display_under(&intel_popcnt, &[0xf3, 0x0f, 0xb8, 0xc1], "popcnt eax, ecx");
    test_invalid_under(&no_popcnt, &[0xf3, 0x0f, 0xb8, 0xc1]);
}

#[test]
fn bitwise() {
    test_display_under(&Decoder::minimal(), &[0x0f, 0xbc, 0xd3], "bsf edx, ebx");
    test_display_under(
        &Decoder::minimal(),
        &[0x0f, 0xbb, 0x17],
        "btc dword [edi], edx",
    );
    test_display_under(
        &Decoder::minimal(),
        &[0xf0, 0x0f, 0xbb, 0x17],
        "lock btc dword [edi], edx",
    );
    test_display(&[0x0f, 0xa3, 0xd0], "bt eax, edx");
    test_display(&[0x0f, 0xab, 0xd0], "bts eax, edx");
    test_display(&[0x0f, 0xb3, 0xd0], "btr eax, edx");
    test_display(&[0x66, 0x0f, 0xb3, 0xc0], "btr ax, ax");
    test_display(&[0xd2, 0xe0], "shl al, cl");
}

#[test]
fn misc() {
    test_display(&[0xf1], "int 0x1");
    test_display(&[0xf5], "cmc");
    test_display(&[0xc8, 0x01, 0x02, 0x03], "enter 0x201, 0x3");
    test_display(&[0xc9], "leave");
    test_display(&[0xca, 0x12, 0x34], "retf 0x3412");
    test_display(&[0xcb], "retf");
    test_display(&[0x66, 0xcf], "iret");
    test_display(&[0xcf], "iretd");
    test_display(&[0xf2, 0x0f, 0x38, 0xf0, 0xc1], "crc32 eax, cl");
    test_display(&[0xf2, 0x0f, 0x38, 0xf1, 0xc1], "crc32 eax, ecx");
    test_display(&[0xfe, 0x00], "inc byte [eax]");
    test_display(&[0xfe, 0x08], "dec byte [eax]");
    test_display(&[0xff, 0x00], "inc dword [eax]");
    test_display(&[0xff, 0x08], "dec dword [eax]");
    test_display(&[0xe4, 0x99], "in al, 0x99");
    test_display(&[0xe5, 0x99], "in eax, 0x99");
    test_display(&[0x67, 0xe5, 0x99], "in eax, 0x99");
    test_display(&[0xe5, 0x99], "in eax, 0x99");
    test_display(&[0xe6, 0x99], "out 0x99, al");
    test_display(&[0xe7, 0x99], "out 0x99, eax");
    test_display(&[0xec], "in al, dx");
    test_display(&[0xed], "in eax, dx");
    test_display(&[0xee], "out dx, al");
    test_display(&[0xef], "out dx, eax");
    test_display(&[0xcd, 0x00], "int 0x0");
    test_display(&[0xcd, 0xff], "int 0xff");
    test_display(&[0x9c], "pushf");
    test_display(&[0x98], "cwde");
    test_display(&[0x66, 0x99], "cwd");
    test_display(
        &[0x66, 0x2e, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00],
        "nop word cs:[eax + eax * 1]",
    );
    test_display(
        &[0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00],
        "nop word [eax + eax * 1]",
    );
    test_display(
        &[0x8d, 0xa4, 0xc7, 0x20, 0x00, 0x00, 0x12],
        "lea esp, dword [edi + eax * 8 + 0x12000020]",
    );
    test_display(&[0x33, 0xc0], "xor eax, eax");
    test_display(&[0x8d, 0x53, 0x08], "lea edx, dword [ebx + 0x8]");
    test_invalid(&[0x8d, 0xdd]);
    test_display(&[0x31, 0xc9], "xor ecx, ecx");
    test_display(&[0x29, 0xc8], "sub eax, ecx");
    test_display(&[0x03, 0x0b], "add ecx, dword [ebx]");
    test_display(&[0x8d, 0x0c, 0x12], "lea ecx, dword [edx + edx * 1]");
    test_display(&[0xf6, 0xc2, 0x18], "test dl, 0x18");
    test_display(&[0xf3, 0xab], "rep stos dword es:[edi], eax");
    test_display(&[0xf3, 0xa5], "rep movs dword es:[edi], dword ds:[esi]");
    test_display(&[0xf3, 0x0f, 0xbc, 0xd7], "tzcnt edx, edi");

    // TODO:
    // this is actually vmx
    // test_invalid(&[0x66, 0x0f, 0xc7, 0x03]);
    test_display(&[0x66, 0x0f, 0xc7, 0x33], "vmclear qword [ebx]");
    test_display(&[0xf3, 0x0f, 0xc7, 0x33], "vmxon qword [ebx]");

    test_display(&[0xf3, 0x0f, 0xae, 0x26], "ptwrite dword [esi]");
    test_display(&[0xf3, 0x0f, 0xae, 0xe6], "ptwrite esi");
    test_invalid(&[0x66, 0xf3, 0x0f, 0xae, 0xe6]);
    test_display(&[0xf3, 0x0f, 0xae, 0xc4], "rdfsbase esp");
    test_display(&[0xf3, 0x0f, 0xae, 0xcc], "rdgsbase esp");
    test_display(&[0xf3, 0x0f, 0xae, 0xd4], "wrfsbase esp");
    test_display(&[0xf3, 0x0f, 0xae, 0xdc], "wrgsbase esp");
    test_display(&[0x66, 0x0f, 0xae, 0x3f], "clflushopt zmmword [edi]"); // or clflush without 66
    test_invalid(&[0x66, 0x0f, 0xae, 0xff]);
    test_display(&[0x66, 0x0f, 0xae, 0x37], "clwb zmmword [edi]");
    test_display(&[0x66, 0x0f, 0xae, 0xf7], "tpause edi");
    test_display(&[0xf3, 0x0f, 0xae, 0xf1], "umonitor ecx");
    test_display(&[0x67, 0xf3, 0x0f, 0xae, 0xf1], "umonitor cx");
    test_display(&[0xf2, 0x0f, 0xae, 0xf1], "umwait ecx");
    test_display(&[0x66, 0x0f, 0x38, 0x80, 0x2f], "invept ebp, xmmword [edi]");
    test_invalid(&[0x0f, 0x38, 0x80, 0x2f]);
    test_display(
        &[0x66, 0x0f, 0x38, 0x81, 0x2f],
        "invvpid ebp, xmmword [edi]",
    );
    test_invalid(&[0x0f, 0x38, 0x81, 0x2f]);
    test_display(
        &[0x66, 0x0f, 0x38, 0x82, 0x2f],
        "invpcid ebp, xmmword [edi]",
    );
    test_invalid(&[0x0f, 0x38, 0x82, 0x2f]);
    test_display(&[0x66, 0x0f, 0xae, 0xf1], "tpause ecx");
}

#[test]
fn evex() {
    test_display(
        &[0x62, 0xf2, 0x7d, 0x48, 0x2a, 0x44, 0x40, 0x01],
        "vmovntdqa zmm0, zmmword [eax + eax * 2 + 0x40]",
    );
    test_display(
        &[0x62, 0xf2, 0x7d, 0x08, 0x2a, 0x44, 0x40, 0x01],
        "vmovntdqa xmm0, xmmword [eax + eax * 2 + 0x10]",
    );
}

#[test]
fn vex() {
    fn test_instr(bytes: &[u8], text: &'static str) {
        test_display_under(&Decoder::minimal().with_avx(), bytes, text);
        test_display_under(&Decoder::default(), bytes, text);
        test_invalid_under(&Decoder::minimal(), bytes);
    }

    fn test_avx2(bytes: &[u8], text: &'static str) {
        test_display_under(&Decoder::minimal().with_avx().with_avx2(), bytes, text);
        test_display_under(&Decoder::default(), bytes, text);
        test_invalid_under(&Decoder::minimal(), bytes);
    }

    fn test_instr_vex_aesni(bytes: &[u8], text: &'static str) {
        test_display_under(&Decoder::minimal().with_avx().with_aesni(), bytes, text);
        test_display_under(&Decoder::default(), bytes, text);
        test_invalid_under(&Decoder::minimal(), bytes);
    }

    #[allow(dead_code)]
    fn test_instr_invalid(bytes: &[u8]) {
        test_invalid_under(&Decoder::minimal().with_avx(), bytes);
        test_invalid_under(&Decoder::default(), bytes);
    }

    // prefix 03
    test_invalid(&[0xc4, 0b110_00011, 0b1_1111_001, 0x00, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_101, 0x00, 0b11_001_010, 0x77]);
    test_avx2(
        &[0xc4, 0b110_00011, 0b1_1111_101, 0x00, 0b11_001_010, 0x77],
        "vpermq ymm1, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b1_1111_001, 0x01, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_101, 0x01, 0b11_001_010, 0x77]);
    test_avx2(
        &[0xc4, 0b110_00011, 0b1_1111_101, 0x01, 0b11_001_010, 0x77],
        "vpermpd ymm1, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b1_1111_001, 0x02, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b1_1111_101, 0x02, 0b11_001_010, 0x77]);
    test_avx2(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x02, 0b11_001_010, 0x77],
        "vpblendd xmm1, xmm0, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x02, 0b00_001_010, 0x77],
        "vpblendd xmm1, xmm0, xmmword [edx], 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x02, 0b11_001_010, 0x77],
        "vpblendd ymm1, ymm0, ymm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x02, 0b00_001_010, 0x77],
        "vpblendd ymm1, ymm0, ymmword [edx], 0x77",
    );

    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x04, 0b11_001_010, 0x77],
        "vpermilps xmm1, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x04, 0b11_001_010, 0x77],
        "vpermilps ymm1, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x05, 0b11_001_010, 0x77],
        "vpermilpd xmm1, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x05, 0b11_001_010, 0x77],
        "vpermilpd ymm1, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_001, 0x06, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x06, 0b11_001_010, 0x77],
        "vperm2f128 ymm1, ymm0, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x06, 0b00_001_010, 0x77],
        "vperm2f128 ymm1, ymm0, ymmword [edx], 0x77",
    );

    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x0c, 0b11_001_010, 0x77],
        "vblendps xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_101, 0x0c, 0b11_001_010, 0x77],
        "vblendps ymm1, ymm0, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x0d, 0b11_001_010, 0x77],
        "vblendpd xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_101, 0x0d, 0b11_001_010, 0x77],
        "vblendpd ymm1, ymm0, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x0e, 0b11_001_010, 0x77],
        "vpblendw xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_101, 0x0e, 0b11_001_010, 0x77],
        "vpblendw ymm1, ymm0, ymm2, 0x77",
    );

    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x08, 0b11_001_010, 0x77],
        "vroundps xmm1, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x08, 0b11_001_010, 0x77],
        "vroundps ymm1, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_001, 0x08, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x08, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x09, 0b11_001_010, 0x77],
        "vroundpd xmm1, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x09, 0b11_001_010, 0x77],
        "vroundpd ymm1, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_001, 0x09, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x09, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x0a, 0b11_001_010, 0x77],
        "vroundss xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_101, 0x0a, 0b11_001_010, 0x77],
        "vroundss xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x0b, 0b11_001_010, 0x77],
        "vroundsd xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_101, 0x0b, 0b11_001_010, 0x77],
        "vroundsd xmm1, xmm0, xmm2, 0x77",
    );

    test_instr(
        &[0xc4, 0b110_00011, 0b1_0111_001, 0x0f, 0b11_001_010, 0x77],
        "vpalignr xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b1_0111_101, 0x0f, 0b11_001_010, 0x77],
        "vpalignr ymm1, ymm0, ymm2, 0x77",
    );

    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x14, 0b11_001_010, 0x77],
        "vpextrb edx, xmm1, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x14, 0b00_001_010, 0x77],
        "vpextrb byte [edx], xmm1, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_001, 0x14, 0b00_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_101, 0x14, 0b00_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x15, 0b11_001_010, 0x77],
        "vpextrw edx, xmm1, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x15, 0b00_001_010, 0x77],
        "vpextrw word [edx], xmm1, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_001, 0x15, 0b00_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_101, 0x15, 0b00_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x16, 0b11_001_010, 0x77],
        "vpextrd edx, xmm1, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x16, 0b00_001_010, 0x77],
        "vpextrd dword [edx], xmm1, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_001, 0x16, 0b00_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_101, 0x16, 0b00_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b1_1111_001, 0x16, 0b11_001_010, 0x77],
        "vpextrd edx, xmm1, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b1_0111_001, 0x16, 0b00_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b1_1111_001, 0x16, 0b00_001_010, 0x77],
        "vpextrd dword [edx], xmm1, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x17, 0b11_001_010, 0x77],
        "vextractps edx, xmm1, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x17, 0b00_001_010, 0x77],
        "vextractps dword [edx], xmm1, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_101, 0x17, 0b00_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x17, 0b00_001_010, 0x77]);

    test_invalid(&[0xc4, 0b110_00011, 0b1_0111_001, 0x18, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_101, 0x18, 0b11_001_010, 0x77],
        "vinsertf128 ymm1, ymm0, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b1_0111_101, 0x18, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x19, 0b11_001_010, 0x77],
        "vextractf128 xmm2, ymm1, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_001, 0x19, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b1_1111_101, 0x19, 0b11_001_010, 0x77]);

    test_invalid(&[0xc4, 0b110_00011, 0b1_0111_001, 0x18, 0b11_001_010, 0x77]);
    test_avx2(
        &[0xc4, 0b110_00011, 0b0_0111_101, 0x38, 0b11_001_010, 0x77],
        "vinserti128 ymm1, ymm0, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b1_0111_101, 0x18, 0b11_001_010, 0x77]);
    test_avx2(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x39, 0b11_001_010, 0x77],
        "vextracti128 xmm2, ymm1, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_001, 0x19, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b1_1111_101, 0x19, 0b11_001_010, 0x77]);

    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x20, 0b11_001_010, 0x77],
        "vpinsrb xmm1, xmm0, edx, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x20, 0b00_001_010, 0x77],
        "vpinsrb xmm1, xmm0, byte [edx], 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x20, 0b00_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x21, 0b11_001_010, 0x77],
        "vinsertps xmm1, xmm0, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x21, 0b00_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x22, 0b11_001_010, 0x77],
        "vpinsrd xmm1, xmm0, edx, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x22, 0b00_001_010, 0x77],
        "vpinsrd xmm1, xmm0, dword [edx], 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x22, 0b00_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b1_0111_001, 0x22, 0b11_001_010, 0x77],
        "vpinsrd xmm1, xmm0, edx, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b1_0111_001, 0x22, 0b00_001_010, 0x77],
        "vpinsrd xmm1, xmm0, dword [edx], 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b1_0111_101, 0x22, 0b00_001_010, 0x77]);

    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x40, 0b11_001_010, 0x77],
        "vdpps xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_101, 0x40, 0b11_001_010, 0x77],
        "vdpps ymm1, ymm0, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x41, 0b11_001_010, 0x77],
        "vdppd xmm1, xmm0, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x41, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x42, 0b11_001_010, 0x77],
        "vmpsadbw xmm1, xmm0, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00011, 0b0_0111_101, 0x42, 0b11_001_010, 0x77],
        "vmpsadbw ymm1, ymm0, ymm2, 0x77",
    );

    test_avx2(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x46, 0b11_001_010, 0x77],
        "vperm2i128 ymm1, ymm0, ymm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00011, 0b0_1111_101, 0x46, 0b00_001_010, 0x77],
        "vperm2i128 ymm1, ymm0, ymmword [edx], 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_001, 0x46, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b1_1111_101, 0x46, 0b11_001_010, 0x77]);

    test_instr(
        &[0xc4, 0b110_00011, 0b0_0111_001, 0x4c, 0b11_001_010, 0x77],
        "vpblendvb xmm1, xmm0, xmm2, xmm7",
    );
    test_avx2(
        &[0xc4, 0b110_00011, 0b0_0111_101, 0x4c, 0b11_001_010, 0x77],
        "vpblendvb ymm1, ymm0, ymm2, ymm7",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b1_0111_001, 0x4c, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b1_0111_101, 0x4c, 0b11_001_010, 0x77]);

    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x60, 0b11_001_010, 0x77],
        "vpcmpestrm xmm1, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_101, 0x60, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_001, 0x60, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x60, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x61, 0b11_001_010, 0x77],
        "vpcmpestri xmm1, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_101, 0x61, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_001, 0x61, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x61, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x62, 0b11_001_010, 0x77],
        "vpcmpistrm xmm1, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_101, 0x62, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_001, 0x62, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x62, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00011, 0b0_1111_001, 0x63, 0b11_001_010, 0x77],
        "vpcmpistri xmm1, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b0_1111_101, 0x63, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_001, 0x63, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b0_0111_101, 0x63, 0b11_001_010, 0x77]);

    test_instr_vex_aesni(
        &[0xc4, 0b110_00011, 0b1_1111_001, 0xdf, 0b11_001_010, 0x77],
        "vaeskeygenassist xmm1, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00011, 0b1_0111_001, 0xdf, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00011, 0b1_0111_101, 0xdf, 0b11_001_010, 0x77]);

    // prefix 02
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x00, 0b11_001_010],
        "vpshufb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x00, 0b11_001_010],
        "vpshufb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x01, 0b11_001_010],
        "vphaddw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x01, 0b11_001_010],
        "vphaddw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x02, 0b11_001_010],
        "vphaddd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x02, 0b11_001_010],
        "vphaddd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x03, 0b11_001_010],
        "vphaddsw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x03, 0b11_001_010],
        "vphaddsw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x04, 0b11_001_010],
        "vpmaddubsw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x04, 0b11_001_010],
        "vpmaddubsw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x05, 0b11_001_010],
        "vphsubw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x05, 0b11_001_010],
        "vphsubw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x06, 0b11_001_010],
        "vphsubd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x06, 0b11_001_010],
        "vphsubd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x07, 0b11_001_010],
        "vphsubsw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x07, 0b11_001_010],
        "vphsubsw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x08, 0b11_001_010],
        "vpsignb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x08, 0b11_001_010],
        "vpsignb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x09, 0b11_001_010],
        "vpsignw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x09, 0b11_001_010],
        "vpsignw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x0a, 0b11_001_010],
        "vpsignd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x0a, 0b11_001_010],
        "vpsignd ymm1, ymm0, ymm2",
    );

    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x0b, 0b11_001_010],
        "vpmulhrsw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x0b, 0b11_001_010],
        "vpmulhrsw ymm1, ymm0, ymm2",
    );

    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x0c, 0b11_001_010],
        "vpermilps xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x0c, 0b11_001_010],
        "vpermilps ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x0d, 0b11_001_010],
        "vpermilpd xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x0d, 0b11_001_010],
        "vpermilpd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x0e, 0b11_001_010],
        "vtestps xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x0e, 0b11_001_010],
        "vtestps ymm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x0f, 0b11_001_010],
        "vtestpd xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x0f, 0b11_001_010],
        "vtestpd ymm1, ymm2",
    );

    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x16, 0b11_001_010],
        "vpermps ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x16, 0b00_001_010],
        "vpermps ymm1, ymm0, ymmword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_1111_001, 0x16, 0b00_011_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_1111_101, 0x16, 0b00_011_010]);

    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x17, 0b11_001_010],
        "vptest xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x17, 0b11_001_010],
        "vptest ymm1, ymm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x17, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x17, 0b11_001_010]);

    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x18, 0b00_001_010],
        "vbroadcastss xmm1, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x18, 0b00_001_010],
        "vbroadcastss ymm1, dword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b1_1111_001, 0x18, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x18, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x19, 0b00_001_010],
        "vbroadcastsd ymm1, qword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x19, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_101, 0x19, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x1a, 0b00_001_010],
        "vbroadcastf128 ymm1, xmmword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_101, 0x1a, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_001, 0x1a, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x5a, 0b00_001_010],
        "vbroadcasti128 ymm1, xmmword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_1111_101, 0x5a, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_1111_001, 0x5a, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_1111_101, 0x5a, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x18, 0b11_001_010],
        "vbroadcastss xmm1, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x18, 0b11_001_010],
        "vbroadcastss ymm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x18, 0b00_001_010],
        "vbroadcastss ymm1, dword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_001, 0x18, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_101, 0x18, 0b11_001_010]);
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x19, 0b11_001_010],
        "vbroadcastsd ymm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x19, 0b00_001_010],
        "vbroadcastsd ymm1, qword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b1_1111_101, 0x19, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_101, 0x19, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_001, 0x19, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_001, 0x1a, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_101, 0x1a, 0b11_001_010]);

    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x1c, 0b11_001_010],
        "vpabsb xmm1, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x1c, 0b11_001_010],
        "vpabsb ymm1, ymm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x1c, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x1d, 0b11_001_010],
        "vpabsw xmm1, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x1d, 0b11_001_010],
        "vpabsw ymm1, ymm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x1d, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x1e, 0b11_001_010],
        "vpabsd xmm1, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x1e, 0b11_001_010],
        "vpabsd ymm1, ymm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x1e, 0b11_001_010]);

    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x20, 0b11_001_010],
        "vpmovsxbw xmm1, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x20, 0b11_001_010],
        "vpmovsxbw ymm1, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x20, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x21, 0b11_001_010],
        "vpmovsxbd xmm1, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x21, 0b11_001_010],
        "vpmovsxbd ymm1, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x21, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x22, 0b11_001_010],
        "vpmovsxbq xmm1, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x22, 0b11_001_010],
        "vpmovsxbq ymm1, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x22, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x23, 0b11_001_010],
        "vpmovsxwd xmm1, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x23, 0b11_001_010],
        "vpmovsxwd ymm1, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x23, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x24, 0b11_001_010],
        "vpmovsxwq xmm1, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x24, 0b11_001_010],
        "vpmovsxwq ymm1, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x24, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x25, 0b11_001_010],
        "vpmovsxdq xmm1, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x25, 0b11_001_010],
        "vpmovsxdq ymm1, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x25, 0b11_001_010]);

    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x28, 0b11_001_010],
        "vpmuldq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x28, 0b11_001_010],
        "vpmuldq ymm1, ymm0, ymm2",
    );

    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x29, 0b11_001_010],
        "vpcmpeqq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x29, 0b11_001_010],
        "vpcmpeqq ymm1, ymm0, ymm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x2a, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x2a, 0b00_001_010],
        "vmovntdqa xmm1, xmmword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x2a, 0b00_001_010]);
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x2a, 0b00_001_010],
        "vmovntdqa ymm1, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x2b, 0b11_001_010],
        "vpackusdw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x2b, 0b11_001_010],
        "vpackusdw ymm1, ymm0, ymm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x2b, 0b00_001_010],
        "vpackusdw ymm1, ymm0, ymmword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x30, 0b11_001_010],
        "vpmovzxbw xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x30, 0b11_001_010],
        "vpmovzxbw ymm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x31, 0b11_001_010],
        "vpmovzxbd xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x31, 0b11_001_010],
        "vpmovzxbd ymm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x32, 0b11_001_010],
        "vpmovzxbq xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x32, 0b11_001_010],
        "vpmovzxbq ymm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x33, 0b11_001_010],
        "vpmovzxwd xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x33, 0b11_001_010],
        "vpmovzxwd ymm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x34, 0b11_001_010],
        "vpmovzxwq xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x34, 0b11_001_010],
        "vpmovzxwq ymm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x35, 0b11_001_010],
        "vpmovzxdq xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x35, 0b11_001_010],
        "vpmovzxdq ymm1, xmm2",
    );

    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x30, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x30, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x31, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x31, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x32, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x32, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x33, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x33, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x34, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x34, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x35, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x35, 0b11_001_010]);

    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x36, 0b11_001_010]);
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x36, 0b11_001_010],
        "vpermd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x37, 0b11_001_010],
        "vpcmpgtq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x37, 0b11_001_010],
        "vpcmpgtq ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x38, 0b11_001_010],
        "vpminsb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x38, 0b11_001_010],
        "vpminsb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x39, 0b11_001_010],
        "vpminsd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x39, 0b11_001_010],
        "vpminsd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x3a, 0b11_001_010],
        "vpminuw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x3a, 0b11_001_010],
        "vpminuw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x3b, 0b11_001_010],
        "vpminud xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x3b, 0b11_001_010],
        "vpminud ymm1, ymm0, ymm2",
    );

    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x3c, 0b11_001_010],
        "vpmaxsb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x3c, 0b11_001_010],
        "vpmaxsb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x3d, 0b11_001_010],
        "vpmaxsd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x3d, 0b11_001_010],
        "vpmaxsd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x3e, 0b11_001_010],
        "vpmaxuw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x3e, 0b11_001_010],
        "vpmaxuw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x3f, 0b11_001_010],
        "vpmaxud xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x3f, 0b11_001_010],
        "vpmaxud ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_0111_001, 0x40, 0b11_001_010],
        "vpmulld xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_0111_101, 0x40, 0b11_001_010],
        "vpmulld ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x41, 0b11_001_010],
        "vphminposuw xmm1, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_001, 0x41, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0x41, 0b11_001_010]);
    // TODO: should something b11at opcode 42 here?
    //    test_instr(&[0xc4, 0b110_00010, 0b1_0111_001, 0x42, 0b11_001_010], "vphminposuw xmm");
    //    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_101, 0x41, 0b11_001_010]);

    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x45, 0b00_001_010],
        "vpsrlvd xmm1, xmm0, xmmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x45, 0b00_001_010],
        "vpsrlvd ymm1, ymm0, ymmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x45, 0b11_001_010],
        "vpsrlvd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x45, 0b11_001_010],
        "vpsrlvd ymm1, ymm0, ymm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_001, 0x45, 0b00_001_010],
        "vpsrlvq xmm1, xmm0, xmmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_101, 0x45, 0b00_001_010],
        "vpsrlvq ymm1, ymm0, ymmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_001, 0x45, 0b11_001_010],
        "vpsrlvq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_101, 0x45, 0b11_001_010],
        "vpsrlvq ymm1, ymm0, ymm2",
    );

    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x46, 0b00_001_010],
        "vpsravd xmm1, xmm0, xmmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x46, 0b00_001_010],
        "vpsravd ymm1, ymm0, ymmword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b1_1111_001, 0x46, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_1111_101, 0x46, 0b00_001_010]);

    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x47, 0b00_001_010],
        "vpsllvd xmm1, xmm0, xmmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x47, 0b00_001_010],
        "vpsllvd ymm1, ymm0, ymmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x47, 0b11_001_010],
        "vpsllvd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x47, 0b11_001_010],
        "vpsllvd ymm1, ymm0, ymm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_001, 0x47, 0b00_001_010],
        "vpsllvq xmm1, xmm0, xmmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_101, 0x47, 0b00_001_010],
        "vpsllvq ymm1, ymm0, ymmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_001, 0x47, 0b11_001_010],
        "vpsllvq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_101, 0x47, 0b11_001_010],
        "vpsllvq ymm1, ymm0, ymm2",
    );

    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x8c, 0b00_001_010],
        "vpmaskmovd xmm1, xmm0, xmmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x8c, 0b00_001_010],
        "vpmaskmovd ymm1, ymm0, ymmword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_1111_001, 0x8c, 0b11_001_010]);
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_001, 0x8c, 0b00_001_010],
        "vpmaskmovq xmm1, xmm0, xmmword [edx]",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_101, 0x8c, 0b00_001_010],
        "vpmaskmovq ymm1, ymm0, ymmword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_1111_001, 0x8c, 0b11_001_010]);

    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0x8e, 0b00_001_010],
        "vpmaskmovd xmmword [edx], xmm0, xmm1",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b0_1111_101, 0x8e, 0b00_001_010],
        "vpmaskmovd ymmword [edx], ymm0, ymm1",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_1111_001, 0x8e, 0b11_001_010]);
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_001, 0x8e, 0b00_001_010],
        "vpmaskmovq xmmword [edx], xmm0, xmm1",
    );
    test_avx2(
        &[0xc4, 0b110_00010, 0b1_1111_101, 0x8e, 0b00_001_010],
        "vpmaskmovq ymmword [edx], ymm0, ymm1",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_1111_001, 0x8e, 0b11_001_010]);

    /*
    test_avx2(&[0xc4, 0b110_00010, 0b0_1111_001, 0x90, 0b00_000_100, 0xa1], "vpgatherdd xmm0, dword [ecx + xmm12 * 4], xmm0");
    test_avx2(&[0xc4, 0b110_00010, 0b0_1111_101, 0x90, 0b00_000_100, 0xa1], "vpgatherdd ymm0, dword [ecx + ymm12 * 4], ymm0");
    test_avx2(&[0xc4, 0b110_00010, 0b1_1111_001, 0x90, 0b00_000_100, 0xa1], "vpgatherdq xmm0, dword [ecx + xmm12 * 4], xmm0");
    test_avx2(&[0xc4, 0b110_00010, 0b1_1111_101, 0x90, 0b00_000_100, 0xa1], "vpgatherdq ymm0, qword [ecx + ymm12 * 4], ymm0");

    test_avx2(&[0xc4, 0b000_00010, 0b0_1111_001, 0x91, 0b00_000_100, 0xa1], "vpgatherqd xmm0, dword xmmword [ecx + xmm12 * 4], xmm0");
    test_avx2(&[0xc4, 0b000_00010, 0b0_1111_101, 0x91, 0b00_000_100, 0xa1], "vpgatherqd xmm0, dword xmmword [ecx + ymm12 * 4], xmm0");
    test_avx2(&[0xc4, 0b000_00010, 0b1_1111_001, 0x91, 0b00_000_100, 0xa1], "vpgatherqq xmm0, dword xmmword [ecx + xmm12 * 4], xmm0");
    test_avx2(&[0xc4, 0b000_00010, 0b1_1111_101, 0x91, 0b00_000_100, 0xa1], "vpgatherqq ymm0, qword xmmword [ecx + ymm12 * 4], ymm0");

    test_avx2(&[0xc4, 0b110_00010, 0b0_1111_001, 0x92, 0b00_000_100, 0xa1], "vgatherdps xmm0, dword [ecx + xmm12 * 4], xmm0");
    test_avx2(&[0xc4, 0b110_00010, 0b0_1111_101, 0x92, 0b00_000_100, 0xa1], "vgatherdps ymm0, qword [ecx + ymm12 * 4], ymm0");
    test_avx2(&[0xc4, 0b110_00010, 0b1_1111_001, 0x92, 0b00_000_100, 0xa1], "vgatherdpd xmm0, dword [ecx + xmm12 * 4], xmm0");
    test_avx2(&[0xc4, 0b110_00010, 0b1_1111_101, 0x92, 0b00_000_100, 0xa1], "vgatherdpd ymm0, qword [ecx + ymm12 * 4], ymm0");

    test_avx2(&[0xc4, 0b110_00010, 0b0_1111_001, 0x93, 0b00_000_100, 0xa1], "vgatherqps xmm0, dword [ecx + xmm12 * 4], xmm0");
    test_avx2(&[0xc4, 0b110_00010, 0b0_1111_101, 0x93, 0b00_000_100, 0xa1], "vgatherqps ymm0, qword [ecx + ymm12 * 4], ymm0");
    test_avx2(&[0xc4, 0b110_00010, 0b1_1111_001, 0x93, 0b00_000_100, 0xa1], "vgatherqpd xmm0, dword [ecx + xmm12 * 4], xmm0");
    test_avx2(&[0xc4, 0b110_00010, 0b1_1111_101, 0x93, 0b00_000_100, 0xa1], "vgatherqpd ymm0, qword [ecx + ymm12 * 4], ymm0");
    */

    test_instr_vex_aesni(
        &[0xc4, 0b110_00010, 0b0_1111_001, 0xdb, 0b11_001_010],
        "vaesimc xmm1, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00010, 0b0_0111_101, 0xdb, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00010, 0b1_0111_101, 0xdb, 0b11_001_010]);
    test_instr_vex_aesni(
        &[0xc4, 0b110_00010, 0b1_0111_001, 0xdc, 0b11_001_010],
        "vaesenc xmm1, xmm0, xmm2",
    );
    test_instr_vex_aesni(
        &[0xc4, 0b110_00010, 0b1_0111_101, 0xdc, 0b11_001_010],
        "vaesenc ymm1, ymm0, ymm2",
    );
    test_instr_vex_aesni(
        &[0xc4, 0b110_00010, 0b1_0111_001, 0xdd, 0b11_001_010],
        "vaesenclast xmm1, xmm0, xmm2",
    );
    test_instr_vex_aesni(
        &[0xc4, 0b110_00010, 0b1_0111_101, 0xdd, 0b11_001_010],
        "vaesenclast ymm1, ymm0, ymm2",
    );
    test_instr_vex_aesni(
        &[0xc4, 0b110_00010, 0b1_0111_001, 0xde, 0b11_001_010],
        "vaesdec xmm1, xmm0, xmm2",
    );
    test_instr_vex_aesni(
        &[0xc4, 0b110_00010, 0b1_0111_101, 0xde, 0b11_001_010],
        "vaesdec ymm1, ymm0, ymm2",
    );
    test_instr_vex_aesni(
        &[0xc4, 0b110_00010, 0b1_0111_001, 0xdf, 0b11_001_010],
        "vaesdeclast xmm1, xmm0, xmm2",
    );
    test_instr_vex_aesni(
        &[0xc4, 0b110_00010, 0b1_0111_101, 0xdf, 0b11_001_010],
        "vaesdeclast ymm1, ymm0, ymm2",
    );

    // prefix 01
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_011, 0x10, 0b00_001_010],
        "vmovsd xmm1, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_111, 0x10, 0b00_001_010],
        "vmovsd xmm1, qword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_011, 0x10, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_111, 0x10, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x10, 0b00_001_010],
        "vmovupd xmm1, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x10, 0b00_001_010],
        "vmovupd ymm1, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_011, 0x11, 0b00_001_010],
        "vmovsd qword [edx], xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_111, 0x11, 0b00_001_010],
        "vmovsd qword [edx], xmm1",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_011, 0x11, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_111, 0x11, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x10, 0b00_001_010],
        "vmovupd xmm1, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x10, 0b00_001_010],
        "vmovupd ymm1, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x10, 0b00_001_010],
        "vmovss xmm1, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x10, 0b00_001_010],
        "vmovss xmm1, dword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_010, 0x10, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_110, 0x10, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x10, 0b00_001_010],
        "vmovups xmm1, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x10, 0b00_001_010],
        "vmovups ymm1, ymmword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_011, 0x11, 0b11_001_010],
        "vmovsd xmm2, xmm0, xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_111, 0x11, 0b11_001_010],
        "vmovsd xmm2, xmm0, xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x11, 0b00_001_010],
        "vmovss dword [edx], xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x11, 0b00_001_010],
        "vmovss dword [edx], xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x11, 0b00_001_010],
        "vmovups xmmword [edx], xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x11, 0b00_001_010],
        "vmovups ymmword [edx], ymm1",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_011, 0x12, 0b00_001_010],
        "vmovddup xmm1, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_111, 0x12, 0b00_001_010],
        "vmovddup ymm1, ymmword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b0_0111_011, 0x12, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_0111_111, 0x12, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x12, 0b11_001_010],
        "vmovhlps xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x12, 0b00_001_010],
        "vmovlps xmm1, xmm0, qword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_100, 0x12, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_111, 0x12, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x12, 0b00_001_010],
        "vmovsldup xmm1, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x12, 0b00_001_010],
        "vmovsldup ymm1, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_010, 0x12, 0b00_001_010],
        "vmovsldup xmm1, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_110, 0x12, 0b00_001_010],
        "vmovsldup ymm1, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x12, 0b00_001_010],
        "vmovlpd xmm1, xmm0, qword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_101, 0x12, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x13, 0b00_001_010],
        "vmovlpd qword [edx], xmm1",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_001, 0x13, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_101, 0x13, 0b00_001_010]);

    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_000, 0x14, 0b00_001_010],
        "vunpcklps xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_100, 0x14, 0b00_001_010],
        "vunpcklps ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x14, 0b00_001_010],
        "vunpcklpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x14, 0b00_001_010],
        "vunpcklpd ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_000, 0x15, 0b00_001_010],
        "vunpckhps xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_100, 0x15, 0b00_001_010],
        "vunpckhps ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x15, 0b00_001_010],
        "vunpckhpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x15, 0b00_001_010],
        "vunpckhpd ymm1, ymm0, ymmword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x16, 0b11_001_010],
        "vmovshdup xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x16, 0b11_001_010],
        "vmovshdup ymm1, ymm2",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_010, 0x16, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_110, 0x16, 0b11_001_010]);

    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x16, 0b00_001_010],
        "vmovhps xmm1, xmm0, qword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_100, 0x16, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x16, 0b00_001_010],
        "vmovhpd xmm1, xmm0, qword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_101, 0x16, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_001, 0x16, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x17, 0b00_001_010],
        "vmovhps qword [edx], xmm1",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_100, 0x17, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_000, 0x17, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_100, 0x17, 0b00_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x17, 0b00_001_010],
        "vmovhpd qword [edx], xmm1",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_001, 0x17, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_101, 0x17, 0b00_001_010]);

    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_000, 0x28, 0b11_001_010],
        "vmovaps xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x28, 0b11_001_010],
        "vmovaps ymm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_000, 0x29, 0b11_001_010],
        "vmovaps xmm2, xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x29, 0b11_001_010],
        "vmovaps ymm2, ymm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x28, 0b11_001_010],
        "vmovapd xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x28, 0b11_001_010],
        "vmovapd ymm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x29, 0b11_001_010],
        "vmovapd xmm2, xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x29, 0b11_001_010],
        "vmovapd ymm2, ymm1",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_010, 0x2a, 0b11_001_010],
        "vcvtsi2ss xmm1, xmm0, edx",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_010, 0x2a, 0b00_001_010],
        "vcvtsi2ss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x2a, 0b11_001_010],
        "vcvtsi2ss xmm1, xmm0, edx",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x2a, 0b00_001_010],
        "vcvtsi2ss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x2a, 0b11_001_010],
        "vcvtsi2ss xmm1, xmm0, edx",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_011, 0x2a, 0b11_001_010],
        "vcvtsi2sd xmm1, xmm0, edx",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_111, 0x2a, 0b11_001_010],
        "vcvtsi2sd xmm1, xmm0, edx",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_111, 0x2a, 0b11_001_010],
        "vcvtsi2sd xmm1, xmm0, edx",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_011, 0x2a, 0b00_001_010],
        "vcvtsi2sd xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_011, 0x2a, 0b00_001_010],
        "vcvtsi2sd xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc5, 0b1_1111_011, 0x2a, 0b11_001_010],
        "vcvtsi2sd xmm1, xmm0, edx",
    );
    test_instr(
        &[0xc5, 0b1_1111_111, 0x2a, 0b11_001_010],
        "vcvtsi2sd xmm1, xmm0, edx",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_000, 0x2b, 0b00_001_010],
        "vmovntps xmmword [edx], xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x2b, 0b00_001_010],
        "vmovntps ymmword [edx], ymm1",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_000, 0x2b, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_100, 0x2b, 0b11_001_010]);

    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x2b, 0b00_001_010],
        "vmovntpd xmmword [edx], xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x2b, 0b00_001_010],
        "vmovntpd ymmword [edx], ymm1",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_001, 0x2b, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_101, 0x2b, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_010, 0x2c, 0b11_001_010],
        "vcvttss2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_110, 0x2c, 0b11_001_010],
        "vcvttss2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x2c, 0b11_001_010],
        "vcvttss2si ecx, xmm2",
    );
    test_instr(
        &[0xc5, 0b1_1111_010, 0x2c, 0b11_001_010],
        "vcvttss2si ecx, xmm2",
    );
    test_instr(
        &[0xc5, 0b1_1111_010, 0x2c, 0b00_001_010],
        "vcvttss2si ecx, dword [edx]",
    );
    test_instr(
        &[0xc5, 0b1_1111_110, 0x2c, 0b11_001_010],
        "vcvttss2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_011, 0x2c, 0b11_001_010],
        "vcvttsd2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_111, 0x2c, 0b11_001_010],
        "vcvttsd2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_111, 0x2c, 0b11_001_010],
        "vcvttsd2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_111, 0x2c, 0b00_001_010],
        "vcvttsd2si ecx, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_111, 0x2c, 0b00_001_010],
        "vcvttsd2si ecx, qword [edx]",
    );
    test_instr(
        &[0xc5, 0b1_1111_011, 0x2c, 0b11_001_010],
        "vcvttsd2si ecx, xmm2",
    );
    test_instr(
        &[0xc5, 0b1_1111_111, 0x2c, 0b11_001_010],
        "vcvttsd2si ecx, xmm2",
    );
    test_instr(
        &[0xc5, 0b1_1111_111, 0x2c, 0b00_001_010],
        "vcvttsd2si ecx, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_010, 0x2d, 0b11_001_010],
        "vcvtss2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_110, 0x2d, 0b11_001_010],
        "vcvtss2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_110, 0x2d, 0b00_001_010],
        "vcvtss2si ecx, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x2d, 0b00_001_010],
        "vcvtss2si ecx, dword [edx]",
    );
    test_instr(
        &[0xc5, 0b1_1111_010, 0x2d, 0b11_001_010],
        "vcvtss2si ecx, xmm2",
    );
    test_instr(
        &[0xc5, 0b1_1111_110, 0x2d, 0b11_001_010],
        "vcvtss2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_011, 0x2d, 0b11_001_010],
        "vcvtsd2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_011, 0x2d, 0b00_001_010],
        "vcvtsd2si ecx, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_111, 0x2d, 0b11_001_010],
        "vcvtsd2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_111, 0x2d, 0b00_001_010],
        "vcvtsd2si ecx, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_111, 0x2d, 0b11_001_010],
        "vcvtsd2si ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_011, 0x2d, 0b00_001_010],
        "vcvtsd2si ecx, qword [edx]",
    );
    test_instr(
        &[0xc5, 0b1_1111_011, 0x2d, 0b11_001_010],
        "vcvtsd2si ecx, xmm2",
    );
    test_instr(
        &[0xc5, 0b1_1111_111, 0x2d, 0b11_001_010],
        "vcvtsd2si ecx, xmm2",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x2e, 0b00_001_010],
        "vucomisd xmm1, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_101, 0x2e, 0b00_001_010],
        "vucomisd xmm1, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x2e, 0b11_001_010],
        "vucomisd xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_101, 0x2e, 0b11_001_010],
        "vucomisd xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x2f, 0b00_001_010],
        "vcomisd xmm1, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_101, 0x2f, 0b00_001_010],
        "vcomisd xmm1, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x2f, 0b11_001_010],
        "vcomisd xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_101, 0x2f, 0b11_001_010],
        "vcomisd xmm1, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b0_0111_001, 0x2e, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_0111_101, 0x2e, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_0111_001, 0x2e, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_0111_101, 0x2e, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_0111_001, 0x2f, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_0111_101, 0x2f, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_0111_001, 0x2f, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_0111_101, 0x2f, 0b11_001_010]);

    test_instr(
        &[0xc5, 0b1_1111_000, 0x2e, 0b11_001_010],
        "vucomiss xmm1, xmm2",
    );
    test_instr(
        &[0xc5, 0b1_1111_100, 0x2e, 0b00_001_010],
        "vucomiss xmm1, dword [edx]",
    );
    test_instr(
        &[0xc5, 0b1_1111_000, 0x2f, 0b11_001_010],
        "vcomiss xmm1, xmm2",
    );
    test_instr(
        &[0xc5, 0b1_1111_100, 0x2f, 0b00_001_010],
        "vcomiss xmm1, dword [edx]",
    );
    test_invalid(&[0xc5, 0b1_1111_111, 0x2f, 0b11_001_010]);

    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x50, 0b11_001_010],
        "vmovmskps ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x50, 0b11_001_010],
        "vmovmskps ecx, ymm2",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_000, 0x50, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_100, 0x50, 0b00_001_010]);

    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x50, 0b11_001_010],
        "vmovmskpd ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x50, 0b11_001_010],
        "vmovmskpd ecx, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x50, 0b11_001_010],
        "vmovmskpd ecx, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_101, 0x50, 0b11_001_010],
        "vmovmskpd ecx, ymm2",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_001, 0x50, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_101, 0x50, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_001, 0x50, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_101, 0x50, 0b00_001_010]);

    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x51, 0b00_001_010],
        "vsqrtpd xmm1, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_101, 0x51, 0b00_001_010],
        "vsqrtpd ymm1, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_011, 0x51, 0b00_001_010],
        "vsqrtsd xmm1, xmm0, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_111, 0x51, 0b00_001_010],
        "vsqrtsd xmm1, xmm0, qword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x51, 0b00_001_010],
        "vsqrtps xmm1, xmmword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_000, 0x51, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x51, 0b00_001_010],
        "vsqrtps ymm1, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x51, 0b00_001_010],
        "vsqrtss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x51, 0b00_001_010],
        "vsqrtss xmm1, xmm0, dword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x52, 0b11_001_010],
        "vrsqrtps xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x52, 0b11_001_010],
        "vrsqrtps ymm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x52, 0b11_001_010],
        "vrsqrtss xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x52, 0b11_001_010],
        "vrsqrtss xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x53, 0b11_001_010],
        "vrcpps xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x53, 0b11_001_010],
        "vrcpps ymm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x53, 0b11_001_010],
        "vrcpss xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x53, 0b11_001_010],
        "vrcpss xmm1, xmm0, xmm2",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x54, 0b11_001_010],
        "vandps xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_100, 0x54, 0b11_001_010],
        "vandps ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x55, 0b11_001_010],
        "vandnps xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_100, 0x55, 0b11_001_010],
        "vandnps ymm1, ymm0, ymm2",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x54, 0b00_001_010],
        "vandpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x54, 0b00_001_010],
        "vandpd ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x55, 0b00_001_010],
        "vandnpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x55, 0b00_001_010],
        "vandnpd ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x56, 0b00_001_010],
        "vorpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x56, 0b00_001_010],
        "vorpd ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x56, 0b00_001_010],
        "vorps xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_100, 0x56, 0b00_001_010],
        "vorps ymm1, ymm0, ymmword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x57, 0b11_001_010],
        "vxorps xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_100, 0x57, 0b11_001_010],
        "vxorps ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x57, 0b11_001_010],
        "vxorpd xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x57, 0b11_001_010],
        "vxorpd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x58, 0b11_001_010],
        "vaddps xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_100, 0x58, 0b11_001_010],
        "vaddps ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_010, 0x58, 0b11_001_010],
        "vaddss xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_110, 0x58, 0b11_001_010],
        "vaddss xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_010, 0x58, 0b00_001_010],
        "vaddss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_110, 0x58, 0b00_001_010],
        "vaddss xmm1, xmm0, dword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x58, 0b00_001_010],
        "vaddpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x58, 0b00_001_010],
        "vaddpd ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_011, 0x58, 0b00_001_010],
        "vaddsd xmm1, xmm0, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_111, 0x58, 0b00_001_010],
        "vaddsd xmm1, xmm0, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x59, 0b00_001_010],
        "vmulps xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_100, 0x59, 0b00_001_010],
        "vmulps ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x59, 0b00_001_010],
        "vmulpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x59, 0b00_001_010],
        "vmulpd ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_010, 0x59, 0b00_001_010],
        "vmulss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_110, 0x59, 0b00_001_010],
        "vmulss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_011, 0x59, 0b00_001_010],
        "vmulsd xmm1, xmm0, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_111, 0x59, 0b00_001_010],
        "vmulsd xmm1, xmm0, qword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x5a, 0b11_001_010],
        "vcvtps2pd xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x5a, 0b11_001_010],
        "vcvtps2pd ymm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x5a, 0b00_001_010],
        "vcvtps2pd xmm1, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x5a, 0b00_001_010],
        "vcvtps2pd ymm1, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x5a, 0b11_001_010],
        "vcvtpd2ps xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x5a, 0b11_001_010],
        "vcvtpd2ps xmm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_011, 0x5a, 0b11_001_010],
        "vcvtsd2ss xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_111, 0x5a, 0b11_001_010],
        "vcvtsd2ss xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x5a, 0b11_001_010],
        "vcvtss2sd xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x5a, 0b11_001_010],
        "vcvtss2sd xmm1, xmm0, xmm2",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x5b, 0b11_001_010],
        "vcvtps2dq xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x5b, 0b11_001_010],
        "vcvtps2dq ymm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x5b, 0b11_001_010],
        "vcvttps2dq xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x5b, 0b11_001_010],
        "vcvttps2dq ymm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x5b, 0b11_001_010],
        "vcvtdq2ps xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_000, 0x5b, 0b00_001_010],
        "vcvtdq2ps xmm1, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x5b, 0b11_001_010],
        "vcvtdq2ps ymm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_100, 0x5b, 0b00_001_010],
        "vcvtdq2ps ymm1, ymmword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_000, 0x5c, 0b00_001_010],
        "vsubps xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_100, 0x5c, 0b00_001_010],
        "vsubps ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_010, 0x5c, 0b00_001_010],
        "vsubss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_110, 0x5c, 0b00_001_010],
        "vsubss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x5c, 0b00_001_010],
        "vsubpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_101, 0x5c, 0b00_001_010],
        "vsubpd ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_011, 0x5c, 0b00_001_010],
        "vsubsd xmm1, xmm0, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_111, 0x5c, 0b00_001_010],
        "vsubsd xmm1, xmm0, qword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x5d, 0b00_001_010],
        "vminps xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_100, 0x5d, 0b00_001_010],
        "vminps ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_010, 0x5d, 0b00_001_010],
        "vminss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_110, 0x5d, 0b00_001_010],
        "vminss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x5d, 0b00_001_010],
        "vminpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x5d, 0b00_001_010],
        "vminpd ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_011, 0x5d, 0b00_001_010],
        "vminsd xmm1, xmm0, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_111, 0x5d, 0b00_001_010],
        "vminsd xmm1, xmm0, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x5e, 0b00_001_010],
        "vdivps xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x5e, 0b00_001_010],
        "vdivps xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x5e, 0b00_001_010],
        "vdivpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_010, 0x5e, 0b00_001_010],
        "vdivss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_011, 0x5e, 0b00_001_010],
        "vdivsd xmm1, xmm0, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_100, 0x5e, 0b00_001_010],
        "vdivps ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x5e, 0b00_001_010],
        "vdivpd ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_110, 0x5e, 0b00_001_010],
        "vdivss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_111, 0x5e, 0b00_001_010],
        "vdivsd xmm1, xmm0, qword [edx]",
    );

    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0x5f, 0b00_001_010],
        "vmaxps xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x5f, 0b00_001_010],
        "vmaxpd xmm1, xmm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_010, 0x5f, 0b00_001_010],
        "vmaxss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_011, 0x5f, 0b00_001_010],
        "vmaxsd xmm1, xmm0, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_100, 0x5f, 0b00_001_010],
        "vmaxps ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x5f, 0b00_001_010],
        "vmaxpd ymm1, ymm0, ymmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_110, 0x5f, 0b00_001_010],
        "vmaxss xmm1, xmm0, dword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_111, 0x5f, 0b00_001_010],
        "vmaxsd xmm1, xmm0, qword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x60, 0b11_001_010],
        "vpunpcklbw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x60, 0b11_001_010],
        "vpunpcklbw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x61, 0b11_001_010],
        "vpunpcklwd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x61, 0b11_001_010],
        "vpunpcklwd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x62, 0b11_001_010],
        "vpunpckldq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x62, 0b11_001_010],
        "vpunpckldq ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x63, 0b11_001_010],
        "vpacksswb xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x63, 0b11_001_010],
        "vpacksswb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x64, 0b11_001_010],
        "vpcmpgtb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x64, 0b11_001_010],
        "vpcmpgtb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x65, 0b11_001_010],
        "vpcmpgtw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x65, 0b11_001_010],
        "vpcmpgtw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x66, 0b11_001_010],
        "vpcmpgtd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x66, 0b11_001_010],
        "vpcmpgtd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x67, 0b11_001_010],
        "vpackuswb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x67, 0b11_001_010],
        "vpackuswb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x68, 0b11_001_010],
        "vpunpckhbw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x68, 0b11_001_010],
        "vpunpckhbw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x69, 0b11_001_010],
        "vpunpckhwd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x69, 0b11_001_010],
        "vpunpckhwd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x6a, 0b11_001_010],
        "vpunpckhdq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x6a, 0b11_001_010],
        "vpunpckhdq ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0x6b, 0b11_001_010],
        "vpackssdw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0x6b, 0b11_001_010],
        "vpackssdw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x6c, 0b11_001_010],
        "vpunpcklqdq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x6c, 0b11_001_010],
        "vpunpcklqdq ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x6d, 0b11_001_010],
        "vpunpckhqdq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x6d, 0b11_001_010],
        "vpunpckhqdq ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x6e, 0b11_001_010],
        "vmovd xmm1, edx",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x6e, 0b00_001_010],
        "vmovd xmm1, dword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_101, 0x6e, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x6f, 0b11_001_010],
        "vmovdqa xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_101, 0x6f, 0b11_001_010],
        "vmovdqa ymm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_010, 0x6f, 0b11_001_010],
        "vmovdqu xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_110, 0x6f, 0b11_001_010],
        "vmovdqu ymm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x70, 0b11_001_010, 0x77],
        "vpshufd xmm1, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x70, 0b11_001_010, 0x77],
        "vpshufd ymm1, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0x70, 0b11_001_010, 0x77],
        "vpshufhw xmm1, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0x70, 0b11_001_010, 0x77],
        "vpshufhw ymm1, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_011, 0x70, 0b11_001_010, 0x77],
        "vpshuflw xmm1, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_111, 0x70, 0b11_001_010, 0x77],
        "vpshuflw ymm1, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b00_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b11_010_010, 0x77],
        "vpsrlw xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x71, 0b11_010_010, 0x77],
        "vpsrlw xmm0, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x71, 0b11_010_010, 0x77],
        "vpsrlw ymm0, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b00_011_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b11_100_010, 0x77],
        "vpsraw xmm0, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x71, 0b11_100_010, 0x77],
        "vpsraw ymm0, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b11_101_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x71, 0b11_110_010, 0x77],
        "vpsllw xmm0, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x71, 0b11_110_010, 0x77],
        "vpsllw ymm0, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_000_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b11_010_010, 0x77],
        "vpsrld xmm0, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x72, 0b11_010_010, 0x77],
        "vpsrld ymm0, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_101, 0x72, 0b11_011_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b11_100_010, 0x77],
        "vpsrad xmm0, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_100_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x72, 0b11_100_010, 0x77],
        "vpsrad ymm0, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_101_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b11_110_010, 0x77],
        "vpslld xmm0, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_110_010, 0x77]);
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x72, 0b11_110_010, 0x77],
        "vpslld ymm0, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x72, 0b00_111_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_000_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_010_010, 0x77],
        "vpsrlq xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_011_010, 0x77],
        "vpsrldq xmm0, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x73, 0b11_010_010, 0x77],
        "vpsrlq ymm0, ymm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x73, 0b11_011_010, 0x77],
        "vpsrldq ymm0, ymm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_100_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_101_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_110_010, 0x77],
        "vpsllq xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x73, 0b11_111_010, 0x77],
        "vpslldq xmm0, xmm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x73, 0b11_110_010, 0x77],
        "vpsllq ymm0, ymm2, 0x77",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0x73, 0b11_111_010, 0x77],
        "vpslldq ymm0, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x74, 0b11_001_010],
        "vpcmpeqb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x74, 0b11_001_010],
        "vpcmpeqb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x75, 0b11_001_010],
        "vpcmpeqw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x75, 0b11_001_010],
        "vpcmpeqw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x76, 0b11_001_010],
        "vpcmpeqd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x76, 0b11_001_010],
        "vpcmpeqd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x7c, 0b11_001_010],
        "vhaddpd xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x7c, 0b11_001_010],
        "vhaddpd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_011, 0x7c, 0b11_001_010],
        "vhaddps xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_111, 0x7c, 0b11_001_010],
        "vhaddps ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0x7d, 0b11_001_010],
        "vhsubpd xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0x7d, 0b11_001_010],
        "vhsubpd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_011, 0x7d, 0b11_001_010],
        "vhsubps xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_111, 0x7d, 0b11_001_010],
        "vhsubps ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x7e, 0b11_001_010],
        "vmovd edx, xmm1",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_101, 0x7e, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0x7e, 0b11_001_010],
        "vmovd edx, xmm1",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_101, 0x7e, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0x7f, 0b11_001_010],
        "vmovdqa xmm2, xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_101, 0x7f, 0b11_001_010],
        "vmovdqa ymm2, ymm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_010, 0x7f, 0b11_001_010],
        "vmovdqu xmm2, xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_110, 0x7f, 0b11_001_010],
        "vmovdqu ymm2, ymm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_000, 0xae, 0b00_010_001],
        "vldmxcsr dword [ecx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_100, 0xae, 0b00_010_001]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_000, 0xae, 0b11_010_001]);
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_000, 0xae, 0b00_011_001],
        "vstmxcsr dword [ecx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_100, 0xae, 0b00_011_001]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_000, 0xae, 0b11_011_001]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_000, 0xc2, 0b11_001_010, 0x77],
        "vcmpps xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_100, 0xc2, 0b11_001_010, 0x77],
        "vcmpps ymm1, ymm0, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xc2, 0b11_001_010, 0x77],
        "vcmppd xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xc2, 0b11_001_010, 0x77],
        "vcmppd ymm1, ymm0, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_011, 0xc2, 0b11_001_010, 0x77],
        "vcmpsd xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_111, 0xc2, 0b11_001_010, 0x77],
        "vcmpsd xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xc4, 0b11_001_010, 0x77],
        "vpinsrw xmm1, xmm0, edx, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_101, 0xc4, 0b11_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_001, 0xc5, 0b00_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0xc5, 0b11_001_010, 0x77],
        "vpextrw ecx, xmm2, 0x77",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0xc5, 0b00_001_010, 0x77]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_101, 0xc5, 0b11_001_010, 0x77]);
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0xc6, 0b11_001_010, 0x77],
        "vshufpd xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0xc6, 0b11_001_010, 0x77],
        "vshufpd ymm1, ymm0, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_000, 0xc6, 0b11_001_010, 0x77],
        "vshufps xmm1, xmm0, xmm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_100, 0xc6, 0b11_001_010, 0x77],
        "vshufps ymm1, ymm0, ymm2, 0x77",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xd0, 0b11_001_010],
        "vaddsubpd xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xd0, 0b11_001_010],
        "vaddsubpd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_011, 0xd0, 0b11_001_010],
        "vaddsubps xmm1, xmm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_111, 0xd0, 0b11_001_010],
        "vaddsubps ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xd1, 0b11_001_010],
        "vpsrlw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xd1, 0b11_001_010],
        "vpsrlw ymm1, ymm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xd1, 0b00_001_010],
        "vpsrlw ymm1, ymm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xd2, 0b11_001_010],
        "vpsrld xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xd2, 0b11_001_010],
        "vpsrld ymm1, ymm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xd2, 0b00_001_010],
        "vpsrld ymm1, ymm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xd3, 0b11_001_010],
        "vpsrlq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xd3, 0b11_001_010],
        "vpsrlq ymm1, ymm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xd3, 0b00_001_010],
        "vpsrlq ymm1, ymm0, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xd4, 0b11_001_010],
        "vpaddq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xd4, 0b11_001_010],
        "vpaddq ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0xd5, 0b11_001_010],
        "vpmullw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0xd5, 0b11_001_010],
        "vpmullw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_1111_001, 0xd7, 0b11_001_010],
        "vpmovmskb ecx, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b0_1111_001, 0xd7, 0b00_001_010]);
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_1111_101, 0xd7, 0b11_001_010],
        "vpmovmskb ecx, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xd8, 0b11_001_010],
        "vpsubusb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xd8, 0b11_001_010],
        "vpsubusb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xd9, 0b11_001_010],
        "vpsubusw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xd9, 0b11_001_010],
        "vpsubusw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0xda, 0b11_001_010],
        "vpminub xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0xda, 0b11_001_010],
        "vpminub ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xdb, 0b11_001_010],
        "vpand xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xdb, 0b11_001_010],
        "vpand ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xdc, 0b11_001_010],
        "vpaddusb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xdc, 0b11_001_010],
        "vpaddusb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xdd, 0b11_001_010],
        "vpaddusw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xdd, 0b11_001_010],
        "vpaddusw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0xde, 0b11_001_010],
        "vpmaxub xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0xde, 0b11_001_010],
        "vpmaxub ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xdf, 0b11_001_010],
        "vpandn xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xdf, 0b11_001_010],
        "vpandn ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xe0, 0b11_001_010],
        "vpavgb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xe0, 0b11_001_010],
        "vpavgb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xe1, 0b11_001_010],
        "vpsraw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xe1, 0b11_001_010],
        "vpsraw ymm1, ymm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xe2, 0b11_001_010],
        "vpsrad xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xe2, 0b11_001_010],
        "vpsrad ymm1, ymm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xe3, 0b11_001_010],
        "vpavgw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xe3, 0b11_001_010],
        "vpavgw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xe4, 0b11_001_010],
        "vpmulhuw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xe4, 0b11_001_010],
        "vpmulhuw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xe5, 0b11_001_010],
        "vpmulhw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xe5, 0b11_001_010],
        "vpmulhw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0xe6, 0b11_001_010],
        "vcvttpd2dq xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0xe6, 0b11_001_010],
        "vcvttpd2dq xmm1, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_010, 0xe6, 0b11_001_010],
        "vcvtdq2pd xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_110, 0xe6, 0b11_001_010],
        "vcvtdq2pd ymm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_011, 0xe6, 0b11_001_010],
        "vcvtpd2dq xmm1, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_111, 0xe6, 0b11_001_010],
        "vcvtpd2dq xmm1, ymm2",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0xe7, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_101, 0xe7, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0xe7, 0b00_001_010],
        "vmovntdq xmmword [edx], xmm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0xe7, 0b00_001_010],
        "vmovntdq ymmword [edx], ymm1",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xe8, 0b11_001_010],
        "vpsubsb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xe8, 0b11_001_010],
        "vpsubsb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xe9, 0b11_001_010],
        "vpsubsw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xe9, 0b11_001_010],
        "vpsubsw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0xea, 0b11_001_010],
        "vpminsw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0xea, 0b11_001_010],
        "vpminsw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0xeb, 0b11_001_010],
        "vpor xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0xeb, 0b11_001_010],
        "vpor ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0xec, 0b11_001_010],
        "vpaddsb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0xec, 0b11_001_010],
        "vpaddsb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0xed, 0b11_001_010],
        "vpaddsw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0xed, 0b11_001_010],
        "vpaddsw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0xee, 0b11_001_010],
        "vpmaxsw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0xee, 0b11_001_010],
        "vpmaxsw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b0_0111_001, 0xef, 0b11_001_010],
        "vpxor xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b0_0111_101, 0xef, 0b11_001_010],
        "vpxor ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_011, 0xf0, 0b00_001_010],
        "vlddqu xmm1, xmmword [edx]",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_111, 0xf0, 0b00_001_010],
        "vlddqu ymm1, ymmword [edx]",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_011, 0xf0, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_011, 0xf0, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_0111_111, 0xf0, 0b11_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_111, 0xf0, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xf1, 0b11_001_010],
        "vpsllw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xf1, 0b11_001_010],
        "vpsllw ymm1, ymm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xf2, 0b11_001_010],
        "vpslld xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xf2, 0b11_001_010],
        "vpslld ymm1, ymm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xf3, 0b11_001_010],
        "vpsllq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xf3, 0b11_001_010],
        "vpsllq ymm1, ymm0, xmm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xf4, 0b11_001_010],
        "vpmuludq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xf4, 0b11_001_010],
        "vpmuludq ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0xf5, 0b11_001_010],
        "vpmaddwd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0xf5, 0b11_001_010],
        "vpmaddwd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0xf6, 0b11_001_010],
        "vpsadbw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_1111_101, 0xf6, 0b11_001_010],
        "vpsadbw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_1111_001, 0xf7, 0b11_001_010],
        "vmaskmovdqu xmm1, xmm2",
    );
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_001, 0xf7, 0b00_001_010]);
    test_invalid(&[0xc4, 0b110_00001, 0b1_1111_101, 0xf7, 0b11_001_010]);
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xf8, 0b11_001_010],
        "vpsubb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xf8, 0b11_001_010],
        "vpsubb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xf9, 0b11_001_010],
        "vpsubw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xf9, 0b11_001_010],
        "vpsubw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xfa, 0b11_001_010],
        "vpsubd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xfa, 0b11_001_010],
        "vpsubd ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xfb, 0b11_001_010],
        "vpsubq xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xfb, 0b11_001_010],
        "vpsubq ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xfc, 0b11_001_010],
        "vpaddb xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xfc, 0b11_001_010],
        "vpaddb ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xfd, 0b11_001_010],
        "vpaddw xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xfd, 0b11_001_010],
        "vpaddw ymm1, ymm0, ymm2",
    );
    test_instr(
        &[0xc4, 0b110_00001, 0b1_0111_001, 0xfe, 0b11_001_010],
        "vpaddd xmm1, xmm0, xmm2",
    );
    test_avx2(
        &[0xc4, 0b110_00001, 0b1_0111_101, 0xfe, 0b11_001_010],
        "vpaddd ymm1, ymm0, ymm2",
    );

    test_instr(&[0xc5, 0xf8, 0x10, 0x00], "vmovups xmm0, xmmword [eax]");
    test_instr(&[0xc5, 0xf8, 0x10, 0x01], "vmovups xmm0, xmmword [ecx]");
}

#[test]
fn strange_prefixing() {
    test_display(&[0x66, 0x0f, 0x21, 0xc8], "mov eax, dr1");
    test_display(&[0xf2, 0x0f, 0x21, 0xc8], "mov eax, dr1");
    test_display(&[0xf3, 0x0f, 0x21, 0xc8], "mov eax, dr1");
}

#[test]
fn prefixed_0f() {
    test_display(&[0x0f, 0x02, 0xc0], "lar eax, ax");
    test_display(&[0x0f, 0x03, 0xc0], "lsl eax, eax");
    test_display(&[0x0f, 0x05], "syscall");
    test_display(&[0x66, 0x0f, 0x05], "syscall");
    test_display(&[0x0f, 0x06], "clts");
    test_display(&[0xf2, 0x0f, 0x06], "clts");
    test_display(&[0x0f, 0x07], "sysret");
    test_display(&[0xf2, 0x0f, 0x07], "sysret");
    test_display(&[0x0f, 0x12, 0x0f], "movlps xmm1, qword [edi]");
    test_display(&[0x0f, 0x12, 0xcf], "movhlps xmm1, xmm7");
    test_display(&[0x0f, 0x16, 0x0f], "movhps xmm1, qword [edi]");
    test_display(&[0x0f, 0x16, 0xcf], "movlhps xmm1, xmm7");
    test_display(&[0x0f, 0x12, 0xc0], "movhlps xmm0, xmm0");
    test_invalid(&[0x0f, 0x13, 0xc0]);
    test_display(&[0x0f, 0x13, 0x00], "movlps qword [eax], xmm0");
    test_display(&[0x0f, 0x14, 0x08], "unpcklps xmm1, xmmword [eax]");
    test_display(&[0x0f, 0x15, 0x08], "unpckhps xmm1, xmmword [eax]");
    test_display(&[0x0f, 0x16, 0x0f], "movhps xmm1, qword [edi]");
    test_display(&[0x0f, 0x16, 0xc0], "movlhps xmm0, xmm0");
    test_invalid(&[0x0f, 0x17, 0xc0]);
    test_display(&[0x0f, 0x17, 0x00], "movhps qword [eax], xmm0");
    test_display(&[0x0f, 0x18, 0xc0], "nop eax"); // capstone says invalid, xed says nop
    test_display(&[0x0f, 0x18, 0x00], "prefetchnta zmmword [eax]");
    test_display(&[0x0f, 0x18, 0x08], "prefetch0 zmmword [eax]");
    test_display(&[0x0f, 0x18, 0x10], "prefetch1 zmmword [eax]");
    test_display(&[0x0f, 0x18, 0x18], "prefetch2 zmmword [eax]");
    test_display(&[0x0f, 0x18, 0x20], "nop zmmword [eax]");
    test_display(&[0x0f, 0x18, 0xcc], "nop esp");
    test_display(&[0x0f, 0x19, 0x20], "nop dword [eax]");
    test_display(&[0x0f, 0x1a, 0x20], "nop dword [eax]");
    test_display(&[0x0f, 0x1b, 0x20], "nop dword [eax]");
    test_display(&[0x0f, 0x1c, 0x20], "nop dword [eax]");
    test_display(&[0x0f, 0x1d, 0x20], "nop dword [eax]");
    test_display(&[0x0f, 0x1e, 0x20], "nop dword [eax]");
    test_display(&[0x0f, 0x1f, 0x20], "nop dword [eax]");
    test_display(&[0x0f, 0x20, 0xc0], "mov eax, cr0");
    test_invalid(&[0x0f, 0x20, 0xc8]);
    test_display(&[0x0f, 0x21, 0xc8], "mov eax, dr1");
    test_display(&[0x0f, 0x22, 0xc0], "mov cr0, eax");
    test_invalid(&[0x0f, 0x22, 0xc8]);
    test_display(&[0x0f, 0x22, 0xc7], "mov cr0, edi");
    test_display(&[0x0f, 0x23, 0xc8], "mov dr1, eax");
    test_display(&[0x0f, 0x23, 0xcf], "mov dr1, edi");
    test_display(&[0x0f, 0x30], "wrmsr");
    test_display(&[0x0f, 0x31], "rdtsc");
    test_display(&[0x0f, 0x32], "rdmsr");
    test_display(&[0x0f, 0x33], "rdpmc");
    test_display(&[0x0f, 0x34], "sysenter");
    test_display(&[0x0f, 0x35], "sysexit");
    test_invalid(&[0x0f, 0x36]);
    test_display(&[0x0f, 0x37], "getsec");
    test_invalid(&[0x66, 0x0f, 0x37]);
    test_invalid(&[0xf2, 0x0f, 0x37]);
    test_invalid(&[0xf3, 0x0f, 0x37]);
    test_display(&[0x0f, 0x60, 0x00], "punpcklbw mm0, dword [eax]");
    test_display(&[0x0f, 0x60, 0xc2], "punpcklbw mm0, mm2");
    test_display(&[0x0f, 0x61, 0x00], "punpcklwd mm0, dword [eax]");
    test_display(&[0x0f, 0x61, 0xc2], "punpcklwd mm0, mm2");
    test_display(&[0x0f, 0x62, 0x00], "punpckldq mm0, dword [eax]");
    test_display(&[0x0f, 0x62, 0xc2], "punpckldq mm0, mm2");
    test_display(&[0x0f, 0x63, 0x00], "packsswb mm0, qword [eax]");
    test_display(&[0x0f, 0x63, 0xc2], "packsswb mm0, mm2");
    test_display(&[0x0f, 0x64, 0x00], "pcmpgtb mm0, qword [eax]");
    test_display(&[0x0f, 0x64, 0xc2], "pcmpgtb mm0, mm2");
    test_display(&[0x0f, 0x65, 0x00], "pcmpgtw mm0, qword [eax]");
    test_display(&[0x0f, 0x65, 0xc2], "pcmpgtw mm0, mm2");
    test_display(&[0x0f, 0x66, 0x00], "pcmpgtd mm0, qword [eax]");
    test_display(&[0x0f, 0x66, 0xc2], "pcmpgtd mm0, mm2");
    test_display(&[0x0f, 0x67, 0x00], "packuswb mm0, qword [eax]");
    test_display(&[0x0f, 0x67, 0xc2], "packuswb mm0, mm2");
    test_display(&[0x0f, 0x68, 0x00], "punpckhbw mm0, qword [eax]");
    test_display(&[0x0f, 0x68, 0xc2], "punpckhbw mm0, mm2");
    test_display(&[0x0f, 0x69, 0x00], "punpckhwd mm0, qword [eax]");
    test_display(&[0x0f, 0x69, 0xc2], "punpckhwd mm0, mm2");
    test_display(&[0x0f, 0x6a, 0x00], "punpckhdq mm0, qword [eax]");
    test_display(&[0x0f, 0x6a, 0xc2], "punpckhdq mm0, mm2");
    test_display(&[0x0f, 0x6b, 0x00], "packssdw mm0, qword [eax]");
    test_display(&[0x0f, 0x6b, 0xc2], "packssdw mm0, mm2");
    test_invalid(&[0x0f, 0x6c]);
    test_invalid(&[0x0f, 0x6d]);
    test_display(&[0x0f, 0x6e, 0x00], "movd mm0, dword [eax]");
    test_display(&[0x0f, 0x6e, 0xc2], "movd mm0, edx");
    test_display(&[0x0f, 0x6f, 0x00], "movq mm0, qword [eax]");
    test_display(&[0x0f, 0x6f, 0xc2], "movq mm0, mm2");
    test_display(&[0x0f, 0x6f, 0xfb], "movq mm7, mm3");
    test_display(&[0x0f, 0x70, 0x00, 0x7f], "pshufw mm0, qword [eax], 0x7f");
    test_invalid(&[0x0f, 0x71, 0x00, 0x7f]);
    test_invalid(&[0x0f, 0x71, 0xc0, 0x7f]);
    test_display(&[0x0f, 0x71, 0xd0, 0x7f], "psrlw mm0, 0x7f");
    test_display(&[0x0f, 0x71, 0xe0, 0x7f], "psraw mm0, 0x7f");
    test_display(&[0x0f, 0x71, 0xf0, 0x7f], "psllw mm0, 0x7f");
    test_invalid(&[0x0f, 0x72, 0x00, 0x7f]);
    test_invalid(&[0x0f, 0x72, 0xc0, 0x7f]);
    test_display(&[0x0f, 0x72, 0xd0, 0x7f], "psrld mm0, 0x7f");
    test_display(&[0x0f, 0x72, 0xe0, 0x7f], "psrad mm0, 0x7f");
    test_display(&[0x0f, 0x72, 0xf0, 0x7f], "pslld mm0, 0x7f");
    test_invalid(&[0x0f, 0x73, 0x00, 0x7f]);
    test_invalid(&[0x0f, 0x73, 0xc0, 0x7f]);
    test_display(&[0x0f, 0x73, 0xd0, 0x7f], "psrlq mm0, 0x7f");
    test_invalid(&[0x0f, 0x73, 0xe0, 0x7f]);
    test_display(&[0x0f, 0x73, 0xf0, 0x7f], "psllq mm0, 0x7f");
    test_display(&[0x0f, 0xa0], "push fs");
    test_display(&[0x0f, 0xa1], "pop fs");
    test_display(&[0x0f, 0xa2], "cpuid");
    test_display(&[0x0f, 0xa4, 0xc0, 0x11], "shld eax, eax, 0x11");
    test_display(&[0x66, 0x0f, 0xa4, 0xcf, 0x11], "shld di, cx, 0x11");
    test_display(&[0x0f, 0xa5, 0xc0], "shld eax, eax, cl");
    test_display(&[0x0f, 0xa5, 0xc9], "shld ecx, ecx, cl");
    test_display(&[0x0f, 0xac, 0xc0, 0x11], "shrd eax, eax, 0x11");
    test_display(&[0x66, 0x0f, 0xac, 0xcf, 0x11], "shrd di, cx, 0x11");
    test_display(&[0x0f, 0xad, 0xc9], "shrd ecx, ecx, cl");
}

#[test]
fn prefixed_660f() {
    test_display(&[0x66, 0x0f, 0x10, 0xc0], "movupd xmm0, xmm0");
    test_display(&[0xf2, 0x66, 0x66, 0x0f, 0x10, 0xc0], "movsd xmm0, xmm0");
}

#[test]
fn prefixed_f20f() {
    test_invalid(&[0xf2, 0x0f, 0x16, 0xcf]);
    test_invalid(&[0x66, 0xf2, 0x66, 0x0f, 0x16, 0xcf]);
}

#[test]
fn prefixed_f30f() {
    test_display(&[0xf3, 0x0f, 0x16, 0xcf], "movshdup xmm1, xmm7");
    test_display(&[0xf3, 0x0f, 0x1e, 0xfa], "endbr64");
    test_display(&[0xf3, 0x0f, 0x1e, 0xfb], "endbr32");
    test_display(&[0xf3, 0x0f, 0x1e, 0xfc], "nop esp, edi");
}

#[test]
fn only_32bit() {
    test_display(
        &[0x9a, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66],
        "callf 0x6655:0x44332211",
    );
    test_display(&[0x66, 0x9a, 0x11, 0x22, 0x33, 0x44], "callf 0x4433:0x2211");
    test_display(&[0x67, 0xac], "lods al, byte ds:[si]");
    test_display(&[0x67, 0xae], "scas byte es:[di], al");
    test_display(&[0xac], "lods al, byte ds:[esi]");
    test_display(&[0xae], "scas byte es:[edi], al");
    test_display(&[0x67, 0xf3, 0xa4], "rep movs byte es:[di], byte ds:[si]");
    test_display(&[0xf3, 0xa4], "rep movs byte es:[edi], byte ds:[esi]");
    test_display(&[0x67, 0xf3, 0xa5], "rep movs dword es:[di], dword ds:[si]");
    test_display(&[0xf3, 0xa5], "rep movs dword es:[edi], dword ds:[esi]");
    test_display(
        &[0x66, 0x67, 0x8b, 0x0e, 0x55, 0xaa],
        "mov cx, word [0xaa55]",
    );
    test_display(&[0x66, 0x8b, 0x0e], "mov cx, word [esi]");
    test_display(&[0x40], "inc eax");
    test_display(&[0x41], "inc ecx");
    test_display(&[0x47], "inc edi");
    test_display(&[0x48], "dec eax");
    test_display(&[0x4f], "dec edi");

    test_display(&[0xa0, 0xc0, 0xb0, 0xa0, 0x90], "mov al, byte [0x90a0b0c0]");
    test_display(&[0x67, 0xa0, 0xc0, 0xb0], "mov al, byte [0xb0c0]");
    test_display(&[0x67, 0xa1, 0xc0, 0xb0], "mov eax, dword [0xb0c0]");
    test_display(&[0x66, 0x67, 0xa1, 0xc0, 0xb0], "mov ax, word [0xb0c0]");

    test_display(&[0x60], "pusha");
    test_display(&[0x61], "popa");
    test_display(&[0xce], "into");
    test_display(&[0x06], "push es");
    test_display(&[0x07], "pop es");
    test_display(&[0x0e], "push cs");
    test_display(&[0x16], "push ss");
    test_display(&[0x17], "pop ss");
    test_display(&[0x1e], "push ds");
    test_display(&[0x1f], "pop ds");
    test_display(&[0x27], "daa");
    test_display(&[0x2f], "das");
    test_display(&[0x37], "aaa");
    test_display(&[0x3f], "aas");
    test_display(&[0xd4, 0x01], "aam 0x1");
    test_display(&[0xd4, 0x0a], "aam 0xa");
    test_display(&[0xd5, 0x01], "aad 0x1");
    test_display(&[0xd5, 0x0a], "aad 0xa");

    test_display(&[0xc5, 0x78, 0x10], "lds edi, far [eax + 0x10]");
    test_display(&[0x66, 0xc5, 0x78, 0x10], "lds di, dword [eax + 0x10]");
}

#[test]
fn adx() {
    test_display(&[0x66, 0x0f, 0x38, 0xf6, 0xc1], "adcx eax, ecx");
    test_display(&[0x66, 0x0f, 0x38, 0xf6, 0x01], "adcx eax, dword [ecx]");
    test_display(&[0xf3, 0x0f, 0x38, 0xf6, 0xc1], "adox eax, ecx");
    test_display(&[0xf3, 0x0f, 0x38, 0xf6, 0x01], "adox eax, dword [ecx]");
}

#[test]
fn prefetchw() {
    test_display(&[0x0f, 0x0d, 0x08], "prefetchw zmmword [eax]");
}

#[test]
fn lzcnt() {
    test_display(&[0x66, 0xf3, 0x0f, 0xbd, 0xc1], "lzcnt ax, cx");
    test_display(&[0xf3, 0x0f, 0xbd, 0xc1], "lzcnt eax, ecx");
}

#[test]
fn svm() {
    test_display(&[0x0f, 0x01, 0xdf], "invlpga eax, ecx");
    test_display(&[0x0f, 0x01, 0xde], "skinit eax");
    test_display(&[0x0f, 0x01, 0xdd], "clgi");
    test_display(&[0x0f, 0x01, 0xdc], "stgi");
    test_display(&[0x0f, 0x01, 0xdb], "vmsave eax");
    test_display(&[0x0f, 0x01, 0xda], "vmload eax");
    test_display(&[0x0f, 0x01, 0xd9], "vmmcall");
    test_display(&[0x0f, 0x01, 0xd8], "vmrun eax");
    test_display(&[0x0f, 0x78, 0xc4], "vmread esp, eax");
    test_display(&[0x0f, 0x79, 0xc5], "vmwrite eax, ebp");
    test_display(&[0x0f, 0x78, 0x0b], "vmread qword [ebx], ecx");
    test_invalid(&[0x66, 0x0f, 0x78, 0x03]);
    test_display(&[0x0f, 0x79, 0x0b], "vmwrite ecx, qword [ebx]");
    test_invalid(&[0x66, 0x0f, 0x79, 0x03]);
}

#[test]
fn movbe() {
    test_display(&[0x0f, 0x38, 0xf0, 0x06], "movbe eax, dword [esi]");
    test_invalid(&[0x0f, 0x38, 0xf0, 0xc6]);
    test_display(&[0x0f, 0x38, 0xf1, 0x06], "movbe dword [esi], eax");
    test_display(&[0x66, 0x0f, 0x38, 0xf1, 0x06], "movbe word [esi], ax");
    test_invalid(&[0x66, 0x0f, 0x38, 0xf1, 0xc6]);
}

#[test]
fn tsx() {
    test_display(&[0xc6, 0xf8, 0x10], "xabort 0x10");
    test_display(&[0xc7, 0xf8, 0x10, 0x12, 0x34, 0x56], "xbegin 0x56341210");
    test_display(&[0x66, 0xc7, 0xf8, 0x10, 0x12], "xbegin 0x1210");
    test_display(&[0x0f, 0x01, 0xd5], "xend");
    test_display(&[0x0f, 0x01, 0xd6], "xtest");
}

#[test]
fn rand() {
    test_display(&[0x0f, 0xc7, 0xfd], "rdseed ebp");
    test_display(&[0x66, 0x0f, 0xc7, 0xfd], "rdseed bp");
    test_display(&[0x0f, 0xc7, 0xf5], "rdrand ebp");
    test_display(&[0x66, 0x0f, 0xc7, 0xf5], "rdrand bp");
}

#[test]
fn sha() {
    test_display(
        &[0x0f, 0x3a, 0xcc, 0x12, 0x40],
        "sha1rnds4 xmm2, xmmword [edx], 0x40",
    );
    test_display(
        &[0x0f, 0x3a, 0xcc, 0x12, 0xff],
        "sha1rnds4 xmm2, xmmword [edx], 0xff",
    );
    test_display(&[0x0f, 0x38, 0xc8, 0x12], "sha1nexte xmm2, xmmword [edx]");
    test_display(&[0x0f, 0x38, 0xc9, 0x12], "sha1msg1 xmm2, xmmword [edx]");
    test_display(&[0x0f, 0x38, 0xca, 0x12], "sha1msg2 xmm2, xmmword [edx]");
    test_display(&[0x0f, 0x38, 0xcb, 0x12], "sha256rnds2 xmm2, xmmword [edx]");
    test_display(&[0x0f, 0x38, 0xcc, 0x12], "sha256msg1 xmm2, xmmword [edx]");
    test_display(&[0x0f, 0x38, 0xcd, 0x12], "sha256msg2 xmm2, xmmword [edx]");
}

#[test]
fn vmx() {
    test_display(&[0x0f, 0xc7, 0x3f], "vmptrst qword [edi]");
    test_display(&[0x0f, 0xc7, 0x37], "vmptrld qword [edi]");
    test_display(&[0xf3, 0x0f, 0xc7, 0x37], "vmxon qword [edi]");
    test_display(&[0x66, 0x0f, 0xc7, 0xf7], "rdrand di");
    test_display(&[0x66, 0x0f, 0xc7, 0x37], "vmclear qword [edi]");

    // this is actually vmx
    // test_invalid(&[0x66, 0x0f, 0xc7, 0x03]);
    test_display(&[0x66, 0x0f, 0xc7, 0x33], "vmclear qword [ebx]");
    test_display(&[0xf3, 0x0f, 0xc7, 0x33], "vmxon qword [ebx]");
}

#[test]
fn rdpid() {
    test_display(&[0xf3, 0x0f, 0xc7, 0xfd], "rdpid ebp");
}

#[test]
fn cmpxchg8b() {
    test_display(&[0x0f, 0xc7, 0x0f], "cmpxchg8b qword [edi]");
    test_display(&[0xf2, 0x0f, 0xc7, 0x0f], "cmpxchg8b qword [edi]");
    test_display(&[0xf3, 0x0f, 0xc7, 0x0f], "cmpxchg8b qword [edi]");
    test_display(&[0x66, 0x0f, 0xc7, 0x0f], "cmpxchg8b qword [edi]");
}

#[test]
fn x87() {
    //    test_display(&[0xd8, 0x03], "fadd st, dword ptr [ebx]");
    test_display(&[0xd8, 0x03], "fadd st(0), dword [ebx]");
    //    test_display(&[0xd8, 0x0b], "fmul st, dword ptr [ebx]");
    test_display(&[0xd8, 0x0b], "fmul st(0), dword [ebx]");
    //    test_display(&[0xd8, 0x13], "fcom st, dword ptr [ebx]");
    test_display(&[0xd8, 0x13], "fcom st(0), dword [ebx]");
    //    test_display(&[0xd8, 0x1b], "fcomp st, dword ptr [ebx]");
    test_display(&[0xd8, 0x1b], "fcomp st(0), dword [ebx]");
    //    test_display(&[0xd8, 0x23], "fsub st, dword ptr [ebx]");
    test_display(&[0xd8, 0x23], "fsub st(0), dword [ebx]");
    //    test_display(&[0xd8, 0x2b], "fsubr st, dword ptr [ebx]");
    test_display(&[0xd8, 0x2b], "fsubr st(0), dword [ebx]");
    //    test_display(&[0xd8, 0x33], "fdiv st, dword ptr [ebx]");
    test_display(&[0xd8, 0x33], "fdiv st(0), dword [ebx]");
    //    test_display(&[0xd8, 0x3b], "fdivr st, dword ptr [ebx]");
    test_display(&[0xd8, 0x3b], "fdivr st(0), dword [ebx]");
    //    test_display(&[0xd8, 0xc3], "fadd st, st(3)");
    test_display(&[0xd8, 0xc3], "fadd st(0), st(3)");
    //    test_display(&[0xd8, 0xcb], "fmul st, st(3)");
    test_display(&[0xd8, 0xcb], "fmul st(0), st(3)");
    //    test_display(&[0xd8, 0xd3], "fcom st, st(3)");
    test_display(&[0xd8, 0xd3], "fcom st(0), st(3)");
    //    test_display(&[0xd8, 0xdb], "fcomp st, st(3)");
    test_display(&[0xd8, 0xdb], "fcomp st(0), st(3)");
    //    test_display(&[0xd8, 0xe3], "fsub st, st(3)");
    test_display(&[0xd8, 0xe3], "fsub st(0), st(3)");
    //    test_display(&[0xd8, 0xeb], "fsubr st, st(3)");
    test_display(&[0xd8, 0xeb], "fsubr st(0), st(3)");
    //    test_display(&[0xd8, 0xf3], "fdiv st, st(3)");
    test_display(&[0xd8, 0xf3], "fdiv st(0), st(3)");
    //    test_display(&[0xd8, 0xfb], "fdivr st, st(3)");
    test_display(&[0xd8, 0xfb], "fdivr st(0), st(3)");
    //    test_display(&[0xd9, 0x03], "fld st, dword ptr [ebx]");
    test_display(&[0xd9, 0x03], "fld st(0), dword [ebx]");
    test_invalid(&[0xd9, 0x08]);
    test_invalid(&[0xd9, 0x09]);
    test_invalid(&[0xd9, 0x0a]);
    test_invalid(&[0xd9, 0x0b]);
    test_invalid(&[0xd9, 0x0c]);
    test_invalid(&[0xd9, 0x0d]);
    test_invalid(&[0xd9, 0x0e]);
    test_invalid(&[0xd9, 0x0f]);
    //    test_display(&[0xd9, 0x13], "fst dword ptr [ebx], st");
    test_display(&[0xd9, 0x13], "fst dword [ebx], st(0)");
    //    test_display(&[0xd9, 0x1b], "fstp dword ptr [ebx], st");
    test_display(&[0xd9, 0x1b], "fstp dword [ebx], st(0)");
    //    test_display(&[0xd9, 0x23], "fldenv ptr [ebx]");
    test_display(&[0xd9, 0x23], "fldenv ptr [ebx]");
    //    test_display(&[0xd9, 0x2b], "fldcw word ptr [ebx]");
    test_display(&[0xd9, 0x2b], "fldcw word [ebx]");
    //    test_display(&[0xd9, 0x33], "fnstenv ptr [ebx]");
    test_display(&[0xd9, 0x33], "fnstenv ptr [ebx]");
    //    test_display(&[0xd9, 0x3b], "fnstcw word ptr [ebx]");
    test_display(&[0xd9, 0x3b], "fnstcw word [ebx]");
    //    test_display(&[0xd9, 0xc3], "fld st, st(3)");
    test_display(&[0xd9, 0xc3], "fld st(0), st(3)");
    //    test_display(&[0xd9, 0xcb], "fxch st, st(3)");
    test_display(&[0xd9, 0xcb], "fxch st(0), st(3)");
    test_display(&[0xd9, 0xd0], "fnop");
    test_invalid(&[0xd9, 0xd1]);
    test_invalid(&[0xd9, 0xd2]);
    test_invalid(&[0xd9, 0xd3]);
    test_invalid(&[0xd9, 0xd4]);
    test_invalid(&[0xd9, 0xd5]);
    test_invalid(&[0xd9, 0xd6]);
    test_invalid(&[0xd9, 0xd7]);
    // undocumented save for intel xed
    //    test_display(&[0xd9, 0xdb], "fstpnce st(3), st");
    test_display(&[0xd9, 0xdb], "fstpnce st(3), st(0)");
    test_display(&[0xd9, 0xe0], "fchs");
    test_display(&[0xd9, 0xe1], "fabs");
    test_invalid(&[0xd9, 0xe2]);
    test_invalid(&[0xd9, 0xe3]);
    test_display(&[0xd9, 0xe4], "ftst");
    test_display(&[0xd9, 0xe5], "fxam");
    test_invalid(&[0xd9, 0xe6]);
    test_invalid(&[0xd9, 0xe7]);
    test_display(&[0xd9, 0xe8], "fld1");
    test_display(&[0xd9, 0xe9], "fldl2t");
    test_display(&[0xd9, 0xea], "fldl2e");
    test_display(&[0xd9, 0xeb], "fldpi");
    test_display(&[0xd9, 0xec], "fldlg2");
    test_display(&[0xd9, 0xed], "fldln2");
    test_display(&[0xd9, 0xee], "fldz");
    test_invalid(&[0xd9, 0xef]);
    test_display(&[0xd9, 0xf0], "f2xm1");
    test_display(&[0xd9, 0xf1], "fyl2x");
    test_display(&[0xd9, 0xf2], "fptan");
    test_display(&[0xd9, 0xf3], "fpatan");
    test_display(&[0xd9, 0xf4], "fxtract");
    test_display(&[0xd9, 0xf5], "fprem1");
    test_display(&[0xd9, 0xf6], "fdecstp");
    test_display(&[0xd9, 0xf7], "fincstp");
    test_display(&[0xd9, 0xf8], "fprem");
    test_display(&[0xd9, 0xf9], "fyl2xp1");
    test_display(&[0xd9, 0xfa], "fsqrt");
    test_display(&[0xd9, 0xfb], "fsincos");
    test_display(&[0xd9, 0xfc], "frndint");
    test_display(&[0xd9, 0xfd], "fscale");
    test_display(&[0xd9, 0xfe], "fsin");
    test_display(&[0xd9, 0xff], "fcos");
    //    test_display(&[0xda, 0x03], "fiadd st, dword ptr [ebx]");
    test_display(&[0xda, 0x03], "fiadd st(0), dword [ebx]");
    //    test_display(&[0xda, 0x0b], "fimul st, dword ptr [ebx]");
    test_display(&[0xda, 0x0b], "fimul st(0), dword [ebx]");
    //    test_display(&[0xda, 0x13], "ficom st, dword ptr [ebx]");
    test_display(&[0xda, 0x13], "ficom st(0), dword [ebx]");
    //    test_display(&[0xda, 0x1b], "ficomp st, dword ptr [ebx]");
    test_display(&[0xda, 0x1b], "ficomp st(0), dword [ebx]");
    //    test_display(&[0xda, 0x23], "fisub st, dword ptr [ebx]");
    test_display(&[0xda, 0x23], "fisub st(0), dword [ebx]");
    //    test_display(&[0xda, 0x2b], "fisubr st, dword ptr [ebx]");
    test_display(&[0xda, 0x2b], "fisubr st(0), dword [ebx]");
    //    test_display(&[0xda, 0x33], "fidiv st, dword ptr [ebx]");
    test_display(&[0xda, 0x33], "fidiv st(0), dword [ebx]");
    //    test_display(&[0xda, 0x3b], "fidivr st, dword ptr [ebx]");
    test_display(&[0xda, 0x3b], "fidivr st(0), dword [ebx]");
    //    test_display(&[0xda, 0xc3], "fcmovb st, st(3)");
    test_display(&[0xda, 0xc3], "fcmovb st(0), st(3)");
    //    test_display(&[0xda, 0xcb], "fcmove st, st(3)");
    test_display(&[0xda, 0xcb], "fcmove st(0), st(3)");
    //    test_display(&[0xda, 0xd3], "fcmovbe st, st(3)");
    test_display(&[0xda, 0xd3], "fcmovbe st(0), st(3)");
    //    test_display(&[0xda, 0xdb], "fcmovu st, st(3)");
    test_display(&[0xda, 0xdb], "fcmovu st(0), st(3)");
    test_invalid(&[0xda, 0xe0]);
    test_invalid(&[0xda, 0xe1]);
    test_invalid(&[0xda, 0xe2]);
    test_invalid(&[0xda, 0xe3]);
    test_invalid(&[0xda, 0xe4]);
    test_invalid(&[0xda, 0xe5]);
    test_invalid(&[0xda, 0xe6]);
    test_invalid(&[0xda, 0xe7]);
    test_invalid(&[0xda, 0xe8]);
    test_display(&[0xda, 0xe9], "fucompp");
    test_invalid(&[0xda, 0xea]);
    test_invalid(&[0xda, 0xeb]);
    test_invalid(&[0xda, 0xec]);
    test_invalid(&[0xda, 0xed]);
    test_invalid(&[0xda, 0xee]);
    test_invalid(&[0xda, 0xef]);
    test_invalid(&[0xda, 0xf0]);
    test_invalid(&[0xda, 0xf1]);
    test_invalid(&[0xda, 0xf2]);
    test_invalid(&[0xda, 0xf3]);
    test_invalid(&[0xda, 0xf4]);
    test_invalid(&[0xda, 0xf5]);
    test_invalid(&[0xda, 0xf6]);
    test_invalid(&[0xda, 0xf7]);
    test_invalid(&[0xda, 0xf8]);
    test_invalid(&[0xda, 0xf9]);
    test_invalid(&[0xda, 0xfa]);
    test_invalid(&[0xda, 0xfb]);
    test_invalid(&[0xda, 0xfc]);
    test_invalid(&[0xda, 0xfd]);
    test_invalid(&[0xda, 0xfe]);
    test_invalid(&[0xda, 0xff]);
    //    test_display(&[0xdb, 0x03], "fild st, dword ptr [ebx]");
    test_display(&[0xdb, 0x03], "fild st(0), dword [ebx]");
    //    test_display(&[0xdb, 0x0b], "fisttp dword ptr [ebx], st");
    test_display(&[0xdb, 0x0b], "fisttp dword [ebx], st(0)");
    //    test_display(&[0xdb, 0x13], "fist dword ptr [ebx], st");
    test_display(&[0xdb, 0x13], "fist dword [ebx], st(0)");
    //    test_display(&[0xdb, 0x1b], "fistp dword ptr [ebx], st");
    test_display(&[0xdb, 0x1b], "fistp dword [ebx], st(0)");
    test_invalid(&[0xdb, 0x20]);
    test_invalid(&[0xdb, 0x21]);
    test_invalid(&[0xdb, 0x22]);
    test_invalid(&[0xdb, 0x23]);
    test_invalid(&[0xdb, 0x24]);
    test_invalid(&[0xdb, 0x25]);
    test_invalid(&[0xdb, 0x26]);
    test_invalid(&[0xdb, 0x27]);
    //    test_display(&[0xdb, 0x2b], "fld st, ptr [ebx]");
    test_display(&[0xdb, 0x2b], "fld st(0), mword [ebx]");
    test_invalid(&[0xdb, 0x30]);
    test_invalid(&[0xdb, 0x31]);
    test_invalid(&[0xdb, 0x32]);
    test_invalid(&[0xdb, 0x33]);
    test_invalid(&[0xdb, 0x34]);
    test_invalid(&[0xdb, 0x35]);
    test_invalid(&[0xdb, 0x36]);
    test_invalid(&[0xdb, 0x37]);
    //    test_display(&[0xdb, 0x3b], "fstp ptr [ebx], st");
    test_display(&[0xdb, 0x3b], "fstp mword [ebx], st(0)");
    //    test_display(&[0xdb, 0xc3], "fcmovnb st, st(3)");
    test_display(&[0xdb, 0xc3], "fcmovnb st(0), st(3)");
    //    test_display(&[0xdb, 0xcb], "fcmovne st, st(3)");
    test_display(&[0xdb, 0xcb], "fcmovne st(0), st(3)");
    //    test_display(&[0xdb, 0xd3], "fcmovnbe st, st(3)");
    test_display(&[0xdb, 0xd3], "fcmovnbe st(0), st(3)");
    //    test_display(&[0xdb, 0xdb], "fcmovnu st, st(3)");
    test_display(&[0xdb, 0xdb], "fcmovnu st(0), st(3)");
    test_display(&[0xdb, 0xe0], "feni8087_nop");
    test_display(&[0xdb, 0xe1], "fdisi8087_nop");
    test_display(&[0xdb, 0xe2], "fnclex");
    test_display(&[0xdb, 0xe3], "fninit");
    test_display(&[0xdb, 0xe4], "fsetpm287_nop");
    test_invalid(&[0xdb, 0xe5]);
    test_invalid(&[0xdb, 0xe6]);
    test_invalid(&[0xdb, 0xe7]);
    //    test_display(&[0xdb, 0xeb], "fucomi st, st(3)");
    test_display(&[0xdb, 0xeb], "fucomi st(0), st(3)");
    //    test_display(&[0xdb, 0xf3], "fcomi st, st(3)");
    test_display(&[0xdb, 0xf3], "fcomi st(0), st(3)");
    test_invalid(&[0xdb, 0xf8]);
    test_invalid(&[0xdb, 0xf9]);
    test_invalid(&[0xdb, 0xfa]);
    test_invalid(&[0xdb, 0xfb]);
    test_invalid(&[0xdb, 0xfc]);
    test_invalid(&[0xdb, 0xfd]);
    test_invalid(&[0xdb, 0xfe]);
    test_invalid(&[0xdb, 0xff]);
    //    test_display(&[0xdc, 0x03], "fadd st, qword ptr [ebx]");
    test_display(&[0xdc, 0x03], "fadd st(0), qword [ebx]");
    //    test_display(&[0xdc, 0x0b], "fmul st, qword ptr [ebx]");
    test_display(&[0xdc, 0x0b], "fmul st(0), qword [ebx]");
    //    test_display(&[0xdc, 0x13], "fcom st, qword ptr [ebx]");
    test_display(&[0xdc, 0x13], "fcom st(0), qword [ebx]");
    //    test_display(&[0xdc, 0x1b], "fcomp st, qword ptr [ebx]");
    test_display(&[0xdc, 0x1b], "fcomp st(0), qword [ebx]");
    //    test_display(&[0xdc, 0x23], "fsub st, qword ptr [ebx]");
    test_display(&[0xdc, 0x23], "fsub st(0), qword [ebx]");
    //    test_display(&[0xdc, 0x2b], "fsubr st, qword ptr [ebx]");
    test_display(&[0xdc, 0x2b], "fsubr st(0), qword [ebx]");
    //    test_display(&[0xdc, 0x33], "fdiv st, qword ptr [ebx]");
    test_display(&[0xdc, 0x33], "fdiv st(0), qword [ebx]");
    //    test_display(&[0xdc, 0x3b], "fdivr st, qword ptr [ebx]");
    test_display(&[0xdc, 0x3b], "fdivr st(0), qword [ebx]");
    //    test_display(&[0xdc, 0xc3], "fadd st(3), st");
    test_display(&[0xdc, 0xc3], "fadd st(3), st(0)");
    //    test_display(&[0xdc, 0xcb], "fmul st(3), st");
    test_display(&[0xdc, 0xcb], "fmul st(3), st(0)");
    //    test_display(&[0xdc, 0xd3], "fcom st, st(3)");
    test_display(&[0xdc, 0xd3], "fcom st(0), st(3)");
    //    test_display(&[0xdc, 0xdb], "fcomp st, st(3)");
    test_display(&[0xdc, 0xdb], "fcomp st(0), st(3)");
    //    test_display(&[0xdc, 0xe3], "fsubr st(3), st");
    test_display(&[0xdc, 0xe3], "fsubr st(3), st(0)");
    //    test_display(&[0xdc, 0xeb], "fsub st(3), st");
    test_display(&[0xdc, 0xeb], "fsub st(3), st(0)");
    //    test_display(&[0xdc, 0xf3], "fdivr st(3), st");
    test_display(&[0xdc, 0xf3], "fdivr st(3), st(0)");
    //    test_display(&[0xdc, 0xfb], "fdiv st(3), st");
    test_display(&[0xdc, 0xfb], "fdiv st(3), st(0)");
    //    test_display(&[0xdd, 0x03], "fld st, qword ptr [ebx]");
    test_display(&[0xdd, 0x03], "fld st(0), qword [ebx]");
    //    test_display(&[0xdd, 0x0b], "fisttp qword ptr [ebx], st");
    test_display(&[0xdd, 0x0b], "fisttp qword [ebx], st(0)");
    //    test_display(&[0xdd, 0x13], "fst qword ptr [ebx], st");
    test_display(&[0xdd, 0x13], "fst qword [ebx], st(0)");
    //    test_display(&[0xdd, 0x1b], "fstp qword ptr [ebx], st");
    test_display(&[0xdd, 0x1b], "fstp qword [ebx], st(0)");
    //    test_display(&[0xdd, 0x23], "frstor ptr [ebx]");
    test_display(&[0xdd, 0x23], "frstor ptr [ebx]");
    test_invalid(&[0xdd, 0x28]);
    test_invalid(&[0xdd, 0x29]);
    test_invalid(&[0xdd, 0x2a]);
    test_invalid(&[0xdd, 0x2b]);
    test_invalid(&[0xdd, 0x2c]);
    test_invalid(&[0xdd, 0x2d]);
    test_invalid(&[0xdd, 0x2e]);
    test_invalid(&[0xdd, 0x2f]);
    //    test_display(&[0xdd, 0x33], "fnsave ptr [ebx]");
    test_display(&[0xdd, 0x33], "fnsave ptr [ebx]");
    //    test_display(&[0xdd, 0x3b], "fnstsw word ptr [ebx]");
    test_display(&[0xdd, 0x3b], "fnstsw word [ebx]");
    test_display(&[0xdd, 0xc3], "ffree st(3)");
    //    test_display(&[0xdd, 0xcb], "fxch st, st(3)");
    test_display(&[0xdd, 0xcb], "fxch st(0), st(3)");
    //    test_display(&[0xdd, 0xd3], "fst st(3), st");
    test_display(&[0xdd, 0xd3], "fst st(3), st(0)");
    //    test_display(&[0xdd, 0xdb], "fstp st(3), st");
    test_display(&[0xdd, 0xdb], "fstp st(3), st(0)");
    //    test_display(&[0xdd, 0xe3], "fucom st, st(3)");
    test_display(&[0xdd, 0xe3], "fucom st(0), st(3)");
    //    test_display(&[0xdd, 0xeb], "fucomp st, st(3)");
    test_display(&[0xdd, 0xeb], "fucomp st(0), st(3)");
    test_invalid(&[0xdd, 0xf0]);
    test_invalid(&[0xdd, 0xf1]);
    test_invalid(&[0xdd, 0xf2]);
    test_invalid(&[0xdd, 0xf3]);
    test_invalid(&[0xdd, 0xf4]);
    test_invalid(&[0xdd, 0xf5]);
    test_invalid(&[0xdd, 0xf6]);
    test_invalid(&[0xdd, 0xf7]);
    test_invalid(&[0xdd, 0xf8]);
    test_invalid(&[0xdd, 0xf9]);
    test_invalid(&[0xdd, 0xfa]);
    test_invalid(&[0xdd, 0xfb]);
    test_invalid(&[0xdd, 0xfc]);
    test_invalid(&[0xdd, 0xfd]);
    test_invalid(&[0xdd, 0xfe]);
    test_invalid(&[0xdd, 0xff]);
    //    test_display(&[0xde, 0x03], "fiadd st, word ptr [ebx]");
    test_display(&[0xde, 0x03], "fiadd st(0), word [ebx]");
    //    test_display(&[0xde, 0x0b], "fimul st, word ptr [ebx]");
    test_display(&[0xde, 0x0b], "fimul st(0), word [ebx]");
    //    test_display(&[0xde, 0x13], "ficom st, word ptr [ebx]");
    test_display(&[0xde, 0x13], "ficom st(0), word [ebx]");
    //    test_display(&[0xde, 0x1b], "ficomp st, word ptr [ebx]");
    test_display(&[0xde, 0x1b], "ficomp st(0), word [ebx]");
    //    test_display(&[0xde, 0x23], "fisub st, word ptr [ebx]");
    test_display(&[0xde, 0x23], "fisub st(0), word [ebx]");
    //    test_display(&[0xde, 0x2b], "fisubr st, word ptr [ebx]");
    test_display(&[0xde, 0x2b], "fisubr st(0), word [ebx]");
    //    test_display(&[0xde, 0x33], "fidiv st, word ptr [ebx]");
    test_display(&[0xde, 0x33], "fidiv st(0), word [ebx]");
    //    test_display(&[0xde, 0x3b], "fidivr st, word ptr [ebx]");
    test_display(&[0xde, 0x3b], "fidivr st(0), word [ebx]");
    //    test_display(&[0xde, 0xc3], "faddp st(3), st");
    test_display(&[0xde, 0xc3], "faddp st(3), st(0)");
    //    test_display(&[0xde, 0xcb], "fmulp st(3), st");
    test_display(&[0xde, 0xcb], "fmulp st(3), st(0)");
    //    test_display(&[0xde, 0xd3], "fcomp st, st(3)");
    test_display(&[0xde, 0xd3], "fcomp st(0), st(3)");
    test_invalid(&[0xde, 0xd8]);
    test_display(&[0xde, 0xd9], "fcompp");
    test_invalid(&[0xde, 0xda]);
    test_invalid(&[0xde, 0xdb]);
    test_invalid(&[0xde, 0xdc]);
    test_invalid(&[0xde, 0xdd]);
    test_invalid(&[0xde, 0xde]);
    test_invalid(&[0xde, 0xdf]);
    //    test_display(&[0xde, 0xe3], "fsubrp st(3), st");
    test_display(&[0xde, 0xe3], "fsubrp st(3), st(0)");
    //    test_display(&[0xde, 0xeb], "fsubp st(3), st");
    test_display(&[0xde, 0xeb], "fsubp st(3), st(0)");
    //    test_display(&[0xde, 0xf3], "fdivrp st(3), st");
    test_display(&[0xde, 0xf3], "fdivrp st(3), st(0)");
    //    test_display(&[0xde, 0xfb], "fdivp st(3), st");
    test_display(&[0xde, 0xfb], "fdivp st(3), st(0)");
    //    test_display(&[0xdf, 0x03], "fild st, word ptr [ebx]");
    test_display(&[0xdf, 0x03], "fild st(0), word [ebx]");
    //    test_display(&[0xdf, 0x0b], "fisttp word ptr [ebx], st");
    test_display(&[0xdf, 0x0b], "fisttp word [ebx], st(0)");
    //    test_display(&[0xdf, 0x13], "fist word ptr [ebx], st");
    test_display(&[0xdf, 0x13], "fist word [ebx], st(0)");
    //    test_display(&[0xdf, 0x1b], "fistp word ptr [ebx], st");
    test_display(&[0xdf, 0x1b], "fistp word [ebx], st(0)");
    //    test_display(&[0xdf, 0x23], "fbld st, ptr [ebx]");
    test_display(&[0xdf, 0x23], "fbld st(0), mword [ebx]");
    //    test_display(&[0xdf, 0x2b], "fild st, qword ptr [ebx]");
    test_display(&[0xdf, 0x2b], "fild st(0), qword [ebx]");
    //    test_display(&[0xdf, 0x33], "fbstp ptr [ebx], st");
    test_display(&[0xdf, 0x33], "fbstp mword [ebx], st(0)");
    //    test_display(&[0xdf, 0x3b], "fistp qword ptr [ebx], st");
    test_display(&[0xdf, 0x3b], "fistp qword [ebx], st(0)");
    //    test_display(&[0xdf, 0xc3], "ffreep st(3)");
    test_display(&[0xdf, 0xc3], "ffreep st(3)");
    //    test_display(&[0xdf, 0xcb], "fxch st, st(3)");
    test_display(&[0xdf, 0xcb], "fxch st(0), st(3)");
    //    test_display(&[0xdf, 0xd3], "fstp st(3), st");
    test_display(&[0xdf, 0xd3], "fstp st(3), st(0)");
    //    test_display(&[0xdf, 0xdb], "fstp st(3), st");
    test_display(&[0xdf, 0xdb], "fstp st(3), st(0)");
    test_display(&[0xdf, 0xe0], "fnstsw ax");
    test_invalid(&[0xdf, 0xe1]);
    test_invalid(&[0xdf, 0xe2]);
    test_invalid(&[0xdf, 0xe3]);
    test_invalid(&[0xdf, 0xe4]);
    test_invalid(&[0xdf, 0xe5]);
    test_invalid(&[0xdf, 0xe6]);
    test_invalid(&[0xdf, 0xe7]);
    //    test_display(&[0xdf, 0xeb], "fucomip st, st(3)");
    test_display(&[0xdf, 0xeb], "fucomip st(0), st(3)");
    //    test_display(&[0xdf, 0xf3], "fcomip st, st(3)");
    test_display(&[0xdf, 0xf3], "fcomip st(0), st(3)");
    test_invalid(&[0xdf, 0xf8]);
    test_invalid(&[0xdf, 0xf9]);
    test_invalid(&[0xdf, 0xfa]);
    test_invalid(&[0xdf, 0xfb]);
    test_invalid(&[0xdf, 0xfc]);
    test_invalid(&[0xdf, 0xfd]);
    test_invalid(&[0xdf, 0xfe]);
    test_invalid(&[0xdf, 0xff]);
}

#[test]
fn mishegos_finds() {
    test_invalid(&[0xc5, 0x8c, 0x77]);
    test_display(
        &[0x0f, 0xfc, 0xaf, 0x40, 0x38, 0x25, 0xbf],
        "paddb mm5, qword [edi - 0x40dac7c0]",
    );
    test_invalid(&[0xf3, 0x67, 0x0f, 0x3a, 0xf0, 0xfb, 0xb4]);
    test_display(&[0x65, 0x66, 0x0f, 0x01, 0xdc], "stgi");
    test_display(&[0x66, 0x0f, 0x01, 0xd8], "vmrun eax");
    test_invalid(&[
        0x2e, 0x2e, 0xf2, 0x36, 0x0f, 0xb2, 0xdb, 0x42, 0xd6, 0xa3, 0x16,
    ]);
    test_display(&[0x65, 0x67, 0x65, 0x65, 0x0f, 0x0e], "femms");
    test_display(
        &[0x26, 0x66, 0x67, 0x0f, 0x38, 0xdf, 0xe4],
        "aesdeclast xmm4, xmm4",
    );
    test_display(
        &[0x65, 0x66, 0x66, 0x64, 0x0f, 0x38, 0xdb, 0x0f],
        "aesimc xmm1, xmmword fs:[edi]",
    );
    test_invalid(&[0xf3, 0xf2, 0x0f, 0xae, 0x8f, 0x54, 0x3c, 0x58, 0xb7]);
    /*
    test_display(&[652e662e0f3814ff], "blendvps");
    test_display(&[66666565450f3acf2b4b], "gf2 ");
    */

    // might just be yax trying to do a f20f decode when it should not be f2
    // impossible instruction if operands could be read: lock is illegal here.
    // test_display(&[f06565f2640f16], "???");
    //    test_display(&[0x0f, 0x38, 0xf6, 0x8c, 0x98, 0x4d, 0x33, 0xf5, 0xd3, ], "wrssd");
    test_display(
        &[0x26, 0x36, 0x0f, 0x0f, 0x70, 0xfb, 0x0c],
        "pi2fw mm6, qword ss:[eax - 0x5]",
    );
    test_display(&[0x0f, 0xc7, 0x0f], "cmpxchg8b qword [edi]");
    test_display(
        &[0x66, 0x3e, 0x26, 0x2e, 0x2e, 0x0f, 0x38, 0x2a, 0x2b],
        "movntdqa xmm5, xmmword cs:[ebx]",
    );
    test_display(
        &[0x66, 0x2e, 0x67, 0x0f, 0x3a, 0x0d, 0xb8, 0xf0, 0x2f, 0x7c],
        "blendpd xmm7, xmmword cs:[bx + si + 0x2ff0], 0x7c",
    );
    test_display(
        &[
            0x66, 0x66, 0x64, 0x3e, 0x0f, 0x38, 0x23, 0x9d, 0x69, 0x0f, 0xa8, 0x2d,
        ],
        "pmovsxwd xmm3, qword [ebp + 0x2da80f69]",
    );
    test_display(
        &[0x2e, 0x66, 0x26, 0x64, 0x0f, 0x3a, 0x21, 0x0b, 0xb1],
        "insertps xmm1, dword fs:[ebx], -0x4f",
    );
    test_display(
        &[
            0x66, 0x26, 0x0f, 0x3a, 0x42, 0x96, 0x74, 0x29, 0x96, 0xf9, 0x6a,
        ],
        "mpsadbw xmm2, xmmword es:[esi - 0x669d68c], 0x6a",
    );
    test_display(
        &[0x67, 0x26, 0x66, 0x65, 0x0f, 0x38, 0x3f, 0x9d, 0xcc, 0x03],
        "pmaxud xmm3, xmmword gs:[di + 0x3cc]",
    );
    test_display(
        &[0x36, 0x36, 0x2e, 0x0f, 0x38, 0xf9, 0x55, 0x3e],
        "movdiri dword cs:[ebp + 0x3e], edx",
    );
    test_invalid(&[0x66, 0x2e, 0x64, 0x66, 0x0f, 0x38, 0xf8, 0xe2]);
    test_display(
        &[0x67, 0x66, 0x65, 0x3e, 0x0f, 0x6d, 0xd1],
        "punpckhqdq xmm2, xmm1",
    );
    test_display(
        &[0x2e, 0x66, 0x0f, 0x3a, 0x0d, 0x40, 0x2d, 0x57],
        "blendpd xmm0, xmmword cs:[eax + 0x2d], 0x57",
    );
    test_display(
        &[0xf2, 0x3e, 0x26, 0x67, 0x0f, 0xf0, 0xa0, 0x1b, 0x5f],
        "lddqu xmm4, xmmword es:[bx + si + 0x5f1b]",
    );
    test_display(
        &[0x2e, 0x3e, 0x66, 0x3e, 0x0f, 0x3a, 0x41, 0x30, 0x48],
        "dppd xmm6, xmmword [eax], 0x48",
    );

    test_display(&[0x2e, 0x36, 0x0f, 0x18, 0xe7], "nop edi");
    test_display(&[0x65, 0xf0, 0x87, 0x0f], "lock xchg dword gs:[edi], ecx");
    test_display(
        &[0x66, 0x0f, 0x3a, 0x44, 0x88, 0xb3, 0xad, 0x26, 0x35, 0x75],
        "pclmulqdq xmm1, xmmword [eax + 0x3526adb3], 0x75",
    );
    test_display(&[0x0f, 0xff, 0x6b, 0xac], "ud0 ebp, dword [ebx - 0x54]");

    test_display(
        &[
            0xf2, 0xf2, 0x2e, 0x36, 0x0f, 0x38, 0xf8, 0x83, 0x09, 0x1c, 0x9d, 0x3f,
        ],
        "enqcmd eax, zmmword ss:[ebx + 0x3f9d1c09]",
    );
    test_display(
        &[0x3e, 0x64, 0xf3, 0x64, 0x0f, 0x38, 0xf8, 0x72, 0x54],
        "enqcmds esi, zmmword fs:[edx + 0x54]",
    );
    test_invalid(&[0xf3, 0x0f, 0x38, 0xf8, 0xf3]);

    test_display(
        &[0xf3, 0x64, 0x2e, 0x65, 0x0f, 0x38, 0xdc, 0xe8],
        "loadiwkey xmm5, xmm0",
    );

    test_invalid(&[0xf3, 0x2e, 0x0f, 0x6a, 0x18]);
}

#[test]
fn cet() {
    // see
    // https://software.intel.com/sites/default/files/managed/4d/2a/control-flow-enforcement-technology-preview.pdf
    // includes encodings:
    // wruss{d,q} 066 f 38 f5
    // wrss{d,q} 0f 38 f6
    // rstorssp f3 0f 01 /5
    // saveprevssp f3 0f 01 ea
    // rdssp{d,q} f3 0f 1e
    // incssp{d,q} f3 0f ae /5
    // test_display(&[0x0f, 0x38, 0xf6, 0x8c, 0x98, 0x4d, 0x33, 0xf5, 0xd3, ], "wrssd [eax + ebx * 4 - 0x2c0accb3], ecx");
    // setssbsy f3 0f 01 e8
    // clrssbsy f3 0f ae /6
    // endbr64 f3 0f ae fa
    // endbr32 f3 0f ae fb
    test_display(&[0xf3, 0x0f, 0xae, 0xe9], "incssp ecx");
    test_display(&[0x3e, 0x0f, 0x38, 0xf6, 0x23], "wrss dword [ebx], esp");
    test_display(
        &[0x66, 0x0f, 0x38, 0xf5, 0x47, 0xe9],
        "wruss dword [edi - 0x17], eax",
    );
    test_invalid(&[0x0f, 0x38, 0xf5, 0x47, 0xe9]);
    test_invalid(&[0x66, 0x3e, 0x65, 0x3e, 0x0f, 0x38, 0xf5, 0xf0]);
    test_display(&[0xf3, 0x0f, 0x01, 0xe8], "setssbsy");
    test_display(&[0xf3, 0x0f, 0x01, 0xea], "saveprevssp");
    test_display(&[0x66, 0xf3, 0x0f, 0x01, 0xe8], "setssbsy");
    test_display(&[0x66, 0xf3, 0x0f, 0x01, 0xea], "saveprevssp");
    test_display(&[0xf3, 0x66, 0x0f, 0x01, 0xe8], "setssbsy");
    test_display(&[0xf3, 0x66, 0x0f, 0x01, 0xea], "saveprevssp");
    test_display(&[0xf3, 0x0f, 0x01, 0x29], "rstorssp qword [ecx]");
    test_display(&[0xf3, 0x66, 0x0f, 0x01, 0x29], "rstorssp qword [ecx]");
    test_display(&[0xf3, 0x0f, 0xae, 0x30], "clrssbsy qword [eax]");
}

#[test]
fn sse4a() {
    fn test_instr(bytes: &[u8], text: &'static str) {
        test_display_under(&Decoder::minimal().with_sse4a(), bytes, text);
        test_display_under(&Decoder::default(), bytes, text);
        test_invalid_under(&Decoder::minimal(), bytes);
    }

    test_instr(&[0xf2, 0x0f, 0x2b, 0x06], "movntsd qword [esi], xmm0");
    test_invalid(&[0xf2, 0x0f, 0x2b, 0xc6]);
    test_instr(&[0xf3, 0x0f, 0x2b, 0x06], "movntss dword [esi], xmm0");
    test_invalid(&[0xf3, 0x0f, 0xba, 0xc6]);
    test_instr(&[0x66, 0xf2, 0x0f, 0x79, 0xcf], "insertq xmm1, xmm7");
    test_invalid(&[0x66, 0xf2, 0x0f, 0x79, 0x0f]);
    test_instr(&[0xf2, 0x0f, 0x79, 0xcf], "insertq xmm1, xmm7");
    test_instr(
        &[0xf2, 0x0f, 0x78, 0xf1, 0x4e, 0x76],
        "insertq xmm6, xmm1, 0x4e, 0x76",
    );
    test_invalid(&[0xf2, 0x0f, 0x79, 0x0f]);
    test_instr(&[0x66, 0x0f, 0x79, 0xcf], "extrq xmm1, xmm7");
    test_invalid(&[0x66, 0x0f, 0x79, 0x0f]);
    test_instr(
        &[0x66, 0x0f, 0x78, 0xc1, 0x4e, 0x76],
        "extrq xmm1, 0x4e, 0x76",
    );
    test_invalid(&[0x66, 0x0f, 0x78, 0xc9, 0x4e, 0x76]);
}

#[test]
fn threednow() {
    test_display(&[0x0f, 0x0f, 0xe0, 0x8a], "pfnacc mm4, mm0");
    test_display(&[0x0f, 0x0f, 0x38, 0x8e], "pfpnacc mm7, qword [eax]");
    test_display(&[0x65, 0x67, 0x65, 0x65, 0x0f, 0x0e], "femms");
    test_display(
        &[0x3e, 0xf3, 0x2e, 0xf2, 0x0f, 0x0f, 0x64, 0x93, 0x93, 0xa4],
        "pfmax mm4, qword cs:[ebx + edx * 4 - 0x6d]",
    );
    test_display(
        &[0x26, 0x36, 0x0f, 0x0f, 0x70, 0xfb, 0x0c],
        "pi2fw mm6, qword ss:[eax - 0x5]",
    );
    test_display(&[0x66, 0x0f, 0x0f, 0xc6, 0xb7], "pmulhrw mm0, mm6");
    test_display(&[0x0f, 0x0f, 0xc6, 0xb7], "pmulhrw mm0, mm6");
}

// first appeared in tremont
#[test]
fn direct_stores() {
    test_display(
        &[0x36, 0x36, 0x2e, 0x0f, 0x38, 0xf9, 0x55, 0x3e],
        "movdiri dword cs:[ebp + 0x3e], edx",
    );
    test_invalid(&[
        0x36, 0x26, 0x66, 0x0f, 0x38, 0xf8, 0xad, 0x0b, 0x08, 0x29, 0x07,
    ]);
}

#[test]
fn key_locker() {
    test_display(
        &[0xf3, 0x64, 0x2e, 0x65, 0x0f, 0x38, 0xdc, 0xe8],
        "loadiwkey xmm5, xmm0",
    );
    test_display(&[0xf3, 0x0f, 0x38, 0xfa, 0xde], "encodekey128 ebx, esi");
    test_display(&[0xf3, 0x0f, 0x38, 0xfb, 0xde], "encodekey256 ebx, esi");
}

// these uinter test cases come from llvm:
// https://reviews.llvm.org/differential/changeset/?ref=2226860
#[test]
fn uintr() {
    test_display(&[0xf3, 0x0f, 0x01, 0xec], "uiret");
    test_display(&[0xf3, 0x0f, 0x01, 0xed], "testui");
    test_display(&[0xf3, 0x0f, 0x01, 0xee], "clui");
    test_display(&[0xf3, 0x0f, 0x01, 0xef], "stui");
    test_display(&[0xf3, 0x0f, 0xc7, 0xf0], "senduipi eax");
    test_display(&[0xf3, 0x0f, 0xc7, 0xf2], "senduipi edx");
}

// started shipping in sapphire rapids
#[test]
fn enqcmd() {
    test_display(
        &[
            0xf2, 0xf2, 0x2e, 0x36, 0x0f, 0x38, 0xf8, 0x83, 0x09, 0x1c, 0x9d, 0x3f,
        ],
        "enqcmd eax, zmmword ss:[ebx + 0x3f9d1c09]",
    );
    test_display(
        &[0x3e, 0x64, 0xf3, 0x64, 0x0f, 0x38, 0xf8, 0x72, 0x54],
        "enqcmds esi, zmmword fs:[edx + 0x54]",
    );
}

#[test]
fn gfni() {
    test_display(
        &[
            0x3e, 0x64, 0x64, 0x66, 0x0f, 0x3a, 0xcf, 0xba, 0x13, 0x23, 0x04, 0xba, 0x6b,
        ],
        "gf2p8affineinvqb xmm7, xmmword fs:[edx - 0x45fbdced], 0x6b",
    );
    test_display(
        &[
            0x66, 0x36, 0x0f, 0x3a, 0xce, 0x8c, 0x56, 0x9e, 0x82, 0xd1, 0xbe, 0xad,
        ],
        "gf2p8affineqb xmm1, xmmword ss:[esi + edx * 2 - 0x412e7d62], 0xad",
    );
    test_display(
        &[0x66, 0x0f, 0x38, 0xcf, 0x1c, 0x54],
        "gf2p8mulb xmm3, xmmword [esp + edx * 2]",
    );
}

#[test]
fn tdx() {
    test_display(&[0x66, 0x0f, 0x01, 0xcc], "tdcall");
    test_display(&[0x66, 0x0f, 0x01, 0xcd], "seamret");
    test_display(&[0x66, 0x0f, 0x01, 0xce], "seamops");
    test_display(&[0x66, 0x0f, 0x01, 0xcf], "seamcall");
}

#[test]
fn tsxldtrk() {
    test_display(&[0xf2, 0x0f, 0x01, 0xe8], "xsusldtrk");
    test_display(&[0xf2, 0x0f, 0x01, 0xe9], "xresldtrk");
}

#[test]
fn sevsnp() {
    test_display(&[0xf3, 0x0f, 0x01, 0xff], "psmash");
    test_display(&[0xf2, 0x0f, 0x01, 0xff], "pvalidate");
    test_display(&[0xf3, 0x0f, 0x01, 0xfe], "rmpadjust");
    test_display(&[0xf2, 0x0f, 0x01, 0xfe], "rmpupdate");
}

// some test cases are best just lifted from llvm or gcc.
#[test]
fn from_llvm() {
    test_display(&[0xf3, 0x0f, 0x3a, 0xf0, 0xc0, 0x01], "hreset 0x1");
}

#[test]
fn from_reports() {
    // negative compressed evex displacements should not overflow and panic
    test_display(
        &[0x62, 0xf2, 0x6d, 0xac, 0x00, 0x59, 0xa7],
        "vpshufb ymm3{k4}{z}, ymm2, ymmword [ecx - 0xb20]",
    );
    test_display(
        &[0x62, 0xf2, 0xfd, 0x0f, 0x8a, 0x62, 0xf2],
        "vcompresspd xmmword [edx - 0x70]{k7}, xmm4",
    );
    test_display(&[0xf3, 0x0f, 0x1e, 0x0f], "nop dword [edi], ecx");
}

mod reg_masks {
    use crate::protected_mode::RegSpec;

    #[test]
    #[should_panic]
    fn invalid_mask_reg() {
        RegSpec::mask(8);
    }

    #[test]
    #[should_panic]
    fn invalid_dword_reg() {
        RegSpec::d(8);
    }

    #[test]
    #[should_panic]
    fn invalid_word_reg() {
        RegSpec::w(8);
    }

    #[test]
    #[should_panic]
    fn invalid_byte_reg() {
        RegSpec::b(8);
    }

    #[test]
    #[should_panic]
    fn invalid_x87_reg() {
        RegSpec::st(8);
    }

    #[test]
    #[should_panic]
    fn invalid_xmm_reg() {
        RegSpec::xmm(32);
    }

    #[test]
    #[should_panic]
    fn invalid_ymm_reg() {
        RegSpec::ymm(32);
    }

    #[test]
    #[should_panic]
    fn invalid_zmm_reg() {
        RegSpec::zmm(32);
    }
}
