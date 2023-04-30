use std::fmt::Write;

use crate::long_mode::Decoder;
use decoder::ToTokens;
use yaxpeax_arch::{AddressBase, Decoder, LengthedInstruction};

fn test_display(data: &[u8], expected: &'static str) {
    let dekoder = Decoder::default();
    let mut stream = decoder::TokenStream::new();
    let mut hex = String::new();
    for b in data {
        write!(hex, "{:02x}", b).unwrap();
    }

    let mut reader = yaxpeax_arch::U8Reader::new(data);
    match dekoder.decode(&mut reader) {
        Ok(instr) => {
            instr.tokenize(&mut stream);
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
                (0u64.wrapping_offset(instr.len()).to_linear()) as usize,
                data.len(),
                "instruction length is incorrect, wanted instruction {}",
                expected
            );
        }
        Err(e) => {
            assert!(
                false,
                "decode error ({}) for {} under decoder {}:\n  expected: {}\n",
                e, hex, dekoder, expected
            );
        }
    }
}

// decided i do not like at&t syntax much at all. not going to write a formatter for it. some test
// cases will live on in case someone else feels like adding one, or i get mad enough to do it.
#[allow(unreachable_code)]
#[ignore]
#[test]
fn test_instructions_atnt() {
    // `ignore` is now used to avoid running (slow!) exhaustive tests in a default `cargo test`.
    // running exhaustive tests now runs these tests, which fail. so instead, return early.
    return;
    // just modrm
    test_display(&[0x33, 0x08], "xor (%rax), %ecx");
    test_display(&[0x33, 0x20], "xor (%rax), %esp");
    test_display(
        &[0x33, 0x05, 0x78, 0x56, 0x34, 0x12],
        "xor 0x12345678(%rip), %eax",
    );
    test_display(&[0x33, 0x41, 0x23], "xor 0x23(%rcx), %eax");
    test_display(
        &[0x33, 0x81, 0x23, 0x01, 0x65, 0x43],
        "xor %0x43650123, %eax",
    );
    test_display(&[0x33, 0xc1], "xor %ecx, %eax");

    // modrm + rex.w
    test_display(&[0x48, 0x33, 0x08], "xor (%rax), %rcx");
    test_display(&[0x48, 0x33, 0x20], "xor (%rax), %rsp");
    test_display(
        &[0x48, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12],
        "xor 0x12345678(%rip), %rax",
    );
    test_display(&[0x48, 0x33, 0x41, 0x23], "xor 0x23(%rcx), %rax");
    test_display(
        &[0x48, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43],
        "xor 0x43650123(%rcx), %rax",
    );
    test_display(&[0x48, 0x33, 0xc1], "xor %rcx, %rax");

    // modrm + rex.r
    test_display(&[0x44, 0x33, 0x08], "xor (%rax), %r9d");
    test_display(&[0x44, 0x33, 0x20], "xor (%rax), %r12d");
    test_display(
        &[0x44, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12],
        "xor 0x12345678(%rip), %r8d",
    );
    test_display(&[0x44, 0x33, 0x41, 0x23], "xor 0x23(%rcx), %r8d");
    test_display(
        &[0x44, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43],
        "xor 0x43650123(%rcx), %r8d",
    );
    test_display(&[0x44, 0x33, 0xc1], "xor %ecx, %r8d");

    // modrm + rex.rb
    test_display(&[0x45, 0x33, 0x08], "xor (%r8), %r9d");
    test_display(&[0x45, 0x33, 0x20], "xor (%r8), %r12d");
    test_display(
        &[0x45, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12],
        "xor 0x12345678(%rip), %r8d",
    );
    test_display(&[0x45, 0x33, 0x41, 0x23], "xor 0x23(%r9), %r8d");
    test_display(
        &[0x45, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43],
        "xor 0x43650123(%r9), %r8d",
    );
    test_display(&[0x45, 0x33, 0xc1], "xor %r9d, %r8d");

    // sib
    test_display(
        &[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44],
        "xor (0x44332211), %eax",
    );
    test_display(
        &[0x41, 0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44],
        "xor (0x44332211), %eax",
    );

    test_display(&[0x41, 0x33, 0x44, 0x65, 0x11], "xor 0x11(%r13), %eax");

    test_display(
        &[0x42, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50],
        "xor 0x50403020(,%r12,1), %esi",
    );

    test_display(&[0x4f, 0x0f, 0xe7, 0x03], "movntq %mm0, (%r11)");
    test_display(&[0x0f, 0xe7, 0x03], "movntq %mm0, (%rbx)");

    test_display(&[0x4f, 0x0f, 0x7f, 0x0f], "movq %mm1, (%r15)");
    test_display(&[0x0f, 0xc4, 0xc0, 0x14], "pinsrw $0x14, %eax, %mm0");

    test_display(&[0x4f, 0x0f, 0xd1, 0x00], "psrlw (%r8), %mm0");
    test_display(
        &[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77],
        "pmulhw 0x77ccbbaa(%rip), %mm7",
    );
}
