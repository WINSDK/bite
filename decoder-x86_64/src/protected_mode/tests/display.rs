use std::fmt::Write;

use crate::protected_mode::Decoder;
use decoder::{Decodable, Decoded, Reader, ToTokens};

fn test_display(data: &[u8], expected: &'static str) {
    let dekoder = Decoder::default();
    let mut stream = decoder::TokenStream::new();
    let mut hex = String::new();
    for b in data {
        write!(hex, "{:02x}", b).unwrap();
    }

    let mut reader = Reader::new(data);
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
    test_display(&[0x33, 0x08], "xor (%eax), %ecx");
    test_display(&[0x33, 0x20], "xor (%eax), %esp");
    test_display(
        &[0x33, 0x05, 0x78, 0x56, 0x34, 0x12],
        "xor (0x12345678), %eax",
    );
    test_display(&[0x33, 0x41, 0x23], "xor 0x23(%ecx), %eax");
    test_display(
        &[0x33, 0x81, 0x23, 0x01, 0x65, 0x43],
        "xor %0x43650123, %eax",
    );
    test_display(&[0x33, 0xc1], "xor %ecx, %eax");

    // sib
    test_display(
        &[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44],
        "xor (0x44332211), %eax",
    );
    test_display(
        &[0x41, 0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44],
        "xor (0x44332211), %eax",
    );

    test_display(&[0x33, 0x44, 0x65, 0x11], "xor 0x11(%r13), %eax");

    test_display(
        &[0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50],
        "xor 0x50403020, %esi",
    );

    test_display(&[0x0f, 0xe7, 0x03], "movntq %mm0, (%ebx)");

    test_display(&[0x0f, 0x7f, 0x0f], "movq %mm1, (%edi)");
    test_display(&[0x0f, 0xc4, 0xc0, 0x14], "pinsrw $0x14, %eax, %mm0");

    test_display(&[0x0f, 0xd1, 0x00], "psrlw (%eax), %mm0");
    test_display(
        &[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77],
        "pmulhw 0x77ccbbaa, %mm7",
    );
}
