use std::fmt::Write;

use yaxpeax_arch::{AddressBase, Decoder, LengthedInstruction};
use crate::long_mode::{DisplayStyle, InstDecoder};

fn test_display(data: &[u8], expected: &'static str) {
    test_display_under(&InstDecoder::default(), DisplayStyle::Intel, data, expected);
}

fn test_c_display(data: &[u8], expected: &'static str) {
    test_display_under(&InstDecoder::default(), DisplayStyle::C, data, expected);
}

fn test_display_under(decoder: &InstDecoder, style: DisplayStyle, data: &[u8], expected: &'static str) {
    let mut hex = String::new();
    for b in data {
        write!(hex, "{:02x}", b).unwrap();
    }
    let mut reader = yaxpeax_arch::U8Reader::new(data);
    match decoder.decode(&mut reader) {
        Ok(instr) => {
            let text = format!("{}", instr.display_with(style));
            assert!(
                text == expected,
                "display error for {}:\n  decoded: {:?} under decoder {}\n displayed: {}\n expected: {}\n",
                hex,
                instr,
                decoder,
                text,
                expected
            );
            // while we're at it, test that the instruction is as long, and no longer, than its
            // input
            assert_eq!((0u64.wrapping_offset(instr.len()).to_linear()) as usize, data.len(), "instruction length is incorrect, wanted instruction {}", expected);
        },
        Err(e) => {
            assert!(false, "decode error ({}) for {} under decoder {}:\n  expected: {}\n", e, hex, decoder, expected);
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
    test_display(&[0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "xor 0x12345678(%rip), %eax");
    test_display(&[0x33, 0x41, 0x23], "xor 0x23(%rcx), %eax");
    test_display(&[0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "xor %0x43650123, %eax");
    test_display(&[0x33, 0xc1], "xor %ecx, %eax");

    // modrm + rex.w
    test_display(&[0x48, 0x33, 0x08], "xor (%rax), %rcx");
    test_display(&[0x48, 0x33, 0x20], "xor (%rax), %rsp");
    test_display(&[0x48, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "xor 0x12345678(%rip), %rax");
    test_display(&[0x48, 0x33, 0x41, 0x23], "xor 0x23(%rcx), %rax");
    test_display(&[0x48, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "xor 0x43650123(%rcx), %rax");
    test_display(&[0x48, 0x33, 0xc1], "xor %rcx, %rax");

    // modrm + rex.r
    test_display(&[0x44, 0x33, 0x08], "xor (%rax), %r9d");
    test_display(&[0x44, 0x33, 0x20], "xor (%rax), %r12d");
    test_display(&[0x44, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "xor 0x12345678(%rip), %r8d");
    test_display(&[0x44, 0x33, 0x41, 0x23], "xor 0x23(%rcx), %r8d");
    test_display(&[0x44, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "xor 0x43650123(%rcx), %r8d");
    test_display(&[0x44, 0x33, 0xc1], "xor %ecx, %r8d");

    // modrm + rex.rb
    test_display(&[0x45, 0x33, 0x08], "xor (%r8), %r9d");
    test_display(&[0x45, 0x33, 0x20], "xor (%r8), %r12d");
    test_display(&[0x45, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "xor 0x12345678(%rip), %r8d");
    test_display(&[0x45, 0x33, 0x41, 0x23], "xor 0x23(%r9), %r8d");
    test_display(&[0x45, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "xor 0x43650123(%r9), %r8d");
    test_display(&[0x45, 0x33, 0xc1], "xor %r9d, %r8d");

    // sib
    test_display(&[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44], "xor (0x44332211), %eax");
    test_display(&[0x41, 0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44], "xor (0x44332211), %eax");

    test_display(&[0x41, 0x33, 0x44, 0x65, 0x11], "xor 0x11(%r13), %eax");

    test_display(&[0x42, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50], "xor 0x50403020(,%r12,1), %esi");

    test_display(&[0x4f, 0x0f, 0xe7, 0x03], "movntq %mm0, (%r11)");
    test_display(&[0x0f, 0xe7, 0x03], "movntq %mm0, (%rbx)");

    test_display(&[0x4f, 0x0f, 0x7f, 0x0f], "movq %mm1, (%r15)");
    test_display(&[0x0f, 0xc4, 0xc0, 0x14], "pinsrw $0x14, %eax, %mm0");

    test_display(&[0x4f, 0x0f, 0xd1, 0x00], "psrlw (%r8), %mm0");
    test_display(&[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77], "pmulhw 0x77ccbbaa(%rip), %mm7");
}

#[test]
fn test_instructions_c() {
    // just modrm
    test_c_display(&[0x33, 0x08], "ecx ^= [rax]");
    test_c_display(&[0x33, 0x20], "esp ^= [rax]");
    test_c_display(&[0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "eax ^= [rip + 0x12345678]");
    test_c_display(&[0x33, 0x41, 0x23], "eax ^= [rcx + 0x23]");
    test_c_display(&[0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "eax ^= [rcx + 0x43650123]");
    test_c_display(&[0x33, 0xc1], "eax ^= ecx");

    // modrm + rex.w
    test_c_display(&[0x48, 0x33, 0x08], "rcx ^= [rax]");
    test_c_display(&[0x48, 0x33, 0x20], "rsp ^= [rax]");
    test_c_display(&[0x48, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "rax ^= [rip + 0x12345678]");
    test_c_display(&[0x48, 0x33, 0x41, 0x23], "rax ^= [rcx + 0x23]");
    test_c_display(&[0x48, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "rax ^= [rcx + 0x43650123]");
    test_c_display(&[0x48, 0x33, 0xc1], "rax ^= rcx");

    // modrm + rex.r
    test_c_display(&[0x44, 0x33, 0x08], "r9d ^= [rax]");
    test_c_display(&[0x44, 0x33, 0x20], "r12d ^= [rax]");
    test_c_display(&[0x44, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "r8d ^= [rip + 0x12345678]");
    test_c_display(&[0x44, 0x33, 0x41, 0x23], "r8d ^= [rcx + 0x23]");
    test_c_display(&[0x44, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "r8d ^= [rcx + 0x43650123]");
    test_c_display(&[0x44, 0x33, 0xc1], "r8d ^= ecx");

    // modrm + rex.rb
    test_c_display(&[0x45, 0x33, 0x08], "r9d ^= [r8]");
    test_c_display(&[0x45, 0x33, 0x20], "r12d ^= [r8]");
    test_c_display(&[0x45, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "r8d ^= [rip + 0x12345678]");
    test_c_display(&[0x45, 0x33, 0x41, 0x23], "r8d ^= [r9 + 0x23]");
    test_c_display(&[0x45, 0x33, 0x81, 0x23, 0x01, 0x65, 0x43], "r8d ^= [r9 + 0x43650123]");
    test_c_display(&[0x45, 0x33, 0xc1], "r8d ^= r9d");

    // sib
    test_c_display(&[0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44], "eax ^= [0x44332211]");
    test_c_display(&[0x41, 0x33, 0x04, 0x25, 0x11, 0x22, 0x33, 0x44], "eax ^= [0x44332211]");

    test_c_display(&[0x41, 0x33, 0x44, 0x65, 0x11], "eax ^= [r13 + 0x11]");

    test_c_display(&[0x42, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50], "esi ^= [r12 * 1 + 0x50403020]");

    test_c_display(&[0x4f, 0x0f, 0xe7, 0x03], "[r11] = movntq(mm0)");
    test_c_display(&[0x0f, 0xe7, 0x03], "[rbx] = movntq(mm0)");

    test_c_display(&[0x4f, 0x0f, 0x7f, 0x0f], "[r15] = movq(mm1)");
    test_c_display(&[0x0f, 0xc4, 0xc0, 0x14], "mm0 = pinsrw(mm0, eax, 0x14)");

    test_c_display(&[0x4f, 0x0f, 0xd1, 0x00], "mm0 = psrlw(mm0, [r8])");
    test_c_display(&[0x0f, 0xe5, 0x3d, 0xaa, 0xbb, 0xcc, 0x77], "mm7 = pmulhw(mm7, [rip + 0x77ccbbaa])");

    test_c_display(&[0xf3, 0x48, 0xa5], "rep qword { es:[rdi++] = ds:[rsi++] }");
    test_c_display(&[0xf3, 0xa5], "rep dword { es:[rdi++] = ds:[rsi++] }");
    test_c_display(&[0xf3, 0x66, 0xa5], "rep word { es:[rdi++] = ds:[rsi++] }");
    test_c_display(&[0xf3, 0xa4], "rep byte { es:[rdi++] = ds:[rsi++] }");

    test_c_display(&[0xf6, 0xc2, 0x18], "rflags = flags(dl & 0x18)");
    test_c_display(&[0xf6, 0xc2, 0x18], "rflags = flags(dl & 0x18)");
    test_c_display(&[0x84, 0xc0], "rflags = flags(al & al)");
    test_c_display(&[0x85, 0xc0], "rflags = flags(eax & eax)");
    test_c_display(&[0x3a, 0xc0], "rflags = flags(al - al)");
    test_c_display(&[0x3b, 0xc0], "rflags = flags(eax - eax)");

    test_c_display(&[0x41, 0x0f, 0xbc, 0xd3], "edx = lsb(r11d) (x86 bsf)");
    test_c_display(&[0xf3, 0x41, 0x0f, 0xbc, 0xd3], "edx = lsb(r11d)");
    // test_c_display(&[0x41, 0x0f, 0xbc, 0xd3], "edx = lsb(r11d) (x86 bsf"); // for non-bm1
    test_c_display(&[0x41, 0x0f, 0xbd, 0xd3], "edx = msb(r11d)");
    // test_c_display(&[0x41, 0x0f, 0xbc, 0xd3], "edx = lsb(r11d) (x86 bsr"); // for non-bm1
    test_c_display(&[0xd2, 0xc0], "al = al rol cl");
    test_c_display(&[0xd2, 0xc8], "al = al ror cl");
    test_c_display(&[0xd2, 0xd0], "al = al rcl cl");
    test_c_display(&[0xd2, 0xd8], "al = al rcr cl");
    test_c_display(&[0xd2, 0xe0], "al = al << cl");
    test_c_display(&[0xd2, 0xe8], "al = al >> cl");
    test_c_display(&[0xd2, 0xf0], "al = al <<< cl");
    test_c_display(&[0xd2, 0xf8], "al = al >>> cl");

    test_c_display(&[0xc4, 0xc3, 0x7b, 0xf0, 0x01, 0x05], "eax = [r9] ror 0x5 (x86 rorx)");
    test_c_display(&[0xc4, 0xc2, 0xe3, 0xf7, 0x01], "rax = [r9] >> rbx (x86 shrx)");
    test_c_display(&[0xc4, 0xc2, 0xe1, 0xf7, 0x01], "rax = [r9] << rbx (x86 shlx)");

    test_c_display(&[0xd2, 0xe0], "al = al << cl");

    test_c_display(&[0x66, 0x0f, 0xac, 0xcf, 0x11], "di = shrd(di, cx, 0x11)");
    test_c_display(&[0x0f, 0xa5, 0xc9], "ecx = shld(ecx, ecx, cl)");

    test_c_display(&[0x66, 0x0f, 0x38, 0xf6, 0x01], "eax += [rcx] + rflags.cf");
    test_c_display(&[0xf3, 0x4f, 0x0f, 0x38, 0xf6, 0x01], "r8 += [r9] + rflags.of");

    test_c_display(&[0xfe, 0x00], "byte [rax]++");
    test_c_display(&[0x66, 0xff, 0x08], "word [rax]--");
    test_c_display(&[0xff, 0x00], "dword [rax]++");
    test_c_display(&[0x48, 0xff, 0x00], "qword [rax]++");

    test_c_display(&[0xff, 0xe0], "jmp rax");
}
