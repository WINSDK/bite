#![cfg(test)]

use object::{Object, ObjectSection, SectionKind};
use decoder::Streamable;

macro_rules! decode_instructions {
    ($code:literal) => {{
        static CRC: crc::Crc<u32> = crc::Crc::<u32>::new(&crc::CRC_32_ISCSI);

        let code = format!("
            #![deny(warnings)]
            #![no_std]
            #![no_main]

            core::arch::global_asm!(\"{}\");

            #[panic_handler]
            fn panic(_: &core::panic::PanicInfo) -> ! {{
                loop {{}}
            }}
        ", $code);

        let mut out_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));

        out_path.push("..");
        out_path.push("target");
        out_path.push(format!("test_riscv{}", CRC.checksum($code.as_bytes())));

        let src_path = out_path.with_extension("rs");

        std::fs::write(&src_path, code).unwrap();

        if cfg!(target_family = "windows") {
            out_path.set_extension("exe");
        }

        let rustc = std::process::Command::new("rustc")
            .arg(format!("-o{}", out_path.display()))
            .arg("--target=riscv64gc-unknown-none-elf")
            .arg("-Cstrip=symbols")
            .arg(format!("{}", src_path.display()))
            .output()?;

        if !rustc.stderr.is_empty() {
            eprintln!("{}", String::from_utf8_lossy(&rustc.stderr[..]));
        }

        if !rustc.status.success() {
            return Err(format!("rustc failed with exit code: {}", rustc.status).into());
        }

        let binary = std::fs::read(out_path).unwrap();
        let binary = object::File::parse(&binary[..])?;
        let section = binary
            .sections()
            .filter(|s| s.kind() == SectionKind::Text)
            .find(|t| t.name() == Ok(".text"))
            .expect("failed to find `.text` section");

        let binary = section.uncompressed_data()?;
        let mut decoded = Vec::new();
        let mut stream = $crate::Stream {
            bytes: &binary[..],
            offset: 0,
            width: 4,
            is_64: true,
            section_base: section.address() as usize,
        };

        while let Some(ref mut inst) = stream.next() {
            match inst {
                Ok(inst) => {
                    let mut fmt = inst.mnemomic.to_string();
                    let operands = &inst.operands[..inst.operand_count];

                    if operands.is_empty() {
                        decoded.push(fmt);
                        continue;
                    }

                    fmt += " ";

                    if operands.len() > 1 {
                        for operand in &operands[..operands.len() - 1] {
                            fmt += operand;
                            fmt += ", ";
                        }
                    }

                    fmt += &operands[operands.len() - 1];
                    decoded.push(fmt);
                },
                Err(..) => decoded.push("????".to_string()),
            }
        }

        decoded
    }};
}

#[test]
fn deref() -> Result<(), Box<dyn std::error::Error>> {
    let decoded = decode_instructions!("
        .global _start
        _start:
            lui	a0, 4096
            li	a1, 12
            sw	a1, 0(a0)
            li	a0, 0
            ret
   ");

    let test = [
        "lui a0, 4096",
        "li a1, 12",
        "sw a1, a0, 0",
        "li a0, 0",
        "ret",
    ];

    for (test, decoded) in test.iter().zip(decoded) {
        if *test != decoded {
            eprintln!("objdump: '{test}' != our: '{decoded}'");
            panic!("instructions don't match");
        }
    }

    Ok(())
}

#[test]
fn sha256() -> Result<(), Box<dyn std::error::Error>> {
    let decoded = decode_instructions!("
        .global _start
        memset:
            li	a3, 0
            beq	a2, a3, .LIB0_2

        .LIB0_1:
            add	a4, a0, a3
            sb	a1, 0(a4)
            addi	a3, a3, 1
            bne	a2, a3, .LIB0_1

        .LIB0_2:
            ret

        sha256_transform:
            addi	sp, sp, -352
            sd	s0, 344(sp)
            sd	s1, 336(sp)
            sd	s2, 328(sp)
            sd	s3, 320(sp)
            sd	s4, 312(sp)
            sd	s5, 304(sp)
            sd	s6, 296(sp)
            sd	s7, 288(sp)
            sd	s8, 280(sp)
            sd	s9, 272(sp)
            sd	s10, 264(sp)
            sd	s11, 256(sp)
            li	a2, 0
            li	a3, 64
            mv	a6, sp
            beq	a2, a3, .LIB1_2

        .LIB1_1:
            add	a5, a1, a2
            lb	s1, 0(a5)
            lbu	s0, 1(a5)
            slli	s1, s1, 24
            lbu	a4, 2(a5)
            slli	s0, s0, 16
            lbu	a5, 3(a5)
            or	s1, s1, s0
            slli	a4, a4, 8
            or	a4, a4, s1
            or	a4, a4, a5
            add	a5, a6, a2
            sw	a4, 0(a5)
            addi	a2, a2, 4
            bne	a2, a3, .LIB1_1

        .LIB1_2:
            li	a1, 0
            li	a7, 192
            mv	a6, sp
            beq	a1, a7, .LIB1_4

        .LIB1_3:
            add	a4, a6, a1
            lwu	a5, 56(a4)
            srli	s1, a5, 17
            slliw	s0, a5, 15
            or	s1, s1, s0
            srli	s0, a5, 19
            slliw	a3, a5, 13
            or	a3, a3, s0
            xor	a3, a3, s1
            lw	s1, 36(a4)
            lwu	s0, 4(a4)
            srli	a5, a5, 10
            xor	a3, a3, a5
            addw	a3, a3, s1
            srli	a5, s0, 7
            slli	s1, s0, 25
            or	a5, a5, s1
            srli	s1, s0, 18
            slli	a2, s0, 14
            or	a2, a2, s1
            lw	s1, 0(a4)
            xor	a2, a2, a5
            srli	a5, s0, 3
            xor	a2, a2, a5
            addw	a3, a3, s1
            addw	a2, a2, a3
            sw	a2, 64(a4)
            addi	a1, a1, 4
            bne	a1, a7, .LIB1_3

        .LIB1_4:
            li	s9, 0
            lw	t5, 80(a0)
            lw	t4, 84(a0)
            lw	t3, 88(a0)
            lw	t2, 92(a0)
            lw	t1, 96(a0)
            lw	t0, 100(a0)
            lw	a7, 104(a0)
            lw	a6, 108(a0)
            li	t6, 256
            lui	a2, 16
            addi	s3, a2, 344
            mv	s2, sp
            mv	s7, t3
            mv	s4, t2
            mv	s6, t1
            mv	a3, t0
            mv	s10, a7
            mv	s5, a6
            mv	s8, t4
            mv	s0, t5

        .LIB1_5:
            mv	s11, s10
            mv	s10, a3
            mv	a3, s6
            mv	s1, s7
            beq	s9, t6, .LIB1_7
            srliw	a4, a3, 6
            slliw	a1, a3, 26
            or	a1, a1, a4
            srliw	a4, a3, 11
            slliw	a2, a3, 21
            or	a2, a2, a4
            xor	a1, a1, a2
            srliw	a2, a3, 25
            slliw	a4, a3, 7
            or	a2, a2, a4
            xor	s6, a1, a2
            and	a2, s10, a3
            not	a4, a3
            and	a4, s11, a4
            add	a5, s3, s9
            lw	a5, 0(a5)
            add	a1, s2, s9
            lw	a1, 0(a1)
            addw	a2, s6, a2
            addw	a2, a2, s5
            addw	a2, a2, a4
            addw	a2, a2, a5
            addw	a1, a1, a2
            srliw	a2, s0, 2
            slliw	a4, s0, 30
            or	a2, a2, a4
            srliw	a4, s0, 13
            slliw	a5, s0, 19
            or	a4, a4, a5
            xor	a2, a2, a4
            srliw	a4, s0, 22
            slli	a5, s0, 10
            or	a4, a4, a5
            xor	a2, a2, a4
            xor	a4, s8, s1
            and	a4, a4, s0
            and	a5, s8, s1
            xor	a4, a4, a5
            addw	a2, a2, a4
            addw	s6, a1, s4
            mv	s7, s8
            mv	s8, s0
            addw	s0, a2, a1
            addi	s9, s9, 4
            mv	s4, s1
            mv	s5, s11
            j	.LIB1_5

        .LIB1_7:
            addw	a1, s0, t5
            sw	a1, 80(a0)
            addw	a1, s8, t4
            sw	a1, 84(a0)
            addw	a1, s1, t3
            sw	a1, 88(a0)
            addw	a1, s4, t2
            sw	a1, 92(a0)
            addw	a1, a3, t1
            sw	a1, 96(a0)
            addw	a1, s10, t0
            sw	a1, 100(a0)
            addw	a1, s11, a7
            sw	a1, 104(a0)
            addw	a1, s5, a6
            sw	a1, 108(a0)
            ld	s0, 344(sp)
            ld	s1, 336(sp)
            ld	s2, 328(sp)
            ld	s3, 320(sp)
            ld	s4, 312(sp)
            ld	s5, 304(sp)
            ld	s6, 296(sp)
            ld	s7, 288(sp)
            ld	s8, 280(sp)
            ld	s9, 272(sp)
            ld	s10, 264(sp)
            ld	s11, 256(sp)
            addi	sp, sp, 352
            ret

        sha256_init:
            lui	a1, 18
            ld	a1, 1632(a1)
            lui	a2, 18
            ld	a2, 1640(a2)
            sd	a1, 80(a0)
            lui	a1, 18
            ld	a1, 1648(a1)
            sd	a2, 88(a0)
            lui	a2, 18
            ld	a2, 1656(a2)
            sd	a1, 96(a0)
            li	a1, 0
            sw	a1, 64(a0)
            sd	a1, 72(a0)
            sd	a2, 104(a0)
            ret

        sha256_update:
            addi	sp, sp, -48
            sd	ra, 40(sp)
            sd	s0, 32(sp)
            sd	s1, 24(sp)
            sd	s2, 16(sp)
            sd	s3, 8(sp)
            sd	s4, 0(sp)
            mv	s3, a2
            mv	s2, a1
            mv	s1, a0
            li	s0, 0
            li	s4, 64

        .LIB3_1:
            slli	a0, s0, 32
            srli	a0, a0, 32
            bgeu	a0, s3, .LIB3_5
            add	a0, a0, s2
            lwu	a1, 64(s1)
            lb	a0, 0(a0)
            add	a1, a1, s1
            sb	a0, 0(a1)
            lw	a0, 64(s1)
            addiw	a0, a0, 1
            sw	a0, 64(s1)
            bne	a0, s4, .LIB3_4
            mv	a0, s1
            mv	a1, s1
            jal	sha256_transform
            ld	a0, 72(s1)
            addi	a0, a0, 512
            sd	a0, 72(s1)
            sw	zero, 64(s1)

        .LIB3_4:
            addiw	s0, s0, 1
            j	.LIB3_1

        .LIB3_5:
            ld	ra, 40(sp)
            ld	s0, 32(sp)
            ld	s1, 24(sp)
            ld	s2, 16(sp)
            ld	s3, 8(sp)
            ld	s4, 0(sp)
            addi	sp, sp, 48
            ret

        sha256_final:
            addi	sp, sp, -32
            sd	ra, 24(sp)
            sd	s0, 16(sp)
            sd	s1, 8(sp)
            mv	s0, a0
            lwu	a0, 64(a0)
            mv	s1, a1
            sext.w	a1, a0
            add	a2, s0, a0
            li	a3, 128
            li	a4, 56
            sb	a3, 0(a2)
            bgeu	a1, a4, .LIB4_3
            addi	a1, s0, 1
            li	a2, 55
            beq	a0, a2, .LIB4_7

        .LIB4_2:
            add	a3, a1, a0
            addi	a0, a0, 1
            sb	zero, 0(a3)
            bne	a0, a2, .LIB4_2
            j	.LIB4_7

        .LIB4_3:
            li	a1, 63

        .LIB4_4:
            addiw	a0, a0, 1
            bltu	a1, a0, .LIB4_6
            add	a2, s0, a0
            sb	zero, 0(a2)
            j	.LIB4_4

        .LIB4_6:
            mv	a0, s0
            mv	a1, s0
            jal	sha256_transform
            li	a2, 56
            mv	a0, s0
            li	a1, 0
            jal	memset

        .LIB4_7:
            lw	a0, 64(s0)
            ld	a1, 72(s0)
            slli	a0, a0, 35
            srli	a0, a0, 32
            add	a0, a0, a1
            sd	a0, 72(s0)
            sb	a0, 63(s0)
            srli	a1, a0, 8
            sb	a1, 62(s0)
            srli	a1, a0, 16
            sb	a1, 61(s0)
            srli	a1, a0, 24
            sb	a1, 60(s0)
            srli	a1, a0, 32
            sb	a1, 59(s0)
            srli	a1, a0, 40
            sb	a1, 58(s0)
            srli	a1, a0, 48
            sb	a1, 57(s0)
            srli	a0, a0, 56
            sb	a0, 56(s0)
            mv	a0, s0
            mv	a1, s0
            jal	sha256_transform
            li	a0, 0
            addi	a1, s1, 16
            li	a2, 4
            li	a3, 24
            beq	a0, a2, .LIB4_9

        .LIB4_8:
            lw	a4, 80(s0)
            slliw	a5, a0, 3
            subw	a5, a3, a5
            srlw	a4, a4, a5
            add	s1, a1, a0
            sb	a4, -16(s1)
            lw	a4, 84(s0)
            srlw	a4, a4, a5
            sb	a4, -12(s1)
            lw	a4, 88(s0)
            srlw	a4, a4, a5
            sb	a4, -8(s1)
            lw	a4, 92(s0)
            srlw	a4, a4, a5
            sb	a4, -4(s1)
            lw	a4, 96(s0)
            srlw	a4, a4, a5
            sb	a4, 0(s1)
            lw	a4, 100(s0)
            srlw	a4, a4, a5
            sb	a4, 4(s1)
            lw	a4, 104(s0)
            srlw	a4, a4, a5
            sb	a4, 8(s1)
            lw	a4, 108(s0)
            srlw	a4, a4, a5
            sb	a4, 12(s1)
            addi	a0, a0, 1
            bne	a0, a2, .LIB4_8

        .LIB4_9:
            ld	ra, 24(sp)
            ld	s0, 16(sp)
            ld	s1, 8(sp)
            addi	sp, sp, 32
            ret

        _start:
            addi	sp, sp, -128
            sd	ra, 120(sp)
            sw	zero, 72(sp)
            sd	zero, 80(sp)
            lui	a0, 18
            ld	a0, 1664(a0)
            lui	a1, 18
            ld	a1, 1672(a1)
            lui	a2, 18
            ld	a2, 1680(a2)
            lui	a3, 18
            ld	a3, 1688(a3)
            sd	a0, 88(sp)
            sd	a1, 96(sp)
            sd	a2, 104(sp)
            sd	a3, 112(sp)
            addi	a0, sp, 8
            lui	a1, 1
            li	a2, 1024
            jal	sha256_update
            addi	a0, sp, 8
            lui	a1, 2
            jal	sha256_final
            ld	ra, 120(sp)
            addi	sp, sp, 128
            ret
   ");

    let test = [
        "li a3, 0",
        "beq a2, a3, 0x11134",
        "add a4, a0, a3",
        "sb a1, a4, 0",
        "addi a3, a3, 1",
        "bne a2, a3, 0x11126",
        "ret",
        "addi16sp -352",
        "sdsp s0, 344",
        "sdsp s1, 336",
        "sdsp s2, 328",
        "sdsp s3, 320",
        "sdsp s4, 312",
        "sdsp s5, 304",
        "sdsp s6, 296",
        "sdsp s7, 288",
        "sdsp s8, 280",
        "sdsp s9, 272",
        "sdsp s10, 264",
        "sdsp s11, 256",
        "li a2, 0",
        "li a3, 64",
        "mv a6, sp",
        "beq a2, a3, 0x11188",
        "add a5, a1, a2",
        "lb s1, a5, 0",
        "lbu s0, a5, 1",
        "slli s1, s1, 24",
        "lbu a4, a5, 2",
        "slli s0, s0, 16",
        "lbu a5, a5, 3",
        "or s1, s1, s0",
        "slli a4, a4, 8",
        "or a4, a4, s1",
        "or a4, a4, a5",
        "add a5, a6, a2",
        "sw a4, a5, 0",
        "addi a2, a2, 4",
        "bne a2, a3, 0x1115c",
        "li a1, 0",
        "li a7, 192",
        "mv a6, sp",
        "beq a1, a7, 0x111e8",
        "add a4, a6, a1",
        "lwu a5, a4, 56",
        "srli s1, a5, 17",
        "slliw s0, a5, 15",
        "or s1, s1, s0",
        "srli s0, a5, 19",
        "slliw a3, a5, 13",
        "or a3, a3, s0",
        "xor a3, a3, s1",
        "lw s1, a4, 36",
        "lwu s0, a4, 4",
        "srli a5, a5, 10",
        "xor a3, a3, a5",
        "addw a3, a3, s1",
        "srli a5, s0, 7",
        "slli s1, s0, 25",
        "or a5, a5, s1",
        "srli s1, s0, 18",
        "slli a2, s0, 14",
        "or a2, a2, s1",
        "lw s1, a4, 0",
        "xor a2, a2, a5",
        "srli a5, s0, 3",
        "xor a2, a2, a5",
        "addw a3, a3, s1",
        "addw a2, a2, a3",
        "sw a2, a4, 64",
        "addi a1, a1, 4",
        "bne a1, a7, 0x11194",
        "li s9, 0",
        "lw t5, a0, 80",
        "lw t4, a0, 84",
        "lw t3, a0, 88",
        "lw t2, a0, 92",
        "lw t1, a0, 96",
        "lw t0, a0, 100",
        "lw a7, a0, 104",
        "lw a6, a0, 108",
        "li t6, 256",
        "lui a2, 16",
        "addi s3, a2, 344",
        "mv s2, sp",
        "mv s7, t3",
        "mv s4, t2",
        "mv s6, t1",
        "mv a3, t0",
        "mv s10, a7",
        "mv s5, a6",
        "mv s8, t4",
        "mv s0, t5",
        "mv s11, s10",
        "mv s10, a3",
        "mv a3, s6",
        "mv s1, s7",
        "beq s9, t6, 0x112c0",
        "srliw a4, a3, 6",
        "slliw a1, a3, 26",
        "or a1, a1, a4",
        "srliw a4, a3, 11",
        "slliw a2, a3, 21",
        "or a2, a2, a4",
        "xor a1, a1, a2",
        "srliw a2, a3, 25",
        "slliw a4, a3, 7",
        "or a2, a2, a4",
        "xor s6, a1, a2",
        "and a2, s10, a3",
        "not a4, a3",
        "and a4, s11, a4",
        "add a5, s3, s9",
        "lw a5, a5, 0",
        "add a1, s2, s9",
        "lw a1, a1, 0",
        "addw a2, s6, a2",
        "addw a2, a2, s5",
        "addw a2, a2, a4",
        "addw a2, a2, a5",
        "addw a1, a1, a2",
        "srliw a2, s0, 2",
        "slliw a4, s0, 30",
        "or a2, a2, a4",
        "srliw a4, s0, 13",
        "slliw a5, s0, 19",
        "or a4, a4, a5",
        "xor a2, a2, a4",
        "srliw a4, s0, 22",
        "slli a5, s0, 10",
        "or a4, a4, a5",
        "xor a2, a2, a4",
        "xor a4, s8, s1",
        "and a4, a4, s0",
        "and a5, s8, s1",
        "xor a4, a4, a5",
        "addw a2, a2, a4",
        "addw s6, a1, s4",
        "mv s7, s8",
        "mv s8, s0",
        "addw s0, a2, a1",
        "addi s9, s9, 4",
        "mv s4, s1",
        "mv s5, s11",
        "j 0x11226",
        "addw a1, s0, t5",
        "sw a1, a0, 80",
        "addw a1, s8, t4",
        "sw a1, a0, 84",
        "addw a1, s1, t3",
        "sw a1, a0, 88",
        "addw a1, s4, t2",
        "sw a1, a0, 92",
        "addw a1, a3, t1",
        "sw a1, a0, 96",
        "addw a1, s10, t0",
        "sw a1, a0, 100",
        "addw a1, s11, a7",
        "sw a1, a0, 104",
        "addw a1, s5, a6",
        "sw a1, a0, 108",
        "ldsp s0, 344",
        "ldsp s1, 336",
        "ldsp s2, 328",
        "ldsp s3, 320",
        "ldsp s4, 312",
        "ldsp s5, 304",
        "ldsp s6, 296",
        "ldsp s7, 288",
        "ldsp s8, 280",
        "ldsp s9, 272",
        "ldsp s10, 264",
        "ldsp s11, 256",
        "addi16sp 352",
        "ret",
        "lui a1, 18",
        "ld a1, a1, 1632",
        "lui a2, 18",
        "ld a2, a2, 1640",
        "sd a1, a0, 80",
        "lui a1, 18",
        "ld a1, a1, 1648",
        "sd a2, a0, 88",
        "lui a2, 18",
        "ld a2, a2, 1656",
        "sd a1, a0, 96",
        "li a1, 0",
        "sw a1, a0, 64",
        "sd a1, a0, 72",
        "sd a2, a0, 104",
        "ret",
        "addi16sp -48",
        "sdsp ra, 40",
        "sdsp s0, 32",
        "sdsp s1, 24",
        "sdsp s2, 16",
        "sdsp s3, 8",
        "sdsp s4, 0",
        "mv s3, a2",
        "mv s2, a1",
        "mv s1, a0",
        "li s0, 0",
        "li s4, 64",
        "slli a0, s0, 32",
        "srli a0, a0, 32",
        "bgeu a0, s3, 0x1138a",
        "add a0, a0, s2",
        "lwu a1, s1, 64",
        "lb a0, a0, 0",
        "add a1, a1, s1",
        "sb a0, a1, 0",
        "lw a0, s1, 64",
        "addiw a0, a0, 1",
        "sw a0, s1, 64",
        "bne a0, s4, 0x11386",
        "mv a0, s1",
        "mv a1, s1",
        "jal 0x11136",
        "ld a0, s1, 72",
        "addi a0, a0, 512",
        "sd a0, s1, 72",
        "sw zero, s1, 64",
        "addiw s0, s0, 1",
        "j 0x1134e",
        "ldsp ra, 40",
        "ldsp s0, 32",
        "ldsp s1, 24",
        "ldsp s2, 16",
        "ldsp s3, 8",
        "ldsp s4, 0",
        "addi16sp 48",
        "ret",
        "addi sp, sp, -32",
        "sdsp ra, 24",
        "sdsp s0, 16",
        "sdsp s1, 8",
        "mv s0, a0",
        "lwu a0, a0, 64",
        "mv s1, a1",
        "sext.w a1, a0",
        "add a2, s0, a0",
        "li a3, 128",
        "li a4, 56",
        "sb a3, a2, 0",
        "bgeu a1, a4, 0x113de",
        "addi a1, s0, 1",
        "li a2, 55",
        "beq a0, a2, 0x11406",
        "add a3, a1, a0",
        "addi a0, a0, 1",
        "sb zero, a3, 0",
        "bne a0, a2, 0x113ce",

        "j 0x11406",
        "li a1, 63",
        "addiw a0, a0, 1",
        "bltu a1, a0, 0x113f2",
        "add a2, s0, a0",
        "sb zero, a2, 0",
        "j 0x113e2",
        "mv a0, s0",
        "mv a1, s0",
        "jal 0x11136",
        "li a2, 56",
        "mv a0, s0",
        "li a1, 0",
        "jal 0x11120",
        "lw a0, s0, 64",
        "ld a1, s0, 72",
        "slli a0, a0, 35",
        "srli a0, a0, 32",
        "add a0, a0, a1",
        "sd a0, s0, 72",
        "sb a0, s0, 63",
        "srli a1, a0, 8",
        "sb a1, s0, 62",
        "srli a1, a0, 16",
        "sb a1, s0, 61",
        "srli a1, a0, 24",
        "sb a1, s0, 60",
        "srli a1, a0, 32",
        "sb a1, s0, 59",
        "srli a1, a0, 40",
        "sb a1, s0, 58",
        "srli a1, a0, 48",
        "sb a1, s0, 57",
        "srli a0, a0, 56",
        "sb a0, s0, 56",
        "mv a0, s0",
        "mv a1, s0",
        "jal 0x11136",
        "li a0, 0",
        "addi a1, s1, 16",
        "li a2, 4",
        "li a3, 24",
        "beq a0, a2, 0x114c4",
        "lw a4, s0, 80",
        "slliw a5, a0, 3",
        "subw a5, a3, a5",
        "srlw a4, a4, a5",
        "add s1, a1, a0",
        "sb a4, s1, -16",
        "lw a4, s0, 84",
        "srlw a4, a4, a5",
        "sb a4, s1, -12",
        "lw a4, s0, 88",
        "srlw a4, a4, a5",
        "sb a4, s1, -8",
        "lw a4, s0, 92",
        "srlw a4, a4, a5",
        "sb a4, s1, -4",
        "lw a4, s0, 96",
        "srlw a4, a4, a5",
        "sb a4, s1, 0",
        "lw a4, s0, 100",
        "srlw a4, a4, a5",
        "sb a4, s1, 4",
        "lw a4, s0, 104",
        "srlw a4, a4, a5",
        "sb a4, s1, 8",
        "lw a4, s0, 108",
        "srlw a4, a4, a5",
        "sb a4, s1, 12",
        "addi a0, a0, 1",
        "bne a0, a2, 0x11462",
        "ldsp ra, 24",
        "ldsp s0, 16",
        "ldsp s1, 8",
        "addi16sp 32",
        "ret",
        "addi16sp -128",
        "sdsp ra, 120",
        "swsp zero, 72",
        "sdsp zero, 80",
        "lui a0, 18",
        "ld a0, a0, 1664",
        "lui a1, 18",
        "ld a1, a1, 1672",
        "lui a2, 18",
        "ld a2, a2, 1680",
        "lui a3, 18",
        "ld a3, a3, 1688",
        "sdsp a0, 88",
        "sdsp a1, 96",
        "sdsp a2, 104",
        "sdsp a3, 112",
        "addi4spn a0, 8",
        "lui a1, 1",
        "li a2, 1024",
        "jal 0x11334",
        "addi4spn a0, 8",
        "lui a1, 2",
        "jal 0x1139a",
        "ldsp ra, 120",
        "addi16sp 128",
        "ret"
    ];

    for (test, decoded) in test.iter().zip(decoded) {
        if *test != decoded {
            eprintln!("objdump: '{test}' != our: '{decoded}'");
            panic!("instructions don't match");
        }
    }

    Ok(())
}
