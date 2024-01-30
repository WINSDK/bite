#![cfg(test)]

use decoder::{Decodable, ToTokens};
use object::{Object, ObjectSection, SectionKind};

macro_rules! decode_instructions {
    ($code:literal) => {{
        static CRC: crc::Crc<u32> = crc::Crc::<u32>::new(&crc::CRC_32_ISCSI);

        let code = format!(
            "
            #![deny(warnings)]
            #![no_std]
            #![no_main]

            core::arch::global_asm!(\"{}\");

            #[panic_handler]
            fn panic(_: &core::panic::PanicInfo) -> ! {{
                loop {{}}
            }}
        ",
            $code
        );

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
        let mut reader = decoder::Reader::new(&binary[..]);
        let mut line = decoder::TokenStream::new();
        let decoder = crate::Decoder { is_64: true };
        let symbols = symbols::Index::default();

        loop {
            match decoder.decode(&mut reader) {
                Ok(inst) => {
                    inst.tokenize(&mut line, &symbols);
                    decoded.push(line.to_string());
                    line = decoder::TokenStream::new();
                }
                Err(err) => {
                    if err.kind == decoder::ErrorKind::ExhaustedInput {
                        break;
                    }

                    decoded.push(format!("{err:?}"));
                }
            }
        }

        decoded
    }};
}

#[test]
fn deref() -> Result<(), Box<dyn std::error::Error>> {
    let decoded = decode_instructions!(
        "
        .global _start
        _start:
            lui	a0, 4096
            li	a1, 12
            sw	a1, 0(a0)
            li	a0, 0
            ret
   "
    );

    let test = [
        "lui a0, 4096",
        "c.li a1, 12",
        "c.sw a1, a0, 0",
        "c.li a0, 0",
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
    let decoded = decode_instructions!(
        "
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
   "
    );

    let test = [
        "c.li a3, 0",
        "beq a2, a3, 18",
        "add a4, a0, a3",
        "sb a1, a4, 0",
        "c.addi a3, 1",
        "bne a2, a3, -10",
        "ret",
        "c.addi16sp -352",
        "c.sdsp s0, 344",
        "c.sdsp s1, 336",
        "c.sdsp s2, 328",
        "c.sdsp s3, 320",
        "c.sdsp s4, 312",
        "c.sdsp s5, 304",
        "c.sdsp s6, 296",
        "c.sdsp s7, 288",
        "c.sdsp s8, 280",
        "c.sdsp s9, 272",
        "c.sdsp s10, 264",
        "c.sdsp s11, 256",
        "c.li a2, 0",
        "li a3, 64",
        "c.mv a6, sp",
        "beq a2, a3, 48",
        "add a5, a1, a2",
        "lb s1, a5, 0",
        "lbu s0, a5, 1",
        "c.slli s1, 24",
        "lbu a4, a5, 2",
        "c.slli s0, 16",
        "lbu a5, a5, 3",
        "c.or s1, s0",
        "c.slli a4, 8",
        "c.or a4, s1",
        "c.or a4, a5",
        "add a5, a6, a2",
        "c.sw a4, a5, 0",
        "c.addi a2, 4",
        "bne a2, a3, -40",
        "c.li a1, 0",
        "li a7, 192",
        "c.mv a6, sp",
        "beq a1, a7, 88",
        "add a4, a6, a1",
        "lwu a5, a4, 56",
        "srli s1, a5, 17",
        "slliw s0, a5, 15",
        "c.or s1, s0",
        "srli s0, a5, 19",
        "slliw a3, a5, 13",
        "c.or a3, s0",
        "c.xor a3, s1",
        "c.lw s1, a4, 36",
        "lwu s0, a4, 4",
        "c.srli a5, 10",
        "c.xor a3, a5",
        "c.addw a3, s1",
        "srli a5, s0, 7",
        "slli s1, s0, 25",
        "c.or a5, s1",
        "srli s1, s0, 18",
        "slli a2, s0, 14",
        "c.or a2, s1",
        "c.lw s1, a4, 0",
        "c.xor a2, a5",
        "srli a5, s0, 3",
        "c.xor a2, a5",
        "c.addw a3, s1",
        "c.addw a2, a3",
        "c.sw a2, a4, 64",
        "c.addi a1, 4",
        "bne a1, a7, -80",
        "c.li s9, 0",
        "lw t5, a0, 80",
        "lw t4, a0, 84",
        "lw t3, a0, 88",
        "lw t2, a0, 92",
        "lw t1, a0, 96",
        "lw t0, a0, 100",
        "lw a7, a0, 104",
        "lw a6, a0, 108",
        "li t6, 256",
        "c.lui a2, 16",
        "addi s3, a2, 344",
        "c.mv s2, sp",
        "c.mv s7, t3",
        "c.mv s4, t2",
        "c.mv s6, t1",
        "c.mv a3, t0",
        "c.mv s10, a7",
        "c.mv s5, a6",
        "c.mv s8, t4",
        "c.mv s0, t5",
        "c.mv s11, s10",
        "c.mv s10, a3",
        "c.mv a3, s6",
        "c.mv s1, s7",
        "beq s9, t6, 146",
        "srliw a4, a3, 6",
        "slliw a1, a3, 26",
        "c.or a1, a4",
        "srliw a4, a3, 11",
        "slliw a2, a3, 21",
        "c.or a2, a4",
        "c.xor a1, a2",
        "srliw a2, a3, 25",
        "slliw a4, a3, 7",
        "c.or a2, a4",
        "xor s6, a1, a2",
        "and a2, s10, a3",
        "not a4, a3",
        "and a4, s11, a4",
        "add a5, s3, s9",
        "c.lw a5, a5, 0",
        "add a1, s2, s9",
        "c.lw a1, a1, 0",
        "addw a2, s6, a2",
        "addw a2, s5",
        "c.addw a2, a4",
        "c.addw a2, a5",
        "c.addw a1, a2",
        "srliw a2, s0, 2",
        "slliw a4, s0, 30",
        "c.or a2, a4",
        "srliw a4, s0, 13",
        "slliw a5, s0, 19",
        "c.or a4, a5",
        "c.xor a2, a4",
        "srliw a4, s0, 22",
        "slli a5, s0, 10",
        "c.or a4, a5",
        "c.xor a2, a4",
        "xor a4, s8, s1",
        "c.and a4, s0",
        "and a5, s8, s1",
        "c.xor a4, a5",
        "c.addw a2, a4",
        "addw s6, a1, s4",
        "c.mv s7, s8",
        "c.mv s8, s0",
        "addw s0, a2, a1",
        "c.addi s9, 4",
        "c.mv s4, s1",
        "c.mv s5, s11",
        "c.j -152",
        "addw a1, s0, t5",
        "c.sw a1, a0, 80",
        "addw a1, s8, t4",
        "c.sw a1, a0, 84",
        "addw a1, s1, t3",
        "c.sw a1, a0, 88",
        "addw a1, s4, t2",
        "c.sw a1, a0, 92",
        "addw a1, a3, t1",
        "c.sw a1, a0, 96",
        "addw a1, s10, t0",
        "c.sw a1, a0, 100",
        "addw a1, s11, a7",
        "c.sw a1, a0, 104",
        "addw a1, s5, a6",
        "c.sw a1, a0, 108",
        "c.ldsp s0, 344",
        "c.ldsp s1, 336",
        "c.ldsp s2, 328",
        "c.ldsp s3, 320",
        "c.ldsp s4, 312",
        "c.ldsp s5, 304",
        "c.ldsp s6, 296",
        "c.ldsp s7, 288",
        "c.ldsp s8, 280",
        "c.ldsp s9, 272",
        "c.ldsp s10, 264",
        "c.ldsp s11, 256",
        "c.addi16sp 352",
        "ret",
        "c.lui a1, 18",
        "ld a1, a1, 1632",
        "c.lui a2, 18",
        "ld a2, a2, 1640",
        "c.sd a1, a0, 80",
        "c.lui a1, 18",
        "ld a1, a1, 1648",
        "c.sd a2, a0, 88",
        "c.lui a2, 18",
        "ld a2, a2, 1656",
        "c.sd a1, a0, 96",
        "c.li a1, 0",
        "c.sw a1, a0, 64",
        "c.sd a1, a0, 72",
        "c.sd a2, a0, 104",
        "ret",
        "c.addi16sp -48",
        "c.sdsp ra, 40",
        "c.sdsp s0, 32",
        "c.sdsp s1, 24",
        "c.sdsp s2, 16",
        "c.sdsp s3, 8",
        "c.sdsp s4, 0",
        "c.mv s3, a2",
        "c.mv s2, a1",
        "c.mv s1, a0",
        "c.li s0, 0",
        "li s4, 64",
        "slli a0, s0, 32",
        "c.srli a0, 32",
        "bgeu a0, s3, 54",
        "c.add a0, s2",
        "lwu a1, s1, 64",
        "lb a0, a0, 0",
        "c.add a1, s1",
        "sb a0, a1, 0",
        "c.lw a0, s1, 64",
        "c.addiw a0, 1",
        "c.sw a0, s1, 64",
        "bne a0, s4, 24",
        "c.mv a0, s1",
        "c.mv a1, s1",
        "jal -576",
        "c.ld a0, s1, 72",
        "addi a0, 512",
        "c.sd a0, s1, 72",
        "sw zero, s1, 64",
        "c.addiw s0, 1",
        "c.j -58",
        "c.ldsp ra, 40",
        "c.ldsp s0, 32",
        "c.ldsp s1, 24",
        "c.ldsp s2, 16",
        "c.ldsp s3, 8",
        "c.ldsp s4, 0",
        "c.addi16sp 48",
        "ret",
        "c.addi sp, -32",
        "c.sdsp ra, 24",
        "c.sdsp s0, 16",
        "c.sdsp s1, 8",
        "c.mv s0, a0",
        "lwu a0, a0, 64",
        "c.mv s1, a1",
        "sext.w a1, a0",
        "add a2, s0, a0",
        "li a3, 128",
        "li a4, 56",
        "sb a3, a2, 0",
        "bgeu a1, a4, 32",
        "addi a1, s0, 1",
        "li a2, 55",
        "beq a0, a2, 60",
        "add a3, a1, a0",
        "c.addi a0, 1",
        "sb zero, a3, 0",
        "bne a0, a2, -10",
        "c.j 42",
        "li a1, 63",
        "c.addiw a0, 1",
        "bltu a1, a0, 14",
        "add a2, s0, a0",
        "sb zero, a2, 0",
        "c.j -14",
        "c.mv a0, s0",
        "c.mv a1, s0",
        "jal -704",
        "li a2, 56",
        "c.mv a0, s0",
        "c.li a1, 0",
        "jal -738",
        "c.lw a0, s0, 64",
        "c.ld a1, s0, 72",
        "c.slli a0, 35",
        "c.srli a0, 32",
        "c.add a0, a1",
        "c.sd a0, s0, 72",
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
        "c.srli a0, 56",
        "sb a0, s0, 56",
        "c.mv a0, s0",
        "c.mv a1, s0",
        "jal -794",
        "c.li a0, 0",
        "addi a1, s1, 16",
        "c.li a2, 4",
        "c.li a3, 24",
        "beq a0, a2, 102",
        "c.lw a4, s0, 80",
        "slliw a5, a0, 3",
        "subw a5, a3, a5",
        "srlw a4, a4, a5",
        "add s1, a1, a0",
        "sb a4, s1, -16",
        "c.lw a4, s0, 84",
        "srlw a4, a4, a5",
        "sb a4, s1, -12",
        "c.lw a4, s0, 88",
        "srlw a4, a4, a5",
        "sb a4, s1, -8",
        "c.lw a4, s0, 92",
        "srlw a4, a4, a5",
        "sb a4, s1, -4",
        "c.lw a4, s0, 96",
        "srlw a4, a4, a5",
        "sb a4, s1, 0",
        "c.lw a4, s0, 100",
        "srlw a4, a4, a5",
        "sb a4, s1, 4",
        "c.lw a4, s0, 104",
        "srlw a4, a4, a5",
        "sb a4, s1, 8",
        "c.lw a4, s0, 108",
        "srlw a4, a4, a5",
        "sb a4, s1, 12",
        "c.addi a0, 1",
        "bne a0, a2, -94",
        "c.ldsp ra, 24",
        "c.ldsp s0, 16",
        "c.ldsp s1, 8",
        "c.addi16sp 32",
        "ret",
        "c.addi16sp -128",
        "c.sdsp ra, 120",
        "c.swsp zero, 72",
        "c.sdsp zero, 80",
        "c.lui a0, 18",
        "ld a0, a0, 1664",
        "c.lui a1, 18",
        "ld a1, a1, 1672",
        "c.lui a2, 18",
        "ld a2, a2, 1680",
        "c.lui a3, 18",
        "ld a3, a3, 1688",
        "c.sdsp a0, 88",
        "c.sdsp a1, 96",
        "c.sdsp a2, 104",
        "c.sdsp a3, 112",
        "c.addi4spn a0, 8",
        "c.lui a1, 1",
        "li a2, 1024",
        "jal -458",
        "c.addi4spn a0, 8",
        "c.lui a1, 2",
        "jal -364",
        "c.ldsp ra, 120",
        "c.addi16sp 128",
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
