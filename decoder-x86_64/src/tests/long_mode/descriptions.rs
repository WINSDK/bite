use std::fmt::Write;

use yaxpeax_arch::{AddressBase, LengthedInstruction};
use crate::long_mode::Opcode;
use crate::long_mode::RegSpec;
use crate::long_mode::InstDecoder;
use crate::long_mode::Instruction;
use crate::long_mode::InnerDescription;
use yaxpeax_arch::annotation::{AnnotatingDecoder, FieldDescription};

fn test_annotations(data: &[u8], expected: &'static str, checks: &[AnnotationCheck]) {
    test_annotations_under(&InstDecoder::default(), data, expected, checks);
}

// pair up field descriptions and the check that matched them. we'll use this for
// reporting errors if checks don't match up.
#[derive(PartialEq, Eq, Copy, Clone)]
enum CheckResult {
    Matched,
    Failed,
    Ignored,
}

impl CheckResult {
    fn consumed_check(&self) -> bool {
        *self != CheckResult::Ignored
    }
}

struct MatchResult {
    check: AnnotationCheck,
    result: CheckResult,
}

#[derive(Clone)]
enum AnnotationCheck {
    // does not match any description; intended to assert that there should be no extra annotations
    // after the last check.
    NoExtra,
    Exact {
        // check the reported annotation matches this description
        desc: InnerDescription,
        start_bit: u32,
        end_bit: u32,
    },
    Approximate {
        check: fn(&InnerDescription) -> bool,
        start_bit: u32,
        end_bit: u32,
    }
}

impl std::fmt::Display for AnnotationCheck {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AnnotationCheck::NoExtra => {
                write!(f, "no further field descriptions expected")
            }
            AnnotationCheck::Exact { desc, start_bit, end_bit } => {
                write!(f, "bit {}:{}; {}", start_bit, end_bit, desc)
            }
            AnnotationCheck::Approximate {
                start_bit, end_bit, ..
            } => {
                write!(f, "bit {}:{}; (fn-based match)", start_bit, end_bit)
            }
        }
    }
}

impl AnnotationCheck {
    fn exact(start: u32, end: u32, desc: InnerDescription) -> AnnotationCheck {
        AnnotationCheck::Exact {
            desc,
            start_bit: start,
            end_bit: end,
        }
    }

    fn approximate(start: u32, end: u32, check: fn(&InnerDescription) -> bool) -> AnnotationCheck {
        AnnotationCheck::Approximate {
            check,
            start_bit: start,
            end_bit: end,
        }
    }

    fn no_extra() -> AnnotationCheck {
        AnnotationCheck::NoExtra
    }

    fn matches(&self, actual_start: u32, actual_end: u32, actual_desc: InnerDescription) -> CheckResult {
        match self {
            AnnotationCheck::NoExtra => {
                CheckResult::Failed
            },
            AnnotationCheck::Exact { start_bit, end_bit, desc } => {
                let bits_match = *start_bit == actual_start && *end_bit == actual_end;
                let desc_match = desc == &actual_desc;
                let fail_anyway = match (desc, &actual_desc) {
                    // expect that there's only one `Number` field with a given name, so if the
                    // bits are wrong or the value is wrong, that's a guaranteed fail.
                    (InnerDescription::Number(expected_name, _), InnerDescription::Number(actual_name, _)) => {
                        if expected_name == actual_name {
                            true
                        } else {
                            false
                        }
                    }
                    // expect that there's only one opcode field. there won't be a second one that
                    // we might match on later.
                    (InnerDescription::Opcode(_), InnerDescription::Opcode(_)) => {
                        true
                    }
                    (_expected, _actual) => false
                };

                if (!bits_match && !desc_match) && !fail_anyway {
                   return CheckResult::Ignored;
                }

                if bits_match && desc_match {
                    CheckResult::Matched
                } else {
                    CheckResult::Failed
                }
            },
            AnnotationCheck::Approximate { start_bit, end_bit, check } => {
                let bits_match = *start_bit == actual_start && *end_bit == actual_end;
                let desc_match = check(&actual_desc);

                if !bits_match && !desc_match {
                   return CheckResult::Ignored;
                }

                if bits_match && desc_match {
                    CheckResult::Matched
                } else {
                    CheckResult::Failed
                }
            }
        }
    }
}

impl std::fmt::Display for CheckResult {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            CheckResult::Matched => "\x1b[32mmatched\x1b[0m   ",
            CheckResult::Failed  => "\x1b[31mfailed\x1b[0m    ",
            CheckResult::Ignored => "\x1b[33mignored\x1b[0m",
        };
        f.write_str(s)
    }
}

fn test_annotations_under(decoder: &InstDecoder, data: &[u8], expected: &'static str, checks: &[AnnotationCheck]) {
    let mut hex = String::new();
    for b in data {
        write!(hex, "{:02x}", b).unwrap();
    }
    let mut reader = yaxpeax_arch::U8Reader::new(data);
    let mut sink = yaxpeax_arch::annotation::VecSink::new();
    let mut inst = Instruction::default();
    match decoder.decode_with_annotation(&mut inst, &mut reader, &mut sink) {
        Ok(()) => {
            let text = format!("{}", inst);
            assert!(
                text == expected,
                "display error for {}:\n  decoded: {:?} under decoder {}\n displayed: {}\n expected: {}\n",
                hex,
                inst,
                decoder,
                text,
                expected
            );

            let mut matches: Vec<((u32, u32, InnerDescription), Option<MatchResult>)> = Vec::new();
            let mut extra_checks: Vec<AnnotationCheck> = Vec::new();

            sink.records.sort_by_key(|x| x.2.id());
            let mut rec_iter = sink.records.iter();
            let mut check_iter = checks.iter();

            let mut check = check_iter.next();

            let mut failed = false;

            while let Some((bit_start, bit_end, desc)) = rec_iter.next().cloned() {
                if let Some(curr_check) = check {
                    if let AnnotationCheck::NoExtra = curr_check {
                        failed = true;
                    }

                    let check_result = curr_check.matches(bit_start, bit_end, desc.desc().clone());
                    if check_result == CheckResult::Failed {
                        failed = true;
                    }

                    matches.push(((bit_start, bit_end, desc.desc().clone()), Some(MatchResult {
                        check: curr_check.clone(),
                        result: check_result
                    })));

                    if check_result.consumed_check() {
                        check = check_iter.next();
                    }
                } else {
                    // no more checks, so we'll have passed the test at least. continue scooping up
                    // field descriptions into `matches` with no checks.
                    matches.push(((bit_start, bit_end, desc.desc().clone()), None));
                }
            }

            while let Some(missed_check) = check {
                check = check_iter.next();
                if let AnnotationCheck::NoExtra = missed_check {
                    // "no extra" will be "missed" in that nothing matches it above. in the success
                    // case, it's a leftover check, and should be the only one remaining if the
                    // test is written correctly. so skip it here, and see if we've exhausted the
                    // list of checks..
                    continue;
                }
                extra_checks.push(missed_check.clone());
            }

            if extra_checks.len() > 0 {
                failed = true;
            }

            if failed {
                eprintln!("[!] annotation check for {}, `{}`, failed:", hex, inst);
                for ((bit_start, bit_end, desc), check) in matches {
                    let mut desc = format!("bit {}:{}; {}", bit_start, bit_end, desc);
                    while desc.len() < 60 {
                        desc.push(' ');
                    }
                    desc.push(' ');
                    let comment = match check {
                        None => {
                            "\x1b[34mno check\x1b[0m".to_owned()
                        }
                        Some(MatchResult {
                            result,
                            check
                        }) => {
                            if result == CheckResult::Ignored {
                                result.to_string()
                            } else {
                                format!("{}{}", result, check)
                            }
                        }
                    };
                    eprintln!(" - {}{}", desc, comment);
                }
                for check in extra_checks {
                    eprintln!(" !  \x1b[31mextra check\x1b[0m: {}", check);
                }
            }
            assert!(!failed);

            // while we're at it, test that the instruction is as long, and no longer, than its
            // input
            assert_eq!((0u64.wrapping_offset(inst.len()).to_linear()) as usize, data.len(), "instruction length is incorrect, wanted instruction {}", expected);
        },
        Err(e) => {
            assert!(false, "decode error ({}) for {} under decoder {}:\n  expected: {}\n", e, hex, decoder, expected);
        }
    }
}

#[test]
fn test_modrm_decode() {
    test_annotations(&[0xff, 0xc0], "inc eax", &[
        AnnotationCheck::exact(11, 13, InnerDescription::Opcode(Opcode::INC)),
        AnnotationCheck::approximate(0, 7, |desc| { desc.to_string().contains("ModRM_0xff_Ev") }),
        AnnotationCheck::approximate(14, 15, |desc| {
            desc.to_string().contains("mmm") &&
            desc.to_string().contains("register number") &&
            desc.to_string().contains("mod bits: 11")
        }),
        AnnotationCheck::exact(8, 10, InnerDescription::RegisterNumber("mmm", 0, RegSpec::eax())),
        AnnotationCheck::no_extra(),
    ]);
    test_annotations(&[0xc1, 0xe0, 0x03], "shl eax, 0x3", &[
        AnnotationCheck::exact(11, 13, InnerDescription::Opcode(Opcode::SHL)),
        AnnotationCheck::exact(16, 23, InnerDescription::Number("imm", 3)),
        AnnotationCheck::approximate(0, 7, |desc| { desc.to_string().contains("ModRM_0xc1_Ev_Ib") }),
        AnnotationCheck::approximate(14, 15, |desc| {
            desc.to_string().contains("mmm") &&
            desc.to_string().contains("register number") &&
            desc.to_string().contains("mod bits: 11")
        }),
        AnnotationCheck::exact(8, 10, InnerDescription::RegisterNumber("mmm", 0, RegSpec::eax())),
        AnnotationCheck::no_extra(),
    ]);
    test_annotations(&[0x33, 0x08], "xor ecx, dword [rax]", &[
        AnnotationCheck::exact(0, 7, InnerDescription::Opcode(Opcode::XOR)),
        AnnotationCheck::approximate(0, 7, |desc| { desc.to_string() == "operand code `Gv_Ev`" }),
        AnnotationCheck::approximate(7, 7, |desc| { desc.to_string().contains("operands begin") }),
        AnnotationCheck::approximate(14, 15, |desc| {
            desc.to_string().contains("memory operand is [reg]") &&
            desc.to_string().contains("mod bits: 00")
        }),
        AnnotationCheck::exact(11, 13, InnerDescription::RegisterNumber("rrr", 1, RegSpec::ecx())),
        AnnotationCheck::exact(8, 10, InnerDescription::RegisterNumber("mmm", 0, RegSpec::rax())),
        AnnotationCheck::no_extra(),
    ]);
    test_annotations(&[0x66, 0x0f, 0x38, 0x00, 0xc1], "pshufb xmm0, xmm1", &[
        AnnotationCheck::exact(0, 7, InnerDescription::Misc("operand size override (to 16 bits)")),
        AnnotationCheck::approximate(38, 39, |desc| {
            desc.to_string().contains("mmm") &&
            desc.to_string().contains("register number") &&
            desc.to_string().contains("mod bits: 11")
        }),
        AnnotationCheck::exact(32, 34, InnerDescription::RegisterNumber("mmm", 1, RegSpec::ecx())),
        AnnotationCheck::exact(35, 37, InnerDescription::RegisterNumber("rrr", 0, RegSpec::eax())),
        AnnotationCheck::no_extra(),
    ]);

    // modrm + rex.w
    test_annotations(&[0x48, 0x33, 0x08], "xor rcx, qword [rax]", &[]);
    test_annotations(&[0x48, 0x33, 0x20], "xor rsp, qword [rax]", &[]);
    test_annotations(&[0x48, 0x33, 0x05, 0x78, 0x56, 0x34, 0x12], "xor rax, qword [rip + 0x12345678]", &[]);

    // specifically sib with base == 0b101
    // mod bits 00
    test_annotations(&[0x42, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50], "xor esi, dword [r12 * 1 + 0x50403020]", &[]);
    test_annotations(&[0x43, 0x33, 0x34, 0x25, 0x20, 0x30, 0x40, 0x50], "xor esi, dword [r12 * 1 + 0x50403020]", &[]);
    // mod bits 01
    test_annotations(&[0x42, 0x33, 0x74, 0x25, 0x20], "xor esi, dword [rbp + r12 * 1 + 0x20]", &[]);
    test_annotations(&[0x43, 0x33, 0x74, 0x25, 0x20], "xor esi, dword [r13 + r12 * 1 + 0x20]", &[]);
    // mod bits 10
    test_annotations(&[0x42, 0x33, 0xb4, 0x25, 0x20, 0x30, 0x40, 0x50], "xor esi, dword [rbp + r12 * 1 + 0x50403020]", &[]);
    test_annotations(&[0x43, 0x33, 0xb4, 0x25, 0x20, 0x30, 0x40, 0x50], "xor esi, dword [r13 + r12 * 1 + 0x50403020]", &[]);
}
