use crate::safer_unchecked::GetSaferUnchecked as _;
use std::fmt;

use crate::protected_mode::{
    Decoder, Instruction, MergeMode, Opcode, Operand, OperandSpec, PrefixVex, RegSpec, Segment,
};
use crate::{Number, MEM_SIZE_STRINGS};

use decoder::ToTokens;
use tokenizing::{ColorScheme, Colors};

impl fmt::Display for Decoder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self == &Decoder::default() {
            return write!(f, "<all features>");
        } else if self == &Decoder::minimal() {
            return write!(f, "<no features>");
        }
        if self.sse3() {
            write!(f, "sse3 ")?
        }
        if self.ssse3() {
            write!(f, "ssse3 ")?
        }
        if self.monitor() {
            write!(f, "monitor ")?
        }
        if self.vmx() {
            write!(f, "vmx ")?
        }
        if self.fma3() {
            write!(f, "fma3 ")?
        }
        if self.cmpxchg16b() {
            write!(f, "cmpxchg16b ")?
        }
        if self.sse4_1() {
            write!(f, "sse4_1 ")?
        }
        if self.sse4_2() {
            write!(f, "sse4_2 ")?
        }
        if self.movbe() {
            write!(f, "movbe ")?
        }
        if self.popcnt() {
            write!(f, "popcnt ")?
        }
        if self.aesni() {
            write!(f, "aesni ")?
        }
        if self.xsave() {
            write!(f, "xsave ")?
        }
        if self.rdrand() {
            write!(f, "rdrand ")?
        }
        if self.sgx() {
            write!(f, "sgx ")?
        }
        if self.bmi1() {
            write!(f, "bmi1 ")?
        }
        if self.avx2() {
            write!(f, "avx2 ")?
        }
        if self.bmi2() {
            write!(f, "bmi2 ")?
        }
        if self.invpcid() {
            write!(f, "invpcid ")?
        }
        if self.mpx() {
            write!(f, "mpx ")?
        }
        if self.avx512_f() {
            write!(f, "avx512_f ")?
        }
        if self.avx512_dq() {
            write!(f, "avx512_dq ")?
        }
        if self.rdseed() {
            write!(f, "rdseed ")?
        }
        if self.adx() {
            write!(f, "adx ")?
        }
        if self.avx512_fma() {
            write!(f, "avx512_fma ")?
        }
        if self.pcommit() {
            write!(f, "pcommit ")?
        }
        if self.clflushopt() {
            write!(f, "clflushopt ")?
        }
        if self.clwb() {
            write!(f, "clwb ")?
        }
        if self.avx512_pf() {
            write!(f, "avx512_pf ")?
        }
        if self.avx512_er() {
            write!(f, "avx512_er ")?
        }
        if self.avx512_cd() {
            write!(f, "avx512_cd ")?
        }
        if self.sha() {
            write!(f, "sha ")?
        }
        if self.avx512_bw() {
            write!(f, "avx512_bw ")?
        }
        if self.avx512_vl() {
            write!(f, "avx512_vl ")?
        }
        if self.prefetchwt1() {
            write!(f, "prefetchwt1 ")?
        }
        if self.avx512_vbmi() {
            write!(f, "avx512_vbmi ")?
        }
        if self.avx512_vbmi2() {
            write!(f, "avx512_vbmi2 ")?
        }
        if self.gfni() {
            write!(f, "gfni ")?
        }
        if self.vaes() {
            write!(f, "vaes ")?
        }
        if self.pclmulqdq() {
            write!(f, "pclmulqdq ")?
        }
        if self.avx_vnni() {
            write!(f, "avx_vnni ")?
        }
        if self.avx512_bitalg() {
            write!(f, "avx512_bitalg ")?
        }
        if self.avx512_vpopcntdq() {
            write!(f, "avx512_vpopcntdq ")?
        }
        if self.avx512_4vnniw() {
            write!(f, "avx512_4vnniw ")?
        }
        if self.avx512_4fmaps() {
            write!(f, "avx512_4fmaps ")?
        }
        if self.cx8() {
            write!(f, "cx8 ")?
        }
        if self.syscall() {
            write!(f, "syscall ")?
        }
        if self.rdtscp() {
            write!(f, "rdtscp ")?
        }
        if self.abm() {
            write!(f, "abm ")?
        }
        if self.sse4a() {
            write!(f, "sse4a ")?
        }
        if self._3dnowprefetch() {
            write!(f, "_3dnowprefetch ")?
        }
        if self.xop() {
            write!(f, "xop ")?
        }
        if self.skinit() {
            write!(f, "skinit ")?
        }
        if self.tbm() {
            write!(f, "tbm ")?
        }
        if self.intel_quirks() {
            write!(f, "intel_quirks ")?
        }
        if self.amd_quirks() {
            write!(f, "amd_quirks ")?
        }
        if self.avx() {
            write!(f, "avx ")?
        }
        Ok(())
    }
}

impl fmt::Display for PrefixVex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.present() {
            write!(
                f,
                "vex:{}{}{}{}",
                if self.w() { "w" } else { "-" },
                if self.r() { "r" } else { "-" },
                if self.x() { "x" } else { "-" },
                if self.b() { "b" } else { "-" },
            )
        } else {
            write!(f, "vex:none")
        }
    }
}

impl fmt::Display for Segment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Segment::CS => write!(f, "cs"),
            Segment::DS => write!(f, "ds"),
            Segment::ES => write!(f, "es"),
            Segment::FS => write!(f, "fs"),
            Segment::GS => write!(f, "gs"),
            Segment::SS => write!(f, "ss"),
        }
    }
}

// register names are grouped by indices scaled by 16.
// xmm, ymm, zmm all get two indices.
const REG_NAMES: &[&str] = &[
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "ax", "cx", "dx", "bx", "sp", "bp",
    "si", "di", "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh", "cr0", "cr1", "cr2", "cr3", "cr4",
    "cr5", "cr6", "cr7", "dr0", "dr1", "dr2", "dr3", "dr4", "dr5", "dr6", "dr7", "es", "cs", "ss",
    "ds", "fs", "gs", "", "", "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
    "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15", "xmm16", "xmm17",
    "xmm18", "xmm19", "xmm20", "xmm21", "xmm22", "xmm23", "xmm24", "xmm25", "xmm26", "xmm27",
    "xmm28", "xmm29", "xmm30", "xmm31", "ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6",
    "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15", "ymm16", "ymm17",
    "ymm18", "ymm19", "ymm20", "ymm21", "ymm22", "ymm23", "ymm24", "ymm25", "ymm26", "ymm27",
    "ymm28", "ymm29", "ymm30", "ymm31", "zmm0", "zmm1", "zmm2", "zmm3", "zmm4", "zmm5", "zmm6",
    "zmm7", "zmm8", "zmm9", "zmm10", "zmm11", "zmm12", "zmm13", "zmm14", "zmm15", "zmm16", "zmm17",
    "zmm18", "zmm19", "zmm20", "zmm21", "zmm22", "zmm23", "zmm24", "zmm25", "zmm26", "zmm27",
    "zmm28", "zmm29", "zmm30", "zmm31", "st(0)", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)",
    "st(6)", "st(7)", "mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7", "k0", "k1", "k2",
    "k3", "k4", "k5", "k6", "k7", "eip", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "eflags",
    "BUG", "BUG", "BUG", "BUG", "BUG", "BUG", "BUG",
];

pub(crate) fn regspec_label(spec: &RegSpec) -> &'static str {
    unsafe { REG_NAMES.get_kinda_unchecked((spec.num as u16 + ((spec.bank as u16) << 3)) as usize) }
}

impl fmt::Display for RegSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(regspec_label(self))
    }
}

impl ToTokens for Operand {
    fn tokenize(&self, stream: &mut decoder::TokenStream) {
        match *self {
            Operand::ImmediateU8(imm) => {
                let text = decoder::encode_hex(imm as i64);
                stream.push_owned(text, Colors::immediate());
            }
            Operand::ImmediateI8(imm) => {
                let text = decoder::encode_hex(imm as i64);
                stream.push_owned(text, Colors::immediate());
            }
            Operand::ImmediateU16(imm) => {
                let text = decoder::encode_hex(imm as i64);
                stream.push_owned(text, Colors::immediate());
            }
            Operand::ImmediateI16(imm) => {
                let text = decoder::encode_hex(imm as i64);
                stream.push_owned(text, Colors::immediate());
            }
            Operand::ImmediateU32(imm) => {
                let text = decoder::encode_hex(imm as i64);
                stream.push_owned(text, Colors::immediate());
            }
            Operand::ImmediateI32(imm) => {
                let text = decoder::encode_hex(imm as i64);
                stream.push_owned(text, Colors::immediate());
            }
            Operand::AbsoluteFarAddress { segment, address } => {
                stream.push_owned(decoder::encode_hex(segment as i64), Colors::immediate());
                stream.push(":", Colors::expr());
                stream.push_owned(decoder::encode_hex(address as i64), Colors::immediate());
            }
            Operand::Register(ref spec) => {
                stream.push(regspec_label(spec), Colors::register());
            }
            Operand::RegisterMaskMerge(ref spec, ref mask, merge_mode) => {
                stream.push(regspec_label(spec), Colors::register());

                if mask.num != 0 {
                    stream.push("{", Colors::brackets());
                    stream.push(regspec_label(mask), Colors::register());
                    stream.push("}", Colors::brackets());
                }
                if let MergeMode::Zero = merge_mode {
                    stream.push("{", Colors::brackets());
                    stream.push("z", Colors::register());
                    stream.push("}", Colors::brackets());
                }
            }
            Operand::RegisterMaskMergeSae(ref spec, ref mask, merge_mode, sae_mode) => {
                stream.push(regspec_label(spec), Colors::register());

                if mask.num != 0 {
                    stream.push("{", Colors::brackets());
                    stream.push(regspec_label(mask), Colors::register());
                    stream.push("}", Colors::brackets());
                }
                if let MergeMode::Zero = merge_mode {
                    stream.push("{", Colors::brackets());
                    stream.push("z", Colors::register());
                    stream.push("}", Colors::brackets());
                }

                sae_mode.tokenize(stream);
            }
            Operand::RegisterMaskMergeSaeNoround(ref spec, ref mask, merge_mode) => {
                stream.push(regspec_label(spec), Colors::register());

                if mask.num != 0 {
                    stream.push("{", Colors::brackets());
                    stream.push(regspec_label(mask), Colors::register());
                    stream.push("}", Colors::brackets());
                }
                if let MergeMode::Zero = merge_mode {
                    stream.push("{", Colors::brackets());
                    stream.push("z", Colors::register());
                    stream.push("}", Colors::brackets());
                }

                stream.push("{", Colors::brackets());
                stream.push("sae", Colors::register());
                stream.push("}", Colors::brackets());
            }
            Operand::DisplacementU16(imm) => {
                stream.push("[", Colors::brackets());
                stream.push_owned(decoder::encode_hex(imm as i64), Colors::immediate());
                stream.push("]", Colors::brackets());
            }
            Operand::DisplacementU32(imm) => {
                stream.push("[", Colors::brackets());
                stream.push_owned(decoder::encode_hex(imm as i64), Colors::immediate());
                stream.push("]", Colors::brackets());
            }
            Operand::RegDisp(ref spec, disp) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(spec), Colors::register());
                Number(disp).tokenize(stream);
                stream.push("]", Colors::brackets());
            }
            Operand::RegDeref(ref spec) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(spec), Colors::register());
                stream.push("]", Colors::brackets());
            }
            Operand::RegScale(ref spec, scale) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(spec), Colors::register());
                stream.push(" * ", Colors::expr());
                stream.push_owned(scale.to_string(), Colors::immediate());
                stream.push("]", Colors::brackets());
            }
            Operand::RegScaleDisp(ref spec, scale, disp) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(spec), Colors::register());
                stream.push(" * ", Colors::expr());
                stream.push_owned(format!("{scale}"), Colors::immediate());
                Number(disp).tokenize(stream);
                stream.push("]", Colors::brackets());
            }
            Operand::RegIndexBase(ref base, ref index) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(base), Colors::register());
                stream.push(" + ", Colors::expr());
                stream.push(regspec_label(index), Colors::register());
                stream.push("]", Colors::brackets());
            }
            Operand::RegIndexBaseDisp(ref base, ref index, disp) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(base), Colors::register());
                stream.push(" + ", Colors::expr());
                stream.push(regspec_label(index), Colors::register());
                Number(disp).tokenize(stream);
                stream.push("]", Colors::brackets());
            }
            Operand::RegIndexBaseScale(ref base, ref index, scale) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(base), Colors::register());
                stream.push(" + ", Colors::expr());
                stream.push(regspec_label(index), Colors::register());
                stream.push(" * ", Colors::expr());
                stream.push_owned(scale.to_string(), Colors::immediate());
                stream.push("]", Colors::brackets());
            }
            Operand::RegIndexBaseScaleDisp(ref base, ref index, scale, disp) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(base), Colors::register());
                stream.push(" + ", Colors::expr());
                stream.push(regspec_label(index), Colors::register());
                stream.push(" * ", Colors::expr());
                stream.push_owned(scale.to_string(), Colors::immediate());
                Number(disp).tokenize(stream);
                stream.push("]", Colors::brackets());
            }
            Operand::RegDispMasked(ref spec, disp, ref mask_reg) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(spec), Colors::register());
                Number(disp).tokenize(stream);
                stream.push("]", Colors::brackets());

                stream.push("{", Colors::brackets());
                stream.push(regspec_label(mask_reg), Colors::register());
                stream.push("}", Colors::brackets());
            }
            Operand::RegDerefMasked(ref spec, ref mask_reg) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(spec), Colors::register());
                stream.push("]", Colors::brackets());

                stream.push("{", Colors::brackets());
                stream.push(regspec_label(mask_reg), Colors::register());
                stream.push("}", Colors::brackets());
            }
            Operand::RegScaleMasked(ref spec, scale, ref mask_reg) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(spec), Colors::register());
                stream.push(" * ", Colors::expr());
                stream.push_owned(scale.to_string(), Colors::register());
                stream.push("]", Colors::brackets());

                stream.push("{", Colors::brackets());
                stream.push(regspec_label(mask_reg), Colors::register());
                stream.push("}", Colors::brackets());
            }
            Operand::RegScaleDispMasked(ref spec, scale, disp, ref mask_reg) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(spec), Colors::register());
                stream.push(" * ", Colors::expr());
                stream.push_owned(scale.to_string(), Colors::register());
                Number(disp).tokenize(stream);
                stream.push("]", Colors::brackets());

                stream.push("{", Colors::brackets());
                stream.push(regspec_label(mask_reg), Colors::register());
                stream.push("}", Colors::brackets());
            }
            Operand::RegIndexBaseMasked(ref base, ref index, ref mask_reg) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(base), Colors::register());
                stream.push(" + ", Colors::expr());
                stream.push(regspec_label(index), Colors::register());
                stream.push("]", Colors::brackets());

                stream.push("{", Colors::brackets());
                stream.push(regspec_label(mask_reg), Colors::register());
                stream.push("}", Colors::brackets());
            }
            Operand::RegIndexBaseDispMasked(ref base, ref index, disp, ref mask_reg) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(base), Colors::register());
                stream.push(" + ", Colors::expr());
                stream.push(regspec_label(index), Colors::register());
                Number(disp).tokenize(stream);
                stream.push("]", Colors::brackets());

                stream.push("{", Colors::brackets());
                stream.push(regspec_label(mask_reg), Colors::register());
                stream.push("}", Colors::brackets());
            }
            Operand::RegIndexBaseScaleMasked(ref base, ref index, scale, ref mask_reg) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(base), Colors::register());
                stream.push(" + ", Colors::expr());
                stream.push(regspec_label(index), Colors::register());
                stream.push(" * ", Colors::expr());
                stream.push_owned(scale.to_string(), Colors::immediate());
                stream.push("]", Colors::brackets());

                stream.push("{", Colors::brackets());
                stream.push(regspec_label(mask_reg), Colors::register());
                stream.push("}", Colors::brackets());
            }
            Operand::RegIndexBaseScaleDispMasked(
                ref base,
                ref index,
                scale,
                disp,
                ref mask_reg,
            ) => {
                stream.push("[", Colors::brackets());
                stream.push(regspec_label(base), Colors::register());
                stream.push(" + ", Colors::expr());
                stream.push(regspec_label(index), Colors::register());
                stream.push(" * ", Colors::expr());
                stream.push_owned(scale.to_string(), Colors::immediate());
                Number(disp).tokenize(stream);
                stream.push("]", Colors::brackets());

                stream.push("{", Colors::brackets());
                stream.push(regspec_label(mask_reg), Colors::register());
                stream.push("}", Colors::brackets());
            }
            Operand::Nothing => {}
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.name())
    }
}

const MNEMONICS: &[&str] = &[
    "invalid",
    "add",
    "or",
    "adc",
    "sbb",
    "and",
    "xor",
    "sub",
    "cmp",
    "xadd",
    "bt",
    "bts",
    "btc",
    "btr",
    "bsf",
    "bsr",
    "tzcnt",
    "movss",
    "addss",
    "subss",
    "mulss",
    "divss",
    "minss",
    "maxss",
    "sqrtss",
    "movsd",
    "sqrtsd",
    "addsd",
    "subsd",
    "mulsd",
    "divsd",
    "minsd",
    "maxsd",
    "movsldup",
    "movshdup",
    "movddup",
    "haddps",
    "hsubps",
    "addsubpd",
    "addsubps",
    "cvtsi2ss",
    "cvtsi2sd",
    "cvttsd2si",
    "cvttps2dq",
    "cvtpd2dq",
    "cvtpd2ps",
    "cvtps2dq",
    "cvtsd2si",
    "cvtsd2ss",
    "cvttss2si",
    "cvtss2si",
    "cvtss2sd",
    "cvtdq2pd",
    "lddqu",
    "movzx",
    "movsx",
    "movsxd",
    "sar",
    "sal",
    "shr",
    "shrd",
    "shl",
    "rcr",
    "rcl",
    "ror",
    "rol",
    "inc",
    "dec",
    "hlt",
    "call",
    "callf",
    "jmp",
    "jmpf",
    "push",
    "pop",
    "lea",
    "nop",
    "prefetchnta",
    "prefetch0",
    "prefetch1",
    "prefetch2",
    "xchg",
    "popf",
    "int",
    "into",
    "iret",
    "iretd",
    "iretq",
    "retf",
    "enter",
    "leave",
    "mov",
    "ret",
    "pushf",
    "wait",
    "cbw",
    "cwde",
    "cdqe",
    "cwd",
    "cdq",
    "cqo",
    "lods",
    "stos",
    "lahf",
    "sahf",
    "cmps",
    "scas",
    "movs",
    "test",
    "ins",
    "in",
    "outs",
    "out",
    "imul",
    "jo",
    "jno",
    "jb",
    "jnb",
    "jz",
    "jnz",
    "ja",
    "jna",
    "js",
    "jns",
    "jp",
    "jnp",
    "jl",
    "jge",
    "jle",
    "jg",
    "cmova",
    "cmovb",
    "cmovg",
    "cmovge",
    "cmovl",
    "cmovle",
    "cmovna",
    "cmovnb",
    "cmovno",
    "cmovnp",
    "cmovns",
    "cmovnz",
    "cmovo",
    "cmovp",
    "cmovs",
    "cmovz",
    "div",
    "idiv",
    "mul",
    "neg",
    "not",
    "cmpxchg",
    "seto",
    "setno",
    "setb",
    "setae",
    "setz",
    "setnz",
    "setbe",
    "seta",
    "sets",
    "setns",
    "setp",
    "setnp",
    "setl",
    "setge",
    "setle",
    "setg",
    "cpuid",
    "ud0",
    "ud1",
    "ud2",
    "wbinvd",
    "invd",
    "sysret",
    "clts",
    "syscall",
    "lsl",
    "lar",
    "les",
    "lds",
    "sgdt",
    "sidt",
    "lgdt",
    "lidt",
    "smsw",
    "lmsw",
    "swapgs",
    "rdtscp",
    "invlpg",
    "fxsave",
    "fxrstor",
    "ldmxcsr",
    "stmxcsr",
    "xsave",
    "xrstor",
    "xsaveopt",
    "lfence",
    "mfence",
    "sfence",
    "clflush",
    "clflushopt",
    "clwb",
    "wrmsr",
    "rdtsc",
    "rdmsr",
    "rdpmc",
    "sldt",
    "str",
    "lldt",
    "ltr",
    "verr",
    "verw",
    "cmc",
    "clc",
    "stc",
    "cli",
    "sti",
    "cld",
    "std",
    "jmpe",
    "popcnt",
    "movdqu",
    "movdqa",
    "movq",
    "cmpss",
    "cmpsd",
    "unpcklps",
    "unpcklpd",
    "unpckhps",
    "unpckhpd",
    "pshufhw",
    "pshuflw",
    "movups",
    "movq2dq",
    "movdq2q",
    "rsqrtss",
    "rcpss",
    "andn",
    "bextr",
    "blsi",
    "blsmsk",
    "blsr",
    "vmclear",
    "vmxon",
    "vmcall",
    "vmlaunch",
    "vmresume",
    "vmxoff",
    "pconfig",
    "monitor",
    "mwait",
    "monitorx",
    "mwaitx",
    "clac",
    "stac",
    "encls",
    "enclv",
    "xgetbv",
    "xsetbv",
    "vmfunc",
    "xabort",
    "xbegin",
    "xend",
    "xtest",
    "enclu",
    "rdpkru",
    "wrpkru",
    "rdpru",
    "clzero",
    "rdseed",
    "rdrand",
    "addps",
    "addpd",
    "andnps",
    "andnpd",
    "andps",
    "andpd",
    "bswap",
    "cmppd",
    "cmpps",
    "comisd",
    "comiss",
    "cvtdq2ps",
    "cvtpi2ps",
    "cvtpi2pd",
    "cvtps2pd",
    "cvtps2pi",
    "cvtpd2pi",
    "cvttps2pi",
    "cvttpd2pi",
    "cvttpd2dq",
    "divps",
    "divpd",
    "emms",
    "getsec",
    "lfs",
    "lgs",
    "lss",
    "maskmovq",
    "maskmovdqu",
    "maxps",
    "maxpd",
    "minps",
    "minpd",
    "movaps",
    "movapd",
    "movd",
    "movlps",
    "movlpd",
    "movhps",
    "movhpd",
    "movlhps",
    "movhlps",
    "movupd",
    "movmskps",
    "movmskpd",
    "movnti",
    "movntps",
    "movntpd",
    "extrq",
    "insertq",
    "movntss",
    "movntsd",
    "movntq",
    "movntdq",
    "mulps",
    "mulpd",
    "orps",
    "orpd",
    "packssdw",
    "packsswb",
    "packuswb",
    "paddb",
    "paddd",
    "paddq",
    "paddsb",
    "paddsw",
    "paddusb",
    "paddusw",
    "paddw",
    "pand",
    "pandn",
    "pavgb",
    "pavgw",
    "pcmpeqb",
    "pcmpeqd",
    "pcmpeqw",
    "pcmpgtb",
    "pcmpgtd",
    "pcmpgtw",
    "pinsrw",
    "pmaddwd",
    "pmaxsw",
    "pmaxub",
    "pminsw",
    "pminub",
    "pmovmskb",
    "pmulhuw",
    "pmulhw",
    "pmullw",
    "pmuludq",
    "por",
    "psadbw",
    "pshufw",
    "pshufd",
    "pslld",
    "pslldq",
    "psllq",
    "psllw",
    "psrad",
    "psraw",
    "psrld",
    "psrldq",
    "psrlq",
    "psrlw",
    "psubb",
    "psubd",
    "psubq",
    "psubsb",
    "psubsw",
    "psubusb",
    "psubusw",
    "psubw",
    "punpckhbw",
    "punpckhdq",
    "punpckhwd",
    "punpcklbw",
    "punpckldq",
    "punpcklwd",
    "punpcklqdq",
    "punpckhqdq",
    "pxor",
    "rcpps",
    "rsm",
    "rsqrtps",
    "shld",
    "shufpd",
    "shufps",
    "slhd",
    "sqrtps",
    "sqrtpd",
    "subps",
    "subpd",
    "sysenter",
    "sysexit",
    "ucomisd",
    "ucomiss",
    "vmread",
    "vmwrite",
    "xorps",
    "xorpd",
    "vmovddup",
    "vpshuflw",
    "vpshufhw",
    "vhaddps",
    "vhsubps",
    "vaddsubps",
    "vcvtpd2dq",
    "vlddqu",
    "vcomisd",
    "vcomiss",
    "vucomisd",
    "vucomiss",
    "vaddpd",
    "vaddps",
    "vaddsd",
    "vaddss",
    "vaddsubpd",
    "vaesdec",
    "vaesdeclast",
    "vaesenc",
    "vaesenclast",
    "vaesimc",
    "vaeskeygenassist",
    "vblendpd",
    "vblendps",
    "vblendvpd",
    "vblendvps",
    "vbroadcastf128",
    "vbroadcasti128",
    "vbroadcastsd",
    "vbroadcastss",
    "vcmpsd",
    "vcmpss",
    "vcmppd",
    "vcmpps",
    "vcvtdq2pd",
    "vcvtdq2ps",
    "vcvtpd2ps",
    "vcvtph2ps",
    "vcvtps2dq",
    "vcvtps2pd",
    "vcvtss2sd",
    "vcvtsi2ss",
    "vcvtsi2sd",
    "vcvtsd2si",
    "vcvtsd2ss",
    "vcvtps2ph",
    "vcvtss2si",
    "vcvttpd2dq",
    "vcvttps2dq",
    "vcvttss2si",
    "vcvttsd2si",
    "vdivpd",
    "vdivps",
    "vdivsd",
    "vdivss",
    "vdppd",
    "vdpps",
    "vextractf128",
    "vextracti128",
    "vextractps",
    "vfmadd132pd",
    "vfmadd132ps",
    "vfmadd132sd",
    "vfmadd132ss",
    "vfmadd213pd",
    "vfmadd213ps",
    "vfmadd213sd",
    "vfmadd213ss",
    "vfmadd231pd",
    "vfmadd231ps",
    "vfmadd231sd",
    "vfmadd231ss",
    "vfmaddsub132pd",
    "vfmaddsub132ps",
    "vfmaddsub213pd",
    "vfmaddsub213ps",
    "vfmaddsub231pd",
    "vfmaddsub231ps",
    "vfmsub132pd",
    "vfmsub132ps",
    "vfmsub132sd",
    "vfmsub132ss",
    "vfmsub213pd",
    "vfmsub213ps",
    "vfmsub213sd",
    "vfmsub213ss",
    "vfmsub231pd",
    "vfmsub231ps",
    "vfmsub231sd",
    "vfmsub231ss",
    "vfmsubadd132pd",
    "vfmsubadd132ps",
    "vfmsubadd213pd",
    "vfmsubadd213ps",
    "vfmsubadd231pd",
    "vfmsubadd231ps",
    "vfnmadd132pd",
    "vfnmadd132ps",
    "vfnmadd132sd",
    "vfnmadd132ss",
    "vfnmadd213pd",
    "vfnmadd213ps",
    "vfnmadd213sd",
    "vfnmadd213ss",
    "vfnmadd231pd",
    "vfnmadd231ps",
    "vfnmadd231sd",
    "vfnmadd231ss",
    "vfnmsub132pd",
    "vfnmsub132ps",
    "vfnmsub132sd",
    "vfnmsub132ss",
    "vfnmsub213pd",
    "vfnmsub213ps",
    "vfnmsub213sd",
    "vfnmsub213ss",
    "vfnmsub231pd",
    "vfnmsub231ps",
    "vfnmsub231sd",
    "vfnmsub231ss",
    "vgatherdpd",
    "vgatherdps",
    "vgatherqpd",
    "vgatherqps",
    "vhaddpd",
    "vhsubpd",
    "vinsertf128",
    "vinserti128",
    "vinsertps",
    "vmaskmovdqu",
    "vmaskmovpd",
    "vmaskmovps",
    "vmaxpd",
    "vmaxps",
    "vmaxsd",
    "vmaxss",
    "vminpd",
    "vminps",
    "vminsd",
    "vminss",
    "vmovapd",
    "vmovaps",
    "vmovd",
    "vmovdqa",
    "vmovdqu",
    "vmovhlps",
    "vmovhpd",
    "vmovhps",
    "vmovlhps",
    "vmovlpd",
    "vmovlps",
    "vmovmskpd",
    "vmovmskps",
    "vmovntdq",
    "vmovntdqa",
    "vmovntpd",
    "vmovntps",
    "vmovq",
    "vmovss",
    "vmovsd",
    "vmovshdup",
    "vmovsldup",
    "vmovupd",
    "vmovups",
    "vmpsadbw",
    "vmulpd",
    "vmulps",
    "vmulsd",
    "vmulss",
    "vpabsb",
    "vpabsd",
    "vpabsw",
    "vpackssdw",
    "vpackusdw",
    "vpacksswb",
    "vpackuswb",
    "vpaddb",
    "vpaddd",
    "vpaddq",
    "vpaddsb",
    "vpaddsw",
    "vpaddusb",
    "vpaddusw",
    "vpaddw",
    "vpalignr",
    "vandpd",
    "vandps",
    "vorpd",
    "vorps",
    "vandnpd",
    "vandnps",
    "vpand",
    "vpandn",
    "vpavgb",
    "vpavgw",
    "vpblendd",
    "vpblendvb",
    "vpblendw",
    "vpbroadcastb",
    "vpbroadcastd",
    "vpbroadcastq",
    "vpbroadcastw",
    "vpclmulqdq",
    "vpcmpeqb",
    "vpcmpeqd",
    "vpcmpeqq",
    "vpcmpeqw",
    "vpcmpgtb",
    "vpcmpgtd",
    "vpcmpgtq",
    "vpcmpgtw",
    "vpcmpestri",
    "vpcmpestrm",
    "vpcmpistri",
    "vpcmpistrm",
    "vperm2f128",
    "vperm2i128",
    "vpermd",
    "vpermilpd",
    "vpermilps",
    "vpermpd",
    "vpermps",
    "vpermq",
    "vpextrb",
    "vpextrd",
    "vpextrq",
    "vpextrw",
    "vpgatherdd",
    "vpgatherdq",
    "vpgatherqd",
    "vpgatherqq",
    "vphaddd",
    "vphaddsw",
    "vphaddw",
    "vpmaddubsw",
    "vphminposuw",
    "vphsubd",
    "vphsubsw",
    "vphsubw",
    "vpinsrb",
    "vpinsrd",
    "vpinsrq",
    "vpinsrw",
    "vpmaddwd",
    "vpmaskmovd",
    "vpmaskmovq",
    "vpmaxsb",
    "vpmaxsd",
    "vpmaxsw",
    "vpmaxub",
    "vpmaxuw",
    "vpmaxud",
    "vpminsb",
    "vpminsw",
    "vpminsd",
    "vpminub",
    "vpminuw",
    "vpminud",
    "vpmovmskb",
    "vpmovsxbd",
    "vpmovsxbq",
    "vpmovsxbw",
    "vpmovsxdq",
    "vpmovsxwd",
    "vpmovsxwq",
    "vpmovzxbd",
    "vpmovzxbq",
    "vpmovzxbw",
    "vpmovzxdq",
    "vpmovzxwd",
    "vpmovzxwq",
    "vpmuldq",
    "vpmulhrsw",
    "vpmulhuw",
    "vpmulhw",
    "vpmullq",
    "vpmulld",
    "vpmullw",
    "vpmuludq",
    "vpor",
    "vpsadbw",
    "vpshufb",
    "vpshufd",
    "vpsignb",
    "vpsignd",
    "vpsignw",
    "vpslld",
    "vpslldq",
    "vpsllq",
    "vpsllvd",
    "vpsllvq",
    "vpsllw",
    "vpsrad",
    "vpsravd",
    "vpsraw",
    "vpsrld",
    "vpsrldq",
    "vpsrlq",
    "vpsrlvd",
    "vpsrlvq",
    "vpsrlw",
    "vpsubb",
    "vpsubd",
    "vpsubq",
    "vpsubsb",
    "vpsubsw",
    "vpsubusb",
    "vpsubusw",
    "vpsubw",
    "vptest",
    "vpunpckhbw",
    "vpunpckhdq",
    "vpunpckhqdq",
    "vpunpckhwd",
    "vpunpcklbw",
    "vpunpckldq",
    "vpunpcklqdq",
    "vpunpcklwd",
    "vpxor",
    "vrcpps",
    "vroundpd",
    "vroundps",
    "vroundsd",
    "vroundss",
    "vrsqrtps",
    "vrsqrtss",
    "vrcpss",
    "vshufpd",
    "vshufps",
    "vsqrtpd",
    "vsqrtps",
    "vsqrtss",
    "vsqrtsd",
    "vsubpd",
    "vsubps",
    "vsubsd",
    "vsubss",
    "vtestpd",
    "vtestps",
    "vunpckhpd",
    "vunpckhps",
    "vunpcklpd",
    "vunpcklps",
    "vxorpd",
    "vxorps",
    "vzeroupper",
    "vzeroall",
    "vldmxcsr",
    "vstmxcsr",
    "pclmulqdq",
    "aeskeygenassist",
    "aesimc",
    "aesenc",
    "aesenclast",
    "aesdec",
    "aesdeclast",
    "pcmpgtq",
    "pcmpistrm",
    "pcmpistri",
    "pcmpestri",
    "packusdw",
    "pcmpestrm",
    "pcmpeqq",
    "ptest",
    "phminposuw",
    "dpps",
    "dppd",
    "mpsadbw",
    "pmovzxdq",
    "pmovsxdq",
    "pmovzxbd",
    "pmovsxbd",
    "pmovzxwq",
    "pmovsxwq",
    "pmovzxbq",
    "pmovsxbq",
    "pmovsxwd",
    "pmovzxwd",
    "pextrq",
    "pextrd",
    "pextrw",
    "pextrb",
    "pmovsxbw",
    "pmovzxbw",
    "pinsrq",
    "pinsrd",
    "pinsrb",
    "extractps",
    "insertps",
    "roundss",
    "roundsd",
    "roundps",
    "roundpd",
    "pmaxsb",
    "pmaxsd",
    "pmaxuw",
    "pmaxud",
    "pminsd",
    "pminsb",
    "pminud",
    "pminuw",
    "blendw",
    "pblendvb",
    "pblendw",
    "blendvps",
    "blendvpd",
    "blendps",
    "blendpd",
    "pmuldq",
    "movntdqa",
    "pmulld",
    "palignr",
    "psignw",
    "psignd",
    "psignb",
    "pshufb",
    "pmulhrsw",
    "pmaddubsw",
    "pabsd",
    "pabsw",
    "pabsb",
    "phsubsw",
    "phsubw",
    "phsubd",
    "phaddd",
    "phaddsw",
    "phaddw",
    "hsubpd",
    "haddpd",
    "sha1rnds4",
    "sha1nexte",
    "sha1msg1",
    "sha1msg2",
    "sha256rnds2",
    "sha256msg1",
    "sha256msg2",
    "lzcnt",
    "clgi",
    "stgi",
    "skinit",
    "vmload",
    "vmmcall",
    "vmsave",
    "vmrun",
    "invlpga",
    "invlpgb",
    "tlbsync",
    "movbe",
    "adcx",
    "adox",
    "prefetchw",
    "rdpid",
    "cmpxchg8b",
    "cmpxchg16b",
    "vmptrld",
    "vmptrst",
    "bzhi",
    "mulx",
    "shlx",
    "shrx",
    "sarx",
    "pdep",
    "pext",
    "rorx",
    "xrstors",
    "xrstors64",
    "xsavec",
    "xsavec64",
    "xsaves",
    "xsaves64",
    "rdfsbase",
    "rdgsbase",
    "wrfsbase",
    "wrgsbase",
    "crc32",
    "salc",
    "xlat",
    "f2xm1",
    "fabs",
    "fadd",
    "faddp",
    "fbld",
    "fbstp",
    "fchs",
    "fcmovb",
    "fcmovbe",
    "fcmove",
    "fcmovnb",
    "fcmovnbe",
    "fcmovne",
    "fcmovnu",
    "fcmovu",
    "fcom",
    "fcomi",
    "fcomip",
    "fcomp",
    "fcompp",
    "fcos",
    "fdecstp",
    "fdisi8087_nop",
    "fdiv",
    "fdivp",
    "fdivr",
    "fdivrp",
    "feni8087_nop",
    "ffree",
    "ffreep",
    "fiadd",
    "ficom",
    "ficomp",
    "fidiv",
    "fidivr",
    "fild",
    "fimul",
    "fincstp",
    "fist",
    "fistp",
    "fisttp",
    "fisub",
    "fisubr",
    "fld",
    "fld1",
    "fldcw",
    "fldenv",
    "fldl2e",
    "fldl2t",
    "fldlg2",
    "fldln2",
    "fldpi",
    "fldz",
    "fmul",
    "fmulp",
    "fnclex",
    "fninit",
    "fnop",
    "fnsave",
    "fnstcw",
    "fnstenv",
    "fnstor",
    "fnstsw",
    "fpatan",
    "fprem",
    "fprem1",
    "fptan",
    "frndint",
    "frstor",
    "fscale",
    "fsetpm287_nop",
    "fsin",
    "fsincos",
    "fsqrt",
    "fst",
    "fstp",
    "fstpnce",
    "fsub",
    "fsubp",
    "fsubr",
    "fsubrp",
    "ftst",
    "fucom",
    "fucomi",
    "fucomip",
    "fucomp",
    "fucompp",
    "fxam",
    "fxch",
    "fxtract",
    "fyl2x",
    "fyl2xp1",
    "loopnz",
    "loopz",
    "loop",
    "jecxz",
    "pusha",
    "popa",
    "bound",
    "arpl",
    "aas",
    "aaa",
    "das",
    "daa",
    "aam",
    "aad",
    "movdir64b",
    "movdiri",
    "aesdec128kl",
    "aesdec256kl",
    "aesdecwide128kl",
    "aesdecwide256kl",
    "aesenc128kl",
    "aesenc256kl",
    "aesencwide128kl",
    "aesencwide256kl",
    "encodekey128",
    "encodekey256",
    "loadiwkey",
    // unsure
    "hreset",
    // 3dnow
    "femms",
    "pi2fw",
    "pi2fd",
    "pi2iw",
    "pi2id",
    "pmulhrw",
    "pfcmpge",
    "pfmin",
    "pfrcp",
    "pfrsqrt",
    "pfsub",
    "pfadd",
    "pfcmpgt",
    "pfmax",
    "pfrcpit1",
    "pfrsqit1",
    "pfsubr",
    "pfacc",
    "pfcmpeq",
    "pfmul",
    "pfmulhrw",
    "pfrcpit2",
    "pfnacc",
    "pfpnacc",
    "pswapd",
    "pavgusb",
    // ENQCMD
    "enqcmd",
    "enqcmds",
    // INVPCID,
    "invept",
    "invvpid",
    "invpcid",
    // PTWRITE
    "ptwrite",
    // GFNI
    "gf2p8affineqb",
    "gf2p8affineinvqb",
    "gf2p8mulb",
    // CET
    "wruss",
    "wrss",
    "incssp",
    "saveprevssp",
    "setssbsy",
    "clrssbsy",
    "rstorssp",
    "endbr64",
    "endbr32",
    // TDX
    "tdcall",
    "seamret",
    "seamops",
    "seamcall",
    // WAITPKG
    "tpause",
    "umonitor",
    "umwait",
    // UINTR
    "uiret",
    "testui",
    "clui",
    "stui",
    "senduipi",
    // TSXLDTRK
    "xsusldtrk",
    "xresldtrk",
    // AVX512F
    "valignd",
    "valignq",
    "vblendmpd",
    "vblendmps",
    "vcompresspd",
    "vcompressps",
    "vcvtpd2udq",
    "vcvttpd2udq",
    "vcvtps2udq",
    "vcvttps2udq",
    "vcvtqq2pd",
    "vcvtqq2ps",
    "vcvtsd2usi",
    "vcvttsd2usi",
    "vcvtss2usi",
    "vcvttss2usi",
    "vcvtudq2pd",
    "vcvtudq2ps",
    "vcvtusi2usd",
    "vcvtusi2uss",
    "vexpandpd",
    "vexpandps",
    "vextractf32x4",
    "vextractf64x4",
    "vextracti32x4",
    "vextracti64x4",
    "vfixupimmpd",
    "vfixupimmps",
    "vfixupimmsd",
    "vfixupimmss",
    "vgetexppd",
    "vgetexpps",
    "vgetexpsd",
    "vgetexpss",
    "vgetmantpd",
    "vgetmantps",
    "vgetmantsd",
    "vgetmantss",
    "vinsertf32x4",
    "vinsertf64x4",
    "vinserti64x4",
    "vmovdqa32",
    "vmovdqa64",
    "vmovdqu32",
    "vmovdqu64",
    "vpblendmd",
    "vpblendmq",
    "vpcmpd",
    "vpcmpud",
    "vpcmpq",
    "vpcmpuq",
    "vpcompressq",
    "vpcompressd",
    "vpermi2d",
    "vpermi2q",
    "vpermi2pd",
    "vpermi2ps",
    "vpermt2d",
    "vpermt2q",
    "vpermt2pd",
    "vpermt2ps",
    "vpmaxsq",
    "vpmaxuq",
    "vpminsq",
    "vpminuq",
    "vpmovsqb",
    "vpmovusqb",
    "vpmovsqw",
    "vpmovusqw",
    "vpmovsqd",
    "vpmovusqd",
    "vpmovsdb",
    "vpmovusdb",
    "vpmovsdw",
    "vpmovusdw",
    "vprold",
    "vprolq",
    "vprolvd",
    "vprolvq",
    "vprord",
    "vprorq",
    "vprorrd",
    "vprorrq",
    "vpscatterdd",
    "vpscatterdq",
    "vpscatterqd",
    "vpscatterqq",
    "vpsraq",
    "vpsravq",
    "vptestnmd",
    "vptestnmq",
    "vpternlogd",
    "vpternlogq",
    "vptestmd",
    "vptestmq",
    "vrcp14pd",
    "vrcp14ps",
    "vrcp14sd",
    "vrcp14ss",
    "vrndscalepd",
    "vrndscaleps",
    "vrndscalesd",
    "vrndscaless",
    "vrsqrt14pd",
    "vrsqrt14ps",
    "vrsqrt14sd",
    "vrsqrt14ss",
    "vscaledpd",
    "vscaledps",
    "vscaledsd",
    "vscaledss",
    "vscatterdd",
    "vscatterdq",
    "vscatterqd",
    "vscatterqq",
    "vshuff32x4",
    "vshuff64x2",
    "vshufi32x4",
    "vshufi64x2",
    // AVX512DQ
    "vcvttpd2qq",
    "vcvtpd2qq",
    "vcvttpd2uqq",
    "vcvtpd2uqq",
    "vcvttps2qq",
    "vcvtps2qq",
    "vcvttps2uqq",
    "vcvtps2uqq",
    "vcvtuqq2pd",
    "vcvtuqq2ps",
    "vextractf64x2",
    "vextracti64x2",
    "vfpclasspd",
    "vfpclassps",
    "vfpclasssd",
    "vfpclassss",
    "vinsertf64x2",
    "vinserti64x2",
    "vpmovm2d",
    "vpmovm2q",
    "vpmovb2d",
    "vpmovq2m",
    "vrangepd",
    "vrangeps",
    "vrangesd",
    "vrangess",
    "vreducepd",
    "vreduceps",
    "vreducesd",
    "vreducess",
    // AVX512BW
    "vdbpsadbw",
    "vmovdqu8",
    "vmovdqu16",
    "vpblendmb",
    "vpblendmw",
    "vpcmpb",
    "vpcmpub",
    "vpcmpw",
    "vpcmpuw",
    "vpermw",
    "vpermi2b",
    "vpermi2w",
    "vpmovm2b",
    "vpmovm2w",
    "vpmovb2m",
    "vpmovw2m",
    "vpmovswb",
    "vpmovuswb",
    "vpsllvw",
    "vpsravw",
    "vpsrlvw",
    "vptestnmb",
    "vptestnmw",
    "vptestmb",
    "vptestmw",
    // AVX512CD
    "vpbroadcastm",
    "vpconflictd",
    "vpconflictq",
    "vplzcntd",
    "vplzcntq",
    "kunpckbw",
    "kunpckwd",
    "kunpckdq",
    "kaddb",
    "kandb",
    "kandnb",
    "kmovb",
    "knotb",
    "korb",
    "kortestb",
    "kshiftlb",
    "kshiftrb",
    "ktestb",
    "kxnorb",
    "kxorb",
    "kaddw",
    "kandw",
    "kandnw",
    "kmovw",
    "knotw",
    "korw",
    "kortestw",
    "kshiftlw",
    "kshiftrw",
    "ktestw",
    "kxnorw",
    "kxorw",
    "kaddd",
    "kandd",
    "kandnd",
    "kmovd",
    "knotd",
    "kord",
    "kortestd",
    "kshiftld",
    "kshiftrd",
    "ktestd",
    "kxnord",
    "kxord",
    "kaddq",
    "kandq",
    "kandnq",
    "kmovq",
    "knotq",
    "korq",
    "kortestq",
    "kshiftlq",
    "kshiftrq",
    "ktestq",
    "kxnorq",
    "kxorq",
    // AVX512ER
    "vexp2pd",
    "vexp2ps",
    "vexp2sd",
    "vexp2ss",
    "vrcp28pd",
    "vrcp28ps",
    "vrcp28sd",
    "vrcp28ss",
    "vrsqrt28pd",
    "vrsqrt28ps",
    "vrsqrt28sd",
    "vrsqrt28ss",
    // AVX512PF
    "vgatherpf0dpd",
    "vgatherpf0dps",
    "vgatherpf0qpd",
    "vgatherpf0qps",
    "vgatherpf1dpd",
    "vgatherpf1dps",
    "vgatherpf1qpd",
    "vgatherpf1qps",
    "vscatterpf0dpd",
    "vscatterpf0dps",
    "vscatterpf0qpd",
    "vscatterpf0qps",
    "vscatterpf1dpd",
    "vscatterpf1dps",
    "vscatterpf1qpd",
    "vscatterpf1qps",
    // MPX
    "bndmk",
    "bndcl",
    "bndcu",
    "bndcn",
    "bndmov",
    "bndldx",
    "bndstx",
    "vgf2p8affineqb",
    "vgf2p8affineinvqb",
    "vpshrdq",
    "vpshrdd",
    "vpshrdw",
    "vpshldq",
    "vpshldd",
    "vpshldw",
    "vbroadcastf32x8",
    "vbroadcastf64x4",
    "vbroadcastf32x4",
    "vbroadcastf64x2",
    "vbroadcastf32x2",
    "vbroadcasti32x8",
    "vbroadcasti64x4",
    "vbroadcasti32x4",
    "vbroadcasti64x2",
    "vbroadcasti32x2",
    "vextracti32x8",
    "vextractf32x8",
    "vinserti32x8",
    "vinsertf32x8",
    "vinserti32x4",
    "v4fnmaddss",
    "v4fnmaddps",
    "vcvtneps2bf16",
    "v4fmaddss",
    "v4fmaddps",
    "vcvtne2ps2bf16",
    "vp2intersectd",
    "vp2intersectq",
    "vp4dpwssds",
    "vp4dpwssd",
    "vpdpwssds",
    "vpdpwssd",
    "vpdpbusds",
    "vdpbf16ps",
    "vpbroadcastmw2d",
    "vpbroadcastmb2q",
    "vpmovd2m",
    "vpmovqd",
    "vpmovwb",
    "vpmovdb",
    "vpmovdw",
    "vpmovqb",
    "vpmovqw",
    "vgf2p8mulb",
    "vpmadd52huq",
    "vpmadd52luq",
    "vpshufbitqmb",
    "vpermb",
    "vpexpandd",
    "vpexpandq",
    "vpabsq",
    "vprorvd",
    "vprorvq",
    "vpmultishiftqb",
    "vpermt2b",
    "vpermt2w",
    "vpshrdvq",
    "vpshrdvd",
    "vpshrdvw",
    "vpshldvq",
    "vpshldvd",
    "vpshldvw",
    "vpcompressb",
    "vpcompressw",
    "vpexpandb",
    "vpexpandw",
    "vpopcntd",
    "vpopcntq",
    "vpopcntb",
    "vpopcntw",
    "vscalefss",
    "vscalefsd",
    "vscalefps",
    "vscalefpd",
    "vpdpbusd",
    "vcvtusi2sd",
    "vcvtusi2ss",
    "vpxord",
    "vpxorq",
    "vpord",
    "vporq",
    "vpandnd",
    "vpandnq",
    "vpandd",
    "vpandq",
    "psmash",
    "pvalidate",
    "rmpadjust",
    "rmpupdate",
];

impl Opcode {
    fn name(&self) -> &'static str {
        unsafe { MNEMONICS.get_kinda_unchecked(*self as usize) }
    }
}

impl ToTokens for Instruction {
    fn tokenize(&self, stream: &mut decoder::TokenStream) {
        let opcode_name = self.opcode().name();
        let mut op = String::with_capacity(opcode_name.len());

        if self.xacquire() {
            op.push_str("xacquire ");
        }
        if self.xrelease() {
            op.push_str("xrelease ");
        }
        if self.prefixes.lock() {
            op.push_str("lock ");
        }

        if self.prefixes.rep_any() {
            let ops = [
                Opcode::MOVS,
                Opcode::CMPS,
                Opcode::LODS,
                Opcode::STOS,
                Opcode::INS,
                Opcode::OUTS,
            ];
            if ops.contains(&self.opcode) && self.prefixes.rep() {
                if self.prefixes.rep() {
                    op.push_str("rep ");
                } else if self.prefixes.repnz() {
                    op.push_str("repnz ");
                }
            }
        }

        op.push_str(opcode_name);
        stream.push_owned(op, Colors::opcode());

        if self.opcode == Opcode::XBEGIN {
            if (self.imm as i64) >= 0 {
                stream.push(" $+", Colors::expr());
                stream.push_owned(decoder::encode_hex(self.imm as i64), Colors::immediate());
                return;
            } else {
                stream.push(" $", Colors::expr());
                stream.push_owned(decoder::encode_hex(self.imm as i64), Colors::immediate());
                return;
            }
        }

        if self.operand_count > 0 {
            stream.push(" ", Colors::spacing());

            let op = Operand::from_spec(&self, self.operands[0]);

            const RELATIVE_BRANCHES: [Opcode; 21] = [
                Opcode::JMP,
                Opcode::JECXZ,
                Opcode::LOOP,
                Opcode::LOOPZ,
                Opcode::LOOPNZ,
                Opcode::JO,
                Opcode::JNO,
                Opcode::JB,
                Opcode::JNB,
                Opcode::JZ,
                Opcode::JNZ,
                Opcode::JNA,
                Opcode::JA,
                Opcode::JS,
                Opcode::JNS,
                Opcode::JP,
                Opcode::JNP,
                Opcode::JL,
                Opcode::JGE,
                Opcode::JLE,
                Opcode::JG,
            ];

            if (self.operands[0] == OperandSpec::ImmI8 || self.operands[0] == OperandSpec::ImmI32)
                && RELATIVE_BRANCHES.contains(&self.opcode)
            {
                return match op {
                    Operand::ImmediateI8(rel) => {
                        if rel >= 0 {
                            stream.push("$+", Colors::expr());
                            let rel = decoder::encode_hex(rel as i64);
                            stream.push_owned(rel, Colors::immediate());
                        } else {
                            stream.push("$", Colors::expr());
                            let rel = decoder::encode_hex(rel as i64);
                            stream.push_owned(rel, Colors::immediate());
                        }
                    }
                    Operand::ImmediateI32(rel) => {
                        if rel >= 0 {
                            stream.push("$+", Colors::expr());
                            let rel = decoder::encode_hex(rel as i64);
                            stream.push_owned(rel, Colors::immediate());
                        } else {
                            stream.push("$", Colors::expr());
                            let rel = decoder::encode_hex(rel as i64);
                            stream.push_owned(rel, Colors::immediate());
                        }
                    }
                    _ => {
                        unreachable!()
                    }
                };
            }

            if op.is_memory() {
                stream.push(
                    MEM_SIZE_STRINGS[self.mem_size as usize - 1],
                    Colors::annotation(),
                );
            }

            if let Some(prefix) = self.segment_override_for_op(0) {
                stream.push_owned(prefix.to_string(), Colors::segment());
                stream.push(":", Colors::expr());
            }

            op.tokenize(stream);

            for idx in 1..self.operand_count {
                if self.operands[idx as usize] == OperandSpec::Nothing {
                    continue;
                }

                stream.push(", ", Colors::expr());
                let op = Operand::from_spec(&self, self.operands[idx as usize]);
                if op.is_memory() {
                    stream.push(
                        MEM_SIZE_STRINGS[self.mem_size as usize - 1],
                        Colors::annotation(),
                    );
                }
                if let Some(prefix) = self.segment_override_for_op(idx) {
                    stream.push_owned(prefix.to_string(), Colors::segment());
                    stream.push(":", Colors::expr());
                }

                // TODO: remove clone
                op.clone().tokenize(stream);
                if let Some(evex) = self.prefixes.evex() {
                    if evex.broadcast() && op.is_memory() {
                        let scale = if self.opcode == Opcode::VCVTPD2PS
                            || self.opcode == Opcode::VCVTTPD2UDQ
                            || self.opcode == Opcode::VCVTPD2UDQ
                            || self.opcode == Opcode::VCVTUDQ2PD
                            || self.opcode == Opcode::VCVTPS2PD
                            || self.opcode == Opcode::VCVTQQ2PS
                            || self.opcode == Opcode::VCVTDQ2PD
                            || self.opcode == Opcode::VCVTTPD2DQ
                            || self.opcode == Opcode::VFPCLASSPS
                            || self.opcode == Opcode::VFPCLASSPD
                            || self.opcode == Opcode::VCVTNEPS2BF16
                            || self.opcode == Opcode::VCVTUQQ2PS
                            || self.opcode == Opcode::VCVTPD2DQ
                            || self.opcode == Opcode::VCVTTPS2UQQ
                            || self.opcode == Opcode::VCVTPS2UQQ
                            || self.opcode == Opcode::VCVTTPS2QQ
                            || self.opcode == Opcode::VCVTPS2QQ
                        {
                            if self.opcode == Opcode::VFPCLASSPS
                                || self.opcode == Opcode::VCVTNEPS2BF16
                            {
                                if evex.vex().l() {
                                    8
                                } else if evex.lp() {
                                    16
                                } else {
                                    4
                                }
                            } else {
                                // vcvtpd2ps is "cool": in broadcast mode, it can read a
                                // double-precision float (qword), resize to single-precision,
                                // then broadcast that to the whole destination register. this
                                // means we need to show `xmm, qword [addr]{1to4}` if vector
                                // size is 256. likewise, scale of 8 for the same truncation
                                // reason if vector size is 512.
                                // vcvtudq2pd is the same story.
                                // vfpclassp{s,d} is a mystery to me.
                                if evex.vex().l() {
                                    4
                                } else if evex.lp() {
                                    8
                                } else {
                                    2
                                }
                            }
                        } else {
                            // this should never be `None` - that would imply two
                            // memory operands for a broadcasted operation.
                            if let Some(width) =
                                Operand::from_spec(&self, self.operands[idx as usize - 1]).width()
                            {
                                width / self.mem_size
                            } else {
                                0
                            }
                        };

                        stream.push("{", Colors::brackets());
                        stream.push("1to", Colors::expr());
                        stream.push_owned(scale.to_string(), Colors::immediate());
                        stream.push("}", Colors::brackets());
                    }
                }
            }
        }
    }
}
