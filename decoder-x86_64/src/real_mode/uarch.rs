pub mod amd {
    //! most information about instruction set extensions for microarchitectures here was sourced
    //! from
    //! [https://en.wikipedia.org/wiki/AMD_Accelerated_Processing_Unit#Feature_overview](https://docs.rs/yaxpeax-x86/0.0.12/x86_64/real_mode/uarch/intel/index.html)
    //! and
    //! [https://en.wikipedia.org/wiki/Template:AMD_x86_CPU_features](https://docs.rs/yaxpeax-x86/0.0.12/x86_64/real_mode/uarch/intel/index.html).
    //! these mappings are best-effort but fairly unused, so a critical eye should be kept towards
    //! these decoders rejecting instructions they should not, or incorrectly accepting
    //! instructions.
    //!
    //! microarchitectures as defined here are with respect to flags reported by CPUID. notably,
    //! `Zen` does not report `FMA4` support by `CPUID`, but instructions in that extension
    //! reportedly function correctly (agner p217).
    //!
    //! [agner](https://www.agner.org/optimize/microarchitecture.pdf)
    //! as retrieved 2020 may 19,
    //! `sha256: 87ff152ae18c017dcbfb9f7ee6e88a9f971f6250fd15a70a3dd87c3546323bd5`

    use crate::real_mode::InstDecoder;

    /// `k8` was the first AMD microarchitecture to implement x86_64, launched in 2003. while later
    /// `k8`-based processors supported SSE3, these predefined decoders pick the lower end of
    /// support - SSE2 and no later.
    pub fn k8() -> InstDecoder {
        InstDecoder::minimal()
    }

    /// `k10` was the successor to `k8`, launched in 2007. `k10` cores extended SSE support through
    /// to SSE4.2a, as well as consistent `cmov` support, among other features.
    pub fn k10() -> InstDecoder {
        k8()
            .with_cmov()
            .with_cmpxchg16b()
            .with_svm()
            .with_abm()
            .with_lahfsahf()
            .with_sse3()
            .with_ssse3()
            .with_sse4()
            .with_sse4_2()
            .with_sse4a()
    }

    /// `Bulldozer` was the successor to `K10`, launched in 2011. `Bulldozer` cores include AVX
    /// support among other extensions, and are notable for including `AESNI`.
    pub fn bulldozer() -> InstDecoder {
        k10()
            .with_bmi1()
            .with_aesni()
            .with_pclmulqdq()
            .with_f16c()
            .with_avx()
            .with_fma4()
            .with_xop()
    }

    /// `Piledriver` was the successor to `Bulldozer`, launched in 2012.
    pub fn piledriver() -> InstDecoder {
        bulldozer()
            .with_tbm()
            .with_fma3()
            .with_fma4()
    }

    /// `Steamroller` was the successor to `Piledriver`, launched in 2014. unlike `Piledriver`
    /// cores, these cores do not support `TBM` or `FMA3`.
    pub fn steamroller() -> InstDecoder {
        bulldozer()
    }

    /// `Excavator` was the successor to `Steamroller`, launched in 2015.
    pub fn excavator() -> InstDecoder {
        steamroller()
            .with_movbe()
            .with_bmi2()
            .with_rdrand()
            .with_avx()
            .with_xop()
            .with_bmi2()
            .with_sha()
            .with_rdrand()
            .with_avx2()
    }

    /// `Zen` was the successor to `Excavator`, launched in 2017. `Zen` cores extend SIMD
    /// instructions to AVX2 and discarded FMA4, TBM, and XOP extensions. they also gained ADX,
    /// SHA, RDSEED, and other extensions.
    pub fn zen() -> InstDecoder {
        k10()
            .with_avx()
            .with_avx2()
            .with_bmi1()
            .with_aesni()
            .with_pclmulqdq()
            .with_f16c()
            .with_movbe()
            .with_bmi2()
            .with_rdrand()
            .with_adx()
            .with_sha()
            .with_rdseed()
            .with_fma3()
            // TODO: XSAVEC, XSAVES, XRSTORS, CLFLUSHOPT, CLZERO?
    }
}

pub mod intel {
    //! sourced by walking wikipedia pages. seriously! this stuff is kinda hard to figure out!

    use crate::real_mode::InstDecoder;

    /// `Netburst` was the first Intel microarchitecture to implement x86_64, beginning with the
    /// `Prescott` family launched in 2004. while the wider `Netburst` family launched in 2000
    /// with only SSE2, the first `x86_64`-supporting incarnation was `Prescott` which indeed
    /// included SSE3.
    pub fn netburst() -> InstDecoder {
        InstDecoder::minimal()
            .with_cmov()
            .with_sse3()
    }

    /// `Core` was the successor to `Netburst`, launched in 2006. it included up to SSE4, with
    /// processors using this architecture shipped under the names "Merom", "Conroe", and
    /// "Woodcrest", for mobile, desktop, and server processors respectively. not to be confused
    /// with the later `Nehalem` microarchitecture that introduced the `Core i*` product lines,
    /// `Core 2 *` processors used the `Core` architecture.
    pub fn core() -> InstDecoder {
        netburst()
            .with_ssse3()
            .with_sse4()
    }

    /// `Penryn` was the successor to `Core`, launched in early 2008. it added SSE4.1, along with
    /// virtualization extensions.
    pub fn penryn() -> InstDecoder {
        core()
            .with_sse4_1()
    }

    /// `Nehalem` was the successor to `Penryn`, launched in late 2008. not to be confused with the
    /// earlier `Core` microarchitecture, the `Core i*` products were based on `Nehalem` cores.
    /// `Nehalem` added SSE4.2 extensions, along with the `POPCNT` instruction.
    pub fn nehalem() -> InstDecoder {
        penryn()
            .with_sse4_2()
            .with_popcnt()
    }

    /// `Westmere` was the successor to `Nehalem`, launched in 2010. it added AES-NI and CLMUL
    /// extensions.
    pub fn westmere() -> InstDecoder {
        nehalem()
            .with_aesni()
            .with_pclmulqdq()
    }

    /// `Sandy Bridge` was the successor to `Westmere`, launched in 2011. it added AVX
    /// instructions.
    pub fn sandybridge() -> InstDecoder {
        westmere()
            .with_avx()
    }

    /// `Ivy Bridge` was the successor to `Sandy Bridge`, launched in 2012. it added F16C
    /// extensions for 16-bit floating point conversion, and the RDRAND instruction.
    pub fn ivybridge() -> InstDecoder {
        sandybridge()
            .with_f16c()
            .with_rdrand()
    }

    /// `Haswell` was the successor to `Ivy Bridge`, launched in 2013. it added several instruction
    /// set extensions: AVX2, BMI1, BMI2, ABM, and FMA3.
    pub fn haswell() -> InstDecoder {
        ivybridge()
            .with_bmi1()
            .with_bmi2()
            .with_abm()
            .with_fma3()
            .with_avx2()
    }

    /// `Haswell-EX` was a variant of `Haswell` launched in 2015 with functional TSX. these cores
    /// were shipped as `E7-48xx/E7-88xx v3` models of processors.
    pub fn haswell_ex() -> InstDecoder {
        haswell()
            .with_tsx()
    }

    /// `Broadwell` was the successor to `Haswell`, launched in late 2014. it added ADX, RDSEED,
    /// and PREFETCHW, as well as broadly rolling out TSX. TSX is enabled on this decoder because
    /// some chips of this microarchitecture rolled out with TSX, and lack of TSX seems to be
    /// reported as an errata (for example, the `Broadwell-Y` line of parts).
    pub fn broadwell() -> InstDecoder {
        haswell_ex()
            .with_adx()
            .with_rdseed()
            .with_prefetchw()
    }

    /// `Skylake` was the successor to `Broadwell`, launched in mid 2015. it added MPX and SGX
    /// extensions, as well as a mixed rollout of AVX512 in different subsets for different product
    /// lines.
    ///
    /// AVX512 is not enabled on this decoder by default because there doesn't seem to be a lowest
    /// common denominator: if you want a `Skylake` decoder with AVX512, something like the
    /// following:
    /// ```
    /// x86_64::real_mode::uarch::intel::skylake()
    ///     .with_avx512_f()
    ///     .with_avx512_dq();
    /// ```
    /// is likely your best option.
    pub fn skylake() -> InstDecoder {
        broadwell()
            .with_mpx()
            .with_sgx()
    }

    /// `Kaby Lake` was the successor to `Sky Lake`, launched in 2016. it adds no extensions to
    /// x86_64 implementaiton beyond `skylake`.
    pub fn kabylake() -> InstDecoder {
        skylake()
    }
    // ice lake is shipping so that should probably be included...
}
