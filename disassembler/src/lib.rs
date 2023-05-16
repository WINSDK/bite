//! Consumes decoder crates and provides an interface to interact with the decoders.

pub use decoder::{ToTokens, TokenStream};

pub mod x86 {
    pub use x86_64::protected_mode::Decoder;
}

pub mod x64 {
    pub use x86_64::long_mode::Decoder;
}

pub mod riscv {
    pub use riscv::Decoder;
}

pub mod mips {
    pub use mips::Decoder;
}
