//! # `yaxpeax-arm`, a decoder for `arm` instruction sets.
//!
//! `yaxpeax-arm` provides `armv7` (and below) decoders, including `thumb` support, as well a
//! decoder for `armv8`/`a64`.

/// `yaxpeax-arm`'s `ARMv7` decoder and `Arch` implementation.
pub mod armv7;
/// `yaxpeax-arm`'s `ARMv8` decoder and `Arch` implementation.
pub mod armv8;
