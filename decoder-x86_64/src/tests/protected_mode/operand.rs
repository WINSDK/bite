use crate::protected_mode::{InstDecoder, Operand, RegSpec};
use crate::MemoryAccessSize;

#[test]
fn register_widths() {
    assert_eq!(Operand::Register(RegSpec::esp()).width(), Some(4));
    assert_eq!(Operand::Register(RegSpec::sp()).width(), Some(2));
    assert_eq!(Operand::Register(RegSpec::cl()).width(), Some(1));
    assert_eq!(Operand::Register(RegSpec::ch()).width(), Some(1));
    assert_eq!(Operand::Register(RegSpec::gs()).width(), Some(2));
}

#[test]
fn memory_widths() {
    // the register operand directly doesn't report a size - it comes from the `Instruction` for
    // which this is an operand.
    assert_eq!(Operand::RegDeref(RegSpec::esp()).width(), None);

    fn mem_size_of(data: &[u8]) -> MemoryAccessSize {
        let decoder = InstDecoder::default();
        decoder.decode_slice(data).unwrap().mem_size().unwrap()
    }

    // and checking the memory size direcly reports correct names
    assert_eq!(mem_size_of(&[0x32, 0x00]).size_name(), "byte");
    assert_eq!(mem_size_of(&[0x66, 0x33, 0x00]).size_name(), "word");
    assert_eq!(mem_size_of(&[0x33, 0x00]).size_name(), "dword");
}

#[test]
fn test_implied_memory_width() {
    fn mem_size_of(data: &[u8]) -> Option<u8> {
        let decoder = InstDecoder::default();
        decoder.decode_slice(data).unwrap().mem_size().unwrap().bytes_size()
    }

    // test push, pop, call, and ret
    assert_eq!(mem_size_of(&[0xc3]), Some(4));
    assert_eq!(mem_size_of(&[0xe8, 0x11, 0x22, 0x33, 0x44]), Some(4));
    assert_eq!(mem_size_of(&[0x50]), Some(4));
    assert_eq!(mem_size_of(&[0x58]), Some(4));
    assert_eq!(mem_size_of(&[0x66, 0x50]), Some(4));
    assert_eq!(mem_size_of(&[0x66, 0x58]), Some(4));
    assert_eq!(mem_size_of(&[0xff, 0xf0]), Some(4));
    assert_eq!(mem_size_of(&[0x66, 0xff, 0xf0]), Some(2));
    // unlike 64-bit mode, operand-size prefixed call and jump do have a different size: they read
    // two bytes.
    assert_eq!(mem_size_of(&[0x66, 0xff, 0x10]), Some(2));
    assert_eq!(mem_size_of(&[0x66, 0xff, 0x20]), Some(2));
}
