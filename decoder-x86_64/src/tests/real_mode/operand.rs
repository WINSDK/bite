use crate::real_mode::{InstDecoder};

#[test]
fn test_implied_memory_width() {
    fn mem_size_of(data: &[u8]) -> Option<u8> {
        let decoder = InstDecoder::default();
        decoder.decode_slice(data).unwrap().mem_size().unwrap().bytes_size()
    }

    // test push, pop, call, and ret
    assert_eq!(mem_size_of(&[0xc3]), Some(2));
    assert_eq!(mem_size_of(&[0xe8, 0x11, 0x22, 0x33, 0x44]), Some(2));
    assert_eq!(mem_size_of(&[0x50]), Some(2));
    assert_eq!(mem_size_of(&[0x58]), Some(2));
    assert_eq!(mem_size_of(&[0x66, 0x50]), Some(2));
    assert_eq!(mem_size_of(&[0x66, 0x58]), Some(2));
    assert_eq!(mem_size_of(&[0xff, 0xf0]), Some(2));
    assert_eq!(mem_size_of(&[0x66, 0xff, 0xf0]), Some(4));
    // unlike 64-bit mode, operand-size prefixed call and jump do have a different size: they read
    // four bytes.
    assert_eq!(mem_size_of(&[0x66, 0xff, 0x10]), Some(4));
    assert_eq!(mem_size_of(&[0x66, 0xff, 0x20]), Some(4));
}
