#![cfg(test)]

use decoder::{Decodable, ToTokens};

fn test_display(bytes: &[u8], str: &str) {
    let mut reader = decoder::Reader::new(bytes);
    let mut line = decoder::TokenStream::new();
    let symbols = symbols::Index::default();
    let decoder = crate::Decoder::default();

    let decoded = match decoder.decode(&mut reader) {
        Ok(inst) => {
            inst.tokenize(&mut line, &symbols);
            line.to_string()
        }
        Err(err) => format!("{err:?}"),
    };

    assert_eq!(decoded, str);
}

#[test]
fn jump() {
    test_display(&[0x9, 0, 0, 0], "j 0x0");
}

#[test]
fn beq() {
    test_display(&[0x11, 0x2a, 0x10, 0x0], "beq t1, t2, 0x1000");
}

#[test]
fn sll() {
    test_display(&[0x0, 0xa, 0x4c, 0x80], "sll t1, t2, 0x12");
}

#[test]
fn sllv() {
    test_display(&[0x1, 0x49, 0x48, 0x4], "sllv t1, t1, t2");
}

#[test]
fn lb() {
    test_display(&[0x81, 0x49, 0x0, 0x10], "lb t1, t2, 0x10");
}
