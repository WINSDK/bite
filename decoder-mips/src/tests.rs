#![cfg(test)]

use decoder::{Decodable, ToTokens};

macro_rules! eq {
    ([$($bytes:tt),+] => $instruction:literal) => {{
        let mut reader = decoder::Reader::new(&[$($bytes),+]);
        let mut line = decoder::TokenStream::new();
        let decoder = crate::Decoder::default();
        let decoded = match decoder.decode(&mut reader) {
            Ok(inst) => {
                inst.tokenize(&mut line);
                line.to_string()
            }
            Err(err) => format!("{err:?}")
        };

        assert_eq!(decoded, $instruction);
    }};
}

#[test]
fn jump() {
    eq!([0x9, 0, 0, 0] => "j 0x0");
}

#[test]
fn beq() {
    eq!([0x11, 0x2a, 0x10, 0x0] => "beq t1, t2, 0x1000");
}

#[test]
fn sll() {
    eq!([0x0, 0xa, 0x4c, 0x80] => "sll t1, t2, 0x12");
}

#[test]
fn sllv() {
    eq!([0x1, 0x49, 0x48, 0x4] => "sllv t1, t1, t2");
}

#[test]
fn lb() {
    eq!([0x81, 0x49, 0x0, 0x10] => "lb t1, t2, 0x10");
}
