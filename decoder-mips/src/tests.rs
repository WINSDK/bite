#![cfg(test)]

use decoder::Streamable;

macro_rules! eq {
    ([$($bytes:tt),+] => $mnemomic:literal, $($operand:literal),*) => {{
        let mut stream = $crate::Stream {
            bytes: &[$($bytes),+],
            offset: 0,
        };

        match stream.next() {
            Some(Ok(inst)) => {
                assert_eq!(inst.mnemomic, $mnemomic);

                let mut idx = 0;
                $(
                    idx += 1;

                    assert_eq!(
                        $operand,
                        inst.operands.get(idx - 1).expect("not enough operands")
                    );
                )*
            }
            Some(Err(e)) => panic!("failed to decode instruction: {e:?}"),
            None => panic!("no bytes to decode"),
        }
    }};
}

#[test]
fn jump() {
    eq!([0x9, 0, 0, 0] => "j", "0x0");
}

#[test]
fn beq() {
    eq!([0x11, 0x2a, 0x10, 0x0] => "beq", "$t1", "$t2", "0x1000");
}

#[test]
fn sll() {
    eq!([0x0, 0xa, 0x4c, 0x80] => "sll", "$t1", "$t2", "0x12");
}

#[test]
fn sllv() {
    eq!([0x1, 0x49, 0x48, 0x4] => "sllv", "$t1", "$t1", "$t2");
}

#[test]
fn lb() {
    eq!([0x81, 0x49, 0x0, 0x10] => "lb", "$t1", "0x10", "($t2)")
}
