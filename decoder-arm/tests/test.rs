mod armv7;
mod armv8;

use std::fmt::Write;

use decoder::{Decodable, Decoded, Reader, TokenStream, ToTokens};
use symbols::Index;

fn test_range<A: Decodable>(decoder: &A, start: u64, end: u64) {
    let mut stream = TokenStream::new();
    let symbols = Index::default();

    for i in start..=end {
        if i & 0x01_ff_ff_ff == 0 {
            eprintln!("case {:08x}", i);
        }
        let i = i as u32;
        let bytes = i.to_le_bytes();
        if let Ok(inst) = decoder.decode(&mut Reader::new(&bytes)) {
            stream.clear();
            inst.tokenize(&mut stream, &symbols);
        }
    }
}

fn par_test_u32(test_range: fn(u64, u64)) {
    const NR_THREADS: u64 = 512;

    const RANGE_SIZE: u64 = (u32::MAX as u64 + 1) / NR_THREADS;

    let mut handles = Vec::new();

    for i in 0..NR_THREADS {
        let handle = std::thread::spawn(move || test_range(i * RANGE_SIZE, (i + 1) * RANGE_SIZE));
        handles.push(handle);
    }

    while let Some(handle) = handles.pop() {
        handle.join().unwrap();
    }
}

#[test]
#[ignore]
fn test_armv7_does_not_panic() {
    par_test_u32(|start, end| {
        let armv7 = arm::armv7::Decoder::default();
        test_range(&armv7, start, end);
    });
}
#[test]
#[ignore]
fn test_armv7_thumb_does_not_panic() {
    par_test_u32(|start, end| {
        let mut armv7_t = arm::armv7::Decoder::default();
        armv7_t.set_thumb_mode(true);

        test_range(&armv7_t, start, end);
    });
}

#[test]
#[ignore]
fn test_armv8_does_not_panic() {
    par_test_u32(|start, end| {
        let armv8 = arm::armv8::a64::Decoder::default();

        test_range(&armv8, start, end);
    });
}
