#!/bin/sh

TARGET="mips-unknown-none"
OPTS="-C link-arg=--entry='_start' -C panic=abort -C opt-level=z -C lto=fat"

cat << EOF | rustc -o target/test --crate-type=bin --target $TARGET $OPTS -
#![no_std]
#![no_main]

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    unsafe { core::hint::unreachable_unchecked() }
}

#[no_mangle]
pub fn _start() {
    for _ in 0..128 {
        unsafe { core::ptr::write_volatile(0x1000000 as *mut _, 0) }
    }
}
EOF
