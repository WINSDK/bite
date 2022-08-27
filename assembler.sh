#!/bin/sh

read -p "Input assembly instruction: " -r asm

printf "#![feature(naked_functions)]\n#![no_std]\n\n#[no_mangle]\n#[naked]\npub unsafe extern \"C\" fn asm() {\n    core::arch::asm!(\n        \"%s\", \n        options(noreturn)\n    )\n}\n" "$asm" > target/asm.rs

rust_err=$(
    rustc -C panic=abort \
    --crate-type lib \
    -o target/asm \
    target/asm.rs 2>&1 > /dev/null
)

if [ $? -ne 0 ]; then
    cat target/asm.rs
    printf "\n%s" "$rust_err"
    exit $?
fi

objdump -M intel \
    --section=.text.asm \
    -D target/asm \
    | grep -E "\s{3}0:" \
    | sed -E "s/   0:\t/\n/;s/\s{13}//"
