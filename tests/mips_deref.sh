#!/bin/sh

TARGET="mips-unknown-none"

cat <<EOF | clang -Oz -o target/test -target $TARGET -c -xc -
int main() {
    *(int *)0x1000000 = 0;

    return 0;
}
EOF
