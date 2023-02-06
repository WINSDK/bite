#!/bin/sh

fname=$(basename $BASH_SOURCE)
fname="${fname%%.*}"
fname="target/$fname-$TARGET"

cat <<EOF | clang -Oz -o $fname -target $TARGET -c -xc -
int main() {
    *(int *)0x1000000 = 0;

    return 0;
}
EOF

cargo run --quiet -- -D $fname > /dev/null
