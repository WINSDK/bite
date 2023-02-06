export RUSTFLAGS="-Awarnings"
TARGETS="mips-unknown-none riscv64-gc-unknown" # x86_64-unknown-none

mkdir -p target

for TARGET in $TARGETS; do
    find tests -type f -iname "*.sh" | while read file; do
        source $file

        exit_code=$?
        if [ $exit_code -ne 0 ]; then
            printf "test '$fname' failed with exit code $exit_code\n\n"
        else
            printf "test '$fname' succeeded with exit code $exit_code\n"
        fi
    done
done
