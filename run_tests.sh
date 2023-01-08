export RUSTFLAGS="-Awarnings"
TARGETS="mips-unknown-none x86_64-unknown-none"

mkdir -p target

for TARGET in $TARGETS; do
    find tests -type f | while read file; do
        source $file

        exit_code=$?
        if [ $exit_code -ne 0 ]; then
            printf "test '$file' failed with exit code $exit_code\n\n"
        fi
    done
done
