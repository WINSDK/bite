use nix::sys::signal::{self, Signal};

#[inline(never)]
fn some_function() -> usize {
    (0..1000).map(|x| x * 2).sum()
}

fn main() {
    // Stop and notify debugger.
    signal::raise(Signal::SIGTRAP).unwrap();

    some_function();
}
