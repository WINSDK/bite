use std::time::{Instant, Duration};
use nix::sys::signal::{self, Signal};

fn main() {
    let now = Instant::now();

    // Stop and notify debugger.
    signal::raise(Signal::SIGTRAP).unwrap();

    // Check whether the interrupt caused the time elapsed to be another 300ms (10ms margin).
    if now.elapsed() < Duration::from_millis(290) {
        panic!("Pause/Continue didn't go through {:?}", now.elapsed());
    }
}
