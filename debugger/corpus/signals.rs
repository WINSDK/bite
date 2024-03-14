fn main() {
    let now = std::time::Instant::now();

    // Wait for thread to do it's thing.
    std::thread::sleep(std::time::Duration::from_millis(100));

    dbg!(now.elapsed());
    // Check whether the pause really was 1sec + 100ms warmup (10ms margin).
    if now.elapsed().as_millis() <= 1090 {
        panic!("Pause/Continue didn't go through");
    }
}
