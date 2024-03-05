use std::thread;

fn spawn_threads(level: u32, max_depth: u32) {
    if level >= max_depth {
        return;
    }

    let handle1 = thread::spawn(move || {
        println!("Spawned thread at level {}", level + 1);
        spawn_threads(level + 1, max_depth);
    });

    let handle2 = thread::spawn(move || {
        println!("Spawned thread at level {}", level + 1);
        spawn_threads(level + 1, max_depth);
    });

    handle1.join().unwrap();
    handle2.join().unwrap();
}

fn main() {
    let max_depth = 8;
    spawn_threads(0, max_depth);
    println!("All threads spawned up to depth {} have completed.", max_depth);
}
