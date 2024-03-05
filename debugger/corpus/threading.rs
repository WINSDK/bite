fn main() {
    let threads: Vec<_> = (0..10)
        .map(|idx| {
            std::thread::spawn(move || {
                std::thread::sleep(std::time::Duration::from_millis(100));
                println!("thread {idx} finished");
            })
        })
        .collect();

    for thread in threads {
        thread.join().unwrap();
    }
}
