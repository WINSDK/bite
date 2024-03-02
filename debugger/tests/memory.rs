use debugger::*;
use processor::Processor;
use std::sync::Arc;
use std::sync::Barrier;

mod build;

#[test]
fn read_process() -> Result<(), Error> {
    let bin_path = build!("./corpus/hello_world.rs");
    let processor = Arc::new(Processor::parse(bin_path).unwrap());

    let settings = DebuggerSettings {
        tracing: false,
        follow_children: false,
        env: Vec::<&str>::new(),
    };
    let desc = DebuggerDescriptor {
        args: Vec::new(),
        module: Arc::clone(&processor),
    };

    let debugger = Debugger::new(settings, desc);
    let barrier = Arc::new(Barrier::new(2));
    let thread = {
        let barrier = Arc::clone(&barrier);
        let mut debugger = debugger.clone();
        std::thread::spawn(move || {
            debugger.spawn().unwrap();
            barrier.wait();
            debugger.trace().unwrap();
        })
    };
    barrier.wait();

    let mut bytes = vec![b'0'; "Hello, world!\n".len()];
    let read_text = {
        let mut debugger = debugger.lock();
        let process = debugger.processes().next().expect("No processes");

        let sym_addr = processor.index.get_by_name("hello_world::MSG").expect("Symbol missing");
        let sym_addr = process.trans(sym_addr);

        unsafe {
            let mut ptr = 0usize;
            ReadMemory::new(&process)
                .read_ptr(&mut ptr, sym_addr)
                .apply()
                .unwrap();

            let ptr = process.trans(ptr);

            ReadMemory::new(&process)
                .read_slice(&mut bytes, ptr)
                .apply()
                .unwrap();

            std::str::from_utf8(&bytes).expect("Invalid utf-8 read")
        }
    };

    assert_eq!(read_text, "Hello, world!\n", "Misread text");
    thread.join().unwrap();
    Ok(())
}
