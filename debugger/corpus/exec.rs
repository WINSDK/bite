use std::ffi::CString;

fn main() {
    // Spawn 10 threads that do nothing.
    for _ in 0..10 {
        std::thread::spawn(std::thread::park);
    }

    // Spawn the 11th thread to execute /bin/echo.
    let exec_handle = std::thread::spawn(|| {
        let prog = CString::new("/bin/echo").unwrap();
        let arg = CString::new("1").unwrap();
        nix::unistd::execv(&prog, &[prog.clone(), arg]).unwrap();
        unsafe { nix::libc::_exit(1); }
    });

    // Wait for the 11th thread (executing command).
    exec_handle.join().unwrap();
}
