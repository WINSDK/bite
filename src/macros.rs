#[macro_export]
macro_rules! exit {
    () => {
        std::process::exit(0)
    };

    (fail) => {
        std::process::exit(1)
    };

    (fail, $($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
    }};

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(0);
    }};
}

#[macro_export]
macro_rules! assert_exit {
    ($cond:expr $(,)?) => {{
        if !($cond) {
            $crate::exit!(fail);
        }
    }};

    ($cond:expr, $($arg:tt)+) => {{
        if !($cond) {
            $crate::exit!($($arg)*);
        }
    }};
}

#[macro_export]
macro_rules! unchecked_println {
    ($($arg:tt)*) => {{
        use std::io::Write;

        let mut stdout = std::io::stdout();
        match stdout.write_fmt(format_args!($($arg)*)) {
            Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => $crate::exit!(),
            _ => {}
        }

        match stdout.write(b"\n") {
            Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => $crate::exit!(),
            _ => {}
        }
    }};
}

#[macro_export]
macro_rules! push_unsafe {
    ($v:expr, $off:expr, $c:expr) => {
        unsafe {
            *$v.get_unchecked_mut($off) = $c;
            $off += 1;
        }
    };
}
