#[macro_export]
macro_rules! exit {
    () => {
        std::process::exit(0)
    };

    ($($arg:tt)*) => {{
        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            rfd::MessageDialog::new()
                .set_title("Bite failed at runtime")
                .set_description(format!($($arg)*).as_str())
                .set_parent(&*window)
                .show();
        }

        eprintln!($($arg)*);
        std::process::exit(0);
    }};
}

#[macro_export]
macro_rules! fail {
    () => {
        unsafe { std::arch::asm!("int3") }
        std::process::exit(1)
    };

    ($($arg:tt)*) => {{
        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            rfd::MessageDialog::new()
                .set_title("Bite failed at runtime")
                .set_description(format!($($arg)*).as_str())
                .set_parent(&*window)
                .show();
        }

        eprintln!($($arg)*);
        unsafe { std::arch::asm!("int3") }
        std::process::exit(1)
    }};
}

#[macro_export]
macro_rules! warning {
    () => {
        eprintln!("Unknown warning occurred.");
    };

    ($($arg:tt)*) => {{
        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            rfd::MessageDialog::new()
                .set_title("Bite Warning")
                .set_description(format!($($arg)*).as_str())
                .set_parent(&*window)
                .show();
        }

        eprintln!($($arg)*);
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
