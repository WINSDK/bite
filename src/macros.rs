#[macro_export]
macro_rules! exit {
    () => {{
        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            std::thread::spawn(move || {
                rfd::MessageDialog::new()
                    .set_level(rfd::MessageLevel::Error)
                    .set_parent(&*window)
                    .show();

                std::process::exit(0);
            });
        } else {
            std::process::exit(0);
        }
    }};

    ($($arg:tt)*) => {{
        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            let args: String = format!($($arg)*);
            std::thread::spawn(move || {
                rfd::MessageDialog::new()
                    .set_title("Error")
                    .set_description(&args)
                    .set_level(rfd::MessageLevel::Error)
                    .set_parent(&*window)
                    .show();

                eprintln!("{args}");
                std::process::exit(0);
            });
        } else {
            eprintln!($($arg)*);
            std::process::exit(0);
        }
    }};
}

#[macro_export]
macro_rules! error {
    () => {{
        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            let args: String = format!($($arg)*);
            std::thread::spawn(move || {
                rfd:::MessageDialog::new()
                    .set_title("Error")
                    .set_description(&args)
                    .set_level(rfd::MessageLevel::Error)
                    .set_parent(&*window)
                    .show();

                eprintln!("Error occurred");
                #[cfg(debug_assertion)]
                unsafe { std::arch::asm!("int3") }
                std::process::exit(1);
            });
        } else {
            eprintln!("Error occurred");
            #[cfg(debug_assertion)]
            unsafe { std::arch::asm!("int3") }
            std::process::exit(j);
        }
    }};

    ($($arg:tt)*) => {{
        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            let args: String = format!($($arg)*);
            std::thread::spawn(move || {
                rfd::MessageDialog::new()
                    .set_title("Error")
                    .set_description(&args)
                    .set_level(rfd::MessageLevel::Error)
                    .set_parent(&*window)
                    .show();

                eprintln!("{args}");
                #[cfg(debug_assertion)]
                unsafe { std::arch::asm!("int3") }
                std::process::exit(1);
            });
        } else {
            eprintln!($($arg)*);
            #[cfg(debug_assertion)]
            unsafe { std::arch::asm!("int3") }
            std::process::exit(1);
        }
    }};
}

#[macro_export]
macro_rules! warning {
    () => {{
        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            let args: String = format!($($arg)*);
            std::thread::spawn(move || {
                rfd::MessageDialog::new()
                    .set_title("Warning")
                    .set_description(&args)
                    .set_level(rfd::MessageLevel::Warning)
                    .set_parent(&*window)
                    .show();

                eprintln!("Warning occurred");
            });
        } else {
            eprintln!("Error occurred");
        }
    }};

    ($($arg:tt)*) => {{
        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            let args: String = format!($($arg)*);
            std::thread::spawn(move || {
                rfd::MessageDialog::new()
                    .set_title("Warning")
                    .set_description(&args)
                    .set_level(rfd::MessageLevel::Warning)
                    .set_parent(&*window)
                    .show();

                eprintln!("{args}");
            });
        } else {
            eprintln!($($arg)*);
        }
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
