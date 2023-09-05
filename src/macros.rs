#[macro_export]
macro_rules! exit {
    () => {{
        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            rfd::MessageDialog::new()
                .set_level(rfd::MessageLevel::Error)
                .set_parent(&*window)
                .show();
        }

        std::process::exit(0);
    }};

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);

        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            let args: String = format!($($arg)*);
            rfd::MessageDialog::new()
                .set_title("Error")
                .set_description(&args)
                .set_level(rfd::MessageLevel::Error)
                .set_parent(&*window)
                .show();
        }

        std::process::exit(0);
    }};
}

#[macro_export]
macro_rules! error {
    () => {{
        eprintln!("Error occurred.");

        if let Some(window) = $crate::gui::WINDOW.get() {
            let args: String = format!($($arg)*);
                rfd:::MessageDialog::new()
                    .set_title("Error")
                    .set_description(&args)
                    .set_level(rfd::MessageLevel::Error)
                    .set_parent(&*window)
                    .show();
        }

        #[cfg(debug_assertion)]
        unsafe { std::arch::asm!("int3") }
        std::process::exit(1);
    }};

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);

        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            let args: String = format!($($arg)*);
            rfd::MessageDialog::new()
                .set_title("Error")
                .set_description(&args)
                .set_level(rfd::MessageLevel::Error)
                .set_parent(&*window)
                .show();
        }

        #[cfg(debug_assertion)]
        unsafe { std::arch::asm!("int3") }
        std::process::exit(1);
    }};
}

#[macro_export]
macro_rules! warning {
    () => {{
        eprintln!("Warning occurred.");

        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            let args: String = format!($($arg)*);
            rfd::MessageDialog::new()
                .set_title("Warning")
                .set_description(&args)
                .set_level(rfd::MessageLevel::Warning)
                .set_parent(&*window)
                .show();
        }
    }};

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);

        if let Some(window) = $crate::gui::WINDOW.get() {
            let window = std::sync::Arc::clone(&window);

            let args: String = format!($($arg)*);
            rfd::MessageDialog::new()
                .set_title("Warning")
                .set_description(&args)
                .set_level(rfd::MessageLevel::Warning)
                .set_parent(&*window)
                .show();
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
