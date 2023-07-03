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

        // return a ! as this macro never returns
        loop {}
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

        // return a ! as this macro never returns
        loop {}
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

        // return a ! as this macro never returns
        loop {}
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

        // return a ! as this macro never returns
        loop {}
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

        // return a ! as this macro never returns
        loop {}
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

        // return a ! as this macro never returns
        loop {}
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
