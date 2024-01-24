mod progress;

use egui::text::LayoutJob;
pub use progress::ProgressBar;
pub use rfd::{MessageDialog, MessageLevel};
use std::sync::Mutex;

pub static PROGRESS: ProgressBar = ProgressBar::unset();

/// Time a given expression.
#[macro_export]
macro_rules! time {
    ($e:expr) => {{
        let now = std::time::Instant::now();
        let result = $e;
        $crate::complex!(
            w "[timing] ",
            w std::file!(),
            w ":",
            g std::line!().to_string(),
            w ":",
            g std::column!().to_string(),
            w " took ",
            y format!("{:?}", now.elapsed()),
            w "."
        );
        result
    }};
}

#[macro_export]
macro_rules! exit {
    () => {{
        $crate::MessageDialog::new()
            .set_level($crate::MessageLevel::Error)
            .show();

        std::process::exit(0);
    }};

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        let args: String = format!($($arg)*);
        $crate::MessageDialog::new()
            .set_title("Error")
            .set_description(&args)
            .set_level($crate::MessageLevel::Error)
            .show();

        std::process::exit(0);
    }};
}

#[macro_export]
macro_rules! error {
    () => {{
        eprintln!("Error occurred.");

        let args: String = format!($($arg)*);
            $crate:::MessageDialog::new()
                .set_title("Error")
                .set_description(&args)
                .set_level($crate::MessageLevel::Error)
                .show();

        #[cfg(debug_assertion)]
        unsafe { std::arch::asm!("int3") }
        std::process::exit(1);
    }};

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);

        let args: String = format!($($arg)*);
        $crate::MessageDialog::new()
            .set_title("Error")
            .set_description(&args)
            .set_level($crate::MessageLevel::Error)
            .show();

        #[cfg(debug_assertion)]
        unsafe { std::arch::asm!("int3") }
        std::process::exit(1);
    }};
}

#[macro_export]
macro_rules! warning {
    () => {{
        eprintln!("Warning occurred.");

        let args: String = format!($($arg)*);
        $crate::MessageDialog::new()
            .set_title("Warning")
            .set_description("Warning occurred.")
            .set_level($crate::MessageLevel::Warning)
            .show();
    }};

    ($($arg:tt)*) => {{
        eprintln!($($arg)*);

        let args: String = format!($($arg)*);
        $crate::MessageDialog::new()
            .set_title("Warning")
            .set_description(&args)
            .set_level($crate::MessageLevel::Warning)
            .show();
    }};
}

pub enum Color {
    Red,
    Green,
    Blue,
    Yellow,
    White,
    Gray,
}

#[macro_export]
macro_rules! trace {
    () => {};

    ($($arg:tt)*) => {{
        $crate::LOGGER.lock().unwrap().append(
            format!($($arg)*) + "\n",
            $crate::Color::White,
        );
    }};
}

/// Internal macro, don't use.
#[macro_export]
macro_rules! complex_recurse {
    (r $arg:expr $(,)?) => {
        $crate::LOGGER.lock().unwrap().append(
            $arg,
            $crate::Color::Red,
        );
    };

    (r $arg:expr, $($args:tt)+) => {{
        $crate::complex_recurse!(r $arg,);
        $crate::complex_recurse!($($args)+);
    }};

    (g $arg:expr $(,)?) => {
        $crate::LOGGER.lock().unwrap().append(
            $arg,
            $crate::Color::Green,
        );
    };

    (g $arg:expr, $($args:tt)+) => {{
        $crate::complex_recurse!(g $arg,);
        $crate::complex_recurse!($($args)+);
    }};

    (b $arg:expr $(,)?) => {
        $crate::LOGGER.lock().unwrap().append(
            $arg,
            $crate::Color::Blue,
        );
    };

    (b $arg:expr, $($args:tt)+) => {{
        $crate::complex_recurse!(b $arg,);
        $crate::complex_recurse!($($args)+);
    }};

    (y $arg:expr $(,)?) => {
        $crate::LOGGER.lock().unwrap().append(
            $arg,
            $crate::Color::Yellow,
        );
    };

    (y $arg:expr, $($args:tt)+) => {{
        $crate::complex_recurse!(y $arg,);
        $crate::complex_recurse!($($args)+);
    }};

    (w $arg:expr $(,)?) => {
        $crate::LOGGER.lock().unwrap().append(
            $arg,
            $crate::Color::White,
        );
    };

    (w $arg:expr, $($args:tt)+) => {{
        $crate::complex_recurse!(w $arg,);
        $crate::complex_recurse!($($args)+);
    }};
}

/// Multi-color logging.
#[macro_export]
macro_rules! complex {
    () => {
        $crate::LOGGER.lock().unwrap().append(
            "\n".into(),
            $crate::Color::White,
        );
    };

    (r $arg:expr $(,)?) => {
        $crate::LOGGER.lock().unwrap().append(
            $arg,
            $crate::Color::Red,
        );
    };

    (r $arg:expr, $($args:tt)+) => {{
        $crate::complex!(r $arg,);
        $crate::complex_recurse!($($args)+);

        $crate::LOGGER.lock().unwrap().append(
            "\n",
            $crate::Color::White,
        );
    }};

    (g $arg:expr $(,)?) => {
        $crate::LOGGER.lock().unwrap().append(
            $arg,
            $crate::Color::Green,
        );
    };

    (g $arg:expr, $($args:tt)+) => {{
        $crate::complex!(g $arg,);
        $crate::complex_recurse!($($args)+);

        $crate::LOGGER.lock().unwrap().append(
            "\n",
            $crate::Color::Green,
        );
    }};

    (b $arg:expr $(,)?) => {
        $crate::LOGGER.lock().unwrap().append(
            $arg,
            $crate::Color::Blue,
        );
    };

    (b $arg:expr, $($args:tt)+) => {{
        $crate::complex!(b $arg,);
        $crate::complex_recurse!($($args)+);

        $crate::LOGGER.lock().unwrap().append(
            "\n",
            $crate::Color::White,
        );
    }};

    (y $arg:expr $(,)?) => {
        $crate::LOGGER.lock().unwrap().append(
            $arg,
            $crate::Color::Yellow,
        );
    };

    (y $arg:expr, $($args:tt)+) => {{
        $crate::complex!(y $arg,);
        $crate::complex_recurse!($($args)+);

        $crate::LOGGER.lock().unwrap().append(
            "\n",
            $crate::Color::White,
        );
    }};

    (w $arg:expr $(,)?) => {
        $crate::LOGGER.lock().unwrap().append(
            $arg,
            $crate::Color::White,
        );
    };

    (w $arg:expr, $($args:tt)+) => {{
        $crate::complex!(w $arg,);
        $crate::complex_recurse!($($args)+);

        $crate::LOGGER.lock().unwrap().append(
            "\n",
            $crate::Color::White,
        );
    }};
}

pub static LOGGER: Mutex<Logger<1000>> = Mutex::new(Logger::new());

type Segment = (String, Color);

pub struct Logger<const N: usize> {
    segments: [Segment; N],
    head: usize,
    len: usize,
}

impl<const N: usize> Logger<N> {
    const fn new() -> Self {
        const EMPTY_SEGMENT: Segment = (String::new(), Color::White);

        Self {
            segments: [EMPTY_SEGMENT; N],
            head: 0,
            len: 0,
        }
    }

    pub fn append(&mut self, line: impl Into<String>, color: Color) {
        let line = line.into();

        // if a line of the same length is being appended
        if self.segments[self.head.saturating_sub(1)].0.len() == line.len() {
            if self.segments[self.head.saturating_sub(1)].0 == line {
                return;
            }
        }

        self.segments[self.head] = (line, color);
        self.head = (self.head + 1) % N;
        self.len += 1;
    }

    pub fn clear(&mut self) {
        self.len = 0;
        self.head = 0;
    }

    fn segments(&self) -> impl Iterator<Item = &Segment> {
        let (a, b) = if self.len < N {
            (Default::default(), &self.segments[..self.len])
        } else {
            // wrapped around, so we need to return two slices
            self.segments.split_at(self.head)
        };

        b.iter().chain(a)
    }

    pub fn format(&self) -> LayoutJob {
        let mut layout = LayoutJob::default();

        for (line, color) in self.segments() {
            layout.append(
                line,
                0.0,
                egui::TextFormat {
                    font_id: egui::FontId {
                        size: 12.0,
                        family: egui::FontFamily::Monospace,
                    },
                    color: match color {
                        Color::Green => egui::Color32::LIGHT_GREEN,
                        Color::Red => egui::Color32::RED,
                        Color::Blue => egui::Color32::from_rgb(0x3e, 0xbc, 0xe6),
                        Color::Yellow => egui::Color32::GOLD,
                        Color::White => egui::Color32::WHITE,
                        Color::Gray => egui::Color32::GRAY,
                    },
                    ..Default::default()
                },
            );
        }

        layout
    }
}
