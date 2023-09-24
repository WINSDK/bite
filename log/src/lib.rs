use std::sync::Mutex;

use egui::text::LayoutJob;
use once_cell::sync::Lazy;

pub enum Color {
    Green,
    Red,
    Gold,
    Gray,
}

#[macro_export]
macro_rules! notify {
    () => {};

    ($($arg:tt)*) => {{
        let mut logger = $crate::LOGGER.lock().unwrap();

        logger.append(
            format!(
                $($arg)*
            ),
            $crate::Color::Green,
        );
    }};
}

#[macro_export]
macro_rules! strong {
    () => {};

    ($($arg:tt)*) => {{
        let mut logger = $crate::LOGGER.lock().unwrap();

        logger.append(
            format!(
                $($arg)*
            ),
            $crate::Color::Red,
        );
    }};
}

#[macro_export]
macro_rules! warn {
    () => {};

    ($($arg:tt)*) => {{
        let mut logger = $crate::LOGGER.lock().unwrap();

        logger.append(
            format!(
                $($arg)*
            ),
            $crate::Color::Gold,
        );
    }};
}

#[macro_export]
macro_rules! trace {
    () => {};

    ($($arg:tt)*) => {{
        let mut logger = $crate::LOGGER.lock().unwrap();

        logger.append(
            format!(
                $($arg)*
            ),
            $crate::Color::Gray,
        );
    }};
}

pub static LOGGER: Lazy<Mutex<Logger<300>>> = Lazy::new(|| Mutex::new(Logger::new()));

pub struct Logger<const N: usize> {
    lines: [(String, Color); N],
    head: usize,
    len: usize,
}

impl<const N: usize> Logger<N> {
    fn new() -> Self {
        Self {
            lines: std::array::from_fn(|_| (String::new(), Color::Gray)),
            head: 0,
            len: 0,
        }
    }

    pub fn append(&mut self, line: String, color: Color) {
        self.lines[self.head] = (line + "\n", color);
        self.head = (self.head + 1) % N;
        self.len += 1;
    }

    pub fn clear(&mut self) {
        self.head = 0;
    }

    fn lines(&self) -> (&[(String, Color)], &[(String, Color)]) {
        if self.len < N {
            (&self.lines[0..self.len], &[])
        } else {
            // wrapped around, so we need to return two slices
            let (a, b) = self.lines.split_at(self.head);

            (b, a)
        }
    }

    pub fn format(&self) -> LayoutJob {
        let mut layout = LayoutJob::default();
        let lines = self.lines();

        for (line, color) in lines.0.iter().chain(lines.1) {
            layout.append(
                &line,
                0.0,
                egui::TextFormat {
                    font_id: egui::FontId {
                        size: 12.0,
                        family: egui::FontFamily::Monospace,
                    },
                    color: match color {
                        Color::Green => egui::Color32::LIGHT_GREEN,
                        Color::Red => egui::Color32::RED,
                        Color::Gold => egui::Color32::GOLD,
                        Color::Gray => egui::Color32::LIGHT_GRAY,
                    },
                    ..Default::default()
                },
            );
        }

        layout
    }
}
