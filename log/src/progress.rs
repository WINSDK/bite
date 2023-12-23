use std::sync::RwLock;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct ProgressBar {
    desc: RwLock<&'static str>,
    steps_done: AtomicUsize,
    step_count: AtomicUsize,
    size: egui::Vec2,
    bg_col: egui::Color32,
    fg_col: egui::Color32,
}

impl ProgressBar {
    pub const fn unset() -> Self {
        Self {
            desc: RwLock::new("???"),
            steps_done: AtomicUsize::new(0),
            step_count: AtomicUsize::new(0),
            size: egui::vec2(300.0, 18.0),
            bg_col: egui::Color32::from_gray(66),
            fg_col: egui::Color32::from_rgb(0x34, 0x73, 0xcf)
        }
    }

    pub fn set(&self, desc: &'static str, step_count: usize) {
        *self.desc.write().unwrap() = desc;
        self.step_count.store(step_count, Ordering::SeqCst);
        self.steps_done.store(0, Ordering::SeqCst);
    }

    pub fn step(&self) {
        self.steps_done.fetch_add(1, Ordering::Relaxed);
    }

    pub fn step_n(&mut self, n: usize) {
        self.steps_done.fetch_add(n, Ordering::Relaxed);
    }

    pub fn show(&self, ui: &mut egui::Ui) {
        let desc = self.desc.read().unwrap();
        let steps_done = self.steps_done.load(Ordering::Relaxed);
        let step_count = self.step_count.load(Ordering::Relaxed);

        let progress = steps_done as f64 / step_count as f64;
        let progress = progress.clamp(0.0, 1.0) as f32;

        let rect = ui.allocate_exact_size(self.size, egui::Sense::hover()).0;

        // not yet set so don't display text
        if *desc == "???" {
            return;
        }

        let (top_rect, bot_rect) = rect.split_top_bottom_at_fraction(0.5);
        let (bar_rect, circles_rect) = top_rect.split_left_right_at_fraction(0.9);
        let (l, r) = bar_rect.split_left_right_at_fraction(progress);

        let painter = ui.painter();

        // draw background bar
        painter.rect_filled(r, 0.0, self.bg_col);

        let t = ui.input(|i| i.time);
        let r = circles_rect.height() / 2.1;
        let speed = 1.5;
        let offsets = [
            speed * 0.0,
            speed * 0.333,
            speed * 0.666,
        ];

        // draw dots
        for offset in offsets {
            let o = (circles_rect.width() + r) as f64 * (t + offset - speed * ((t + offset) / speed).floor()) / speed;
            let circle_center = egui::Pos2::new(circles_rect.max.x - o as f32, (circles_rect.min.y + circles_rect.max.y) / 2.0);
            painter.circle_filled(circle_center, r, self.bg_col);
        }

        // draw filled bar
        painter.rect_filled(l, 0.0, self.fg_col);

        // draw centered text
        ui.allocate_ui_at_rect(bot_rect, |ui| {
            ui.with_layout(egui::Layout::centered_and_justified(egui::Direction::LeftToRight), |ui| {
                let steps_done = steps_done.clamp(0, step_count);
                let status = format!("{desc} {steps_done}/{step_count}");
                ui.label(egui::RichText::new(status).size(top_rect.height() * 1.3))
            });
        });
    }
}
