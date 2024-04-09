use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::RwLock;
use egui::Spinner;

pub struct ProgressBar {
    desc: RwLock<&'static str>,
    steps_done: AtomicUsize,
    step_count: AtomicUsize,
    size: egui::Vec2,
    bg_col: egui::Color32,
    fg_col: egui::Color32,
}

impl ProgressBar {
    pub const fn new() -> Self {
        Self {
            desc: RwLock::new("???"),
            steps_done: AtomicUsize::new(0),
            step_count: AtomicUsize::new(0),
            size: egui::vec2(300.0, 18.0),
            bg_col: egui::Color32::from_gray(66),
            fg_col: egui::Color32::from_rgb(0x34, 0x73, 0xcf),
        }
    }

    pub fn unset(&self) {
        *self.desc.write().unwrap() = "???";
    }

    pub fn set(&self, desc: &'static str, step_count: usize) {
        // this can't be a valid progress bar
        if step_count == 0 {
            return;
        }

        *self.desc.write().unwrap() = desc;
        self.step_count.store(step_count, Ordering::SeqCst);
        self.steps_done.store(0, Ordering::SeqCst);
    }

    pub fn step(&self) {
        self.steps_done.fetch_add(1, Ordering::Relaxed);
    }

    pub fn step_n(&self, n: usize) {
        self.steps_done.fetch_add(n, Ordering::Relaxed);
    }

    pub fn show(&self, ui: &mut egui::Ui) {
        let desc = self.desc.read().unwrap();
        let steps_done = self.steps_done.load(Ordering::Relaxed);
        let step_count = self.step_count.load(Ordering::Relaxed);

        let progress = steps_done as f64 / step_count as f64;
        let progress = progress.clamp(0.0, 1.0) as f32;

        let rect = ui.allocate_exact_size(self.size, egui::Sense::hover()).0;

        // Not yet set so don't display text.
        if *desc == "???" {
            return;
        }

        let (top_rect, bot_rect) = rect.split_top_bottom_at_fraction(0.5);
        let (bar_rect, spinner_rect) = top_rect.split_left_right_at_fraction(0.95);
        let (l, r) = bar_rect.split_left_right_at_fraction(progress);

        let painter = ui.painter();

        // Draw background bar.
        painter.rect_filled(r, 0.0, self.bg_col);

        // Draw filled bar.
        painter.rect_filled(l, 0.0, self.fg_col);

        // Draw spinner.
        Spinner::new().color(self.fg_col).paint_at(ui, spinner_rect);

        // Draw centered text.
        ui.allocate_ui_at_rect(bot_rect, |ui| {
            ui.with_layout(
                egui::Layout::centered_and_justified(egui::Direction::LeftToRight),
                |ui| {
                    let steps_done = steps_done.clamp(0, step_count);
                    let status = format!("{desc} {steps_done}/{step_count}");
                    ui.label(egui::RichText::new(status).size(top_rect.height() * 1.3))
                },
            );
        });
    }
}
