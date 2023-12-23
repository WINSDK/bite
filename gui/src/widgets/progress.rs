#![allow(dead_code)]

use crate::common::*;

pub struct ProgressBar {
    desc: &'static str,
    steps_done: usize,
    step_count: usize,
    size: egui::Vec2,
    bg_col: egui::Color32,
    fg_col: egui::Color32,
}

impl ProgressBar {
    pub fn new(desc: &'static str, step_count: usize) -> Self {
        Self {
            desc,
            steps_done: 0,
            step_count,
            size: egui::vec2(300.0,18.0),
            bg_col: egui::Color32::from_gray(66),
            fg_col: egui::Color32::from_rgb(0x34, 0x73, 0xCF)
        }
    }

    pub fn step(&mut self) {
        self.steps_done += 1;
    }

    pub fn step_n(&mut self, n: usize) {
        self.steps_done += n;
    }
}

impl Display for ProgressBar {
    fn show(&mut self, ui: &mut egui::Ui) {
        let progress = self.steps_done as f64 / self.step_count as f64;
        let progress = progress.clamp(0.0, 1.0) as f32;

        let rect = ui.allocate_exact_size(self.size, egui::Sense::hover()).0;
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
                let progress = self.steps_done.clamp(0, self.step_count);
                let status = format!("{} {}/{}", self.desc, progress, self.step_count);
                ui.label(egui::RichText::new(status).size(top_rect.height() * 1.3))
            });
        });
    }
}