pub const FONT: egui::FontId = egui::FontId::new(14.0, egui::FontFamily::Monospace);

pub struct Timer {
    start: std::time::Instant,
    ups: usize,
}

impl Timer {
    pub fn new(ups: usize) -> Self {
        Self {
            start: std::time::Instant::now(),
            ups,
        }
    }

    pub fn times_elapsed(&self) -> usize {
        self.start.elapsed().as_millis() as usize * self.ups / 1000
    }

    pub fn reset(&mut self) {
        self.start = std::time::Instant::now();
    }
}

pub trait Display {
    fn show(&mut self, ui: &mut egui::Ui);
}

pub fn tokens_to_layoutjob(tokens: Vec<tokenizing::Token>) -> egui::text::LayoutJob {
    let mut job = egui::text::LayoutJob::default();

    for token in tokens {
        job.append(
            &token.text,
            0.0,
            egui::TextFormat {
                font_id: FONT,
                color: token.color,
                ..Default::default()
            },
        );
    }

    job
}
