// use crate::common::*;
use egui::TextEdit;
use egui::text::LayoutJob;
use egui::text::{CCursor, CCursorRange};

pub struct TextSelection {
    pub layout: LayoutJob,
    cursor_position: usize,
}

impl TextSelection {
    pub fn new() -> Self {
        Self { layout: LayoutJob::default(), cursor_position: 0 }
    }

    pub fn append(&mut self, text: &str, format: &egui::TextFormat) {
        self.layout.append(text, 0.0, format.clone());
    }

    pub fn set_cursor_position(&mut self, position: usize) {
        self.cursor_position = position;
    }

    pub fn show(mut self, ui: &mut egui::Ui, reset_cursor: bool) {
        let mut layout = self.layout.clone();
        let mut layouter = |ui: &egui::Ui, _s: &str, wrap_width: f32| {
            // let mut layout_job: egui::text::LayoutJob = my_memoized_highlighter(string);
            layout.wrap.max_width = wrap_width;

            ui.fonts(|f| f.layout_job(layout.clone()))
        };

        let response = ui.add(
            TextEdit::multiline(&mut self.layout.text)
                // .font(egui::TextStyle::Monospace) // for cursor height
                // .code_editor()
                .desired_width(f32::INFINITY)
                .layouter(&mut layouter),
        );

        let text_edit_id = response.id;
        if let Some(mut state) = TextEdit::load_state(ui.ctx(), text_edit_id) {
            if reset_cursor {
                let ccursor = CCursor::new(self.cursor_position);
                state.set_ccursor_range(Some(CCursorRange::one(ccursor)));
                state.store(ui.ctx(), text_edit_id);
                // give focus back to the `TextEdit`.
                ui.ctx().memory_mut(|m| m.request_focus(text_edit_id));
            }
        }
    }
}
