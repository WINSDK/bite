use crate::style::EGUI;
use crate::widgets::TextEdit;

use egui::text::LayoutJob;
use egui::text::{CCursor, CCursorRange};
use egui::FontId;

type Layouter<'l> = &'l mut dyn FnMut(&str) -> LayoutJob;

pub struct TextSelection<'l> {
    font: FontId,
    text: String,
    reset_position: Option<usize>,
    layouter: Option<Layouter<'l>>,
    precomputed: Option<&'l LayoutJob>,
}

impl<'l> TextSelection<'l> {
    pub fn new(font: FontId) -> Self {
        Self {
            font,
            text: String::new(),
            reset_position: None,
            layouter: None,
            precomputed: None,
        }
    }

    pub fn precomputed(layoutjob: &'l LayoutJob) -> Self {
        let font = layoutjob.sections.first().map(|s| s.format.font_id.clone()).unwrap_or_default();
        let mut this = Self::new(font);
        this.precomputed = Some(layoutjob);
        this
    }

    pub fn set_reset_position(&mut self, reset_position: usize) {
        self.reset_position = Some(reset_position);
    }
}

impl egui::Widget for TextSelection<'_> {
    fn ui(mut self, ui: &mut egui::Ui) -> egui::Response {
        let mut layouter = |ui: &egui::Ui, s: &str, wrap_width: f32| {
            let layout = match (self.layouter.as_mut(), self.precomputed) {
                (Some(stored_layouter), None) | (Some(stored_layouter), Some(_)) => {
                    let mut layout = stored_layouter(s);
                    layout.wrap.max_width = wrap_width;
                    layout
                }
                (None, Some(precomputed)) => precomputed.clone(),
                (None, None) => LayoutJob::simple(
                    self.text.clone(),
                    self.font.clone(),
                    EGUI.noninteractive().fg_stroke.color,
                    wrap_width,
                ),
            };

            ui.fonts(|f| f.layout_job(layout))
        };

        // cursor requires a TextBuffer to be displayed, I don't know why but
        // that's the reason for the clone
        let mut text = if let Some(LayoutJob { text, .. }) = self.precomputed {
            text.clone()
        } else {
            self.text.clone()
        };

        let response = ui.add(
            TextEdit::multiline(&mut text)
                .font(self.font.clone())
                .lock_focus(true)
                .desired_width(f32::INFINITY)
                .layouter(&mut layouter),
        );

        let text_edit_id = response.id;
        if let Some(mut state) = TextEdit::load_state(ui.ctx(), text_edit_id) {
            if let Some(position) = self.reset_position {
                let ccursor = CCursor::new(position);
                state.set_ccursor_range(Some(CCursorRange::one(ccursor)));
                state.store(ui.ctx(), text_edit_id);
            }
        }

        response
    }
}
