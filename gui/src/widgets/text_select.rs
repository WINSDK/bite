#![allow(dead_code)]

// Example highlighting code:
//
// ```
// #[derive(Default)]
// struct Highlighter {
//     tokens: Vec<Token>,
// }
// pub fn highlight(ctx: &egui::Context, s: &str, tokens: &[Token]) -> LayoutJob {
//     impl egui::util::cache::ComputerMut<(&str, &[Token]), LayoutJob> for Highlighter {
//         fn compute(&mut self, (_, tokens): (&str, &[Token])) -> LayoutJob {
//             let mut output = LayoutJob::default();
//             for token in tokens {
//                 output.append(
//                     &*token.text,
//                     0.0,
//                     egui::TextFormat {
//                         font_id: FONT,
//                         color: token.color,
//                         ..Default::default()
//                     }
//                 );
//             }
//             output
//         }
//     }
//
//     type HighlightCache = egui::util::cache::FrameCache<LayoutJob, Highlighter>;
//
//     ctx.memory_mut(|mem| {
//         mem.caches
//             .cache::<HighlightCache>()
//             .get((s, tokens))
//     })
// }
// ```

use crate::common::*;
use crate::style::EGUI;

use egui::text::LayoutJob;
use egui::text::{CCursor, CCursorRange};
use egui::TextEdit;

type Layouter<'l> = &'l mut dyn FnMut(&str) -> LayoutJob;

pub struct TextSelection<'l> {
    text: String,
    reset_position: Option<usize>,
    layouter: Option<Layouter<'l>>,
    precomputed: Option<&'l LayoutJob>,
}

impl<'l> TextSelection<'l> {
    pub fn new() -> Self {
        Self {
            text: String::new(),
            reset_position: None,
            layouter: None,
            precomputed: None,
        }
    }

    pub fn precomputed(layoutjob: &'l LayoutJob) -> Self {
        let mut this = Self::new();
        this.precomputed = Some(layoutjob);
        this
    }

    pub fn append(&mut self, text: &str) {
        debug_assert!(
            self.precomputed.is_none(),
            "can't append to precomputed text"
        );
        self.text.push_str(text);
    }

    /// Color highlighting for [`TextSelection`].
    ///
    /// Example code for setting a layout:
    /// ```
    /// let mut layouter = |input: &str| {
    ///     let mut output = LayoutJob::default();

    ///     for (idx, s) in input.split_inclusive(' ').enumerate() {
    ///         let color = if idx % 2 == 0 {
    ///             egui::Color32::LIGHT_RED
    ///         } else {
    ///             egui::Color32::LIGHT_BLUE
    ///         };

    ///         output.append(
    ///             s,
    ///             0.0,
    ///             egui::TextFormat {
    ///                 font_id: FONT,
    ///                 color,
    ///                 ..Default::default()
    ///             }
    ///         );
    ///     }
    ///     output
    /// };
    /// text_area.set_layouter(&mut layouter);
    /// ```
    pub fn set_layouter(&mut self, layouter: Layouter<'l>) {
        debug_assert!(
            self.precomputed.is_none(),
            "can't set layout for precomputed text"
        );
        self.layouter = Some(layouter);
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
                    FONT,
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
                .code_editor()
                .desired_width(f32::INFINITY)
                .layouter(&mut layouter),
        );

        let text_edit_id = response.id;
        if let Some(mut state) = TextEdit::load_state(ui.ctx(), text_edit_id) {
            if let Some(position) = self.reset_position {
                let ccursor = CCursor::new(position);
                state.set_ccursor_range(Some(CCursorRange::one(ccursor)));
                state.store(ui.ctx(), text_edit_id);
                // give focus back to the `TextEdit`.
                ui.ctx().memory_mut(|m| m.request_focus(text_edit_id));
            }
        }

        response
    }
}
