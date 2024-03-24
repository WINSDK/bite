use std::sync::Arc;

use crate::common::*;
use crate::widgets::{ProcessorView, TextSelection};
use egui::text::LayoutJob;
use processor::Processor;
use processor_shared::PhysAddr;

pub struct Listing {
    pub processor_view: ProcessorView,
    processor: Arc<Processor>,
    lines: LayoutJob,
    min_row: usize,
    max_row: usize,
}

impl Listing {
    pub fn new(processor: Arc<Processor>) -> Self {
        Self {
            processor_view: ProcessorView::new(),
            processor,
            lines: LayoutJob::default(),
            min_row: 0,
            max_row: 0,
        }
    }

    /// Force refresh listing.
    /// Force refresh listing.
    pub fn update(&mut self) {
        let instructions = self.processor_view.format();
        self.lines = tokens_to_layoutjob(instructions);
    }

    #[allow(dead_code)]
    pub fn jump(&mut self, addr: PhysAddr) -> bool {
        self.processor_view.jump(&self.processor, addr)
    }
}

impl Display for Listing {
    fn show(&mut self, ui: &mut egui::Ui) {
        if !self.processor_view.no_code() {
            if let Some(text) = self.processor.section_name(self.processor_view.addr()) {
                let max_width = ui.available_width();
                let size = egui::vec2(9.0 * text.len() as f32, 25.0);
                let offset = egui::pos2(20.0, 60.0);
                let rect = egui::Rect::from_two_pos(
                    egui::pos2(max_width - offset.x, offset.y),
                    egui::pos2(max_width - offset.x - size.x, offset.y + size.y),
                );

                ui.painter().rect(
                    rect.expand2(egui::vec2(5.0, 0.0)),
                    0.0,
                    tokenizing::colors::GRAY35,
                    egui::Stroke::new(2.5, egui::Color32::BLACK),
                );

                ui.painter().text(
                    rect.center(),
                    egui::Align2::CENTER_CENTER,
                    text,
                    FONT,
                    egui::Color32::WHITE,
                );
            }
        }

        let spacing = ui.spacing().item_spacing;
        let row_height = FONT.size + spacing.y;

        let area = egui::ScrollArea::vertical()
            .auto_shrink(false)
            .drag_to_scroll(false)
            .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysHidden);

        area.show_viewport(ui, |ui, viewport| {
            let min_row = (viewport.min.y / row_height).floor() as usize;
            let max_row = (viewport.max.y / row_height).ceil() as usize + 1;

            let y_min = ui.max_rect().top() + min_row as f32 * row_height;
            let y_max = ui.max_rect().top() + max_row as f32 * row_height;
            let row_change = usize::abs_diff(self.max_row - self.min_row, max_row - min_row);

            let rect = egui::Rect::from_x_y_ranges(ui.max_rect().x_range(), y_min..=y_max);

            ui.allocate_ui_at_rect(rect, |ui| {
                ui.skip_ahead_auto_ids(min_row);

                if row_change > 1 {
                    self.processor_view.set_max_lines(max_row - min_row, &self.processor);
                    self.update();
                    self.min_row = min_row;
                    self.max_row = max_row;
                    return;
                }

                if min_row < self.min_row {
                    self.processor_view.scroll_up(&self.processor, self.min_row - min_row);
                    self.update();
                    self.min_row = min_row;
                    self.max_row = max_row;
                }

                if min_row > self.min_row {
                    self.processor_view.scroll_down(&self.processor, min_row - self.min_row);
                    self.update();
                    self.min_row = min_row;
                    self.max_row = max_row;
                }

                let text_area = TextSelection::precomputed(&self.lines);
                ui.add(text_area);
            });
        });
    }
}
