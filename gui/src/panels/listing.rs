use super::common::{FONT, tokens_to_layoutjob};

use std::sync::Arc;
use egui::text::LayoutJob;

pub struct Listing {
    pub disassembly: Arc<disassembler::Disassembly>,
    pub disassembly_view: disassembler::DisassemblyView,
    instructions: LayoutJob,
    instruction_count: usize,
    min_row: usize,
    max_row: usize,
}

impl Listing {
    pub fn new(disassembly: Arc<disassembler::Disassembly>) -> Self {
        Self {
            disassembly,
            disassembly_view: disassembler::DisassemblyView::new(),
            instructions: LayoutJob::default(),
            instruction_count: 0,
            min_row: 0,
            max_row: 0
        }
    }

    /// Force refresh listing.
    pub fn update(&mut self) {
        let instructions = self.disassembly_view.format();
        self.instructions = tokens_to_layoutjob(instructions);
    }
}

impl super::Display for Listing {
    fn show(&mut self, ui: &mut egui::Ui) {
        if !self.disassembly_view.no_code() {
            if let Some(text) = self.disassembly.section(self.disassembly_view.addr()) {
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
        let row_height_with_spacing = FONT.size + spacing.y;
        let area = egui::ScrollArea::both()
            .auto_shrink([false, false])
            .drag_to_scroll(false)
            .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysHidden);

        area.show_viewport(ui, |ui, viewport| {
            let min_row = (viewport.min.y / row_height_with_spacing).floor() as usize;
            let max_row = (viewport.max.y / row_height_with_spacing).ceil() as usize + 1;

            let y_min = ui.max_rect().top() + min_row as f32 * row_height_with_spacing;
            let y_max = ui.max_rect().top() + max_row as f32 * row_height_with_spacing;

            ui.set_height(row_height_with_spacing * self.instruction_count as f32 - spacing.y);

            let rect = egui::Rect::from_x_y_ranges(ui.max_rect().x_range(), y_min..=y_max);

            ui.allocate_ui_at_rect(rect, |ui| {
                ui.skip_ahead_auto_ids(min_row);

                if (min_row..max_row) != (self.min_row..self.min_row) {
                    let row_count = max_row - min_row;
                    self.disassembly_view.set_max_lines(row_count * 2, &self.disassembly);

                    // initial rendering of listing
                    if min_row == 0 {
                        self.update();
                        self.min_row = min_row;
                        self.max_row = max_row;
                    }
                }

                if min_row != self.min_row {
                    if min_row > self.min_row {
                        let row_diff = min_row - self.min_row;
                        self.disassembly_view.scroll_down(&self.disassembly, row_diff);
                    }

                    if min_row < self.min_row {
                        let row_diff = self.min_row - min_row;
                        self.disassembly_view.scroll_up(&self.disassembly, row_diff);
                    }

                    self.update();
                    self.min_row = min_row;
                    self.max_row = max_row;
                }

                ui.label(self.instructions.clone());
            });
        });
    }
}
