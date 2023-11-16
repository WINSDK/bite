use super::FONT;

use std::sync::Arc;
use egui::text::LayoutJob;

fn tokens_to_layoutjob(tokens: Vec<tokenizing::Token>) -> LayoutJob {
    let mut job = LayoutJob::default();

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

pub struct Disassembly {
    disassembly: Arc<disassembler::Disassembly>,
    disassembly_view: disassembler::DisassemblyView,
    instructions: LayoutJob,
    instruction_count: usize,
    min_row: usize,
    max_row: usize,
}

impl Disassembly {
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
}

impl super::Display for Disassembly {
    fn show(&mut self, ui: &mut egui::Ui) {
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
                        let instructions = self.disassembly_view.format();
                        self.instructions = tokens_to_layoutjob(instructions);
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

                    let instructions = self.disassembly_view.format();
                    self.instructions = tokens_to_layoutjob(instructions);
                    self.min_row = min_row;
                    self.max_row = max_row;
                }

                ui.label(self.instructions.clone());
            });
        });
    }
}

pub struct Functions {
    disassembly: Arc<disassembler::Disassembly>,
    functions: LayoutJob,
    function_count: usize,
    min_row: usize,
    max_row: usize,
}

impl Functions {
    pub fn new(disassembly: Arc<disassembler::Disassembly>) -> Self {
        let function_count = disassembly.symbols.named_len();

        Self {
            disassembly,
            functions: LayoutJob::default(),
            function_count,
            min_row: 0,
            max_row: 0
        }
    }
}

impl super::Display for Functions {
    fn show(&mut self, ui: &mut egui::Ui) {
        let area = egui::ScrollArea::both().auto_shrink([false, false]).drag_to_scroll(false);

        area.show_rows(ui, FONT.size, self.function_count, |ui, row_range| {
            if row_range != (self.min_row..self.max_row) {
                let functions = self.disassembly.functions(row_range);
                self.functions = tokens_to_layoutjob(functions);
                self.function_count = self.disassembly.symbols.named_len();
            }

            ui.label(self.functions.clone());
        });
    }
}
