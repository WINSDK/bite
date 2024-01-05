use crate::common::*;
use crate::widgets::TextSelection;

use disassembler::Processor;
use egui::text::LayoutJob;
use std::sync::Arc;

pub struct Functions {
    processor: Arc<Processor>,
    lines: LayoutJob,
    lines_count: usize,
    min_row: usize,
    max_row: usize,
}

impl Functions {
    pub fn new(processor: Arc<Processor>) -> Self {
        let function_count = processor.symbols().named_len();

        Self {
            processor,
            lines: LayoutJob::default(),
            lines_count: function_count,
            min_row: 0,
            max_row: 0,
        }
    }
}

impl Display for Functions {
    fn show(&mut self, ui: &mut egui::Ui) {
        let area = egui::ScrollArea::both().auto_shrink([false, false]).drag_to_scroll(false);

        area.show_rows(ui, FONT.size, self.lines_count, |ui, row_range| {
            if row_range != (self.min_row..self.max_row) {
                let functions = self.processor.functions(row_range.clone());
                self.lines = tokens_to_layoutjob(functions);
                self.lines_count = self.processor.symbols().named_len();
                self.min_row = row_range.start;
                self.max_row = row_range.end;
            }

            let text_area = TextSelection::precomputed(&self.lines);
            ui.add(text_area);
        });
    }
}
