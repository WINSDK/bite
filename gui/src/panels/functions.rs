use super::common::{tokens_to_layoutjob, FONT};

use egui::text::LayoutJob;
use std::sync::Arc;

pub struct Functions {
    disassembly: Arc<disassembler::Disassembly>,
    functions: LayoutJob,
    function_count: usize,
    min_row: usize,
    max_row: usize,
}

impl Functions {
    pub fn new(disassembly: Arc<disassembler::Disassembly>) -> Self {
        let function_count = disassembly.processor.symbols().named_len();

        Self {
            disassembly,
            functions: LayoutJob::default(),
            function_count,
            min_row: 0,
            max_row: 0,
        }
    }
}

impl super::Display for Functions {
    fn show(&mut self, ui: &mut egui::Ui) {
        let area = egui::ScrollArea::both().auto_shrink([false, false]).drag_to_scroll(false);

        area.show_rows(ui, FONT.size, self.function_count, |ui, row_range| {
            if row_range != (self.min_row..self.max_row) {
                let functions = self.disassembly.functions(row_range.clone());
                self.functions = tokens_to_layoutjob(functions);
                self.function_count = self.disassembly.processor.symbols().named_len();
                self.min_row = row_range.start;
                self.max_row = row_range.end;
            }

            ui.label(self.functions.clone());
        });
    }
}
