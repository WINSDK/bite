use super::common::{FONT, tokens_to_layoutjob};

use std::sync::Arc;
use egui::text::LayoutJob;

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
