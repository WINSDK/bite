use crate::common::*;
use crate::{UiQueue, UIEvent};
use config::CONFIG;
use processor_shared::Addressed;
use processor::Processor;
use std::sync::Arc;
use tokenizing::{colors, Token};

pub struct Functions {
    processor: Arc<Processor>,
    ui_queue: Arc<UiQueue>,
    lines: Vec<(usize, Vec<Token>)>,
    lines_count: usize,
    min_row: usize,
    max_row: usize,
}

impl Functions {
    pub fn new(processor: Arc<Processor>, ui_queue: Arc<UiQueue>) -> Self {
        let function_count = processor.index.named_funcs_count();

        Self {
            processor,
            ui_queue,
            lines: Vec::new(),
            lines_count: function_count,
            min_row: 0,
            max_row: 0,
        }
    }
}

fn tokenize_functions(index: &debugvault::Index, range: std::ops::Range<usize>) -> Vec<(usize, Vec<Token>)> {
    let mut functions = Vec::new();
    let lines_to_read = range.end - range.start;
    let lines = index
        .functions()
        .filter(|func| !func.item.intrinsic())
        .skip(range.start)
        .take(lines_to_read + 10);

    for Addressed { addr, item } in lines {
        let mut tokens = Vec::new();
        tokens.push(Token::from_string(format!("{addr:0>10X}"), colors::WHITE));
        tokens.push(Token::from_str(" | ", colors::WHITE));

        if let Some(module) = item.module() {
            tokens.push(Token::from_string(module.to_string(), CONFIG.colors.asm.component));
            tokens.push(Token::from_str("!", CONFIG.colors.delimiter));
        }

        for token in item.name() {
            tokens.push(token.clone());
        }

        functions.push((*addr, tokens));
    }

    functions
}

impl Display for Functions {
    fn show(&mut self, ui: &mut egui::Ui) {
        let area = egui::ScrollArea::both().auto_shrink([false, false]).drag_to_scroll(false);

        area.show_rows(ui, FONT.size, self.lines_count, |ui, row_range| {
            if row_range != (self.min_row..self.max_row) {
                self.lines = tokenize_functions(&self.processor.index, row_range.clone());
                self.lines_count = self.processor.index.named_funcs_count();
                self.min_row = row_range.start;
                self.max_row = row_range.end;
            }

            for (addr, line) in self.lines.iter() {
                let output = tokens_to_layoutjob(line.clone());

                if ui.link(output).clicked() {
                    self.ui_queue.push(UIEvent::GotoAddr(*addr));
                }
            }
        });
    }
}
