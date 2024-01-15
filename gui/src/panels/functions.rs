use crate::common::*;
use crate::widgets::TextSelection;

use egui::text::LayoutJob;
use processor::Processor;
use std::sync::Arc;
use tokenizing::{colors, Token};

pub struct Functions {
    processor: Arc<Processor>,
    lines: LayoutJob,
    lines_count: usize,
    min_row: usize,
    max_row: usize,
}

impl Functions {
    pub fn new(processor: Arc<Processor>) -> Self {
        let function_count = processor.index.named_len();

        Self {
            processor,
            lines: LayoutJob::default(),
            lines_count: function_count,
            min_row: 0,
            max_row: 0,
        }
    }
}

fn tokenize_functions(index: &symbols::Index, range: std::ops::Range<usize>) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();

    let lines_to_read = range.end - range.start;
    let lines = index
        .iter()
        .filter(|(_, func)| !func.intrinsic())
        .skip(range.start)
        .take(lines_to_read + 10);

    // for each instruction
    for (addr, symbol) in lines {
        tokens.push(Token::from_string(format!("{addr:0>10X}"), colors::WHITE));
        tokens.push(Token::from_str(" | ", colors::WHITE));

        if let Some(module) = symbol.module() {
            tokens.push(Token::from_string(module.to_string(), colors::MAGENTA));
            tokens.push(Token::from_str("!", colors::GRAY60));
        }

        for token in symbol.name() {
            tokens.push(token.clone());
        }

        tokens.push(Token::from_str("\n", colors::WHITE));
    }

    tokens
}

impl Display for Functions {
    fn show(&mut self, ui: &mut egui::Ui) {
        let area = egui::ScrollArea::both().auto_shrink([false, false]).drag_to_scroll(false);

        area.show_rows(ui, FONT.size, self.lines_count, |ui, row_range| {
            if row_range != (self.min_row..self.max_row) {
                let functions = tokenize_functions(&self.processor.index, row_range.clone());
                self.lines = tokens_to_layoutjob(functions);
                self.lines_count = self.processor.index.named_len();
                self.min_row = row_range.start;
                self.max_row = row_range.end;
            }

            let text_area = TextSelection::precomputed(&self.lines);
            ui.add(text_area);
        });
    }
}
