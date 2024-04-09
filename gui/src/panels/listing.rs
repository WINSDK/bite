#![allow(dead_code)]

use std::sync::Arc;
use crate::common::*;
use processor::{Processor, Block};
use infinite_scroll::InfiniteScroll;
use tokenizing::{colors, TokenStream};

type BlockIdx = usize;
type LineIdx = usize;

pub struct Listing {
    processor: Arc<Processor>,
    scroll: InfiniteScroll<Block, BlockIdx>,
}

fn processor_start(processor: &Processor) -> usize {
    processor.segments().next().map(|seg| seg.start).unwrap()
}

impl Listing {
    pub fn new(processor: Arc<Processor>) -> Self {
        let processor_cln = Arc::clone(&processor);
        let boundaries = Arc::new(processor.compute_block_boundaries());

        let infinite_scroll = InfiniteScroll::new()
            .end_loader(move |cursor, callback| {
                let boundaries = Arc::clone(&boundaries);
                let processor_cln = Arc::clone(&processor_cln);
                let block_idx = cursor.unwrap_or(0);

                std::thread::spawn(move || {
                    let mut all_blocks = Vec::new();
                    let mut idx = block_idx;

                    let mut lines_parsed = 0;
                    'done: loop {
                        if idx >= boundaries.len() {
                            break;
                        }

                        let blocks = processor_cln.parse_blocks(boundaries[idx]);
                        for block in blocks.iter() {
                            lines_parsed += block.len();

                            if lines_parsed >= 100 {
                                break 'done;
                            }
                        }

                        all_blocks.extend(blocks);
                        idx += 1;
                    }

                    callback(Ok((all_blocks, Some(idx))));
                });
            });

        Self { scroll: infinite_scroll, processor }
    }
}

fn draw_horizontal_line(ui: &mut egui::Ui) {
    let thickness = 1.0;
    let y = ui.cursor().min.y;

    let dashed_line = egui::Shape::dashed_line(
        &[egui::pos2(5.0, y), egui::pos2(ui.available_width(), y)],
        egui::Stroke::new(thickness, colors::WHITE),
        10.0,
        5.0,
    );

    ui.add_space(5.0);
    ui.painter().extend(dashed_line);
}

impl Display for Listing {
    fn show(&mut self, ui: &mut egui::Ui) {
        let area = egui::ScrollArea::vertical()
            .drag_to_scroll(false)
            .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysHidden)
            .animated(false);

        area.show(ui, |ui| {
            ui.set_width(ui.available_width());
            ui.vertical_centered(|ui| {
                ui.set_visible(self.scroll.top_loading_state().loading());
                ui.spinner();
            });

            self.scroll.ui(ui, 10, |ui, _idx, block| {
                match block {
                    Block::SectionStart { .. } => {
                        draw_horizontal_line(ui);
                    }
                    _ => {}
                }

                let mut stream = TokenStream::new();
                block.tokenize(&mut stream);
                ui.label(tokens_to_layoutjob(stream.inner));
            });

//             self.scroll.ui_custom_layout(ui, 1, |ui, _start_idx, blocks| {
//                 let mut stream = TokenStream::new();
//                 for block in blocks.iter() {
//                     block.tokenize(&mut stream);
//                 }
//                 ui.label(tokens_to_layoutjob(stream.inner));
//                 blocks.len() // this might not always be true
//             });

            ui.vertical_centered(|ui| {
                ui.set_visible(self.scroll.bottom_loading_state().loading());
                ui.spinner();
            });
        });

        // Overlay current section.
        if let Some(text) = self.processor.section_name(self.processor.entrypoint) {
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
}
