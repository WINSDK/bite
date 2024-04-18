use crate::common::*;
use config::CONFIG;
use egui::mutex::RwLock;
use egui::Color32;
use infinite_scroll::{Callback, InfiniteScroll};
use processor::{Block, BlockContent, Processor};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokenizing::{colors, TokenStream};

pub struct Listing {
    processor: Arc<Processor>,
    boundaries: Arc<RwLock<Vec<usize>>>,
    scroll: InfiniteScroll<Block, usize>,
    reset_position: Arc<AtomicUsize>,
    current_addr: usize,
    jump_list: Vec<usize>,
}

impl Listing {
    pub fn new(processor: Arc<Processor>) -> Self {
        let boundaries: Arc<RwLock<Vec<usize>>> = Arc::default();

        {
            // Compute boundaries on a separate thread to prevent GUI from blocking.
            let processor = Arc::clone(&processor);
            let boundaries = Arc::clone(&boundaries);
            std::thread::spawn(move || {
                let mut locked_boundaries = boundaries.write();
                *locked_boundaries = processor.compute_block_boundaries();
            });
        };

        let reset_position = Arc::new(AtomicUsize::new(0));

        let start_loader = {
            let reset_position = Arc::clone(&reset_position);
            let boundaries = Arc::clone(&boundaries);
            let processor = Arc::clone(&processor);

            move |cursor: Option<usize>, callback: Callback<Block, usize>| {
                let boundaries = Arc::clone(&boundaries);
                let processor = Arc::clone(&processor);

                let block_idx = cursor.unwrap_or_else(|| reset_position.load(Ordering::SeqCst));

                std::thread::spawn(move || {
                    let boundaries = boundaries.read();
                    let mut all_blocks = Vec::new();

                    if block_idx == 0 {
                        return callback(Ok((all_blocks, None)));
                    }

                    let mut idx = block_idx - 1;
                    let mut lines_parsed = 0;
                    loop {
                        let addr = boundaries[idx];
                        let blocks = processor.parse_blocks(addr);
                        for block in blocks.into_iter().rev() {
                            lines_parsed += block.len();
                            all_blocks.push(block);
                        }

                        if lines_parsed >= 100 {
                            break;
                        }

                        if idx == 0 {
                            return callback(Ok((all_blocks, None)));
                        }

                        idx -= 1;
                    }

                    // Reserve since we're adding blocks in reverse.
                    all_blocks.reverse();

                    callback(Ok((all_blocks, Some(idx))));
                });
            }
        };

        let end_loader = {
            let reset_position = Arc::clone(&reset_position);
            let boundaries = Arc::clone(&boundaries);
            let processor = Arc::clone(&processor);

            move |cursor: Option<usize>, callback: Callback<Block, usize>| {
                let boundaries = Arc::clone(&boundaries);
                let processor = Arc::clone(&processor);

                let block_idx = cursor.unwrap_or_else(|| reset_position.load(Ordering::SeqCst));

                std::thread::spawn(move || {
                    let boundaries = boundaries.read();
                    let mut all_blocks = Vec::new();

                    let mut idx = block_idx;
                    let mut lines_parsed = 0;
                    loop {
                        if idx >= boundaries.len() {
                            return callback(Ok((all_blocks, None)));
                        }

                        let addr = boundaries[idx];
                        let blocks = processor.parse_blocks(addr);
                        for block in blocks {
                            lines_parsed += block.len();
                            all_blocks.push(block);
                        }

                        if lines_parsed >= 100 {
                            idx += 1;
                            break;
                        }

                        idx += 1;
                    }

                    callback(Ok((all_blocks, Some(idx))));
                });
            }
        };

        let scroll = InfiniteScroll::new().start_loader(start_loader).end_loader(end_loader);
        let current_addr = processor.sections().next().unwrap().start;

        Self {
            scroll,
            boundaries,
            processor,
            reset_position,
            current_addr,
            jump_list: Vec::new(),
        }
    }

    pub fn jump(&mut self, addr: usize) -> bool {
        if let Ok(boundary) = self.boundaries.read().binary_search(&addr) {
            self.jump_list.push(self.current_addr);
            self.reset_position.store(boundary, Ordering::SeqCst);
            self.scroll.reset();
            return true;
        }

        false
    }

    pub fn record_input(&mut self, events: &mut Vec<egui::Event>) {
        events.retain(|event| match event {
            egui::Event::Key {
                key: egui::Key::Escape,
                pressed: true,
                modifiers: egui::Modifiers::NONE,
                ..
            } => {
                if let Some(addr) = self.jump_list.pop() {
                    let boundary = self.boundaries.read().binary_search(&addr).unwrap();
                    self.reset_position.store(boundary, Ordering::SeqCst);
                    self.scroll.reset();
                }
                false
            }
            _ => true,
        });
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

        let start_y = ui.cursor().min.y;

        area.show(ui, |ui| {
            ui.set_width(ui.available_width());

            let mut idx = 0;
            self.scroll.ui(ui, 10, |ui, _, block| {
                if idx == 0 {
                    self.current_addr = block.addr;
                }

                if let BlockContent::SectionStart { .. } = block.content {
                    draw_horizontal_line(ui);
                }

                let mut stream = TokenStream::new();
                block.tokenize(&mut stream);
                ui.label(tokens_to_layoutjob(stream.inner));
                idx += 1;
            });

            ui.vertical_centered(|ui| {
                ui.set_visible(self.scroll.bottom_loading_state().loading());
                ui.spinner();
            });
        });

        // Overlay current section.
        let text = self.processor.section_name(self.current_addr).unwrap();
        let max_width = ui.available_width();
        let size = egui::vec2(9.0 * text.len() as f32, 25.0);
        let offset = egui::pos2(8.0, start_y + 6.0);
        let rect = egui::Rect::from_two_pos(
            egui::pos2(max_width - offset.x, offset.y),
            egui::pos2(max_width - offset.x - size.x, offset.y + size.y),
        );

        ui.painter().rect(
            rect.expand2(egui::vec2(5.0, 0.0)),
            0.0,
            {
                let color = CONFIG.colors.bg_primary;
                Color32::from_rgb(
                    (color[0] as f32 * 1.1) as u8,
                    (color[1] as f32 * 1.1) as u8,
                    (color[2] as f32 * 1.1) as u8
                )
            },
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
