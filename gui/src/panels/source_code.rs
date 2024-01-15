use egui::text::LayoutJob;

use crate::common::*;
use crate::style::EGUI;
use crate::widgets::TextSelection;
use tokenizing::colors;

struct Line {
    number: String,
    content: String,
    highlighted: bool,
}

pub struct Source {
    lines: Vec<Line>,
    max_number_width: usize,
    scroll: Option<usize>,
}

impl Source {
    pub fn new(src: impl Into<String>, line_highlighted: usize) -> Self {
        let src = src.into();
        // let mut output = LayoutJob::default();

        let max_width = (src.lines().count().ilog10() + 1) as usize;
        let mut lines = Vec::new();
        for (idx, line) in src.lines().enumerate() {
            let idx = idx + 1;

            lines.push(Line {
                number: format!("{idx:max_width$} \n"),
                content: line.to_string() + "\n",
                highlighted: idx == line_highlighted,
            });
        }

        Self {
            lines,
            max_number_width: max_width,
            scroll: Some(line_highlighted.saturating_sub(1)),
        }
    }
}

fn draw_columns<R>(
    ui: &mut egui::Ui,
    split: f32,
    add_contents: impl FnOnce(&mut egui::Ui, &mut egui::Ui) -> R,
) -> R {
    debug_assert!(split >= 0.0 && split <= 1.0);
    let spacing = ui.spacing().item_spacing.x;
    let total_spacing = spacing * (2 as f32 - 1.0);
    let column_width = ui.available_width() - total_spacing;
    let top_left = ui.cursor().min;

    let (mut left, mut right) = {
        let lpos = top_left;
        let rpos = top_left + egui::vec2(split * (column_width + spacing), 0.0);

        let lrect = egui::Rect::from_min_max(
            lpos,
            egui::pos2(lpos.x + column_width * split, ui.max_rect().right_bottom().y),
        );
        let rrect = egui::Rect::from_min_max(
            rpos,
            egui::pos2(rpos.x + column_width * (1.0 - split), ui.max_rect().right_bottom().y),
        );

        let mut lcolumn_ui = ui.child_ui(
            lrect,
            egui::Layout::top_down_justified(egui::Align::LEFT),
        );
        let mut rcolumn_ui = ui.child_ui(
            rrect,
            egui::Layout::top_down_justified(egui::Align::LEFT),
        );
        lcolumn_ui.set_width(column_width * split);
        rcolumn_ui.set_width(column_width * (1.0 - split));
        (lcolumn_ui, rcolumn_ui)
    };

    let result = add_contents(&mut left, &mut right);

    let mut max_column_width = column_width;
    let mut max_height = 0.0;
    for column in &[left, right] {
        max_column_width = max_column_width.max(column.min_rect().width());
        max_height = column.min_size().y.max(max_height);
    }

    // make sure we fit everything next frame
    let total_required_width = total_spacing + max_column_width * 2.0;

    let size = egui::vec2(ui.available_width().max(total_required_width), max_height);
    ui.advance_cursor_after_rect(egui::Rect::from_min_size(top_left, size));
    result
}

impl Display for Source {
    fn show(&mut self, ui: &mut egui::Ui) {
        let mut area = egui::ScrollArea::vertical().auto_shrink(false).drag_to_scroll(false);

        if let Some(scroll) = self.scroll.take() {
            let row_height = FONT.size;
            let spacing_y = ui.spacing().item_spacing.y;
            let y = scroll as f32 * (row_height + spacing_y);
            area = area.vertical_scroll_offset(y)
        }

        area.show_rows(ui, FONT.size, self.lines.len(), |ui, row_range| {
            let color = EGUI.noninteractive().fg_stroke.color;
            let num_format = egui::TextFormat {
                font_id: FONT,
                color: colors::GRAY60,
                ..Default::default()
            };

            let width = ui.fonts(|f| f.glyph_width(&FONT, ' ')) * self.max_number_width as f32;
            let split = (width / ui.available_width()) * 1.2;

            draw_columns(ui, split , |lcolumn, rcolumn| {
                let mut output = LayoutJob::default();
                for line in &self.lines[row_range.clone()] {
                    output.append(&line.number, 0.0, num_format.clone());
                }
                lcolumn.label(output);

                let mut output = LayoutJob::default();
                for line in &self.lines[row_range] {
                    // highlight selected line
                    let txt_format = if line.highlighted {
                        egui::TextFormat {
                            font_id: FONT,
                            color,
                            background: egui::Color32::RED,
                            ..Default::default()
                        }
                    } else {
                        egui::TextFormat {
                            font_id: FONT,
                            color,
                            ..Default::default()
                        }
                    };

                    output.append(&line.content, 0.0, txt_format);
                }

                let text_area = TextSelection::precomputed(&output);
                rcolumn.add(text_area);
            });
        });
    }
}
