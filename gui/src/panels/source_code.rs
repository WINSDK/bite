use std::cmp::Ordering;
use std::ops::Range;
use std::path::Path;

use egui::text::LayoutJob;
use egui::Color32;
use tree_sitter::{Language, QueryError};
use tree_sitter_highlight::{Highlight, HighlightConfiguration, HighlightEvent, Highlighter};

use crate::common::*;
use crate::widgets::TextSelection;
use commands::CONFIG;
use symbols::FileAttr;
use tokenizing::colors;

pub struct Source {
    src: String,
    lines: Vec<Line>,
    max_number_width: usize,
    scroll: Option<usize>,
}

struct Line {
    number: String,
    sections: Vec<HighlightedSection>,
}

fn compute_sections<P: AsRef<Path>>(path: P, src: &str) -> Vec<HighlightedSection> {
    let lang_cfg = LanguageConfig::guess(path).expect("Failed to guess langauge");
    let highlight_cfg = lang_cfg.highlight_cfg().unwrap();

    let mut highlighter = Highlighter::new();
    let highlight_events =
        highlighter.highlight(&highlight_cfg, src.as_bytes(), None, |_| None).unwrap();

    let names = highlight_cfg.names();
    let mut styles = Vec::<&str>::new();
    let mut sections = Vec::new();

    for event in highlight_events {
        match event {
            Ok(HighlightEvent::Source { start, end }) => {
                if let Some(style) = styles.last() {
                    sections.push(HighlightedSection {
                        range: start..end,
                        fg_color: CONFIG.colors.get_by_style(style),
                        bg_color: Color32::TRANSPARENT,
                    });
                }
            }
            Ok(HighlightEvent::HighlightStart(Highlight(idx))) => {
                styles.push(&names[idx]);
            }
            Ok(HighlightEvent::HighlightEnd) => {
                styles.pop();
            }
            Err(err) => eprintln!("{err}"),
        }
    }

    sections.sort_unstable();

    // insert non-highlighted sections
    let mut last_end = 0;
    for idx in 0..sections.len() {
        let section = &sections[idx];
        let section_end = section.range.end;
        if section.range.start > last_end {
            // this is a non-highlighted section
            sections.push(HighlightedSection {
                range: last_end..section.range.start,
                fg_color: Color32::WHITE,
                bg_color: Color32::TRANSPARENT,
            });
        }
        last_end = section_end;
    }

    // handle the case where the file ends without a highlight
    if last_end < src.len() {
        sections.push(HighlightedSection {
            range: last_end..src.len(),
            fg_color: Color32::WHITE,
            bg_color: Color32::TRANSPARENT,
        });
    }

    sections.sort_unstable();
    sections
}

fn find_matching_sections(
    line: &str,
    offset: usize,
    sections: &[HighlightedSection],
) -> Vec<HighlightedSection> {
    let line_end = offset + line.len() + 1;
    sections
        .iter()
        .filter(|s| {
            // check if there is any overlap between the section and the current line
            s.range.start < line_end && s.range.end > offset
        })
        .cloned()
        .map(|mut s| {
            // adjust the range of the section to fit within the current line
            s.range.start = s.range.start.max(offset);
            s.range.end = s.range.end.min(line_end);
            s
        })
        .collect()
}

impl Source {
    pub fn new(src: String, file_attr: &FileAttr) -> Self {
        let max_width = (src.lines().count().ilog10() + 1) as usize;
        let mut lines = Vec::new();
        let sections = compute_sections(&file_attr.path, &src);

        let mut offset = 0;
        for (idx, line) in src.lines().enumerate() {
            let line_nr = idx + 1;
            let line_len = line.len();
            let mut line = Line {
                number: format!("{idx:max_width$} \n"),
                sections: find_matching_sections(line, offset, &sections),
            };

            if line_nr == file_attr.line {
                for section in line.sections.iter_mut() {
                    section.bg_color = Color32::RED;
                }
            }

            lines.push(line);
            offset += line_len + 1;
        }

        Self {
            src,
            lines,
            max_number_width: max_width,
            scroll: Some(file_attr.line.saturating_sub(1)),
        }
    }
}

impl Source {
    fn show_code(&mut self, ui: &mut egui::Ui, row_range: Range<usize>) {
        let mut output = LayoutJob::default();
        for line in &self.lines[row_range] {
            for section in &line.sections {
                output.append(
                    &self.src[section.range.clone()],
                    0.0,
                    egui::TextFormat {
                        color: section.fg_color,
                        background: section.bg_color,
                        font_id: FONT,
                        ..Default::default()
                    },
                );
            }
        }

        let text_area = TextSelection::precomputed(&output);
        ui.add(text_area);
    }

    fn show_line_numbers(&mut self, ui: &mut egui::Ui, row_range: Range<usize>) {
        let num_format = egui::TextFormat {
            font_id: FONT,
            color: colors::GRAY60,
            ..Default::default()
        };

        let mut output = LayoutJob::default();
        for line in &self.lines[row_range.clone()] {
            output.append(&line.number, 0.0, num_format.clone());
        }
        ui.label(output);
    }

    pub fn show(&mut self, ui: &mut egui::Ui) {
        let mut area = egui::ScrollArea::vertical().auto_shrink(false).drag_to_scroll(false);

        if let Some(scroll) = self.scroll.take() {
            let row_height = FONT.size;
            let spacing_y = ui.spacing().item_spacing.y;
            let y = scroll as f32 * (row_height + spacing_y);
            area = area.vertical_scroll_offset(y)
        }

        area.show_rows(ui, FONT.size, self.lines.len(), |ui, row_range| {
            let width = ui.fonts(|f| f.glyph_width(&FONT, ' ')) * self.max_number_width as f32;
            let split = (width / ui.available_width()) * 1.2;

            draw_columns(ui, split, |lcolumn, rcolumn| {
                self.show_line_numbers(lcolumn, row_range.clone());
                self.show_code(rcolumn, row_range.clone());
            });
        });
    }
}

struct LanguageConfig<'a> {
    lang: Language,
    highlights_query: &'a str,
    injection_query: Option<&'a str>,
    locals_query: Option<&'a str>,
    names_override: &'a [&'a str],
}

impl LanguageConfig<'_> {
    fn guess<P: AsRef<Path>>(path: P) -> Option<Self> {
        Some(match path.as_ref().extension().and_then(|s| s.to_str()) {
            Some("rs") => Self {
                lang: tree_sitter_rust::language(),
                highlights_query: tree_sitter_rust::HIGHLIGHT_QUERY,
                injection_query: Some(tree_sitter_rust::INJECTIONS_QUERY),
                locals_query: None,
                names_override: &[],
            },
            Some("c") => Self {
                lang: tree_sitter_c::language(),
                highlights_query: tree_sitter_c::HIGHLIGHT_QUERY,
                injection_query: None,
                locals_query: None,
                names_override: &[],
            },
            Some("cc" | "cpp" | "h" | "hh" | "hpp" | "cxx" | "cu") => Self {
                lang: tree_sitter_cpp::language(),
                highlights_query: tree_sitter_cpp::HIGHLIGHT_QUERY,
                injection_query: None,
                locals_query: None,
                names_override: &[],
            },
            None | Some(_) => return None,
        })
    }

    fn highlight_cfg(&self) -> Result<HighlightConfiguration, QueryError> {
        let mut cfg = HighlightConfiguration::new(
            self.lang,
            self.highlights_query,
            self.injection_query.unwrap_or_default(),
            self.locals_query.unwrap_or_default(),
        )?;
        match &self.names_override {
            &[_, ..] => cfg.configure(self.names_override),
            &[] => cfg.configure(&cfg.query.capture_names().to_vec()),
        }
        Ok(cfg)
    }
}

#[derive(Clone, Debug)]
struct HighlightedSection {
    range: Range<usize>,
    fg_color: Color32,
    bg_color: Color32,
}

impl PartialOrd for HighlightedSection {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HighlightedSection {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.range.start.cmp(&other.range.start)
    }
}

impl PartialEq for HighlightedSection {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.range.start == other.range.start
    }
}

impl Eq for HighlightedSection {}

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
            egui::pos2(
                lpos.x + column_width * split,
                ui.max_rect().right_bottom().y,
            ),
        );
        let rrect = egui::Rect::from_min_max(
            rpos,
            egui::pos2(
                rpos.x + column_width * (1.0 - split),
                ui.max_rect().right_bottom().y,
            ),
        );

        let mut lcolumn_ui =
            ui.child_ui(lrect, egui::Layout::top_down_justified(egui::Align::LEFT));
        let mut rcolumn_ui =
            ui.child_ui(rrect, egui::Layout::top_down_justified(egui::Align::LEFT));
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
