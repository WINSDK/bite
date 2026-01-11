use crate::common::FONT;
use debugvault::Index;
use egui::text::LayoutJob;
use nucleo::pattern::{CaseMatching, Normalization};
use nucleo::{Config, Matcher, Nucleo, Utf32String};
use std::sync::Arc;
use tokenizing::colors;

const MAX_RESULTS: u32 = 50;
const VISIBLE_SUGGESTIONS: usize = 8;
const MATCHER_TIMEOUT_MS: u64 = 10;

#[derive(Clone)]
struct SearchEntry {
    name: String,
    addr: usize,
}

#[derive(Clone)]
struct SearchResult {
    name: String,
    addr: usize,
    indices: Vec<u32>,
}

fn best_slice(len: usize, visible_chars: usize, indices: &[u32]) -> (usize, usize) {
    if len <= visible_chars {
        return (0, len);
    }

    // find longest contiguous hit run, fall back to middle if none
    let mut best_start = 0;
    let mut best_len = 0;
    let mut current_start = 0;
    let mut current_len = 0;
    let mut prev_idx: Option<u32> = None;
    for &idx in indices {
        if prev_idx.map_or(false, |p| idx == p + 1) {
            current_len += 1;
        } else {
            current_start = idx as usize;
            current_len = 1;
        }

        if current_len > best_len {
            best_len = current_len;
            best_start = current_start;
        }

        prev_idx = Some(idx);
    }

    let center = if best_len == 0 { len / 2 } else { best_start + best_len / 2 };

    let window_len = visible_chars.max(1);

    let mut slice_start = center.saturating_sub(window_len / 2);
    if slice_start + window_len > len {
        slice_start = len - window_len;
    }
    let slice_end = (slice_start + window_len).min(len);

    (slice_start, slice_end)
}

fn truncated_job(entry: &SearchResult, visible_chars: usize) -> LayoutJob {
    let chars: Vec<char> = entry.name.chars().collect();
    let len = chars.len();
    let (start, end) = best_slice(len, visible_chars, &entry.indices);

    let mut job = LayoutJob::default();
    job.wrap.max_rows = 1;
    job.wrap.break_anywhere = true;

    let mut buffer = String::new();
    let mut current_color = colors::WHITE;
    let mut hits = entry
        .indices
        .iter()
        .copied()
        .filter(|&i| (start as u32) <= i && i < end as u32)
        .peekable();

    let flush = |buf: &mut String, color: egui::Color32, job: &mut LayoutJob| {
        if buf.is_empty() {
            return;
        }
        job.append(
            buf,
            0.0,
            egui::TextFormat {
                font_id: FONT,
                color,
                ..Default::default()
            },
        );
        buf.clear();
    };

    for (idx, chr) in chars[start..end].iter().enumerate() {
        let gidx = start + idx;
        let is_hit = hits.peek().map_or(false, |&i| i == gidx as u32);
        let color = if is_hit {
            hits.next();
            colors::GREEN
        } else {
            colors::WHITE
        };

        if color != current_color {
            flush(&mut buffer, current_color, &mut job);
            current_color = color;
        }

        buffer.push(*chr);
    }

    flush(&mut buffer, current_color, &mut job);

    job
}

struct MatcherState {
    nucleo: Nucleo<SearchEntry>,
    last_query: String,
    index_ptr: *const Index,
}

pub struct SearchPopup {
    visible: bool,
    focus_input: bool,
    query: String,
    cursor: usize,
    selected: usize,
    pending_jump: Option<usize>,
    results: Vec<SearchResult>,
    matcher: Option<MatcherState>,
    last_hover_pointer: Option<egui::Pos2>,
}

impl SearchPopup {
    pub fn new() -> Self {
        Self {
            visible: false,
            focus_input: false,
            query: String::new(),
            cursor: 0,
            selected: 0,
            pending_jump: None,
            results: Vec::new(),
            matcher: None,
            last_hover_pointer: None,
        }
    }

    pub fn open(&mut self) {
        self.visible = true;
        self.focus_input = true;
        self.query.clear();
        self.cursor = 0;
        self.selected = 0;
    }

    pub fn close(&mut self) {
        self.visible = false;
        self.focus_input = false;
    }

    fn build(ctx: &egui::Context, index: &Index) -> MatcherState {
        let notify = {
            let ctx = ctx.clone();
            Arc::new(move || ctx.request_repaint())
        };

        let now = std::time::Instant::now();
        let mut nucleo = Nucleo::new(Config::DEFAULT, notify, None, 1);
        let injector = nucleo.injector();

        for func in index.functions().filter(|func| !func.item.intrinsic()) {
            let entry = SearchEntry {
                name: func.item.as_str().to_string(),
                addr: func.addr,
            };

            injector.push(entry, |entry, cols| {
                cols[0] = Utf32String::from(entry.name.as_str());
            });
        }

        let _ = nucleo.tick(MATCHER_TIMEOUT_MS);
        log::complex!(
            w "[search::build] fuzzy matcher ready in ",
            y format!("{:?}", now.elapsed()),
            w " (",
            g nucleo.snapshot().item_count().to_string(),
            w " entries)."
        );

        MatcherState {
            nucleo,
            last_query: String::new(),
            index_ptr: index,
        }
    }

    fn matcher_for(&mut self, ctx: &egui::Context, index: &Index) -> &mut MatcherState {
        let needs_new = self.matcher.as_ref().is_none_or(|m| !std::ptr::eq(m.index_ptr, index));

        if needs_new {
            self.matcher = Some(Self::build(ctx, index));
        }

        self.matcher.as_mut().unwrap()
    }

    fn refresh_results(&mut self, ctx: &egui::Context, index: Option<&Index>) {
        let Some(index) = index else {
            self.results.clear();
            return;
        };

        let query = self.query.clone();
        let mut refreshed = Vec::new();

        let matcher = self.matcher_for(ctx, index);

        if matcher.last_query != query {
            let append = query.starts_with(&matcher.last_query);
            matcher.nucleo.pattern.reparse(
                0,
                &query,
                CaseMatching::Smart,
                Normalization::Smart,
                append,
            );
            matcher.last_query.clone_from(&query);
        }

        let _ = matcher.nucleo.tick(MATCHER_TIMEOUT_MS);
        let snapshot = matcher.nucleo.snapshot();
        let count = snapshot.matched_item_count().min(MAX_RESULTS);

        let mut indices_buf = Vec::new();
        let mut scorer = Matcher::default();
        let pattern = snapshot.pattern().column_pattern(0).clone();

        if count > 0 {
            for item in snapshot.matched_items(0..count) {
                indices_buf.clear();
                if pattern
                    .indices(
                        item.matcher_columns[0].slice(..),
                        &mut scorer,
                        &mut indices_buf,
                    )
                    .is_none()
                {
                    continue;
                }
                indices_buf.sort_unstable();
                indices_buf.dedup();

                refreshed.push(SearchResult {
                    name: item.data.name.clone(),
                    addr: item.data.addr,
                    indices: indices_buf.clone(),
                });
            }
        }

        self.results = refreshed;
    }

    fn resolve_to_addr(&mut self, ctx: &egui::Context, index: Option<&Index>) -> Option<usize> {
        self.refresh_results(ctx, index);

        if let Some(result) = self.results.get(self.selected) {
            return Some(result.addr);
        }

        index.and_then(|idx| idx.get_func_by_name(&self.query))
    }

    pub fn take_jump(&mut self) -> Option<usize> {
        self.pending_jump.take()
    }

    pub fn is_visible(&self) -> bool {
        self.visible
    }

    pub fn handle_events(&mut self, events: &mut Vec<egui::Event>) -> bool {
        let modifier = if cfg!(target_os = "macos") {
            egui::Modifiers::MAC_CMD
        } else {
            egui::Modifiers::CTRL
        };

        let mut consumed = false;

        // Always listen for the shortcut to prevent leaks to the terminal.
        if !self.visible {
            let mut opened = false;
            events.retain(|event| match event {
                egui::Event::Key {
                    key: egui::Key::F,
                    pressed: true,
                    modifiers,
                    ..
                } if *modifiers == modifier => {
                    self.open();
                    opened = true;
                    consumed = true;
                    false
                }
                egui::Event::Text(_) if opened => {
                    consumed = true;
                    false
                }
                _ => true,
            });

            return consumed;
        }

        events.retain(|event| match event {
            egui::Event::Text(text) => {
                self.query.insert_str(self.cursor, text);
                self.cursor += text.chars().count();
                self.selected = 0;
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::Backspace,
                pressed: true,
                modifiers,
                ..
            } if *modifiers == egui::Modifiers::NONE => {
                if self.cursor > 0 {
                    let remove_idx =
                        self.query.char_indices().nth(self.cursor - 1).map(|(i, _)| i).unwrap_or(0);
                    self.query.remove(remove_idx);
                    self.cursor -= 1;
                }
                self.selected = 0;
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::Delete,
                pressed: true,
                modifiers,
                ..
            } if *modifiers == egui::Modifiers::NONE => {
                if self.cursor < self.query.chars().count() {
                    let start = self
                        .query
                        .char_indices()
                        .nth(self.cursor)
                        .map(|(i, _)| i)
                        .unwrap_or(self.query.len());
                    let end = self
                        .query
                        .char_indices()
                        .nth(self.cursor + 1)
                        .map(|(i, _)| i)
                        .unwrap_or_else(|| self.query.len());
                    self.query.drain(start..end);
                }
                self.selected = 0;
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::ArrowLeft,
                pressed: true,
                ..
            } => {
                if self.cursor > 0 {
                    self.cursor -= 1;
                }
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::ArrowRight,
                pressed: true,
                ..
            } => {
                if self.cursor < self.query.chars().count() {
                    self.cursor += 1;
                }
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::Home,
                modifiers:
                    egui::Modifiers {
                        ctrl: true,
                        shift: false,
                        ..
                    },
                pressed: true,
                ..
            } => {
                self.cursor = 0;
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::End,
                modifiers:
                    egui::Modifiers {
                        ctrl: true,
                        shift: false,
                        ..
                    },
                pressed: true,
                ..
            } => {
                self.cursor = self.query.chars().count();
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::End,
                pressed: true,
                ..
            } => {
                self.cursor = self.query.chars().count();
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::ArrowUp,
                pressed: true,
                ..
            } => {
                let total = self.results.len();
                if total > 0 {
                    if self.selected >= total {
                        self.selected = total - 1;
                    } else if self.selected > 0 {
                        self.selected -= 1;
                    }
                }
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::ArrowDown,
                pressed: true,
                ..
            } => {
                let total = self.results.len();
                if total > 0 {
                    if self.selected >= total.saturating_sub(1) {
                        self.selected = total.saturating_sub(1);
                    } else {
                        self.selected += 1;
                    }
                }
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::F,
                pressed: true,
                modifiers,
                ..
            } if *modifiers == modifier => {
                self.open();
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::C | egui::Key::D,
                pressed: true,
                modifiers: egui::Modifiers::CTRL,
                ..
            } => {
                self.close();
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::Escape,
                pressed: true,
                ..
            } => {
                self.close();
                consumed = true;
                false
            }
            _ => true,
        });

        if consumed {
            self.focus_input = true;
        }

        consumed
    }

    pub fn handle_input(&mut self, ctx: &egui::Context, index: Option<&Index>) -> bool {
        let modifier = if cfg!(target_os = "macos") {
            egui::Modifiers::MAC_CMD
        } else {
            egui::Modifiers::CTRL
        };

        if ctx.input_mut(|i| i.consume_key(modifier, egui::Key::F)) {
            self.open();
            return true;
        }

        if self.visible {
            if ctx.input_mut(|i| i.consume_key(egui::Modifiers::NONE, egui::Key::Enter)) {
                if let Some(addr) = self.resolve_to_addr(ctx, index) {
                    self.pending_jump = Some(addr);
                }
                self.close();
                return true;
            }
        }

        false
    }

    pub fn show(&mut self, ui: &mut egui::Ui, index: Option<&Index>) {
        let available_width = ui.available_width();

        ui.set_width(available_width);

        let layout = {
            let mut job = LayoutJob::default();
            job.append(
                &self.query,
                0.0,
                egui::TextFormat {
                    font_id: FONT,
                    color: colors::WHITE,
                    ..Default::default()
                },
            );
            job
        };

        let mut widget = crate::widgets::TextSelection::precomputed(&layout);
        widget.set_reset_position(self.cursor);
        let input_id =
            ui.add_sized([ui.available_width(), ui.spacing().interact_size.y], widget).id;

        if self.focus_input {
            ui.ctx().memory_mut(|m| m.request_focus(input_id));
            self.focus_input = false;
        }

        ui.add_space(6.0);

        self.refresh_results(ui.ctx(), index);
        let total = self.results.len();
        if total == 0 {
            return;
        }
        if self.selected >= total {
            self.selected = total - 1;
        }

        let start = self.selected.saturating_sub(VISIBLE_SUGGESTIONS.saturating_sub(1));
        let end = (start + VISIBLE_SUGGESTIONS).min(total);
        let char_width = ui.fonts_mut(|f| f.glyph_width(&FONT, 'a')).max(1.0);

        for idx in start..end {
            let suggestion = &self.results[idx];
            let name = suggestion.name.clone();
            let addr = suggestion.addr;
            let row_size = egui::vec2(ui.available_width(), ui.spacing().interact_size.y);
            let (row_rect, mut response) = ui.allocate_exact_size(row_size, egui::Sense::click());

            if idx == self.selected {
                ui.painter().rect_filled(row_rect, 0.0, colors::GRAY35);
            }

            let bar_color = if idx == self.selected {
                egui::Color32::from_rgb(0xdb, 0x3c, 0x30)
            } else {
                colors::GRAY60
            };

            let bar_rect =
                egui::Rect::from_min_size(row_rect.min, egui::vec2(4.0, row_rect.height()));
            ui.painter().rect_filled(bar_rect, 0.0, bar_color);

            let text_rect = egui::Rect::from_min_size(
                egui::pos2(bar_rect.max.x + 6.0, row_rect.min.y),
                egui::vec2(row_rect.width() - bar_rect.width() - 6.0, row_rect.height()),
            );

            let max_chars = (text_rect.width() / char_width).floor().max(1.0) as usize;
            let job = truncated_job(suggestion, max_chars);

            response |= ui
                .scope_builder(
                    egui::UiBuilder::new()
                        .max_rect(text_rect)
                        .layout(ui.layout().clone()),
                    |ui| {
                        ui.set_clip_rect(text_rect);
                        ui.add(egui::Label::new(job))
                    },
                )
                .inner;

            if response.hovered() {
                ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                if let Some(pos) = ui.ctx().input(|i| i.pointer.latest_pos()) {
                    if self.last_hover_pointer.map_or(true, |prev| prev != pos) {
                        self.selected = idx;
                        self.last_hover_pointer = Some(pos);
                    }
                }
            }

            if response.clicked() {
                self.query = name;
                self.cursor = self.query.chars().count();
                self.selected = idx;
                self.pending_jump = Some(addr);
                self.close();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{best_slice, truncated_job, SearchResult};
    use egui::text::LayoutJob;
    use tokenizing::colors;

    fn job_text(job: &LayoutJob) -> String {
        job.sections
            .iter()
            .map(|s| job.text[s.byte_range.clone()].to_string())
            .collect()
    }

    #[test]
    fn best_slice_prefers_hits() {
        let name = "xx__prefix__MATCH__suffix__yy";
        let hits: Vec<u32> = (12..17).collect(); // MATCH
        let (start, end) = best_slice(name.len(), 10, &hits);
        assert!(start <= 12 && end >= 16, "slice should include match window");
        assert!(end <= name.len(), "slice end within bounds");
    }

    #[test]
    fn truncated_job_retains() {
        let entry = SearchResult {
            name: "abc__prefix__MATCH__suffix__xyz".to_string(),
            addr: 0,
            indices: (13..18).collect(), // MATCH
        };

        let job = truncated_job(&entry, 10);
        let text = job_text(&job);
        assert!(text.contains("MATCH"), "match chunk should be visible");

        let has_green = job.sections.iter().any(|s| s.format.color == colors::GREEN);
        assert!(has_green, "match highlight should remain");
    }

    #[test]
    fn truncated_job_no_truncation() {
        let entry = SearchResult {
            name: "short".to_string(),
            addr: 0,
            indices: vec![1, 3],
        };

        let job = truncated_job(&entry, 20);
        let text = job_text(&job);
        assert_eq!(text, "short");
        let green_hits = job
            .sections
            .iter()
            .filter(|s| s.format.color == colors::GREEN)
            .map(|s| job.text[s.byte_range.clone()].chars().count())
            .sum::<usize>();
        assert_eq!(green_hits, 2);
    }
}
