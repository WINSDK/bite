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
    fn highlight_job(entry: &SearchResult) -> LayoutJob {
        let mut job = LayoutJob::default();
        let mut buffer = String::new();
        let mut current_color = colors::WHITE;
        let mut hits = entry.indices.iter().copied().peekable();

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

        for (idx, ch) in entry.name.chars().enumerate() {
            let is_hit = hits.peek().map_or(false, |&i| i == idx as u32);
            if is_hit {
                hits.next();
            }

            let color = if is_hit { colors::GREEN } else { colors::WHITE };
            if color != current_color {
                flush(&mut buffer, current_color, &mut job);
                current_color = color;
            }

            buffer.push(ch);
        }

        flush(&mut buffer, current_color, &mut job);
        job
    }

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

        let query = self.query.trim();
        index.and_then(|idx| idx.get_func_by_name(query))
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

        for idx in start..end {
            let suggestion = &self.results[idx];
            let name = suggestion.name.clone();
            let addr = suggestion.addr;
            let job = Self::highlight_job(suggestion);
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

            response |= ui
                .allocate_ui_at_rect(text_rect, |ui| {
                    ui.set_clip_rect(text_rect);
                    ui.add(egui::Label::new(job.clone()))
                })
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
