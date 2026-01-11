use crate::common::FONT;
use debugvault::Index;
use nucleo::pattern::{CaseMatching, Normalization};
use nucleo::{Config, Nucleo, Utf32String};
use std::sync::Arc;
use tokenizing::colors;

const MAX_RESULTS: u32 = 50;
const MATCHER_TIMEOUT_MS: u64 = 10;
const VISIBLE_SUGGESTIONS: usize = 8;

#[derive(Clone)]
struct SearchEntry {
    name: String,
    addr: usize,
}

#[derive(Clone)]
struct SearchResult {
    name: String,
    addr: usize,
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

        if count > 0 {
            for item in snapshot.matched_items(0..count) {
                refreshed.push(SearchResult {
                    name: item.data.name.clone(),
                    addr: item.data.addr,
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
                pressed: true,
                ..
            } => {
                self.cursor = 0;
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
                let visible_len = self.results.len().min(VISIBLE_SUGGESTIONS);
                if visible_len > 0 && self.selected > 0 {
                    self.selected -= 1;
                }
                consumed = true;
                false
            }
            egui::Event::Key {
                key: egui::Key::ArrowDown,
                pressed: true,
                ..
            } => {
                let visible_len = self.results.len().min(VISIBLE_SUGGESTIONS);
                if visible_len > 0 {
                    self.selected = (self.selected + 1).min(visible_len.saturating_sub(1));
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
        let frame = egui::Frame::default()
            .rounding(egui::Rounding::ZERO)
            .stroke(egui::Stroke::NONE)
            .fill(colors::GRAY35);

        frame.show(ui, |ui| {
            ui.set_width(available_width);

            let layout = {
                let mut job = egui::text::LayoutJob::default();
                job.append(
                    &self.query,
                    0.0,
                    egui::TextFormat {
                        font_id: FONT,
                        color: egui::Color32::WHITE,
                        ..Default::default()
                    },
                );
                job
            };

            let input_id = egui::Frame::none()
                .fill(colors::GRAY60)
                .show(ui, |ui| {
                    let mut widget = crate::widgets::TextSelection::precomputed(&layout);
                    widget.set_reset_position(self.cursor);
                    ui.add_sized([ui.available_width(), ui.spacing().interact_size.y], widget).id
                })
                .inner;

            if self.focus_input {
                ui.ctx().memory_mut(|m| m.request_focus(input_id));
                self.focus_input = false;
            }

            ui.add_space(6.0);

            self.refresh_results(ui.ctx(), index);
            let selected = self.selected;
            let suggestions: Vec<SearchResult> =
                self.results.iter().take(VISIBLE_SUGGESTIONS).cloned().collect();
            if suggestions.is_empty() {
                return;
            }

            for (idx, suggestion) in suggestions.into_iter().enumerate() {
                let mut render = |ui: &mut egui::Ui| {
                    let label = ui.label(egui::RichText::new(&suggestion.name).font(FONT));
                    if label.clicked() {
                        self.query = suggestion.name.clone();
                        self.cursor = self.query.chars().count();
                        self.selected = 0;
                        self.pending_jump = Some(suggestion.addr);
                        self.close();
                    }
                };

                if idx == selected {
                    egui::Frame::none()
                        .fill(colors::GRAY60)
                        .inner_margin(egui::Margin::symmetric(6.0, 4.0))
                        .show(ui, render);
                } else {
                    render(ui);
                }
            }
        });
    }
}
