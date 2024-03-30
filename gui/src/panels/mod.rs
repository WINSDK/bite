mod functions;
mod listing;
mod source_code;

use crate::common::*;
use crate::style::{EGUI, STYLE};
use crate::widgets::{Donut, Terminal, TextSelection};
use processor::Processor;

use egui::{Button, RichText};
use egui_dock::{DockArea, DockState};

use std::collections::BTreeMap;
use std::sync::Arc;

pub type Identifier = &'static str;

pub const SOURCE: Identifier = crate::icon!(EMBED2, " Source");
pub const DISASSEMBLY: Identifier = crate::icon!(PARAGRAPH_LEFT, " Disassembly");
pub const FUNCTIONS: Identifier = crate::icon!(LIGATURE, " Functions");
pub const LOGGING: Identifier = crate::icon!(TERMINAL, " Logs");

enum PanelKind {
    Disassembly(listing::Listing),
    Functions(functions::Functions),
    Source(source_code::Source),
    Logging,
}

pub struct Tabs {
    mapping: BTreeMap<Identifier, PanelKind>,
    terminal: Terminal,
    processor: Option<Arc<Processor>>,
    donut: Donut,
}

impl Tabs {
    fn new() -> Self {
        Self {
            mapping: {
                let mut mapping = BTreeMap::new();
                mapping.insert(LOGGING, PanelKind::Logging);
                mapping
            },
            terminal: Terminal::new(),
            processor: None,
            donut: Donut::new(false),
        }
    }
}

impl egui_dock::TabViewer for Tabs {
    type Tab = Identifier;

    fn title(&mut self, title: &mut Self::Tab) -> egui::WidgetText {
        egui::WidgetText::from(*title)
    }

    fn ui(&mut self, ui: &mut egui::Ui, title: &mut Self::Tab) {
        match self.mapping.get_mut(title) {
            Some(PanelKind::Disassembly(disassembly)) => disassembly.show(ui),
            Some(PanelKind::Functions(functions)) => functions.show(ui),
            Some(PanelKind::Source(src)) => src.show(ui),
            Some(PanelKind::Logging) => {
                let area = egui::ScrollArea::vertical()
                    .auto_shrink([false, false])
                    .drag_to_scroll(false)
                    .stick_to_bottom(true);

                area.show(ui, |ui| {
                    let layout = log::LOGGER.read().unwrap().format();
                    let text_area = TextSelection::precomputed(&layout);
                    ui.add(text_area);
                });
            }
            None => {}
        };
    }
}

pub struct Panels {
    pub layout: DockState<Identifier>,
    pub tabs: Tabs,
    pub ui_queue: Arc<crate::UIQueue>,
    pub winit_queue: crate::WinitQueue,
    loading: bool,
}

impl Panels {
    pub fn new(
        ui_queue: Arc<crate::UIQueue>,
        winit_queue: crate::WinitQueue,
    ) -> Self {
        let mut layout = DockState::new(vec![DISASSEMBLY, FUNCTIONS, LOGGING]);

        layout.set_focused_node_and_surface((
            egui_dock::SurfaceIndex::main(),
            egui_dock::NodeIndex::root(),
        ));

        Self {
            layout,
            tabs: Tabs::new(),
            ui_queue,
            winit_queue,
            loading: false,
        }
    }

    pub fn listing(&mut self) -> Option<&mut listing::Listing> {
        self.tabs.mapping.get_mut(DISASSEMBLY).and_then(|kind| match kind {
            PanelKind::Disassembly(listing) => Some(listing),
            _ => None,
        })
    }

    #[inline]
    pub fn processor(&mut self) -> Option<&Arc<Processor>> {
        self.tabs.processor.as_ref()
    }

    #[inline]
    pub fn terminal(&mut self) -> &mut Terminal {
        &mut self.tabs.terminal
    }

    pub fn is_loading(&self) -> bool {
        self.loading
    }

    pub fn start_loading(&mut self) {
        // create new donut to restart internal timer
        self.tabs.donut = Donut::new(false);
        self.loading = true;
    }

    pub fn stop_loading(&mut self) {
        self.loading = false;
    }

    pub fn load_source(&mut self, addr: usize) {
        let file_attr = match self.processor().and_then(|proc| proc.index.get_file_by_addr(addr)) {
            Some(file_attr) => file_attr,
            None => return,
        };

        if let Ok(src) = std::fs::read_to_string(&file_attr.path) {
            let src = source_code::Source::new(src, file_attr);
            self.tabs.mapping.insert(SOURCE, PanelKind::Source(src));
            self.goto_window(SOURCE);
        }
    }

    pub fn load_binary(&mut self, processor: Processor) {
        let processor = Arc::new(processor);

        self.tabs.mapping.insert(
            DISASSEMBLY,
            PanelKind::Disassembly(listing::Listing::new(processor.clone())),
        );

        self.tabs.mapping.insert(
            FUNCTIONS,
            PanelKind::Functions(functions::Functions::new(processor.clone())),
        );

        self.tabs.processor = Some(processor);
    }

    fn ask_for_binary(&self) {
        if let Some(path) = rfd::FileDialog::new().pick_file() {
            self.ui_queue.push(crate::UIEvent::BinaryRequested(path));
        }
    }

    pub fn handle_events(&mut self, events: &mut Vec<egui::Event>) {
        let empty_index = debugvault::Index::default();
        let index = self.tabs.processor.as_ref().map(|proc| &proc.index).unwrap_or(&empty_index);

        self.tabs.terminal.record_input(events, index);
    }

    /// Show some close/maximize/minimize buttons for the native window.
    fn top_bar_native(&mut self, ui: &mut egui::Ui) {
        let height = 12.0;
        let close_response = ui.add(Button::new(RichText::new(crate::icon!(CROSS)).size(height)));

        if close_response.clicked() {
            self.winit_queue.push(crate::WinitEvent::CloseRequest);
        }

        let maximized_response = ui.add(Button::new(
            RichText::new(crate::icon!(CHECKBOX_UNCHECKED)).size(height),
        ));

        if maximized_response.clicked() {
            self.winit_queue.push(crate::WinitEvent::Fullscreen);
        }

        let minimized_response =
            ui.add(Button::new(RichText::new(crate::icon!(MINUS)).size(height)));

        if minimized_response.clicked() {
            self.winit_queue.push(crate::WinitEvent::Minimize);
        }
    }

    fn goto_window(&mut self, title: Identifier) {
        match self.layout.find_tab(&title) {
            Some(tab) => self.layout.set_active_tab(tab),
            None => self.layout.push_to_first_leaf(title),
        }
    }

    fn top_bar(&mut self, ui: &mut egui::Ui) {
        let bar = egui::menu::bar(ui, |ui| {
            ui.menu_button("File", |ui| {
                if ui.button(crate::icon!(FOLDER_OPEN, " Open")).clicked() {
                    self.ask_for_binary();
                    ui.close_menu();
                }

                if ui.button(crate::icon!(CROSS, " Exit")).clicked() {
                    self.winit_queue.push(crate::WinitEvent::CloseRequest);
                    ui.close_menu();
                }
            });

            ui.menu_button("Windows", |ui| {
                if ui.button(DISASSEMBLY).clicked() {
                    self.goto_window(DISASSEMBLY);
                    ui.close_menu();
                }

                if ui.button(FUNCTIONS).clicked() {
                    self.goto_window(FUNCTIONS);
                    ui.close_menu();
                }

                if ui.button(SOURCE).clicked() {
                    self.goto_window(SOURCE);
                    ui.close_menu();
                }

                if ui.button(LOGGING).clicked() {
                    self.goto_window(LOGGING);
                    ui.close_menu();
                }
            });

            ui.with_layout(egui::Layout::right_to_left(egui::Align::Max), |ui| {
                ui.spacing_mut().item_spacing.x = 5.0;
                self.top_bar_native(ui);
            });
        });

        if bar.response.interact(egui::Sense::click()).double_clicked() {
            self.winit_queue.push(crate::WinitEvent::Fullscreen);
        }

        if bar.response.interact(egui::Sense::drag()).dragged() {
            self.winit_queue.push(crate::WinitEvent::DragWindow);
        }
    }

    fn input(&mut self, ctx: &mut egui::Context) {
        if ctx.input_mut(|i| i.consume_key(egui::Modifiers::CTRL, egui::Key::O)) {
            self.ask_for_binary();
        }

        // alt-tab'ing between tabs
        if ctx.input_mut(|i| i.consume_key(egui::Modifiers::CTRL, egui::Key::Tab)) {
            let (focused_surface, focused_node) = self.layout.focused_leaf().unwrap_or((
                egui_dock::SurfaceIndex::main(),
                egui_dock::NodeIndex::root(),
            ));

            // don't do tab'ing if there are no tabs
            if self.layout.main_surface().num_tabs() == 0 {
                return;
            }

            let focused = &mut self.layout[focused_surface][focused_node];
            if let egui_dock::Node::Leaf { tabs, active, .. } = focused {
                if active.0 != tabs.len() - 1 {
                    let tab_idx = active.0 + 1;
                    self.layout.set_active_tab((
                        focused_surface,
                        focused_node,
                        egui_dock::TabIndex(tab_idx),
                    ));
                } else {
                    self.layout.set_active_tab((
                        focused_surface,
                        focused_node,
                        egui_dock::TabIndex(0),
                    ));
                }
            }
        }
    }

    pub fn draw(&mut self, ctx: &mut egui::Context) {
        // generic keyboard inputs
        self.input(ctx);

        egui::TopBottomPanel::top("top bar").show(ctx, |ui| self.top_bar(ui));

        // terminal needs to be rendered last as it can take focus away from other panels
        let terminal = egui::TopBottomPanel::bottom("terminal")
            .min_height(80.0)
            .max_height(510.0)
            .resizable(true)
            .frame({
                egui::Frame::default()
                    .inner_margin(egui::Margin::same(STYLE.separator_width * 2.0))
                    .fill(tokenizing::colors::GRAY35)
            });

        let mut visuals = EGUI.visuals.clone();

        // set alternative background color
        visuals.extreme_bg_color = STYLE.primary_background;
        // disable on-hover highlighting for terminal
        visuals.widgets.active.fg_stroke = egui::Stroke::NONE;
        visuals.widgets.hovered.fg_stroke = egui::Stroke::NONE;

        ctx.set_visuals(visuals);

        let request_focus = self.terminal().should_reset_cursor();
        let term_response = terminal.show(ctx, |ui| {
            let response = ui
                .with_layout(egui::Layout::top_down_justified(egui::Align::Min), |ui| {
                    self.tabs.terminal.show(ui)
                });

            response.inner
        });

        ctx.set_visuals(EGUI.visuals.clone());

        let dock_area = egui::CentralPanel::default().frame({
            egui::Frame::default().inner_margin(egui::Margin {
                top: crate::style::STYLE.separator_width,
                ..Default::default()
            })
        });

        dock_area.show(ctx, |ui| {
            if self.loading {
                ui.spacing_mut().item_spacing.y = 20.0;
                ui.with_layout(
                    egui::Layout::top_down_justified(egui::Align::Center),
                    |ui| {
                        self.tabs.donut.show(ui);
                        log::PROGRESS.show(ui);
                    },
                );
            } else {
                let style = crate::style::DOCK.clone();

                DockArea::new(&mut self.layout)
                    .style(style)
                    .show_close_buttons(true)
                    .show_window_close_buttons(true)
                    .show_window_collapse_buttons(false)
                    .tab_context_menus(false)
                    .show_inside(ui, &mut self.tabs);
            }

            // give focus to terminal if any valid keyboard input happened
            if request_focus {
                ui.ctx().memory_mut(|m| m.request_focus(term_response.inner.id));
            }
        });
    }
}
