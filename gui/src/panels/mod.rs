mod functions;
mod listing;
mod source_code;

use crate::common::*;
use crate::style::{EGUI, STYLE};
use crate::widgets::{Donut, Terminal};
use egui_tiles::{Container, SimplificationOptions, Tile, TileId, Tiles, Tree, UiResponse};
use config::CONFIG;
use processor::Processor;
use tokenizing::colors;

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

impl egui_tiles::Behavior<Identifier> for Tabs {
    fn tab_title_for_pane(&mut self, pane: &Identifier) -> egui::WidgetText {
        (*pane).into()
    }

    fn tab_bg_color(
        &self,
        _: &egui::Visuals,
        _: &Tiles<Identifier>,
        _: TileId,
        active: bool,
    ) -> egui::Color32 {
        if active {
            CONFIG.colors.bg_primary
        } else {
            CONFIG.colors.bg_secondary
        }
    }

    fn tab_bar_color(&self, _: &egui::Visuals) -> egui::Color32 {
        CONFIG.colors.bg_secondary
    }

    fn drag_preview_color(&self, _: &egui::Visuals) -> egui::Color32 {
        STYLE.selection_color
    }

    fn tab_bar_height(&self, _: &egui::Style) -> f32 {
        30.0
    }

    fn gap_width(&self, _: &egui::Style) -> f32 {
        5.0
    }

    fn min_size(&self) -> f32 {
        100.0
    }

    fn simplification_options(&self) -> SimplificationOptions {
        SimplificationOptions {
            all_panes_must_have_tabs: true,
            ..Default::default()
        }
    }

    fn pane_ui(
        &mut self,
        ui: &mut egui::Ui,
        _tile_id: egui_tiles::TileId,
        pane: &mut Identifier,
    ) -> egui_tiles::UiResponse {
        // Set pane background color.
        ui.painter().rect_filled(ui.max_rect(), 0.0, CONFIG.colors.bg_primary);

        egui::Frame::default().inner_margin(egui::Margin::same(5.0)).show(ui, |ui| {
            match self.mapping.get_mut(pane) {
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
                        ui.label(layout);
                    });
                }
                None => {}
            };
        });

        UiResponse::None
    }
}

pub struct Panels {
    pub tree: egui_tiles::Tree<Identifier>,
    pub panes: Tabs,
    pub ui_queue: Arc<crate::UIQueue>,
    pub winit_queue: crate::WinitQueue,
    loading: bool,
}

impl Panels {
    pub fn new(ui_queue: Arc<crate::UIQueue>, winit_queue: crate::WinitQueue) -> Self {
        let mut tiles = Tiles::default();
        let tabs = vec![
            tiles.insert_pane(DISASSEMBLY),
            tiles.insert_pane(FUNCTIONS),
            tiles.insert_pane(LOGGING),
        ];
        let root: TileId = tiles.insert_tab_tile(tabs);
        let tree = Tree::new("tree", root, tiles);

        Self {
            tree,
            panes: Tabs::new(),
            ui_queue,
            winit_queue,
            loading: false,
        }
    }

    pub fn listing(&mut self) -> Option<&mut listing::Listing> {
        self.panes.mapping.get_mut(DISASSEMBLY).and_then(|kind| match kind {
            PanelKind::Disassembly(listing) => Some(listing),
            _ => None,
        })
    }

    #[inline]
    pub fn processor(&mut self) -> Option<&Arc<Processor>> {
        self.panes.processor.as_ref()
    }

    #[inline]
    pub fn terminal(&mut self) -> &mut Terminal {
        &mut self.panes.terminal
    }

    pub fn is_loading(&self) -> bool {
        self.loading
    }

    pub fn start_loading(&mut self) {
        // create new donut to restart internal timer
        self.panes.donut = Donut::new(false);
        self.loading = true;
    }

    pub fn stop_loading(&mut self) {
        self.loading = false;
    }

    /// Jump to both the source and the assembly.
    pub fn load_src(&mut self, addr: usize) {
        let file_attr = match self.processor().and_then(|proc| proc.index.get_file_by_addr(addr)) {
            Some(file_attr) => file_attr,
            None => return,
        };

        if let Ok(src) = std::fs::read_to_string(&file_attr.path) {
            let src = source_code::Source::new(&src, file_attr);
            self.panes.mapping.insert(SOURCE, PanelKind::Source(src));
        }
    }

    pub fn load_binary(&mut self, processor: Processor) {
        let processor = Arc::new(processor);

        self.panes.mapping.insert(
            DISASSEMBLY,
            PanelKind::Disassembly(listing::Listing::new(processor.clone())),
        );

        self.panes.mapping.insert(
            FUNCTIONS,
            PanelKind::Functions(functions::Functions::new(processor.clone())),
        );

        self.panes.processor = Some(processor);
    }

    pub fn ask_for_binary(&self) {
        if let Some(path) = rfd::FileDialog::new().pick_file() {
            self.ui_queue.push(crate::UIEvent::BinaryRequested(path));
        }
    }

    pub fn handle_events(&mut self, events: &mut Vec<egui::Event>) {
        if let Some(listing) = self.listing() {
            listing.record_input(events);
        }

        let empty_index = debugvault::Index::default();
        let index = self.panes.processor.as_ref().map(|proc| &proc.index).unwrap_or(&empty_index);
        self.panes.terminal.record_input(events, index);
    }

    pub fn goto_window(&mut self, tile: Identifier) {
        if let Some(id) = self.tree.tiles.find_pane(&tile) {
            if let Some(parent_id) = self.tree.tiles.parent_of(id) {
                if let Some(Tile::Container(Container::Tabs(tabs))) =
                    self.tree.tiles.get_mut(parent_id)
                {
                    tabs.set_active(id);
                }
            }
        } else {
            let pane = self.tree.tiles.insert_pane(tile);
            let root = self.tree.root.unwrap();

            match self.tree.tiles.get_mut(root) {
                Some(Tile::Container(Container::Tabs(tabs))) => {
                    tabs.add_child(pane);
                    tabs.set_active(pane);
                }
                Some(Tile::Container(Container::Linear(linear))) => {
                    linear.add_child(pane);
                }
                Some(Tile::Container(Container::Grid(grid))) => {
                    grid.add_child(pane);
                }
                Some(Tile::Pane(_)) => {
                    let parent_id = self.tree.tiles.parent_of(root).unwrap();

                    let parent = self.tree.tiles.get_mut(parent_id);
                    if let Some(Tile::Container(container)) = parent {
                        container.add_child(pane);
                    }
                }
                None => unreachable!(),
            }
        }
    }

    /// Show some close/maximize/minimize buttons for the native window.
    #[cfg(any(target_family = "windows", target_os = "linux"))]
    fn top_bar_native(&mut self, ui: &mut egui::Ui) {
        let height = 12.0;
        let close_response = ui.add(egui::Button::new(
            egui::RichText::new(crate::icon!(CROSS)).size(height),
        ));

        if close_response.clicked() {
            self.winit_queue.push(crate::WinitEvent::CloseRequest);
        }

        let maximized_response = ui.add(egui::Button::new(
            egui::RichText::new(crate::icon!(CHECKBOX_UNCHECKED)).size(height),
        ));

        if maximized_response.clicked() {
            self.winit_queue.push(crate::WinitEvent::Fullscreen);
        }

        let minimized_response = ui.add(egui::Button::new(
            egui::RichText::new(crate::icon!(MINUS)).size(height),
        ));

        if minimized_response.clicked() {
            self.winit_queue.push(crate::WinitEvent::Minimize);
        }
    }

    #[cfg(any(target_family = "windows", target_os = "linux"))]
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
        let modifier = if cfg!(target_os = "macos") {
            egui::Modifiers::MAC_CMD
        } else {
            egui::Modifiers::CTRL
        };

        if ctx.input_mut(|i| i.consume_key(modifier, egui::Key::O)) {
            self.ask_for_binary();
        }

        // alt-tab'ing between tabs
        if ctx.input_mut(|i| i.consume_key(egui::Modifiers::CTRL, egui::Key::Tab)) {
            for id in self.tree.active_tiles() {
                if let Some(parent_id) = self.tree.tiles.parent_of(id) {
                    let parent = self.tree.tiles.get_mut(parent_id).unwrap();
                    if let Tile::Container(Container::Tabs(tabs)) = parent {
                        if tabs.children.len() < 2 {
                            continue;
                        }
                        let active_tab = match tabs.active {
                            Some(active) => active,
                            None => continue,
                        };

                        let mut active_idx = 0;
                        for (idx, child) in tabs.children.iter().enumerate() {
                            if *child == active_tab {
                                active_idx = idx;
                            }
                        }

                        let next_idx = (active_idx + 1) % tabs.children.len();
                        let next_id = tabs.children[next_idx];
                        tabs.set_active(next_id);
                        break;
                    }
                }
            }
        }
    }

    pub fn draw(&mut self, ctx: &mut egui::Context) {
        // generic keyboard inputs
        self.input(ctx);

        #[cfg(any(target_family = "windows", target_os = "linux"))]
        egui::TopBottomPanel::top("top bar").show(ctx, |ui| self.top_bar(ui));

        // terminal needs to be rendered last as it can take focus away from other panels
        let terminal = egui::TopBottomPanel::bottom("terminal")
            .min_height(80.0)
            .max_height(510.0)
            .resizable(true)
            .frame({
                egui::Frame::default()
                    .inner_margin(egui::Margin::same(STYLE.separator_width * 2.0))
                    .fill(colors::GRAY35)
            });

        let mut visuals = EGUI.visuals.clone();

        // set alternative background color
        visuals.extreme_bg_color = CONFIG.colors.bg_secondary;
        // disable on-hover highlighting for terminal
        visuals.widgets.active.fg_stroke = egui::Stroke::NONE;
        visuals.widgets.hovered.fg_stroke = egui::Stroke::NONE;

        ctx.set_visuals(visuals);

        let request_focus = self.terminal().should_reset_cursor();
        let term_response = terminal.show(ctx, |ui| {
            let response = ui
                .with_layout(egui::Layout::top_down_justified(egui::Align::Min), |ui| {
                    self.panes.terminal.show(ui)
                });

            response.inner
        });

        ctx.set_visuals(EGUI.visuals.clone());

        let frame = egui::Frame::default().inner_margin(egui::Margin::same(0.0));
        egui::CentralPanel::default().frame(frame).show(ctx, |ui| {
            if self.loading {
                ui.spacing_mut().item_spacing.y = 20.0;
                ui.with_layout(
                    egui::Layout::top_down_justified(egui::Align::Center),
                    |ui| {
                        self.panes.donut.show(ui);
                        log::PROGRESS.show(ui);
                    },
                );
            } else {
                self.tree.ui(&mut self.panes, ui);
            }

            // give focus to terminal if any valid keyboard input happened
            if request_focus {
                ui.ctx().memory_mut(|m| m.request_focus(term_response.inner.id));
            }
        });
    }
}
