mod donut;
mod listing;
mod terminal;

use egui::{Button, RichText};
use egui_dock::{DockArea, DockState};

use std::collections::BTreeMap;
use std::sync::Arc;

type Identifier = &'static str;

const FONT: egui::FontId = egui::FontId::new(14.0, egui::FontFamily::Monospace);

// pub const SOURCE: Identifier = crate::icon!(EMBED2, " Source");
const DISASSEMBLY: Identifier = crate::icon!(PARAGRAPH_LEFT, " Disassembly");
const FUNCTIONS: Identifier = crate::icon!(LIGATURE, " Functions");
const LOGGING: Identifier = crate::icon!(TERMINAL, " Logs");

pub trait Display {
    fn show(&mut self, ui: &mut egui::Ui);
}

enum PanelKind {
    Disassembly(listing::Disassembly),
    Functions(listing::Functions),
    Logging,
}

struct Tabs {
    mapping: BTreeMap<Identifier, PanelKind>,
    pub terminal: terminal::Terminal,
    donut: donut::Donut,
}

impl Tabs {
    fn new() -> Self {
        Self {
            mapping: {
                let mut mapping = BTreeMap::new();
                mapping.insert(LOGGING, PanelKind::Logging);
                mapping
            },
            terminal: terminal::Terminal::new(),
            donut: donut::Donut::new(false),
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
            Some(PanelKind::Logging) => {
                ui.style_mut().wrap = Some(true);

                let area = egui::ScrollArea::vertical()
                    .auto_shrink([false, false])
                    .drag_to_scroll(false)
                    .stick_to_bottom(true);

                area.show(ui, |ui| ui.label(log::LOGGER.lock().unwrap().format()));

                ui.style_mut().wrap = Some(false);
            }
            None => {}
        };
    }
}

pub struct Panels {
    layout: DockState<Identifier>,
    tabs: Tabs,
    proxy: crate::Proxy,
    is_loading: bool,
}

impl Panels {
    pub fn new(proxy: crate::Proxy) -> Self {
        let mut layout = DockState::new(vec![DISASSEMBLY, FUNCTIONS, LOGGING]);

        layout.set_focused_node_and_surface((
            egui_dock::SurfaceIndex::main(),
            egui_dock::NodeIndex::root(),
        ));

        Self {
            layout,
            tabs: Tabs::new(),
            proxy,
            is_loading: false,
        }
    }

    pub fn terminal(&mut self) -> &mut terminal::Terminal {
        &mut self.tabs.terminal
    }

    pub fn toggle_loading(&mut self) {
        self.is_loading = !self.is_loading;
    }

    pub fn load_binary(&mut self, disassembly: Arc<disassembler::Disassembly>) {
        self.tabs.mapping.insert(
            DISASSEMBLY,
            PanelKind::Disassembly(
                listing::Disassembly::new(disassembly.clone())
            )
        );

        self.tabs.mapping.insert(
            FUNCTIONS,
            PanelKind::Functions(
                listing::Functions::new(disassembly.clone())
            )
        );
    }

    fn ask_for_binary(&self) {
        // create dialog popup and get references to the donut and dissasembly
        let dialog = rfd::FileDialog::new().pick_file();

        // load binary
        if let Some(path) = dialog {
            self.proxy.send(crate::CustomEvent::BinaryRequested(path));
        }
    }

    /// Show some close/maximize/minimize buttons for the native window.
    fn top_bar_native(&mut self, ui: &mut egui::Ui) {
        let height = 12.0;
        let close_response = ui.add(Button::new(RichText::new(crate::icon!(CROSS)).size(height)));

        if close_response.clicked() {
            self.proxy.send(crate::CustomEvent::CloseRequest);
        }

        let maximized_response = ui.add(Button::new(
            RichText::new(crate::icon!(CHECKBOX_UNCHECKED)).size(height),
        ));

        if maximized_response.clicked() {
            self.proxy.send(crate::CustomEvent::Fullscreen);
        }

        let minimized_response = ui.add(Button::new(RichText::new(crate::icon!(MINUS)).size(height)));

        if minimized_response.clicked() {
            self.proxy.send(crate::CustomEvent::Minimize);
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
                    self.proxy.send(crate::CustomEvent::CloseRequest);
                    ui.close_menu();
                }
            });

            ui.menu_button("Windows", |ui| {
                let mut goto_window = |title| match self.layout.find_tab(&title) {
                    Some(tab) => self.layout.set_active_tab(tab),
                    None => self.layout.push_to_first_leaf(title),
                };

                if ui.button(DISASSEMBLY).clicked() {
                    goto_window(DISASSEMBLY);
                    ui.close_menu();
                }

                if ui.button(FUNCTIONS).clicked() {
                    goto_window(FUNCTIONS);
                    ui.close_menu();
                }

                if ui.button(LOGGING).clicked() {
                    goto_window(LOGGING);
                    ui.close_menu();
                }
            });

            ui.with_layout(egui::Layout::right_to_left(egui::Align::Max), |ui| {
                ui.spacing_mut().item_spacing.x = 5.0;
                self.top_bar_native(ui);
            });
        });

        if bar.response.interact(egui::Sense::click()).double_clicked() {
            self.proxy.send(crate::CustomEvent::Fullscreen);
        }

        if bar.response.interact(egui::Sense::drag()).dragged() {
            self.proxy.send(crate::CustomEvent::DragWindow);
        }
    }

    fn input(&mut self, ctx: &mut egui::Context) {
        if ctx.input_mut(|i| i.consume_key(egui::Modifiers::CTRL, egui::Key::O)) {
            self.ask_for_binary();
        }

        // alt-tab'ing between tabs
        if ctx.input_mut(|i| i.consume_key(egui::Modifiers::CTRL, egui::Key::Tab)) {
            let (focused_surface, focused_node) = self.layout
                .focused_leaf()
                .unwrap_or((egui_dock::SurfaceIndex::main(), egui_dock::NodeIndex::root()));

            // don't do tab'ing if there are no tabs
            if self.layout.main_surface().num_tabs() == 0 {
                return;
            }

            let focused = &mut self.layout[focused_surface][focused_node];
            if let egui_dock::Node::Leaf { tabs, active, .. } = focused {
                if active.0 != tabs.len() - 1 {
                    let tab_idx = active.0 + 1;
                    self.layout.set_active_tab(
                        (focused_surface, focused_node, egui_dock::TabIndex(tab_idx)),
                    );
                } else {
                    self.layout.set_active_tab(
                        (focused_surface, focused_node, egui_dock::TabIndex(0)),
                    );
                }
            }
        }
    }

    pub fn draw(&mut self, ctx: &mut egui::Context) {
        // generic keyboard inputs
        self.input(ctx);

        egui::TopBottomPanel::top("top bar").show(ctx, |ui| self.top_bar(ui));

        let terminal = egui::TopBottomPanel::bottom("terminal")
            .min_height(80.0)
            .max_height(510.0)
            .resizable(true)
            .frame({
                let mut margin = crate::style::EGUI.spacing.window_margin;
                margin.top = crate::style::STYLE.separator_width * 2.0;

                egui::Frame::default()
                    .outer_margin(egui::Margin {
                        top: crate::style::STYLE.separator_width * 2.0,
                        ..Default::default()
                    })
                    .inner_margin(margin)
                    .fill(tokenizing::colors::GRAY35)
            });

        // disable on-hover highlighting for terminal
        let mut visuals = crate::style::EGUI.visuals.clone();
        visuals.widgets.active.fg_stroke = egui::Stroke::NONE;
        visuals.widgets.hovered.fg_stroke = egui::Stroke::NONE;
        ctx.set_visuals(visuals);

        terminal.show(ctx, |ui| {
            ui.with_layout(egui::Layout::bottom_up(egui::Align::Min), |ui| {
                self.tabs.terminal.show(ui);
            })
        });

        // re-enable on-hover highlighting
        let visuals = crate::style::EGUI.visuals.clone();
        ctx.set_visuals(visuals);

        // draw the primary panel
        egui::CentralPanel::default()
            .frame({
                egui::Frame::default().inner_margin(egui::Margin {
                    top: crate::style::STYLE.separator_width,
                    ..Default::default()
                })
            })
            .show(ctx, |ui| {
                if self.is_loading {
                    let layout = egui::Layout::centered_and_justified(egui::Direction::LeftToRight);

                    ui.with_layout(layout, |ui| self.tabs.donut.show(ui));
                } else {
                    let surface = self.layout.main_surface();
                    let tab_count = surface.num_tabs();
                    let style = crate::style::DOCK.clone();

                    DockArea::new(&mut self.layout)
                        .style(style)
                        .draggable_tabs(tab_count > 1)
                        .show_close_buttons(false)
                        .show_window_close_buttons(true)
                        .show_window_collapse_buttons(false)
                        .tab_context_menus(false)
                        .show_inside(ui, &mut self.tabs);
                }
            });
    }
}
