mod backend;
mod donut;
mod egui_backend;
mod icons;
mod style;
mod texture;
mod utils;
mod winit_backend;

use egui::text::LayoutJob;
use egui_dock::tree::Tree;
use once_cell::sync::{Lazy, OnceCell};
use pollster::FutureExt;
use tokenizing::Token;
use winit::event::{Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoopBuilder};

use crate::disassembly::Disassembly;
use crate::terminal::Terminal;
use backend::Backend;
use debugger::{Debugger, Process};
use egui::{Button, RichText};
use egui_backend::Pipeline;
use winit_backend::{CustomEvent, Platform, PlatformDescriptor};

use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::JoinHandle;
use std::time::Instant;

pub static WINDOW: OnceCell<Arc<winit::window::Window>> = OnceCell::new();
pub static STYLE: Lazy<style::Style> = Lazy::new(style::Style::default);

const WIDTH: u32 = 1200;
const HEIGHT: u32 = 800;

const DISASS_TITLE: &str = crate::icon!(PARAGRAPH_LEFT, " Disassembly");
const SOURCE_TITLE: &str = crate::icon!(EMBED2, " Source");
const FUNCS_TITLE: &str = crate::icon!(LIGATURE, " Functions");

type Title = &'static str;
type DisassThread = JoinHandle<Result<Disassembly, crate::disassembly::DecodeError>>;

pub enum Error {
    DrawText(String),
    WindowCreation,
    SurfaceCreation(wgpu::CreateSurfaceError),
    AdapterRequest,
    DeviceRequest(wgpu::RequestDeviceError),
    InvalidTextureId(egui::TextureId),
    PngDecode,
    PngFormat,
    NotFound(std::path::PathBuf),
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DrawText(msg) => f.write_str(msg),
            Self::WindowCreation => f.write_str("Failed to create a window."),
            Self::SurfaceCreation(..) => f.write_str("Failed to create a surface."),
            Self::AdapterRequest => {
                f.write_str("Failed to find a adapter that supports our surface.")
            }
            Self::DeviceRequest(..) => {
                f.write_str("Failed to find a device that meets our adapter's limits.")
            }
            Self::InvalidTextureId(id) => {
                f.write_fmt(format_args!("Egui texture id '{id:?}' was invalid."))
            }
            Self::PngDecode => f.write_str("Invalid data given to the png decoder."),
            Self::PngFormat => {
                f.write_str("Unsupported texture format produced by the png decoder.")
            }
            Self::NotFound(path) => {
                f.write_fmt(format_args!("Failed to find path: '{}'", path.display()))
            }
        }
    }
}

pub struct RenderContext {
    panels: Tree<Title>,
    pub buffers: Buffers,

    style: style::Style,
    window: Arc<winit::window::Window>,
    donut: donut::Donut,
    show_donut: Arc<AtomicBool>,
    timer60: utils::Timer,
    pub dissasembly: Option<Arc<Disassembly>>,
    disassembling_thread: Option<DisassThread>,

    #[cfg(target_family = "windows")]
    unwindowed_size: winit::dpi::PhysicalSize<u32>,
    #[cfg(target_family = "windows")]
    unwindowed_pos: winit::dpi::PhysicalPosition<i32>,

    terminal: Terminal,

    pub process_path: Option<std::path::PathBuf>,
    pub terminal_prompt: String,
}

impl RenderContext {
    pub fn start_disassembling(&mut self, path: impl AsRef<std::path::Path> + 'static + Send) {
        let show_donut = Arc::clone(&self.show_donut);

        self.process_path = Some(path.as_ref().to_path_buf());
        self.disassembling_thread = Some(std::thread::spawn(move || {
            Disassembly::new(path, show_donut)
        }));
    }

    pub fn start_debugging(
        &mut self,
        path: impl AsRef<std::path::Path> + 'static + Send,
        args: &[&str],
    ) {
        let mut session = Debugger::spawn(path, args).unwrap();

        #[cfg(target_os = "linux")]
        std::thread::spawn(move || {
            session.run_to_end().unwrap();
        });
    }
}

#[derive(Debug, PartialEq, Eq)]
enum TabKind {
    Source,
    Listing,
    Functions,
}

pub struct Buffers {
    mapping: HashMap<Title, TabKind>,
    dissasembly: Option<Arc<Disassembly>>,

    pub updated_offset: Option<usize>,

    cached_diss_range: std::ops::Range<usize>,
    cached_diss: Vec<Token>,

    cached_funcs_range: std::ops::Range<usize>,
    cached_funcs: Vec<Token>,
}

impl Buffers {
    fn new(mapping: HashMap<Title, TabKind>) -> Self {
        Self {
            mapping,
            dissasembly: None,
            updated_offset: None,
            cached_diss_range: std::ops::Range { start: 0, end: 0 },
            cached_diss: Vec::new(),
            cached_funcs_range: std::ops::Range { start: 0, end: 0 },
            cached_funcs: Vec::new(),
        }
    }

    fn show_listing(&mut self, ui: &mut egui::Ui) {
        let dissasembly = match self.dissasembly {
            Some(ref dissasembly) => dissasembly,
            None => return,
        };

        let text_style = egui::TextStyle::Body;
        let row_height = ui.text_style_height(&text_style);
        let total_rows = dissasembly.proc.instruction_count();
        let font_id = text_style.resolve(STYLE.egui());

        let area = egui::ScrollArea::both().auto_shrink([false, false]).drag_to_scroll(false);
        let area = match std::mem::take(&mut self.updated_offset) {
            Some(offset) => area.scroll_offset(egui::vec2(0.0, offset as f32 * row_height)),
            None => area,
        };

        area.show_rows(ui, row_height, total_rows, |ui, row_range| {
            if row_range != self.cached_diss_range {
                self.cached_diss = dissasembly.listing(row_range);
            }

            let tokens = &self.cached_diss[..];
            let mut job = LayoutJob::default();

            for token in tokens {
                job.append(
                    &token.text,
                    0.0,
                    egui::TextFormat {
                        font_id: font_id.clone(),
                        color: *token.color,
                        ..Default::default()
                    },
                );
            }

            ui.label(job);
        });
    }

    fn show_functions(&mut self, ui: &mut egui::Ui) {
        let dissasembly = match self.dissasembly {
            Some(ref dissasembly) => dissasembly,
            None => return,
        };

        let text_style = egui::TextStyle::Small;
        let row_height = ui.text_style_height(&text_style);
        let total_rows = dissasembly.symbols.named_len();

        let area = egui::ScrollArea::both().auto_shrink([false, false]).drag_to_scroll(false);

        let font_id = text_style.resolve(&STYLE.egui());

        area.show_rows(ui, row_height, total_rows, |ui, row_range| {
            if row_range != self.cached_funcs_range {
                self.cached_funcs = dissasembly.functions(row_range);
            }

            let tokens = &self.cached_funcs[..];
            let mut job = LayoutJob::default();

            for token in tokens {
                job.append(
                    &token.text,
                    0.0,
                    egui::TextFormat {
                        font_id: font_id.clone(),
                        color: *token.color,
                        ..Default::default()
                    },
                );
            }

            ui.label(job);
        });
    }
}

impl egui_dock::TabViewer for Buffers {
    type Tab = Title;

    fn ui(&mut self, ui: &mut egui::Ui, title: &mut Self::Tab) {
        egui::Frame::none().outer_margin(STYLE.separator_width).show(ui, |ui| {
            match self.mapping.get(title) {
                Some(TabKind::Source) => {
                    ui.label("todo");
                }
                Some(TabKind::Functions) => self.show_functions(ui),
                Some(TabKind::Listing) => self.show_listing(ui),
                None => return,
            };
        });
    }

    fn title(&mut self, title: &mut Self::Tab) -> egui::WidgetText {
        (*title).into()
    }
}

fn top_bar(ui: &mut egui::Ui, ctx: &mut RenderContext, platform: &mut Platform) {
    let bar = egui::menu::bar(ui, |ui| {
        ui.menu_button("File", |ui| {
            if ui.button(crate::icon!(FOLDER_OPEN, " Open")).clicked() {
                backend::ask_for_binary(ctx);
                ui.close_menu();
            }

            if ui.button(crate::icon!(CROSS, " Exit")).clicked() {
                platform.send_event(CustomEvent::CloseRequest);
                ui.close_menu();
            }
        });

        ui.menu_button("Windows", |ui| {
            let mut goto_window = |title| match ctx.panels.find_tab(&title) {
                Some((node_idx, tab_idx)) => ctx.panels.set_active_tab(node_idx, tab_idx),
                None => ctx.panels.push_to_first_leaf(title),
            };

            if ui.button(DISASS_TITLE).clicked() {
                goto_window(DISASS_TITLE);
                ui.close_menu();
            }

            if ui.button(SOURCE_TITLE).clicked() {
                goto_window(SOURCE_TITLE);
                ui.close_menu();
            }

            if ui.button(FUNCS_TITLE).clicked() {
                goto_window(FUNCS_TITLE);
                ui.close_menu();
            }
        });

        ui.with_layout(egui::Layout::right_to_left(egui::Align::Max), |ui| {
            ui.spacing_mut().item_spacing.x = 5.0;
            top_bar_native(ui, platform, ctx);
        });
    });

    if bar.response.interact(egui::Sense::click()).double_clicked() {
        backend::fullscreen(ctx);
    }

    if bar.response.interact(egui::Sense::drag()).dragged() {
        platform.start_dragging();
    } else {
        platform.stop_dragging();
    }
}

/// Show some close/maximize/minimize buttons for the native window.
fn top_bar_native(ui: &mut egui::Ui, platform: &mut Platform, ctx: &mut RenderContext) {
    let height = 12.0;
    let close_response = ui.add(Button::new(RichText::new(crate::icon!(CROSS)).size(height)));

    if close_response.clicked() {
        platform.send_event(CustomEvent::CloseRequest);
    }

    let maximized_response = ui.add(Button::new(
        RichText::new(crate::icon!(CHECKBOX_UNCHECKED)).size(height),
    ));

    if maximized_response.clicked() {
        backend::fullscreen(ctx);
    }

    let minimized_response = ui.add(Button::new(RichText::new(crate::icon!(MINUS)).size(height)));

    if minimized_response.clicked() {
        ctx.window.set_minimized(true);
    }
}

fn tabbed_panel(ui: &mut egui::Ui, ctx: &mut RenderContext) {
    let tab_count = ctx.panels.num_tabs();

    egui_dock::DockArea::new(&mut ctx.panels)
        .style(ctx.style.dock().clone())
        .draggable_tabs(tab_count > 1)
        .show_inside(ui, &mut ctx.buffers);
}

fn terminal(ui: &mut egui::Ui, ctx: &mut RenderContext) {
    ui.style_mut().wrap = Some(true);

    let area = egui::ScrollArea::vertical().auto_shrink([false, false]).drag_to_scroll(false);

    area.show(ui, |ui| {
        let mut output = LayoutJob::default();

        let text_style = egui::TextStyle::Body;
        let font_id = text_style.resolve(STYLE.egui());

        output.append(
            &ctx.terminal_prompt,
            0.0,
            egui::TextFormat {
                font_id: font_id.clone(),
                color: STYLE.egui().noninteractive().fg_stroke.color,
                ..Default::default()
            },
        );

        output.append(
            "(bite) ",
            0.0,
            egui::TextFormat {
                font_id: font_id.clone(),
                color: STYLE.egui().noninteractive().fg_stroke.color,
                ..Default::default()
            },
        );

        ctx.terminal.format(&mut output, font_id);
        ui.label(output);
    });

    ui.style_mut().wrap = Some(false);
}

pub fn init() -> Result<(), Error> {
    let event_loop = EventLoopBuilder::<CustomEvent>::with_user_event().build();

    let window = {
        #[cfg(target_os = "linux")]
        let decode = utils::decode_png_bytes(include_bytes!("../../assets/iconx64.png"));
        #[cfg(any(target_os = "windows", target_os = "macos"))]
        let decode = utils::decode_png_bytes(include_bytes!("../../assets/iconx256.png"));

        let mut icon = None;
        if let Ok(png) = decode {
            icon = winit::window::Icon::from_rgba(png.data, png.width, png.height).ok();
        }

        Arc::new(utils::generate_window("bite", icon, &event_loop)?)
    };

    WINDOW.set(Arc::clone(&window)).unwrap();

    let mut backend = Backend::new(&window).block_on()?;

    let mut egui_rpass = Pipeline::new(&backend.device, backend.surface_cfg.format, 1);
    let mut panels = Tree::new(vec![DISASS_TITLE, FUNCS_TITLE]);

    panels.set_focused_node(egui_dock::NodeIndex::root());

    let buffers = HashMap::from([
        (DISASS_TITLE, TabKind::Listing),
        (FUNCS_TITLE, TabKind::Functions),
        (SOURCE_TITLE, TabKind::Source),
    ]);

    let mut ctx = RenderContext {
        panels,
        buffers: Buffers::new(buffers),
        style: STYLE.clone(),
        window: Arc::clone(&window),
        donut: donut::Donut::new(true),
        show_donut: Arc::new(AtomicBool::new(false)),
        timer60: utils::Timer::new(60),
        dissasembly: None,
        disassembling_thread: None,
        #[cfg(target_family = "windows")]
        unwindowed_size: window.outer_size(),
        #[cfg(target_family = "windows")]
        unwindowed_pos: window.outer_position().unwrap_or_default(),
        terminal: Terminal::new(),
        process_path: None,
        terminal_prompt: String::new(),
    };

    let mut platform = Platform::new(PlatformDescriptor {
        physical_width: 580,
        physical_height: 300,
        scale_factor: window.scale_factor() as f32,
        style: STYLE.egui().clone(),
        winit: event_loop.create_proxy(),
    });

    if let Some(ref path) = crate::ARGS.path {
        ctx.start_disassembling(path);
    }

    let start_time = Instant::now();

    event_loop.run(move |event, _, control| {
        // Pass the winit events to the platform integration
        platform.handle_event(&event);

        match event {
            Event::RedrawRequested(..) => {
                // update time elapsed
                platform.update_time(start_time.elapsed().as_secs_f64());

                // draw ui
                if let Err(err) = backend.redraw(&mut ctx, &mut platform, &mut egui_rpass) {
                    crate::warning!("{err:?}");
                }
            }
            Event::UserEvent(CustomEvent::CloseRequest) => *control = ControlFlow::Exit,
            Event::UserEvent(CustomEvent::DragWindow) => {
                let _ = ctx.window.drag_window();
            }
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::Resized(size) => backend.resize(size),
                WindowEvent::CloseRequested => *control = ControlFlow::Exit,
                WindowEvent::DroppedFile(path) => ctx.start_disassembling(path),
                _ => {}
            },
            Event::MainEventsCleared => handle_post_render(&mut ctx),
            _ => {}
        }
    })
}

fn handle_post_render(ctx: &mut RenderContext) {
    if ctx.show_donut.load(Ordering::Relaxed) && ctx.timer60.reached() {
        ctx.donut.update_frame();
        ctx.timer60.reset();
    }

    // if there is a binary being loaded
    if let Some(true) = ctx.disassembling_thread.as_ref().map(JoinHandle::is_finished) {
        let thread = ctx.disassembling_thread.take().unwrap();

        // check if it's finished loading
        if thread.is_finished() {
            // store the loaded binary
            match thread.join() {
                Err(err) => {
                    ctx.show_donut.store(false, Ordering::Relaxed);
                    crate::warning!("{err:?}");
                }
                Ok(Err(err)) => {
                    ctx.show_donut.store(false, Ordering::Relaxed);
                    crate::warning!("{err:?}");
                }
                Ok(Ok(val)) => {
                    let dissasembly = Arc::new(val);

                    ctx.dissasembly = Some(Arc::clone(&dissasembly));
                    ctx.buffers.dissasembly = Some(Arc::clone(&dissasembly));
                }
            }

            // mark the disassembling thread as not loading anything
            ctx.disassembling_thread = None;
        }
    }

    ctx.window.request_redraw();
}
