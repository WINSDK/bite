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

use crate::disassembly::{Disassembly, DisassemblyView};
use crate::terminal::Terminal;
use backend::Backend;
use debugger::{Debugger, Process};
use egui::{Button, FontId, RichText};
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

const LIST_FONT: FontId = egui::FontId::new(14.0, egui::FontFamily::Monospace);

const WIDTH: u32 = 1200;
const HEIGHT: u32 = 800;

const DISASS_TITLE: &str = crate::icon!(PARAGRAPH_LEFT, " Disassembly");
const FUNCS_TITLE: &str = crate::icon!(LIGATURE, " Functions");
const SOURCE_TITLE: &str = crate::icon!(EMBED2, " Source");
const LOG_TITLE: &str = crate::icon!(TERMINAL, " Logs");

type Title = &'static str;
type DisassThread = JoinHandle<Result<Disassembly, crate::disassembly::DecodeError>>;

pub enum Error {
    WindowCreation,
    SurfaceCreation(wgpu::CreateSurfaceError),
    AdapterRequest,
    DeviceRequest(wgpu::RequestDeviceError),
    InvalidTextureId(egui::TextureId),
    PngDecode,
    PngFormat,
    NotFound(std::path::PathBuf),
    Exit,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
            Self::Exit => Ok(()),
        }
    }
}

pub fn tokens_to_layoutjob(tokens: Vec<Token>) -> LayoutJob {
    let mut job = LayoutJob::default();

    for token in tokens {
        job.append(
            &token.text,
            0.0,
            egui::TextFormat {
                font_id: LIST_FONT,
                color: token.color,
                ..Default::default()
            },
        );
    }

    job
}

pub struct RenderContext {
    show_donut: Arc<AtomicBool>,

    pub dissasembly: Option<Arc<Disassembly>>,
    disassembling_thread: Option<DisassThread>,
}

impl RenderContext {
    pub fn start_disassembling(&mut self, path: impl AsRef<std::path::Path> + 'static + Send) {
        let show_donut = Arc::clone(&self.show_donut);

        self.process_path = Some(path.as_ref().to_path_buf());
        self.disassembling_thread = Some(std::thread::spawn(move || {
            // self.show_donut = true
            Disassembly::parse(path, show_donut)
            // self.show_donut = false
        }));
    }

    pub fn start_debugging(
        &mut self,
        path: impl AsRef<std::path::Path> + 'static + Send,
        args: Vec<String>,
    ) {
        #[cfg(target_os = "linux")]
        std::thread::spawn(move || {
            // the debugger must not be moved to a different thread,
            // not sure why this is the case
            let mut session = Debugger::spawn(path, args).unwrap();

            session.trace_syscalls(true);
            session.run_to_end().unwrap();
        });
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

            if ui.button(LOG_TITLE).clicked() {
                goto_window(LOG_TITLE);
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
    let mut panels = Tree::new(vec![DISASS_TITLE, FUNCS_TITLE, LOG_TITLE]);

    panels.set_focused_node(egui_dock::NodeIndex::root());

    let buffers = HashMap::from([
        (DISASS_TITLE, TabKind::Listing),
        (FUNCS_TITLE, TabKind::Functions),
        (SOURCE_TITLE, TabKind::Source),
        (LOG_TITLE, TabKind::Log),
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
                match backend.redraw(&mut ctx, &mut platform, &mut egui_rpass) {
                    Err(Error::Exit) => *control = ControlFlow::Exit,
                    Err(err) => crate::warning!("{err:?}"),
                    Ok(()) => {}
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
        ctx.donut.step();
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
                    ctx.buffers.disassembly = Some(Arc::clone(&dissasembly));
                }
            }

            // mark the disassembling thread as not loading anything
            ctx.disassembling_thread = None;
        }
    }

    ctx.window.request_redraw();
}
