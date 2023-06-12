mod donut;
mod backend;
mod winit_backend;
mod egui_backend;
mod icons;
mod style;
mod texture;
mod utils;

use egui::text::LayoutJob;
use egui::epaint::text::TextWrapping;
use egui_dock::tree::Tree;
use once_cell::sync::{OnceCell, Lazy};
use pollster::FutureExt;
use tokenizing::Token;
use winit::event::{Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoopBuilder};

use crate::disassembly::Disassembly;
use egui_backend::Pipeline;
use backend::Backend;
use winit_backend::{CustomEvent, Platform, PlatformDescriptor};

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::JoinHandle;
use std::time::Instant;

pub static WINDOW: OnceCell<Arc<winit::window::Window>> = OnceCell::new();
static WIDTH: u32 = 1200;
static HEIGHT: u32 = 800;
static STYLE: Lazy<style::Style> = Lazy::new(style::Style::default);

type Title = &'static str;
type DisassThread = JoinHandle<Result<Disassembly, crate::disassembly::DecodeError>>;

#[derive(Debug)]
pub enum Error {
    /// Failure from wgpu_glyph to draw text.
    DrawText(String),

    /// Failed to create a winit window.
    WindowCreation,

    /// Failed to to create a surface.
    SurfaceCreation(wgpu::CreateSurfaceError),

    /// Failed to find a adapter that supports our surface.
    AdapterRequest,

    /// Failed to find a device that meets our adapter's limits.
    DeviceRequest(wgpu::RequestDeviceError),

    /// Invalid data given to the png decoder.
    PngDecode,

    /// Unsupported texture format produced by the png decoder.
    PngFormat,

    /// File is not found.
    NotFound(std::path::PathBuf),
}

#[derive(PartialEq)]
enum TabKind {
    Source,
    Listing,
}

struct Buffers {
    inner: HashMap<Title, TabKind>,
    dissasembly: Option<Arc<Disassembly>>,

    cached_range: std::ops::Range<usize>,
    cached_dissasembly: Vec<Token<'static>>,
}

impl Buffers {
    fn new(buffers: HashMap<Title, TabKind>) -> Self {
        Self {
            inner: buffers,
            dissasembly: None,
            cached_range: std::ops::Range { start: 0, end: 0 },
            cached_dissasembly: Vec::new(),
        }
    }

    fn has_multiple_tabs(&self) -> bool {
        self.inner.len() != 1
    }

    fn show_listing(&mut self, ui: &mut egui::Ui) {
        let dissasembly = match self.dissasembly {
            Some(ref dissasembly) => dissasembly,
            None => return,
        };

        let text_style = egui::TextStyle::Body;
        let row_height = ui.text_style_height(&text_style);
        let total_rows = dissasembly.proc.instruction_count();
        let area = egui::ScrollArea::vertical().auto_shrink([false, false]);

        let style = STYLE.egui();
        let font_id = egui::TextStyle::Body.resolve(&style);

        area.show_rows(ui, row_height, total_rows, |ui, row_range| {
            if row_range != self.cached_range {
                self.cached_dissasembly = dissasembly.listing(row_range);
            }

            let tokens = &self.cached_dissasembly[..];
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

fn show_source(ui: &mut egui::Ui) {
    ui.label("todo");
}

impl egui_dock::TabViewer for Buffers {
    type Tab = Title;

    fn ui(&mut self, ui: &mut egui::Ui, title: &mut Self::Tab) {
        match self.inner.get(title) {
            Some(TabKind::Source) => show_source(ui),
            Some(TabKind::Listing) => self.show_listing(ui),
            _ => return,
        };
    }

    fn title(&mut self, title: &mut Self::Tab) -> egui::WidgetText {
        (*title).into()
    }

    fn on_close(&mut self, tab: &mut Self::Tab) -> bool {
        if self.inner.len() == 1 {
            false
        } else {
            self.inner.remove(tab);
            true
        }
    }
}

pub struct RenderContext {
    tabs: Tree<Title>,
    buffers: Buffers,

    style: style::Style,
    window: Arc<winit::window::Window>,
    donut: donut::Donut,
    show_donut: Arc<AtomicBool>,
    timer60: utils::Timer,
    dissasembly: Option<Arc<Disassembly>>,
    disassembling_thread: Option<DisassThread>,

    #[cfg(target_family = "windows")]
    unwindowed_size: winit::dpi::PhysicalSize<u32>,
    #[cfg(target_family = "windows")]
    unwindowed_pos: winit::dpi::PhysicalPosition<i32>,
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
    let mut platform = Platform::new(PlatformDescriptor {
        physical_width: 580,
        physical_height: 300,
        scale_factor: window.scale_factor(),
        style: STYLE.egui(),
        winit: event_loop.create_proxy(),
    });

    let disass_title = crate::icon!(PARAGRAPH_LEFT, "Disassembly");
    let source_title = crate::icon!(EMBED2, "Source");

    let mut egui_rpass = Pipeline::new(&backend.device, backend.surface_cfg.format, 1);
    let mut tabs = Tree::new(vec![disass_title, source_title]);

    tabs.set_focused_node(egui_dock::NodeIndex::root());

    let buffers = HashMap::from([
        (disass_title, TabKind::Listing),
        (source_title, TabKind::Source),
    ]);

    let mut ctx = RenderContext {
        tabs,
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
    };

    if let Some(ref path) = crate::ARGS.path {
        let show_donut = Arc::clone(&ctx.show_donut);
        ctx.dissasembly = None;
        ctx.disassembling_thread = Some(std::thread::spawn(move || {
            Disassembly::new(path, show_donut)
        }));
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
	    },
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::Resized(size) => backend.resize(size),
                WindowEvent::CloseRequested => *control = ControlFlow::Exit,
                WindowEvent::DroppedFile(path) => {
                    let show_donut = Arc::clone(&ctx.show_donut);
                    ctx.dissasembly = None;
                    ctx.disassembling_thread =
                        Some(std::thread::spawn(|| Disassembly::new(path, show_donut)));
                }
                _ => {}
            },
            Event::MainEventsCleared => {
                handle_post_render(&mut ctx);
            }
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
