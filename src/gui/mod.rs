mod donut;
mod egui_backend;
mod icons;
mod quad;
mod style;
mod texture;
mod utils;
mod window;
mod winit_backend;

use egui_dock::tree::Tree;
use once_cell::sync::OnceCell;
use pollster::FutureExt;
use winit::event::{Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoopBuilder};

use crate::disassembly::Disassembly;
use egui_backend::Pipeline;
use winit_backend::{CustomEvent, Platform, PlatformDescriptor};
use window::Backend;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::JoinHandle;
use std::time::Instant;
use std::collections::HashMap;

pub static WINDOW: OnceCell<Arc<winit::window::Window>> = OnceCell::new();
static WIDTH: u32 = 580;
static HEIGHT: u32 = 580;

type Title = &'static str;
type DisassThread = JoinHandle<Result<Disassembly, crate::disassembly::DecodeError>>;

#[derive(Debug)]
pub enum Error {
    /// Failure to retrieve the current texture from our surface.
    DrawTexture(wgpu::SurfaceError),

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
    Source(String),
    Listing(usize),
}

struct Buffers {
    inner: HashMap<Title, TabKind>,
}

impl Buffers {
    fn has_multiple_tabs(&self) -> bool {
        self.inner.len() != 1
    }
}

impl egui_dock::TabViewer for Buffers {
    type Tab = Title;

    fn ui(&mut self, ui: &mut egui::Ui, title: &mut Self::Tab) {
        match self.inner.get(title) {
            Some(TabKind::Source(src)) => ui.label(src),
            Some(TabKind::Listing(id)) => ui.label(id.to_string()),
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
    dissasembly: Option<Disassembly>,
    disassembling_thread: Option<DisassThread>,

    #[cfg(target_family = "windows")]
    unwindowed_size: PhysicalSize<u32>,
    #[cfg(target_family = "windows")]
    unwindowed_pos: PhysicalPosition<i32>,
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

    let style = style::Style::default();
    let mut backend = Backend::new(&window).block_on()?;
    let mut platform = Platform::new(PlatformDescriptor {
        physical_width: 580,
        physical_height: 300,
        scale_factor: window.scale_factor(),
        style: style.egui(),
        winit: event_loop.create_proxy(),
    });

    let source_title = crate::icon!(EMBED2, "Source");
    let disass_title = crate::icon!(PARAGRAPH_LEFT, "Disassembly");

    let mut egui_rpass = Pipeline::new(&backend, 1);
    let mut tabs = Tree::new(vec![source_title, disass_title]);

    tabs.set_focused_node(egui_dock::NodeIndex::root());

    let buffers = HashMap::from([
        (source_title, TabKind::Listing(1600)),
        (
            disass_title,
            TabKind::Source(String::from("line 1\nline 2\nline 3")),
        ),
    ]);

    let mut ctx = RenderContext {
        tabs,
        buffers: Buffers { inner: buffers },
        style,
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
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::Resized(size) => backend.resize(&mut ctx, size),
                WindowEvent::CloseRequested => *control = ControlFlow::Exit,
                WindowEvent::DroppedFile(path) => {
                    let show_donut = Arc::clone(&ctx.show_donut);
                    ctx.dissasembly = None;
                    ctx.disassembling_thread =
                        Some(std::thread::spawn(|| Disassembly::new(path, show_donut)));
                }
                _ => {}
            }
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
                Ok(Ok(val)) => ctx.dissasembly = Some(val),
            }

            // mark the disassembling thread as not loading anything
            ctx.disassembling_thread = None;
        }
    }

    ctx.window.request_redraw();
}
