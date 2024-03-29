mod common;
mod fmt;
mod icon;
mod interp;
mod panels;
mod style;
pub mod unix;
mod wgpu_backend;
pub mod widgets;
pub mod windows;
mod winit_backend;

use copypasta::ClipboardProvider;
use std::sync::Arc;
use winit::event::{Event, WindowEvent};
use winit::event_loop::{EventLoop, EventLoopBuilder};

/// Print to the terminal.
#[macro_export]
macro_rules! tprint {
    ($dst:expr, $($arg:tt)*) => {{
        #[allow(unused_imports)]
        use ::std::fmt::Write;
        let _ = writeln!($dst, $($arg)*);
    }};
}

pub enum Error {
    WindowCreation,
    SurfaceCreation(wgpu::CreateSurfaceError),
    AdapterRequest,
    DeviceRequest(wgpu::RequestDeviceError),
    InvalidTextureId(egui::TextureId),
    PngDecode,
    PngFormat,
    NotFound(std::path::PathBuf),
    EventLoopCreation(winit::error::EventLoopError),
}

type Window = winit::window::Window;

pub trait Target: Sized {
    #[cfg(target_family = "windows")]
    fn new(arch_desc: windows::ArchDescriptor) -> Self;

    #[cfg(target_family = "unix")]
    fn new() -> Self;

    fn create_window<T>(
        title: &str,
        width: u32,
        height: u32,
        event_loop: &winit::event_loop::EventLoop<T>,
    ) -> Result<Window, Error>;

    fn fullscreen(&mut self, window: &Window);

    fn clipboard(window: &Window) -> Box<dyn ClipboardProvider>;
}

/// A custom event type for the winit backend.
pub enum WinitEvent {
    CloseRequest,
    DragWindow,
    Fullscreen,
    Minimize,
}

/// Global UI events.
pub enum UIEvent {
    BinaryRequested(std::path::PathBuf),
    BinaryFailed(processor::Error),
    BinaryLoaded(processor::Processor),
}

#[derive(Clone)]
pub struct WinitQueue {
    inner: winit::event_loop::EventLoopProxy<WinitEvent>,
}

impl WinitQueue {
    pub fn push(&self, event: WinitEvent) {
        if let Err(..) = self.inner.send_event(event) {
            panic!("missing an event loop to handle event");
        }
    }
}

pub struct UIQueue {
    inner: crossbeam_queue::ArrayQueue<UIEvent>,
}

impl UIQueue {
    pub fn push(&self, event: UIEvent) {
        let _ = self.inner.push(event);
    }
}

pub struct UI<Arch: Target> {
    arch: Arch,
    window: &'static Window, // Box::leak'd
    event_loop: Option<EventLoop<WinitEvent>>,
    panels: panels::Panels,
    instance: wgpu_backend::Instance<'static>,
    egui_render_pass: wgpu_backend::egui::Pipeline,
    platform: winit_backend::Platform,
    ui_queue: Arc<UIQueue>,
}

impl<Arch: Target> UI<Arch> {
    pub fn new() -> Result<Self, Error> {
        let event_loop = EventLoopBuilder::<WinitEvent>::with_user_event()
            .build()
            .map_err(Error::EventLoopCreation)?;

        let window = Arch::create_window("bite", 1200, 1200, &event_loop)?;
        let window: &'static Window = Box::leak(Box::new(window));

        #[cfg(target_family = "windows")]
        let arch = Arch::new(windows::ArchDescriptor {
            initial_size: window.outer_size(),
            initial_pos: window.outer_position().unwrap_or_default(),
        });

        #[cfg(target_family = "unix")]
        let arch = Arch::new();

        let ui_queue = Arc::new(UIQueue {
            inner: crossbeam_queue::ArrayQueue::new(100),
        });

        let winit_queue = WinitQueue {
            inner: event_loop.create_proxy(),
        };

        let panels = panels::Panels::new(ui_queue.clone(), winit_queue.clone());
        let instance = wgpu_backend::Instance::new(&window)?;
        let egui_render_pass = wgpu_backend::egui::Pipeline::new(&instance, 1);
        let platform = winit_backend::Platform::new::<Arch>(&window);

        Ok(Self {
            arch,
            event_loop: Some(event_loop),
            window,
            panels,
            instance,
            egui_render_pass,
            platform,
            ui_queue,
        })
    }

    pub fn process_args(&mut self) {
        if let Some(path) = commands::ARGS.path.as_ref().cloned() {
            self.offload_binary_processing(path);
        }
    }

    fn offload_binary_processing(&mut self, path: std::path::PathBuf) {
        // don't load multiple binaries at a time
        if self.panels.is_loading() {
            return;
        }

        self.panels.start_loading();
        let ui_queue = self.ui_queue.clone();

        std::thread::spawn(move || {
            match processor::Processor::parse(&path) {
                Ok(diss) => ui_queue.push(UIEvent::BinaryLoaded(diss)),
                Err(err) => ui_queue.push(UIEvent::BinaryFailed(err)),
            };
        });
    }

    fn handle_ui_events(&mut self) {
        while let Some(event) = self.ui_queue.inner.pop() {
            match event {
                UIEvent::BinaryFailed(err) => {
                    self.panels.stop_loading();
                    log::warning!("{err:?}");
                }
                UIEvent::BinaryRequested(path) => self.offload_binary_processing(path),
                UIEvent::BinaryLoaded(disassembly) => {
                    self.panels.stop_loading();
                    self.panels.load_binary(disassembly);
                }
            }
        }
    }

    pub fn run(mut self) {
        let now = std::time::Instant::now();

        // necessary as `run` takes a self
        let event_loop = self.event_loop.take().unwrap();

        let _ = event_loop.run(move |mut event, target| {
            // pass the winit events to the platform integration
            self.platform.handle_event(&self.window, &mut event);

            self.handle_ui_events();

            let events = self.platform.unprocessed_events();
            self.panels.handle_events(events);

            let cmds = self.panels.terminal().take_commands().to_vec();
            if !self.process_commands(&cmds) {
                target.exit();
            }

            match event {
                Event::WindowEvent { event, .. } => match event {
                    WindowEvent::RedrawRequested => {
                        // update time elapsed
                        self.platform.update_time(now.elapsed().as_secs_f64());

                        let result = self.instance.draw(
                            &self.window,
                            &mut self.platform,
                            &mut self.egui_render_pass,
                            &mut self.panels,
                        );

                        if let Err(err) = result {
                            log::warning!("{err:?}");
                        }
                    }
                    WindowEvent::Resized(size) => {
                        self.instance.resize(size.width, size.height);
                        self.window.request_redraw();
                    }
                    WindowEvent::DroppedFile(path) => self.offload_binary_processing(path),
                    WindowEvent::CloseRequested => target.exit(),
                    _ => {}
                },
                Event::UserEvent(event) => match event {
                    WinitEvent::CloseRequest => target.exit(),
                    WinitEvent::DragWindow => {
                        let _ = self.window.drag_window();
                    }
                    WinitEvent::Fullscreen => self.arch.fullscreen(&self.window),
                    WinitEvent::Minimize => self.window.set_minimized(true),
                },
                Event::AboutToWait => self.window.request_redraw(),
                _ => {}
            }
        });
    }
}
