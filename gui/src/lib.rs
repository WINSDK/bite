mod commands;
mod fmt;
mod icon;
mod panels;
mod style;
pub mod unix;
mod wgpu_backend;
pub mod windows;
mod winit_backend;

use std::fmt::Write;
use std::sync::Arc;

use copypasta::ClipboardProvider;
use debugger::{DebugeeEvent, DebuggerEvent, Debugger, Debuggable, DebuggerDescriptor};
use winit::event::{Event, WindowEvent};
use winit::event_loop::{EventLoop, EventLoopBuilder};

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
    DebuggerExecute(Vec<String>),
    DebuggerFailed(debugger::Error),
    BinaryRequested(std::path::PathBuf),
    BinaryFailed(disassembler::Error),
    BinaryLoaded(Arc<disassembler::Disassembly>),
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
    window: Arc<Window>,
    event_loop: Option<EventLoop<WinitEvent>>,
    panels: panels::Panels,
    instance: wgpu_backend::Instance,
    egui_render_pass: wgpu_backend::egui::Pipeline,
    platform: winit_backend::Platform,
    ui_queue: Arc<UIQueue>,
    dbg_ctx: Arc<debugger::Context>,
}

impl<Arch: Target> UI<Arch> {
    pub fn new() -> Result<Self, Error> {
        let event_loop = EventLoopBuilder::<WinitEvent>::with_user_event()
            .build()
            .map_err(Error::EventLoopCreation)?;

        let window = Arc::new(Arch::create_window("bite", 1200, 800, &event_loop)?);

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

        let dbg_ctx = Arc::new(debugger::Context::new());

        let panels = panels::Panels::new(ui_queue.clone(), winit_queue.clone(), dbg_ctx.clone());
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
            dbg_ctx,
        })
    }

    pub fn process_args(&mut self) {
        if let Some(path) = args::ARGS.path.as_ref().cloned() {
            self.offload_binary_processing(path);
        }
    }

    fn offload_binary_processing(&mut self, path: std::path::PathBuf) {
        // don't load multiple binaries at a time
        if self.panels.loading {
            return;
        }

        self.panels.loading = true;
        let ui_queue = self.ui_queue.clone();

        std::thread::spawn(move || {
            match disassembler::Disassembly::parse(&path) {
                Ok(diss) => ui_queue.push(UIEvent::BinaryLoaded(Arc::new(diss))),
                Err(err) => ui_queue.push(UIEvent::BinaryFailed(err)),
            };
        });
    }

    fn offload_debugging(&mut self, args: Vec<String>) {
        // don't debug multiple binaries at a time
        if self.dbg_ctx.attached() {
            tprint!(self.panels.terminal(), "Debugger is already running.");
            return;
        }

        let dbg_ctx = self.dbg_ctx.clone();
        let ui_queue = self.ui_queue.clone();
        let path = match self.panels.listing() {
            Some(listing) => listing.disassembly.processor.path.clone(),
            None => {
                tprint!(self.panels.terminal(), "No binary to debug.");
                return;
            }
        };
        let module = self.panels.listing().unwrap().disassembly.processor.clone();

        tprint!(self.panels.terminal(), "Running debugger.");

        std::thread::spawn(move || {
            let desc = DebuggerDescriptor {
                module,
                tracing: false,
                follow_children: false,
            };

            let session = match Debugger::spawn(path, args, desc) {
                Ok(session) => session,
                Err(err) => {
                    ui_queue.push(UIEvent::DebuggerFailed(err));
                    return;
                }
            };

            if let Err(err) = session.run(dbg_ctx) {
                ui_queue.push(UIEvent::DebuggerFailed(err));
            }
        });
    }

    fn handle_ui_events(&mut self) {
        while let Some(event) = self.ui_queue.inner.pop() {
            match event {
                UIEvent::DebuggerExecute(args) => self.offload_debugging(args),
                UIEvent::DebuggerFailed(err) => {
                    tprint!(self.panels.terminal(), "{err:?}.");
                }
                UIEvent::BinaryRequested(path) => self.offload_binary_processing(path),
                UIEvent::BinaryFailed(err) => {
                    self.panels.loading = false;
                    log::warning!("{err:?}");
                }
                UIEvent::BinaryLoaded(disassembly) => {
                    self.panels.loading = false;
                    self.panels.load_binary(disassembly);
                }
            }
        }
    }

    fn handle_dbg_events(&mut self) {
        while let Some(event) = self.dbg_ctx.queue.pop() {
            match event {
                DebuggerEvent::Exited(code) => {
                    tprint!(self.panels.terminal(), "Process exited with code {code}.");
                }
                DebuggerEvent::BreakpointSet(addr) => {
                    tprint!(self.panels.terminal(), "Breakpoint set at {addr:#X}.");
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
            self.handle_dbg_events();

            let events = self.platform.unprocessed_events();
            let terminal_events = self.panels.terminal().record_input(events);

            if terminal_events > 0 {
                // if a goto command is being run, start performing the autocomplete
                let split = self.panels.terminal().current_line().split_once(' ');
                if let Some(("g" | "goto", arg)) = split {
                    let arg = arg.to_string();

                    if let Some(listing) = self.panels.listing() {
                        let _ = dbg!(disassembler::expr::parse(
                            &listing.disassembly.processor.symbols(),
                            &arg
                        ));
                    }
                }

                // store new commands recorded
                let _ = self.panels.terminal().save_command_history();
            }

            let cmds = self.panels.terminal().take_commands().to_vec();

            if !self.panels.process_commands(&cmds) {
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
                    WindowEvent::Resized(size) => self.instance.resize(size.width, size.height),
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

impl<Arch: Target> Drop for UI<Arch> {
    fn drop(&mut self) {
        // if a debugger is running
        if self.dbg_ctx.attached() {
            self.dbg_ctx.queue.push(DebugeeEvent::Exit);

            // wait for debugger to signal it exited
            loop {
                if let Some(DebuggerEvent::Exited(..)) = self.dbg_ctx.queue.pop() {
                    return;
                }
            }
        }
    }
}
