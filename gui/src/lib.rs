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
    DuplicateInstance,
    Exit,
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

/// A custom event type for the winit app.
pub enum CustomEvent {
    CloseRequest,
    DragWindow,
    Fullscreen,
    Minimize,
    DebuggerExecute(Vec<String>),
    DebuggerFailed(debugger::Error),
    DebuggerFinished,
    BinaryRequested(std::path::PathBuf),
    BinaryFailed(disassembler::Error),
    BinaryLoaded(Arc<disassembler::Disassembly>),
}

#[derive(Clone)]
pub struct Proxy {
    sender: winit::event_loop::EventLoopProxy<CustomEvent>,
}

impl Proxy {
    pub fn send(&self, custom_event: CustomEvent) {
        if let Err(..) = self.sender.send_event(custom_event) {
            panic!("missing an event loop to handle event");
        }
    }
}

pub struct UI<Arch: Target> {
    arch: Arch,
    window: winit::window::Window,
    event_loop: Option<EventLoop<CustomEvent>>,
    panels: panels::Panels,
    instance: wgpu_backend::Instance,
    egui_render_pass: wgpu_backend::egui::Pipeline,
    platform: winit_backend::Platform,
    proxy: Proxy,
}

impl<Arch: Target> UI<Arch> {
    pub fn new() -> Result<Self, Error> {
        let event_loop = EventLoopBuilder::<CustomEvent>::with_user_event()
            .build()
            .map_err(|_| Error::DuplicateInstance)?;

        let window = Arch::create_window("bite", 1200, 800, &event_loop)?;

        #[cfg(target_family = "windows")]
        let arch = Arch::new(windows::ArchDescriptor {
            initial_size: window.outer_size(),
            initial_pos: window.outer_position().unwrap_or_default(),
        });

        #[cfg(target_family = "unix")]
        let arch = Arch::new();

        let proxy = Proxy {
            sender: event_loop.create_proxy(),
        };

        let panels = panels::Panels::new(proxy.clone());
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
            proxy,
        })
    }

    pub fn process_args(&mut self) {
        if let Some(path) = args::ARGS.path.as_ref().cloned() {
            self.proxy.send(CustomEvent::BinaryRequested(path));
        }
    }

    fn offload_binary_processing(&mut self, path: std::path::PathBuf) {
        // don't load multiple binaries at a time
        if self.panels.loading {
            return;
        }

        self.panels.loading = true;
        let proxy = self.proxy.clone();

        std::thread::spawn(move || {
            match disassembler::Disassembly::parse(&path) {
                Ok(diss) => proxy.send(CustomEvent::BinaryLoaded(Arc::new(diss))),
                Err(err) => proxy.send(CustomEvent::BinaryFailed(err)),
            };
        });
    }

    fn offload_debugging(&mut self, args: Vec<String>) {
        // don't debug multiple binaries at a time
        if self.panels.debugging {
            print_extern!(self.panels.terminal(), "Debugger is already running.");
            return;
        }

        let proxy = self.proxy.clone();
        let path = match self.panels.listing() {
            Some(listing) => listing.disassembly.path.clone(),
            None => {
                print_extern!(self.panels.terminal(), "Missing binary to debug.");
                return;
            }
        };

        self.panels.debugging = true;
        print_extern!(self.panels.terminal(), "Running debugger.");

        std::thread::spawn(move || {
            use debugger::Process;

            let mut session = match debugger::Debugger::spawn(path, args) {
                Ok(session) => session,
                Err(err) => {
                    proxy.send(CustomEvent::DebuggerFailed(err));
                    return;
                }
            };

            session.trace_syscalls(true);

            match session.run_to_end() {
                Ok(()) => proxy.send(CustomEvent::DebuggerFinished),
                Err(err) => proxy.send(CustomEvent::DebuggerFailed(err)),
            }
        });
    }

    pub fn run(mut self) {
        let now = std::time::Instant::now();

        // necessary as `run` takes a self
        let event_loop = self.event_loop.take().unwrap();

        let _ = event_loop.run(move |mut event, target| {
            // Pass the winit events to the platform integration
            self.platform.handle_event(&self.window, &mut event);

            match event {
                Event::WindowEvent { event, .. } => match event {
                    WindowEvent::RedrawRequested => {
                        // update time elapsed
                        self.platform.update_time(now.elapsed().as_secs_f64());

                        match self.draw() {
                            Err(Error::Exit) => target.exit(), // might not be needed
                            Err(err) => log::warning!("{err:?}"),
                            Ok(()) => {}
                        }
                    }
                    WindowEvent::Resized(size) => self.instance.resize(size.width, size.height),
                    WindowEvent::DroppedFile(path) => self.offload_binary_processing(path),
                    WindowEvent::CloseRequested => target.exit(),
                    _ => {}
                },
                Event::UserEvent(event) => match event {
                    CustomEvent::CloseRequest => target.exit(),
                    CustomEvent::DragWindow => {
                        let _ = self.window.drag_window();
                    }
                    CustomEvent::Fullscreen => self.arch.fullscreen(&self.window),
                    CustomEvent::Minimize => self.window.set_minimized(true),
                    CustomEvent::DebuggerExecute(args) => self.offload_debugging(args),
                    CustomEvent::DebuggerFailed(err) => {
                        self.panels.debugging = false;
                        print_extern!(self.panels.terminal(), "{err:?}.");
                    }
                    CustomEvent::DebuggerFinished => {
                        self.panels.debugging = false;
                    }
                    CustomEvent::BinaryRequested(path) => self.offload_binary_processing(path),
                    CustomEvent::BinaryFailed(err) => {
                        self.panels.loading = false;
                        log::warning!("{err:?}");
                    }
                    CustomEvent::BinaryLoaded(disassembly) => {
                        self.panels.loading = false;
                        self.panels.load_binary(disassembly);
                    }
                },
                Event::AboutToWait => self.window.request_redraw(),
                _ => {}
            }
        });
    }

    fn draw(&mut self) -> Result<(), Error> {
        let events = self.platform.unprocessed_events();
        let events_processed = self.panels.terminal().record_input(events);

        if events_processed > 0 {
            // if a goto command is being run, start performing the autocomplete
            let split = self.panels.terminal().current_line().split_once(' ');
            if let Some(("g" | "goto", arg)) = split {
                let arg = arg.to_string();

                if let Some(listing) = self.panels.listing() {
                    let _ = dbg!(disassembler::expr::parse(
                        &listing.disassembly.symbols,
                        &arg
                    ));
                }
            }

            // store new commands recorded
            let _ = self.panels.terminal().save_command_history();
        }

        let cmds = self.panels.terminal().take_commands().to_vec();

        if !self.panels.process_commands(&cmds) {
            return Err(Error::Exit);
        }

        self.instance.draw(
            &self.window,
            &mut self.platform,
            &mut self.egui_render_pass,
            &mut self.panels,
        )
    }
}
