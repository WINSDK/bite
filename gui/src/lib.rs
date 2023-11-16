mod fmt;
mod icon;
mod panels;
mod style;
pub mod unix;
mod wgpu_backend;
pub mod windows;
mod winit_backend;

use std::sync::Arc;

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
}

/// A custom event type for the winit app.
pub enum CustomEvent {
    CloseRequest,
    DragWindow,
    Fullscreen,
    Minimize,
    BinaryRequested(std::path::PathBuf),
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
    pub arch: Arch,
    pub window: winit::window::Window,
    pub event_loop: Option<EventLoop<CustomEvent>>,
    pub panels: panels::Panels,
    pub instance: wgpu_backend::Instance,
    pub egui_render_pass: wgpu_backend::egui::Pipeline,
    pub platform: winit_backend::Platform,
    pub proxy: Proxy,
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
        let platform = winit_backend::Platform::new(&window, proxy.clone());

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
                    WindowEvent::DroppedFile(_file) => todo!("drop file"),
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
                    CustomEvent::BinaryRequested(path) => {
                        self.panels.toggle_loading();
                        let proxy = self.proxy.clone();

                        std::thread::spawn(move || {
                            match disassembler::Disassembly::parse(path) {
                                Ok(diss) => proxy.send(CustomEvent::BinaryLoaded(Arc::new(diss))),
                                Err(err) => log::warning!("{err:?}"),
                            };
                        });
                    }
                    CustomEvent::BinaryLoaded(disassembly) => {
                        self.panels.toggle_loading();
                        self.panels.load_binary(disassembly.clone());
                    }
                },
                Event::AboutToWait => self.window.request_redraw(),
                _ => {}
            }
        });
    }

    fn draw(&mut self) -> Result<(), Error> {
        let terminal = &mut self.panels.terminal();

        // handle inputs before `begin_frame` consumes them
        let events_processed = terminal.record_input(self.platform.unprocessed_events());

        if events_processed > 0 {
            // if a goto command is being run, start performing the autocomplete
            if let Some(("g" | "goto", _arg)) = terminal.current_line().split_once(' ') {
                // if let Some(ref dissasembly) = ctx.dissasembly {
                //     let _ = dbg!(crate::expr::parse(&dissasembly.symbols, arg));
                // }
            }

            // store new commands recorded
            let _ = terminal.save_command_history();
        }

        let cmds = terminal.take_commands().to_vec();

        // if !crate::commands::process_commands(ctx, &cmds) {
        //     return Err(Error::Exit);
        // }

        self.instance.draw(
            &self.window,
            &mut self.platform,
            &mut self.egui_render_pass,
            &mut self.panels,
        )
    }
}
