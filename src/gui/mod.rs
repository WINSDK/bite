mod controls;
mod donut;
mod texture;
mod uniforms;
mod utils;
mod window;

use winit::dpi::{PhysicalSize, Size};
use winit::event::{ElementState, Event, KeyboardInput, ModifiersState, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::Fullscreen;

use crate::disassembler::{InstructionStream, Line};
use object::{Object, ObjectSection, SectionKind};
use std::sync::Arc;
use std::sync::Mutex;

#[derive(Debug)]
pub enum Error {
    /// Generic IO operation failed.
    IO(std::io::Error),

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

    /// A shader given to the compiler wasn't of type `COMPUTE`, `VERTEX` or `FRAGMENT`.
    UnknownShaderStage,

    /// Shader failed to compile for any number of reasons.
    CompilationFailed,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:#?}"))
    }
}

impl std::error::Error for Error {}

pub struct RenderContext<'src> {
    fps: usize,
    donut: donut::Donut,
    show_donut: bool,
    timer60: utils::Timer,
    timer10: utils::Timer,
    dissasembly: Arc<Mutex<Vec<Line<'src>>>>,
}

pub const MIN_REAL_SIZE: PhysicalSize<u32> = PhysicalSize::new(580, 300);
pub const MIN_WIN_SIZE: Size = Size::Physical(MIN_REAL_SIZE);

pub async fn main() -> Result<(), Error> {
    let event_loop = EventLoop::new();

    // generate window
    let window = {
        #[cfg(target_os = "linux")]
        let decode = utils::decode_png_bytes(include_bytes!("../../assets/iconx64.png"));
        #[cfg(any(target_os = "windows", target_os = "macos"))]
        let decode = utils::decode_png_bytes(include_bytes!("../../assets/iconx256.png"));

        let mut icon = None;
        if let Ok(png) = decode {
            icon = winit::window::Icon::from_rgba(png.data, png.width, png.height).ok();
        }

        utils::generate_window("bite", icon, &event_loop)?
    };

    let mut backend = window::Backend::new(&window).await?;
    let mut ctx = RenderContext {
        fps: 0,
        donut: donut::Donut::new(true),
        show_donut: true,
        timer60: utils::Timer::new(60),
        timer10: utils::Timer::new(10),
        dissasembly: Arc::new(Mutex::new(Vec::new())),
    };

    // parse and generate dissasembly
    let dissasembly = Arc::clone(&ctx.dissasembly);
    tokio::task::spawn(async move {
        let mut dissasembly = dissasembly.lock().unwrap();
        let now = std::time::Instant::now();

        let Some(ref path) = crate::ARGS.path else {
            return;
        };

        let binary = std::fs::read(path)
            .expect("Unexpected read of binary failed.")
            .leak();

        let obj = object::File::parse(&*binary).expect("Failed to parse binary.");
        let symbols = Box::leak(Box::new(
            crate::symbols::table::parse(&obj).expect("Failed to parse symbols table."),
        ));

        let section = obj
            .sections()
            .filter(|s| s.kind() == SectionKind::Text)
            .find(|t| t.name() == Ok(".text"))
            .expect("Failed to find `.text` section.");

        let raw = section
            .uncompressed_data()
            .expect("Failed to decompress .text section.")
            .into_owned()
            .leak();

        let base_offset = section.address() as usize;
        let stream = InstructionStream::new(raw, obj.architecture(), base_offset, symbols);

        // TODO: optimize for lazy chunk loading
        for inst in stream {
            dissasembly.push(inst);
        }

        println!("took {:#?} to parse {:?}", now.elapsed(), path);
    });

    let mut frame_time = std::time::Instant::now();
    let mut keyboard_modi = ModifiersState::empty();
    let keybinds = controls::Keybinds::default();

    event_loop.run(move |event, _, control| {
        if ctx.timer10.reached() {
            ctx.fps = (1_000_000_000 / frame_time.elapsed().as_nanos()) as usize;
            ctx.timer10.reset();
        }

        if ctx.timer60.reached() {
            ctx.donut.update_frame();
            ctx.timer60.reset();
        }

        match event {
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => {
                    *control = ControlFlow::Exit;
                }
                WindowEvent::ModifiersChanged(modi) => {
                    keyboard_modi ^= modi;
                }
                WindowEvent::KeyboardInput {
                    input:
                        KeyboardInput {
                            virtual_keycode: Some(keycode),
                            state,
                            ..
                        },
                    ..
                } => {
                    let keypress = (keycode, keyboard_modi);

                    if state == ElementState::Pressed {
                        if keybinds.matching_action(controls::Actions::Maximize, keypress) {
                            if window.fullscreen().is_some() {
                                window.set_fullscreen(None);
                            } else {
                                let handle = window.current_monitor();
                                window.set_fullscreen(Some(Fullscreen::Borderless(handle)));
                            }
                        }

                        if keybinds.matching_action(controls::Actions::CloseRequest, keypress) {
                            *control = ControlFlow::Exit;
                        }
                    }
                }
                WindowEvent::Resized(size) => backend.resize(size),
                _ => (),
            },
            Event::RedrawRequested(_) => {
                frame_time = std::time::Instant::now();

                if let Err(e) = backend.redraw(&mut ctx) {
                    eprintln!("Failed to redraw frame, due to {e:?}");
                }
            }
            Event::MainEventsCleared => window.request_redraw(),
            _ => (),
        }
    })
}
