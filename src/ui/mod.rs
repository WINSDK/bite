mod controls;
mod texture;
mod uniforms;
mod utils;
mod window;

use winit::dpi::{PhysicalSize, Size};
use winit::event::{ElementState, Event, VirtualKeyCode, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::Fullscreen;

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

pub const MIN_REAL_SIZE: PhysicalSize<u32> = PhysicalSize::new(580, 300);
pub const MIN_WIN_SIZE: Size = Size::Physical(MIN_REAL_SIZE);

pub async fn main() -> Result<(), Error> {
    let event_loop = EventLoop::new();
    let window = {
        #[cfg(target_os = "linux")]
        let decode = utils::decode_png_bytes(include_bytes!("../../assets/iconx64.png"));
        #[cfg(any(target_os = "windows", target_os = "macos"))]
        let decode = utils::decode_png_bytes(include_bytes!("../../assets/iconx256.png"));

        let mut icon = None;
        if let Ok(png) = decode {
            icon = winit::window::Icon::from_rgba(png.data, png.width, png.height).ok();
        }

        utils::generate_window("rustdump", icon, &event_loop)?
    };

    let mut backend = window::Backend::new(&window).await?;
    let mut keyboard = controls::Keybind::new(VirtualKeyCode::Yen);

    // frame time
    let mut frame_timer = std::time::Instant::now();

    // 100ms time
    let mut short_timer = std::time::Instant::now();
    let mut fps = 0;

    event_loop.run(move |event, _, control| {
        let controls = controls::Inputs::default();

        match event {
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => {
                    *control = ControlFlow::Exit;
                }
                WindowEvent::ModifiersChanged(modi) => {
                    keyboard.modifier ^= modi;
                }
                WindowEvent::KeyboardInput { input, .. } => {
                    keyboard.key = input.virtual_keycode.unwrap();

                    if input.state == ElementState::Pressed {
                        if controls.matching_action(controls::Actions::Maximize, keyboard) {
                            if window.fullscreen().is_some() {
                                window.set_fullscreen(None);
                            } else {
                                let handle = window.current_monitor();
                                window.set_fullscreen(Some(Fullscreen::Borderless(handle)));
                            }
                        }

                        if controls.matching_action(controls::Actions::CloseRequest, keyboard) {
                            *control = ControlFlow::Exit;
                        }
                    }
                }
                WindowEvent::Resized(size) => backend.resize(size),
                _ => (),
            },
            Event::RedrawRequested(_) => {
                if short_timer.elapsed().as_millis() > 100 {
                    fps = (1_000_000_000 / frame_timer.elapsed().as_nanos()) as usize;
                    short_timer = std::time::Instant::now();
                }

                frame_timer = std::time::Instant::now();

                if let Err(e) = backend.redraw(fps) {
                    eprintln!("Failed to redraw frame, due to {e:?}");
                }
            }
            Event::MainEventsCleared => window.request_redraw(),
            _ => (),
        }
    })
}
