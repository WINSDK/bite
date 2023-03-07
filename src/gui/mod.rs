mod controls;
mod donut;
mod texture;
mod uniforms;
mod utils;
mod window;
mod quad;

use winit::dpi::{PhysicalPosition, PhysicalSize, Size};
use winit::event::{
    ElementState, Event, KeyboardInput, ModifiersState, MouseScrollDelta, VirtualKeyCode,
    WindowEvent,
};
use winit::event_loop::{ControlFlow, EventLoop};

use once_cell::sync::OnceCell;

use crate::disassembler::Dissasembly;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

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

pub struct RenderContext {
    fps: usize,
    donut: donut::Donut,
    show_donut: Arc<AtomicBool>,
    timer60: utils::Timer,
    timer10: utils::Timer,
    dissasembly: Arc<Dissasembly>,
    listing_offset: f64,
    scale_factor: f32,
    font_size: f32,
}

pub const MIN_REAL_SIZE: PhysicalSize<u32> = PhysicalSize::new(580, 300);
pub const MIN_WIN_SIZE: Size = Size::Physical(MIN_REAL_SIZE);

pub static WINDOW: OnceCell<Arc<winit::window::Window>> = OnceCell::new();

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

        Arc::new(utils::generate_window("bite", icon, &event_loop)?)
    };

    WINDOW.set(Arc::clone(&window)).unwrap();

    let mut backend = window::Backend::new(&window).await?;
    let mut ctx = RenderContext {
        fps: 0,
        donut: donut::Donut::new(true),
        show_donut: Arc::new(AtomicBool::new(false)),
        timer60: utils::Timer::new(60),
        timer10: utils::Timer::new(10),
        dissasembly: Arc::new(crate::disassembler::Dissasembly::new()),
        listing_offset: 0.0,
        scale_factor: window.scale_factor() as f32,
        font_size: 20.0,
    };

    if let Some(ref path) = crate::ARGS.path {
        Arc::clone(&ctx.dissasembly)
            .load(path, Arc::clone(&ctx.show_donut))
            .await;
    }

    let mut frame_time = std::time::Instant::now();
    let mut keyboard = controls::KeyMap::new();

    #[cfg(target_family = "windows")]
    let mut unwindowed_size = window.outer_size();
    #[cfg(target_family = "windows")]
    let mut unwindowed_pos = window.outer_position().unwrap_or_default();

    event_loop.run(move |event, _, control| {
        if ctx.timer10.reached() {
            ctx.fps = (1_000_000_000 / frame_time.elapsed().as_nanos()) as usize;
            ctx.timer10.reset();
        }

        if ctx.show_donut.load(Ordering::Relaxed) && ctx.timer60.reached() {
            ctx.donut.update_frame();
            ctx.timer60.reset();
        }

        match event {
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => *control = ControlFlow::Exit,
                WindowEvent::ModifiersChanged(modi) => keyboard.press_modifiers(modi),
                WindowEvent::KeyboardInput {
                    input:
                        KeyboardInput {
                            virtual_keycode: Some(keycode),
                            state,
                            ..
                        },
                    ..
                } => match state {
                    ElementState::Pressed => keyboard.press(keycode),
                    ElementState::Released => keyboard.release(keycode),
                },
                WindowEvent::Resized(size) => backend.resize(size),
                WindowEvent::DroppedFile(path) => {
                    let dissasembly = Arc::clone(&ctx.dissasembly);
                    let show_donut = Arc::clone(&ctx.show_donut);

                    tokio::spawn(async move {
                        dissasembly.clear();
                        dissasembly.load(path, show_donut).await;
                    });
                }
                WindowEvent::MouseWheel { delta, .. } => {
                    let delta = -match delta {
                        // I'm assuming a line is about 100 pixels
                        MouseScrollDelta::LineDelta(_, scroll) => scroll as f64 * 100.0,
                        MouseScrollDelta::PixelDelta(PhysicalPosition { y: scroll, .. }) => scroll,
                    };

                    ctx.listing_offset = f64::max(0.0, ctx.listing_offset + delta);
                }
                WindowEvent::ScaleFactorChanged { scale_factor, .. } => {
                    ctx.scale_factor = scale_factor as f32;
                }
                _ => {}
            },
            Event::RedrawRequested(_) => {
                frame_time = std::time::Instant::now();

                if let Err(e) = backend.redraw(&mut ctx) {
                    eprintln!("Failed to redraw frame, due to {e:?}");
                }
            }
            Event::MainEventsCleared => {
                if keyboard.pressed(VirtualKeyCode::O, ModifiersState::CTRL) {
                    // create dialog popup and get references to the donut and dissasembly
                    let dialog = rfd::AsyncFileDialog::new().set_parent(&*window).pick_file();
                    let show_donut = Arc::clone(&ctx.show_donut);
                    let dissasembly = Arc::clone(&ctx.dissasembly);

                    tokio::spawn(async move {
                        if let Some(file) = dialog.await {
                            dissasembly.clear();
                            dissasembly.load(file.path().to_owned(), show_donut).await;
                        }
                    });
                }

                if keyboard.pressed(VirtualKeyCode::F, ModifiersState::CTRL) {
                    let Some(monitor) = window.current_monitor() else {
                        return;
                    };

                    #[cfg(target_family = "windows")]
                    unsafe {
                        use utils::windows::*;
                        use winit::platform::windows::{MonitorHandleExtWindows, WindowExtWindows};

                        let mut info = MonitorInfo {
                            size: std::mem::size_of::<MonitorInfo>() as u32,
                            monitor_area: Rect::default(),
                            work_area: Rect::default(),
                            flags: 0,
                        };

                        if GetMonitorInfoW(monitor.hmonitor(), &mut info) == 0 {
                            return;
                        }

                        let PhysicalSize { width, height } = window.outer_size();
                        let work_area_width = info.work_area.right - info.work_area.left;
                        let work_area_height = info.work_area.bottom - info.work_area.top;

                        // check if the window is fullscreen borderless
                        if width == work_area_width && height == work_area_height {
                            let attr = WS_VISIBLE | WS_THICKFRAME | WS_POPUP;

                            SetWindowLongPtrW(window.hwnd(), GWL_STYLE, attr);
                            SetWindowPos(
                                window.hwnd(),
                                HWND_TOP,
                                unwindowed_pos.x as u32,
                                unwindowed_pos.y as u32,
                                unwindowed_size.width,
                                unwindowed_size.height,
                                SWP_NOZORDER,
                            );
                        } else {
                            let attr = WS_VISIBLE | WS_OVERLAPPED;

                            unwindowed_size = window.outer_size();
                            unwindowed_pos = window.outer_position().unwrap_or_default();

                            SetWindowLongPtrW(window.hwnd(), GWL_STYLE, attr);
                            SetWindowPos(
                                window.hwnd(),
                                HWND_TOP,
                                info.work_area.left,
                                info.work_area.top,
                                work_area_width,
                                work_area_height,
                                SWP_NOZORDER,
                            );
                        }
                    }

                    #[cfg(target_family = "unix")]
                    window.set_fullscreen(match window.fullscreen() {
                        Some(..) => None,
                        None => Some(winit::window::Fullscreen::Borderless(Some(monitor))),
                    });
                }

                if keyboard.pressed(VirtualKeyCode::Minus, ModifiersState::CTRL) {
                    ctx.scale_factor = f32::clamp(ctx.scale_factor / 1.025, 0.5, 10.0);
                }

                if keyboard.pressed(VirtualKeyCode::Equals, ModifiersState::CTRL) {
                    ctx.scale_factor = f32::clamp(ctx.scale_factor * 1.025, 0.5, 10.0);
                }

                window.request_redraw();
                keyboard.release_pressed();
            }
            _ => (),
        }
    })
}
