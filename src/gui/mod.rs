mod controls;
mod donut;
mod quad;
mod texture;
mod uniforms;
mod utils;
mod window;

use winit::dpi::{PhysicalPosition, PhysicalSize, Size};
use winit::event::{
    ElementState, Event, KeyboardInput, ModifiersState, MouseScrollDelta, VirtualKeyCode,
    WindowEvent,
};
use winit::event_loop::{ControlFlow, EventLoopBuilder};

use once_cell::sync::OnceCell;

use crate::disassembly::Disassembly;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;

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

#[derive(Debug)]
enum CustomEvent {
    ScaleFactorChanged(f64),
}

pub struct RenderContext {
    fps: usize,
    donut: donut::Donut,
    show_donut: Arc<AtomicBool>,
    timer60: utils::Timer,
    timer10: utils::Timer,
    dissasembly: Option<Disassembly>,
    disassembling_thread: Option<DisassemblingThrd>,
    listing_offset: f32,
    scale_factor: f32,
    font_size: f32,
    window: Arc<winit::window::Window>,
}

struct EventContext {
    frame_time: Instant,
    keyboard: controls::KeyMap,
    backend: window::Backend,
}

type DisassemblingThrd =
    tokio::task::JoinHandle<Result<Disassembly, crate::disassembly::DecodeError>>;

pub const MIN_REAL_SIZE: PhysicalSize<u32> = PhysicalSize::new(580, 300);
pub const MIN_WIN_SIZE: Size = Size::Physical(MIN_REAL_SIZE);

pub static WINDOW: OnceCell<Arc<winit::window::Window>> = OnceCell::new();

pub async fn main() -> Result<(), Error> {
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

    let mut rctx = RenderContext {
        fps: 0,
        donut: donut::Donut::new(true),
        show_donut: Arc::new(AtomicBool::new(false)),
        timer60: utils::Timer::new(60),
        timer10: utils::Timer::new(10),
        dissasembly: None,
        disassembling_thread: None,
        listing_offset: 0.0,
        scale_factor: window.scale_factor() as f32,
        font_size: 20.0,
        window: Arc::clone(&window),
    };

    let mut ectx = EventContext {
        frame_time: Instant::now(),
        keyboard: controls::KeyMap::new(),
        backend: window::Backend::new(&window).await?,
    };

    if let Some(ref path) = crate::ARGS.path {
        let show_donut = Arc::clone(&rctx.show_donut);

        rctx.disassembling_thread = Some(tokio::spawn(Disassembly::new(path, show_donut)));
    }

    // async thread handling events
    let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
    tokio::spawn(async move {
        loop {
            let event = rx.recv().await.unwrap();
            handle_event(&mut rctx, &mut ectx, event).await;
        }
    });

    event_loop.run(move |event, _, control| match event {
        Event::WindowEvent {
            event: WindowEvent::CloseRequested,
            ..
        } => *control = ControlFlow::Exit,
        Event::WindowEvent {
            event: WindowEvent::ScaleFactorChanged { scale_factor, .. },
            ..
        } => tx
            .send(winit::event::Event::UserEvent(
                CustomEvent::ScaleFactorChanged(scale_factor),
            ))
            .unwrap(),
        _ => {
            // the only non-static event is a `ScaleFactorChanged`, therefore `CustomEvent` exists
            // For that reason it's also ok to unwrap this
            tx.send(event.to_static().unwrap()).unwrap();
        }
    })
}

async fn handle_event(
    rctx: &mut RenderContext,
    ectx: &mut EventContext,
    event: winit::event::Event<'_, CustomEvent>,
) {
    #[cfg(target_family = "windows")]
    let mut unwindowed_size = window.outer_size();
    #[cfg(target_family = "windows")]
    let mut unwindowed_pos = window.outer_position().unwrap_or_default();

    match event {
        Event::UserEvent(CustomEvent::ScaleFactorChanged(scale_factor)) => {
            rctx.scale_factor = scale_factor as f32;
        }
        Event::WindowEvent { event, .. } => match event {
            WindowEvent::ModifiersChanged(modi) => ectx.keyboard.press_modifiers(modi),
            WindowEvent::KeyboardInput {
                input:
                    KeyboardInput {
                        virtual_keycode: Some(keycode),
                        state,
                        ..
                    },
                ..
            } => match state {
                ElementState::Pressed => ectx.keyboard.press(keycode),
                ElementState::Released => ectx.keyboard.release(keycode),
            },
            WindowEvent::Resized(size) => ectx.backend.resize(size),
            WindowEvent::DroppedFile(path) => {
                let show_donut = Arc::clone(&rctx.show_donut);

                rctx.disassembling_thread = Some(tokio::spawn(Disassembly::new(path, show_donut)));
            }
            WindowEvent::MouseWheel { delta, .. } => {
                let delta = -match delta {
                    // I'm assuming a line is about 100 pixels
                    MouseScrollDelta::LineDelta(_, scroll) => scroll * 100.0,
                    MouseScrollDelta::PixelDelta(PhysicalPosition { y: scroll, .. }) => {
                        scroll as f32
                    }
                };

                rctx.listing_offset = f32::max(0.0, rctx.listing_offset + delta);
            }
            _ => {}
        },
        Event::RedrawRequested(_) => {
            ectx.frame_time = std::time::Instant::now();

            if let Err(e) = ectx.backend.redraw(rctx) {
                eprintln!("Failed to redraw frame, due to {e:?}");
            }
        }
        Event::MainEventsCleared => {
            if rctx.timer10.reached() {
                rctx.fps = (1_000_000_000 / ectx.frame_time.elapsed().as_nanos()) as usize;
                rctx.timer10.reset();
            }

            if rctx.show_donut.load(Ordering::Relaxed) && rctx.timer60.reached() {
                rctx.donut.update_frame();
                rctx.timer60.reset();
            }

            if let Some(ref thread) = rctx.disassembling_thread {
                if thread.is_finished() {
                    let thread = rctx.disassembling_thread.take().unwrap();
                    rctx.dissasembly = Some(thread.await.unwrap().unwrap());
                }
            }

            if ectx
                .keyboard
                .pressed(VirtualKeyCode::O, ModifiersState::CTRL)
            {
                // create dialog popup and get references to the donut and dissasembly
                let dialog = rfd::FileDialog::new().set_parent(&*rctx.window).pick_file();
                let show_donut = Arc::clone(&rctx.show_donut);

                // cancel disassembling if a file is already loaded.
                if let Some(thread) = &rctx.disassembling_thread {
                    thread.abort();
                    rctx.disassembling_thread = None;
                }

                if let Some(path) = dialog {
                    rctx.disassembling_thread =
                        Some(tokio::spawn(Disassembly::new(path, show_donut)));
                }
            }

            if ectx
                .keyboard
                .pressed(VirtualKeyCode::F, ModifiersState::CTRL)
            {
                let Some(monitor) = rctx.window.current_monitor() else {
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

                        SetWindowLongPtrW(rctx.window.hwnd(), GWL_STYLE, attr);
                        SetWindowPos(
                            rctx.window.hwnd(),
                            HWND_TOP,
                            unwindowed_pos.x as u32,
                            unwindowed_pos.y as u32,
                            unwindowed_size.width,
                            unwindowed_size.height,
                            SWP_NOZORDER,
                        );
                    } else {
                        let attr = WS_VISIBLE | WS_OVERLAPPED;

                        unwindowed_size = rctx.window.outer_size();
                        unwindowed_pos = rctx.window.outer_position().unwrap_or_default();

                        SetWindowLongPtrW(rctx.window.hwnd(), GWL_STYLE, attr);
                        SetWindowPos(
                            rctx.window.hwnd(),
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
                rctx.window.set_fullscreen(match rctx.window.fullscreen() {
                    Some(..) => None,
                    None => Some(winit::window::Fullscreen::Borderless(Some(monitor))),
                });
            }

            if ectx
                .keyboard
                .pressed(VirtualKeyCode::Minus, ModifiersState::CTRL)
            {
                rctx.scale_factor = f32::clamp(rctx.scale_factor / 1.025, 0.5, 10.0);
            }

            if ectx
                .keyboard
                .pressed(VirtualKeyCode::Equals, ModifiersState::CTRL)
            {
                rctx.scale_factor = f32::clamp(rctx.scale_factor * 1.025, 0.5, 10.0);
            }

            rctx.window.request_redraw();
            ectx.keyboard.release_pressed();
        }
        _ => (),
    }
}
