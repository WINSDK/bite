mod controls;
mod donut;
mod quad;
mod texture;
mod utils;
mod window;

use winit::dpi::{PhysicalPosition, PhysicalSize, Size};
use winit::event::{
    ElementState, Event, KeyboardInput, ModifiersState, MouseScrollDelta, VirtualKeyCode,
    WindowEvent,
};
use winit::event_loop::{ControlFlow, EventLoop};

use once_cell::sync::OnceCell;

use crate::disassembly::Disassembly;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::JoinHandle;
use std::time::Instant;

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

type DisassThread = JoinHandle<Result<Disassembly, crate::disassembly::DecodeError>>;

pub struct RenderContext {
    fps: usize,
    donut: donut::Donut,
    show_donut: Arc<AtomicBool>,
    timer60: utils::Timer,
    timer10: utils::Timer,
    dissasembly: Option<Disassembly>,
    disassembling_thread: Option<DisassThread>,
    listing_offset: f32,
    scale_factor: f32,
    font_size: f32,
}

struct EventContext {
    frame_time: Instant,
    keyboard: controls::KeyMap,
    backend: window::Backend,
    window: Arc<winit::window::Window>,
    #[cfg(target_family = "windows")]
    unwindowed_size: PhysicalSize<u32>,
    #[cfg(target_family = "windows")]
    unwindowed_pos: PhysicalPosition<i32>,
}

pub const MIN_REAL_SIZE: PhysicalSize<u32> = PhysicalSize::new(580, 300);
pub const MIN_WIN_SIZE: Size = Size::Physical(MIN_REAL_SIZE);

pub static WINDOW: OnceCell<Arc<winit::window::Window>> = OnceCell::new();

pub async fn init() -> Result<(), Error> {
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
    };

    let mut ectx = EventContext {
        frame_time: Instant::now(),
        keyboard: controls::KeyMap::new(),
        backend: window::Backend::new(&window).await?,
        window: Arc::clone(&window),
        #[cfg(target_family = "windows")]
        unwindowed_size: window.outer_size(),
        #[cfg(target_family = "windows")]
        unwindowed_pos: window.outer_position().unwrap_or_default(),
    };

    if let Some(ref path) = crate::ARGS.path {
        let show_donut = Arc::clone(&rctx.show_donut);
        rctx.dissasembly = None;
        rctx.disassembling_thread = Some(std::thread::spawn(move || {
            Disassembly::new(path, show_donut)
        }));
    }

    event_loop.run(move |event, _, control| {
        // exit window on closing it
        if let Event::WindowEvent { ref event, .. } = event {
            if event == &WindowEvent::CloseRequested {
                *control = ControlFlow::Exit;
            }
        }

        // handle events that don't rqeuire references
        handle_event(&mut rctx, &mut ectx, event);
    })
}

fn handle_event(
    rctx: &mut RenderContext,
    ectx: &mut EventContext,
    event: winit::event::Event<'_, ()>,
) {
    match event {
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
                rctx.dissasembly = None;
                rctx.disassembling_thread =
                    Some(std::thread::spawn(|| Disassembly::new(path, show_donut)));
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
            WindowEvent::ScaleFactorChanged { scale_factor, .. } => {
                rctx.scale_factor = scale_factor as f32;
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
            handle_post_render(rctx, ectx);
        }
        _ => {}
    }
}

fn handle_post_render(rctx: &mut RenderContext, ectx: &mut EventContext) {
    if rctx.timer10.reached() {
        rctx.fps = (1_000_000_000 / ectx.frame_time.elapsed().as_nanos()) as usize;
        rctx.timer10.reset();
    }

    if rctx.show_donut.load(Ordering::Relaxed) && rctx.timer60.reached() {
        rctx.donut.update_frame();
        rctx.timer60.reset();
    }

    // if there is a binary being loaded
    if let Some(true) = rctx.disassembling_thread.as_ref().map(JoinHandle::is_finished) {
        let thread = rctx.disassembling_thread.take().unwrap();

        // check if it's finished loading
        if thread.is_finished() {
            // store the loaded binary
            match thread.join() {
                Err(err) => {
                    rctx.show_donut.store(false, Ordering::Relaxed);
                    crate::warning!("{err:?}");
                }
                Ok(Err(err)) => {
                    rctx.show_donut.store(false, Ordering::Relaxed);
                    crate::warning!("{err:?}");
                }
                Ok(Ok(val)) => rctx.dissasembly = Some(val),
            }

            // mark the disassembling thread as not loading anything
            rctx.disassembling_thread = None;
        }
    }

    if ectx.keyboard.pressed(VirtualKeyCode::O, ModifiersState::CTRL) {
        // create dialog popup and get references to the donut and dissasembly
        let dialog = rfd::FileDialog::new().set_parent(&*ectx.window).pick_file();

        // load binary
        if let Some(path) = dialog {
            // ghost disassembling thread if a binary is already loaded.
            if rctx.disassembling_thread.is_some() {
                rctx.disassembling_thread = None;
            }

            let show_donut = Arc::clone(&rctx.show_donut);
            rctx.dissasembly = None;
            rctx.disassembling_thread =
                Some(std::thread::spawn(|| Disassembly::new(path, show_donut)));
        }
    }

    if ectx.keyboard.pressed(VirtualKeyCode::F, ModifiersState::CTRL) {
        let monitor = match ectx.window.current_monitor() {
            Some(monitor) => monitor,
            None => return,
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

            let PhysicalSize { width, height } = ectx.window.outer_size();
            let work_area_width = info.work_area.right - info.work_area.left;
            let work_area_height = info.work_area.bottom - info.work_area.top;

            // check if the window is fullscreen borderless
            if width == work_area_width && height == work_area_height {
                let attr = WS_VISIBLE | WS_THICKFRAME | WS_POPUP;

                SetWindowLongPtrW(ectx.window.hwnd(), GWL_STYLE, attr);
                SetWindowPos(
                    ectx.window.hwnd(),
                    HWND_TOP,
                    ectx.unwindowed_pos.x as u32,
                    ectx.unwindowed_pos.y as u32,
                    ectx.unwindowed_size.width,
                    ectx.unwindowed_size.height,
                    SWP_NOZORDER,
                );
            } else {
                let attr = WS_VISIBLE | WS_OVERLAPPED;

                ectx.unwindowed_size = ectx.window.outer_size();
                ectx.unwindowed_pos = ectx.window.outer_position().unwrap_or_default();

                SetWindowLongPtrW(ectx.window.hwnd(), GWL_STYLE, attr);
                SetWindowPos(
                    ectx.window.hwnd(),
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
        ectx.window.set_fullscreen(match ectx.window.fullscreen() {
            Some(..) => None,
            None => Some(winit::window::Fullscreen::Borderless(Some(monitor))),
        });
    }

    if ectx.keyboard.pressed(VirtualKeyCode::Minus, ModifiersState::CTRL) {
        rctx.scale_factor = f32::clamp(rctx.scale_factor / 1.025, 0.5, 10.0);
    }

    if ectx.keyboard.pressed(VirtualKeyCode::Equals, ModifiersState::CTRL) {
        rctx.scale_factor = f32::clamp(rctx.scale_factor * 1.025, 0.5, 10.0);
    }

    ectx.window.request_redraw();
    ectx.keyboard.release_pressed();
}
