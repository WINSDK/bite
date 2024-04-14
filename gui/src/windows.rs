#![cfg(target_os = "windows")]

use crate::{Error, Window, WinitEvent};
use copypasta::{ClipboardContext, ClipboardProvider};
use winit::dpi::{PhysicalPosition, PhysicalSize, LogicalSize};
use winit::event_loop::{EventLoop, EventLoopBuilder};
use winit::platform::windows::HMONITOR;
use winit::platform::windows::HWND;
use winit::platform::windows::{MonitorHandleExtWindows, WindowBuilderExtWindows};
use winit::raw_window_handle::HasWindowHandle;

pub struct ArchDescriptor {
    pub initial_size: PhysicalSize<u32>,
    pub initial_pos: PhysicalPosition<i32>,
}

pub struct Arch {
    unwindowed_size: PhysicalSize<u32>,
    unwindowed_pos: PhysicalPosition<i32>,
}

// this sucks but winit 0.29 doesn't provide an interface for getting an hwnd anymore :(
fn query_hwnd(window: &Window) -> HWND {
    match window.window_handle().unwrap().as_raw() {
        winit::raw_window_handle::RawWindowHandle::Win32(handle) => handle.hwnd.get(),
        _ => unsafe { std::hint::unreachable_unchecked() },
    }
}

const STYLE: isize = WS_BORDER | WS_CLIPSIBLINGS | WS_SYSMENU | WS_VISIBLE | WS_POPUP;
const STYLE_MAXIMIZED: isize = WS_VISIBLE | WS_OVERLAPPED;
const STYLE_EX: isize = WS_EX_ACCEPTFILES | WS_EX_WINDOWEDGE;

impl Arch {
    pub fn new(arch_desc: ArchDescriptor) -> Self {
        Self {
            unwindowed_size: arch_desc.initial_size,
            unwindowed_pos: arch_desc.initial_pos,
        }
    }

    pub fn create_event_loop() -> Result<EventLoop<WinitEvent>, Error> {
        EventLoopBuilder::<WinitEvent>::with_user_event()
            .build()
            .map_err(Error::EventLoopCreation)
    }

    pub fn create_window(
        title: &str,
        width: u32,
        height: u32,
        event_loop: &EventLoop<WinitEvent>,
    ) -> Result<Window, Error> {
        let icon = include_bytes!("../../assets/iconx256.png");
        let icon = crate::icon::PngIcon::decode_bytes(icon)?;
        let icon = winit::window::Icon::from_rgba(icon.data, icon.width, icon.height).ok();

        let window = winit::window::WindowBuilder::new()
            .with_title(title)
            .with_taskbar_icon(icon.clone())
            .with_window_icon(icon)
            .with_inner_size(LogicalSize { width, height })
            .with_min_inner_size(LogicalSize { width: width / 3, height: height / 3 })
            .build(event_loop)
            .map_err(|_| Error::WindowCreation)?;

        let hwnd = query_hwnd(&window);

        unsafe {
            // set basic window attributes
            if SetWindowLongPtrW(hwnd, GWL_STYLE, STYLE) == 0 {
                return Err(Error::WindowCreation);
            }

            // set extended window attributes
            if SetWindowLongPtrW(hwnd, GWL_EXSTYLE, STYLE_EX) == 0 {
                return Err(Error::WindowCreation);
            }

            // apply updated attributes, required by windows
            let flags = SWP_NOZORDER | SWP_NOMOVE | SWP_NOSIZE | SWP_FRAMECHANGED;
            SetWindowPos(hwnd, 0, 0, 0, 0, 0, flags);
        }

        Ok(window)
    }

    pub fn fullscreen(&mut self, window: &Window) {
        unsafe {
            let mut info = MonitorInfo {
                size: std::mem::size_of::<MonitorInfo>() as u32,
                monitor_area: Rect::default(),
                work_area: Rect::default(),
                flags: 0,
            };

            let monitor = match window.current_monitor() {
                Some(handle) => handle.hmonitor(),
                None => return,
            };

            if GetMonitorInfoW(monitor, &mut info) == 0 {
                return;
            }

            let PhysicalSize { width, height } = window.outer_size();
            let work_area_width = info.work_area.right - info.work_area.left;
            let work_area_height = info.work_area.bottom - info.work_area.top;
            let hwnd = query_hwnd(&window);

            // check if the window is fullscreen borderless
            if width == work_area_width && height == work_area_height {
                SetWindowLongPtrW(hwnd, GWL_STYLE, STYLE);
                SetWindowPos(
                    hwnd,
                    0,
                    self.unwindowed_pos.x as u32,
                    self.unwindowed_pos.y as u32,
                    self.unwindowed_size.width,
                    self.unwindowed_size.height,
                    SWP_NOZORDER,
                );
            } else {
                self.unwindowed_size = window.outer_size();
                self.unwindowed_pos = window.outer_position().unwrap_or_default();

                SetWindowLongPtrW(hwnd, GWL_STYLE, STYLE_MAXIMIZED);
                SetWindowPos(
                    hwnd,
                    0,
                    info.work_area.left,
                    info.work_area.top,
                    work_area_width,
                    work_area_height,
                    SWP_NOZORDER,
                );
            }
        }
    }

    pub fn clipboard(_: &Window) -> Box<dyn ClipboardProvider> {
        ClipboardContext::new()
            .map(|clip| Box::new(clip) as Box<dyn ClipboardProvider>)
            .unwrap()
    }
}

const GWL_EXSTYLE: i32 = -20;
const GWL_STYLE: i32 = -16;

const WS_OVERLAPPED: isize = 0x00000000;
const WS_POPUP: isize = 0x80000000;
const WS_VISIBLE: isize = 0x10000000;
const WS_CLIPSIBLINGS: isize = 0x04000000;
const WS_BORDER: isize = 0x00800000;
const WS_SYSMENU: isize = 0x00080000;
const WS_EX_ACCEPTFILES: isize = 0x00000010;
const WS_EX_WINDOWEDGE: isize = 0x00000100;

const SWP_NOSIZE: i32 = 0x0001;
const SWP_NOMOVE: i32 = 0x0002;
const SWP_NOZORDER: i32 = 0x0004;
const SWP_FRAMECHANGED: i32 = 0x0020;

#[repr(C)]
#[derive(Default)]
pub struct Rect {
    pub left: u32,
    pub top: u32,
    pub right: u32,
    pub bottom: u32,
}

#[repr(C)]
pub struct MonitorInfo {
    pub size: u32,
    pub monitor_area: Rect,
    pub work_area: Rect,
    pub flags: u32,
}

extern "system" {
    fn SetWindowLongPtrW(handle: HWND, idx: i32, dw_new_long: isize) -> isize;
    fn SetWindowPos(
        handle: HWND,
        insert_after: HWND,
        x: u32,
        y: u32,
        cx: u32,
        cy: u32,
        flags: i32,
    ) -> i32;
    fn GetMonitorInfoW(monitor: HMONITOR, info: &mut MonitorInfo) -> i32;
}
