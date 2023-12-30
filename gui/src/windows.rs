#![cfg(target_os = "windows")]

use copypasta::{ClipboardContext, ClipboardProvider};
use winit::dpi::{PhysicalPosition, PhysicalSize};
use winit::platform::windows::HMONITOR;
use winit::platform::windows::HWND;
use winit::platform::windows::{MonitorHandleExtWindows, WindowBuilderExtWindows};
use winit::raw_window_handle::HasWindowHandle;

use crate::{Error, Window};

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

impl crate::Target for Arch {
    fn new(arch_desc: ArchDescriptor) -> Self {
        Self {
            unwindowed_size: arch_desc.initial_size,
            unwindowed_pos: arch_desc.initial_pos,
        }
    }

    fn clipboard(_: &crate::Window) -> Box<dyn ClipboardProvider> {
        ClipboardContext::new()
            .map(|clip| Box::new(clip) as Box<dyn ClipboardProvider>)
            .unwrap()
    }

    fn create_window<T>(
        title: &str,
        width: u32,
        height: u32,
        event_loop: &winit::event_loop::EventLoop<T>,
    ) -> Result<winit::window::Window, Error> {
        let icon = include_bytes!("../../assets/iconx256.png");
        let icon = crate::icon::PngIcon::decode_bytes(icon)?;
        let icon = winit::window::Icon::from_rgba(icon.data, icon.width, icon.height).ok();

        let window = winit::window::WindowBuilder::new()
            .with_title(title)
            .with_visible(false)
            .with_decorations(true)
            .with_taskbar_icon(icon.clone())
            .with_window_icon(icon)
            .with_inner_size(winit::dpi::LogicalSize { width, height })
            .build(event_loop)
            .map_err(|_| Error::WindowCreation)?;

        let PhysicalSize { width, height } =
            window.current_monitor().ok_or(Error::WindowCreation)?.size();

        let hwnd = query_hwnd(&window);

        unsafe {
            let width = width * 2 / 5;
            let height = height * 2 / 3;

            // set basic window attributes
            let attr = WS_THICKFRAME | WS_POPUP;
            if SetWindowLongPtrW(hwnd, GWL_STYLE, attr) == 0 {
                return Err(Error::WindowCreation);
            }

            // set extended window attributes
            if SetWindowLongPtrW(hwnd, GWL_EXSTYLE, WS_EX_ACCEPTFILES) == 0 {
                return Err(Error::WindowCreation);
            }

            // resize window to some reasonable dimensions, whilst applying the window attributes
            if SetWindowPos(hwnd, HWND_TOP, 0, 0, width, height, SWP_NOZORDER) == 0 {
                return Err(Error::WindowCreation);
            }

            // set window visibility
            if SetWindowLongPtrW(hwnd, GWL_STYLE, attr | WS_VISIBLE) == 0 {
                return Err(Error::WindowCreation);
            }
        }

        Ok(window)
    }

    fn fullscreen(&mut self, window: &crate::Window) {
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
                let attr = WS_VISIBLE | WS_THICKFRAME | WS_POPUP;

                SetWindowLongPtrW(hwnd, GWL_STYLE, attr);
                SetWindowPos(
                    hwnd,
                    HWND_TOP,
                    self.unwindowed_pos.x as u32,
                    self.unwindowed_pos.y as u32,
                    self.unwindowed_size.width,
                    self.unwindowed_size.height,
                    SWP_NOZORDER,
                );
            } else {
                let attr = WS_VISIBLE | WS_OVERLAPPED;

                self.unwindowed_size = window.outer_size();
                self.unwindowed_pos = window.outer_position().unwrap_or_default();

                SetWindowLongPtrW(hwnd, GWL_STYLE, attr);
                SetWindowPos(
                    hwnd,
                    HWND_TOP,
                    info.work_area.left,
                    info.work_area.top,
                    work_area_width,
                    work_area_height,
                    SWP_NOZORDER,
                );
            }
        }
    }
}

pub const GWL_EXSTYLE: i32 = -20;
pub const GWL_STYLE: i32 = -16;
pub const SWP_NOZORDER: i32 = 4;
pub const WS_POPUP: isize = 2147483648;
pub const WS_VISIBLE: isize = 268435456;
pub const WS_THICKFRAME: isize = 262144;
pub const WS_EX_ACCEPTFILES: isize = 16;
pub const WS_OVERLAPPED: isize = 0;
pub const HWND_TOP: isize = 0;

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
    pub fn SetWindowLongPtrW(handle: HWND, idx: i32, dw_new_long: isize) -> isize;
    pub fn SetWindowPos(
        handle: HWND,
        insert_after: HWND,
        x: u32,
        y: u32,
        cx: u32,
        cy: u32,
        flags: i32,
    ) -> i32;
    pub fn GetMonitorInfoW(monitor: HMONITOR, info: &mut MonitorInfo) -> i32;
}
