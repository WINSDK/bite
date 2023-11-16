use crate::gui::Error;

pub struct Timer {
    start: std::time::Instant,
    ups: usize,
}

impl Timer {
    pub fn new(ups: usize) -> Self {
        Self {
            start: std::time::Instant::now(),
            ups,
        }
    }

    pub fn reached(&self) -> bool {
        self.start.elapsed().as_millis() as usize * self.ups > 1000
    }

    pub fn reset(&mut self) {
        self.start = std::time::Instant::now();
    }
}

impl std::ops::Deref for Timer {
    type Target = std::time::Instant;

    fn deref(&self) -> &Self::Target {
        &self.start
    }
}

pub struct Png {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
}

pub fn decode_png<P: AsRef<std::path::Path>>(path: P) -> Result<Png, Error> {
    let bytes = std::fs::read(&path).map_err(|_| Error::NotFound(path.as_ref().to_owned()))?;
    decode_png_bytes(&bytes)
}

pub fn decode_png_bytes(bytes: &[u8]) -> Result<Png, Error> {
    let mut decoder = png::Decoder::new(bytes);
    decoder.set_transformations(png::Transformations::STRIP_16 | png::Transformations::EXPAND);

    let mut reader = decoder.read_info().map_err(|_| Error::PngDecode)?;
    let mut data = vec![0; reader.output_buffer_size()];
    let info = reader.next_frame(&mut data).map_err(|_| Error::PngDecode)?;

    if info.width == 0 || info.height == 0 {
        return Err(Error::PngDecode);
    }

    if info.color_type != png::ColorType::Rgba {
        return Err(Error::PngFormat);
    }

    Ok(Png {
        data,
        width: info.width,
        height: info.height,
    })
}

#[cfg(target_os = "windows")]
pub mod windows {
    use winit::platform::windows::HMONITOR;
    use winit::platform::windows::HWND;

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
}

#[cfg(target_family = "unix")]
pub fn generate_window<T>(
    title: &str,
    icon: Option<winit::window::Icon>,
    event_loop: &winit::event_loop::EventLoop<T>,
) -> Result<winit::window::Window, Error> {
    winit::window::WindowBuilder::new()
        .with_title(title)
        .with_theme(Some(winit::window::Theme::Dark))
        .with_window_icon(icon)
        .with_inner_size(winit::dpi::LogicalSize {
            width: super::WIDTH,
            height: super::HEIGHT,
        })
        .build(event_loop)
        .map_err(|_| Error::WindowCreation)
}

#[cfg(target_family = "windows")]
pub fn generate_window<T>(
    title: &str,
    icon: Option<winit::window::Icon>,
    event_loop: &winit::event_loop::EventLoop<T>,
) -> Result<winit::window::Window, Error> {
    use windows::*;
    use winit::dpi::PhysicalSize;
    use winit::platform::windows::{WindowBuilderExtWindows, WindowExtWindows};

    let window = winit::window::WindowBuilder::new()
        .with_title(title)
        .with_visible(false)
        .with_decorations(true)
        .with_taskbar_icon(icon.clone())
        .with_window_icon(icon)
        .with_inner_size(winit::dpi::LogicalSize {
            width: super::WIDTH,
            height: super::HEIGHT,
        })
        .build(event_loop)
        .map_err(|_| Error::WindowCreation)?;

    let PhysicalSize { width, height } =
        window.current_monitor().ok_or(Error::WindowCreation)?.size();

    unsafe {
        let width = width * 2 / 5;
        let height = height * 2 / 3;

        // set basic window attributes
        let attr = WS_THICKFRAME | WS_POPUP;
        if SetWindowLongPtrW(window.hwnd(), GWL_STYLE, attr) == 0 {
            return Err(Error::WindowCreation);
        }

        // set extended window attributes
        if SetWindowLongPtrW(window.hwnd(), GWL_EXSTYLE, WS_EX_ACCEPTFILES) == 0 {
            return Err(Error::WindowCreation);
        }

        // resize window to some reasonable dimensions, whilst applying the window attributes
        if SetWindowPos(window.hwnd(), HWND_TOP, 0, 0, width, height, SWP_NOZORDER) == 0 {
            return Err(Error::WindowCreation);
        }

        // set window visibility
        if SetWindowLongPtrW(window.hwnd(), GWL_STYLE, attr | WS_VISIBLE) == 0 {
            return Err(Error::WindowCreation);
        }
    }

    Ok(window)
}
