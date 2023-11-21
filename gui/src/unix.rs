#![cfg(target_family = "unix")]

use crate::Error;

use copypasta::{ClipboardContext, ClipboardProvider};
use winit::raw_window_handle::{HasDisplayHandle, RawDisplayHandle};

pub struct Arch {}

impl crate::Target for Arch {
    fn new() -> Self {
        Self {}
    }

    fn clipboard(window: &crate::Window) -> Box<dyn ClipboardProvider> {
        match window.display_handle().unwrap().as_raw() {
            #[cfg(target_os = "linux")]
            RawDisplayHandle::Wayland(handle) => {
                // wayland requires a display handle when creating a clipboard
                let (_primary, clipboard) = unsafe {
                    copypasta::wayland_clipboard::create_clipboards_from_external(
                        handle.display.as_ptr(),
                    )
                };

                Box::new(clipboard) as Box<dyn ClipboardProvider>
            }
            _ => ClipboardContext::new()
                .map(|clip| Box::new(clip) as Box<dyn ClipboardProvider>)
                .unwrap(),
        }
    }

    fn create_window<T>(
        title: &str,
        width: u32,
        height: u32,
        event_loop: &winit::event_loop::EventLoop<T>,
    ) -> Result<winit::window::Window, Error> {
        let icon = crate::icon::PngIcon::decode("./assets/iconx64.png")?;
        let icon = winit::window::Icon::from_rgba(icon.data, icon.width, icon.height).ok();

        winit::window::WindowBuilder::new()
            .with_title(title)
            .with_theme(Some(winit::window::Theme::Dark))
            .with_window_icon(icon)
            .with_inner_size(winit::dpi::LogicalSize { width, height })
            .build(event_loop)
            .map_err(|_| Error::WindowCreation)
    }

    fn fullscreen(&mut self, window: &crate::Window) {
        let monitor = match window.current_monitor() {
            Some(monitor) => monitor,
            None => return,
        };

        window.set_fullscreen(match window.fullscreen() {
            Some(..) => None,
            None => Some(winit::window::Fullscreen::Borderless(Some(monitor))),
        });
    }
}
