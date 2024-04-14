#![cfg(target_family = "unix")]

use std::mem::ManuallyDrop;
use std::path::Path;

use crate::panels::{self, Identifier};
use crate::{Error, Window, WinitEvent};
use copypasta::{ClipboardContext, ClipboardProvider};
use muda::{
    accelerator::{Accelerator, Code, Modifiers},
    CheckMenuItem, Menu, MenuItem, PredefinedMenuItem, Submenu,
};
use muda::{MenuEvent, MenuEventReceiver};
use winit::event_loop::{EventLoop, EventLoopBuilder};
use winit::platform::macos::EventLoopBuilderExtMacOS;
use winit::raw_window_handle::HasDisplayHandle;
use winit::window::WindowBuilder;

pub struct Arch {
    pub menu_channel: MenuEventReceiver,
    pub bar: MenuBar,
}

pub struct MenuBar {
    bar: Menu,
    windows: Vec<CheckMenuItem>,
}

impl MenuBar {
    fn build() -> muda::Result<Self> {
        let bar = Menu::new();
        let app_m = ManuallyDrop::new(Submenu::new("App", true));
        bar.append(&*app_m)?;
        app_m.append_items(&[
            &PredefinedMenuItem::services(None),
            &PredefinedMenuItem::separator(),
            &PredefinedMenuItem::hide(None),
            &PredefinedMenuItem::hide_others(None),
            &PredefinedMenuItem::show_all(None),
            &PredefinedMenuItem::separator(),
            &MenuItem::with_id(
                "open",
                "Open...",
                true,
                Some(Accelerator::new(Some(Modifiers::SUPER), Code::KeyO)),
            ),
            &PredefinedMenuItem::quit(None),
        ])?;

        let edit_m = ManuallyDrop::new(Submenu::new("&Edit", true));
        edit_m.append_items(&[
            &PredefinedMenuItem::copy(None),
            &PredefinedMenuItem::cut(None),
            &PredefinedMenuItem::paste(None),
        ])?;

        let window_m = ManuallyDrop::new(Submenu::new("&Window", true));

        let mut windows = Vec::new();
        windows.push(CheckMenuItem::with_id(
            panels::DISASSEMBLY,
            "Disassembly",
            true,
            true,
            None,
        ));
        windows.push(CheckMenuItem::with_id(
            panels::FUNCTIONS,
            "Functions",
            true,
            false,
            None,
        ));
        windows.push(CheckMenuItem::with_id(
            panels::SOURCE,
            "Source",
            true,
            false,
            None,
        ));
        windows.push(CheckMenuItem::with_id(
            panels::LOGGING,
            "Logging",
            true,
            false,
            None,
        ));

        for item in windows.iter() {
            window_m.append(item)?;
        }

        window_m.append_items(&[
            &PredefinedMenuItem::separator(),
            &PredefinedMenuItem::fullscreen(None),
        ])?;
        window_m.set_as_windows_menu_for_nsapp();

        bar.append_items(&[&*edit_m, &*window_m])?;
        bar.init_for_nsapp();

        Ok(Self { bar, windows })
    }

    pub fn set_checked(&self, ident: Identifier) {
        for item in self.windows.iter() {
            let is_ident = item.id().0.as_str() == ident;
            item.set_checked(is_ident);
        }
    }

    pub fn set_path(&self, path: &Path) {
        let path = path.to_string_lossy();
        let title_m = ManuallyDrop::new(Submenu::new(format!(":: {path}"), false));
        let _ = self.bar.append(&*title_m);
    }
}

impl Arch {
    pub fn new() -> Self {
        Self {
            menu_channel: MenuEvent::receiver().clone(),
            bar: MenuBar::build().unwrap(),
        }
    }

    pub fn create_event_loop() -> Result<EventLoop<WinitEvent>, Error> {
        EventLoopBuilder::<WinitEvent>::with_user_event()
            .with_default_menu(false)
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

        WindowBuilder::new()
            .with_title(title)
            .with_theme(Some(winit::window::Theme::Dark))
            .with_window_icon(icon)
            .with_inner_size(winit::dpi::LogicalSize { width, height })
            .build(event_loop)
            .map_err(|_| Error::WindowCreation)
    }

    pub fn fullscreen(&mut self, window: &Window) {
        let monitor = match window.current_monitor() {
            Some(monitor) => monitor,
            None => return,
        };

        window.set_fullscreen(match window.fullscreen() {
            Some(..) => None,
            None => Some(winit::window::Fullscreen::Borderless(Some(monitor))),
        });
    }

    pub fn clipboard(window: &Window) -> Box<dyn ClipboardProvider> {
        match window.display_handle().unwrap().as_raw() {
            #[cfg(target_os = "linux")]
            winit::raw_window_handle::RawDisplayHandle::Wayland(handle) => {
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
}
