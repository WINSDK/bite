//! You need to create a [`Platform`] and feed it with `winit::event::Event` events.
//! Use `begin_frame()` and `end_frame()` to start drawing the egui UI.

use crate::Window;

use std::collections::HashMap;

use copypasta::ClipboardProvider;
use egui::emath::{pos2, vec2};
use egui::{Context, FontData, FontDefinitions, FontFamily, Key};

use winit::dpi::PhysicalSize;
use winit::event::{DeviceId, Event, TouchPhase, WindowEvent, KeyEvent};
use winit::keyboard::{KeyCode, ModifiersState};

/// Provides the integration between egui and winit.
pub struct Platform {
    scale_factor: f32,
    context: egui::Context,
    raw_input: egui::RawInput,
    modifier_state: ModifiersState,
    pointer_pos: Option<egui::Pos2>,
    clipboard: Box<dyn ClipboardProvider>,

    // for emulating pointer events from touch events we merge multi-touch
    // pointers, and ref-count the press state
    touch_pointer_pressed: u32,

    // egui requires unique u64 device IDs for touch events but Winit's
    // device IDs are opaque, so we have to create our own ID mapping
    device_indices: HashMap<DeviceId, u64>,
    next_device_index: u64,
}

impl Platform {
    pub fn new<Arch: crate::Target>(window: &Window) -> Self {
        let scale_factor = window.scale_factor() as f32;
        let context = Context::default();
        let mut fonts = FontDefinitions::default();

        fonts.font_data.insert(
            "liga".to_owned(),
            FontData::from_static(include_bytes!("../fonts/LigaSFMonoNerdFont-Regular.ttf")),
        );

        fonts.font_data.insert(
            "icons".to_owned(),
            FontData::from_static(include_bytes!("../fonts/IcoMoon.ttf")),
        );

        fonts.families.get_mut(&FontFamily::Monospace).unwrap().push("icons".to_owned());
        fonts.families.get_mut(&FontFamily::Monospace).unwrap().push("liga".to_owned());

        context.set_fonts(fonts);
        context.set_style(crate::style::EGUI.clone());

        let raw_input = egui::RawInput {
            pixels_per_point: Some(2.0 * scale_factor),
            screen_rect: Some(egui::Rect::from_min_size(
                egui::Pos2::default(),
                vec2(
                    window.inner_size().width as f32,
                    window.inner_size().height as f32,
                ) / scale_factor,
            )),
            ..Default::default()
        };

        Self {
            scale_factor,
            context,
            raw_input,
            modifier_state: ModifiersState::empty(),
            pointer_pos: Some(egui::Pos2::default()),
            clipboard: Arch::clipboard(window),
            touch_pointer_pressed: 0,
            device_indices: HashMap::new(),
            next_device_index: 1,
        }
    }

    pub fn unprocessed_events(&mut self) -> &mut Vec<egui::Event> {
        &mut self.raw_input.events
    }

    fn handle_key_text(&mut self, pressed: bool, event: &KeyEvent) {
        if pressed {
            if let Some(ref text) = event.text {
                if text.chars().all(is_printable) {
                    self.raw_input
                        .events
                        .push(egui::Event::Text(text.to_string()));
                }
            }
        }
    }

    /// Handles the given winit event and updates the egui context. Should be
    // called before starting a new frame with `start_frame()`.
    pub fn handle_event(&mut self, window: &Window, winit_event: &mut Event<crate::CustomEvent>) {
        match winit_event {
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::KeyboardInput { event, .. } => {
                    let pressed = event.state == winit::event::ElementState::Pressed;
                    let ctrl = self.modifier_state.control_key();

                    if let winit::keyboard::PhysicalKey::Code(key) = event.physical_key {
                        match (pressed, ctrl, key) {
                            (true, true, KeyCode::KeyC) => {
                                self.raw_input.events.push(egui::Event::Copy);
                            }
                            (true, true, KeyCode::KeyX) => {
                                self.raw_input.events.push(egui::Event::Cut)
                            }
                            (true, true, KeyCode::KeyV) => {
                                if let Ok(contents) = self.clipboard.get_contents() {
                                    self.raw_input.events.push(egui::Event::Text(contents))
                                }
                            }
                            _ => {
                                if let Some(key) = winit_to_egui_key_code(key) {
                                    // we must first push the key and then the value
                                    // so the terminal can potentially get rid of a keypress

                                    self.raw_input.events.push(egui::Event::Key {
                                        key,
                                        pressed,
                                        modifiers: winit_to_egui_modifiers(self.modifier_state),
                                        repeat: false,
                                    });

                                    self.handle_key_text(pressed, event);
                                }
                            }
                        }
                    }
                }
                WindowEvent::ScaleFactorChanged { scale_factor, .. } => {
                    // update the window's size
                    let new_inner_size = window.inner_size();
                    let new_inner_size = PhysicalSize {
                        width: new_inner_size.width * *scale_factor as u32,
                        height: new_inner_size.height * *scale_factor as u32,
                    };

                    let _ = window.request_inner_size(new_inner_size);

                    self.scale_factor = *scale_factor as f32;
                    self.raw_input.pixels_per_point = Some(*scale_factor as f32);
                    self.raw_input.screen_rect = Some(egui::Rect::from_min_size(
                        Default::default(),
                        vec2(new_inner_size.width as f32, new_inner_size.height as f32)
                            / self.scale_factor as f32,
                    ));
                }
                WindowEvent::Resized(physical_size) => {
                    if physical_size.width == 0 && physical_size.height == 0 {
                        return;
                    }

                    self.raw_input.screen_rect = Some(egui::Rect::from_min_size(
                        Default::default(),
                        vec2(physical_size.width as f32, physical_size.height as f32)
                            / self.scale_factor as f32,
                    ));
                }
                WindowEvent::MouseInput { state, button, .. } => {
                    // push event only if the cursor is inside the window
                    if let Some(pointer_pos) = self.pointer_pos {
                        self.raw_input.events.push(egui::Event::PointerButton {
                            pos: pointer_pos,
                            button: match button {
                                winit::event::MouseButton::Left => egui::PointerButton::Primary,
                                winit::event::MouseButton::Right => egui::PointerButton::Secondary,
                                winit::event::MouseButton::Middle => egui::PointerButton::Middle,
                                winit::event::MouseButton::Back => egui::PointerButton::Extra1,
                                winit::event::MouseButton::Forward => egui::PointerButton::Extra2,
                                winit::event::MouseButton::Other(_) => return,
                            },
                            pressed: *state == winit::event::ElementState::Pressed,
                            modifiers: Default::default(),
                        });
                    }
                }
                WindowEvent::Touch(touch) => {
                    let pointer_pos = pos2(
                        touch.location.x as f32 / self.scale_factor as f32,
                        touch.location.y as f32 / self.scale_factor as f32,
                    );

                    let device_id = match self.device_indices.get(&touch.device_id) {
                        Some(id) => *id,
                        None => {
                            let device_id = self.next_device_index;
                            self.device_indices.insert(touch.device_id, device_id);
                            self.next_device_index += 1;
                            device_id
                        }
                    };
                    let egui_phase = match touch.phase {
                        TouchPhase::Started => egui::TouchPhase::Start,
                        TouchPhase::Moved => egui::TouchPhase::Move,
                        TouchPhase::Ended => egui::TouchPhase::End,
                        TouchPhase::Cancelled => egui::TouchPhase::Cancel,
                    };

                    let force = touch.force.map(|f| match f {
                        winit::event::Force::Calibrated { force, .. } => force as f32,
                        winit::event::Force::Normalized(force) => force as f32,
                    });

                    self.raw_input.events.push(egui::Event::Touch {
                        device_id: egui::TouchDeviceId(device_id),
                        id: egui::TouchId(touch.id),
                        phase: egui_phase,
                        pos: pointer_pos,
                        force,
                    });

                    // Currently Winit doesn't emulate pointer events based on
                    // touch events but Egui requires pointer emulation.
                    //
                    // For simplicity we just merge all touch pointers into a
                    // single virtual pointer and ref-count the press state
                    // (i.e. the pointer will remain pressed during multi-touch
                    // events until the last pointer is lifted up)
                    let was_pressed = self.touch_pointer_pressed > 0;

                    match touch.phase {
                        TouchPhase::Started => {
                            self.touch_pointer_pressed += 1;
                        }
                        TouchPhase::Ended | TouchPhase::Cancelled => {
                            self.touch_pointer_pressed =
                                self.touch_pointer_pressed.checked_sub(1).unwrap_or(0);
                        }
                        TouchPhase::Moved => {
                            self.raw_input.events.push(egui::Event::PointerMoved(pointer_pos));
                        }
                    }

                    if !was_pressed && self.touch_pointer_pressed > 0 {
                        self.raw_input.events.push(egui::Event::PointerButton {
                            pos: pointer_pos,
                            button: egui::PointerButton::Primary,
                            pressed: true,
                            modifiers: Default::default(),
                        });
                    } else if was_pressed && self.touch_pointer_pressed == 0 {
                        // Egui docs say that the pressed=false should be sent _before_
                        // the PointerGone.
                        self.raw_input.events.push(egui::Event::PointerButton {
                            pos: pointer_pos,
                            button: egui::PointerButton::Primary,
                            pressed: false,
                            modifiers: Default::default(),
                        });
                        self.raw_input.events.push(egui::Event::PointerGone);
                    }
                }
                WindowEvent::MouseWheel { delta, .. } => {
                    let delta = match *delta {
                        winit::event::MouseScrollDelta::LineDelta(x, y) => {
                            let line_height = 50.0;
                            vec2(x, y) * line_height
                        }
                        winit::event::MouseScrollDelta::PixelDelta(delta) => {
                            vec2(delta.x as f32, delta.y as f32)
                        }
                    };

                    // The ctrl (cmd on macos) key indicates a zoom is desired.
                    if self.raw_input.modifiers.ctrl || self.raw_input.modifiers.command {
                        self.raw_input.events.push(egui::Event::Zoom((delta.y / 200.0).exp()));
                    } else {
                        self.raw_input.events.push(egui::Event::Scroll(delta));
                    }
                }
                WindowEvent::CursorMoved { position, .. } => {
                    let pointer_pos = pos2(
                        position.x as f32 / self.scale_factor as f32,
                        position.y as f32 / self.scale_factor as f32,
                    );
                    self.pointer_pos = Some(pointer_pos);
                    self.raw_input.events.push(egui::Event::PointerMoved(pointer_pos));
                }
                WindowEvent::CursorLeft { .. } => {
                    self.pointer_pos = None;
                    self.raw_input.events.push(egui::Event::PointerGone);
                }
                WindowEvent::ModifiersChanged(input) => {
                    self.modifier_state = input.state();
                    self.raw_input.modifiers = winit_to_egui_modifiers(input.state());
                }
                _ => {}
            },
            _ => {}
        }
    }

    /// Updates the internal time for egui used for animations. `elapsed_seconds` should be the
    /// seconds since some point in time (for example application start).
    pub fn update_time(&mut self, elapsed_seconds: f64) {
        self.raw_input.time = Some(elapsed_seconds);
    }

    /// Starts a new frame by providing a new `Ui` instance to write into.
    pub fn begin_frame(&mut self) {
        self.context.begin_frame(self.raw_input.take());
    }

    /// Ends the frame. Returns what has happened as `Output` and gives you the draw instructions
    /// as `PaintJobs`. If the optional `window` is set, it will set the cursor key based on
    /// egui's instructions.
    pub fn end_frame(&mut self, window: Option<&winit::window::Window>) -> egui::FullOutput {
        let output = self.context.end_frame();

        if let Some(window) = window {
            if let Some(cursor_icon) = egui_to_winit_cursor_icon(output.platform_output.cursor_icon)
            {
                window.set_cursor_visible(true);
                // if the pointer is located inside the window, set cursor icon
                if self.pointer_pos.is_some() {
                    window.set_cursor_icon(cursor_icon);
                }
            } else {
                window.set_cursor_visible(false);
            }
        }

        let copied_text = &output.platform_output.copied_text;
        if !copied_text.is_empty() {
            let _ = self.clipboard.set_contents(copied_text.clone());
        }

        output
    }

    /// Returns the internal egui context.
    pub fn context(&self) -> Context {
        self.context.clone()
    }

    /// Current scale factor being used to render the UI.
    pub fn scale_factor(&self) -> f32 {
        self.scale_factor
    }
}

/// Translates winit to egui keycodes.
#[inline]
fn winit_to_egui_key_code(key: KeyCode) -> Option<egui::Key> {
    Some(match key {
        KeyCode::Escape => Key::Escape,
        KeyCode::Insert => Key::Insert,
        KeyCode::Home => Key::Home,
        KeyCode::Delete => Key::Delete,
        KeyCode::End => Key::End,
        KeyCode::PageDown => Key::PageDown,
        KeyCode::PageUp => Key::PageUp,
        KeyCode::ArrowLeft => Key::ArrowLeft,
        KeyCode::ArrowUp => Key::ArrowUp,
        KeyCode::ArrowRight => Key::ArrowRight,
        KeyCode::ArrowDown => Key::ArrowDown,
        KeyCode::Backspace => Key::Backspace,
        KeyCode::Enter => Key::Enter,
        KeyCode::Tab => Key::Tab,
        KeyCode::Space => Key::Space,
        KeyCode::Digit1 => Key::Num1,
        KeyCode::Digit2 => Key::Num2,
        KeyCode::Digit3 => Key::Num3,
        KeyCode::Digit4 => Key::Num4,
        KeyCode::Digit5 => Key::Num5,
        KeyCode::Digit6 => Key::Num6,
        KeyCode::Digit7 => Key::Num7,
        KeyCode::Digit8 => Key::Num8,
        KeyCode::Digit9 => Key::Num9,
        KeyCode::Digit0 => Key::Num0,
        KeyCode::KeyA => Key::A,
        KeyCode::KeyB => Key::B,
        KeyCode::KeyC => Key::C,
        KeyCode::KeyD => Key::D,
        KeyCode::KeyE => Key::E,
        KeyCode::KeyF => Key::F,
        KeyCode::KeyG => Key::G,
        KeyCode::KeyH => Key::H,
        KeyCode::KeyI => Key::I,
        KeyCode::KeyJ => Key::J,
        KeyCode::KeyK => Key::K,
        KeyCode::KeyL => Key::L,
        KeyCode::KeyM => Key::M,
        KeyCode::KeyN => Key::N,
        KeyCode::KeyO => Key::O,
        KeyCode::KeyP => Key::P,
        KeyCode::KeyQ => Key::Q,
        KeyCode::KeyR => Key::R,
        KeyCode::KeyS => Key::S,
        KeyCode::KeyT => Key::T,
        KeyCode::KeyU => Key::U,
        KeyCode::KeyV => Key::V,
        KeyCode::KeyW => Key::W,
        KeyCode::KeyX => Key::X,
        KeyCode::KeyY => Key::Y,
        KeyCode::KeyZ => Key::Z,
        _ => return None,
    })
}

/// Translates winit to egui modifier keys.
#[inline]
fn winit_to_egui_modifiers(modifiers: ModifiersState) -> egui::Modifiers {
    egui::Modifiers {
        alt: modifiers.alt_key(),
        ctrl: modifiers.control_key(),
        shift: modifiers.shift_key(),
        #[cfg(target_os = "macos")]
        mac_cmd: modifiers.super_key(),
        #[cfg(target_os = "macos")]
        command: modifiers.super_key(),
        #[cfg(not(target_os = "macos"))]
        mac_cmd: false,
        #[cfg(not(target_os = "macos"))]
        command: modifiers.control_key(),
    }
}

#[inline]
fn egui_to_winit_cursor_icon(icon: egui::CursorIcon) -> Option<winit::window::CursorIcon> {
    match icon {
        egui::CursorIcon::Default => Some(winit::window::CursorIcon::Default),
        egui::CursorIcon::ContextMenu => Some(winit::window::CursorIcon::ContextMenu),
        egui::CursorIcon::Help => Some(winit::window::CursorIcon::Help),
        egui::CursorIcon::PointingHand => Some(winit::window::CursorIcon::Pointer),
        egui::CursorIcon::Progress => Some(winit::window::CursorIcon::Progress),
        egui::CursorIcon::Wait => Some(winit::window::CursorIcon::Wait),
        egui::CursorIcon::Cell => Some(winit::window::CursorIcon::Cell),
        egui::CursorIcon::Crosshair => Some(winit::window::CursorIcon::Crosshair),
        egui::CursorIcon::Text => Some(winit::window::CursorIcon::Text),
        egui::CursorIcon::VerticalText => Some(winit::window::CursorIcon::VerticalText),
        egui::CursorIcon::Alias => Some(winit::window::CursorIcon::Alias),
        egui::CursorIcon::Copy => Some(winit::window::CursorIcon::Copy),
        egui::CursorIcon::Move => Some(winit::window::CursorIcon::Move),
        egui::CursorIcon::NoDrop => Some(winit::window::CursorIcon::NoDrop),
        egui::CursorIcon::NotAllowed => Some(winit::window::CursorIcon::NotAllowed),
        egui::CursorIcon::Grab => Some(winit::window::CursorIcon::Grab),
        egui::CursorIcon::Grabbing => Some(winit::window::CursorIcon::Grabbing),
        egui::CursorIcon::AllScroll => Some(winit::window::CursorIcon::AllScroll),
        egui::CursorIcon::ResizeHorizontal => Some(winit::window::CursorIcon::EwResize),
        egui::CursorIcon::ResizeNeSw => Some(winit::window::CursorIcon::NeswResize),
        egui::CursorIcon::ResizeNwSe => Some(winit::window::CursorIcon::NwseResize),
        egui::CursorIcon::ResizeVertical => Some(winit::window::CursorIcon::NsResize),
        egui::CursorIcon::ResizeEast => Some(winit::window::CursorIcon::EResize),
        egui::CursorIcon::ResizeSouthEast => Some(winit::window::CursorIcon::SeResize),
        egui::CursorIcon::ResizeSouth => Some(winit::window::CursorIcon::SResize),
        egui::CursorIcon::ResizeSouthWest => Some(winit::window::CursorIcon::SwResize),
        egui::CursorIcon::ResizeWest => Some(winit::window::CursorIcon::WResize),
        egui::CursorIcon::ResizeNorthWest => Some(winit::window::CursorIcon::NwResize),
        egui::CursorIcon::ResizeNorth => Some(winit::window::CursorIcon::NResize),
        egui::CursorIcon::ResizeNorthEast => Some(winit::window::CursorIcon::NeResize),
        egui::CursorIcon::ResizeColumn => Some(winit::window::CursorIcon::ColResize),
        egui::CursorIcon::ResizeRow => Some(winit::window::CursorIcon::RowResize),
        egui::CursorIcon::ZoomIn => Some(winit::window::CursorIcon::ZoomIn),
        egui::CursorIcon::ZoomOut => Some(winit::window::CursorIcon::ZoomOut),
        egui::CursorIcon::None => Option::None,
    }
}

/// We only want printable characters and ignore all special keys.
#[inline]
fn is_printable(chr: char) -> bool {
    let is_in_private_use_area = ('\u{e000}'..='\u{f8ff}').contains(&chr)
        || ('\u{f0000}'..='\u{ffffd}').contains(&chr)
        || ('\u{100000}'..='\u{10fffd}').contains(&chr);

    !is_in_private_use_area && !chr.is_ascii_control()
}
