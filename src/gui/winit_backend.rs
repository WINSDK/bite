//! You need to create a [`Platform`] and feed it with `winit::event::Event` events.
//! Use `begin_frame()` and `end_frame()` to start drawing the egui UI.

use std::collections::HashMap;

use copypasta::{ClipboardContext, ClipboardProvider};
use egui::emath::{pos2, vec2};
use egui::{Context, FontData, FontDefinitions, FontFamily, Key};
use winit::dpi::PhysicalSize;
use winit::event::{DeviceId, Event, ModifiersState, TouchPhase, VirtualKeyCode, WindowEvent};

/// Configures the creation of the `Platform`.
pub struct PlatformDescriptor {
    /// Width of the window in physical pixel.
    pub physical_width: u32,

    /// Height of the window in physical pixel.
    pub physical_height: u32,

    /// HiDPI scale factor.
    pub scale_factor: f32,

    /// Egui style configuration.
    pub style: egui::Style,

    /// Handle to winit.
    pub winit: winit::event_loop::EventLoopProxy<CustomEvent>,
}

/// A custom event type for the winit app.
pub enum CustomEvent {
    CloseRequest,
    DragWindow,
}

fn handle_clipboard(output: &egui::PlatformOutput, clipboard: Option<&mut ClipboardContext>) {
    if !output.copied_text.is_empty() {
        if let Some(clipboard) = clipboard {
            let _ = clipboard.set_contents(output.copied_text.clone());
        }
    }
}

/// Provides the integration between egui and winit.
pub struct Platform {
    scale_factor: f32,
    context: egui::Context,
    raw_input: egui::RawInput,
    raw_keys: Vec<(ModifiersState, VirtualKeyCode)>,
    modifier_state: ModifiersState,
    pointer_pos: Option<egui::Pos2>,
    clipboard: Option<ClipboardContext>,

    // For emulating pointer events from touch events we merge multi-touch
    // pointers, and ref-count the press state.
    touch_pointer_pressed: u32,

    // Egui requires unique u64 device IDs for touch events but Winit's
    // device IDs are opaque, so we have to create our own ID mapping.
    device_indices: HashMap<DeviceId, u64>,
    next_device_index: u64,

    dragging: bool,

    winit: winit::event_loop::EventLoopProxy<CustomEvent>,
}

impl Platform {
    pub fn new(descriptor: PlatformDescriptor) -> Self {
        let context = Context::default();
        let mut fonts = FontDefinitions::default();

        fonts.font_data.insert(
            "liga".to_owned(),
            FontData::from_static(include_bytes!(
                "../../assets/LigaSFMonoNerdFont-Regular.ttf"
            )),
        );

        fonts.font_data.insert(
            "icons".to_owned(),
            FontData::from_static(include_bytes!("../../assets/IcoMoon.ttf")),
        );

        fonts.families.get_mut(&FontFamily::Monospace).unwrap().push("icons".to_owned());
        fonts.families.get_mut(&FontFamily::Monospace).unwrap().push("liga".to_owned());

        context.set_fonts(fonts);
        context.set_style(descriptor.style);

        let raw_input = egui::RawInput {
            pixels_per_point: Some(descriptor.scale_factor as f32),
            screen_rect: Some(egui::Rect::from_min_size(
                egui::Pos2::default(),
                vec2(
                    descriptor.physical_width as f32,
                    descriptor.physical_height as f32,
                ) / descriptor.scale_factor as f32,
            )),
            ..Default::default()
        };

        Self {
            scale_factor: descriptor.scale_factor,
            raw_keys: Vec::new(),
            context,
            raw_input,
            modifier_state: ModifiersState::empty(),
            pointer_pos: Some(egui::Pos2::default()),
            clipboard: ClipboardContext::new().ok(),
            touch_pointer_pressed: 0,
            device_indices: HashMap::new(),
            next_device_index: 1,
            dragging: false,
            winit: descriptor.winit,
        }
    }

    /// Handles the given winit event and updates the egui context. Should be
    //called before starting a new frame with `start_frame()`.
    pub fn handle_event(&mut self, winit_event: &Event<CustomEvent>) {
        match winit_event {
            Event::WindowEvent {
                window_id: _window_id,
                event,
            } => match event {
                WindowEvent::Resized(PhysicalSize { width: 0, height: 0 }) => {}
                WindowEvent::Resized(physical_size) => {
                    self.raw_input.screen_rect = Some(egui::Rect::from_min_size(
                        Default::default(),
                        vec2(physical_size.width as f32, physical_size.height as f32)
                            / self.scale_factor as f32,
                    ));
                }
                WindowEvent::ScaleFactorChanged {
                    scale_factor,
                    new_inner_size,
                } => {
                    self.scale_factor = *scale_factor as f32;
                    self.raw_input.pixels_per_point = Some(self.scale_factor);
                    self.raw_input.screen_rect = Some(egui::Rect::from_min_size(
                        Default::default(),
                        vec2(new_inner_size.width as f32, new_inner_size.height as f32)
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

                    let force = match touch.force {
                        Some(winit::event::Force::Calibrated { force, .. }) => force as f32,
                        Some(winit::event::Force::Normalized(force)) => force as f32,
                        None => 0.0f32,
                    };

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
                    self.modifier_state = *input;
                    self.raw_input.modifiers = winit_to_egui_modifiers(*input);
                }
                WindowEvent::KeyboardInput { input, .. } => {
                    if let Some(virtual_keycode) = input.virtual_keycode {
                        let pressed = input.state == winit::event::ElementState::Pressed;
                        let ctrl = self.modifier_state.ctrl();

                        match (pressed, ctrl, virtual_keycode) {
                            (true, true, VirtualKeyCode::C) => {
                                self.raw_input.events.push(egui::Event::Copy)
                            }
                            (true, true, VirtualKeyCode::X) => {
                                self.raw_input.events.push(egui::Event::Cut)
                            }
                            (true, true, VirtualKeyCode::V) => {
                                if let Some(ref mut clipboard) = self.clipboard {
                                    if let Ok(contents) = clipboard.get_contents() {
                                        self.raw_input.events.push(egui::Event::Text(contents))
                                    }
                                }
                            }
                            _ => {
                                if pressed {
                                    self.raw_keys.push((self.modifier_state, virtual_keycode));
                                }

                                if let Some(key) = winit_to_egui_key_code(virtual_keycode) {
                                    self.raw_input.events.push(egui::Event::Key {
                                        key,
                                        pressed,
                                        modifiers: winit_to_egui_modifiers(self.modifier_state),
                                        repeat: false,
                                    });
                                }
                            }
                        }
                    }
                }
                WindowEvent::ReceivedCharacter(ch) => {
                    if is_printable(*ch)
                        && !self.modifier_state.ctrl()
                        && !self.modifier_state.logo()
                    {
                        self.raw_input.events.push(egui::Event::Text(ch.to_string()));
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn send_event(&self, custom_event: CustomEvent) {
        if let Err(..) = self.winit.send_event(custom_event) {
            panic!("missing an event loop to handle event");
        }
    }

    pub fn start_dragging(&mut self) {
        if !self.dragging {
            self.send_event(CustomEvent::DragWindow);
            self.dragging = true;
        }
    }

    pub fn stop_dragging(&mut self) {
        self.dragging = false;
    }

    /// Updates the internal time for egui used for animations. `elapsed_seconds` should be the
    /// seconds since some point in time (for example application start).
    pub fn update_time(&mut self, elapsed_seconds: f64) {
        self.raw_input.time = Some(elapsed_seconds);
    }

    /// Consumes all keys pressed.
    /// Returns true if an escape character was received: escape, enter, etc.
    pub fn raw_characters(&mut self) -> (String, bool) {
        let mut raw = String::new();

        for event in self.raw_input.events.iter() {
            match event {
                egui::Event::Text(received) => raw += &received,
                egui::Event::Key {
                    key: egui::Key::Backspace,
                    pressed: true,
                    modifiers: egui::Modifiers::NONE,
                    ..
                } => {
                    raw.pop();
                },
                egui::Event::Key {
                    key: egui::Key::Enter,
                    pressed: true,
                    modifiers: egui::Modifiers::NONE,
                    ..
                } => return (raw, true),
                egui::Event::Key {
                    key: egui::Key::Escape,
                    pressed: true,
                    modifiers: egui::Modifiers::NONE,
                    ..
                } => return (raw, true),
                _ => {}
            }
        }

        (raw, false)
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

        handle_clipboard(&output.platform_output, self.clipboard.as_mut());

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
fn winit_to_egui_key_code(key: VirtualKeyCode) -> Option<egui::Key> {
    Some(match key {
        VirtualKeyCode::Escape => Key::Escape,
        VirtualKeyCode::Insert => Key::Insert,
        VirtualKeyCode::Home => Key::Home,
        VirtualKeyCode::Delete => Key::Delete,
        VirtualKeyCode::End => Key::End,
        VirtualKeyCode::PageDown => Key::PageDown,
        VirtualKeyCode::PageUp => Key::PageUp,
        VirtualKeyCode::Left => Key::ArrowLeft,
        VirtualKeyCode::Up => Key::ArrowUp,
        VirtualKeyCode::Right => Key::ArrowRight,
        VirtualKeyCode::Down => Key::ArrowDown,
        VirtualKeyCode::Back => Key::Backspace,
        VirtualKeyCode::Return => Key::Enter,
        VirtualKeyCode::Tab => Key::Tab,
        VirtualKeyCode::Space => Key::Space,
        VirtualKeyCode::Key1 => Key::Num1,
        VirtualKeyCode::Key2 => Key::Num2,
        VirtualKeyCode::Key3 => Key::Num3,
        VirtualKeyCode::Key4 => Key::Num4,
        VirtualKeyCode::Key5 => Key::Num5,
        VirtualKeyCode::Key6 => Key::Num6,
        VirtualKeyCode::Key7 => Key::Num7,
        VirtualKeyCode::Key8 => Key::Num8,
        VirtualKeyCode::Key9 => Key::Num9,
        VirtualKeyCode::Key0 => Key::Num0,
        VirtualKeyCode::A => Key::A,
        VirtualKeyCode::B => Key::B,
        VirtualKeyCode::C => Key::C,
        VirtualKeyCode::D => Key::D,
        VirtualKeyCode::E => Key::E,
        VirtualKeyCode::F => Key::F,
        VirtualKeyCode::G => Key::G,
        VirtualKeyCode::H => Key::H,
        VirtualKeyCode::I => Key::I,
        VirtualKeyCode::J => Key::J,
        VirtualKeyCode::K => Key::K,
        VirtualKeyCode::L => Key::L,
        VirtualKeyCode::M => Key::M,
        VirtualKeyCode::N => Key::N,
        VirtualKeyCode::O => Key::O,
        VirtualKeyCode::P => Key::P,
        VirtualKeyCode::Q => Key::Q,
        VirtualKeyCode::R => Key::R,
        VirtualKeyCode::S => Key::S,
        VirtualKeyCode::T => Key::T,
        VirtualKeyCode::U => Key::U,
        VirtualKeyCode::V => Key::V,
        VirtualKeyCode::W => Key::W,
        VirtualKeyCode::X => Key::X,
        VirtualKeyCode::Y => Key::Y,
        VirtualKeyCode::Z => Key::Z,
        _ => return None,
    })
}

/// Translates winit to egui modifier keys.
#[inline]
fn winit_to_egui_modifiers(modifiers: ModifiersState) -> egui::Modifiers {
    egui::Modifiers {
        alt: modifiers.alt(),
        ctrl: modifiers.ctrl(),
        shift: modifiers.shift(),
        #[cfg(target_os = "macos")]
        mac_cmd: modifiers.logo(),
        #[cfg(target_os = "macos")]
        command: modifiers.logo(),
        #[cfg(not(target_os = "macos"))]
        mac_cmd: false,
        #[cfg(not(target_os = "macos"))]
        command: modifiers.ctrl(),
    }
}

#[inline]
fn egui_to_winit_cursor_icon(icon: egui::CursorIcon) -> Option<winit::window::CursorIcon> {
    match icon {
        egui::CursorIcon::Default => Some(winit::window::CursorIcon::Default),
        egui::CursorIcon::ContextMenu => Some(winit::window::CursorIcon::ContextMenu),
        egui::CursorIcon::Help => Some(winit::window::CursorIcon::Help),
        egui::CursorIcon::PointingHand => Some(winit::window::CursorIcon::Hand),
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
