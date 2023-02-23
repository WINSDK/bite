use winit::event::{ModifiersState, VirtualKeyCode};

pub struct KeyMap {
    pressed: [bool; 255],
    modifier: ModifiersState,
}

impl KeyMap {
    pub fn new() -> Self {
        Self {
            pressed: [false; 255],
            modifier: ModifiersState::empty(),
        }
    }

    pub fn press(&mut self, keycode: VirtualKeyCode) {
        self.pressed[keycode as usize] = true;
    }

    pub fn release(&mut self, keycode: VirtualKeyCode) {
        self.pressed[keycode as usize] = false;
    }

    pub fn press_modifiers(&mut self, modifiers: ModifiersState) {
        self.modifier = modifiers;
    }

    pub fn pressed(&self, keycode: VirtualKeyCode, modifier: ModifiersState) -> bool {
        self.pressed[keycode as usize] & self.modifier.contains(modifier)
    }
}
