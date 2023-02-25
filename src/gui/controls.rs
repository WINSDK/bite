use winit::event::{ModifiersState, VirtualKeyCode};

#[derive(Default, Copy, Clone)]
struct Key {
    pressed: bool,
    down: bool,
}

pub struct KeyMap {
    pressed: [Key; 200],
    modifier: ModifiersState,
}

impl KeyMap {
    pub fn new() -> Self {
        Self {
            pressed: [Key::default(); 200],
            modifier: ModifiersState::empty(),
        }
    }

    pub fn press(&mut self, keycode: VirtualKeyCode) {
        let key = &mut self.pressed[keycode as usize];

        if key.pressed {
            key.down = true;
        }

        key.pressed = true;
    }

    pub fn press_modifiers(&mut self, modifiers: ModifiersState) {
        self.modifier = modifiers;
    }

    pub fn release(&mut self, keycode: VirtualKeyCode) {
        let key = &mut self.pressed[keycode as usize];

        key.pressed = false;
        key.down = false;
    }

    pub fn release_pressed(&mut self) {
        for press in self.pressed.iter_mut().filter(|key| !key.down) {
            press.pressed = false;
        }
    }

    pub fn pressed(&self, keycode: VirtualKeyCode, modifier: ModifiersState) -> bool {
        let key = self.pressed[keycode as usize];

        key.pressed & !key.down & self.modifier.contains(modifier)
    }

    pub fn down(&self, keycode: VirtualKeyCode, modifier: ModifiersState) -> bool {
        self.pressed[keycode as usize].down & self.modifier.contains(modifier)
    }
}
