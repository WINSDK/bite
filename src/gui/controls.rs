use std::collections::HashMap;
use winit::event::{ModifiersState, VirtualKeyCode};

#[allow(dead_code)]
#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Actions {
    Left,
    Right,
    Forward,
    Backward,
    Maximize,
    CloseRequest,
}

type Keybind = (VirtualKeyCode, ModifiersState);

#[derive(Debug)]
pub struct Keybinds {
    pub keymap: HashMap<Actions, Vec<Keybind>>,
}

impl Default for Keybinds {
    fn default() -> Self {
        let mut keymap = Keybinds {
            keymap: HashMap::new(),
        };

        keymap.insert(Actions::Maximize, (VirtualKeyCode::F, ModifiersState::CTRL));
        keymap.insert(
            Actions::CloseRequest,
            (VirtualKeyCode::Q, ModifiersState::CTRL),
        );

        keymap
    }
}

impl Keybinds {
    /// Iterates through the bound keys and returns whether an action has been executed.
    pub fn matching_action(&self, action: Actions, input: Keybind) -> bool {
        self.keymap.get(&action);

        if let Some(keybinds) = self.keymap.get(&action) {
            for keybind in keybinds {
                if keybind == &input {
                    return true;
                }
            }
        }

        false
    }

    pub fn insert(&mut self, action: Actions, keybind: Keybind) {
        if let Some(keybinds) = self.keymap.get_mut(&action) {
            keybinds.push(keybind);
        } else {
            self.keymap.insert(action, vec![keybind]);
        }
    }
}
