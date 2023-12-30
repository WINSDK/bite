use crate::common::*;
use crate::widgets::TextSelection;

use once_cell::sync::Lazy;
use std::path::PathBuf;

const HISTORY_PATH: Lazy<PathBuf> = Lazy::new(|| {
    let mut path = match dirs::data_dir() {
        Some(dir) => dir,
        None => log::error!("You must have a home directory set."),
    };

    path.push("bite");

    if !path.is_dir() {
        if let Err(err) = std::fs::create_dir(&path) {
            log::error!("{err}");
        }
    }

    path.push("bite_history");

    if !path.is_file() {
        if let Err(err) = std::fs::File::create(&path) {
            log::error!("{err}");
        }
    }

    path
});

pub struct Terminal {
    prompt: String,
    commands: Vec<String>,
    commands_unprocessed: usize,
    command_position: usize,
    cursor_position: usize, // byte offset
    reset_cursor: bool,
}

impl Terminal {
    pub fn new() -> Self {
        let commands = match Self::read_command_history() {
            Ok(mut cmds) => {
                cmds.push(String::new());
                cmds
            }
            Err(err) => {
                log::warning!("Failed in reading command history: '{err}'.");
                vec![String::new()]
            }
        };

        let command_position = commands.len() - 1;

        Self {
            prompt: String::new(),
            commands,
            command_position,
            commands_unprocessed: 0,
            cursor_position: 0,
            reset_cursor: true,
        }
    }

    pub fn current_line(&self) -> &str {
        &self.commands[self.command_position]
    }

    pub fn reset_line(&mut self) {
        self.cursor_position = 0;
        self.commands[self.command_position].clear();
    }

    pub fn clear(&mut self) {
        self.reset_line();
        self.prompt.clear();
    }

    /// Search through newer commands, finding one that isn't empty.
    pub fn scroll_to_next_cmd(&mut self) {
        while self.command_position != self.commands.len() - 1 {
            self.command_position += 1;
            self.cursor_position = self.current_line().len();

            if !self.cursor_position != 0 {
                break;
            }
        }
    }

    /// Search through older commands, finding one that isn't empty.
    pub fn scroll_to_prev_cmd(&mut self) {
        while self.command_position != 0 {
            self.command_position -= 1;
            self.cursor_position = self.current_line().len();

            if !self.cursor_position != 0 {
                break;
            }
        }
    }

    /// Byte position of a UTF-8 codepoint.
    fn byte_position(&self, char_position: usize) -> usize {
        self.current_line()
            .char_indices()
            .nth(char_position)
            .map_or(self.current_line().len(), |(idx, _)| idx)
    }

    /// Character position from byte position.
    fn char_position(&self) -> usize {
        self.current_line()
            .char_indices()
            .take_while(|&(idx, _)| idx < self.cursor_position)
            .count()
    }

    pub fn move_left(&mut self) {
        if self.cursor_position != 0 {
            self.cursor_position = self.byte_position(self.char_position() - 1);
        }
    }

    pub fn move_right(&mut self) {
        if self.cursor_position < self.current_line().len() {
            self.cursor_position = self.byte_position(self.char_position() + 1);
        }
    }

    pub fn move_to_start(&mut self) {
        self.cursor_position = 0;
    }

    pub fn move_to_end(&mut self) {
        self.cursor_position = self.current_line().len();
    }

    pub fn backspace(&mut self) {
        if self.cursor_position == 0 {
            return;
        }

        self.move_left();
        self.commands[self.command_position].remove(self.cursor_position);
    }

    pub fn append(&mut self, characters: &str) {
        let characters = characters.escape_debug().to_string();
        self.commands[self.command_position].insert_str(self.cursor_position, &characters);
        self.cursor_position += characters.len();
    }

    /// Commence a command to be run.
    pub fn commit(&mut self) {
        // if we're using a command previously used, replace the top command
        // with the currently selected one
        if self.command_position != self.commands.len() - 1 {
            let top = self.commands.len() - 1;
            self.commands[top] = self.current_line().to_string();
        }

        self.commands.push(String::new());
        self.commands_unprocessed += 1;
        self.cursor_position = 0;
        self.command_position = self.commands.len() - 1;
    }

    /// Consumes terminal commands recorded since last frame.
    pub fn take_commands(&mut self) -> &[String] {
        let ncmds = self.commands_unprocessed;
        self.commands_unprocessed = 0;
        &self.commands[self.commands.len() - ncmds - 1..][..ncmds]
    }

    fn read_command_history() -> std::io::Result<Vec<String>> {
        let data = std::fs::read_to_string(&*HISTORY_PATH)?;

        Ok(data.lines().map(ToString::to_string).collect())
    }

    /// Appends newly recorded command's to `DATA_DIR/bite_history`.
    pub fn save_command_history(&mut self) -> std::io::Result<()> {
        let cmds: Vec<&str> = self
            .commands
            .iter()
            .filter(|cmd| !cmd.is_empty())
            .map(|cmd| cmd as &str)
            .collect();

        let mut cmds = cmds[cmds.len().saturating_sub(300)..].join("\n");

        if cmds.len() > 0 {
            cmds += "\n";
        }

        std::fs::write(&*HISTORY_PATH, cmds)
    }

    /// Process all character having been entered.
    /// Returns how many events were processed.
    pub fn record_input(&mut self, events: &mut Vec<egui::Event>) -> usize {
        let mut events_processed = 0;
        let mut prev_consumed = false;

        events.retain(|event| {
            match event {
                egui::Event::Text(received) => {
                    if !prev_consumed {
                        self.append(received);
                    }
                }
                egui::Event::Key {
                    key: egui::Key::Backspace,
                    pressed: true,
                    modifiers: egui::Modifiers::NONE,
                    ..
                } => self.backspace(),
                egui::Event::Key {
                    key: egui::Key::Enter,
                    pressed: true,
                    modifiers: egui::Modifiers::NONE,
                    ..
                } => self.commit(),
                egui::Event::Key {
                    key: egui::Key::Escape,
                    pressed: true,
                    modifiers: egui::Modifiers::NONE,
                    ..
                } => self.reset_line(),
                egui::Event::Key {
                    key: egui::Key::C,
                    pressed: true,
                    modifiers: egui::Modifiers { ctrl: true, .. },
                    ..
                } => self.reset_line(),
                egui::Event::Key {
                    key: egui::Key::A,
                    pressed: true,
                    modifiers: egui::Modifiers { ctrl: true, .. },
                    ..
                } => self.move_to_start(),
                egui::Event::Key {
                    key: egui::Key::E,
                    pressed: true,
                    modifiers: egui::Modifiers { ctrl: true, .. },
                    ..
                } => self.move_to_end(),
                egui::Event::Key {
                    key: egui::Key::L,
                    pressed: true,
                    modifiers: egui::Modifiers { ctrl: true, .. },
                    ..
                } => self.clear(),
                egui::Event::Key {
                    key: egui::Key::ArrowDown,
                    pressed: true,
                    ..
                } => self.scroll_to_next_cmd(),
                egui::Event::Key {
                    key: egui::Key::ArrowUp,
                    pressed: true,
                    ..
                } => self.scroll_to_prev_cmd(),
                egui::Event::Key {
                    key: egui::Key::ArrowLeft,
                    pressed: true,
                    ..
                } => self.move_left(),
                egui::Event::Key {
                    key: egui::Key::ArrowRight,
                    pressed: true,
                    ..
                } => self.move_right(),
                _ => {
                    prev_consumed = false;
                    return true;
                }
            }

            events_processed += 1;
            prev_consumed = true;
            false
        });

        if events_processed > 0 {
            self.reset_cursor = true;
        }

        events_processed
    }
}

impl std::fmt::Write for Terminal {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.prompt.push_str(s);
        Ok(())
    }
}

impl Display for Terminal {
    fn show(&mut self, ui: &mut egui::Ui) {
        let area = egui::ScrollArea::vertical()
            .auto_shrink([false, false])
            .drag_to_scroll(false)
            .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysHidden);

        let format = egui::TextFormat {
            font_id: FONT,
            color: crate::style::EGUI.noninteractive().fg_stroke.color,
            ..Default::default()
        };

        let ex_bg_color = ui.style().visuals.extreme_bg_color;
        ui.style_mut().visuals.extreme_bg_color = crate::style::STYLE.primary_background;

        area.show(ui, |ui| {
            let mut text_area = TextSelection::new();
            let title = "(bite) ";

            text_area.append(&self.prompt, &format);
            text_area.append(title, &format);

            let input = self.current_line();
            let (left, right) = input.split_at(self.cursor_position);
            let (select, right) = if right.is_empty() {
                (" ", "")
            } else {
                // right's one char byte offset
                let off = right.char_indices().nth(1).map_or(right.len(), |(idx, _)| idx);
                right.split_at(off)
            };

            text_area.append(left, &format);
            text_area.append(select, &format);
            text_area.append(right, &format);

            // HACK: keep track of cumulative command position 
            let pos = self.commands[..self.command_position]
                .iter()
                .map(|l| l.bytes().count())
                .sum::<usize>()
                + self.cursor_position
                + title.len()
                - 1;

            text_area.set_cursor_position(pos);
            text_area.show(ui, self.reset_cursor);
            self.reset_cursor = false;
        });

        ui.style_mut().visuals.extreme_bg_color = ex_bg_color;
    }
}
