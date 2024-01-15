use crate::common::*;
use crate::style::EGUI;
use crate::widgets::TextSelection;

use symbols::Index;
use tokenizing::colors;

use egui::text::LayoutJob;
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

#[derive(Default)]
struct Autocomplete {
    cmd: usize,
    cmd_suggestions: Vec<String>,
    term: usize,
    term_suggestions: Vec<String>,
}

impl Autocomplete {
    fn update_term_suggestion(&mut self, command: usize, commands: &[String]) {
        let cmd = &commands[command];
        if cmd.is_empty() {
            self.term_suggestions = Vec::new();
            return;
        }

        // maybe do a fuzzy find instead here
        self.term_suggestions = commands
            .iter()
            .enumerate()
            .rev()
            .filter_map(|(idx, scmd)| {
                if idx == command {
                    return None;
                }

                scmd.strip_prefix(cmd)
            })
            .map(str::to_string)
            .collect();
    }

    fn update_cmd_suggestion(&mut self, line: &str, index: &Index, cursor: usize) {
        if let Err((_, suggestions)) = commands::Command::parse(index, &line, cursor) {
            self.cmd_suggestions = suggestions;
        }
    }

    fn cmd_suggestion(&self) -> Option<&str> {
        self.cmd_suggestions.get(self.cmd).map(|x| x.as_str())
    }

    fn term_suggestion(&self) -> Option<&str> {
        self.term_suggestions.get(self.term).map(|x| x.as_str())
    }

    fn next_cmd(&mut self, command: usize, commands: &[String]) {
        self.cmd += 1;
        if self.cmd >= self.cmd_suggestions.len() {
            self.cmd = 0;
        }

        self.update_term_suggestion(command, commands);
    }

    #[allow(dead_code)]
    fn next_term(&mut self, line: &str, index: &Index, cursor: usize) {
        self.term += 1;
        if self.term >= self.term_suggestions.len() {
            self.term = 0;
        }

        self.update_cmd_suggestion(line, index, cursor);
    }

    fn clear(&mut self) {
        *self = Autocomplete::default();
    }
}

pub struct Terminal {
    prompt: String,
    commands: Vec<String>,
    commands_unprocessed: usize,
    command_position: usize,
    cursor_position: usize, // byte offset
    reset_cursor: bool,
    autocomplete: Autocomplete,
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
            autocomplete: Autocomplete::default(),
        }
    }

    /// Call before drawing to figure out whether the terminal requires focus.
    pub fn should_reset_cursor(&self) -> bool {
        self.reset_cursor
    }

    fn current_line(&self) -> &str {
        &self.commands[self.command_position]
    }

    fn clear_line(&mut self) {
        self.cursor_position = 0;
        self.commands[self.command_position].clear();
        self.autocomplete.clear();
    }

    pub fn clear(&mut self) {
        self.prompt.clear();
        self.clear_line();
    }

    /// Search through newer commands, finding one that isn't empty.
    fn scroll_to_next_cmd(&mut self) {
        while self.command_position != self.commands.len() - 1 {
            self.command_position += 1;
            self.cursor_position = self.current_line().len();

            if !self.cursor_position != 0 {
                break;
            }
        }

        self.autocomplete.clear();
    }

    /// Search through older commands, finding one that isn't empty.
    fn scroll_to_prev_cmd(&mut self) {
        while self.command_position != 0 {
            self.command_position -= 1;
            self.cursor_position = self.current_line().len();

            if !self.cursor_position != 0 {
                break;
            }
        }

        self.autocomplete.clear();
    }

    fn update_autocomplete(&mut self, index: &Index) {
        let line = &self.commands[self.command_position];
        self.autocomplete.update_cmd_suggestion(line, index, self.cursor_position);
        self.autocomplete.update_term_suggestion(self.command_position, &self.commands);
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

    fn move_left(&mut self) {
        if self.cursor_position != 0 {
            self.cursor_position = self.byte_position(self.char_position() - 1);
        }
    }

    fn move_right(&mut self) {
        if let Some(suggestion) = self.autocomplete.term_suggestion() {
            self.commands[self.command_position].push_str(suggestion);
            self.move_to_end();
            self.autocomplete.clear();
            return;
        }

        if self.cursor_position < self.current_line().len() {
            self.cursor_position = self.byte_position(self.char_position() + 1);
        }
    }

    /// Moves the cursor to the start of the previous word.
    fn move_left_next_word(&mut self) {
        let current_line = self.current_line();
        let mut is_in_whitespace = true;
        let mut new_position = None;

        for (idx, chr) in current_line[..self.cursor_position].char_indices().rev() {
            if chr.is_whitespace() {
                if !is_in_whitespace {
                    new_position = Some(idx + chr.len_utf8());
                    break;
                }
            } else {
                is_in_whitespace = false;
            }
        }

        // move to the found position or to the start if no suitable position was found.
        self.cursor_position = new_position.unwrap_or(0);
    }

    /// Moves the cursor to the start of the next word.
    fn move_right_next_word(&mut self) {
        let current_line = self.current_line();
        let mut is_in_whitespace = true;
        let mut new_position = None;

        for (idx, chr) in current_line[self.cursor_position..].char_indices() {
            if chr.is_whitespace() {
                if !is_in_whitespace && idx > 0 {
                    new_position = Some(self.cursor_position + idx);
                    break;
                }
            } else {
                is_in_whitespace = false;
            }
        }

        // move to the found position or to the end if no suitable position was found.
        self.cursor_position = new_position.unwrap_or(current_line.len());
    }

    fn move_to_start(&mut self) {
        self.cursor_position = 0;
    }

    fn move_to_end(&mut self) {
        self.cursor_position = self.current_line().len();
    }

    fn backspace(&mut self, index: &Index) {
        if self.cursor_position == 0 {
            return;
        }

        self.move_left();
        self.commands[self.command_position].remove(self.cursor_position);
        self.update_autocomplete(index);
    }

    fn append(&mut self, characters: &str, index: &Index) {
        let characters = characters.escape_debug().to_string();
        self.commands[self.command_position].insert_str(self.cursor_position, &characters);
        self.cursor_position += characters.len();
        self.update_autocomplete(index);
    }

    /// Commence a command to be run.
    fn commit(&mut self) {
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
    fn save_command_history(&mut self) -> std::io::Result<()> {
        let cmds: Vec<&str> = self
            .commands
            .iter()
            .filter(|cmd| !cmd.is_empty())
            .map(|cmd| cmd as &str)
            .collect();

        // only save the last 300 commands
        let mut cmds = cmds[cmds.len().saturating_sub(300)..].join("\n");

        if cmds.len() > 0 {
            cmds += "\n";
        }

        std::fs::write(&*HISTORY_PATH, cmds)
    }

    /// Process all character having been entered.
    /// Returns how many events were processed.
    pub fn record_input(&mut self, events: &mut Vec<egui::Event>, index: &Index) -> usize {
        let mut events_processed = 0;
        let mut prev_consumed = false;

        events.retain(|event| {
            match event {
                egui::Event::Text(received) => {
                    if !prev_consumed {
                        self.append(received, index);
                    }
                }
                egui::Event::Key {
                    key: egui::Key::Tab,
                    pressed: true,
                    modifiers: egui::Modifiers::NONE,
                    ..
                } => {
                    // continue autocompleting if we found an exact match
                    if self.autocomplete.cmd_suggestions.len() == 1 {
                        self.update_autocomplete(index);
                    }

                    if let Some(suggestion) = self.autocomplete.cmd_suggestion() {
                        self.commands[self.command_position] = suggestion.to_string();
                        self.move_to_end();
                    }

                    self.autocomplete.next_cmd(self.command_position, &self.commands);
                }
                egui::Event::Key {
                    key: egui::Key::Backspace,
                    pressed: true,
                    modifiers: egui::Modifiers::NONE,
                    ..
                } => self.backspace(index),
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
                } => self.clear_line(),
                egui::Event::Key {
                    key: egui::Key::C,
                    pressed: true,
                    modifiers:
                        egui::Modifiers {
                            ctrl: true,
                            shift: false,
                            ..
                        },
                    ..
                } => self.clear_line(),
                egui::Event::Key {
                    key: egui::Key::A,
                    pressed: true,
                    modifiers:
                        egui::Modifiers {
                            ctrl: true,
                            shift: false,
                            ..
                        },
                    ..
                } => self.move_to_start(),
                egui::Event::Key {
                    key: egui::Key::E,
                    pressed: true,
                    modifiers:
                        egui::Modifiers {
                            ctrl: true,
                            shift: false,
                            ..
                        },
                    ..
                } => self.move_to_end(),
                egui::Event::Key {
                    key: egui::Key::L,
                    pressed: true,
                    modifiers:
                        egui::Modifiers {
                            ctrl: true,
                            shift: false,
                            ..
                        },
                    ..
                } => self.clear(),
                egui::Event::Key {
                    key: egui::Key::ArrowLeft,
                    pressed: true,
                    modifiers:
                        egui::Modifiers {
                            ctrl: true,
                            shift: false,
                            ..
                        },
                    ..
                } => self.move_left_next_word(),
                egui::Event::Key {
                    key: egui::Key::ArrowRight,
                    pressed: true,
                    modifiers:
                        egui::Modifiers {
                            ctrl: true,
                            shift: false,
                            ..
                        },
                    ..
                } => self.move_right_next_word(),
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
                // copy and cut are events that will reset the cursor
                egui::Event::Copy | egui::Event::Cut => {
                    events_processed += 1;
                    prev_consumed = false;
                    return true;
                }
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
            // store new commands recorded
            let _ = self.save_command_history();
        }

        events_processed
    }

    pub fn show(&mut self, ui: &mut egui::Ui) -> egui::Response {
        let area = egui::ScrollArea::vertical()
            .auto_shrink([false, false])
            .drag_to_scroll(false)
            .stick_to_bottom(true)
            .scroll_bar_visibility(egui::scroll_area::ScrollBarVisibility::AlwaysHidden);

        let response = area.show(ui, |ui| {
            let title = "(bite) ";
            let input = self.current_line();
            let color = EGUI.noninteractive().fg_stroke.color;

            let mut output = LayoutJob::default();
            let mut append = |s: &str, color: egui::Color32| {
                output.append(
                    &s,
                    0.0,
                    egui::TextFormat {
                        font_id: FONT,
                        color,
                        ..Default::default()
                    },
                );
            };

            append(&self.prompt, color);
            append(title, color);
            append(input, color);

            if let Some(suggestion) = self.autocomplete.term_suggestion() {
                append(suggestion, colors::GRAY60);
            }

            let mut text_area = TextSelection::precomputed(&output);

            if self.reset_cursor {
                let abs_position = self.prompt.len() + title.len() + self.cursor_position;
                text_area.set_reset_position(abs_position);
                self.reset_cursor = false;
            }

            ui.add_sized(ui.available_size(), text_area)
        });

        response.inner
    }
}

impl std::fmt::Write for Terminal {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        // TODO: sanitize input here for stuff like symbols being printed
        self.reset_cursor = true;
        self.prompt.push_str(s);
        Ok(())
    }
}
