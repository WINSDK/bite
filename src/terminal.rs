use std::path::PathBuf;

use egui::text::LayoutJob;
use egui::FontId;
use once_cell::sync::Lazy;

const HISTORY_PATH: Lazy<PathBuf> = Lazy::new(|| {
    let mut path = match dirs::data_dir() {
        Some(dir) => dir,
        None => crate::error!("You must have a home directory set."),
    };

    path.push("bite");

    if !path.is_dir() {
        if let Err(err) = std::fs::create_dir(&path) {
            crate::error!("{err}");
        }
    }

    path.push("bite_history");

    if !path.is_file() {
        if let Err(err) = std::fs::File::create(&path) {
            crate::error!("{err}");
        }
    }

    path
});

pub struct Terminal {
    commands: Vec<String>,
    commands_unprocessed: usize,
    command_position: usize,
    cursor_position: usize,
}

impl Terminal {
    pub fn new() -> Self {
        let commands = match Self::read_command_history() {
            Ok(mut cmds) => {
                cmds.push(String::new());
                cmds
            }
            Err(err) => {
                crate::warning!("Failed in reading command history: '{err}'.");
                vec![String::new()]
            }
        };

        let command_position = commands.len() - 1;

        Self {
            commands,
            command_position,
            commands_unprocessed: 0,
            cursor_position: 0,
        }
    }

    pub fn current_line(&self) -> &str {
        &self.commands[self.command_position]
    }

    pub fn reset_line(&mut self) {
        self.cursor_position = 0;
        self.commands[self.command_position].clear();
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

    pub fn move_left(&mut self) {
        if self.cursor_position != 0 {
            self.cursor_position -= 1;
        }
    }

    pub fn move_right(&mut self) {
        if self.cursor_position < self.current_line().len() {
            self.cursor_position += 1;
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
        self.commands[self.command_position].insert_str(self.cursor_position, characters);
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

    pub fn format(&self, buffer: &mut LayoutJob, font_id: FontId) {
        let input = self.current_line();

        let (left, right) = input.split_at(self.cursor_position);
        let (select, right) = if right.is_empty() {
            (" ", "")
        } else {
            right.split_at(1)
        };

        buffer.append(
            left,
            0.0,
            egui::TextFormat {
                font_id: font_id.clone(),
                color: crate::gui::STYLE.egui().noninteractive().fg_stroke.color,
                ..Default::default()
            },
        );

        buffer.append(
            select,
            0.0,
            egui::TextFormat {
                font_id: font_id.clone(),
                color: crate::gui::STYLE.egui().noninteractive().bg_fill,
                background: crate::gui::STYLE.egui().noninteractive().fg_stroke.color,
                ..Default::default()
            },
        );

        buffer.append(
            right,
            0.0,
            egui::TextFormat {
                font_id: font_id.clone(),
                color: crate::gui::STYLE.egui().noninteractive().fg_stroke.color,
                ..Default::default()
            },
        );
    }

    /// Terminal commands recorded since last frame.
    pub fn commands(&mut self) -> &[String] {
        let ncmds = self.commands_unprocessed;
        &self.commands[self.commands.len() - ncmds - 1..][..ncmds]
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
}
