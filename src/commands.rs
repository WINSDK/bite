use crate::gui::RenderContext;

const CMDS: &[&str] = &["open", "pwd"];

fn possible_command(unknown: &str) -> Option<&str> {
    let mut distance = u32::MAX;
    let mut best_guess = "";
    for cmd in CMDS {
        let d = triple_accel::levenshtein_exp(unknown.as_bytes(), cmd.as_bytes());
        if d < distance {
            distance = d;
            best_guess = cmd;
        }
    }

    // A guess that's less than 2 `steps` away from a correct arg.
    (distance <= 2).then_some(best_guess)
}

pub fn process_commands(ctx: &mut RenderContext, commands: &[String]) {
    for cmd in commands {
        ctx.terminal_prompt.push_str(&format!("(bite) {cmd}\n"));

        let mut delimited = cmd.split_whitespace();
        let cmd = match delimited.next() {
            Some(cmd) => cmd,
            None => continue,
        };

        let arg = delimited.next();

        if cmd == "open" {
            if let Some(path) = arg {
                ctx.start_disassembling(path.to_string());
                ctx.terminal_prompt.push_str(&format!("Binary '{path}' was opened.\n"));
                continue;
            }

            ctx.terminal_prompt.push_str(&format!("Command 'open' requires a path.\n"));
            continue
        }

        if cmd == "pwd" {
            match std::env::current_dir() {
                Ok(path) => {
                    ctx.terminal_prompt.push_str(&format!("Working dir {}.\n", path.display()))
                }
                Err(err) => ctx.terminal_prompt.push_str(&format!("{err:?}\n")),
            }
            continue;
        }

        match possible_command(cmd) {
            Some(guess) => ctx.terminal_prompt.push_str(&format!(
                "Command '{cmd}' is unknown, did you mean '{guess}'?.\n"
            )),
            None => ctx.terminal_prompt.push_str(&format!("Command '{cmd}' is unknown.\n")),
        }
    }
}
