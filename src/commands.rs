use std::path::Path;

use crate::gui::RenderContext;

const CMDS: &[&str] = &["open", "pwd", "cd"];

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

fn print_cwd(ctx: &mut RenderContext) {
    match std::env::current_dir() {
        Ok(path) => ctx
            .terminal_prompt
            .push_str(&format!("Working directory {}.\n", path.display())),
        Err(err) => ctx.terminal_prompt.push_str(&format!("Failed to print pwd: '{err}'\n")),
    }
}

fn expand_homedir<P: AsRef<Path>>(path: P) -> std::path::PathBuf {
    let path = path.as_ref();

    if !path.starts_with("~") {
        return path.to_path_buf();
    }

    let mut home_dir = match dirs::home_dir() {
        Some(dir) => dir,
        None => return path.to_path_buf(),
    };

    if path == Path::new("~") {
        return home_dir;
    }

    if home_dir == Path::new("/") {
        // Corner case: `home_dir` root directory;
        // don't prepend extra `/`, just drop the tilde.
        path.strip_prefix("~").unwrap().to_path_buf()
    } else {
        home_dir.push(path.strip_prefix("~/").unwrap());
        home_dir
    }
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
            if let Some(unexpanded) = arg {
                let path = expand_homedir(unexpanded);

                ctx.start_disassembling(path);
                ctx.terminal_prompt.push_str(&format!("Binary '{unexpanded}' was opened.\n"));
                continue;
            }

            ctx.terminal_prompt.push_str(&format!("Command 'open' requires a path.\n"));
            continue;
        }

        if cmd == "cd" {
            if let Some(path) = arg {
                let path = expand_homedir(path);

                if let Err(err) = std::env::set_current_dir(path) {
                    ctx.terminal_prompt
                        .push_str(&format!("Failed to change directory: '{err}'.\n"));
                    continue;
                }
                print_cwd(ctx);
                continue;
            }

            ctx.terminal_prompt.push_str(&format!("Command 'cd' requires a path.\n"));
            continue;
        }

        if cmd == "pwd" {
            print_cwd(ctx);
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
