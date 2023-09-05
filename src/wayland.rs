#![cfg(target_os = "linux")]

fn get_first_direntry<P: AsRef<std::path::Path>>(path: P) -> Option<std::ffi::OsString> {
    if let Ok(mut dir) = std::fs::read_dir(path) {
        if let Some(Ok(entry)) = dir.next() {
            return Some(entry.file_name());
        }
    }

    None
}

pub fn set_env() {
    // if the necessary wayland env variables aren't set, try to guess them
    if std::env::var("XDG_RUNTIME_DIR").is_err() || std::env::var("WAYLAND_DISPLAY").is_err() {
        let user = match get_first_direntry("/run/user") {
            Some(user) => user,
            None => {
                crate::warning!("Failed to guess wayland environmental variables.");
                return;
            }
        };

        let mut path = std::path::PathBuf::from("/run/user/");
        path.push(user);
        path.push("wayland-1");

        std::env::set_var("WAYLAND_DISPLAY", path);
        std::env::set_var("XDG_RUNTIME_DIR", "/");
    }
}
