#![cfg_attr(
    all(not(debug_assertions), target_family = "windows"),
    windows_subsystem = "windows"
)]

#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Bite can only be build for windows, macos and linux.");

mod wayland;
use commands::ARGS;

fn main() {
    #[cfg(target_os = "linux")]
    if nix::unistd::getuid() == 0.into() {
        wayland::set_env();
    }

    let mut ui = gui::UI::new().unwrap();
    ui.offload_binary_processing(ARGS.path.clone());
    ui.run();
}
