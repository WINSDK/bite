fn main() {
    #[cfg(target_family = "windows")]
    winres::WindowsResource::new()
        .set_icon("assets/iconx256.ico")
        .compile()
        .unwrap();
}
