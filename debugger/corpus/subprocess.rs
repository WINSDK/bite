fn main() {
    let output = std::process::Command::new("echo")
        .arg("10\n")
        .output()
        .unwrap();

    assert!(output.status.success());
    assert_eq!(&output.stdout, b"10\n");
}
