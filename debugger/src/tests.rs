#![cfg(test)]

use std::path::PathBuf;
use crate::{Debugger, DebuggerDescriptor};

/// Helper macro for building test cases from the corpus.
#[macro_export]
macro_rules! build {
    ($path:literal) => {{
        let mut in_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        in_path.push($path);

        let mut out_path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        out_path.push("..");
        out_path.push("target");
        out_path.push(format!(
            "test_debugger_{}",
            in_path.file_stem().unwrap().to_str().unwrap()
        ));

        if cfg!(target_family = "windows") {
            out_path.set_extension("exe");
        }

        let rustc = std::process::Command::new("rustc")
            .arg("-Cdebuginfo=2")
            .arg(format!("-o{}", out_path.display()))
            .arg(in_path)
            .output()
            .unwrap();

        if !rustc.stderr.is_empty() {
            eprintln!("{}", String::from_utf8_lossy(&rustc.stderr[..]));
        }

        if !rustc.status.success() {
            panic!("rustc failed with exit code: {}", rustc.status);
        }

        out_path
    }};
}

/// will test whether all the piping of errors within [`Debugger::spawn`] works.
#[test]
#[should_panic]
fn invalid_file() {
    let desc = DebuggerDescriptor {
        path: PathBuf::from("/somerandomfilepaththatdoesntexist"),
        ..Default::default()
    };
    Debugger::spawn(desc).unwrap();
}

/// will test whether all the piping of errors within [`Debugger::spawn`] works.
#[test]
fn valid_file() {
    let desc = DebuggerDescriptor {
        path: PathBuf::from("/bin/echo"),
        ..Default::default()
    };
    let mut debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.run().unwrap(), 0);
}

#[test]
fn sleep_1sec() {
    let desc = DebuggerDescriptor {
        path: PathBuf::from("/bin/sleep"),
        args: vec!["1".to_string()],
        ..Default::default()
    };

    let mut debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.run().unwrap(), 0);
}

#[test]
fn sleep_invalid() {
    let desc = DebuggerDescriptor {
        path: PathBuf::from("/bin/sleep"),
        ..Default::default()
    };

    let mut debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.run().unwrap(), 1);
}

#[test]
fn spawn_some_threads() {
    let desc = DebuggerDescriptor {
        path: build!("./corpus/threading.rs"),
        ..Default::default()
    };

    let mut debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.run().unwrap(), 0);
}

#[test]
fn spawn_a_lot_of_threads() {
    let desc = DebuggerDescriptor {
        path: build!("./corpus/forkbomb.rs"),
        ..Default::default()
    };

    let mut debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.run().unwrap(), 0);
}
