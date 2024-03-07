#![cfg(test)]

use std::path::PathBuf;
use crate::{Debugger, DebuggerDescriptor};

/// Helper macro for building test cases from the corpus.
#[macro_export]
macro_rules! build {
    ($name:literal) => {{
        let command = std::process::Command::new("cargo")
            .args(["build", "--bin", $name])
            .output()
            .expect("Failed to build test target command.");

        if !command.status.success() {
            panic!("{}", String::from_utf8_lossy(&command.stderr));
        }

        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("..");
        path.push("target");
        path.push("debug");
        path.push($name);
        path
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
        path: build!("forkbomb"),
        ..Default::default()
    };

    let mut debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.run().unwrap(), 0);
}

#[test]
fn spawn_a_lot_of_threads() {
    let desc = DebuggerDescriptor {
        path: build!("threading"),
        ..Default::default()
    };

    let mut debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.run().unwrap(), 0);
}

#[test]
fn subprocess() {
    let desc = DebuggerDescriptor {
        path: build!("subprocess"),
        ..Default::default()
    };

    let mut debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.run().unwrap(), 134);
}
