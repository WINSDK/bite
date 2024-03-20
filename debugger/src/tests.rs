#![cfg(test)]

use std::path::PathBuf;
use crate::{Debugger, DebuggerDescriptor};
use nix::sys::signal::{self, Signal};
use processor::Processor;
use procfs::process::MMapPath;

/// Helper macro for building test cases from the corpus.
#[macro_export]
macro_rules! test_case {
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

    let debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.wait_for_exit().unwrap(), 0);
}

#[test]
fn sleep_1sec() {
    let desc = DebuggerDescriptor {
        path: PathBuf::from("/bin/sleep"),
        args: vec!["1".to_string()],
        ..Default::default()
    };

    let debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.wait_for_exit().unwrap(), 0);
}

#[test]
fn sleep_invalid() {
    let desc = DebuggerDescriptor {
        path: PathBuf::from("/bin/sleep"),
        ..Default::default()
    };

    let debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.wait_for_exit().unwrap(), 1);
}

#[test]
fn spawn_some_threads() {
    let desc = DebuggerDescriptor {
        path: test_case!("threading"),
        ..Default::default()
    };

    let debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.wait_for_exit().unwrap(), 0);
}

#[test]
fn spawn_a_lot_of_threads() {
    let desc = DebuggerDescriptor {
        path: test_case!("forkbomb"),
        ..Default::default()
    };

    let debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.wait_for_exit().unwrap(), 0);
}

#[test]
fn subprocess() {
    let desc = DebuggerDescriptor {
        path: test_case!("subprocess"),
        ..Default::default()
    };

    let debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.wait_for_exit().unwrap(), 134);
}

#[test]
fn interrupt() {
    let desc = DebuggerDescriptor {
        path: test_case!("interrupt"),
        ..Default::default()
    };

    let mut debugger = Debugger::spawn(desc).unwrap();

    debugger.wait_for_stop().kontinue();

    // Pause process for 1sec.
    let stopped = debugger.interrupt();
    std::thread::sleep(std::time::Duration::from_millis(300));
    stopped.kontinue();

    assert_eq!(debugger.wait_for_exit().unwrap(), 0, "Tracee wasn't stopped");
}

#[test]
fn exec_from_thread() {
    let desc = DebuggerDescriptor {
        path: test_case!("exec"),
        ..Default::default()
    };

    let debugger = Debugger::spawn(desc).unwrap();
    assert_eq!(debugger.wait_for_exit().unwrap(), 0);
}

#[test]
fn sigkill() {
    let desc = DebuggerDescriptor {
        path: test_case!("loop"),
        ..Default::default()
    };

    let debugger = Debugger::spawn(desc).unwrap();
    let pid = debugger.lock().tracees.root().pid;
    signal::kill(pid, Signal::SIGKILL).unwrap();
    assert_eq!(debugger.wait_for_exit().unwrap(), 128 + Signal::SIGKILL as i32);
}

#[test]
fn breaking() {
    let path = test_case!("breaking");
    let processor = Processor::parse(&path).unwrap();
    let desc = DebuggerDescriptor {
        path: path.clone(),
        ..Default::default()
    };

    let debugger = Debugger::spawn(desc).unwrap();
    let mut stopped = debugger.wait_for_stop();

    let func_addr = processor.index.get_by_name("breaking::some_function").unwrap();
    let func_segment = processor.segments().find(|s| s.contains(func_addr)).unwrap();
    let func_rva = func_addr - func_segment.start;

    let proc = stopped.tracees.root();
    let maps = proc.memory_maps().unwrap();

    // Assume's the root maps are sorted by offset.
    let segments_map = maps
        .iter()
        .filter(|m| m.pathname == MMapPath::Path(path.canonicalize().unwrap()))
        .find(|m| func_segment.start >= m.offset as usize)
        .unwrap();

    let rva = func_segment.start - segments_map.offset as usize;
    let segment_real = segments_map.address.0 as usize + rva;
    let func_real = segment_real + func_rva;
    println!("{func_real:#X}");

    stopped.set_breakpoint(func_real).unwrap();
    stopped.kontinue();

    // Catch breakpoint.
    debugger.wait_for_stop().kontinue();

    assert_eq!(debugger.wait_for_exit().unwrap(), 0);
}
