#![cfg(test)]

use crate::*;
use processor::Processor;

#[test]
fn spawn_sh() {
    let settings = DebuggerSettings {
        tracing: false,
        follow_children: false,
        env: Vec::<&str>::new(),
    };
    let desc = DebuggerDescriptor {
        args: vec!["-c", "echo 10"],
        module: Arc::new(Processor::parse("/bin/sh").unwrap()),
    };

    let mut debugger = Debugger::new(settings, desc);
    debugger.spawn().unwrap();
    assert_eq!(debugger.trace().unwrap(), 0);
}

#[test]
fn spawn_sleep_invalid() {
    let settings = DebuggerSettings {
        tracing: false,
        follow_children: false,
        env: Vec::<String>::new(),
    };
    let desc = DebuggerDescriptor {
        module: Arc::new(Processor::parse("/bin/sleep").unwrap()),
        args: vec![],
    };

    let mut debugger = Debugger::new(settings, desc);
    debugger.spawn().unwrap();
    assert_eq!(debugger.trace().unwrap(), 1);
}
