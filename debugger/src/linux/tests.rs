#![cfg(test)]

use crate::*;
use disassembler::Processor;

#[test]
fn spawn_sh() {
    let ctx = Arc::new(Context::new());
    let settings = DebuggerSettings {
        tracing: false,
        follow_children: false,
        env: Vec::<&str>::new(),
    };
    let desc = DebuggerDescriptor {
        args: vec!["-c", "echo 10"],
        module: Arc::new(Processor::parse_unknown("/bin/sh").unwrap()),
    };
    let session = Debugger::spawn(settings, desc).unwrap();

    assert!(session.run(ctx.clone()).is_ok());
    assert_eq!(ctx.queue.pop(), Some(DebuggerEvent::Exited(0)));
    assert_eq!(ctx.queue.pop(), None);
}

#[test]
fn spawn_sleep_invalid() {
    let ctx = Arc::new(Context::new());
    let settings = DebuggerSettings {
        tracing: false,
        follow_children: false,
        env: Vec::<String>::new(),
    };
    let desc = DebuggerDescriptor {
        module: Arc::new(Processor::parse_unknown("/bin/sleep").unwrap()),
        args: vec![],
    };
    let session = Debugger::spawn(settings, desc).unwrap();

    assert!(session.run(ctx.clone()).is_ok());
    assert_eq!(ctx.queue.pop(), Some(DebuggerEvent::Exited(1)));
    assert_eq!(ctx.queue.pop(), None);
}
