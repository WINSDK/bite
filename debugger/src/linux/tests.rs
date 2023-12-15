#![cfg(test)]

use crate::*;
use disassembler::Processor;

#[test]
fn spawn_echo() {
    let ctx = Arc::new(Context::new());
    let desc = DebuggerDescriptor {
        module: Arc::new(Processor::parse_unknown("/bin/sleep").unwrap()),
        tracing: false,
        follow_children: false,
    };
    let session = Debugger::spawn("sh", vec!["-c", "echo 10"], desc).unwrap();

    assert!(session.run(ctx.clone()).is_ok());
    assert_eq!(ctx.queue.pop(), Some(DebuggerEvent::Exited(0)));
    assert_eq!(ctx.queue.pop(), None);
}

#[test]
fn spawn_error_code() {
    let ctx = Arc::new(Context::new());
    let desc = DebuggerDescriptor {
        module: Arc::new(Processor::parse_unknown("/bin/sleep").unwrap()),
        tracing: false,
        follow_children: false,
    };
    let session = Debugger::spawn("/bin/sleep", Vec::<&str>::new(), desc).unwrap();

    assert!(session.run(ctx.clone()).is_ok());
    assert_eq!(ctx.queue.pop(), Some(DebuggerEvent::Exited(1)));
    assert_eq!(ctx.queue.pop(), None);
}
