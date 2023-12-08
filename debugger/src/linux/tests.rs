#![cfg(test)]

use crate::*;

#[test]
fn spawn_echo() {
    let ctx = Arc::new(Context::new());
    let session = Debugger::spawn("sh", vec!["-c", "echo 10"]).unwrap();

    assert!(session.run(ctx.clone()).is_ok());
    assert_eq!(ctx.queue.pop(), Some(DebuggerEvent::Exited(0)));
    assert_eq!(ctx.queue.pop(), None);
}

#[test]
fn spawn_error_code() {
    let ctx = Arc::new(Context::new());
    let session = Debugger::spawn("sleep", Vec::<&str>::new()).unwrap();

    assert!(session.run(ctx.clone()).is_ok());
    assert_eq!(ctx.queue.pop(), Some(DebuggerEvent::Exited(1)));
    assert_eq!(ctx.queue.pop(), None);
}
