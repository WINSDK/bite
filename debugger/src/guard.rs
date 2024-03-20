use crate::{PTraceCommand, DebuggerImpl, Error};
use std::sync::MutexGuard;
use std::thread::Thread;

pub struct StoppedDebugger<'l> {
    /// Thread handle, it's an option in case kontinue() get's called.
    /// This is so that [`Self::drop`] doesn't double unpark the thread.
    thread: Option<Thread>,
    parked: MutexGuard<'l, bool>,
    inner: MutexGuard<'l, DebuggerImpl>,
}

impl<'l> std::ops::Deref for StoppedDebugger<'l> {
    type Target = MutexGuard<'l, DebuggerImpl>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::ops::DerefMut for StoppedDebugger<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'l> StoppedDebugger<'l> {
    pub(crate) fn new(
        thread: &Thread,
        parked: MutexGuard<'l, bool>,
        guard: MutexGuard<'l, DebuggerImpl>,
    ) -> Self {
        Self {
            parked,
            thread: Some(thread.clone()),
            inner: guard,
        }
    }

    /// Unpark debugger thread running event loop.
    pub fn kontinue(mut self) {
        self.thread.take().unwrap().unpark();
        *self.parked = false;
    }

    pub fn set_breakpoint(&mut self, addr: usize) -> Result<(), Error> {
        self.pending_cmds.push_front(PTraceCommand::SetbreakPoint { addr });
        Ok(())
    }

    pub fn unset_breakpoint(&mut self, addr: usize) -> Result<(), Error> {
        self.pending_cmds.push_front(PTraceCommand::UnsetbreakPoint { addr });
        Ok(())
    }
}

impl Drop for StoppedDebugger<'_> {
    fn drop(&mut self) {
        if let Some(thread) = self.thread.take() {
            thread.unpark();
            *self.parked = false;
        }
    }
}
