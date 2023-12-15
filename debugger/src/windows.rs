use std::marker::PhantomData;
use std::sync::Arc;
use crate::{Tracing, Debuggable, VirtAddr, Context};

pub type Pid = isize;

#[derive(Debug, PartialEq)]
pub enum Error {
    ProcessLost(Pid),
}

pub struct DebuggerDescriptor {}

pub struct Debugger {
    /// Prevent [`Debugger`] implementing Send.
    _not_send: PhantomData<*mut ()>,
}

impl Debuggable for Debugger {
    fn spawn<P: AsRef<std::path::Path>, A: Into<Vec<u8>>>(
        _: P,
        _: Vec<A>,
        _: DebuggerDescriptor
    ) -> Result<Self, Error> {
        todo!("spawn");
    }

    fn attach(_: Pid, _: DebuggerDescriptor) -> Result<Self, Error> {
        todo!("attach");
    }

    fn run(self, _: Arc<Context>) -> Result<(), Error> {
        todo!("run");
    }
}

impl Tracing for Debugger {
    fn attach(&mut self) {
        todo!("detach");
    }

    fn detach(&mut self) {
        todo!("detach");
    }

    fn kill(&mut self) {
        todo!("kill");
    }

    fn pause(&self) {
        todo!("pause");
    }

    fn kontinue(&mut self) {
        todo!("kontinue");
    }

    fn read_memory(&self, _: VirtAddr, _: usize) -> Result<Vec<u8>, Error> {
        todo!("process_memory");
    }

    fn write_memory(&mut self, _: VirtAddr, _: &[u8]) -> Result<(), Error> {
        todo!("process_memory");
    }
}
