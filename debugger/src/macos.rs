use crate::{
    Context, Debuggable, DebuggerDescriptor, DebuggerSettings, PhysAddr, Tracing, VirtAddr,
};
use std::marker::PhantomData;
use std::sync::Arc;

pub type Pid = isize;

#[derive(Debug, PartialEq)]
pub enum Error {
    ProcessLost(Pid),
}

pub struct Debugger {
    /// Prevent [`Debugger`] implementing Send.
    _not_send: PhantomData<*mut ()>,
}

impl Debuggable for Debugger {
    fn spawn<S: Into<Vec<u8>>>(
        _: DebuggerSettings<S>,
        _: DebuggerDescriptor<S>,
    ) -> Result<Self, Error> {
        todo!("spawn");
    }

    fn attach<S: Into<Vec<u8>>>(
        _: Pid,
        _: DebuggerSettings<S>,
        _: DebuggerDescriptor<S>,
    ) -> Result<Self, Error> {
        todo!("attach");
    }

    fn run(self, _: Arc<Context>) -> Result<(), Error> {
        todo!("run");
    }
}

pub struct Process {}

impl Tracing for Process {
    fn attach(&mut self) -> Result<(), Error> {
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
        todo!("write_memory");
    }

    fn write_protected_memory(&mut self, _: VirtAddr, _: &[u8]) -> Result<(), Error> {
        todo!("write_protected_memory");
    }

    fn virt_to_phys(&self, addr: VirtAddr) -> PhysAddr {
        addr
    }

    fn phys_to_virt(&self, addr: PhysAddr) -> VirtAddr {
        addr
    }
}
