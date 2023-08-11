use crate::{Process, Tracee};

pub struct Pid;

#[derive(Debug)]
pub enum Error {}

#[derive(Debug)]
pub struct Debugger {}

impl Process for Debugger {
    fn spawn<P: AsRef<std::path::Path>>(_: P, _: &[&str]) -> Result<Self, Error> {
        todo!("spawn");
    }

    fn attach(_: Pid) -> Result<Self, Error> {
        todo!("attach");
    }
}

impl Tracee for Debugger {
    fn detach(&mut self) {
        todo!("detach");
    }

    fn kill(&mut self) {
        todo!("kill");
    }

    fn pause(&self, _: Pid) {
        todo!("pause");
    }

    fn kontinue(&mut self, _: Pid) {
        todo!("kontinue");
    }

    fn read_process_memory(&self, _: usize, _: usize) -> Result<Vec<u8>, Error> {
        todo!("read_process_memory");
    }

    fn write_process_memory(&mut self, _: usize, _: &[u8]) -> Result<(), Error> {
        todo!("write_process_memory");
    }
}

impl Drop for Debugger {
    fn drop(&mut self) {
        self.kill();
    }
}
