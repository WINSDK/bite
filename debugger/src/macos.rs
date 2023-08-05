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

    fn attach(pid: Pid) -> Result<Self, Error> {
        todo!("attach");
    }
}

impl Tracee for Debugger {
    fn detach(self) {
        todo!("detach");
    }

    fn kill(&self) {
        todo!("kill");
    }

    fn pause(&self, pid: Pid) {
        todo!("pause");
    }

    fn kontinue(&mut self, pid: Pid) {
        todo!("kontinue");
    }

    fn read_process_memory(&self, base_addr: usize, len: usize) -> Result<Vec<u8>, Error> {
        todo!("read_process_memory");
    }

    fn write_process_memory(&mut self, base_addr: usize, data: &[u8]) -> Result<(), Error> {
        todo!("write_process_memory");
    }
}

impl Drop for Debugger {
    fn drop(&mut self) {
        self.kill();
    }
}
