#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    InvalidInputSize(usize)
}


pub struct Instruction {
    mnemomic: &'static str
}

pub fn asm(raw_bytes: &[u8; 4]) -> Result<Instruction, Error> {
    if raw_bytes.len() != 4 {
        return Err(Error::InvalidInputSize(raw_bytes.len()));
    }

    todo!()
}

