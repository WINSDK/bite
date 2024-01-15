use super::Error;
use std::fmt;

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Object(err) => {
                f.write_fmt(format_args!("Failed to parse object (symbols): '{err}'."))
            }
            Self::Dwarf(err) => f.write_fmt(format_args!("Failed to parse dwarf info: '{err}'.")),
            Self::Pdb(err) => f.write_fmt(format_args!("Failed to parse pdb info: '{err}'.")),
            Self::Imports(err) => f.write_fmt(format_args!("Failed to parse imports: '{err}'.")),
        }
    }
}

impl From<object::Error> for Error {
    fn from(error: object::Error) -> Self {
        Error::Object(error)
    }
}

impl From<gimli::Error> for Error {
    fn from(error: gimli::Error) -> Self {
        Error::Dwarf(error)
    }
}

impl From<pdb::Error> for Error {
    fn from(error: pdb::Error) -> Self {
        Error::Pdb(error)
    }
}
