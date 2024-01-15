use std::fmt::{self, Display};

impl fmt::Debug for super::Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IO(err) => f.write_fmt(format_args!("{err}.")),
            Self::Object(err) => {
                f.write_fmt(format_args!("Failed to parse object (processor): '{err}'."))
            }
            Self::Symbol(err) => err.fmt(f),
            Self::NotAnExecutable => f.write_str("A given object is not an executable."),
            Self::DecompressionFailed(..) => {
                f.write_str("Failed to decompress an object's section.")
            }
            Self::UnknownArchitecture(arch) => {
                f.write_fmt(format_args!("Unsupported architecture: '{arch:?}'."))
            }
        }
    }
}
