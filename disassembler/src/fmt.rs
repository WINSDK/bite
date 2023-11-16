use std::fmt;

impl fmt::Debug for super::Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IO(err) => f.write_fmt(format_args!("{err}.")),
            Self::NotAnExecutable => f.write_str("A given object is not an executable."),
            Self::DecompressionFailed(..) => {
                f.write_str("Failed to decompress an object's section.")
            }
            Self::IncompleteObject(err) => {
                f.write_fmt(format_args!("Failed to parse an object: '{err}'."))
            }
            Self::IncompleteImportTable(err) => {
                f.write_fmt(format_args!("Failed to parse import table: '{err}'."))
            }
            Self::IncompleteSymbolTable(err) => {
                f.write_fmt(format_args!("Failed to parse symbol table: '{err}'."))
            }
            Self::UnknownArchitecture(arch) => {
                f.write_fmt(format_args!("Unsupported architecture: '{arch:?}'."))
            }
        }
    }
}

impl fmt::Debug for super::DisassemblyView {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DisassemblyView")
            .field("addr", &format_args!("{:#X}", self.addr))
            .field("block_offset", &self.block_offset)
            .field("block_line_offset", &self.block_line_offset)
            .field("line_offset", &self.line_offset)
            .field("line_count", &self.line_count)
            .field("max_lines", &self.max_lines)
            .finish()
    }
}
