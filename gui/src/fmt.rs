use crate::Error;
use std::fmt;

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WindowCreation => f.write_str("Failed to create a window."),
            Self::SurfaceCreation(err) => {
                f.write_fmt(format_args!("Failed to create a surface: '{err:?}'."))
            }
            Self::AdapterRequest => {
                f.write_str("Failed to find a adapter that supports our surface.")
            }
            Self::DeviceRequest(err) => f.write_fmt(format_args!(
                "Failed to find a device that meets our adapter's limits: '{err:?}'."
            )),
            Self::InvalidTextureId(id) => {
                f.write_fmt(format_args!("Egui texture id '{id:?}' was invalid."))
            }
            Self::PngDecode => f.write_str("Invalid data given to the png decoder."),
            Self::PngFormat => {
                f.write_str("Unsupported texture format produced by the png decoder.")
            }
            Self::NotFound(path) => {
                f.write_fmt(format_args!("Failed to find path: '{}'", path.display()))
            }
            Self::DuplicateInstance => f.write_str("Can't create multiple UI's."),
        }
    }
}
