//! IBM inspired colors currently used by the GUI.

// necessary as floating-point const function aren't stable yet
#[macro_export]
macro_rules! color {
    ($r:expr, $g:expr, $b:expr) => {
        Color([$r as f32 / 255.0, $g as f32 / 255.0, $b as f32 / 255.0, 1.0])
    };
}

pub const WHITE: Color = color!(0xff, 0xff, 0xff);
pub const BLUE: Color = color!(0x0f, 0x62, 0xfe);
pub const MAGENTA: Color = color!(0xf5, 0x12, 0x81);
pub const RED: Color = color!(0xff, 0x00, 0x0b);
pub const TEAL: Color = color!(0x00, 0x5d, 0x5d);

#[derive(Debug, Clone, Copy)]
pub struct Color([f32; 4]);

impl From<Color> for [f32; 4] {
    fn from(val: Color) -> Self {
        val.0
    }
}
