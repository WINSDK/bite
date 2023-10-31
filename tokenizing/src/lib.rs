//! Colors used for rendering text in the GUI.
use std::ops::Deref;
use std::sync::Arc;

pub use egui::Color32 as Color;

/// Currently used global colorscheme
pub type Colors = IBM;

pub trait ColorScheme {
    fn brackets() -> Color;
    fn delimiter() -> Color;
    fn comment() -> Color;
    fn item() -> Color;

    fn spacing() -> Color {
        colors::WHITE
    }

    fn known() -> Color {
        Self::item()
    }

    fn root() -> Color {
        Self::item()
    }

    fn annotation() -> Color {
        Self::item()
    }

    fn special() -> Color {
        Self::item()
    }

    fn expr() -> Color;
    fn opcode() -> Color;
    fn register() -> Color;
    fn immediate() -> Color;
    fn attribute() -> Color;
    fn segment() -> Color;
}

pub struct IBM;

impl ColorScheme for IBM {
    fn brackets() -> Color {
        colors::GRAY60
    }

    fn delimiter() -> Color {
        colors::GRAY40
    }

    fn comment() -> Color {
        colors::GRAY20
    }

    fn item() -> Color {
        colors::MAGENTA
    }

    fn known() -> Color {
        colors::PURPLE
    }

    fn root() -> Color {
        colors::PURPLE
    }

    fn annotation() -> Color {
        colors::BLUE
    }

    fn special() -> Color {
        colors::RED
    }

    fn expr() -> Color {
        colors::GRAY99
    }

    fn opcode() -> Color {
        colors::WHITE
    }

    fn register() -> Color {
        colors::MAGENTA
    }

    fn immediate() -> Color {
        colors::BLUE
    }

    fn attribute() -> Color {
        colors::GRAY40
    }

    fn segment() -> Color {
        colors::GREEN
    }
}

pub mod colors {
    //! IBM inspired colors.

    use super::Color;

    macro_rules! color {
        ($r:expr, $g:expr, $b:expr) => {
            Color::from_rgb($r, $g, $b)
        };
    }

    pub const WHITE: Color = color!(0xff, 0xff, 0xff);
    pub const BLUE: Color = color!(0x0f, 0x62, 0xfe);
    pub const MAGENTA: Color = color!(0xf5, 0x12, 0x81);
    pub const RED: Color = color!(0xff, 0x00, 0x0b);
    pub const PURPLE: Color = color!(0x89, 0x1f, 0xff);
    pub const GREEN: Color = color!(0x02, 0xed, 0x6e);
    pub const GRAY10: Color = color!(0x10, 0x10, 0x10);
    pub const GRAY20: Color = color!(0x20, 0x20, 0x20);
    pub const GRAY30: Color = color!(0x30, 0x30, 0x30);
    pub const GRAY35: Color = color!(0x35, 0x35, 0x35);
    pub const GRAY40: Color = color!(0x40, 0x40, 0x40);
    pub const GRAY60: Color = color!(0x60, 0x60, 0x60);
    pub const GRAY99: Color = color!(0x99, 0x99, 0x99);
    pub const GRAYAA: Color = color!(0xaa, 0xaa, 0xaa);
}

#[derive(Debug, Clone)]
pub enum MaybeStatic {
    Dynamic(Arc<str>),
    Static(&'static str),
}

impl Deref for MaybeStatic {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Dynamic(s) => s as &str,
            Self::Static(s) => s,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub text: MaybeStatic,
    pub color: Color,
}

impl Token {
    #[inline]
    pub fn from_str(text: &'static str, color: Color) -> Self {
        Self {
            text: MaybeStatic::Static(text),
            color,
        }
    }

    #[inline]
    pub fn from_string(text: String, color: Color) -> Self {
        Self {
            text: MaybeStatic::Dynamic(Arc::from(text)),
            color,
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        *self.text == *other.text
    }
}
