//! Colors used for rendering text in the GUI.
use std::borrow::Cow;

/// Currently used global colorscheme
pub type Colors = IBM;

pub trait ColorScheme {
    fn brackets() -> &'static Color;
    fn delimiter() -> &'static Color;
    fn comment() -> &'static Color;
    fn item() -> &'static Color;

    fn spacing() -> &'static Color {
        &colors::WHITE
    }

    fn known() -> &'static Color {
        Self::item()
    }

    fn root() -> &'static Color {
        Self::item()
    }

    fn annotation() -> &'static Color {
        Self::item()
    }

    fn special() -> &'static Color {
        Self::item()
    }

    fn expr() -> &'static Color;
    fn opcode() -> &'static Color;
    fn register() -> &'static Color;
    fn immediate() -> &'static Color;
    fn attribute() -> &'static Color;
    fn segment() -> &'static Color;
}

pub struct IBM;

impl ColorScheme for IBM {
    fn brackets() -> &'static Color {
        &colors::GRAY40
    }

    fn delimiter() -> &'static Color {
        &colors::GRAY20
    }

    fn comment() -> &'static Color {
        &colors::GRAY20
    }

    fn item() -> &'static Color {
        &colors::MAGENTA
    }

    fn known() -> &'static Color {
        &colors::PURPLE
    }

    fn root() -> &'static Color {
        &colors::PURPLE
    }

    fn annotation() -> &'static Color {
        &colors::BLUE
    }

    fn special() -> &'static Color {
        &colors::RED
    }

    fn expr() -> &'static Color {
        &colors::GRAY99
    }

    fn opcode() -> &'static Color {
        &colors::WHITE
    }

    fn register() -> &'static Color {
        &colors::MAGENTA
    }

    fn immediate() -> &'static Color {
        &colors::BLUE
    }

    fn attribute() -> &'static Color {
        &colors::GRAY40
    }

    fn segment() -> &'static Color {
        &colors::GREEN
    }
}

pub mod colors {
    //! IBM inspired colors.

    use super::Color;

    // necessary as floating-point const function aren't stable yet
    #[macro_export]
    macro_rules! color {
        ($r:expr, $g:expr, $b:expr) => {
            $crate::Color([$r as f32 / 255.0, $g as f32 / 255.0, $b as f32 / 255.0, 1.0])
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
    pub const GRAY40: Color = color!(0x40, 0x40, 0x40);
    pub const GRAY99: Color = color!(0x99, 0x99, 0x99);
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Color([f32; 4]);

impl Default for Color {
    fn default() -> Self {
        colors::WHITE
    }
}

unsafe impl bytemuck::Zeroable for Color {}
unsafe impl bytemuck::Pod for Color {}

impl From<Color> for [f32; 4] {
    fn from(val: Color) -> Self {
        val.0
    }
}

#[derive(Debug, Clone)]
pub struct Token<'txt> {
    pub text: Cow<'txt, str>,
    pub color: &'static Color,
}

impl<'txt> Token<'txt> {
    pub fn from_string(text: String, color: &'static Color) -> Self {
        Self {
            text: Cow::Owned(text),
            color,
        }
    }

    pub fn from_str(text: &'static str, color: &'static Color) -> Self {
        Self {
            text: Cow::Borrowed(text),
            color,
        }
    }

    pub fn as_ref(&'txt self) -> Token<'txt> {
        Self {
            text: Cow::Borrowed(self.text.as_ref()),
            color: self.color,
        }
    }

    pub fn text(&self, scale: f32) -> wgpu_glyph::Text {
        wgpu_glyph::Text::new(&self.text)
            .with_color(*self.color)
            .with_scale(scale)
    }
}

pub const EMPTY_TOKEN: Token = Token {
    color: &colors::WHITE,
    text: std::borrow::Cow::Borrowed(""),
};
