//! Colors used for rendering text in the GUI.

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

    fn opcode() -> Color;
    fn register() -> Color;
    fn immediate() -> Color;
    fn attribute() -> Color;
    fn segment() -> Color;
}

pub struct IBM;

impl ColorScheme for IBM {
    fn brackets() -> Color {
        colors::GRAY40
    }

    fn delimiter() -> Color {
        colors::GRAY20
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
pub struct Token {
    pub text: std::borrow::Cow<'static, str>,
    pub color: Color,
}

impl Token {
    pub fn text(&self, scale: f32) -> wgpu_glyph::Text {
        wgpu_glyph::Text::new(&self.text)
            .with_color(self.color)
            .with_scale(scale)
    }
}

pub const EMPTY_TOKEN: Token = Token {
    color: colors::WHITE,
    text: std::borrow::Cow::Borrowed(""),
};
