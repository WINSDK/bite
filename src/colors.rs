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
pub const PURPLE: Color = color!(0xc4, 0x91, 0xfd);
pub const GRAY: Color = color!(0x20, 0x20, 0x20);

#[derive(Debug, Clone, Copy)]
pub struct Color([f32; 4]);

impl From<Color> for [f32; 4] {
    fn from(val: Color) -> Self {
        val.0
    }
}

pub enum LineKind {
    Newline,
    Label(std::sync::Arc<crate::symbols::TokenStream>),
    Instruction(crate::disassembler::TokenStream),
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
    color: WHITE,
    text: std::borrow::Cow::Borrowed(""),
};
