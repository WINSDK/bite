//! Colors used for rendering text in the GUI.
use std::ops::Deref;
use std::sync::Arc;

pub use egui::Color32;

// TODO: Uniform colors for different instructions sets.
//       These groupings are from:
//       https://docs.rs/yaxpeax-arch/latest/yaxpeax_arch/trait.YaxColors.html#method.number
//
// * arithmetic_op
// * stack_op
// * nop_op
// * stop_op
// * control_flow_op
// * data_op
// * comparison_op
// * invalid_op
// * misc_op
// * platform_op
// * register
// * program_counter
// * number
// * zero
// * one
// * minus_one
// * address
// * symbol
// * function

pub mod colors {
    //! IBM inspired colors.

    use egui::Color32;

    macro_rules! color {
        ($r:expr, $g:expr, $b:expr) => {
            Color32::from_rgb($r, $g, $b)
        };
    }

    pub const WHITE: Color32 = color!(0xff, 0xff, 0xff);
    pub const GREEN: Color32 = color!(0x02, 0xed, 0x6e);
    pub const GRAY35: Color32 = color!(0x35, 0x35, 0x35);
    pub const GRAY60: Color32 = color!(0x60, 0x60, 0x60);
    pub const GRAYAA: Color32 = color!(0xaa, 0xaa, 0xaa);
}

#[derive(Debug, Clone)]
pub enum MaybeStatic {
    Dynamic(Arc<str>),
    Static(&'static str),
}

impl Deref for MaybeStatic {
    type Target = str;

    #[inline(always)]
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
    pub color: Color32,
}

impl Token {
    #[inline(always)]
    pub fn from_str(text: &'static str, color: Color32) -> Self {
        Self {
            text: MaybeStatic::Static(text),
            color,
        }
    }

    #[inline(always)]
    pub fn from_string(text: String, color: Color32) -> Self {
        Self {
            text: MaybeStatic::Dynamic(Arc::from(text)),
            color,
        }
    }
}

impl PartialEq for Token {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        *self.text == *other.text
    }
}

#[derive(Debug)]
pub struct TokenStream {
    pub inner: Vec<Token>,
}

impl TokenStream {
    pub fn new() -> Self {
        Self {
            inner: Vec::with_capacity(25),
        }
    }

    pub fn push_token(&mut self, token: Token) {
        self.inner.push(token);
    }

    pub fn push(&mut self, text: &'static str, color: Color32) {
        self.push_token(Token::from_str(text, color));
    }

    pub fn push_owned(&mut self, text: String, color: Color32) {
        self.push_token(Token::from_string(text, color));
    }

    pub fn clear(&mut self) {
        self.inner.clear();
    }
}

impl ToString for TokenStream {
    fn to_string(&self) -> String {
        self.inner.iter().map(|t| &t.text as &str).collect()
    }
}
