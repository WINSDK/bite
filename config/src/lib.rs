use once_cell::sync::Lazy;

pub static CONFIG: Lazy<Config> = Lazy::new(Config::parse);

use egui::Color32;
use serde::de::{self, Deserializer, Visitor};
use serde::Deserialize;
use std::fmt;

#[derive(Debug, Deserialize)]
pub struct Config {
    #[serde(default = "defaults::colors")]
    pub colors: Colors,
}

#[derive(Debug, Deserialize)]
pub struct Colors {
    #[serde(default = "defaults::src_colors")]
    pub src: SourceColors,
    #[serde(default = "defaults::asm_colors")]
    pub asm: AsmColors,
    #[serde(default = "defaults::comment", deserialize_with = "color32")]
    pub comment: Color32,
    #[serde(default = "defaults::address", deserialize_with = "color32")]
    pub address: Color32,
    #[serde(default = "defaults::brackets", deserialize_with = "color32")]
    pub brackets: Color32,
    #[serde(default = "defaults::bytes", deserialize_with = "color32")]
    pub bytes: Color32,
    #[serde(default = "defaults::delimiter", deserialize_with = "color32")]
    pub delimiter: Color32,
    #[serde(default = "defaults::bg_primary", deserialize_with = "color32")]
    pub bg_primary: Color32,
    #[serde(default = "defaults::bg_secondary", deserialize_with = "color32")]
    pub bg_secondary: Color32,
}

#[derive(Debug, Deserialize)]
pub struct SourceColors {
    #[serde(default = "defaults::keyword", deserialize_with = "color32")]
    pub keyword: Color32,
    #[serde(default = "defaults::tipe", deserialize_with = "color32")]
    pub tipe: Color32,
    #[serde(default = "defaults::field", deserialize_with = "color32")]
    pub field: Color32,
    #[serde(default = "defaults::function", deserialize_with = "color32")]
    pub function: Color32,
    #[serde(default = "defaults::operator", deserialize_with = "color32")]
    pub operator: Color32,
    #[serde(default = "defaults::code_string", deserialize_with = "color32")]
    pub string: Color32,
    #[serde(default = "defaults::variable", deserialize_with = "color32")]
    pub variable: Color32,
    #[serde(default = "defaults::constant", deserialize_with = "color32")]
    pub constant: Color32,
    #[serde(default = "defaults::highlight", deserialize_with = "color32")]
    pub highlight: Color32,
}

#[derive(Debug, Deserialize)]
pub struct AsmColors {
    #[serde(default = "defaults::section", deserialize_with = "color32")]
    pub section: Color32,
    #[serde(default = "defaults::opcode", deserialize_with = "color32")]
    pub opcode: Color32,
    #[serde(default = "defaults::component", deserialize_with = "color32")]
    pub component: Color32,
    #[serde(default = "defaults::register", deserialize_with = "color32")]
    pub register: Color32,
    #[serde(default = "defaults::label", deserialize_with = "color32")]
    pub label: Color32,
    #[serde(default = "defaults::segment", deserialize_with = "color32")]
    pub segment: Color32,
    #[serde(default = "defaults::invalid", deserialize_with = "color32")]
    pub invalid: Color32,
    #[serde(default = "defaults::pointer", deserialize_with = "color32")]
    pub pointer: Color32,
    #[serde(default = "defaults::expr", deserialize_with = "color32")]
    pub expr: Color32,
    #[serde(default = "defaults::immediate", deserialize_with = "color32")]
    pub immediate: Color32,
    #[serde(default = "defaults::annotation", deserialize_with = "color32")]
    pub annotation: Color32,
    #[serde(default = "defaults::primitive", deserialize_with = "color32")]
    pub primitive: Color32,
    #[serde(default = "defaults::asm_string", deserialize_with = "color32")]
    pub string: Color32,
}

impl Colors {
    pub fn get_by_style(&self, style: &str) -> Color32 {
        if style.starts_with("none") || style == "_parent" {
            return defaults::anything();
        }
        if style.starts_with("keyword") || style.starts_with("module") {
            return self.src.keyword;
        }
        if style.starts_with("function") {
            return self.src.function;
        }
        if style.starts_with("type") || style.starts_with("property") {
            return self.src.tipe;
        }
        if style.starts_with("punctuation") {
            return self.delimiter;
        }
        if style.starts_with("operator") {
            return self.src.operator;
        }
        if style.starts_with("comment") {
            return self.comment;
        }
        if style.starts_with("string") {
            return self.src.string;
        }
        if style.starts_with("variable.member") {
            return self.src.field;
        }
        if style.starts_with("variable.builtin") {
            return self.src.variable;
        }
        if style.starts_with("variable") {
            return self.src.variable;
        }
        if style.starts_with("escape") {
            return self.src.string;
        }
        if style.starts_with("number") || style.starts_with("constant") || style == "boolean" {
            return self.src.constant;
        }

        log::complex!(
            w "[colors::get_by_style] ",
            y format!("{style} is missing a highlighting implementation."),
        );

        defaults::anything()
    }
}

/// Default color values when one is missing in the config.yaml's colors field.
/// Important to note is that any update to these defaults should also be reflected in the
/// config.yaml.
mod defaults {
    use egui::Color32;

    pub fn config() -> super::Config {
        serde_yaml::from_str("").unwrap()
    }
    pub fn colors() -> super::Colors {
        serde_yaml::from_str("").unwrap()
    }
    pub fn src_colors() -> super::SourceColors {
        serde_yaml::from_str("").unwrap()
    }
    pub fn asm_colors() -> super::AsmColors {
        serde_yaml::from_str("").unwrap()
    }

    pub fn anything() -> Color32 {
        Color32::from_rgb(0xc8, 0xc8, 0xc8)
    }

    pub fn keyword() -> Color32 {
        Color32::from_rgb(0xff, 0x59, 0x00)
    }
    pub fn tipe() -> Color32 {
        Color32::from_rgb(0xfa, 0xa5, 0x1b)
    }
    pub fn field() -> Color32 {
        Color32::from_rgb(0x28, 0x8c, 0xc7)
    }
    pub fn function() -> Color32 {
        Color32::from_rgb(0x02, 0xed, 0x6e)
    }
    pub fn operator() -> Color32 {
        Color32::from_rgb(0xff, 0xa5, 0x00)
    }
    pub fn code_string() -> Color32 {
        Color32::from_rgb(0x02, 0xed, 0x6e)
    }
    pub fn variable() -> Color32 {
        Color32::from_rgb(0xd4, 0x6c, 0xcb)
    }
    pub fn constant() -> Color32 {
        Color32::from_rgb(0x9b, 0x51, 0xc2)
    }
    pub fn highlight() -> Color32 {
        Color32::from_rgb(255, 100, 0)
    }

    pub fn section() -> Color32 {
        label()
    }
    pub fn opcode() -> Color32 {
        Color32::from_rgb(0xff, 0xff, 0xff)
    }
    pub fn component() -> Color32 {
        Color32::from_rgb(0xf5, 0x12, 0x81)
    }
    pub fn register() -> Color32 {
        component()
    }
    pub fn label() -> Color32 {
        Color32::from_rgb(0x3e, 0xbc, 0xe6)
    }
    pub fn segment() -> Color32 {
        Color32::from_rgb(0x02, 0xed, 0x6e)
    }
    pub fn invalid() -> Color32 {
        Color32::from_rgb(0xff, 0x1f, 0xff)
    }
    pub fn pointer() -> Color32 {
        invalid()
    }
    pub fn expr() -> Color32 {
        Color32::from_rgb(0x99, 0x99, 0x99)
    }
    pub fn immediate() -> Color32 {
        Color32::from_rgb(0x3e, 0xbc, 0xe6)
    }
    pub fn annotation() -> Color32 {
        immediate()
    }
    pub fn primitive() -> Color32 {
        Color32::from_rgb(0x69, 0xb5, 0xa9)
    }
    pub fn asm_string() -> Color32 {
        Color32::from_rgb(0xe6, 0xab, 0x3e)
    }

    pub fn brackets() -> Color32 {
        Color32::from_rgb(0x60, 0x60, 0x60)
    }
    pub fn comment() -> Color32 {
        Color32::from_rgb(0xa0, 0xa0, 0xa0)
    }
    pub fn address() -> Color32 {
        Color32::from_rgb(0x40, 0x40, 0x40)
    }
    pub fn bytes() -> Color32 {
        Color32::from_rgb(0x02, 0xed, 0x6e)
    }
    pub fn delimiter() -> Color32 {
        comment()
    }
    pub fn bg_primary() -> Color32 {
        Color32::from_rgb(0x30, 0x30, 0x30)
    }
    pub fn bg_secondary() -> Color32 {
        Color32::from_rgb(0x2d, 0x2d, 0x2d)
    }
}

impl Config {
    pub fn parse() -> Self {
        let path = match dirs::data_dir() {
            Some(mut dir) => {
                dir.push("bite");
                dir.push("config.yaml");
                dir
            },
            None => log::error!("You must have a data directory set."),
        };

        let raw = std::fs::read_to_string(path).unwrap_or_default();
        match serde_yaml::from_str(&raw) {
            Ok(parsed) => parsed,
            Err(err) => {
                log::warning!("Failed to parse config.\nError: {err}.");

                // parse everything as default
                defaults::config()
            }
        }
    }
}

fn color32<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Color32, D::Error> {
    struct ColorParsing;
    impl<'de> Visitor<'de> for ColorParsing {
        type Value = Color32;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("expected hex color values")
        }

        fn visit_str<E: de::Error>(self, s: &str) -> Result<Self::Value, E> {
            Color32::from_hex(s).map_err(|err| E::custom(format!("{err:?}")))
        }
    }

    deserializer.deserialize_str(ColorParsing)
}
