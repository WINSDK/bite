use egui::Color32;
use serde::de::{self, Deserializer, Visitor};
use serde::Deserialize;
use std::fmt;

#[derive(Deserialize)]
pub struct Config {
    #[serde(default = "defaults::colors")]
    pub colors: Colors,
}

#[derive(Deserialize)]
pub struct Colors {
    #[serde(default = "defaults::keyword", deserialize_with = "color32")]
    keyword: Color32,
    #[serde(default = "defaults::tipe", deserialize_with = "color32")]
    tipe: Color32,
    #[serde(default = "defaults::field", deserialize_with = "color32")]
    field: Color32,
    #[serde(default = "defaults::function", deserialize_with = "color32")]
    function: Color32,
    #[serde(default = "defaults::delimiter", deserialize_with = "color32")]
    delimiter: Color32,
    #[serde(default = "defaults::operator", deserialize_with = "color32")]
    operator: Color32,
    #[serde(default = "defaults::comment", deserialize_with = "color32")]
    comment: Color32,
    #[serde(default = "defaults::string", deserialize_with = "color32")]
    string: Color32,
    #[serde(default = "defaults::variable", deserialize_with = "color32")]
    variable: Color32,
    #[serde(default = "defaults::constant", deserialize_with = "color32")]
    constant: Color32,
}

impl Colors {
    pub fn get_by_style(&self, style: &str) -> Color32 {
        if style.starts_with("none") || style == "_parent" {
            return defaults::anything();
        }
        if style.starts_with("keyword") || style.starts_with("module") {
            return self.keyword;
        }
        if style.starts_with("function") {
            return self.function;
        }
        if style.starts_with("type") || style.starts_with("property") {
            return self.tipe;
        }
        if style.starts_with("punctuation") {
            return self.delimiter;
        }
        if style.starts_with("operator") {
            return self.operator;
        }
        if style.starts_with("comment") {
            return self.comment;
        }
        if style.starts_with("string") {
            return self.string;
        }
        if style.starts_with("variable.member") {
            return self.field;
        }
        if style.starts_with("variable.builtin") {
            return self.variable;
        }
        if style.starts_with("variable") {
            return self.variable;
        }
        if style.starts_with("escape") {
            return self.string;
        }
        if style.starts_with("number") || style.starts_with("constant") || style == "boolean" {
            return self.constant;
        }

        log::complex!(
            w "[colors::get_by_style] ",
            y format!("{style} is missing a highlighting implementation."),
        );

        defaults::anything()
    }
}

mod defaults {
    use egui::Color32;

    pub fn colors() -> super::Colors {
        serde_yaml::from_str("").unwrap()
    }

    pub fn anything() -> Color32 {
        Color32::from_rgb(200, 200, 200)
    }

    pub fn keyword() -> Color32 {
        Color32::from_hex("#FF5900").unwrap()
    }
    pub fn tipe() -> Color32 {
        Color32::from_hex("#FAA51B").unwrap()
    }
    pub fn field() -> Color32 {
        Color32::from_hex("#288CC7").unwrap()
    }
    pub fn function() -> Color32 {
        Color32::from_hex("#02ED6E").unwrap()
    }
    pub fn delimiter() -> Color32 {
        Color32::GRAY
    }
    pub fn operator() -> Color32 {
        Color32::from_hex("#FFA500").unwrap()
    }
    pub fn comment() -> Color32 {
        Color32::GRAY
    }
    pub fn string() -> Color32 {
        Color32::from_hex("#02ED6E").unwrap()
    }
    pub fn variable() -> Color32 {
        Color32::from_hex("#D46CCB").unwrap()
    }
    pub fn constant() -> Color32 {
        Color32::from_hex("#9B51C2").unwrap()
    }
}

impl Config {
    pub fn parse() -> Self {
        let raw = std::fs::read_to_string("./config.yaml").unwrap_or_default();
        match serde_yaml::from_str(&raw) {
            Ok(parsed) => parsed,
            Err(err) => {
                log::warning!("{err}");

                // parse everything as default
                serde_yaml::from_str("").unwrap()
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
