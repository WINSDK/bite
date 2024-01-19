use serde::Deserialize;
use serde::de::{self, Deserializer, Visitor};
use egui::Color32;
use std::fmt;

#[derive(Deserialize)]
pub struct Config {
    pub colors: Colors,
}

#[derive(Deserialize)]
pub struct Colors {
    #[serde(default = "defaults::keyword", deserialize_with = "color32")]
    pub keyword: Color32,
    #[serde(default = "defaults::tipe", deserialize_with = "color32", rename = "type")]
    pub tipe: Color32,
    #[serde(default = "defaults::constructor", deserialize_with = "color32")]
    pub constructor: Color32,
    #[serde(default = "defaults::constructor", deserialize_with = "color32")]
    pub property: Color32,
    #[serde(default = "defaults::function", deserialize_with = "color32")]
    pub function: Color32,
    #[serde(default = "defaults::delimiter", deserialize_with = "color32")]
    pub delimiter: Color32,
}

impl Colors {
    pub fn get_by_style(&self, style: &str) -> Color32 {
        if style.starts_with("keyword") {
            return self.keyword;
        }

        if style.starts_with("function") {
            return self.function;
        }

        if style.starts_with("type") {
            return self.tipe;
        }

        if style.starts_with("constructor") {
            return self.constructor;
        }

        if style.starts_with("property") {
            return self.property;
        }

        if style.starts_with("punctuation") {
            return self.delimiter;
        }

        log::complex!(
            w "[colors::get_by_style] ",
            y format!("{style} is missing a highlighting implementation."),
        );

        Color32::WHITE
    }
}

mod defaults {
    use egui::Color32;

    pub fn keyword() -> Color32 {
        Color32::LIGHT_RED
    }
    pub fn tipe() -> Color32 {
        Color32::YELLOW
    }
    pub fn constructor() -> Color32 {
        Color32::LIGHT_BLUE
    }
    pub fn function() -> Color32 {
        Color32::GREEN
    }
    pub fn delimiter() -> Color32 {
        Color32::GRAY
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
