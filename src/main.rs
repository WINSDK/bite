mod args;
mod disassembler;
mod macros;
mod symbols;

use std::borrow::Cow;

use iced::widget::{container, scrollable};
use iced::{Element, Length, Sandbox};
use object::{Object, ObjectSection, SectionKind};

fn set_panic_handler() {
    #[cfg(not(debug_assertions))]
    std::panic::set_hook(Box::new(|details| {
        if let Some(msg) = details.payload().downcast_ref::<String>() {
            return unchecked_println!("{msg}");
        }

        if let Some(msg) = details.payload().downcast_ref::<&str>() {
            return unchecked_println!("{msg}");
        }

        unchecked_println!("panic occurred")
    }));
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    set_panic_handler();

    let args = args::Cli::parse();
    let config = symbols::Config::from_env(&args);

    let binary = std::fs::read(&args.path)
        .expect("unexpected read of binary failed")
        .leak();
    let obj = object::File::parse(&*binary).expect("failed to parse binary");
    let mut symbols = symbols::table::parse(&obj).expect("failed to parse symbols table");

    if args.libs {
        println!("{}:", args.path.display());

        for import in obj.imports().expect("failed to resolve any symbols") {
            let library = match std::str::from_utf8(import.library()) {
                Ok(library) => library,
                Err(_) => continue,
            };

            match std::str::from_utf8(import.name()) {
                Ok(name) => println!("\t{library} => {name}"),
                Err(_) => println!("\t{library}"),
            };
        }
    }

    if args.names {
        if symbols.is_empty() {
            exit!(fail, "no symbols found: '{}'", args.path.display());
        }

        fn valid_symbol(symbol: &(&usize, &mut Cow<'static, str>)) -> bool {
            !symbol.1.starts_with("GCC_except_table")
                && !symbol.1.contains("cgu")
                && !symbol.1.is_empty()
        }

        symbols
            .iter_mut()
            .filter(valid_symbol)
            .for_each(
                |(_, symbol)| match symbols::Symbol::parse_with_config(symbol, &config) {
                    Ok(sym) => println!("{}", sym.display()),
                    Err(..) => println!("{:#}", rustc_demangle::demangle(symbol)),
                },
            );
    }

    if args.disassemble {
        let section = obj
            .sections()
            .filter(|s| s.kind() == SectionKind::Text)
            .find(|t| t.name() == Ok(".text"))
            .expect("failed to find `.text` section");

        let raw = section
            .uncompressed_data()
            .expect("failed to decompress .text section");

        if !args.gui {
            unchecked_println!(
                "Disassembly of section {}:\n",
                section.name().unwrap_or("???")
            );

            let base = section.address() as usize;
            let stream =
                disassembler::InstructionStream::new(&raw, obj.architecture(), base, &symbols);

            for instruction in stream {
                unchecked_println!("{instruction}");
            }
        }

        if args.gui {
            let window = iced::window::Settings {
                min_size: Some((580, 300)),
                ..Default::default()
            };

            let settings = iced::Settings {
                window,
                antialiasing: false,
                default_font: Some(include_bytes!("../assets/vera_sans_mono_bold.ttf")),
                ..Default::default()
            };

            Gui::run(settings).expect("application loop excited");
        }
    }

    Ok(())
}

struct Bordered;

impl container::StyleSheet for Bordered {
    type Style = iced::Theme;

    fn appearance(&self, _style: &Self::Style) -> container::Appearance {
        container::Appearance {
            border_color: iced::color!(50, 47, 47),
            border_width: 4.0,
            ..Default::default()
        }
    }
}

struct Window;

impl container::StyleSheet for Window {
    type Style = iced::Theme;

    fn appearance(&self, _style: &Self::Style) -> container::Appearance {
        container::Appearance {
            background: Some(iced::Background::Color(iced::color!(40, 40, 40))),
            text_color: Some(iced::color!(224, 202, 168)),
            ..Default::default()
        }
    }
}

struct Gui<'a> {
    instructions: Vec<String>,
    shown_files: Vec<String>,
    symbols: std::collections::BTreeMap<usize, Cow<'a, str>>
}

impl Sandbox for Gui<'_> {
    type Message = ();

    fn new() -> Self {
        set_panic_handler();

        let binary = std::fs::read("/tmp/sha")
            .expect("unexpected read of binary failed")
            .leak();
        let obj = object::File::parse(&*binary).expect("failed to parse binary");
        let symbols = symbols::table::parse(&obj).expect("failed to parse symbols table");

        let section = obj
            .sections()
            .filter(|s| s.kind() == SectionKind::Text)
            .find(|t| t.name() == Ok(".text"))
            .expect("failed to find `.text` section");

        let raw = section
            .uncompressed_data()
            .expect("failed to decompress .text section");

        let base = section.address() as usize;
        let stream = disassembler::InstructionStream::new(&raw, obj.architecture(), base, &symbols);

        let shown_files = std::fs::read_dir(".")
            .unwrap()
            .filter_map(std::result::Result::ok)
            .map(|entry| entry.path())
            .map(|path| {
                path.components()
                    .last()
                    .unwrap()
                    .as_os_str()
                    .to_string_lossy()
                    .into_owned()
            })
            .filter(|stem| !stem.starts_with("."))
            .collect();

        let instructions: Vec<String> = stream
            .into_iter()
            .map(|s| {
                s.replace('\t', "    ")
                    .split('\n')
                    .map(|sub| String::from(if sub == "" { " " } else { sub }))
                    .collect::<Vec<String>>()
            })
            .flatten()
            .collect();

        Self {
            instructions,
            shown_files,
            symbols
        }
    }

    fn title(&self) -> String {
        String::from("rustdump")
    }

    fn theme(&self) -> iced::Theme {
        iced::Theme::Dark
    }

    fn update(&mut self, _: ()) {}

    fn view(&self) -> Element<()> {
        let insts = self
            .instructions
            .iter()
            .map(|s| iced::Element::from(s.as_str()))
            .collect();

        let insts = iced::widget::Column::with_children(insts).padding(10);

        let files = self
            .shown_files
            .iter()
            .map(|s| iced::Element::from(s.as_str()))
            .collect();

        let files = iced::widget::Column::with_children(files).padding(10);

        let symbols = self
            .symbols
            .iter()
            .map(|(_, symbol)| iced::Element::from(symbol as &str))
            .collect();

        let symbols = iced::widget::Column::with_children(symbols).padding(10);

        container(iced::widget::row![
            iced::widget::column![
                container(scrollable(files))
                    .style(iced::theme::Container::Custom(Box::new(Bordered))),
                container(scrollable(symbols))
                    .style(iced::theme::Container::Custom(Box::new(Bordered))),
            ].max_width(300).height(Length::Fill),
            scrollable(container(insts).width(Length::Fill))
        ])
        .style(iced::theme::Container::Custom(Box::new(Window)))
        .into()
    }
}
