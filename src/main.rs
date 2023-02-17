#![allow(clippy::unusual_byte_groupings)]

mod args;
mod disassembler;
mod macros;
mod symbols;

use std::collections::VecDeque;

use iced::widget::{self, container, row, scrollable, Column};
use iced::{Application, Element, Length};
use iced_native::keyboard;

use object::{Object, ObjectSection, SectionKind};
use once_cell::sync::Lazy;

static ARGS: Lazy<args::Cli> = Lazy::new(args::Cli::parse);
static CONFIG: Lazy<symbols::Config> = Lazy::new(|| symbols::Config::from_env(&ARGS));

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

    let binary = std::fs::read(&ARGS.path)
        .expect("unexpected read of binary failed")
        .leak();

    let obj = object::File::parse(&*binary).expect("failed to parse binary");
    let symbols = symbols::table::parse(&obj).expect("failed to parse symbols table");

    if ARGS.libs {
        println!("{}:", ARGS.path.display());

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

    if ARGS.disassemble {
        if !ARGS.gui {
            let section = obj
                .sections()
                .filter(|s| s.kind() == SectionKind::Text)
                .find(|t| t.name() == Ok(".text"))
                .expect("failed to find `.text` section");

            let raw = section
                .uncompressed_data()
                .expect("failed to decompress .text section");

            unchecked_println!(
                "Disassembly of section {}:\n",
                section.name().unwrap_or("???")
            );

            let base_offset = section.address() as usize;
            let stream = disassembler::InstructionStream::new(
                &raw,
                obj.architecture(),
                base_offset,
                &symbols,
            );

            for instruction in stream {
                unchecked_println!("{instruction}");
            }
        }

        if ARGS.gui {
            let window = iced::window::Settings {
                min_size: Some((580, 300)),
                ..Default::default()
            };

            let settings = iced::Settings {
                window,
                antialiasing: false,
                default_font: Some(include_bytes!("../assets/LigaSFMonoNerdFont-Regular.otf")),
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
            background: Some(iced::Background::Color(iced::color!(26, 26, 26))),
            text_color: Some(iced::color!(224, 202, 168)),
            ..Default::default()
        }
    }
}

#[derive(Debug)]
enum Message {
    EventOccurred(iced_native::Event),
}

struct Gui<'a> {
    instructions: Vec<String>,
    shown_files: Vec<String>,
    symbols: symbols::table::SymbolLookup<'a>,
    events: VecDeque<iced_native::Event>,
    debug: bool,
}

impl Application for Gui<'_> {
    type Message = Message;
    type Executor = iced::executor::Default;
    type Flags = ();
    type Theme = iced::Theme;

    fn new(_flags: ()) -> (Self, iced::Command<Message>) {
        set_panic_handler();

        let binary = std::fs::read(&ARGS.path)
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
            .filter(|stem| !stem.starts_with('.'))
            .collect();

        let instructions: Vec<String> = stream
            .into_iter()
            .take(4096)
            .flat_map(|s| {
                s.replace('\t', "    ")
                    .split('\n')
                    .map(|s| s.replacen("", " ", 1))
                    .collect::<Vec<String>>()
            })
            .collect();

        let gui = Self {
            instructions,
            shown_files,
            symbols,
            events: VecDeque::new(),
            debug: false,
        };

        (gui, iced::Command::none())
    }

    fn title(&self) -> String {
        String::from("rustdump")
    }

    fn theme(&self) -> iced::Theme {
        iced::Theme::Dark
    }

    fn update(&mut self, message: Message) -> iced::Command<Message> {
        match message {
            Message::EventOccurred(event) => {
                if self.events.len() > 2 {
                    self.events.pop_front();
                }

                self.events.push_back(event.clone());

                match event {
                    iced::Event::Keyboard(keyboard::Event::KeyPressed {
                        key_code: keyboard::KeyCode::F3,
                        ..
                    }) => {
                        self.debug = !self.debug;
                    }
                    _ => {}
                }

                iced::Command::none()
            }
        }
    }

    fn subscription(&self) -> iced::Subscription<Self::Message> {
        iced_native::subscription::events().map(Message::EventOccurred)
    }

    fn view(&self) -> Element<Message> {
        let instructions = self
            .instructions
            .iter()
            .map(|s| iced::Element::from(s as &str))
            .collect();

        let insts = scrollable(
            container(Column::with_children(instructions).padding(10)).width(Length::Fill),
        );

        // let files = self
        //     .shown_files
        //     .iter()
        //     .take(4096)
        //     .map(|s| iced::Element::from(s.as_str()))
        //     .collect();

        // let files = Column::with_children(files).padding(10);

        let symbols = self
            .symbols
            .iter()
            .take(4096)
            .map(|(_, s)| iced::Element::from(s as &str))
            .collect();

        let symbols = container(scrollable(Column::with_children(symbols).padding(10)))
            .style(iced::theme::Container::Custom(Box::new(Bordered)))
            .max_width(300)
            .width(Length::Shrink)
            .height(Length::Fill);

        let mut rhs = Column::new();

        if self.debug {
            let events = self
                .events
                .iter()
                .map(|event| widget::text(format!("{event:?}")).size(15))
                .map(Element::from)
                .collect();

            let events = container(Column::with_children(events))
                .width(Length::Fill)
                .style(iced::theme::Container::Custom(Box::new(Bordered)))
                .padding(10);

            rhs = rhs.push(events);
        }

        rhs = rhs.push(insts);

        let content = row![symbols, rhs];

        container(content)
            .style(iced::theme::Container::Custom(Box::new(Window)))
            .into()
    }
}
