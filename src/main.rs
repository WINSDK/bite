use std::borrow::Cow;

use object::{Object, ObjectSection, SectionKind};
use iced::{Element, Length, Sandbox};
use iced::widget::{container, scrollable};

mod args;
mod symbols;
mod disassembler;
mod config;
mod macros;

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
    let config = config::Config::from_env(&args);

    let binary = std::fs::read(&args.path).expect("unexpected read of binary failed").leak();
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

        symbols.iter_mut().filter(valid_symbol).for_each(|(_, symbol)| {
            match symbols::Symbol::parse_with_config(symbol, &config) {
                Ok(sym) => println!("{}", sym.display()),
                Err(..) => println!("{:#}", rustc_demangle::demangle(symbol)),
            }
        });
    }

    if args.disassemble {
        let section = obj
            .sections()
            .filter(|s| s.kind() == SectionKind::Text)
            .find(|t| t.name() == Ok(".text"))
            .expect("failed to find `.text` section");

        let raw = section.uncompressed_data().expect("failed to decompress .text section");

        if !args.gui {
            unchecked_println!("Disassembly of section {}:", section.name().unwrap_or("???"));

            let base = section.address() as usize;
            let stream = disassembler::InstructionStream::new(
                &raw,
                obj.architecture(),
                base,
                symbols
            );

            for instruction in stream {
                unchecked_println!("\t{instruction}");
            }
        }

        if args.gui {
            let window = iced::window::Settings {
                min_size: Some((300, 300)),
                ..Default::default()
            };

            let settings = iced::Settings {
                window,
                antialiasing: true,
                default_font: Some(include_bytes!("../assets/vera_sans_mono_bold.ttf")),
                ..Default::default()
            };

            Gui::run(settings).expect("application loop excited");
        }
    }

    Ok(())
}

struct Gui {
    instructions: Vec<String>
}

impl Sandbox for Gui {
    type Message = ();

    fn new() -> Self {
        set_panic_handler();

        let binary = std::fs::read("/tmp/sha").expect("unexpected read of binary failed").leak();
        let obj = object::File::parse(&*binary).expect("failed to parse binary");
        let symbols = symbols::table::parse(&obj).expect("failed to parse symbols table");

        let section = obj
            .sections()
            .filter(|s| s.kind() == SectionKind::Text)
            .find(|t| t.name() == Ok(".text"))
            .expect("failed to find `.text` section");

        let raw = section.uncompressed_data().expect("failed to decompress .text section");

        let base = section.address() as usize;
        let stream = disassembler::InstructionStream::new(
            &raw,
            obj.architecture(),
            base,
            symbols
        );

        Self {
            instructions: stream.into_iter().collect()
        }
    }

    fn title(&self) -> String {
        String::from("Custom 2D geometry - Iced")
    }

    fn update(&mut self, _: ()) {}

    fn view(&self) -> Element<()> {
        let insts = self.instructions.iter().map(|s| iced::Element::from(s.as_str())).collect();

        let content = iced::widget::Column::with_children(insts)
            .padding(20);

        scrollable(
            container(content).width(Length::Fill)
        )
        .into()
    }
}
