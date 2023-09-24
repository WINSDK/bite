use object::Object;
use tokenizing::{colors, ColorScheme, Colors, Token};

use std::fmt;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

type Addr = usize;

pub enum DecodeError {
    IO(std::io::Error),
    NotAnExecutable,
    DecompressionFailed(object::Error),
    IncompleteObject(object::Error),
    IncompleteImportTable(object::Error),
    IncompleteSymbolTable(pdb::Error),
    UnknownArchitecture(object::Architecture),
}

impl fmt::Debug for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IO(err) => f.write_fmt(format_args!("{err}.")),
            Self::NotAnExecutable => f.write_str("A given object is not an executable."),
            Self::DecompressionFailed(..) => {
                f.write_str("Failed to decompress an object's section.")
            }
            Self::IncompleteObject(err) => {
                f.write_fmt(format_args!("Failed to parse an object: '{err}'."))
            }
            Self::IncompleteImportTable(err) => {
                f.write_fmt(format_args!("Failed to parse import table: '{err}'."))
            }
            Self::IncompleteSymbolTable(err) => {
                f.write_fmt(format_args!("Failed to parse symbol table: '{err}'."))
            }
            Self::UnknownArchitecture(arch) => {
                f.write_fmt(format_args!("Unsupported architecture: '{arch:?}'."))
            }
        }
    }
}

pub struct Disassembly {
    /// Where execution start.
    pub entrypoint: Addr,

    /// Address lookup for instructions.
    pub processor: disassembler::Processor,

    /// Symbol lookup by absolute address.
    pub symbols: symbols::Index,

    current_addr: AtomicUsize,

    /// Line offset at current address (one address may produce many lines).
    line_offset: AtomicUsize,
}

impl Disassembly {
    /// Jump to address, returning whether it succeeded.
    ///
    /// Try an address range of +- 32 bytes.
    pub fn jump(&self, addr: Addr) -> bool {
        for offset in 0..16 {
            let addr = addr.saturating_add_signed(offset);

            if let Some(_) = self.processor.error_by_addr(addr) {
                self.current_addr.store(addr, Ordering::SeqCst);
                return true;
            }

            if let Some(_) = self.processor.instruction_by_addr(addr) {
                self.current_addr.store(addr, Ordering::SeqCst);
                return true;
            }
        }

        for offset in (-16..0).rev() {
            let addr = addr.saturating_add_signed(offset);

            if let Some(_) = self.processor.error_by_addr(addr) {
                self.current_addr.store(addr, Ordering::SeqCst);
                return true;
            }

            if let Some(_) = self.processor.instruction_by_addr(addr) {
                self.current_addr.store(addr, Ordering::SeqCst);
                return true;
            }
        }

        false
    }

    pub fn scroll_up(&self, lines_to_scroll: usize) {
        let mut addr = self.current_addr.load(Ordering::Acquire);
        let mut lines_scrolled = 0;

        if lines_scrolled == lines_to_scroll {
            return;
        }

        loop {
            if let Some(_) = self.symbols.get_by_addr(addr) {
                self.line_offset.store(0, Ordering::SeqCst);
                lines_scrolled += 2;

                if lines_scrolled >= lines_to_scroll {
                    break;
                }
            }

            if let Some(err) = self.processor.error_by_addr(addr) {
                addr -= err.size();
                lines_scrolled += 1;

                if lines_scrolled >= lines_to_scroll {
                    break;
                }

                continue;
            }

            if let Some(instruction) = self.processor.instruction_by_addr(addr) {
                addr -= self.processor.instruction_width(&instruction);
                lines_scrolled += 1;

                if lines_scrolled >= lines_to_scroll {
                    break;
                }
            }
        }

        self.current_addr.store(addr, Ordering::Release);
    }

    pub fn scroll_down(&self, lines_to_scroll: usize) {
        let mut addr = self.current_addr.load(Ordering::Acquire);
        let mut lines_scrolled = 0;

        if lines_scrolled == lines_to_scroll {
            return;
        }

        loop {
            if let Some(_) = self.symbols.get_by_addr(addr) {
                self.line_offset.store(2, Ordering::SeqCst);
                lines_scrolled += 2;

                if lines_scrolled >= lines_to_scroll {
                    break;
                }
            }

            if let Some(err) = self.processor.error_by_addr(addr) {
                addr += err.size();
                lines_scrolled += 1;

                if lines_scrolled >= lines_to_scroll {
                    break;
                }

                continue;
            }

            if let Some(instruction) = self.processor.instruction_by_addr(addr) {
                addr += self.processor.instruction_width(&instruction);
                lines_scrolled += 1;

                if lines_scrolled >= lines_to_scroll {
                    break;
                }
            }
        }

        self.current_addr.store(addr, Ordering::Release);
    }

    // fn create_line(&self, lines: &mut Vec<Token>, line: Token) {
    // }

    pub fn listing(&self, row_count: usize) -> Vec<Token> {
        let mut addr = self.addr();
        let mut text: Vec<Token> = Vec::new();
        let mut row_count = row_count as isize + 1;
        let mut lines_to_skip = self.line_offset.load(Ordering::SeqCst) as isize;

        while row_count > 0 {
            if let Some(label) = self.symbols.get_by_addr(addr) {
                text.push(Token::from_str("\n<", colors::BLUE));
                for token in label.name() {
                    text.push(token.clone());
                }
                text.push(Token::from_str(">:\n", colors::BLUE));

                row_count -= 2;
            }

            text.push(Token::from_string(
                format!("{addr:0>10X}  "),
                colors::GRAY40,
            ));

            if let Some(err) = self.processor.error_by_addr(addr) {
                text.push(Token::from_string(
                    self.processor.format_bytes(addr, err.size()),
                    colors::GREEN,
                ));

                text.push(Token::from_str("<", colors::GRAY40));
                text.push(Token::from_string(format!("{:?}", err.kind), colors::RED));
                text.push(Token::from_str(">\n", colors::GRAY40));

                addr += err.size();
                row_count -= 1;
                continue;
            }

            if let Some(instruction) = self.processor.instruction_by_addr(addr) {
                let width = self.processor.instruction_width(&instruction);
                let stream = self.processor.instruction_stream(&instruction);

                text.push(Token::from_string(
                    self.processor.format_bytes(addr, width),
                    colors::GREEN,
                ));

                for token in stream.tokens() {
                    text.push(token.clone());
                }

                text.push(Token::from_str("\n", colors::WHITE));

                addr += width;
                row_count -= 1;
            }
        }

        if lines_to_skip == 0 {
            return text;
        }

        'tokens: for idx in 0..text.len() {
            // go through each line (token's can include multiple newline's)
            for jdx in 0..text[idx].text.len() {
                if text[idx].text.as_bytes()[jdx] == b'\n' {
                    lines_to_skip -= 1;

                    if lines_to_skip == 0 {
                        let first = Token::from_string(
                            text[idx].text[jdx..].to_string(),
                            text[idx].color
                        );

                        text = text[idx + 1..].to_vec();
                        text.insert(0, first);
                        break 'tokens;
                    }
                }
            }
        }

        text
    }

    pub fn section(&self) -> &str {
        self.processor
            .sections
            .iter()
            .find(|s| (s.start..=s.end).contains(&self.addr()))
            .map(|s| &s.name as &str)
            .unwrap()
    }

    pub fn functions(&self, range: std::ops::Range<usize>) -> Vec<Token> {
        let mut text: Vec<Token> = Vec::new();

        let lines_to_read = range.end - range.start;
        let lines = self
            .symbols
            .iter()
            .filter(|(_, func)| !func.intrinsic())
            .skip(range.start)
            .take(lines_to_read + 10);

        // for each instruction
        for (addr, symbol) in lines {
            text.push(Token::from_string(format!("{addr:0>10X}"), colors::WHITE));
            text.push(Token::from_str(" | ", colors::WHITE));

            if let Some(module) = symbol.module() {
                text.push(module);
                text.push(Token::from_str("!", Colors::brackets()));
            }

            for token in symbol.name() {
                text.push(token.clone());
            }

            text.push(Token::from_str("\n", colors::WHITE));
        }

        text
    }

    pub fn addr(&self) -> Addr {
        self.current_addr.load(Ordering::Relaxed)
    }

    pub fn parse<P: AsRef<std::path::Path>>(
        path: P,
        show_donut: Arc<AtomicBool>,
    ) -> Result<Self, DecodeError> {
        show_donut.store(true, Ordering::Relaxed);

        let now = std::time::Instant::now();

        let binary = std::fs::read(&path).map_err(DecodeError::IO)?;
        let obj = object::File::parse(&binary[..]).map_err(DecodeError::IncompleteObject)?;

        if obj.entry() == 0 {
            return Err(DecodeError::NotAnExecutable);
        }

        // TODO: refactor disassembly process to not just work on executables
        //       and handle all text sections of any object
        let entrypoint = obj.entry();
        log::notify!("[disassembly::parse] entrypoint {entrypoint:#X}.");

        let mut index = symbols::Index::new();

        index.parse_debug(&obj).map_err(DecodeError::IncompleteSymbolTable)?;
        index
            .parse_imports(&binary[..], &obj)
            .map_err(DecodeError::IncompleteImportTable)?;
        index.label();

        let mut processor = disassembler::Processor::new(obj.sections(), obj.architecture())
            .map_err(DecodeError::UnknownArchitecture)?;

        processor.recurse(&index);
        show_donut.store(false, Ordering::Relaxed);

        log::notify!(
            "[disassembly::parse] took {:#?} to parse {:?}.",
            now.elapsed(),
            path.as_ref()
        );

        Ok(Self {
            entrypoint: obj.entry() as usize,
            processor,
            symbols: index,
            current_addr: AtomicUsize::new(obj.entry() as usize),
            line_offset: AtomicUsize::new(0),
        })
    }
}
