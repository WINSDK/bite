//! Consumes decoder crates and provides an interface to interact with the decoders.
pub mod expr;
mod fmt;
mod processor;

use object::{Object, SectionKind};
use processor::Processor;
use tokenizing::{colors, Token};

type Addr = usize;

pub enum Error {
    IO(std::io::Error),
    NotAnExecutable,
    DecompressionFailed(object::Error),
    IncompleteObject(object::Error),
    IncompleteImportTable(object::Error),
    IncompleteSymbolTable(pdb::Error),
    UnknownArchitecture(object::Architecture),
}

#[derive(Debug)]
pub struct Section {
    /// Section identifier.
    pub name: String,

    /// What kind of data the section holds.
    pub kind: SectionKind,

    /// Uncompressed data.
    pub bytes: Vec<u8>,

    /// Address where section starts.
    pub start: Addr,

    /// Section start + length of uncompressed data.
    pub end: Addr,
}

impl Section {
    pub fn contains(&self, addr: Addr) -> bool {
        (self.start..=self.end).contains(&addr)
    }

    pub fn format_bytes(&self, addr: Addr, len: usize, max_len: usize) -> Option<String> {
        let rva = addr.checked_sub(self.start)?;
        let bytes = self.bytes.get(rva..)?;
        let bytes = &bytes[..std::cmp::min(bytes.len(), len)];

        Some(decoder::encode_hex_bytes_truncated(bytes, max_len))
    }
}

/// A singular address.
///
/// NOTE: `line_count` must be valid and `tokens` must not be empty.
struct Block {
    addr: Addr,
    tokens: Vec<Token>,
    line_count: usize,
}

impl Block {
    /// (Token offset, offset into token).
    fn with_offset(&self, mut line: usize) -> Option<(usize, usize)> {
        // line is out of range
        if line >= self.line_count {
            return None;
        }

        for (idx, token) in self.tokens.iter().enumerate() {
            for (jdx, chr) in token.text.bytes().enumerate() {
                if line == 0 {
                    return Some((idx, jdx));
                }

                if chr == b'\n' {
                    line -= 1;
                }
            }
        }

        None
    }
}

/// Window into a [`Disassembly`], just a reference essentially.
pub struct DisassemblyView {
    /// Address of the current block.
    addr: Addr,

    /// List of all lines.
    blocks: Vec<Block>,

    /// Block offset.
    block_offset: usize,

    /// Offset into block.
    block_line_offset: usize,

    /// Number of lines since start block.
    line_offset: usize,

    /// Number of lines that make up all blocks.
    line_count: usize,

    /// Line's being displayed.
    max_lines: usize,
}

impl DisassemblyView {
    pub fn new() -> Self {
        Self {
            addr: 0,
            blocks: Vec::new(),
            block_offset: 0,
            block_line_offset: 0,
            line_offset: 0,
            line_count: 0,
            max_lines: 0,
        }
    }

    pub fn addr(&self) -> Addr {
        self.addr
    }

    /// Jump to address, returning whether it succeeded.
    ///
    /// Try an address range of +- 32 bytes.
    pub fn jump(&mut self, disassembly: &Disassembly, addr: Addr) -> bool {
        let processor = &disassembly.processor;

        if !disassembly.sections().any(|s| s.contains(addr)) {
            return false;
        }

        for offset in 0..16 {
            let addr = addr.saturating_add_signed(offset);

            if let Some(_) = processor.error_by_addr(addr) {
                self.addr = addr;
                self.block_line_offset = 0;
                self.update(disassembly);
                return true;
            }

            if let Some(_) = processor.instruction_by_addr(addr) {
                self.addr = addr;
                self.block_line_offset = 0;
                self.update(disassembly);
                return true;
            }
        }

        for offset in (-16..0).rev() {
            let addr = addr.saturating_add_signed(offset);

            if let Some(_) = processor.error_by_addr(addr) {
                self.addr = addr;
                self.block_line_offset = 0;
                self.update(disassembly);
                return true;
            }

            if let Some(_) = processor.instruction_by_addr(addr) {
                self.addr = addr;
                self.block_line_offset = 0;
                self.update(disassembly);
                return true;
            }
        }

        // if we don't find any known items near the address, just jump to it
        // as there might be some bytes to inspect instead
        self.addr = addr;
        self.block_line_offset = 0;
        self.update(disassembly);
        true
    }

    /// Set's and update's the number of blocks if it changed.
    pub fn set_max_lines(&mut self, count: usize, disassembly: &Disassembly) {
        log::complex!(
            w "[disassembly::set_block_size] updating listing window to ",
            g count.to_string(),
            w " entries."
        );

        self.max_lines = count + count / 10; // FIXME: count isn't enough to fill window
        self.update(disassembly);
    }

    pub fn no_code(&self) -> bool {
        self.blocks.is_empty()
    }

    /// Read's a [`Block`] at `addr` and also returns how many bytes were read.
    fn read_block(&mut self, disassembly: &Disassembly, section: &Section, addr: Addr) -> usize {
        let mut tokens = Vec::new();
        let mut line_count = 0;

        let mut error = disassembly.processor.error_by_addr(addr);
        let mut instruction = disassembly.processor.instruction_by_addr(addr);
        let function = disassembly.symbols.get_by_addr(addr);

        // annotations require one line spacing
        if section.start == addr || function.is_some() {
            tokens.push(Token::from_str("\n", colors::WHITE));
            line_count += 1;
        }

        if section.start == addr {
            let text = format!("; section {}\n\n", section.name);
            let annotation = Token::from_string(text, colors::GRAY60);
            tokens.push(annotation);
            line_count += 2;
        } else if let Some(function) = disassembly.symbols.get_by_addr(addr) {
            tokens.push(Token::from_str("<", colors::BLUE));
            tokens.extend_from_slice(function.name());
            tokens.push(Token::from_str(">:\n", colors::BLUE));
            line_count += 1;
        }

        if let Some(err) = error {
            tokens.push(Token::from_string(
                format!("{addr:0>10X}  "),
                colors::GRAY40,
            ));

            tokens.push(Token::from_string(
                disassembly.processor.format_bytes(addr, err.size(), section).unwrap(),
                colors::GREEN,
            ));

            tokens.push(Token::from_str("<", colors::GRAY40));
            tokens.push(Token::from_string(format!("{:?}", err.kind), colors::RED));
            tokens.push(Token::from_str(">\n", colors::GRAY40));
            line_count += 1;

            self.line_count += line_count;
            self.blocks.push(Block {
                addr,
                tokens,
                line_count,
            });

            return err.size();
        }

        if let Some(instruction) = instruction {
            let width = disassembly.processor.instruction_width(&instruction);
            let instruction = disassembly.processor.instruction_tokens(&instruction);

            tokens.push(Token::from_string(
                format!("{addr:0>10X}  "),
                colors::GRAY40,
            ));

            tokens.push(Token::from_string(
                disassembly.processor.format_bytes(addr, width, section).unwrap(),
                colors::GREEN,
            ));

            tokens.extend(instruction);
            tokens.push(Token::from_str("\n", colors::WHITE));
            line_count += 1;

            self.line_count += line_count;
            self.blocks.push(Block {
                addr,
                tokens,
                line_count,
            });

            return width;
        }

        let mut dx = 1;
        loop {
            // if we've found 6 bytes
            if dx >= 6 {
                break;
            }

            // if we're past the sections end
            if addr + dx >= section.end {
                break;
            }

            // decode at new offset
            error = disassembly.processor.error_by_addr(addr + dx);
            instruction = disassembly.processor.instruction_by_addr(addr + dx);

            // if there is something at the current address
            if error.is_some() || instruction.is_some() {
                break;
            }

            dx += 1;
        }

        // if we have bytes left in the section to print
        if let Some(bytes) = disassembly.processor.format_bytes(addr, dx, section) {
            tokens.push(Token::from_string(
                format!("{addr:0>10X}  "),
                colors::GRAY40,
            ));

            tokens.push(Token::from_string(bytes, colors::GREEN));
            tokens.push(Token::from_str("\n", colors::WHITE));
            line_count += 1;
        }

        self.line_count += line_count;
        self.blocks.push(Block {
            addr,
            tokens,
            line_count,
        });

        dx
    }

    fn read_sections(&mut self, disassembly: &Disassembly) {
        let mut addr = self.addr;
        let mut sections = disassembly.sections();
        let mut section = sections.find(|s| s.contains(addr)).unwrap();

        loop {
            // if we've read enough lines
            if self.line_count >= self.max_lines {
                break;
            }

            // if we've read all the section's bytes
            if addr == section.end {
                match sections.next() {
                    // if there is a next section, start reading from there
                    Some(new_section) => {
                        addr = new_section.start;
                        section = new_section;
                        continue;
                    }
                    // else stop reading sections
                    None => break,
                };
            }

            let bytes_read = self.read_block(disassembly, section, addr);
            addr = match addr.checked_add(bytes_read) {
                Some(next_addr) => next_addr,
                None => break,
            };
        }
    }

    pub fn update(&mut self, disassembly: &Disassembly) {
        self.blocks.clear();
        self.block_offset = 0;
        self.line_count = 0;

        // check if address is unknown
        if !disassembly.sections().any(|s| s.contains(self.addr)) {
            // set address to the first section
            let section = disassembly.sections().next().unwrap();
            self.addr = section.start;
        }

        self.read_sections(disassembly);

        // set block offset to the first same addr
        self.block_offset = self.blocks.iter().position(|b| b.addr == self.addr).unwrap_or(0);
        self.addr = self.blocks.get(self.block_offset).map(|b| b.addr).unwrap_or(0);

        // set line offset to the lines read minus the first displayed block
        self.line_offset = self.blocks.iter().take(self.block_offset).map(|b| b.line_count).sum();
    }

    // NOTE: there is some bug here on cross-section boundaries
    //       where raw bytes aren't being shown accurately
    pub fn scroll_up(&mut self, disassembly: &Disassembly, mut lines_to_scroll: usize) {
        if self.blocks.is_empty() {
            return;
        }

        // we don't need to go up another item
        if lines_to_scroll <= self.block_line_offset {
            self.block_line_offset -= lines_to_scroll;
            return;
        }

        fn item_exists(disassembly: &Disassembly, addr: Addr) -> bool {
            let err = disassembly.processor.error_by_addr(addr);
            let inst = disassembly.processor.instruction_by_addr(addr);
            err.is_some() || inst.is_some()
        }

        while lines_to_scroll > 0 {
            // HACK: if there is something in the last 15 bytes (intel instruction max size)
            let mut dx = 1;
            while dx < 15 {
                let addr = self.addr.saturating_sub(dx);

                if item_exists(disassembly, addr) {
                    break;
                }

                dx += 1;
            }

            // nothing was found, go back only 6 bytes to display the bytes
            if dx == 15 {
                dx = 6;
            }

            self.addr = self.addr.saturating_sub(dx);
            self.update(disassembly);

            // if there is a next block
            let block = &self.blocks[0];

            if lines_to_scroll < block.line_count {
                self.block_line_offset = block.line_count - lines_to_scroll;
                self.update(disassembly);
                break;
            } else {
                lines_to_scroll -= block.line_count;
                self.block_line_offset = 0;
                self.update(disassembly);
            }
        }
    }

    pub fn scroll_down(&mut self, disassembly: &Disassembly, mut lines_to_scroll: usize) {
        if self.blocks.is_empty() {
            return;
        }

        // we don't need to go down another item
        if lines_to_scroll + self.block_line_offset < self.blocks[0].line_count {
            self.block_line_offset += lines_to_scroll;
            return;
        }

        while lines_to_scroll > 0 {
            // if there is a next block
            if let Some(block) = self.blocks.get(1) {
                self.addr = block.addr;

                if lines_to_scroll < block.line_count {
                    self.block_line_offset = lines_to_scroll - 1;
                    self.update(disassembly);
                    break;
                } else {
                    lines_to_scroll -= block.line_count;
                    self.block_line_offset = 0;
                    self.update(disassembly);
                }
            } else {
                break;
            }
        }
    }

    pub fn format(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut rows_to_add = self.max_lines;

        let block = &self.blocks[self.block_offset];
        let (token_offset, char_offset) = block.with_offset(self.block_line_offset).unwrap();

        tokens.push(Token::from_string(
            block.tokens[token_offset].text[char_offset..].to_string(),
            block.tokens[token_offset].color,
        ));

        for token in block.tokens[token_offset + 1..].iter() {
            tokens.push(token.clone());
        }

        for block in self.blocks.iter().skip(self.block_offset + 1) {
            for token in block.tokens.iter() {
                if rows_to_add == 0 {
                    break;
                }

                tokens.push(token.clone());

                let newlines = token.text.chars().filter(|&c| c == '\n').count();
                rows_to_add = rows_to_add.saturating_sub(newlines);
            }
        }

        tokens
    }
}

/// Everything necessary to display a ASM listing.
pub struct Disassembly {
    /// Where the binary is located.
    pub path: std::path::PathBuf,

    /// Where execution start.
    pub entrypoint: Addr,

    /// Address lookup for instructions.
    pub processor: Processor,

    /// Symbol lookup by absolute address.
    pub symbols: symbols::Index,
}

impl Disassembly {
    pub fn parse<P: AsRef<std::path::Path>>(path: P) -> Result<Self, Error> {
        let now = std::time::Instant::now();

        let binary = std::fs::read(&path).map_err(Error::IO)?;
        let obj = object::File::parse(&binary[..]).map_err(Error::IncompleteObject)?;

        if obj.entry() == 0 {
            return Err(Error::NotAnExecutable);
        }

        // TODO: refactor disassembly process to not just work on executables
        //       and handle all text sections of any object
        let entrypoint = obj.entry();

        log::complex!(
            w "[disassembly::parse] entrypoint ",
            g format!("{entrypoint:#X}"),
            w ".",
        );

        let mut index = symbols::Index::new();

        index.parse_debug(&obj).map_err(Error::IncompleteSymbolTable)?;
        index.parse_imports(&binary[..], &obj).map_err(Error::IncompleteImportTable)?;
        index.label();

        let mut processor = Processor::new(obj.sections(), obj.architecture())
            .map_err(Error::UnknownArchitecture)?;

        processor.recurse(&index);

        log::complex!(
            w "[disassembly::parse] took ",
            y format!("{:#?}", now.elapsed()),
            w " to parse ",
            w format!("{:?}.", path.as_ref())
        );

        Ok(Self {
            path: path.as_ref().to_path_buf(),
            entrypoint: obj.entry() as usize,
            processor,
            symbols: index,
        })
    }

    pub fn section_name(&self, addr: Addr) -> Option<&str> {
        self.processor
            .sections()
            .find(|s| (s.start..=s.end).contains(&addr))
            .map(|s| &s.name as &str)
    }

    fn sections(&self) -> impl DoubleEndedIterator<Item = &Section> {
        self.processor.sections()
    }

    pub fn functions(&self, range: std::ops::Range<usize>) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        let lines_to_read = range.end - range.start;
        let lines = self
            .symbols
            .iter()
            .filter(|(_, func)| !func.intrinsic())
            .skip(range.start)
            .take(lines_to_read + 10);

        // for each instruction
        for (addr, symbol) in lines {
            tokens.push(Token::from_string(format!("{addr:0>10X}"), colors::WHITE));
            tokens.push(Token::from_str(" | ", colors::WHITE));

            if let Some(module) = symbol.module() {
                tokens.push(module);
                tokens.push(Token::from_str("!", colors::GRAY60));
            }

            for token in symbol.name() {
                tokens.push(token.clone());
            }

            tokens.push(Token::from_str("\n", colors::WHITE));
        }

        tokens
    }
}

#[cfg(test)]
mod test {
    use super::Block;
    use tokenizing::Token;

    #[test]
    fn block_token_offsets() {
        let c = egui::Color32::BLACK;

        let block = Block {
            addr: 0,
            line_count: 1,
            tokens: vec![Token::from_str("\n", c)],
        };

        assert_eq!(block.with_offset(0), Some((0, 0)));

        let block = Block {
            addr: 0,
            line_count: 0,
            tokens: vec![Token::from_str("", c)],
        };

        assert_eq!(block.with_offset(1), None);

        let block = Block {
            addr: 0,
            line_count: 1,
            tokens: vec![Token::from_str("", c), Token::from_str("\n", c)],
        };

        assert_eq!(block.with_offset(0), Some((1, 0)));

        let block = Block {
            addr: 0,
            line_count: 1,
            tokens: vec![Token::from_str("a", c), Token::from_str("a\n", c)],
        };

        assert_eq!(block.with_offset(0), Some((0, 0)));

        let block = Block {
            addr: 0,
            line_count: 2,
            tokens: vec![Token::from_str("what\n", c), Token::from_str("a\n", c)],
        };

        assert_eq!(block.with_offset(1), Some((1, 0)));

        let block = Block {
            addr: 0,
            line_count: 3,
            tokens: vec![Token::from_str("wh\nat\n", c), Token::from_str("a\n", c)],
        };

        assert_eq!(block.with_offset(0), Some((0, 0)));
        assert_eq!(block.with_offset(1), Some((0, 3)));
        assert_eq!(block.with_offset(2), Some((1, 0)));
        assert_eq!(block.with_offset(3), None);

        let block = Block {
            addr: 0,
            line_count: 3,
            tokens: vec![Token::from_str("\n<\n", c), Token::from_str("abc>:\n", c)],
        };

        assert_eq!(block.with_offset(0), Some((0, 0)));
        assert_eq!(block.with_offset(1), Some((0, 1)));
        assert_eq!(block.with_offset(2), Some((1, 0)));

        let block = Block {
            addr: 0,
            line_count: 3,
            tokens: vec![
                Token::from_str("\n", c),
                Token::from_str("<function>:\n", c),
            ],
        };

        assert_eq!(block.with_offset(0), Some((0, 0)));
        assert_eq!(block.with_offset(1), Some((1, 0)));
        assert_eq!(block.with_offset(2), None);
    }
}
