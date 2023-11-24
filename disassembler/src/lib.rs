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
    pub name: String,
    pub kind: SectionKind,
    pub bytes: Vec<u8>,
    pub start: Addr,
    pub end: Addr,
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

    /// Line's being displayed * 2.
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

        false
    }

    /// Set's and update's the number of blocks if it changed.
    pub fn set_max_lines(&mut self, count: usize, disassembly: &Disassembly) {
        // sometimes scrolling less than one line will still cause a shift in the `row_count`
        // this prevents that from changing anything
        if self.max_lines.abs_diff(count) > 2 {
            log::complex!(
                w "[disassembly::set_block_size] updating listing window to ",
                g count.to_string(),
                w " entries."
            );

            self.max_lines = count;
            self.update(disassembly);
        }
    }

    pub fn no_code(&self) -> bool {
        self.blocks.is_empty()
    }

    fn read_within_section(&mut self, section: &Section, disassembly: &Disassembly) {
        // try to go backwards
        while self.line_count < self.max_lines / 2 {
            // if there are less than `self.max_lines` lines before
            // the current instruction, break
            if self.addr < section.start {
                break;
            }

            let mut tokens = Vec::new();
            let mut line_count = 0;

            let error = disassembly.processor.error_by_addr(self.addr);
            let instruction = disassembly.processor.instruction_by_addr(self.addr);
            let addr_of_block = self.addr;

            // if there is something at this address
            if error.is_some() || instruction.is_some() {
                if let Some(function) = disassembly.symbols.get_by_addr(self.addr) {
                    tokens.push(Token::from_str("\n<", colors::BLUE));
                    tokens.extend_from_slice(function.name());
                    tokens.push(Token::from_str(">:\n", colors::BLUE));

                    line_count += 2;
                }
            } else {
                match disassembly.processor.prev_item(self.addr) {
                    Some(addr) => {
                        self.addr = addr;
                        continue;
                    },
                    None => break
                }
            }

            if let Some(err) = error {
                tokens.push(Token::from_string(
                    format!("{:0>10X}  ", self.addr),
                    colors::GRAY40,
                ));

                tokens.push(Token::from_string(
                    disassembly.processor.format_bytes(self.addr, err.size()),
                    colors::GREEN,
                ));

                tokens.push(Token::from_str("<", colors::GRAY40));
                tokens.push(Token::from_string(format!("{:?}", err.kind), colors::RED));
                tokens.push(Token::from_str(">\n", colors::GRAY40));

                self.addr = self.addr.saturating_sub(err.size());

                line_count += 1;
            }

            if let Some(instruction) = instruction {
                let width = disassembly.processor.instruction_width(&instruction);
                let instruction = disassembly.processor.instruction_tokens(&instruction);

                tokens.push(Token::from_string(
                    format!("{:0>10X}  ", self.addr),
                    colors::GRAY40,
                ));

                tokens.push(Token::from_string(
                    disassembly.processor.format_bytes(self.addr, width),
                    colors::GREEN,
                ));

                tokens.extend(instruction);
                tokens.push(Token::from_str("\n", colors::WHITE));

                self.addr = self.addr.saturating_sub(width);
                line_count += 1;
            }

            self.blocks.push(Block {
                addr: addr_of_block,
                tokens,
                line_count,
            });

            self.line_count += line_count;
        }

        // try to go forward
        while self.line_count < self.max_lines {
            // if there are less than `self.max_lines` lines in total, break
            if self.addr > section.end {
                break;
            }

            let mut tokens = Vec::new();
            let mut line_count = 0;

            let error = disassembly.processor.error_by_addr(self.addr);
            let instruction = disassembly.processor.instruction_by_addr(self.addr);
            let addr_of_block = self.addr;

            // if there is something at this address
            if error.is_some() || instruction.is_some() {
                if let Some(function) = disassembly.symbols.get_by_addr(self.addr) {
                    tokens.push(Token::from_str("\n<", colors::BLUE));
                    tokens.extend_from_slice(function.name());
                    tokens.push(Token::from_str(">:\n", colors::BLUE));

                    line_count += 2;
                }
            } else {
                match disassembly.processor.next_item(self.addr) {
                    Some(addr) => {
                        self.addr = addr;
                        continue;
                    },
                    None => break
                }
            }

            if let Some(err) = error {
                tokens.push(Token::from_string(
                    format!("{:0>10X}  ", self.addr),
                    colors::GRAY40,
                ));

                tokens.push(Token::from_string(
                    disassembly.processor.format_bytes(self.addr, err.size()),
                    colors::GREEN,
                ));

                tokens.push(Token::from_str("<", colors::GRAY40));
                tokens.push(Token::from_string(format!("{:?}", err.kind), colors::RED));
                tokens.push(Token::from_str(">\n", colors::GRAY40));

                self.addr += err.size();
                line_count += 1;
            }

            if let Some(instruction) = instruction {
                let width = disassembly.processor.instruction_width(&instruction);
                let instruction = disassembly.processor.instruction_tokens(&instruction);

                tokens.push(Token::from_string(
                    format!("{:0>10X}  ", self.addr),
                    colors::GRAY40,
                ));

                tokens.push(Token::from_string(
                    disassembly.processor.format_bytes(self.addr, width),
                    colors::GREEN,
                ));

                tokens.extend(instruction);
                tokens.push(Token::from_str("\n", colors::WHITE));

                self.addr += width;
                line_count += 1;
            }

            self.blocks.push(Block {
                addr: addr_of_block,
                tokens,
                line_count,
            });

            self.line_count += line_count;
        }
    }

    pub fn update(&mut self, disassembly: &Disassembly) {
        self.blocks.clear();
        self.block_offset = 0;
        self.line_count = 0;

        let text_sections =
            || disassembly.processor.sections().filter(|s| s.kind == SectionKind::Text);

        // check if address is unknown
        if !text_sections().any(|s| (s.start..s.end).contains(&self.addr)) {
            // self.addr = disassembly.entrypoint;
            let first = disassembly.processor.sections().min_by_key(|s| s.start).unwrap();
            self.addr = first.start;
        }

        for section in text_sections() {
            self.read_within_section(section, disassembly);
        }

        // set block offset to the first same addr
        self.block_offset = self.blocks.iter().position(|b| b.addr == self.addr).unwrap_or(0);
        self.addr = self.blocks.get(self.block_offset).map(|b| b.addr).unwrap_or(0);

        // set line offset to the lines read minus the first displayed block
        self.line_offset = self.blocks.iter().take(self.block_offset).map(|b| b.line_count).sum();
    }

    pub fn scroll_up(&mut self, _disassembly: &Disassembly, mut lines_to_scroll: usize) {
        // create new block's if out of bound
        // if lines_to_scroll >= self.line_offset {
        //     self.update(disassembly);
        // }

        while lines_to_scroll > 0 {
            let remaining_lines = self.block_line_offset + 1;

            // check if we're at the final block (first line)
            if lines_to_scroll < remaining_lines {
                self.block_line_offset -= lines_to_scroll;
                self.line_offset -= lines_to_scroll;
                break;
            }

            lines_to_scroll -= remaining_lines;

            if self.block_offset > 0 {
                self.block_offset -= 1;
                self.block_line_offset = self.blocks[self.block_offset].line_count - 1;
            } else {
                self.block_line_offset = 0;
            }

            self.line_offset -= remaining_lines;
        }

        self.addr = self.blocks[self.block_offset].addr;
        // dbg!(&self);
    }

    pub fn scroll_down(&mut self, _disassembly: &Disassembly, mut lines_to_scroll: usize) {
        // create new block's if out of bound
        // if self.line_offset * 2 + lines_to_scroll >= self.line_count {
        //     self.update(disassembly);
        // }

        while lines_to_scroll > 0 {
            let block = &self.blocks[self.block_offset];
            let remaining_lines = block.line_count - self.block_line_offset;

            // check if we're at the final block (first line)
            if lines_to_scroll < remaining_lines {
                self.block_line_offset += lines_to_scroll;
                self.line_offset += lines_to_scroll;
                break;
            }

            lines_to_scroll -= remaining_lines;

            self.block_offset += 1;
            self.block_line_offset = 0;
            self.line_offset += remaining_lines;
        }

        self.addr = self.blocks[self.block_offset].addr;
        // dbg!(&self);
    }

    pub fn format(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut rows_to_add = self.max_lines / 2;

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

    pub fn section(&self, addr: Addr) -> Option<&str> {
        self.processor
            .sections()
            .find(|s| (s.start..=s.end).contains(&addr))
            .map(|s| &s.name as &str)
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
