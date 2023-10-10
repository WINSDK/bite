use object::Object;
use tokenizing::{colors, ColorScheme, Colors, Token};

use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};
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
            for jdx in 0..token.text.len() {
                if line == 0 {
                    return Some((idx, jdx));
                }

                if token.text.as_bytes()[jdx] == b'\n' {
                    line -= 1;
                }
            }
        }

        None
    }
}

pub struct DisassemblyView {
    pub addr: Addr,

    /// +- 50 addr
    blocks: Vec<Block>,

    /// Block offset.
    block_offset: usize,

    /// ???
    line_offset: usize,
}

impl DisassemblyView {
    pub fn new() -> Self {
        Self {
            addr: 0,
            blocks: Vec::new(),
            block_offset: 0,
            line_offset: 0,
        }
    }

    pub fn initialized(&self) -> bool {
        !self.blocks.is_empty()
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
                self.update(disassembly);
                return true;
            }

            if let Some(_) = processor.instruction_by_addr(addr) {
                self.addr = addr;
                self.update(disassembly);
                return true;
            }
        }

        for offset in (-16..0).rev() {
            let addr = addr.saturating_add_signed(offset);

            if let Some(_) = processor.error_by_addr(addr) {
                self.addr = addr;
                self.update(disassembly);
                return true;
            }

            if let Some(_) = processor.instruction_by_addr(addr) {
                self.addr = addr;
                self.update(disassembly);
                return true;
            }
        }

        false
    }

    pub fn update(&mut self, disassembly: &Disassembly) {
        let (first_addr, last_addr) = (
            disassembly.processor.sections[0].start,
            disassembly.processor.sections[disassembly.processor.sections.len() - 1].end,
        );

        let mut addr = self.addr;
        let mut addresses_read = 0usize;
        let mut blocks = Vec::with_capacity(100);
        let mut failed_searches = 0;

        // try to go backwards, reading 50 things
        while addresses_read < 50 && failed_searches < 1000 {
            // if there are less than 50 addresses before the current instruction, break
            if addr < first_addr {
                break;
            }

            let mut tokens = Vec::new();
            let mut found_something = false;
            let mut line_count = 0;

            if let Some(function) = disassembly.symbols.get_by_addr(addr) {
                tokens.push(Token::from_str("\n<", colors::BLUE));

                for token in function.name() {
                    tokens.push(token.clone());
                }

                tokens.push(Token::from_str(">:\n", colors::BLUE));

                line_count += 2;
            }

            if let Some(err) = disassembly.processor.error_by_addr(addr) {
                tokens.push(Token::from_string(
                    format!("{addr:0>10X}  "),
                    colors::GRAY40,
                ));

                tokens.push(Token::from_string(
                    disassembly.processor.format_bytes(addr, err.size()),
                    colors::GREEN,
                ));

                tokens.push(Token::from_str("<", colors::GRAY40));
                tokens.push(Token::from_string(format!("{:?}", err.kind), colors::RED));
                tokens.push(Token::from_str(">\n", colors::GRAY40));

                addr = addr.saturating_sub(err.size());
                found_something = true;
                line_count += 1;
            }

            if let Some(instruction) = disassembly.processor.instruction_by_addr(addr) {
                let width = disassembly.processor.instruction_width(&instruction);
                let stream = disassembly.processor.instruction_stream(&instruction);

                tokens.push(Token::from_string(
                    format!("{addr:0>10X}  "),
                    colors::GRAY40,
                ));

                tokens.push(Token::from_string(
                    disassembly.processor.format_bytes(addr, width),
                    colors::GREEN,
                ));

                for token in stream.tokens() {
                    tokens.push(token.clone());
                }

                tokens.push(Token::from_str("\n", colors::WHITE));

                addr = addr.saturating_sub(disassembly.processor.instruction_width(&instruction));
                found_something = true;
                line_count += 1;
            }

            if !found_something {
                // failed to read anything, try to go back 4 bytes
                addr = addr.saturating_sub(4);
                failed_searches += 1;
                continue;
            }

            // FIXME
            blocks.insert(
                0,
                Block {
                    addr,
                    tokens,
                    line_count,
                },
            );
            addresses_read += 1;
        }

        // the block offset is the first block read
        self.block_offset = addresses_read.saturating_sub(1);

        // reset block offset
        self.line_offset = 0;

        let mut failed_searches = 0;

        // try to go forward, reading 50 things
        while addresses_read < 100 && failed_searches < 1000 {
            // if there are less than 50 addresses before the current instruction, break
            if addr > last_addr {
                break;
            }

            let mut tokens = Vec::new();
            let mut found_something = false;
            let mut line_count = 0;

            if let Some(function) = disassembly.symbols.get_by_addr(addr) {
                tokens.push(Token::from_str("\n<", colors::BLUE));

                for token in function.name() {
                    tokens.push(token.clone());
                }

                tokens.push(Token::from_str(">:\n", colors::BLUE));

                line_count += 2;
            }

            if let Some(err) = disassembly.processor.error_by_addr(addr) {
                tokens.push(Token::from_string(
                    format!("{addr:0>10X}  "),
                    colors::GRAY40,
                ));

                tokens.push(Token::from_string(
                    disassembly.processor.format_bytes(addr, err.size()),
                    colors::GREEN,
                ));

                tokens.push(Token::from_str("<", colors::GRAY40));
                tokens.push(Token::from_string(format!("{:?}", err.kind), colors::RED));
                tokens.push(Token::from_str(">\n", colors::GRAY40));

                addr += err.size();
                found_something = true;
                line_count += 1;
            }

            if let Some(instruction) = disassembly.processor.instruction_by_addr(addr) {
                let width = disassembly.processor.instruction_width(&instruction);
                let stream = disassembly.processor.instruction_stream(&instruction);

                tokens.push(Token::from_string(
                    format!("{addr:0>10X}  "),
                    colors::GRAY40,
                ));

                tokens.push(Token::from_string(
                    disassembly.processor.format_bytes(addr, width),
                    colors::GREEN,
                ));

                for token in stream.tokens() {
                    tokens.push(token.clone());
                }

                tokens.push(Token::from_str("\n", colors::WHITE));

                addr += disassembly.processor.instruction_width(&instruction);
                found_something = true;
                line_count += 1;
            }

            if !found_something {
                // failed to read anything, try to go back 4 bytes
                addr += 4;
                failed_searches += 1;
                continue;
            }

            blocks.push(Block {
                addr,
                tokens,
                line_count,
            });

            addresses_read += 1;
        }

        self.blocks = blocks;
    }

    pub fn scroll_up(&mut self, mut lines_to_scroll: usize) {
        let mut block_offset = self.block_offset;
        let mut initial_offset = self.line_offset;
        let initial_block_offset = block_offset;

        loop {
            let block = &self.blocks[block_offset];
            let lines_left_in_block = block.line_count - initial_offset;

            // if our cursor fits in the block
            if lines_to_scroll < lines_left_in_block {
                let line_offset = block.line_count - 1 - lines_to_scroll;

                // if we're in the same block, just decrement the offset into the block
                if block_offset == initial_block_offset {
                    self.line_offset = line_offset - initial_offset;
                } else {
                    self.line_offset = line_offset;
                }

                self.block_offset = block_offset;
                self.addr = block.addr;
                return;
            }

            block_offset -= 1;
            lines_to_scroll -= lines_left_in_block;
            initial_offset = 0;
        }
    }

    pub fn scroll_down(&mut self, mut lines_to_scroll: usize) {
        let mut block_offset = self.block_offset;
        let mut initial_offset = self.line_offset;
        let initial_block_offset = block_offset;

        loop {
            let block = &self.blocks[block_offset];
            let lines_left_in_block = block.line_count - initial_offset;

            // if our cursor fits in the block
            if lines_to_scroll < lines_left_in_block {
                // if we're in the same block, just increment the offset into the block
                if block_offset == initial_block_offset {
                    self.line_offset = initial_offset + lines_to_scroll;
                } else {
                    self.line_offset = lines_to_scroll;
                }

                self.block_offset = block_offset;
                self.addr = block.addr;
                return;
            }

            block_offset += 1;
            lines_to_scroll -= lines_left_in_block;
            initial_offset = 0;
        }
    }

    pub fn to_tokens(&self, row_count: usize) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut rows_to_add = row_count;

        let block = &self.blocks[self.block_offset];
        let (token_offset, char_offset) = block.with_offset(self.line_offset).unwrap();

        tokens.push(Token::from_string(
            block.tokens[token_offset].text[char_offset..].to_string(),
            block.tokens[token_offset].color,
        ));

        for token in block.tokens.iter().skip(token_offset + 1) {
            tokens.push(token.clone());
        }

        for block in self.blocks.iter().skip(self.block_offset + 1) {
            for token in block.tokens.iter() {
                if rows_to_add == 0 {
                    return tokens;
                }

                let newlines = token.text.chars().filter(|&c| c == '\n').count();

                // if there are no newlines, don't do anything special
                if newlines == 0 {
                    tokens.push(token.clone());
                    continue;
                }

                // one newline is simple
                if newlines == 1 {
                    rows_to_add -= 1;
                    tokens.push(token.clone());
                    continue;
                }

                // if we aren't overflowing
                if newlines <= rows_to_add {
                    rows_to_add -= newlines;
                    tokens.push(token.clone());
                    continue;
                }

                let (idx, _) = token
                    .text
                    .char_indices()
                    .filter(|&(_, c)| c == '\n')
                    .skip(rows_to_add)
                    .next()
                    .unwrap();

                rows_to_add = 0;
                tokens.push(Token::from_string(
                    token.text[..idx].to_string(),
                    token.color,
                ));
            }
        }

        tokens
    }
}

pub struct Disassembly {
    /// Where execution start.
    pub entrypoint: Addr,

    /// Address lookup for instructions.
    pub processor: disassembler::Processor,

    /// Symbol lookup by absolute address.
    pub symbols: symbols::Index,
}

impl Disassembly {
    pub fn section(&self, addr: Addr) -> Option<&str> {
        self.processor
            .sections
            .iter()
            .find(|s| (s.start..=s.end).contains(&addr))
            .map(|s| &s.name as &str)
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
        })
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
