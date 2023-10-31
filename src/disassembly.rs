use object::{Object, SectionKind};
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

pub struct DisassemblyView {
    pub addr: Addr,

    blocks: Vec<Block>,

    /// Block offset.
    block_offset: usize,

    /// Offset into block.
    block_line_offset: usize,

    /// Number of lines since start block.
    line_offset: usize,

    /// Number of lines that make up all blocks.
    line_count: usize,

    max_lines: usize,
}

impl fmt::Debug for DisassemblyView {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DisassemblyView")
            .field("addr", &format_args!("{:#X}", self.addr))
            .field("block_offset", &self.block_offset)
            .field("block_line_offset", &self.block_line_offset)
            .field("line_offset", &self.line_offset)
            .field("line_count", &self.line_count)
            .field("max_lines", &self.max_lines)
            .finish()
    }
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

    fn first_visable_block(&self) -> &Block {
        &self.blocks[self.block_offset]
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
        if self.max_lines.abs_diff(count) > 1 {
            log::complex!(
                w "[disassembly::set_block_size] updating listing window to ",
                g count.to_string(),
                w " entries."
            );

            self.max_lines = count;
            self.update(disassembly);
        }
    }

    pub fn update(&mut self, disassembly: &Disassembly) {
        self.blocks.clear();
        self.block_offset = 0;
        self.line_count = 0;

        // range of text sections, can be incorrect because there may be data
        // sections between text sections
        let (first_addr, last_addr) = {
            let sections = &disassembly.processor.sections;
            let start = sections.iter().find(|s| s.kind == SectionKind::Text).unwrap().start;
            let end = sections.iter().rfind(|s| s.kind == SectionKind::Text).unwrap().end;

            (start, end)
        };

        let mut addr = self.addr.clamp(first_addr, last_addr);
        let mut lines_read = 0;

        // try to go backwards
        while lines_read < self.max_lines / 2 {
            // if there are less than `self.max_lines / 2` lines after
            // the current instruction, break
            if addr < first_addr {
                break;
            }

            let mut tokens = Vec::new();
            let mut found_something = false;
            let mut line_count = 0;
            let addr_of_block = addr;

            if let Some(function) = disassembly.symbols.get_by_addr(addr) {
                tokens.push(Token::from_str("\n<", colors::BLUE));
                tokens.extend_from_slice(function.name());
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
                let instruction = disassembly.processor.instruction_tokens(&instruction);

                tokens.push(Token::from_string(
                    format!("{addr:0>10X}  "),
                    colors::GRAY40,
                ));

                tokens.push(Token::from_string(
                    disassembly.processor.format_bytes(addr, width),
                    colors::GREEN,
                ));

                tokens.extend(instruction);
                tokens.push(Token::from_str("\n", colors::WHITE));

                addr = addr.saturating_sub(width);
                found_something = true;
                line_count += 1;
            }

            if !found_something {
                // failed to read anything, try to go back 4 bytes
                match addr.checked_sub(4) {
                    Some(new_addr) => {
                        addr = new_addr;
                        continue;
                    }
                    // break in case of underflow
                    None => break,
                };
            }

            // FIXME
            self.blocks.insert(
                0,
                Block {
                    addr: addr_of_block,
                    tokens,
                    line_count,
                },
            );

            self.block_offset += 1;
            lines_read += line_count;
        }

        // set line offset to the lines read minus the first displayed block
        self.line_offset = lines_read - self.blocks.last().map_or(0, |b| b.line_count);

        // set block offset to the first block read
        self.block_offset = self.block_offset.saturating_sub(1);

        // try to go forward
        while lines_read < self.max_lines {
            // if there are less than `self.max_lines` lines in total, break
            if addr > last_addr {
                break;
            }

            let mut tokens = Vec::new();
            let mut found_something = false;
            let mut line_count = 0;
            let addr_of_block = addr;

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
                let instruction = disassembly.processor.instruction_tokens(&instruction);

                tokens.push(Token::from_string(
                    format!("{addr:0>10X}  "),
                    colors::GRAY40,
                ));

                tokens.push(Token::from_string(
                    disassembly.processor.format_bytes(addr, width),
                    colors::GREEN,
                ));

                tokens.extend(instruction);
                tokens.push(Token::from_str("\n", colors::WHITE));

                addr += width;
                found_something = true;
                line_count += 1;
            }

            if !found_something {
                // failed to read anything, try to go forward 4 bytes
                addr += 4;
                continue;
            }

            self.blocks.push(Block {
                addr: addr_of_block,
                tokens,
                line_count,
            });

            lines_read += 1;
        }

        self.line_count = lines_read;
        self.addr = self.first_visable_block().addr;
        dbg!(&self);
    }

    pub fn scroll_up(&mut self, disassembly: &Disassembly, mut lines_to_scroll: usize) {
        let mut block_offset = self.block_offset;
        let mut initial_offset = self.block_line_offset;

        dbg!(&self);

        loop {
            let block = &self.blocks[block_offset];
            let lines_left_in_block = block.line_count - initial_offset;

            // if our cursor fits in the block
            if lines_to_scroll < lines_left_in_block {
                if block_offset == self.block_offset {
                    // if we're in the same block, just decrement the offset into the block
                    self.block_line_offset -= lines_to_scroll;
                } else {
                    self.block_line_offset = block.line_count - 1 - lines_to_scroll;
                }

                self.block_offset = block_offset;
                self.addr = block.addr;
                return;
            }

            block_offset -= 1;
            lines_to_scroll -= lines_left_in_block;
            initial_offset = 0;

            self.line_offset -= block.line_count;
        }
    }

    pub fn scroll_down(&mut self, disassembly: &Disassembly, mut lines_to_scroll: usize) {
        let block = self.first_visable_block();
        let lines_left_in_block = block.line_count - self.block_line_offset;

        if lines_to_scroll < lines_left_in_block {
            // if we're in the same block, just increment the offset into the block
            self.block_line_offset += lines_to_scroll;
            dbg!(&self);
            return;
        }

        loop {
            let block = self.first_visable_block();
            let lines_left_in_block = block.line_count - self.block_line_offset;

            // if our cursor fits in the block
            if lines_to_scroll < lines_left_in_block {
                self.addr = block.addr;
                self.block_line_offset = lines_to_scroll;
                dbg!(&self);
                return;
            }

            lines_to_scroll -= lines_left_in_block;

            self.line_offset += block.line_count;
            self.block_line_offset = 0;
        }
    }

    pub fn to_tokens(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut rows_to_add = self.max_lines / 2;

        let block = self.first_visable_block();
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
                    return tokens;
                }

                tokens.push(token.clone());

                let newlines = token.text.chars().filter(|&c| c == '\n').count();
                rows_to_add = rows_to_add.saturating_sub(newlines);
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

        log::complex!(
            w "[disassembly::parse] entrypoint ",
            g format!("{entrypoint:#X}"),
            w ".",
        );

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

        log::complex!(
            w "[disassembly::parse] took ",
            y format!("{:#?}", now.elapsed()),
            w " to parse ",
            w format!("{:?}.", path.as_ref())
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
