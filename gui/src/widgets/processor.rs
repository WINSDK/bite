use processor::Processor;
use processor_shared::{PhysAddr, Section};
use tokenizing::{colors, Token};

/// Window into a [`Processor`], just a reference essentially.
pub struct ProcessorView {
    /// Address of the current block.
    addr: PhysAddr,

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

impl ProcessorView {
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

    pub fn addr(&self) -> PhysAddr {
        self.addr
    }

    /// Jump to address, returning whether it succeeded.
    ///
    /// Try an address range of +- 32 bytes.
    pub fn jump(&mut self, processor: &Processor, addr: PhysAddr) -> bool {
        if !processor.sections().any(|s| s.contains(addr)) {
            return false;
        }

        for offset in 0..16 {
            let addr = addr.saturating_add_signed(offset);

            if let Some(_) = processor.error_by_addr(addr) {
                self.addr = addr;
                self.block_line_offset = 0;
                self.update(processor);
                return true;
            }

            if let Some(_) = processor.instruction_by_addr(addr) {
                self.addr = addr;
                self.block_line_offset = 0;
                self.update(processor);
                return true;
            }
        }

        for offset in (-16..0).rev() {
            let addr = addr.saturating_add_signed(offset);

            if let Some(_) = processor.error_by_addr(addr) {
                self.addr = addr;
                self.block_line_offset = 0;
                self.update(processor);
                return true;
            }

            if let Some(_) = processor.instruction_by_addr(addr) {
                self.addr = addr;
                self.block_line_offset = 0;
                self.update(processor);
                return true;
            }
        }

        // if we don't find any known items near the address, just jump to it
        // as there might be some bytes to inspect instead
        self.addr = addr;
        self.block_line_offset = 0;
        self.update(processor);
        true
    }

    /// Set's and update's the number of blocks if it changed.
    pub fn set_max_lines(&mut self, count: usize, processor: &Processor) {
        // log::complex!(
        //     w "[processor::set_block_size] updating listing window to ",
        //     g count.to_string(),
        //     w " entries."
        // );

        self.max_lines = count + count / 10; // FIXME: count isn't enough to fill window
        self.update(processor);
    }

    pub fn no_code(&self) -> bool {
        self.blocks.is_empty()
    }

    /// Read's a [`Block`] at `addr` and also returns how many bytes were read.
    fn read_block(&mut self, processor: &Processor, section: &Section, addr: PhysAddr) -> usize {
        let mut tokens = Vec::new();
        let mut line_count = 0;

        let mut error = processor.error_by_addr(addr);
        let mut instruction = processor.instruction_by_addr(addr);
        let function = processor.index.get_func_by_addr(addr);

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
        }

        if let Some(function) = function {
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
                processor.format_bytes(addr, err.size(), section, true).unwrap(),
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
            let width = processor.instruction_width(instruction);
            let instruction = processor.instruction_tokens(instruction, &processor.index);

            tokens.push(Token::from_string(
                format!("{addr:0>10X}  "),
                colors::GRAY40,
            ));

            tokens.push(Token::from_string(
                processor.format_bytes(addr, width, section, true).unwrap(),
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
            error = processor.error_by_addr(addr + dx);
            instruction = processor.instruction_by_addr(addr + dx);

            // if there is something at the current address
            if error.is_some() || instruction.is_some() {
                break;
            }

            dx += 1;
        }

        // if we have bytes left in the section to print
        if let Some(bytes) = processor.format_bytes(addr, dx, section, false) {
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

    fn read_sections(&mut self, processor: &Processor) {
        let mut addr = self.addr;
        let mut sections = processor.sections();
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

            let bytes_read = self.read_block(processor, section, addr);
            addr = match addr.checked_add(bytes_read) {
                Some(next_addr) => next_addr,
                None => break,
            };
        }
    }

    pub fn update(&mut self, processor: &Processor) {
        self.blocks.clear();
        self.block_offset = 0;
        self.line_count = 0;

        // check if address is unknown
        if !processor.sections().any(|s| s.contains(self.addr)) {
            // set address to the first section
            let section = processor.sections().next().unwrap();
            self.addr = section.start;
        }

        self.read_sections(processor);

        // set block offset to the first same addr
        self.block_offset = self.blocks.iter().position(|b| b.addr == self.addr).unwrap_or(0);
        self.addr = self.blocks.get(self.block_offset).map(|b| b.addr).unwrap_or(0);

        // set line offset to the lines read minus the first displayed block
        self.line_offset = self.blocks.iter().take(self.block_offset).map(|b| b.line_count).sum();
    }

    // NOTE: there is some bug here on cross-section boundaries
    //       where raw bytes aren't being shown accurately
    pub fn scroll_up(&mut self, processor: &Processor, mut lines_to_scroll: usize) {
        if self.blocks.is_empty() {
            return;
        }

        // we don't need to go up another item
        if lines_to_scroll <= self.block_line_offset {
            self.block_line_offset -= lines_to_scroll;
            return;
        }

        fn item_exists(processor: &Processor, addr: PhysAddr) -> bool {
            let err = processor.error_by_addr(addr);
            let inst = processor.instruction_by_addr(addr);
            err.is_some() || inst.is_some()
        }

        while lines_to_scroll > 0 {
            // HACK: if there is something in the last 15 bytes (intel instruction max size)
            let mut dx = 1;
            while dx < 15 {
                let addr = self.addr.saturating_sub(dx);

                if item_exists(processor, addr) {
                    break;
                }

                dx += 1;
            }

            // nothing was found, go back only 6 bytes to display the bytes
            if dx == 15 {
                dx = 6;
            }

            self.addr = self.addr.saturating_sub(dx);
            self.update(processor);

            // if there is a next block
            let block = &self.blocks[0];

            if lines_to_scroll < block.line_count {
                self.block_line_offset = block.line_count - lines_to_scroll;
                self.update(processor);
                break;
            } else {
                lines_to_scroll -= block.line_count;
                self.block_line_offset = 0;
                self.update(processor);
            }
        }
    }

    pub fn scroll_down(&mut self, processor: &Processor, mut lines_to_scroll: usize) {
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
                    self.update(processor);
                    break;
                } else {
                    lines_to_scroll -= block.line_count;
                    self.block_line_offset = 0;
                    self.update(processor);
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

/// A singular address.
/// Important detail is that `line_count` must be valid and `tokens` must not be empty.
struct Block {
    addr: PhysAddr,
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

#[cfg(test)]
mod tests {
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
