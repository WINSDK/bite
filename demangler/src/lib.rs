//! Symbol demangler for common mangling schemes.

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::sync::Arc;

use object::read::elf::{FileHeader, ElfFile};
use object::read::macho::{MachHeader, MachOFile};
use object::read::pe::{ImageNtHeaders, ImageThunkData, PeFile};
use object::BigEndian as BE;
use object::LittleEndian as LE;
use object::{BinaryFormat, Object, ObjectSymbol, ObjectSymbolTable};

use pdb::FallibleIterator;
use tokenizing::{Color, ColorScheme, Colors, Token};

pub mod itanium;
pub mod msvc;
pub mod rust;

fn parser(s: &str) -> TokenStream {
    // symbols without leading underscores are accepted as
    // dbghelp in windows strips them away

    // parse gnu/llvm Rust/C/C++ symbols
    if let Some(s) = itanium::parse(s) {
        return s;
    }

    // parse rust symbols that match the v0 mangling scheme
    if let Some(s) = rust::parse(s) {
        return s;
    }

    // parse windows msvc C/C++ symbols
    if let Some(s) = msvc::parse(s) {
        return s;
    }

    // return the original mangled symbol on failure
    TokenStream::simple(s)
}

#[derive(Debug)]
pub struct Index {
    tree: BTreeMap<usize, Arc<TokenStream>>,
}

impl Index {
    pub fn new() -> Self {
        Self {
            tree: BTreeMap::new(),
        }
    }

    fn pdb_file(obj: &object::File<'_>) -> Option<std::fs::File> {
        let pdb = obj.pdb_info().ok()??;
        let path = std::str::from_utf8(pdb.path()).ok()?;

        std::fs::File::open(path).ok()
    }

    pub fn parse_debug(&mut self, obj: &object::File<'_>) -> pdb::Result<()> {
        let mut symbols: Vec<(usize, &str)> = obj.symbols().filter_map(symbol_addr_name).collect();

        let base_addr = obj.relative_address_base() as usize;
        let pdb_table;

        if let Some(file) = Self::pdb_file(obj) {
            let mut pdb = pdb::PDB::open(file)?;

            // get symbol table
            pdb_table = pdb.global_symbols()?;

            // iterate through symbols collected earlier
            let mut symbol_table = pdb_table.iter();

            // retrieve addresses of symbols
            let address_map = pdb.address_map()?;

            while let Some(symbol) = symbol_table.next()? {
                let symbol = symbol.parse()?;

                let symbol = match symbol {
                    pdb::SymbolData::Public(symbol) if symbol.function => symbol,
                    _ => continue,
                };

                if let Some(addr) = symbol.offset.to_rva(&address_map) {
                    if let Ok(name) = std::str::from_utf8(symbol.name.as_bytes()) {
                        symbols.push((base_addr + addr.0 as usize, name));
                    }
                }
            }
        }

        // insert entrypoint into known symbols
        let entrypoint = obj.entry() as usize;
        println!("entrypoint {entrypoint:#x}");

        // insert entrypoint and override it if it's got a defined name
        self.tree.insert(entrypoint, Arc::new(TokenStream::simple("entry")));

        // insert defined symbols
        let map_symbol = |(addr, symbol): &(usize, &str)| (*addr, Arc::new(parser(symbol)));
        self.tree.extend(symbols.iter().map(map_symbol));

        Ok(())
    }

    pub fn parse_imports(&mut self, binary: &[u8], obj: &object::File<'_>) -> object::Result<()> {
        match obj.format() {
            BinaryFormat::Pe => {
                if obj.is_64() {
                    self.parse_pe_imports::<object::pe::ImageNtHeaders64>(binary)
                } else {
                    self.parse_pe_imports::<object::pe::ImageNtHeaders32>(binary)
                }
            }
            BinaryFormat::Elf => {
                if obj.is_64() {
                    if obj.is_little_endian() {
                        self.parse_elf_imports::<object::elf::FileHeader64<LE>>(binary)
                    } else {
                        self.parse_elf_imports::<object::elf::FileHeader64<BE>>(binary)
                    }
                } else {
                    if obj.is_little_endian() {
                        self.parse_elf_imports::<object::elf::FileHeader32<LE>>(binary)
                    } else {
                        self.parse_elf_imports::<object::elf::FileHeader32<BE>>(binary)
                    }
                }
            }
            BinaryFormat::MachO => {
                if obj.is_64() {
                    if obj.is_little_endian() {
                        self.parse_macho_imports::<object::macho::MachHeader64<LE>>(binary)
                    } else {
                        self.parse_macho_imports::<object::macho::MachHeader64<BE>>(binary)
                    }
                } else {
                    if obj.is_little_endian() {
                        self.parse_macho_imports::<object::macho::MachHeader32<LE>>(binary)
                    } else {
                        self.parse_macho_imports::<object::macho::MachHeader32<BE>>(binary)
                    }
                }
            }
            _ => Ok(()),
        }
    }

    fn parse_pe_imports<H: ImageNtHeaders>(&mut self, binary: &[u8]) -> object::Result<()> {
        let obj = PeFile::<H>::parse(&binary[..])?;

        if let Some(import_table) = obj.import_table()? {
            let mut import_descs = import_table.descriptors()?;
            while let Some(import_desc) = import_descs.next()? {
                // let library = import_table.name(import_desc.name.get(LE))?;
                // println!("library: {}", String::from_utf8_lossy(library));

                let first_thunk = import_desc.first_thunk.get(LE);
                let original_first_thunk = import_desc.original_first_thunk.get(LE);

                let thunk = if first_thunk == 0 {
                    original_first_thunk
                } else {
                    first_thunk
                };

                let mut import_addr_table = import_table.thunks(thunk)?;
                let mut func_rva = first_thunk;
                while let Some(func) = import_addr_table.next::<H>()? {
                    if !func.is_ordinal() {
                        let (hint, name) = match import_table.hint_name(func.address()) {
                            Ok(val) => val,
                            Err(..) => {
                                // skip over an entry
                                func_rva += std::mem::size_of::<H::ImageThunkData>() as u32;
                                continue;
                            }
                        };

                        let name = match std::str::from_utf8(name) {
                            Ok(name) => name,
                            Err(..) => {
                                // skip over an entry
                                func_rva += std::mem::size_of::<H::ImageThunkData>() as u32;
                                continue;
                            }
                        };

                        // `original_first_thunk` uses a `hint` into the export
                        // table whilst iterating thourhg regular `thunk`'s is
                        // a simple offset into the symbol export table
                        let addr = if thunk == original_first_thunk {
                            hint as usize
                        } else {
                            func_rva as usize
                        };

                        self.tree.insert(addr, Arc::new(parser(name)));
                    }

                    // skip over an entry
                    func_rva += std::mem::size_of::<H::ImageThunkData>() as u32;
                }
            }
        }

        Ok(())
    }

    fn parse_elf_imports<H: FileHeader>(&mut self, binary: &[u8]) -> object::Result<()> {
        let obj = ElfFile::<H>::parse(&binary[..])?;

        let relocations = match obj.dynamic_relocations() {
            Some(relocations) => relocations,
            None => return Ok(())
        };

        let dyn_syms = match obj.dynamic_symbol_table() {
            Some(dyn_syms) => dyn_syms,
            None => return Ok(())
        };

        for (addr, reloc) in relocations {
            if let object::read::RelocationTarget::Symbol(idx) = reloc.target() {
                if let Ok(name) = dyn_syms.symbol_by_index(idx).and_then(|sym| sym.name()) {
                    self.tree.insert(addr as usize, Arc::new(parser(name)));
                }
            }
        }

        Ok(())
    }

    fn parse_macho_imports<H: MachHeader>(&mut self, _binary: &[u8]) -> object::Result<()> {
        Ok(())
    }

    pub fn symbols(&self) -> std::collections::btree_map::Values<usize, Arc<TokenStream>> {
        self.tree.values()
    }

    pub fn is_empty(&self) -> bool {
        self.tree.is_empty()
    }

    pub fn get_by_addr(&self, addr: usize) -> Option<Arc<TokenStream>> {
        Some(Arc::clone(self.tree.get(&addr)?))
    }

    pub fn get_by_addr_ref(&self, addr: usize) -> Option<&TokenStream> {
        self.tree.get(&addr).map(Arc::as_ref)
    }
}

fn symbol_addr_name<'sym>(symbol: object::Symbol<'sym, 'sym>) -> Option<(usize, &'sym str)> {
    if let Ok(name) = symbol.name() {
        return Some((symbol.address() as usize, name));
    }

    None
}

#[derive(Debug)]
pub struct TokenStream {
    /// Unmovable string which the [Token]'s have a pointer to.
    inner: std::pin::Pin<String>,

    /// Internal token representation which is unsafe to access outside of calling [Self::tokens].
    tokens: Vec<Token<'static>>,
}

impl TokenStream {
    pub fn new(s: &str) -> Self {
        Self {
            inner: std::pin::Pin::new(s.to_string()),
            tokens: Vec::with_capacity(128),
        }
    }

    pub fn simple(s: &str) -> Self {
        let mut this = Self {
            inner: std::pin::Pin::new(s.to_string()),
            tokens: Vec::with_capacity(1),
        };

        this.tokens.push(Token {
            text: Cow::Borrowed(this.inner()),
            color: Colors::item(),
        });

        this
    }

    /// SAFETY: must downcast &'static str to a lifetime that matches the lifetime of self.
    #[inline]
    pub fn inner<'a>(&self) -> &'a str {
        unsafe { std::mem::transmute(self.inner.as_ref()) }
    }

    #[inline]
    pub fn push(&mut self, text: &'static str, color: &'static Color) {
        self.tokens.push(Token {
            text: Cow::Borrowed(text),
            color,
        })
    }

    #[inline]
    pub fn push_cow(&mut self, text: Cow<'static, str>, color: &'static Color) {
        self.tokens.push(Token { text, color })
    }

    #[inline]
    pub fn pop(&mut self) {
        self.tokens.pop();
    }

    #[inline]
    pub fn tokens(&self) -> &[Token<'static>] {
        self.tokens.as_slice()
    }
}
