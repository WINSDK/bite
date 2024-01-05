//! Symbol demangler for common mangling schemes.

use std::sync::Arc;

use object::elf::{R_X86_64_COPY, R_X86_64_GLOB_DAT, R_X86_64_JUMP_SLOT};
use object::endian::Endian;
use object::read::elf::{ElfFile, FileHeader};
use object::read::macho::MachHeader;
use object::read::pe::{ImageNtHeaders, ImageThunkData, PeFile};
use object::BigEndian as BE;
use object::LittleEndian as LE;
use object::{
    BinaryFormat, Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationKind,
};

use pdb::FallibleIterator;
use radix_trie::{Trie, TrieCommon, TrieKey};
use tokenizing::{Color, ColorScheme, Colors, Token};

pub mod itanium;
pub mod msvc;
pub mod rust;
pub mod rust_legacy;

fn parser(s: &str) -> TokenStream {
    // symbols without leading underscores are accepted as
    // dbghelp in windows strips them away

    let s = s.strip_suffix("$got").unwrap_or(s);
    let s = s.strip_suffix("$plt").unwrap_or(s);
    let s = s.strip_suffix("$pltgot").unwrap_or(s);

    // parse rust symbols
    if let Some(s) = rust_legacy::parse(s) {
        return s;
    }

    // parse gnu/llvm/C/C++ symbols
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

fn parallel_compute<In, Out, F>(items: Vec<In>, output: &mut Vec<Out>, transformer: F)
where
    F: FnOnce(&In) -> Out,
    F: Send + Copy,
    In: Sync,
    Out: Send + Sync,
{
    let thread_count = std::thread::available_parallelism().unwrap().get();

    // for small item counts, perform single-threaded
    if items.len() < thread_count {
        for item in items.iter() {
            output.push(transformer(item));
        }

        return;
    }

    // multithreaded
    std::thread::scope(|s| {
        let mut chunks = items.chunks(items.len() / thread_count);
        let mut threads = Vec::with_capacity(thread_count);

        while let Some(chunk) = chunks.next() {
            let thread = s.spawn(move || {
                let mut result = Vec::with_capacity(chunk.len());
                for item in chunk {
                    result.push(transformer(item));
                }
                result
            });

            threads.push(thread);
        }

        for thread in threads {
            let chunk = thread.join().unwrap();
            output.extend(chunk);
        }
    });
}

#[derive(Debug)]
pub struct Function {
    name: TokenStream,
    name_as_str: ArcStr,
    module: Option<String>,
    is_intrinsics: bool,
}

fn is_name_an_intrinsic(name: &str) -> bool {
    if name.is_empty() {
        return true;
    }

    if name.starts_with("GCC_except_table") {
        return true;
    }

    if name.starts_with("str.") {
        return true;
    }

    if name.starts_with(".L") {
        return true;
    }

    if name.starts_with("anon.") {
        return true;
    }

    false
}

impl Function {
    pub fn new(name: TokenStream, module: Option<String>) -> Self {
        let is_intrinsics = is_name_an_intrinsic(name.inner());
        let name_as_str = String::from_iter(name.tokens().iter().map(|t| &t.text[..]));
        let name_as_str = ArcStr {
            inner: Arc::from(name_as_str),
        };

        Self {
            name_as_str,
            name,
            module,
            is_intrinsics,
        }
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.name_as_str
    }

    #[inline]
    pub fn name(&self) -> &[Token] {
        self.name.tokens.as_slice()
    }

    #[inline]
    pub fn module(&self) -> Option<&str> {
        self.module.as_deref()
    }

    /// Is the function a unnamed compiler generated artifact.
    #[inline]
    pub fn intrinsic(&self) -> bool {
        self.is_intrinsics
    }
}

impl PartialEq for Function {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.name_as_str == other.name_as_str
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ArcStr {
    inner: Arc<str>,
}

impl std::ops::Deref for ArcStr {
    type Target = Arc<str>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl TrieKey for ArcStr {
    #[inline]
    fn encode_bytes(&self) -> Vec<u8> {
        self.as_bytes().encode_bytes()
    }
}

#[derive(Debug)]
pub struct Index {
    /// Mapping from address starting at the header base to functions.
    tree: Vec<(usize, Arc<Function>)>,

    trie: Trie<ArcStr, Arc<Function>>,

    /// Number of named compiler artifacts.
    named_len: usize,
}

impl Index {
    pub fn new() -> Self {
        Self {
            tree: Vec::new(),
            trie: Trie::new(),
            named_len: 0,
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
        let entry_func = Function::new(TokenStream::simple("entry"), None);

        log::PROGRESS.set("Demangling symbols", symbols.len());

        // insert defined symbols
        parallel_compute(symbols, &mut self.tree, |(addr, symbol)| {
            let func = Function::new(parser(symbol), None);
            log::PROGRESS.step();
            (*addr, Arc::new(func))
        });

        // count the number of function's that aren't compiler intrinsics
        self.named_len = self.tree.iter().filter(|(_, func)| func.is_intrinsics).count();

        // only keep one symbol per address
        self.tree.dedup_by_key(|(addr, _)| *addr);

        // only keep symbols with a non-empty name
        self.tree.retain(|(_, func)| !func.as_str().is_empty());

        // insert entrypoint
        self.insert(entrypoint, entry_func);

        // keep tree sorted so it can be binary searched
        self.tree.sort_unstable_by_key(|(addr, _)| *addr);

        log::PROGRESS.set("Building prefix tree", self.tree.len());

        // radix-prefix tree for fast lookups
        for (_, func) in self.tree.iter() {
            self.trie.insert(func.name_as_str.clone(), func.clone());
            log::PROGRESS.step();
        }

        log::complex!(
            w "[index::parse_debug] found ",
            g self.tree.len().to_string(),
            w " symbols."
        );

        Ok(())
    }

    pub fn parse_imports(&mut self, binary: &[u8], obj: &object::File<'_>) -> object::Result<()> {
        match obj.format() {
            BinaryFormat::Pe => {
                if obj.is_64() {
                    self.parse_pe_imports::<object::pe::ImageNtHeaders64>(binary)?
                } else {
                    self.parse_pe_imports::<object::pe::ImageNtHeaders32>(binary)?
                }
            }
            BinaryFormat::Elf => {
                if obj.is_64() {
                    if obj.is_little_endian() {
                        self.parse_elf_imports::<object::elf::FileHeader64<LE>>(binary)?
                    } else {
                        self.parse_elf_imports::<object::elf::FileHeader64<BE>>(binary)?
                    }
                } else {
                    if obj.is_little_endian() {
                        self.parse_elf_imports::<object::elf::FileHeader32<LE>>(binary)?
                    } else {
                        self.parse_elf_imports::<object::elf::FileHeader32<BE>>(binary)?
                    }
                }
            }
            BinaryFormat::MachO => {
                if obj.is_64() {
                    if obj.is_little_endian() {
                        self.parse_macho_imports::<object::macho::MachHeader64<LE>>(binary)?
                    } else {
                        self.parse_macho_imports::<object::macho::MachHeader64<BE>>(binary)?
                    }
                } else {
                    if obj.is_little_endian() {
                        self.parse_macho_imports::<object::macho::MachHeader32<LE>>(binary)?
                    } else {
                        self.parse_macho_imports::<object::macho::MachHeader32<BE>>(binary)?
                    }
                }
            }
            _ => {}
        };

        self.tree.sort_unstable_by_key(|k| k.0);
        Ok(())
    }

    fn parse_pe_imports<H: ImageNtHeaders>(&mut self, binary: &[u8]) -> object::Result<()> {
        let obj = PeFile::<H>::parse(binary)?;

        if let Some(import_table) = obj.import_table()? {
            let mut import_descs = import_table.descriptors()?;
            while let Some(import_desc) = import_descs.next()? {
                let module = import_table.name(import_desc.name.get(LE))?;
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
                        let phys_addr = if thunk == original_first_thunk {
                            hint as u64 + obj.relative_address_base()
                        } else {
                            func_rva as u64 + obj.relative_address_base()
                        };

                        let module = String::from_utf8_lossy(module);
                        let module = module.strip_prefix(".dll").unwrap_or(&module).to_owned();
                        let func = Function::new(parser(name), Some(module));

                        self.insert(phys_addr as usize, func);
                    }

                    // skip over an entry
                    func_rva += std::mem::size_of::<H::ImageThunkData>() as u32;
                }
            }
        }

        Ok(())
    }

    fn parse_elf_imports<H: FileHeader>(&mut self, binary: &[u8]) -> object::Result<()> {
        let obj = ElfFile::<H>::parse(binary)?;

        let relocations = match obj.dynamic_relocations() {
            Some(relocations) => relocations,
            None => return Ok(()),
        };

        let dyn_syms = match obj.dynamic_symbol_table() {
            Some(dyn_syms) => dyn_syms,
            None => return Ok(()),
        };

        for (r_offset, reloc) in relocations {
            if let object::read::RelocationTarget::Symbol(idx) = reloc.target() {
                let opt_section = obj.sections().find(|section| {
                    (section.address()..section.address() + section.size()).contains(&r_offset)
                });

                let section = match opt_section {
                    Some(section) => section,
                    None => continue,
                };

                if let Ok(sym) = dyn_syms.symbol_by_index(idx) {
                    let name = match sym.name() {
                        Ok(name) => name,
                        Err(..) => continue,
                    };

                    let phys_addr = match reloc.kind() {
                        // hard-coded address to function which doesn't require a relocation
                        RelocationKind::Absolute => r_offset as usize,
                        RelocationKind::Elf(R_X86_64_GLOB_DAT) => r_offset as usize,
                        RelocationKind::Elf(R_X86_64_COPY) => r_offset as usize,
                        // address in .got.plt section which contains an address to the function
                        RelocationKind::Elf(R_X86_64_JUMP_SLOT) => {
                            let width = if obj.is_64() { 8 } else { 4 };

                            let bytes = match section.data_range(r_offset, width) {
                                Ok(Some(bytes)) => bytes,
                                _ => continue,
                            };

                            let phys_addr = if obj.is_64() {
                                obj.endian().read_u64_bytes(bytes.try_into().unwrap()) as usize
                            } else {
                                obj.endian().read_u32_bytes(bytes.try_into().unwrap()) as usize
                            };

                            // idk why we need this
                            phys_addr.saturating_sub(6)
                        }
                        _ => continue,
                    };

                    // TODO: find modules
                    let func = Function::new(parser(name), None);
                    self.insert(phys_addr, func);
                }
            }
        }

        Ok(())
    }

    fn parse_macho_imports<H: MachHeader>(&mut self, _binary: &[u8]) -> object::Result<()> {
        Ok(())
    }

    pub fn symbols(&self) -> impl Iterator<Item = &Arc<Function>> {
        self.tree.iter().map(|x| &x.1)
    }

    pub fn is_empty(&self) -> bool {
        self.tree.is_empty()
    }

    pub fn len(&self) -> usize {
        self.tree.len()
    }

    pub fn named_len(&self) -> usize {
        self.named_len
    }

    pub fn iter(&self) -> impl Iterator<Item = &(usize, Arc<Function>)> {
        self.tree.iter()
    }

    pub fn get_by_addr(&self, addr: usize) -> Option<Arc<Function>> {
        let search = self.tree.binary_search_by(|x| x.0.cmp(&addr));

        match search {
            Ok(idx) => Some(self.tree[idx].1.clone()),
            Err(..) => None,
        }
    }

    pub fn get_by_name(&self, name: &str) -> Option<(usize, Arc<Function>)> {
        self.tree
            .iter()
            .find(|(_, func)| func.as_str() == name)
            .map(|(addr, func)| (*addr, func.clone()))
    }

    pub fn prefix_match(&self, prefix: &str) -> Option<String> {
        let arc_prefix = ArcStr {
            inner: Arc::from(prefix),
        };

        self.trie.get_raw_descendant(&arc_prefix).and_then(|desc| {
            let desc: Vec<&ArcStr> = desc.keys().collect();
            find_shortest_match(&desc, prefix)
        })
    }

    pub fn insert(&mut self, addr: usize, function: Function) {
        self.tree.push((addr, Arc::new(function)));
    }
}

fn find_shortest_match(space: &[&ArcStr], prefix: &str) -> Option<String> {
    let mut shortest_match: Option<&str> = None;

    for possible in space {
        if possible.starts_with(prefix) {
            let mut extended_prefix = prefix;

            // find the extended prefix for the current string
            for (idx, _) in possible.char_indices().skip(prefix.chars().count()) {
                extended_prefix = &possible[..=idx];
                let all_match = space
                    .iter()
                    .filter(|s| s.starts_with(prefix))
                    .all(|s| s.starts_with(&extended_prefix));

                if !all_match {
                    // if all strings don't match, break and use the previous extended prefix
                    extended_prefix = &possible[..idx];
                    break;
                }
            }

            // update shortest_match if it's None or found a shorter match
            match &shortest_match {
                None => shortest_match = Some(extended_prefix),
                Some(shortest) if extended_prefix.len() < shortest.len() => {
                    shortest_match = Some(extended_prefix)
                }
                _ => {}
            }
        }
    }

    shortest_match.map(str::to_string)
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
    tokens: Vec<Token>,
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

        this.tokens.push(Token::from_string(s.to_string(), Colors::item()));
        this
    }

    /// SAFETY: must downcast &'static str to a lifetime that matches the lifetime of self.
    #[inline]
    pub fn inner<'a>(&self) -> &'a str {
        unsafe { std::mem::transmute(self.inner.as_ref()) }
    }

    #[inline]
    pub fn push(&mut self, text: &'static str, color: Color) {
        self.tokens.push(Token::from_str(text, color));
    }

    #[inline]
    pub fn push_string(&mut self, text: String, color: Color) {
        self.tokens.push(Token::from_string(text, color));
    }

    #[inline]
    pub fn pop(&mut self) {
        self.tokens.pop();
    }

    #[inline]
    pub fn tokens(&self) -> &[Token] {
        self.tokens.as_slice()
    }
}

impl PartialEq for TokenStream {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}
