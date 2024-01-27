//! Symbol demangler for common mangling schemes.

use std::fs::File;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use object::elf::{R_X86_64_COPY, R_X86_64_GLOB_DAT, R_X86_64_JUMP_SLOT};
use object::endian::Endian;
use object::read::elf::{ElfFile, FileHeader};
use object::read::macho::{MachHeader, MachOFile};
use object::read::pe::{ImageNtHeaders, ImageThunkData, PeFile};
use object::LittleEndian as LE;
use object::{Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationKind};

use crossbeam_queue::SegQueue;
use dashmap::DashMap;
use helper::{parallel_compute, ArcStr};
use pdb::{SymbolData, FallibleIterator};
use processor_types::Section;
use radix_trie::{Trie, TrieCommon};
use tokenizing::{Color, ColorScheme, Colors, Token};

mod error;
mod helper;
pub mod itanium;
pub mod msvc;
pub mod rust;
pub mod rust_legacy;

type PhysAddr = usize;

pub enum Error {
    Object(object::Error),
    Dwarf(gimli::Error),
    Pdb(pdb::Error),
    Imports(object::Error),
}

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

#[derive(Debug)]
pub struct Function {
    name: TokenStream,
    name_as_str: ArcStr,
    module: Option<String>,
    is_intrinsics: bool,
}

fn is_name_an_intrinsic(name: &str) -> bool {
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
        let name_as_str = ArcStr::new(&name_as_str);

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

#[derive(Debug)]
pub struct FileAttr {
    pub path: Arc<Path>,
    pub line: usize,
}

#[derive(Default, Debug)]
pub struct Index {
    /// Mapping from addresses starting at the header base to functions.
    /// The addresses are sorted.
    functions: Vec<(PhysAddr, Arc<Function>)>,

    /// Mapping from addresses starting at the header base to source files.
    file_attrs: Vec<(PhysAddr, FileAttr)>,

    /// Prefix tree for finding symbols.
    trie: Trie<ArcStr, Arc<Function>>,

    /// Number of named compiler artifacts.
    named_len: usize,
}

type GimliSlice<'a> = gimli::EndianSlice<'a, gimli::RunTimeEndian>;
type Cache = Arc<DashMap<u64, Arc<Path>>>;

fn parse_dwarf_unit(
    header_id: u64,
    path_cache: &Cache,
    dwarf: &gimli::Dwarf<GimliSlice>,
    header: gimli::UnitHeader<GimliSlice>,
    file_attrs: &mut Vec<(PhysAddr, FileAttr)>,
) -> Result<(), gimli::Error> {
    let unit = dwarf.unit(header)?;
    let program = match unit.line_program {
        Some(ref program) => program.clone(),
        None => return Ok(()),
    };

    let comp_dir = unit
        .comp_dir
        .map(|dir| PathBuf::from(dir.to_string().unwrap_or_default()))
        .unwrap_or_default();

    // iterate over the line program rows
    let mut rows = program.rows();
    while let Some((header, row)) = rows.next_row()? {
        // end of sequence indicates a possible gap in addresses
        if row.end_sequence() {
            continue;
        }

        let (file, line) = match (row.file(header), row.line()) {
            (Some(file), Some(line)) => (file, line),
            _ => continue,
        };

        // try to use cached path if possible, prevents extra allocations
        let key = header_id << 48 | row.file_index() << 24 | file.directory_index();
        let path = match path_cache.get(&key) {
            Some(cached_path) => Arc::clone(&cached_path),
            None => {
                // compute path for the given line
                let mut path = comp_dir.clone();

                let dir_idx = file.directory_index() as usize;
                if dir_idx != 0 {
                    if let Some(dir) = file.directory(header) {
                        if let Ok(path_comp) = dwarf.attr_string(&unit, dir)?.to_string() {
                            path.push(path_comp);
                        }
                    }
                }

                if let Ok(path_comp) = dwarf.attr_string(&unit, file.path_name())?.to_string() {
                    path.push(path_comp);
                }

                let path = Arc::from(path);
                path_cache.insert(key, Arc::clone(&path));
                path
            }
        };

        file_attrs.push((
            row.address() as PhysAddr,
            FileAttr {
                path,
                line: line.get() as usize,
            },
        ))
    }

    Ok(())
}

impl Index {
    pub fn parse_dwarf(
        &mut self,
        obj: &object::File,
        sections: &[Section],
    ) -> Result<(), Error> {
        let endian = if obj.is_little_endian() {
            gimli::RunTimeEndian::Little
        } else {
            gimli::RunTimeEndian::Big
        };

        // load an already decompressed and read section
        let load_section = |id: gimli::SectionId| -> Result<&[u8], gimli::Error> {
            let target_name = id.name();
            match sections.iter().find(|section| section.name == target_name) {
                Some(section) => Ok(&section.bytes[..]),
                None => Ok(&[]),
            }
        };

        // load all of the sections
        let dwarf_ref = gimli::Dwarf::load(&load_section)?;

        // create `EndianSlice`s for all of the sections
        let dwarf = Arc::new(dwarf_ref.borrow(|section| gimli::EndianSlice::new(section, endian)));

        // iterate over the compilation units and store them in a queue
        let header_queue = Arc::new(SegQueue::new());
        let mut iter = dwarf.units();
        let mut idx = 0;
        while let Some(header) = iter.next()? {
            header_queue.push((idx, header));
            idx += 1;
        }

        // create concurrent hashmap for caching file path's
        let path_cache = Cache::default();

        log::PROGRESS.set("Parsing dwarf.", header_queue.len());
        std::thread::scope(|s| -> Result<_, gimli::Error> {
            let thread_count = std::thread::available_parallelism().unwrap().get();
            let threads: Vec<_> = (0..thread_count)
                .map(|_| {
                    let path_cache = Arc::clone(&path_cache);
                    let dwarf = Arc::clone(&dwarf);
                    let header_queue = Arc::clone(&header_queue);

                    s.spawn(move || -> Result<_, gimli::Error> {
                        let mut file_attrs = Vec::new();

                        while let Some((header_id, header)) = header_queue.pop() {
                            parse_dwarf_unit(
                                header_id,
                                &path_cache,
                                &dwarf,
                                header,
                                &mut file_attrs
                            )?;
                            log::PROGRESS.step();
                        }

                        Ok(file_attrs)
                    })
                })
                .collect();

            for thread in threads {
                self.file_attrs.extend(thread.join().unwrap()?);
            }

            Ok(())
        })?;

        if !path_cache.is_empty() {
            log::complex!(
                w "[index::parse_dwarf] indexed ",
                g path_cache.len().to_string(),
                w " source files."
            );
        }

        Ok(())
    }
}

#[allow(clippy::too_many_arguments)]
fn parse_pdb_module<'a>(
    module_id: u64,
    base_addr: PhysAddr,
    path_cache: &Cache,
    module_info: pdb::ModuleInfo,
    address_map: &pdb::AddressMap,
    string_table: &pdb::StringTable<'a>,
    names: &mut Vec<(PhysAddr, &'a str)>,
    file_attrs: &mut Vec<(PhysAddr, FileAttr)>,
) -> Result<(), pdb::Error> {
    let program = module_info.line_program()?;
    let mut symbols = module_info.symbols()?;

    while let Some(symbol) = symbols.next()? {
        match symbol.parse() {
            Ok(SymbolData::Public(symbol)) if symbol.function => {
                let addr = match symbol.offset.to_rva(address_map) {
                    Some(rva) => rva.0 as PhysAddr,
                    None => continue,
                };

                let name: &'a str = match std::str::from_utf8(symbol.name.as_bytes()) {
                    Ok(name) => unsafe { std::mem::transmute(name) },
                    Err(_) => continue,
                };

                names.push((base_addr + addr, name));
            },
            Ok(SymbolData::Procedure(proc)) => {
                let mut lines = program.lines_for_symbol(proc.offset);
                while let Some(line_info) = lines.next()? {
                    let addr = match line_info.offset.to_rva(address_map) {
                        Some(rva) => rva.0 as PhysAddr,
                        None => continue,
                    };

                    // try to use cached path if possible, prevents extra allocations
                    let key = module_id << 48 | line_info.file_index.0 as u64;
                    let path = match path_cache.get(&key) {
                        Some(cached_path) => Arc::clone(&cached_path),
                        None => {
                            // compute path for the given line
                            let file_info = program.get_file_info(line_info.file_index)?;
                            let file_name = file_info.name.to_raw_string(string_table)?;
                            let path = match std::str::from_utf8(file_name.as_bytes()) {
                                Ok(file_name) => Arc::from(Path::new(file_name)),
                                Err(_) => continue,
                            };

                            path_cache.insert(key, Arc::clone(&path));
                            path
                        }
                    };

                    let file_attr = FileAttr { path, line: line_info.line_start as usize};
                    file_attrs.push((base_addr + addr, file_attr));
                }
            }
            Ok(_) => {
                // TODO: implement support for other types of symbols
            }
            Err(pdb::Error::UnimplementedSymbolKind(_)) => {},
            Err(err) => log::complex!(
                w "[index::parse_pdb] ",
                y format!("{err}."),
            )
        }
    }

    Ok(())
}

impl Index {
    pub fn parse_pdb(&mut self, obj: &object::File) -> Result<(), Error> {
        fn open_pdb(obj: &object::File) -> Option<File> {
            let pdb = obj.pdb_info().ok()??;
            let path = std::str::from_utf8(pdb.path()).ok()?;
            std::fs::File::open(path).ok()
        }

        if let Some(file) = open_pdb(obj) {
            self.parse_pdb_impl(obj, file)?;
        }

        Ok(())
    }

    fn parse_pdb_impl(&mut self, obj: &object::File, file: File) -> Result<(), Error> {
        let mut names = Vec::new();
        let base_addr = obj.relative_address_base() as PhysAddr;

        let mut pdb = pdb::PDB::open(file)?;

        // mapping from offset's to rva's
        let address_map = Arc::new(pdb.address_map()?);

        // mapping from string ref's to strings
        let string_table = Arc::new(pdb.string_table()?);

        // pdb module's
        let dbi = pdb.debug_information()?;
        let mut modules = dbi.modules()?;

        // create concurrent hashmap for caching file path's
        let path_cache = Cache::default();

        // iterate over the modules and store them in a queue
        let module_info_queue = Arc::new(SegQueue::new());
        let mut idx = 0;
        while let Some(module) = modules.next()? {
            let module_info = match pdb.module_info(&module)? {
                Some(info) => info,
                None => continue,
            };

            module_info_queue.push((idx, module_info));
            idx += 1;
        }

        log::PROGRESS.set("parsing pdb.", module_info_queue.len());

        // parse local symbols
        std::thread::scope(|s| -> Result<_, pdb::Error> {
            let thread_count = std::thread::available_parallelism().unwrap().get();
            let threads: Vec<_> = (0..thread_count)
                .map(|_| {
                    let path_cache = Arc::clone(&path_cache);
                    let address_map = Arc::clone(&address_map);
                    let string_table = Arc::clone(&string_table);
                    let module_info_queue = Arc::clone(&module_info_queue);

                    s.spawn(move || -> Result<_, pdb::Error> {
                        let mut names = Vec::new();
                        let mut file_attrs = Vec::new();

                        while let Some((module_id, module_info)) = module_info_queue.pop() {
                            parse_pdb_module(
                                module_id,
                                base_addr,
                                &path_cache,
                                module_info,
                                &address_map,
                                &string_table,
                                &mut names,
                                &mut file_attrs
                            )?;
                            log::PROGRESS.step();
                        }

                        Ok((file_attrs, names))
                    })
                })
                .collect();

            for thread in threads {
                let (file_attrs, part_names) = thread.join().unwrap()?;
                self.file_attrs.extend(file_attrs);
                names.extend(part_names);
            }

            Ok(())
        })?;

        if !path_cache.is_empty() {
            log::complex!(
                w "[index::parse_pdb] indexed ",
                g path_cache.len().to_string(),
                w " source files."
            );
        }

        // iterate through global symbols
        let global_symbols = pdb.global_symbols()?;
        let mut symbol_table = global_symbols.iter();

        // parse global symbols
        while let Some(symbol) = symbol_table.next()? {
            match symbol.parse() {
                Ok(SymbolData::Public(symbol)) if symbol.function => {
                    let addr = match symbol.offset.to_rva(&address_map) {
                        Some(rva) => rva.0 as PhysAddr,
                        None => continue,
                    };

                    let name = match std::str::from_utf8(symbol.name.as_bytes()) {
                        Ok(name) => name,
                        Err(_) => continue,
                    };

                    names.push((base_addr + addr, name));
                }
                Ok(_) => {
                    // TODO: implement support for other types of symbols
                },
                Err(pdb::Error::UnimplementedSymbolKind(_)) => {},
                Err(err) => log::complex!(
                    w "[index::parse_pdb] ",
                    y format!("{err}."),
                )
            };
        }

        // insert defined symbols
        log::PROGRESS.set("parsing pdb symbol", names.len());
        parallel_compute(names, &mut self.functions, |(addr, symbol)| {
            let func = Function::new(parser(symbol), None);
            log::PROGRESS.step();
            (*addr, Arc::new(func))
        });

        Ok(())
    }
}

impl Index {
    pub fn parse_symbols(&mut self, obj: &object::File) -> Result<(), Error> {
        let names: Vec<_> = obj.symbols().filter_map(symbol_addr_name).collect();

        // insert defined symbols
        log::PROGRESS.set("parsing debug symbol", names.len());
        parallel_compute(names, &mut self.functions, |(addr, symbol)| {
            let func = Function::new(parser(symbol), None);
            log::PROGRESS.step();
            (*addr, Arc::new(func))
        });

        // insert entrypoint into known symbols
        let entrypoint = obj.entry() as usize;
        let entry_func = Function::new(TokenStream::simple("entry"), None);

        // insert entrypoint
        self.insert(entrypoint, entry_func);

        Ok(())
    }

    pub fn parse_imports(&mut self, obj: &object::File) -> Result<(), Error> {
        match obj {
            object::File::Pe32(obj) => self.parse_pe_imports(obj),
            object::File::Pe64(obj) => self.parse_pe_imports(obj),
            object::File::Elf32(obj) => self.parse_elf_imports(obj),
            object::File::Elf64(obj) => self.parse_elf_imports(obj),
            object::File::MachO32(obj) => self.parse_macho_imports(obj),
            object::File::MachO64(obj) => self.parse_macho_imports(obj),
            _ => Ok(())
        }
    }

    fn parse_pe_imports<H: ImageNtHeaders>(&mut self, obj: &PeFile<H>) -> Result<(), Error> {
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

    fn parse_elf_imports<H: FileHeader>(&mut self, obj: &ElfFile<H>) -> Result<(), Error> {
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

    fn parse_macho_imports<H: MachHeader>(&mut self, _obj: &MachOFile<H>) -> Result<(), Error> {
        Ok(())
    }

    pub fn complete(&mut self) {
        // only keep one symbol per address
        self.functions.dedup_by_key(|(addr, _)| *addr);

        // only keep valid symbols
        self.functions.retain(|(addr, func)| {
            if *addr == 0 {
                return false;
            }

            if func.as_str().is_empty() {
                return false;
            }

            true
        });

        // keep files sorted so they can be binary searched
        self.file_attrs.sort_unstable_by_key(|(addr, _)| *addr);

        // count the number of function's that aren't compiler intrinsics
        self.named_len = self.functions.iter().filter(|(_, func)| !func.intrinsic()).count();

        // keep tree sorted so it can be binary searched
        self.functions.sort_unstable_by_key(|(addr, _)| *addr);

        log::PROGRESS.set("Building prefix tree", self.functions.len());

        // radix-prefix tree for fast lookups
        for (_, func) in self.functions.iter() {
            self.trie.insert(func.name_as_str.clone(), func.clone());
            log::PROGRESS.step();
        }

        log::complex!(
            w "[index::complete] found ",
            g self.functions.len().to_string(),
            w " functions."
        );
    }

    pub fn symbols(&self) -> impl Iterator<Item = &Arc<Function>> {
        self.functions.iter().map(|x| &x.1)
    }

    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }

    pub fn len(&self) -> usize {
        self.functions.len()
    }

    pub fn named_len(&self) -> usize {
        self.named_len
    }

    pub fn iter(&self) -> impl Iterator<Item = &(usize, Arc<Function>)> {
        self.functions.iter()
    }

    pub fn get_file(&self, addr: usize) -> Option<&FileAttr> {
        let search = self.file_attrs.binary_search_by(|x| x.0.cmp(&addr));

        match search {
            Ok(idx) => Some(&self.file_attrs[idx].1),
            Err(..) => None,
        }
    }

    pub fn get_by_addr(&self, addr: PhysAddr) -> Option<Arc<Function>> {
        let search = self.functions.binary_search_by(|x| x.0.cmp(&addr));

        match search {
            Ok(idx) => Some(self.functions[idx].1.clone()),
            Err(..) => None,
        }
    }

    pub fn get_by_name(&self, name: &str) -> Option<(usize, Arc<Function>)> {
        self.functions
            .iter()
            .find(|(_, func)| func.as_str() == name)
            .map(|(addr, func)| (*addr, func.clone()))
    }

    pub fn prefix_match(&self, prefix: &str) -> Vec<String> {
        let arc_prefix = ArcStr::new(prefix);
        let desc = match self.trie.get_raw_descendant(&arc_prefix) {
            Some(desc) => desc.keys().collect(),
            None => Vec::new(),
        };

        sort_by_shortest_match(&desc, prefix)
    }

    pub fn insert(&mut self, addr: PhysAddr, function: Function) {
        self.functions.push((addr, Arc::new(function)));
    }
}

/// Sort the first 100 strings by length if they have a matching prefix.
fn sort_by_shortest_match(input: &[&ArcStr], prefix: &str) -> Vec<String> {
    let mut matches: Vec<String> = Vec::new();

    for possible in input {
        if matches.len() == 100 {
            break;
        }

        if possible.starts_with(prefix) {
            matches.push(possible.to_string());
        }
    }

    // sort the matches by length
    matches.sort_by_key(|a| a.len());
    matches
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
