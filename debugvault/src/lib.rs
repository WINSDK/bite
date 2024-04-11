use std::fmt;
use std::path::Path;
use std::sync::Arc;
use common::*;
use demangler::TokenStream;
use object::{Object, ObjectSymbol};
use radix_trie::{Trie, TrieCommon};
use tokenizing::{Colors, Token};
use processor_shared::{Addressed, AddressMap};

mod common;
mod demangler;
mod dwarf;
mod elf;
mod error;
mod intern;
mod itanium;
mod macho;
mod msvc;
mod pdb;
mod pe;
mod rust;
mod rust_legacy;

pub enum Error {
    Object(object::Error),
    Dwarf(dwarf::Error),
    Pdb(::pdb::Error),
    Imports(object::Error),
}

#[derive(Debug)]
pub struct FileAttr {
    pub path: Arc<Path>,
    pub line: usize,
    pub column_start: usize,
    pub column_end: usize,
}

pub struct Symbol {
    name: TokenStream,
    name_as_str: ArcStr,
    module: Option<String>,
    is_import: bool,
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

impl Symbol {
    pub fn new(name: TokenStream) -> Self {
        let is_intrinsics = is_name_an_intrinsic(name.inner());
        let name_as_str = String::from_iter(name.tokens().iter().map(|t| &t.text[..]));
        let name_as_str = ArcStr::new(&name_as_str);

        Self {
            name_as_str,
            name,
            module: None,
            is_import: false,
            is_intrinsics,
        }
    }

    fn into_import(mut self) -> Self {
        self.is_import = true;
        self
    }

    fn with_module(mut self, module: String) -> Self {
        self.module = Some(module);
        self
    }

    #[inline]
    pub fn name(&self) -> &[Token] {
        self.name.tokens()
    }

    #[inline]
    pub fn module(&self) -> Option<&str> {
        self.module.as_deref()
    }

    pub fn as_str(&self) -> &str {
        &self.name_as_str
    }

    /// Is the function a unnamed compiler generated artifact.
    pub fn intrinsic(&self) -> bool {
        self.is_intrinsics
    }

    pub fn import(&self) -> bool {
        self.is_import
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl PartialEq for Symbol {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.name_as_str == other.name_as_str
    }
}

#[derive(Default, Debug)]
pub struct Index {
    /// Mapping from addresses starting at the header base to functions.
    /// The addresses are sorted.
    symbols: AddressMap<Arc<Symbol>>,

    /// Mapping from addresses starting at the header base to source files.
    /// The addresses are sorted.
    file_attrs: AddressMap<FileAttr>,

    /// Prefix tree for finding symbols.
    trie: Trie<ArcStr, Arc<Symbol>>,

    /// Number of named compiler artifacts.
    named_len: usize,
}

fn parse_symbol_names<'d, O: Object<'d, 'd>>(
    obj: &'d O,
) -> Result<AddressMap<Arc<Symbol>>, object::Error> {
    let names: Vec<_> = obj
        .symbols()
        .filter_map(|symbol| symbol.name().map(|name| (symbol.address() as usize, name)).ok())
        .collect();

    // Insert defined symbols.
    log::PROGRESS.set("Parsing symbols.", names.len());
    let mut symbols = AddressMap::default();
    parallel_compute(names, &mut symbols, |&(addr, symbol)| {
        let func = Symbol::new(demangler::parse(symbol));
        log::PROGRESS.step();
        Addressed {
            addr,
            item: Arc::new(func),
        }
    });

    Ok(symbols)
}

impl Index {
    pub fn parse(obj: &object::File, path: &Path) -> Result<Self, Error> {
        let mut this = Self::default();

        match obj {
            object::File::MachO32(macho) => {
                let mut debug_info = macho::MachoDebugInfo::parse(macho)?;
                let dwarf = macho::dwarf(obj, path)?;
                let symbols = debug_info.take_symbols();

                this.file_attrs.extend(dwarf.file_attrs);
                this.symbols.extend(symbols);
            }
            object::File::MachO64(macho) => {
                let mut debug_info = macho::MachoDebugInfo::parse(macho)?;
                let dwarf = macho::dwarf(obj, path)?;
                let symbols = debug_info.take_symbols();

                this.file_attrs.extend(dwarf.file_attrs);
                this.symbols.extend(symbols);
            }
            object::File::Elf32(elf) => {
                let debug_info = elf::ElfDebugInfo::parse(elf);
                let dwarf = dwarf::Dwarf::parse(obj)?;
                let imports = debug_info.imports()?;
                let symbols = debug_info.symbols()?;

                this.file_attrs.extend(dwarf.file_attrs);
                this.symbols.extend(imports);
                this.symbols.extend(symbols);
            }
            object::File::Elf64(elf) => {
                let debug_info = elf::ElfDebugInfo::parse(elf);
                let dwarf = dwarf::Dwarf::parse(obj)?;
                let imports = debug_info.imports()?;
                let symbols = debug_info.symbols()?;

                this.file_attrs.extend(dwarf.file_attrs);
                this.symbols.extend(imports);
                this.symbols.extend(symbols);
            }
            object::File::Pe32(pe) => {
                let debug_info = pe::PeDebugInfo::parse(pe);
                let dwarf = dwarf::Dwarf::parse(obj)?;
                let imports = debug_info.imports()?;
                let symbols = debug_info.symbols()?;

                this.file_attrs.extend(dwarf.file_attrs);
                this.symbols.extend(imports);
                this.symbols.extend(symbols);
            }
            object::File::Pe64(pe) => {
                let debug_info = pe::PeDebugInfo::parse(pe);
                let dwarf = dwarf::Dwarf::parse(obj)?;
                let imports = debug_info.imports()?;
                let symbols = debug_info.symbols()?;

                this.file_attrs.extend(dwarf.file_attrs);
                this.symbols.extend(imports);
                this.symbols.extend(symbols);
            }
            _ => {}
        }

        if let Some(pdb) = pdb::PDB::parse(obj) {
            let pdb = pdb?;
            this.file_attrs.extend(pdb.file_attrs);
            this.symbols.extend(pdb.functions);
        }

        this.sort_and_validate();
        this.build_prefix_tree();

        log::complex!(
            w "[index::parse] found ",
            g this.symbols.len().to_string(),
            w " functions."
        );

        Ok(this)
    }

    fn sort_and_validate(&mut self) {
        // Only keep one symbol per address.
        self.symbols.dedup_by_key(|func| func.addr);

        // Only keep valid symbols.
        self.symbols.retain(|Addressed { addr, item: func }| {
            if *addr == 0 {
                return false;
            }

            if func.as_str().is_empty() {
                return false;
            }

            true
        });

        // Count the number of function's that aren't compiler intrinsics.
        self.named_len = self.symbols.iter().filter(|func| !func.item.intrinsic()).count();

        // Keep functions sorted so it can be binary searched.
        self.symbols.sort_unstable();

        // Keep file attrs sorted so it can be binary searched.
        self.file_attrs.sort_unstable();
    }

    fn build_prefix_tree(&mut self) {
        log::PROGRESS.set("Building prefix tree", self.symbols.len());

        // Radix-prefix tree for fast lookups.
        for Addressed { item: func, .. } in self.symbols.iter() {
            self.trie.insert(func.name_as_str.clone(), Arc::clone(func));
            log::PROGRESS.step();
        }
    }

    pub fn named_funcs_count(&self) -> usize {
        self.named_len
    }

    pub fn functions(&self) -> impl Iterator<Item = &Addressed<Arc<Symbol>>> {
        self.symbols.iter()
    }

    pub fn get_file_by_addr(&self, addr: usize) -> Option<&FileAttr> {
        match self.file_attrs.search(addr) {
            Ok(idx) => Some(&self.file_attrs[idx].item),
            Err(..) => None,
        }
    }

    pub fn get_func_by_addr(&self, addr: usize) -> Option<Arc<Symbol>> {
        match self.symbols.search(addr) {
            Ok(idx) => Some(self.symbols[idx].item.clone()),
            Err(..) => None,
        }
    }

    pub fn get_func_by_name(&self, name: &str) -> Option<usize> {
        self.symbols
            .iter()
            .find(|func| func.item.as_str() == name)
            .map(|func| func.addr)
    }

    /// Only used for tests.
    #[doc(hidden)]
    pub fn insert_func(&mut self, addr: usize, name: &str) {
        self.symbols.push(Addressed {
            addr,
            item: Arc::new(Symbol::new(TokenStream::simple(name))),
        })
    }

    pub fn prefix_match_func(&self, prefix: &str) -> Vec<String> {
        let arc_prefix = ArcStr::new(prefix);
        let desc = match self.trie.get_raw_descendant(&arc_prefix) {
            Some(desc) => desc.keys().collect(),
            None => Vec::new(),
        };

        sort_by_shortest_match(&desc, prefix)
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
