use std::fmt;
use std::path::Path;
use std::sync::Arc;

use object::{Object, ObjectSymbol};
use common::*;
use radix_trie::{Trie, TrieCommon};
use tokenizing::{Colors, Token};
use demangler::TokenStream;

mod demangler;
mod itanium;
mod msvc;
mod rust;
mod rust_legacy;
mod error;
mod common;
mod intern;
mod dwarf;
mod pdb;
mod macho;
mod elf;
mod pe;

pub enum Error {
    Object(object::Error),
    Dwarf(gimli::Error),
    Pdb(::pdb::Error),
    Imports(object::Error),
}

#[derive(Debug)]
pub struct FileAttr {
    pub path: Arc<Path>,
    pub line: usize,
}

pub struct Function {
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

impl Function {
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

    fn as_import(mut self) -> Self {
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

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl PartialEq for Function {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.name_as_str == other.name_as_str
    }
}

#[derive(Default, Debug)]
pub struct Index {
    /// Mapping from addresses starting at the header base to functions.
    /// The addresses are sorted.
    functions: Vec<(usize, Arc<Function>)>,

    /// Mapping from addresses starting at the header base to source files.
    file_attrs: Vec<(usize, FileAttr)>,

    /// Prefix tree for finding symbols.
    trie: Trie<ArcStr, Arc<Function>>,

    /// Number of named compiler artifacts.
    named_len: usize,
}

fn parse_symbols(obj: &object::File) -> Result<Vec<(usize, Arc<Function>)>, Error> {
    let names: Vec<_> = obj.symbols().filter_map(symbol_addr_name).collect();

    // Insert defined symbols.
    log::PROGRESS.set("Parsing symbols.", names.len());
    let mut functions = Vec::new();
    parallel_compute(names, &mut functions, |(addr, symbol)| {
        let func = Function::new(demangler::parse(symbol));
        log::PROGRESS.step();
        (*addr, Arc::new(func))
    });

    // Insert entrypoint into known symbols.
    let mut entrypoint = obj.entry() as usize;

    if let object::File::MachO32(..) | object::File::MachO64(..) = obj {
        // This appears to be the header base that isn't added to the entrypoint.
        // Kind of hard to tell.
        entrypoint += 0x100000000;
    }

    let entry_func = Function::new(TokenStream::simple("entry"));

    // Insert entrypoint.
    functions.push((entrypoint, Arc::new(entry_func)));

    Ok(functions)
}

impl Index {
    pub fn parse(obj: &object::File) -> Result<Self, Error> {
        let mut this = Self::default();

        match obj {
            object::File::MachO32(obj) => {
                let debug_info = macho::MachoDebugInfo::parse(obj)?;
                let dwarf = debug_info.dwarf()?;
                let imports = debug_info.imports()?;

                this.file_attrs.extend(dwarf.file_attrs);
                this.functions.extend(imports);
            },
            object::File::MachO64(obj) => {
                let debug_info = macho::MachoDebugInfo::parse(obj)?;
                let dwarf = debug_info.dwarf()?;
                let imports = debug_info.imports()?;

                this.file_attrs.extend(dwarf.file_attrs);
                this.functions.extend(imports);
            },
            object::File::Elf32(obj) => {
                let debug_info = elf::ElfDebugInfo::parse(obj);
                let dwarf = debug_info.dwarf()?;
                let imports = debug_info.imports()?;

                this.file_attrs.extend(dwarf.file_attrs);
                this.functions.extend(imports);
            },
            object::File::Elf64(obj) => {
                let debug_info = elf::ElfDebugInfo::parse(obj);
                let dwarf = debug_info.dwarf()?;
                let imports = debug_info.imports()?;

                this.file_attrs.extend(dwarf.file_attrs);
                this.functions.extend(imports);
            },
            object::File::Pe32(obj) => {
                let debug_info = pe::PeDebugInfo::parse(obj);
                let dwarf = debug_info.dwarf()?;
                let imports = debug_info.imports()?;

                this.file_attrs.extend(dwarf.file_attrs);
                this.functions.extend(imports);
            },
            object::File::Pe64(obj) => {
                let debug_info = pe::PeDebugInfo::parse(obj);
                let dwarf = debug_info.dwarf()?;
                let imports = debug_info.imports()?;

                this.file_attrs.extend(dwarf.file_attrs);
                this.functions.extend(imports);
            },
            _ => {}
        }

        if let Some(pdb) = pdb::PDB::parse(obj) {
            let pdb = pdb?;
            this.file_attrs.extend(pdb.file_attrs);
            this.functions.extend(pdb.functions);
        }

        this.functions.extend(parse_symbols(obj)?);

        this.sort_and_validate();
        this.build_prefix_tree();

        log::complex!(
            w "[index::parse] found ",
            g this.functions.len().to_string(),
            w " functions."
        );

        Ok(this)
    }

    fn sort_and_validate(&mut self) {
        // Only keep one symbol per address.
        self.functions.dedup_by_key(|(addr, _)| *addr);

        // Only keep valid symbols.
        self.functions.retain(|(addr, func)| {
            if *addr == 0 {
                return false;
            }

            if func.as_str().is_empty() {
                return false;
            }

            true
        });

        // Count the number of function's that aren't compiler intrinsics.
        self.named_len = self.functions.iter().filter(|(_, func)| !func.intrinsic()).count();

        // Keep functions sorted so it can be binary searched.
        self.functions.sort_unstable_by_key(|(addr, _)| *addr);
    }

    fn build_prefix_tree(&mut self) {
        log::PROGRESS.set("Building prefix tree", self.functions.len());

        // Radix-prefix tree for fast lookups.
        for (_, func) in self.functions.iter() {
            self.trie.insert(func.name_as_str.clone(), Arc::clone(&func));
            log::PROGRESS.step();
        }
    }

    pub fn named_funcs_count(&self) -> usize {
        self.named_len
    }

    pub fn functions(&self) -> impl Iterator<Item = &(usize, Arc<Function>)> {
        self.functions.iter()
    }

    pub fn get_file_by_addr(&self, addr: usize) -> Option<&FileAttr> {
        let search = self.file_attrs.binary_search_by(|x| x.0.cmp(&addr));

        match search {
            Ok(idx) => Some(&self.file_attrs[idx].1),
            Err(..) => None,
        }
    }

    pub fn get_func_by_addr(&self, addr: usize) -> Option<Arc<Function>> {
        let search = self.functions.binary_search_by(|x| x.0.cmp(&addr));

        match search {
            Ok(idx) => Some(self.functions[idx].1.clone()),
            Err(..) => None,
        }
    }

    pub fn get_func_by_name(&self, name: &str) -> Option<usize> {
        self.functions
            .iter()
            .find(|(_, func)| func.as_str() == name)
            .map(|(addr, _)| *addr)
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

fn symbol_addr_name<'sym>(symbol: object::Symbol<'sym, 'sym>) -> Option<(usize, &'sym str)> {
    if let Ok(name) = symbol.name() {
        return Some((symbol.address() as usize, name));
    }

    None
}
