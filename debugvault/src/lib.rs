use binformat::RawSymbol;
use common::*;
use demangler::TokenStream;
use dwarf::Dwarf;
use processor_shared::{AddressMap, Addressed};
use radix_trie::{Trie, TrieCommon};
use std::path::Path;
use std::sync::Arc;
use std::{fmt, process::Command};
use tokenizing::Token;

mod common;
mod demangler;
mod dwarf;
mod error;
mod intern;
mod itanium;
mod msvc;
mod pdb;
mod rust;
mod rust_legacy;

pub enum Error {
    Object(object::Error),
    Dwarf(dwarf::Error),
    Pdb(::pdb::Error),
    Imports(object::Error),
}

#[derive(Debug, Clone)]
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

impl Default for Symbol {
    fn default() -> Self {
        Self {
            name: TokenStream::new(""),
            name_as_str: ArcStr::new(""),
            module: None,
            is_intrinsics: false,
        }
    }
}

impl Symbol {
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

    pub fn imported(&self) -> bool {
        self.module.is_some()
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
    pub syms: AddressMap<Arc<Symbol>>,

    /// Mapping from addresses starting at the header base to source files.
    /// The addresses are sorted.
    pub file_attrs: AddressMap<FileAttr>,

    /// Prefix tree for finding symbols.
    trie: Trie<ArcStr, Arc<Symbol>>,

    /// Number of named compiler artifacts.
    named_len: usize,
}

impl Index {
    pub fn parse<'data>(
        obj: &object::File<'data>,
        path: &Path,
        mut syms: AddressMap<RawSymbol<'data>>,
    ) -> Result<Self, Error> {
        let mut this = Self::default();

        let dwarf = match obj {
            #[cfg(target_os = "macos")]
            object::File::MachO32(_) | object::File::MachO64(_) => macho_dwarf(obj, path)?,
            _ => dwarf::Dwarf::parse(obj)?,
        };

        this.file_attrs.extend(dwarf.file_attrs);

        let mut pdb = None;
        if let Some(parsed_pdb) = pdb::PDB::parse(obj) {
            pdb = Some(parsed_pdb?);
        }

        // NOTE: This is a little scuffed. We have to take a `ref mut` here
        //       otherwise the PDB will be dropped and so will the symbols.
        if let Some(ref mut pdb) = pdb {
            this.file_attrs.extend(std::mem::take(&mut pdb.file_attrs));
            syms.extend(std::mem::take(&mut pdb.syms));
        }

        log::PROGRESS.set("Parsing symbols.", syms.len());
        parallel_compute(syms.mapping, &mut this.syms, |Addressed { addr, item }| {
            let demangled = demangler::parse(item.name);
            let is_intrinsics = is_name_an_intrinsic(item.name);
            let name_as_str = String::from_iter(demangled.tokens().iter().map(|t| &t.text[..]));
            let name_as_str = ArcStr::new(&name_as_str);
            let symbol = Symbol {
                name_as_str,
                name: demangled,
                module: item.module.map(|x| x.to_string()),
                is_intrinsics,
            };

            log::PROGRESS.step();
            Addressed {
                addr: *addr,
                item: Arc::new(symbol),
            }
        });

        this.sort_and_validate();
        this.build_prefix_tree();

        log::complex!(
            w "[index::parse] found ",
            g this.syms.len().to_string(),
            w " functions."
        );

        Ok(this)
    }

    fn sort_and_validate(&mut self) {
        // Only keep one symbol per address.
        self.syms.dedup_by_key(|func| func.addr);

        // Only keep valid symbols.
        self.syms.retain(|Addressed { addr, item: func }| {
            if *addr == 0 {
                return false;
            }

            if func.as_str().is_empty() {
                return false;
            }

            true
        });

        // Count the number of function's that aren't compiler intrinsics.
        self.named_len = self.syms.iter().filter(|func| !func.item.intrinsic()).count();

        // Keep functions sorted so it can be binary searched.
        self.syms.sort_unstable();

        // Keep file attrs sorted so it can be binary searched.
        self.file_attrs.sort_unstable();
    }

    fn build_prefix_tree(&mut self) {
        log::PROGRESS.set("Building prefix tree", self.syms.len());

        // Radix-prefix tree for fast lookups.
        for Addressed { item: func, .. } in self.syms.iter() {
            self.trie.insert(func.name_as_str.clone(), Arc::clone(func));
            log::PROGRESS.step();
        }
    }

    pub fn named_funcs_count(&self) -> usize {
        self.named_len
    }

    pub fn functions(&self) -> impl Iterator<Item = &Addressed<Arc<Symbol>>> {
        self.syms.iter()
    }

    pub fn get_file_by_addr(&self, addr: usize) -> Option<&FileAttr> {
        match self.file_attrs.search(addr) {
            Ok(idx) => Some(&self.file_attrs[idx].item),
            Err(..) => None,
        }
    }

    pub fn get_sym_by_addr(&self, addr: usize) -> Option<Arc<Symbol>> {
        match self.syms.search(addr) {
            Ok(idx) => Some(self.syms[idx].item.clone()),
            Err(..) => None,
        }
    }

    pub fn get_func_by_name(&self, name: &str) -> Option<usize> {
        self.syms.iter().find(|func| func.item.as_str() == name).map(|func| func.addr)
    }

    /// Only used for tests.
    #[doc(hidden)]
    pub fn insert_func(&mut self, addr: usize, name: &str) {
        self.syms.push(Addressed {
            addr,
            item: Arc::new(Symbol {
                name: TokenStream::simple(name),
                name_as_str: ArcStr::new(name),
                module: None,
                is_intrinsics: false,
            }),
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

pub fn macho_dwarf(obj: &object::File, path: &Path) -> Result<Dwarf, dwarf::Error> {
    let mut dwarf = Dwarf::parse(obj)?;

    let ext = if let Some(exist_ext) = path.extension().and_then(|ext| ext.to_str()) {
        exist_ext.to_string() + ".dSYM"
    } else {
        "dSYM".to_string()
    };

    let opt_dsym = path
        .with_extension(ext)
        .join("Contents/Resources/DWARF")
        .join(path.file_name().unwrap());

    if !opt_dsym.is_file() {
        #[cfg(target_arch = "x86_64")]
        let dsymutil_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("bin/dsymutil_x86_64");
        #[cfg(target_arch = "aarch64")]
        let dsymutil_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("bin/dsymutil_aarch64");

        if dsymutil_path.exists() {
            log::PROGRESS.set("Running dsymutil.", 1);
            let exit_status = Command::new(dsymutil_path)
                .arg("--linker=parallel")
                .arg(path)
                .spawn()?
                .wait()?;
            log::PROGRESS.step();

            if !exit_status.success() {
                log::complex!(
                    w "[macho::dwarf] ",
                    y "Generating dSym failed with exit code ",
                    g exit_status.code().unwrap_or(1).to_string(),
                    y "."
                );
            }
        }
    }

    let dsym_dwarf = Dwarf::load(&opt_dsym)?;
    dwarf.merge(dsym_dwarf);

    Ok(dwarf)
}
