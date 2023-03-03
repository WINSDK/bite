pub mod config;
mod itanium;
mod msvc;
mod rust_legacy;
mod rust_modern;

use object::{Object, ObjectSymbol};
use pdb::FallibleIterator;
use std::collections::BTreeMap;

use crate::disassembler::Line;
use crate::threading::spawn_threaded;

pub use config::Config;

#[derive(Debug)]
pub struct Index {
    tree: BTreeMap<usize, String>,
}

impl Index {
    pub fn new() -> Self {
        Self {
            tree: BTreeMap::new(),
        }
    }

    pub async fn parse<'a>(obj: &'a object::File<'a>) -> pdb::Result<Self> {
        let symbols: Vec<(usize, &str)> = obj.symbols().filter_map(symbol_addr_name).collect();

        if let Ok(Some(pdb)) = obj.pdb_info() {
            let Ok(path) = std::str::from_utf8(pdb.path()) else {
                let tree = symbols
                    .into_iter()
                    .map(|(addr, symbol)| (addr, symbol.to_string()))
                    .collect();

                return Ok(Self { tree });
            };

            let Ok(file) = std::fs::File::open(path) else {
                let tree = symbols
                    .into_iter()
                    .map(|(addr, symbol)| (addr, symbol.to_string()))
                    .collect();

                return Ok(Self { tree });
            };

            let mut pdb = pdb::PDB::open(file)?;
            let mut symbols = Vec::new();

            // get symbol table
            let symbol_table = pdb.global_symbols()?;

            // iterate through symbols collected earlier
            let mut symbol_table = symbol_table.iter();

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
                        symbols.push((addr.0 as usize, name));
                    }
                }
            }
        }

        if crate::ARGS.simplify {
            todo!("simplify symbols");
        }

        let parser = |s: &str| {
            // parse rust symbols that match the v0 mangling scheme
            if let Some(s) = strip_prefixes(s, &["__R", "_R", "R"]) {
                return rust_modern::parse(s).unwrap_or_default();
            }

            // try parsing legacy rust symbols first because they can also be appear as C++ symbols
            if let Some(s) = strip_prefixes(s, &["__ZN", "_ZN", "ZN"]) {
                if let Some(s) = rust_legacy::parse(s) {
                    return s;
                }
            }

            // parse gnu/llvm C/C++ symbols   
            if let Some(s) = strip_prefixes(s, &["@?", "?"]) {
                return itanium::parse(s).unwrap_or_default();
            }

            // parse windows msvc C/C++ symbols
            if let Some(s) = strip_prefixes(s, &["@?", "?"]) {
                return msvc::parse(s).unwrap_or_default();
            }

            // return the original mangled symbol on failure
            s.to_string()
        };

        let tree = spawn_threaded(symbols, move |(addr, symbol)| (addr, parser(symbol))).await;

        Ok(Self {
            tree: BTreeMap::from_iter(tree),
        })
    }

    pub fn symbols(&self) -> std::collections::btree_map::Values<usize, String> {
        self.tree.values()
    }

    pub fn is_empty(&self) -> bool {
        self.tree.is_empty()
    }

    pub fn get_by_line(&self, line: &Line) -> Option<&str> {
        self.tree
            .get(&(line.section_base + line.offset))
            .map(|s| s as &str)
    }
}

fn strip_prefixes<'a>(s: &'a str, prefixes: &[&str]) -> Option<&'a str> {
    for prefix in prefixes {
        if let Some(s) = s.strip_prefix(prefix) {
            return Some(s);
        }
    }

    None
}

fn symbol_addr_name<'a>(symbol: object::Symbol<'a, 'a>) -> Option<(usize, &'a str)> {
    if let Ok(name) = symbol.name() {
        return Some((symbol.address() as usize, name));
    }

    None
}

// fn valid_symbol(symbol: &(&usize, &&str)) -> bool {
//     !symbol.1.starts_with("GCC_except_table") && !symbol.1.contains("cgu") && !symbol.1.is_empty()
// }
