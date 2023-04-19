use object::{Object, ObjectSymbol};
use pdb::FallibleIterator;
use std::collections::BTreeMap;
use std::sync::Arc;

use demangler::TokenStream;

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

    pub async fn parse(obj: &object::File<'_>) -> pdb::Result<Index> {
        let mut symbols: Vec<(usize, &str)> = obj
            .symbols()
            .filter_map(symbol_addr_name)
            .filter(|(_, sym)| is_valid_symbol(sym))
            .collect();

        let base_addr = obj.relative_address_base() as usize;
        let pdb_table;

        if let Ok(Some(pdb)) = obj.pdb_info() {
            let Ok(path) = std::str::from_utf8(pdb.path()) else {
                let tree = symbols
                    .into_iter()
                    .map(|(addr, symbol)| (addr, Arc::new(TokenStream::new(symbol))))
                    .collect();

                return Ok(Self { tree });
            };

            let Ok(file) = std::fs::File::open(path) else {
                let tree = symbols
                    .into_iter()
                    .map(|(addr, symbol)| (addr, Arc::new(TokenStream::new(symbol))))
                    .collect();

                return Ok(Self { tree });
            };

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
                        if is_valid_symbol(name) {
                            symbols.push((base_addr + addr.0 as usize, name));
                        }
                    }
                }
            }
        }

        if crate::ARGS.simplify {
            todo!("simplify symbols");
        }

        let parser = |s: &str| -> TokenStream {
            // symbols without leading underscores are accepted as
            // dbghelp in windows strips them away

            // parse gnu/llvm Rust/C/C++ symbols
            if let Some(s) = demangler::itanium::parse(s) {
                return s;
            }

            // parse rust symbols that match the v0 mangling scheme
            if let Some(s) = demangler::rust::parse(s) {
                return s;
            }

            // parse windows msvc C/C++ symbols
            if let Some(s) = demangler::msvc::parse(s) {
                return s;
            }

            // return the original mangled symbol on failure
            TokenStream::simple(s)
        };

        let tree = symbols.iter().map(|(addr, symbol)| {
            (*addr, Arc::new(parser(symbol)))
        });

        Ok(Self {
            tree: BTreeMap::from_iter(tree),
        })
    }

    pub fn symbols(&self) -> std::collections::btree_map::Values<usize, Arc<TokenStream>> {
        self.tree.values()
    }

    pub fn is_empty(&self) -> bool {
        self.tree.is_empty()
    }

    pub fn clear(&mut self) {
        self.tree.clear();
    }

    pub fn get_by_line(&self, line: &disassembler::Line) -> Option<Arc<TokenStream>> {
        self.tree.get(&(line.section_base + line.offset)).cloned()
    }
}

fn symbol_addr_name<'a>(symbol: object::Symbol<'a, 'a>) -> Option<(usize, &'a str)> {
    if let Ok(name) = symbol.name() {
        return Some((symbol.address() as usize, name));
    }

    None
}

fn is_valid_symbol(s: &str) -> bool {
    !s.starts_with("GCC_except_table") && !s.contains("cgu") && !s.is_empty()
}
