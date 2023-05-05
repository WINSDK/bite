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
    fn pdb_file(obj: &object::File<'_>) -> Option<std::fs::File> {
        let pdb = obj.pdb_info().ok()??;
        let path = std::str::from_utf8(pdb.path()).ok()?;

        std::fs::File::open(path).ok()
    }

    pub fn parse(obj: &object::File<'_>) -> pdb::Result<Index> {
        let mut symbols: Vec<(usize, &str)> = obj
            .symbols()
            .filter_map(symbol_addr_name)
            .filter(|(_, sym)| is_valid_symbol(sym))
            .collect();

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

        // insert entrypoint into known symbols
        let entrypoint = obj.entry() as usize;
        println!("entrypoint {entrypoint:#x}");

        // iterator of just the entrypoint.
        // (first as it may be replaced by a symbol with the same address)
        let entrypoint = std::iter::once((
            entrypoint,
            Arc::new(demangler::TokenStream::simple("entry")),
        ));

        let symbols = symbols
            .iter()
            .map(|(addr, symbol)| (*addr, Arc::new(parser(symbol))));

        Ok(Self {
            tree: entrypoint.chain(symbols).collect(),
        })
    }

    pub fn symbols(&self) -> std::collections::btree_map::Values<usize, Arc<TokenStream>> {
        self.tree.values()
    }

    pub fn is_empty(&self) -> bool {
        self.tree.is_empty()
    }

    pub fn get_by_addr(&self, addr: usize) -> Option<&TokenStream> {
        self.tree.get(&addr).map(Arc::as_ref)
    }
}

fn symbol_addr_name<'sym>(symbol: object::Symbol<'sym, 'sym>) -> Option<(usize, &'sym str)> {
    if let Ok(name) = symbol.name() {
        return Some((symbol.address() as usize, name));
    }

    None
}

fn is_valid_symbol(s: &str) -> bool {
    !s.starts_with("GCC_except_table") && !s.contains("cgu") && !s.is_empty()
}
