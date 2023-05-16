use object::{Object, ObjectSymbol};
use object::read::pe::ImageNtHeaders;
use pdb::FallibleIterator;
use std::collections::BTreeMap;
use std::sync::Arc;

use demangler::TokenStream;

fn parser(s: &str) -> TokenStream {
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
}

#[derive(Debug)]
pub struct Index {
    pub tree: BTreeMap<usize, Arc<TokenStream>>,
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

        // insert entrypoint into known symbols
        let entrypoint = obj.entry() as usize;
        println!("entrypoint {entrypoint:#x}");

        // insert entrypoint and override it if it's got a defined name
        self.tree.insert(
            entrypoint,
            Arc::new(demangler::TokenStream::simple("entry")),
        );

        // insert defined symbols
        let map_symbol = |(addr, symbol): &(usize, &str)| (*addr, Arc::new(parser(symbol)));
        self.tree.extend(symbols.iter().map(map_symbol));

        Ok(())
    }

    // NOTE: import table get's filled in at runtime.
    // TODO: the export table should be explored using binary search if the hint flag isn't yet
    pub fn parse_imports<H: ImageNtHeaders>(&mut self, binary: &[u8]) -> object::Result<()> {
        use object::read::pe::ImageThunkData;
        use object::LittleEndian as LE;

        let obj = object::read::pe::PeFile::<H>::parse(&binary[..])?;

        if let Some(import_table) = obj.import_table()? {
            let mut import_descs = import_table.descriptors()?;
            while let Some(import_desc) = import_descs.next()? {
                // let library = import_table.name(import_desc.name.get(LE))?;
                // println!("library: {}", String::from_utf8_lossy(library));

                let first_thunk = import_desc.first_thunk.get(LE);
                let original_first_thunk = import_desc.original_first_thunk.get(LE);

                let thunk = if first_thunk == 0 {
                    original_first_thunk
                } else  {
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
