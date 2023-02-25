use std::collections::BTreeMap;

use object::{Object, ObjectSymbol};
use pdb::FallibleIterator;

pub type SymbolLookup = BTreeMap<usize, String>;

fn valid_symbol(symbol: &(&usize, &&str)) -> bool {
    !symbol.1.starts_with("GCC_except_table") && !symbol.1.contains("cgu") && !symbol.1.is_empty()
}

fn symbol_addr_name<'a>(symbol: object::Symbol<'a, 'a>) -> Option<(usize, &'a str)> {
    if let Ok(name) = symbol.name() {
        return Some((symbol.address() as usize, name));
    }

    None
}

pub fn parse(obj: &object::File) -> pdb::Result<SymbolLookup> {
    let symbols: BTreeMap<usize, &str> = obj.symbols().filter_map(symbol_addr_name).collect();

    if let Ok(Some(pdb)) = obj.pdb_info() {
        let Ok(path) = std::str::from_utf8(pdb.path()) else {
            return Ok(BTreeMap::new());
        };

        let Ok(file) = std::fs::File::open(path) else {
            return Ok(BTreeMap::new());
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

    let parser = if crate::ARGS.simplify {
        |symbol| match super::Symbol::parse_with_config(symbol, &crate::CONFIG) {
            Ok(sym) => sym.display(),
            Err(..) => format!("{:#}", rustc_demangle::demangle(symbol)),
        }
    } else {
        |symbol| match super::Symbol::parse(symbol) {
            Ok(sym) => sym.display(),
            Err(..) => format!("{:#}", rustc_demangle::demangle(symbol)),
        }
    };

    let symbols = symbols
        .iter()
        .filter(valid_symbol)
        .map(|(&addr, &symbol)| (addr, parser(symbol)))
        .collect();

    Ok(symbols)
}
