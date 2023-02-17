use std::borrow::Cow;
use std::collections::BTreeMap;

use object::{Object, ObjectSymbol};
use pdb::FallibleIterator;

pub type SymbolLookup<'a> = BTreeMap<usize, Cow<'a, str>>;

fn valid_symbol(symbol: &(&usize, &mut Cow<'_, str>)) -> bool {
    !symbol.1.starts_with("GCC_except_table") && !symbol.1.contains("cgu") && !symbol.1.is_empty()
}

pub fn parse<'data>(obj: &object::File<'data>) -> object::Result<SymbolLookup<'data>> {
    let mut symbols: SymbolLookup<'data> = obj
        .symbols()
        .filter_map(|s| {
            s.name()
                .ok()
                .filter(|name| !name.is_empty())
                .map(|name| (s.address() as usize, Cow::Borrowed(name)))
        })
        .collect();

    if let Ok(Some(pdb)) = obj.pdb_info() {
        let read_symbols = |f: std::fs::File| {
            let mut symbols = Vec::new();
            let mut pdb = pdb::PDB::open(f)?;

            // get symbol table
            let symbol_table = pdb.global_symbols()?;

            // leak symbols onto the heap for later use
            let symbol_table = Box::leak(Box::new(symbol_table));

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
                        symbols.push((addr.0 as usize, Cow::Borrowed(name)));
                    }
                }
            }

            Ok(symbols)
        };

        symbols.extend(
            std::str::from_utf8(pdb.path())
                .map_err(|_| std::io::ErrorKind::InvalidData.into())
                .and_then(std::fs::File::open)
                .map_err(|_| pdb::Error::UnrecognizedFileFormat)
                .and_then(read_symbols)
                .unwrap_or_default(),
        );
    }

    let parser = if crate::ARGS.simplify {
        |symbol: &Cow<str>| match super::Symbol::parse_with_config(symbol, &crate::CONFIG) {
            Ok(sym) => sym.display(),
            Err(..) => format!("{:#}", rustc_demangle::demangle(symbol)),
        }
    } else {
        |symbol: &Cow<str>| match super::Symbol::parse(symbol) {
            Ok(sym) => sym.display(),
            Err(..) => format!("{:#}", rustc_demangle::demangle(symbol)),
        }
    };

    symbols
        .iter_mut()
        .filter(valid_symbol)
        .for_each(|(_, symbol)| *symbol = Cow::Owned(parser(symbol)));

    Ok(symbols)
}
