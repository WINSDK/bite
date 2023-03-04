pub mod config;
mod itanium;
mod msvc;
mod rust_legacy;
mod rust_modern;

use object::{Object, ObjectSymbol};
use pdb::FallibleIterator;
use std::collections::BTreeMap;

use crate::colors::Color;
use crate::disassembler::Line;
use crate::threading::spawn_threaded;

pub use config::Config;

#[derive(Debug)]
pub struct Index {
    tree: BTreeMap<usize, TokenStream>,
}

impl Index {
    pub fn new() -> Self {
        Self {
            tree: BTreeMap::new(),
        }
    }

    pub async fn parse(obj: &object::File<'_>) -> pdb::Result<Index> {
        let symbols: Vec<(usize, &str)> = obj.symbols().filter_map(symbol_addr_name).collect();

        if let Ok(Some(pdb)) = obj.pdb_info() {
            let Ok(path) = std::str::from_utf8(pdb.path()) else {
                let tree = symbols
                    .into_iter()
                    .map(|(addr, symbol)| (addr, TokenStream::new(symbol)))
                    .collect();

                return Ok(Self { tree });
            };

            let Ok(file) = std::fs::File::open(path) else {
                let tree = symbols
                    .into_iter()
                    .map(|(addr, symbol)| (addr, TokenStream::new(symbol)))
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

        let parser = |s: &str| -> TokenStream {
            // ignore common invalid symbols
            if s.starts_with("GCC_except_table") || s.contains("cgu") {
                return TokenStream::new(s);
            }

            // parse rust symbols that match the v0 mangling scheme
            if let Some(s) = strip_prefixes(s, &["__R", "_R", "R"]) {
                return rust_modern::parse(s).unwrap_or_else(|| TokenStream::new(s));
            }

            // try parsing legacy rust symbols first because they can also be appear as C++ symbols
            if let Some(s) = strip_prefixes(s, &["__ZN", "_ZN", "ZN"]) {
                if let Some(s) = rust_legacy::parse(s) {
                    return s;
                }
            }

            // parse gnu/llvm C/C++ symbols
            if let Some(s) = strip_prefixes(s, &["__Z", "_Z", "Z"]) {
                return itanium::parse(s).unwrap_or_else(|| TokenStream::new(s));
            }

            // parse windows msvc C/C++ symbols
            if let Some(s) = strip_prefixes(s, &["@?", "?"]) {
                return msvc::parse(s).unwrap_or_else(|| TokenStream::new(s));
            }

            // return the original mangled symbol on failure
            TokenStream::new(s)
        };

        let tree = spawn_threaded(symbols, move |(addr, symbol)| (addr, parser(symbol))).await;

        Ok(Self {
            tree: BTreeMap::from_iter(tree),
        })
    }

    pub fn symbols(&self) -> std::collections::btree_map::Values<usize, TokenStream> {
        self.tree.values()
    }

    pub fn is_empty(&self) -> bool {
        self.tree.is_empty()
    }

    pub fn get_by_line(&self, line: &Line) -> Option<&[Token]> {
        self.tree
            .get(&(line.section_base + line.offset))
            .map(TokenStream::tokens)
    }
}

#[derive(Debug)]
pub struct TokenStream {
    inner: std::pin::Pin<String>,
    __tokens: Vec<Token<'static>>,
}

impl TokenStream {
    pub fn new(s: &str) -> Self {
        Self {
            inner: std::pin::Pin::new(s.to_string()),
            __tokens: Vec::new(),
        }
    }

    #[inline]
    pub fn inner(&self) -> &'static str {
        unsafe { std::mem::transmute(&*self.inner) }
    }

    #[inline]
    pub fn tokens<'src>(&'src self) -> &'src [Token<'src>] {
        self.__tokens.as_slice()
    }

    #[inline]
    pub fn push(&mut self, text: &'static str, color: Color) {
        self.__tokens.push(Token { text, color })
    }
}

#[derive(Debug)]
pub struct Token<'a> {
    pub text: &'a str,
    pub color: Color,
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
