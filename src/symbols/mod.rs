#[allow(dead_code)]
pub mod config;

mod itanium;
mod msvc;
mod rust;
mod test;

use object::{Object, ObjectSymbol};
use pdb::FallibleIterator;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::sync::Arc;

use crate::colors::{self, Color, Token};
use crate::disassembler::Line;
use crate::threading::spawn_threaded;

pub use config::Config;

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
        let symbols: Vec<(usize, &str)> = obj
            .symbols()
            .filter_map(symbol_addr_name)
            .filter(|(_, sym)| is_valid_symbol(sym))
            .collect();

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
                        if is_valid_symbol(name) {
                            symbols.push((addr.0 as usize, name));
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
            if let Some(s) = strip_prefixes(s, &["Z", "_Z", "___Z"]) {
                return itanium::parse(s).unwrap_or_else(|| TokenStream::simple(s));
            }

            // parse rust symbols that match the v0 mangling scheme
            //
            // macOS prefixes symbols with an extra underscore therefore '__R' is allowed
            if let Some(s) = strip_prefixes(s, &["R", "_R", "__R"]) {
                return rust::parse(s).unwrap_or_else(|| TokenStream::simple(s));
            }

            // parse windows msvc C/C++ symbols
            if let Some(s) = msvc::parse(s) {
                return s;
            }

            // return the original mangled symbol on failure
            TokenStream::simple(s)
        };

        let tree = spawn_threaded(symbols, move |(addr, symbol)| {
            (addr, Arc::new(parser(symbol)))
        })
        .await;

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

    pub fn get_by_line(&self, line: &Line) -> Option<Arc<TokenStream>> {
        self.tree.get(&(line.section_base + line.offset)).cloned()
    }
}

#[derive(Debug)]
pub struct TokenStream {
    /// Unmovable string which the [Token]'s have a pointer to.
    inner: std::pin::Pin<String>,

    /// Internal token representation which is unsafe to access outside of calling [Self::tokens].
    __tokens: Vec<Token>,
}

impl TokenStream {
    fn new(s: &str) -> Self {
        Self {
            inner: std::pin::Pin::new(s.to_string()),
            __tokens: Vec::with_capacity(128),
        }
    }

    fn simple(s: &str) -> Self {
        let mut this = Self {
            inner: std::pin::Pin::new(s.to_string()),
            __tokens: Vec::with_capacity(1),
        };

        this.__tokens.push(Token {
            text: Cow::Borrowed(this.inner()),
            color: colors::BLUE,
        });

        this
    }

    /// SAFETY: must downcast &'static str to a lifetime that matches the lifetime of self.
    #[inline]
    fn inner<'a>(&self) -> &'a str {
        unsafe { std::mem::transmute(self.inner.as_ref()) }
    }

    #[inline]
    fn push(&mut self, text: &'static str, color: Color) {
        self.__tokens.push(Token {
            text: Cow::Borrowed(text),
            color,
        })
    }

    #[inline]
    fn push_cow(&mut self, text: Cow<'static, str>, color: Color) {
        self.__tokens.push(Token { text, color })
    }

    #[inline]
    pub fn tokens<'src>(&'src self) -> &'src [Token] {
        self.__tokens.as_slice()
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

fn is_valid_symbol(s: &str) -> bool {
    !s.starts_with("GCC_except_table") && !s.contains("cgu") && !s.is_empty()
}
