use object::{Object, ObjectSymbol};
use processor_shared::{AddressMap, Addressed};

pub mod pe;
pub mod elf;
pub mod macho;

fn parse_symbol_table<'data, O: Object<'data, 'data>>(
    obj: &'data O,
) -> AddressMap<RawSymbol<'data>> {
    let mut syms = AddressMap::default();
    for sym in obj.symbols() {
        match sym.name() {
            Ok(name) => syms.push(Addressed {
                addr: sym.address() as usize,
                item: RawSymbol {
                    name,
                    module: None,
                },
            }),
            Err(err) => {
                log::complex!(
                    w "[parse_symbol_table] ",
                    y err.to_string(),
                    y "."
                );
                continue;
            }
        }
    }
    syms
}

pub struct RawSymbol<'data> {
    pub name: &'data str,
    pub module: Option<&'data str>,
}
