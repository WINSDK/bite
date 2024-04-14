use object::{Object, ObjectSection, ObjectSymbol};
use processor_shared::{AddressMap, Addressed};

pub mod elf;
pub mod macho;
pub mod pe;

pub struct RawSymbol<'data> {
    pub name: &'data str,
    pub module: Option<&'data str>,
}

fn parse_symbol_table<'data, Obj: Object<'data, 'data>>(
    obj: &'data Obj,
) -> AddressMap<RawSymbol<'data>> {
    let mut syms = AddressMap::default();
    for sym in obj.symbols() {
        match sym.name() {
            Ok(name) => syms.push(Addressed {
                addr: sym.address() as usize,
                item: RawSymbol { name, module: None },
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

fn parse_section_generics<'data, Obj: ObjectSection<'data>>(
    section: &Obj,
) -> (String, &'static [u8], usize, usize) {
    let name = match section.name() {
        Ok(name) => name,
        Err(_) => {
            log::complex!(
                w "[macho::parse_sections] ",
                y "Failed to read name.",
            );
            "unknown"
        }
    };

    let bytes: &'static [u8] = match section.data() {
        // The file is memory mapped so only the bytes are of lifetime &'static [u8].
        Ok(data) => unsafe { std::mem::transmute(data) },
        Err(..) => {
            log::complex!(
                w "[macho::parse_sections] ",
                y "Failed to read section ",
                b name,
                y "."
            );
            &[]
        }
    };

    let start = section.address() as usize;
    let end = bytes.len() + start;

    (name.to_string(), bytes, start, end)
}
