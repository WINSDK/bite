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
    section: &'data Obj,
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
    let end = start + section.size() as usize;

    (name.to_string(), bytes, start, end)
}

pub struct Datastructure {
    pub ident: &'static str,
    pub fields: Vec<(usize, &'static str, &'static str, String)>,
}

pub trait ToData {
    fn to_fields(&self, addr: usize) -> Datastructure;
}

// FIXME: This assumes little endianness.
#[macro_export]
macro_rules! datastructure {
    (
        pub struct $name:ident {
            $($field:ident: $ftype:ty,)*
        }
    ) => {
        // Apply attributes to the struct
        #[repr(C)]
        #[derive(Copy, Clone, Debug)]
        pub struct $name {
            pub $($field: $ftype),*
        }

        impl $crate::ToData for $name {
            fn to_fields(&self, mut addr: usize) -> $crate::Datastructure {
                let mut fields = Vec::new();
                $(
                    fields.push((
                        addr,
                        stringify!($field),
                        stringify!($ftype),
                        format!("{:#x}", self.$field)
                    ));
                    #[allow(unused_assignments)]
                    { addr += ::std::mem::size_of::<$ftype>(); }
                )*
                $crate::Datastructure {
                    ident: stringify!($name),
                    fields,
                }
            }
        }

        unsafe impl object::Pod for $name {}
    };
}
