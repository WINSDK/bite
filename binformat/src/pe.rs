use crate::{datastructure, RawSymbol};
use processor_shared::{AddressMap, Addressed, Section, SectionKind};
use object::pe;
use object::read::pe::{ImageNtHeaders, ImageThunkData, PeFile};
use object::LittleEndian as LE;
use object::Object;
use std::mem::size_of;

datastructure! {
    pub struct ExceptionDirectoryEntry {
        begin_addr: u32,
        end_addr: u32,
        unwind_info: u32,
    }
}

unsafe impl object::Pod for ExceptionDirectoryEntry {}

pub struct PeDebugInfo<'data, Pe: ImageNtHeaders> {
    /// Parsed PE32/64 header.
    obj: &'data PeFile<'data, Pe>,
    /// Parsed sections with extra metadata.
    pub sections: Vec<Section>,
    /// Any parsed but not yet relocated symbols.
    pub syms: AddressMap<RawSymbol<'data>>,
}

impl<'data, Pe: ImageNtHeaders> PeDebugInfo<'data, Pe> {
    pub fn parse(obj: &'data PeFile<'data, Pe>) -> Result<Self, object::Error> {
        let mut this = Self {
            obj,
            syms: AddressMap::default(),
            sections: Vec::new(),
        };
        this.sections = parse_sections(obj);
        this.parse_symbols();
        this.parse_imports()?;
        Ok(this)
    }

    pub fn parse_imports(&mut self) -> Result<(), object::Error> {
        let import_table = match self.obj.import_table()? {
            Some(table) => table,
            None => return Ok(()),
        };

        let mut import_descs = import_table.descriptors()?;
        while let Some(import_desc) = import_descs.next()? {
            let module = import_table.name(import_desc.name.get(LE))?;
            let first_thunk = import_desc.first_thunk.get(LE);
            let original_first_thunk = import_desc.original_first_thunk.get(LE);

            let thunk = if first_thunk == 0 {
                original_first_thunk
            } else {
                first_thunk
            };

            let mut import_addr_table = import_table.thunks(thunk)?;
            let mut func_rva = first_thunk;
            while let Some(func) = import_addr_table.next::<Pe>()? {
                if !func.is_ordinal() {
                    let (hint, name) = match import_table.hint_name(func.address()) {
                        Ok(val) => val,
                        Err(..) => {
                            // skip over an entry
                            func_rva += size_of::<Pe::ImageThunkData>() as u32;
                            continue;
                        }
                    };

                    let name = match std::str::from_utf8(name) {
                        Ok(name) => name,
                        Err(..) => {
                            // skip over an entry
                            func_rva += size_of::<Pe::ImageThunkData>() as u32;
                            continue;
                        }
                    };

                    // `original_first_thunk` uses a `hint` into the export
                    // table whilst iterating thourhg regular `thunk`'s is
                    // a simple offset into the symbol export table
                    let addr = if thunk == original_first_thunk {
                        hint as u64 + self.obj.relative_address_base()
                    } else {
                        func_rva as u64 + self.obj.relative_address_base()
                    };

                    let module =
                        std::str::from_utf8(module).ok().and_then(|x| x.strip_suffix(".dll"));
                    self.syms.push(Addressed {
                        addr: addr as usize,
                        item: RawSymbol { name, module },
                    });
                }

                // skip over an entry
                func_rva += size_of::<Pe::ImageThunkData>() as u32;
            }
        }

        Ok(())
    }

    pub fn parse_symbols(&mut self) {
        self.syms.extend(crate::parse_symbol_table(self.obj));
        self.syms.push(Addressed {
            addr: self.obj.entry() as usize,
            item: RawSymbol {
                name: "entry",
                module: None,
            },
        });
    }
}

/// Common ELF dwarf section names I've found so far.
const DWARF_SECTIONS: [&str; 20] = [
    ".debug_abbrev",
    ".debug_addr",
    ".debug_aranges",
    ".debug_cu_index",
    ".debug_frame",
    ".debug_info",
    ".debug_line",
    ".debug_line_str",
    ".debug_loc",
    ".debug_loclists",
    ".debug_macinfo",
    ".debug_macro",
    ".debug_pubnames",
    ".debug_pubtypes",
    ".debug_ranges",
    ".debug_rnglists",
    ".debug_str",
    ".debug_str_offsets",
    ".debug_tu_index",
    ".debug_types",
];

fn parse_sections<'data, Pe: ImageNtHeaders>(obj: &'data PeFile<'data, Pe>) -> Vec<Section> {
    let mut sections = Vec::new();

    // Re-parsing all this data isn't amazing, all this just for getting the section headers.
    let data = obj.data();
    let dos_header = object::pe::ImageDosHeader::parse(data).unwrap();
    let mut offset = dos_header.nt_headers_offset().into();
    let (nt_headers, _) = Pe::parse(data, &mut offset).unwrap();
    let section_headers = nt_headers.sections(data, offset).unwrap();

    for (header, section) in section_headers.iter().zip(obj.sections()) {
        let (name, bytes, start, end) = crate::parse_section_generics(&section);

        let characteristics = header.characteristics.get(LE);
        let (mut kind, ident) = (SectionKind::Raw, "UNKNOWN");

        // Section contains code.
        if characteristics & pe::IMAGE_SCN_CNT_CODE != 0 {
            kind = SectionKind::Code;
        }

        // ExceptionDirectoryEntry's.
        if name == ".pdata" {
            kind = SectionKind::ExceptionDirEntry;
        }

        // Section contains DWARF debug info.
        if DWARF_SECTIONS.contains(&name.as_str()) {
            kind = SectionKind::Debug;
        }

        sections.push(Section::new(
            name,
            ident,
            kind,
            bytes,
            start,
            end
        ));
    }

    sections
}
