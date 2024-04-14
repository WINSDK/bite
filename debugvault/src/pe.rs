use crate::{AddressMap, Addressed, RawSymbol};
use object::read::pe::{ImageNtHeaders, ImageThunkData, PeFile};
use object::LittleEndian as LE;
use object::Object;

pub struct PeDebugInfo<'data, Pe: ImageNtHeaders> {
    /// Parsed PE32/64 header.
    obj: &'data PeFile<'data, Pe>,
    /// Any parsed but not yet relocated symbols.
    pub syms: AddressMap<RawSymbol<'data>>,
}

impl<'data, Pe: ImageNtHeaders> PeDebugInfo<'data, Pe> {
    pub fn parse(obj: &'data PeFile<'data, Pe>) -> Result<Self, object::Error> {
        let mut this = Self {
            obj,
            syms: AddressMap::default(),
        };
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
                            func_rva += std::mem::size_of::<Pe::ImageThunkData>() as u32;
                            continue;
                        }
                    };

                    let name = match std::str::from_utf8(name) {
                        Ok(name) => name,
                        Err(..) => {
                            // skip over an entry
                            func_rva += std::mem::size_of::<Pe::ImageThunkData>() as u32;
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
                func_rva += std::mem::size_of::<Pe::ImageThunkData>() as u32;
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
