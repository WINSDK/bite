use std::sync::Arc;
use object::Object;
use object::read::pe::{ImageNtHeaders, ImageThunkData, PeFile};
use object::LittleEndian as LE;
use crate::demangler::TokenStream;
use crate::{AddressMap, Addressed, Symbol};

pub struct PeDebugInfo<'data, Pe: ImageNtHeaders> {
    obj: &'data PeFile<'data, Pe>,
}

impl<'data, Pe: ImageNtHeaders> PeDebugInfo<'data, Pe> {
    pub fn parse(obj: &'data PeFile<'data, Pe>) -> Self {
        Self { obj, }
    }

    pub fn imports(&self) -> Result<AddressMap<Arc<Symbol>>, object::Error> {
        let mut functions = AddressMap::default();

        if let Some(import_table) = self.obj.import_table()? {
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

                        let module = String::from_utf8_lossy(module);
                        let module = module.strip_prefix(".dll").unwrap_or(&module).to_owned();
                        let func = Symbol::new(crate::demangler::parse(name))
                            .with_module(module)
                            .into_import();

                        functions.push(Addressed {
                            addr: addr as usize,
                            item: Arc::new(func),
                        });
                    }

                    // skip over an entry
                    func_rva += std::mem::size_of::<Pe::ImageThunkData>() as u32;
                }
            }
        }

        Ok(functions)
    }

    pub fn symbols(&self) -> Result<AddressMap<Arc<Symbol>>, object::Error> {
        let mut symbols = crate::parse_symbol_names(self.obj)?;
        let entrypoint = self.obj.entry();

        let entry_func = Symbol::new(TokenStream::simple("entry"));
        symbols.push(Addressed {
            addr: entrypoint as usize,
            item: Arc::new(entry_func),
        });

        Ok(symbols)
    }
}
