use std::sync::Arc;
use object::{Endian, Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationKind, RelocationTarget};
use object::read::elf::{ElfFile, FileHeader};
use object::elf::{R_X86_64_COPY, R_X86_64_GLOB_DAT, R_X86_64_JUMP_SLOT};
use crate::demangler::TokenStream;
use crate::{AddressMap, Addressed, Symbol};

pub struct ElfDebugInfo<'data, Elf: FileHeader> {
    obj: &'data ElfFile<'data, Elf>,
}

impl<'data, Elf: FileHeader> ElfDebugInfo<'data, Elf> {
    pub fn parse(obj: &'data ElfFile<'data, Elf>) -> Self {
        Self { obj, }
    }

    pub fn imports(&self) -> Result<AddressMap<Arc<Symbol>>, object::Error> {
        let relocations = match self.obj.dynamic_relocations() {
            Some(relocations) => relocations,
            None => return Ok(AddressMap::default()),
        };

        let dyn_syms = match self.obj.dynamic_symbol_table() {
            Some(dyn_syms) => dyn_syms,
            None => return Ok(AddressMap::default()),
        };

        let mut functions = AddressMap::default();
        for (r_offset, reloc) in relocations {
            if let RelocationTarget::Symbol(idx) = reloc.target() {
                let opt_section = self.obj.sections().find(|section| {
                    (section.address()..section.address() + section.size()).contains(&r_offset)
                });

                let section = match opt_section {
                    Some(section) => section,
                    None => continue,
                };

                if let Ok(sym) = dyn_syms.symbol_by_index(idx) {
                    let name = match sym.name() {
                        Ok(name) => name,
                        Err(..) => continue,
                    };

                    let addr = match reloc.kind() {
                        // hard-coded address to function which doesn't require a relocation
                        RelocationKind::Absolute => r_offset as usize,
                        RelocationKind::Elf(R_X86_64_GLOB_DAT) => r_offset as usize,
                        RelocationKind::Elf(R_X86_64_COPY) => r_offset as usize,
                        // address in .got.plt section which contains an address to the function
                        RelocationKind::Elf(R_X86_64_JUMP_SLOT) => {
                            let width = if self.obj.is_64() { 8 } else { 4 };

                            let bytes = match section.data_range(r_offset, width) {
                                Ok(Some(bytes)) => bytes,
                                _ => continue,
                            };

                            let phys_addr = if self.obj.is_64() {
                                self.obj.endian().read_u64_bytes(bytes.try_into().unwrap()) as usize
                            } else {
                                self.obj.endian().read_u32_bytes(bytes.try_into().unwrap()) as usize
                            };

                            // idk why we need this
                            phys_addr.saturating_sub(6)
                        }
                        _ => continue,
                    };

                    // TODO: find modules
                    let func = Symbol::new(crate::demangler::parse(name)).into_import();

                    functions.push(Addressed {
                        addr,
                        item: Arc::new(func),
                    });
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
