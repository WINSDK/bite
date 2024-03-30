use std::sync::Arc;
use object::read::macho::{LoadCommandData, MachHeader, MachOFile, Nlist, SymbolTable};
use object::macho::DysymtabCommand;
use object::{Object, ObjectSection};
use crate::dwarf::Dwarf;
use crate::Function;

pub struct MachoDebugInfo<'data, Mach: MachHeader> {
    obj: &'data MachOFile<'data, Mach>,
    load_cmds: Vec<LoadCommandData<'data, Mach::Endian>>,
    symtab: Option<SymbolTable<'data, Mach>>,
    dysymtab: Option<&'data DysymtabCommand<Mach::Endian>>,
}

impl<'data, Mach: MachHeader> MachoDebugInfo<'data, Mach> {
    pub fn parse(obj: &'data MachOFile<'data, Mach>) -> Result<Self, object::Error> {
        let header = obj.raw_header();
        let mut symtab = None;
        let mut dysymtab = None;

        let mut load_cmds = Vec::new();
        let mut load_cmds_iter = header.load_commands(header.endian()?, obj.data(), 0)?;
        while let Some(lcmd) = load_cmds_iter.next()? {
            if let Some(cmd) = lcmd.symtab()? {
                symtab = Some(cmd.symbols(obj.endian(), obj.data())?);
            }
            if let Some(cmd) = lcmd.dysymtab()? {
                dysymtab = Some(cmd);
            }
            load_cmds.push(lcmd);
        }

        Ok(Self {
            obj,
            load_cmds,
            symtab,
            dysymtab,
        })
    }

    pub fn imports(&self) -> Result<Vec<(usize, Arc<Function>)>, object::Error> {
        let header = self.obj.raw_header();
        let endian = self.obj.endian();

        let mut libraries = Vec::new();
        let twolevel = header.flags(endian) & object::macho::MH_TWOLEVEL != 0;
        if twolevel {
            libraries.push(&[][..]);
            for cmd in self.load_cmds.iter() {
                if let Some(dylib) = cmd.dylib()? {
                    libraries.push(cmd.string(endian, dylib.dylib.name)?);
                }
            }
        }

        let stub = self.obj.section_by_name_bytes(b"__stubs");
        let mut functions = Vec::new();

        if let (Some(symtab), Some(dysymtab), Some(stub)) = (self.symtab, self.dysymtab, stub) {
            let idx = dysymtab.iundefsym.get(endian) as usize;
            let number = dysymtab.nundefsym.get(endian) as usize;
            for jdx in idx..(idx.wrapping_add(number)) {
                let symbol: &Mach::Nlist = symtab.symbol(jdx)?;

                let name = symbol.name(endian, symtab.strings())?;
                let name = match std::str::from_utf8(name) {
                    Ok(name) => name,
                    Err(..) => continue,
                };

                let mut func = Function::new(crate::demangler::parse(name)).as_import();
                if twolevel {
                    let library = libraries
                        .get(symbol.library_ordinal(endian) as usize)
                        .copied()
                        .unwrap_or_default();

                    // Strip path prefix.
                    let module = String::from_utf8_lossy(library);
                    let module = module
                        .rsplit_once("/")
                        .map(|x| x.1)
                        .unwrap_or(&module)
                        .to_string();

                    func = func.with_module(module);
                }

                // Each stub entry appears to be 12 bytes, so the address is calculated
                // as (__stub section addr) + (symbol index) * 12.
                let addr = stub.address() as usize + (jdx - idx) * 12;

                functions.push((addr, Arc::new(func)));
            }
        }

        Ok(functions)
    }

    pub fn dwarf(&self) -> Result<Dwarf, crate::Error> {
        Ok(Dwarf::parse(self.obj)?)
    }
}
