use crate::demangler::TokenStream;
use crate::dwarf::{self, Dwarf};
use crate::{AddressMap, Addressed, Symbol};
use object::macho::{self, DysymtabCommand};
use object::read::macho::{LoadCommandData, MachHeader, MachOFile, Nlist, SymbolTable};
use object::{Endianness, Object, ObjectSection, ObjectSegment};
use std::path::Path;
use std::process::Command;
use std::sync::Arc;

pub struct MachoDebugInfo<'data, Mach: MachHeader> {
    obj: &'data MachOFile<'data, Mach>,
    load_cmds: Vec<LoadCommandData<'data, Mach::Endian>>,
    symtab: Option<SymbolTable<'data, Mach>>,
    dysymtab: Option<&'data DysymtabCommand<Mach::Endian>>,
}

impl<'data, Mach: MachHeader<Endian = Endianness>> MachoDebugInfo<'data, Mach> {
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

    pub fn imports(&self) -> Result<AddressMap<Arc<Symbol>>, object::Error> {
        let header = self.obj.raw_header();
        let endian = self.obj.endian();

        let mut libraries = Vec::new();
        let twolevel = header.flags(endian) & macho::MH_TWOLEVEL != 0;
        if twolevel {
            libraries.push(&[][..]);
            for lcmd in self.load_cmds.iter() {
                if let Some(dylib) = lcmd.dylib()? {
                    libraries.push(lcmd.string(endian, dylib.dylib.name)?);
                }
            }
        }

        let stub = self.obj.section_by_name_bytes(b"__stubs");
        let mut functions = AddressMap::default();

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

                let mut func = Symbol::new(crate::demangler::parse(name)).as_import();
                if twolevel {
                    let library = libraries
                        .get(symbol.library_ordinal(endian) as usize)
                        .copied()
                        .unwrap_or_default();

                    // Strip path prefix.
                    let module = String::from_utf8_lossy(library);
                    let module =
                        module.rsplit_once("/").map(|x| x.1).unwrap_or(&module).to_string();

                    func = func.with_module(module);
                }

                // Each stub entry appears to be 12 bytes, so the address is calculated
                // as (__stub section addr) + (symbol index) * 12.
                let addr = stub.address() as usize + (jdx - idx) * 12;

                functions.push(Addressed {
                    addr,
                    item: Arc::new(func),
                });
            }
        }

        Ok(functions)
    }

    pub fn symbols(&self) -> Result<AddressMap<Arc<Symbol>>, object::Error> {
        let mut symbols = crate::parse_symbol_names(self.obj)?;

        // Macho entrypoints are relative to the __TEXT segment.
        for segment in self.obj.segments() {
            if let Some(b"__TEXT") = segment.name_bytes()? {
                let entrypoint = self.obj.entry() + segment.address();
                let entry_func = Symbol::new(TokenStream::simple("entry"));
                symbols.push(Addressed {
                    addr: entrypoint as usize,
                    item: Arc::new(entry_func),
                });
            }
        }

        Ok(symbols)
    }
}

pub fn dwarf(obj: &object::File, path: &Path) -> Result<Dwarf, dwarf::Error> {
    let mut dwarf = Dwarf::parse(obj)?;
    let opt_dsym = path
        .with_extension("dSYM")
        .join("Contents/Resources/DWARF")
        .join(path.file_name().unwrap());

    if !opt_dsym.is_file() {
        let dsymutil_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("bin/dsymutil");
        assert!(dsymutil_path.exists(), "dsymutil somehow missing");

        log::PROGRESS.set("Running dsymutil.", 1);
        let exit_status = Command::new(dsymutil_path)
            .arg("--linker=parallel")
            .arg(path)
            .spawn()?
            .wait()?;
        log::PROGRESS.step();

        if !exit_status.success() {
            log::complex!(
                w "[macho::dwarf] ",
                y "Generating dSym failed with exit code ",
                g exit_status.code().unwrap_or(1).to_string(),
                w "."
            );
        }
    }

    let dsym_dwarf = Dwarf::load(&opt_dsym)?;
    dwarf.merge(dsym_dwarf);

    Ok(dwarf)
}
