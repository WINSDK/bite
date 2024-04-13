use crate::intern::InternMap;
use crate::{AddressMap, Addressed, FileAttr};
use crossbeam_queue::SegQueue;
use object::Object;
use pdb::{FallibleIterator, SymbolData};
use std::path::Path;
use std::pin::Pin;
use std::sync::Arc;

pub struct PDB<'data> {
    /// Mapping from addresses starting at the header base to source files.
    pub file_attrs: AddressMap<FileAttr>,
    /// Container for holding syms.
    global_syms: pdb::SymbolTable<'data>,
    /// Mapping from addresses starting at the header base to functions.
    pub syms: AddressMap<&'data str>,
}

impl<'data> PDB<'data> {
    pub fn parse(obj: &object::File<'data>) -> Option<Result<Pin<Box<Self>>, pdb::Error>> {
        fn open_pdb(obj: &object::File) -> Option<std::fs::File> {
            let pdb = obj.pdb_info().ok()??;
            let path = std::str::from_utf8(pdb.path()).ok()?;
            std::fs::File::open(path).ok()
        }

        open_pdb(obj).map(|file| parse_pdb(obj, file))
    }
}

fn parse_pdb<'data>(
    obj: &object::File<'data>,
    file: std::fs::File,
) -> Result<Pin<Box<PDB<'data>>>, pdb::Error> {
    let base_addr = obj.relative_address_base() as usize;
    let mut pdb = pdb::PDB::open(file)?;

    let mut this = Box::pin(PDB {
        file_attrs: AddressMap::default(),
        syms: AddressMap::default(),
        global_syms: pdb.global_symbols()?,
    });

    // Mapping from offset's to rva's.
    let address_map = pdb.address_map()?;

    // Mapping from string ref's to strings.
    let string_table = pdb.string_table()?;

    // PDB module's.
    let dbi = pdb.debug_information()?;
    let mut modules = dbi.modules()?;

    // Create concurrent interner for caching file path's.
    let path_cache = InternMap::new();

    // Iterate over the modules and store them in a queue.
    let module_info_queue = SegQueue::new();
    let mut id = 0;
    while let Some(module) = modules.next()? {
        let module_info = match pdb.module_info(&module)? {
            Some(info) => info,
            None => continue,
        };

        module_info_queue.push((id, module_info));
        id += 1;
    }

    log::PROGRESS.set("Parsing pdb.", module_info_queue.len());

    // Parse local symbols.
    let mut file_attrs = AddressMap::default();
    std::thread::scope(|s| -> Result<_, pdb::Error> {
        let thread_count = std::thread::available_parallelism().unwrap().get();
        let threads: Vec<_> = (0..thread_count)
            .map(|_| {
                s.spawn(|| -> Result<_, pdb::Error> {
                    let mut syms = AddressMap::default();
                    let mut file_attrs = AddressMap::default();

                    while let Some((module_id, module_info)) = module_info_queue.pop() {
                        parse_pdb_module(
                            module_id,
                            base_addr,
                            &path_cache,
                            module_info,
                            &address_map,
                            &string_table,
                            &mut file_attrs,
                            &mut syms,
                        )?;
                        log::PROGRESS.step();
                    }

                    Ok((file_attrs, syms))
                })
            })
            .collect();

        for thread in threads {
            let (local_file_attrs, local_syms) = thread.join().unwrap()?;
            file_attrs.extend(local_file_attrs);
            this.syms.extend(local_syms);
        }

        Ok(())
    })?;

    if path_cache.len() != 0 {
        log::complex!(
            w "[index::pdb::parse] indexed ",
            g path_cache.len().to_string(),
            w " source files."
        );
    }

    // Iterate through global symbols.
    // SAFETY: this is fine because global_syms is pinned as part of the PDB.
    let mut symbol_table: pdb::SymbolIter<'data> = 
        unsafe { std::mem::transmute(this.global_syms.iter()) };

    // Parse global symbols.
    while let Some(symbol) = symbol_table.next()? {
        match symbol.parse() {
            Ok(SymbolData::Public(symbol)) if symbol.function => {
                let addr = match symbol.offset.to_rva(&address_map) {
                    Some(rva) => rva.0 as usize,
                    None => continue,
                };

                let name = match std::str::from_utf8(symbol.name.as_bytes()) {
                    Ok(name) => name,
                    Err(_) => continue,
                };

                this.syms.push(Addressed {
                    addr: base_addr + addr,
                    item: name,
                });
            }
            Ok(_) => {
                // TODO: implement support for other types of symbols
            }
            Err(pdb::Error::UnimplementedSymbolKind(_)) => {}
            Err(err) => log::complex!(
                w "[index::pdb::parse] ",
                y format!("{err}."),
            ),
        };
    }

    Ok(this)
}

#[allow(clippy::too_many_arguments)]
fn parse_pdb_module<'data>(
    module_id: u64,
    base_addr: usize,
    path_cache: &InternMap<u64, Path>,
    module_info: pdb::ModuleInfo,
    address_map: &pdb::AddressMap,
    string_table: &pdb::StringTable<'data>,
    file_attrs: &mut AddressMap<FileAttr>,
    syms: &mut AddressMap<&'data str>,
) -> Result<(), pdb::Error> {
    let program = module_info.line_program()?;
    let mut symbols = module_info.symbols()?;

    while let Some(symbol) = symbols.next()? {
        match symbol.parse() {
            Ok(SymbolData::Public(symbol)) if symbol.function => {
                let addr = match symbol.offset.to_rva(address_map) {
                    Some(rva) => rva.0 as usize,
                    None => continue,
                };

                let name: &'data str = match std::str::from_utf8(symbol.name.as_bytes()) {
                    Ok(name) => unsafe { std::mem::transmute(name) },
                    Err(_) => continue,
                };

                syms.push(Addressed {
                    addr: base_addr + addr,
                    item: name,
                });
            }
            Ok(SymbolData::Procedure(proc)) => {
                let mut lines = program.lines_for_symbol(proc.offset);
                while let Some(line_info) = lines.next()? {
                    let addr = match line_info.offset.to_rva(address_map) {
                        Some(rva) => rva.0 as usize,
                        None => continue,
                    };

                    // try to use cached path if possible, prevents extra allocations
                    let key = module_id << 48 | line_info.file_index.0 as u64;
                    let path = match path_cache.get(&key) {
                        Some(cached_path) => cached_path,
                        None => {
                            // compute path for the given line
                            let file_info = program.get_file_info(line_info.file_index)?;
                            let file_name = file_info.name.to_raw_string(string_table)?;
                            let path = match std::str::from_utf8(file_name.as_bytes()) {
                                Ok(file_name) => Arc::from(Path::new(file_name)),
                                Err(_) => continue,
                            };

                            path_cache.add(key, &path)
                        }
                    };

                    let column_start = line_info.column_start.unwrap_or(0) as usize;
                    let column_end = line_info.column_end.unwrap_or(0) as usize;

                    file_attrs.push(Addressed {
                        addr: base_addr + addr,
                        item: FileAttr {
                            path,
                            line: line_info.line_start as usize,
                            column_start,
                            column_end,
                        },
                    });
                }
            }
            Ok(_) => {
                // TODO: implement support for other types of symbols
            }
            Err(pdb::Error::UnimplementedSymbolKind(_)) => {}
            Err(err) => log::complex!(
                w "[index::pdb::parse_module] ",
                y format!("{err}."),
            ),
        }
    }

    Ok(())
}
