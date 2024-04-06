use std::path::Path;
use std::sync::Arc;
use crate::intern::InternMap;
use crate::{AddressMap, Addressed, FileAttr, Symbol};
use crossbeam_queue::SegQueue;
use object::Object;
use pdb::{SymbolData, FallibleIterator};

#[derive(Default)]
pub struct PDB {
    /// Mapping from addresses starting at the header base to source files.
    pub file_attrs: AddressMap<FileAttr>,

    pub functions: AddressMap<Arc<Symbol>>,
}

impl PDB {
    pub fn parse(obj: &object::File) -> Option<Result<Self, pdb::Error>> {
        fn open_pdb(obj: &object::File) -> Option<std::fs::File> {
            let pdb = obj.pdb_info().ok()??;
            let path = std::str::from_utf8(pdb.path()).ok()?;
            std::fs::File::open(path).ok()
        }

        open_pdb(obj).map(|file| parse_pdb(obj, file))
    }
}

fn parse_pdb(obj: &object::File, file: std::fs::File) -> Result<PDB, pdb::Error> {
    let mut mangled_funcs = AddressMap::default();
    let base_addr = obj.relative_address_base() as usize;

    let mut pdb = pdb::PDB::open(file)?;

    // mapping from offset's to rva's
    let address_map = Arc::new(pdb.address_map()?);

    // mapping from string ref's to strings
    let string_table = Arc::new(pdb.string_table()?);

    // pdb module's
    let dbi = pdb.debug_information()?;
    let mut modules = dbi.modules()?;

    // create concurrent hashmap for caching file path's
    let path_cache = Arc::new(InternMap::new());

    // iterate over the modules and store them in a queue
    let module_info_queue = Arc::new(SegQueue::new());
    let mut idx = 0;
    while let Some(module) = modules.next()? {
        let module_info = match pdb.module_info(&module)? {
            Some(info) => info,
            None => continue,
        };

        module_info_queue.push((idx, module_info));
        idx += 1;
    }

    log::PROGRESS.set("Parsing pdb.", module_info_queue.len());

    // parse local symbols
    let mut file_attrs = AddressMap::default();
    std::thread::scope(|s| -> Result<_, pdb::Error> {
        let thread_count = std::thread::available_parallelism().unwrap().get();
        let threads: Vec<_> = (0..thread_count)
            .map(|_| {
                let path_cache = Arc::clone(&path_cache);
                let address_map = Arc::clone(&address_map);
                let string_table = Arc::clone(&string_table);
                let module_info_queue = Arc::clone(&module_info_queue);

                s.spawn(move || -> Result<_, pdb::Error> {
                    let mut symbols = AddressMap::default();
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
                            &mut symbols,
                        )?;
                        log::PROGRESS.step();
                    }

                    Ok((file_attrs, symbols))
                })
            })
            .collect();

        for thread in threads {
            let (local_file_attrs, local_functions) = thread.join().unwrap()?;
            file_attrs.extend(local_file_attrs);
            mangled_funcs.extend(local_functions);
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

    // iterate through global symbols
    let global_symbols = pdb.global_symbols()?;
    let mut symbol_table = global_symbols.iter();

    // parse global symbols
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

                mangled_funcs.push(Addressed {
                    addr: base_addr + addr,
                    item: name,
                });
            }
            Ok(_) => {
                // TODO: implement support for other types of symbols
            },
            Err(pdb::Error::UnimplementedSymbolKind(_)) => {},
            Err(err) => log::complex!(
                w "[index::pdb::parse] ",
                y format!("{err}."),
            )
        };
    }

    // insert defined symbols
    log::PROGRESS.set("Parsing symbols.", mangled_funcs.len());
    let mut symbols = AddressMap::default();
    crate::common::parallel_compute(mangled_funcs.mapping, &mut symbols, |func| {
        let demangled_func = Symbol::new(crate::demangler::parse(func.item));
        log::PROGRESS.step();
        Addressed {
            addr: func.addr,
            item: Arc::new(demangled_func),
        }
    });

    Ok(PDB {
        file_attrs,
        functions: symbols,
    })
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
    functions: &mut AddressMap<&'data str>,
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

                functions.push(Addressed {
                    addr: base_addr + addr,
                    item: name,
                });
            },
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
            Err(pdb::Error::UnimplementedSymbolKind(_)) => {},
            Err(err) => log::complex!(
                w "[index::pdb::parse_module] ",
                y format!("{err}."),
            )
        }
    }

    Ok(())
}
