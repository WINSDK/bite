use std::path::{Path, PathBuf};
use std::sync::Arc;
use crate::intern::InternMap;
use crate::FileAttr;
use crossbeam_queue::SegQueue;
use object::Object;
use object::ObjectSection;

pub struct Dwarf {
    /// Mapping from addresses starting at the header base to source files.
    pub file_attrs: Vec<(usize, FileAttr)>,
}

impl Dwarf {
    pub fn parse<'data, O: Object<'data, 'data>>(obj: &'data O) -> Result<Dwarf, gimli::Error> {
        let endian = if obj.is_little_endian() {
            gimli::RunTimeEndian::Little
        } else {
            gimli::RunTimeEndian::Big
        };

        // Load a section (doesn't work when stuff is compressed sorry lol).
        let load_section = |id: gimli::SectionId| -> Result<&[u8], gimli::Error> {
            let target_name = id.name();
            match obj.section_by_name(target_name) {
                Some(section) => Ok(section.data().unwrap_or_default()),
                None => Ok(&[]),
            }
        };

        // Load all of the sections.
        let dwarf_ref = gimli::Dwarf::load(&load_section)?;

        // Create `EndianSlice`s for all of the sections.
        let dwarf = Arc::new(dwarf_ref.borrow(|section| gimli::EndianSlice::new(section, endian)));

        // Iterate over the compilation units and store them in a queue.
        let header_queue = Arc::new(SegQueue::new());
        let mut iter = dwarf.units();
        let mut idx = 0;
        while let Some(header) = iter.next()? {
            header_queue.push((idx, header));
            idx += 1;
        }

        // Create concurrent interner for caching file path's.
        let path_cache = Arc::new(InternMap::new());

        log::PROGRESS.set("Parsing dwarf.", header_queue.len());

        let mut file_attrs = Vec::new();
        std::thread::scope(|s| -> Result<_, gimli::Error> {
            let thread_count = std::thread::available_parallelism().unwrap().get();
            let threads: Vec<_> = (0..thread_count)
                .map(|_| {
                    let path_cache = Arc::clone(&path_cache);
                    let dwarf = Arc::clone(&dwarf);
                    let header_queue = Arc::clone(&header_queue);

                    s.spawn(move || -> Result<_, gimli::Error> {
                        let mut file_attrs = Vec::new();

                        while let Some((header_id, header)) = header_queue.pop() {
                            parse_dwarf_unit(
                                header_id,
                                &path_cache,
                                &dwarf,
                                header,
                                &mut file_attrs
                            )?;
                            log::PROGRESS.step();
                        }

                        Ok(file_attrs)
                    })
                })
                .collect();

            for thread in threads {
                file_attrs.extend(thread.join().unwrap()?);
            }

            Ok(())
        })?;

        // Keep files sorted so they can be binary searched ????.
        file_attrs.sort_unstable_by_key(|(addr, _)| *addr);

        if path_cache.len() != 0 {
            log::complex!(
                w "[index::parse_dwarf] indexed ",
                g path_cache.len().to_string(),
                w " source files."
            );
        }

        Ok(Self { file_attrs })
    }
}

type GimliSlice<'a> = gimli::EndianSlice<'a, gimli::RunTimeEndian>;

fn parse_dwarf_unit(
    header_id: u64,
    path_cache: &InternMap<u64, Path>,
    dwarf: &gimli::Dwarf<GimliSlice>,
    header: gimli::UnitHeader<GimliSlice>,
    file_attrs: &mut Vec<(usize, FileAttr)>,
) -> Result<(), gimli::Error> {
    let unit = dwarf.unit(header)?;
    let program = match unit.line_program {
        Some(ref program) => program.clone(),
        None => return Ok(()),
    };

    let comp_dir = unit
        .comp_dir
        .map(|dir| PathBuf::from(dir.to_string().unwrap_or_default()))
        .unwrap_or_default();

    // Iterate over the line program rows.
    let mut rows = program.rows();
    while let Some((header, row)) = rows.next_row()? {
        // End of sequence indicates a possible gap in addresses.
        if row.end_sequence() {
            continue;
        }

        let (file, line) = match (row.file(header), row.line()) {
            (Some(file), Some(line)) => (file, line),
            _ => continue,
        };

        // Try to use cached path if possible, prevents extra allocations.
        let key = header_id << 48 | row.file_index() << 24 | file.directory_index();
        let path = match path_cache.get(&key) {
            Some(cached_path) => cached_path,
            None => {
                // compute path for the given line
                let mut path = comp_dir.clone();

                let dir_idx = file.directory_index() as usize;
                if dir_idx != 0 {
                    if let Some(dir) = file.directory(header) {
                        if let Ok(path_comp) = dwarf.attr_string(&unit, dir)?.to_string() {
                            path.push(path_comp);
                        }
                    }
                }

                if let Ok(path_comp) = dwarf.attr_string(&unit, file.path_name())?.to_string() {
                    path.push(path_comp);
                }

                path_cache.add(key, &path)
            }
        };

        file_attrs.push((
            row.address() as usize,
            FileAttr {
                path,
                line: line.get() as usize,
            },
        ))
    }

    Ok(())
}
