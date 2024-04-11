use crate::intern::InternMap;
use crate::{AddressMap, Addressed, FileAttr};
use object::{Object, ObjectSection, ObjectSymbol};
use rustc_hash::FxHasher;
use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use std::io;
use std::path::{Path, PathBuf};
use typed_arena::Arena;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Gimli(gimli::Error),
    Object(object::read::Error),
    Loading(io::Error),
}

impl From<gimli::Error> for Error {
    fn from(err: gimli::Error) -> Self {
        Error::Gimli(err)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Loading(err)
    }
}

impl From<object::read::Error> for Error {
    fn from(err: object::read::Error) -> Self {
        Error::Object(err)
    }
}

pub struct Dwarf {
    /// Mapping from addresses starting at the header base to source files.
    pub file_attrs: AddressMap<FileAttr>,
}

impl Dwarf {
    pub fn parse(obj: &object::File) -> Result<Self> {
        let endian = if obj.is_little_endian() {
            gimli::RunTimeEndian::Little
        } else {
            gimli::RunTimeEndian::Big
        };

        let arena_data = Arena::new();
        let arena_relocations = Arena::new();

        let mut load_section = |id: gimli::SectionId| {
            load_file_section(id, obj, endian, &arena_data, &arena_relocations)
        };

        let mut dwarf = gimli::Dwarf::load(&mut load_section)?;
        dwarf.populate_abbreviations_cache(gimli::AbbreviationsCacheStrategy::All);
        let file_attrs = dump_line(&dwarf)?;

        Ok(Dwarf { file_attrs })
    }

    pub fn load(path: &Path) -> Result<Self> {
        let file = std::fs::File::open(path)?;
        let mmap = unsafe { memmap2::Mmap::map(&file)? };
        let obj = object::File::parse(&*mmap)?;
        Self::parse(&obj)
    }

    pub fn merge(&mut self, other: Self) {
        self.file_attrs.extend(other.file_attrs);
    }
}

trait Reader: gimli::Reader<Offset = usize> + Send + Sync {}

impl<'input, Endian: gimli::Endianity + Send + Sync> Reader for gimli::EndianSlice<'input, Endian> {}

type RelocationMap = HashMap<usize, object::Relocation, BuildHasherDefault<FxHasher>>;

fn add_relocations(
    relocations: &mut RelocationMap,
    file: &object::File,
    section: &object::Section,
) {
    for (offset64, mut relocation) in section.relocations() {
        let offset = offset64 as usize;
        if offset as u64 != offset64 {
            continue;
        }
        // There are other things we could match but currently don't
        #[allow(clippy::single_match)]
        match relocation.kind() {
            object::RelocationKind::Absolute => {
                match relocation.target() {
                    object::RelocationTarget::Symbol(symbol_idx) => {
                        match file.symbol_by_index(symbol_idx) {
                            Ok(symbol) => {
                                let addend =
                                    symbol.address().wrapping_add(relocation.addend() as u64);
                                relocation.set_addend(addend as i64);
                            }
                            Err(_) => {
                                log::complex!(
                                    w "[dwarf::add_relocations] ",
                                    y "Relocation with invalid symbol for section ",
                                    b section.name().unwrap(),
                                    y " at offset ",
                                    g format!("0x{offset:08x}"),
                                    w "."
                                );
                            }
                        }
                    }
                    _ => {}
                }
                if relocations.insert(offset, relocation).is_some() {
                    log::complex!(
                        w "[dwarf::add_relocations] ",
                        y "Multiple relocations for section ",
                        b section.name().unwrap(),
                        y " at offset ",
                        g format!("0x{offset:08x}"),
                        w "."
                    );
                }
            }
            _ => {
                log::complex!(
                    w "[dwarf::add_relocations] ",
                    y "Unsupported relocation for section ",
                    b section.name().unwrap(),
                    y " at offset ",
                    g format!("0x{offset:08x}"),
                    w "."
                );
            }
        }
    }
}

/// Apply relocations to addresses and offsets during parsing,
/// instead of requiring the data to be fully relocated prior
/// to parsing.
///
/// Pros
/// - allows readonly buffers, we don't need to implement writing of values back to buffers
/// - potentially allows us to handle addresses and offsets differently
/// - potentially allows us to add metadata from the relocation (eg symbol names)
/// Cons
/// - maybe incomplete
#[derive(Debug, Clone)]
struct Relocate<'a, R: gimli::Reader<Offset = usize>> {
    relocations: &'a RelocationMap,
    section: R,
    reader: R,
}

impl<'a, R: gimli::Reader<Offset = usize>> Relocate<'a, R> {
    fn relocate(&self, offset: usize, value: u64) -> u64 {
        if let Some(relocation) = self.relocations.get(&offset) {
            // There are other things we could match but currently don't
            #[allow(clippy::single_match)]
            match relocation.kind() {
                object::RelocationKind::Absolute => {
                    if relocation.has_implicit_addend() {
                        // Use the explicit addend too, because it may have the symbol value.
                        return value.wrapping_add(relocation.addend() as u64);
                    } else {
                        return relocation.addend() as u64;
                    }
                }
                _ => {}
            }
        };
        value
    }
}

impl<'a, R: gimli::Reader<Offset = usize>> gimli::Reader for Relocate<'a, R> {
    type Endian = R::Endian;
    type Offset = R::Offset;

    fn read_address(&mut self, address_size: u8) -> gimli::Result<u64> {
        let offset = self.reader.offset_from(&self.section);
        let value = self.reader.read_address(address_size)?;
        Ok(self.relocate(offset, value))
    }

    fn read_length(&mut self, format: gimli::Format) -> gimli::Result<usize> {
        let offset = self.reader.offset_from(&self.section);
        let value = self.reader.read_length(format)?;
        <usize as gimli::ReaderOffset>::from_u64(self.relocate(offset, value as u64))
    }

    fn read_offset(&mut self, format: gimli::Format) -> gimli::Result<usize> {
        let offset = self.reader.offset_from(&self.section);
        let value = self.reader.read_offset(format)?;
        <usize as gimli::ReaderOffset>::from_u64(self.relocate(offset, value as u64))
    }

    fn read_sized_offset(&mut self, size: u8) -> gimli::Result<usize> {
        let offset = self.reader.offset_from(&self.section);
        let value = self.reader.read_sized_offset(size)?;
        <usize as gimli::ReaderOffset>::from_u64(self.relocate(offset, value as u64))
    }

    #[inline]
    fn split(&mut self, len: Self::Offset) -> gimli::Result<Self> {
        let mut other = self.clone();
        other.reader.truncate(len)?;
        self.reader.skip(len)?;
        Ok(other)
    }

    // All remaining methods simply delegate to `self.reader`.

    #[inline]
    fn endian(&self) -> Self::Endian {
        self.reader.endian()
    }

    #[inline]
    fn len(&self) -> Self::Offset {
        self.reader.len()
    }

    #[inline]
    fn empty(&mut self) {
        self.reader.empty()
    }

    #[inline]
    fn truncate(&mut self, len: Self::Offset) -> gimli::Result<()> {
        self.reader.truncate(len)
    }

    #[inline]
    fn offset_from(&self, base: &Self) -> Self::Offset {
        self.reader.offset_from(&base.reader)
    }

    #[inline]
    fn offset_id(&self) -> gimli::ReaderOffsetId {
        self.reader.offset_id()
    }

    #[inline]
    fn lookup_offset_id(&self, id: gimli::ReaderOffsetId) -> Option<Self::Offset> {
        self.reader.lookup_offset_id(id)
    }

    #[inline]
    fn find(&self, byte: u8) -> gimli::Result<Self::Offset> {
        self.reader.find(byte)
    }

    #[inline]
    fn skip(&mut self, len: Self::Offset) -> gimli::Result<()> {
        self.reader.skip(len)
    }

    #[inline]
    fn to_slice(&self) -> gimli::Result<Cow<[u8]>> {
        self.reader.to_slice()
    }

    #[inline]
    fn to_string(&self) -> gimli::Result<Cow<str>> {
        self.reader.to_string()
    }

    #[inline]
    fn to_string_lossy(&self) -> gimli::Result<Cow<str>> {
        self.reader.to_string_lossy()
    }

    #[inline]
    fn read_slice(&mut self, buf: &mut [u8]) -> gimli::Result<()> {
        self.reader.read_slice(buf)
    }
}

impl<'a, R: Reader> Reader for Relocate<'a, R> {}

fn load_file_section<'input, 'arena, Endian: gimli::Endianity>(
    id: gimli::SectionId,
    file: &object::File<'input>,
    endian: Endian,
    arena_data: &'arena Arena<Cow<'input, [u8]>>,
    arena_relocations: &'arena Arena<RelocationMap>,
) -> Result<Relocate<'arena, gimli::EndianSlice<'arena, Endian>>> {
    let mut relocations = RelocationMap::default();
    let name = if file.format() == object::BinaryFormat::Xcoff {
        id.xcoff_name()
    } else {
        Some(id.name())
    };

    let data = match name.and_then(|name| file.section_by_name(name)) {
        Some(ref section) => {
            add_relocations(&mut relocations, file, section);
            section.uncompressed_data()?
        }
        // Use a non-zero capacity so that `ReaderOffsetId`s are unique.
        None => Cow::Owned(Vec::with_capacity(1)),
    };
    let data_ref = arena_data.alloc(data);
    let reader = gimli::EndianSlice::new(data_ref, endian);
    let section = reader;
    let relocations = arena_relocations.alloc(relocations);
    Ok(Relocate {
        relocations,
        section,
        reader,
    })
}

fn dump_line<R: Reader>(dwarf: &gimli::Dwarf<R>) -> Result<AddressMap<FileAttr>> {
    let mut iter = dwarf.units();
    let mut file_attrs = AddressMap::default();
    let path_cache = InternMap::new();

    let mut id = 0;
    while let Some(header) = iter.next()? {
        let unit = match dwarf.unit(header) {
            Ok(unit) => unit,
            Err(err) => {
                log::complex!(
                    w "[dwarf::dump_line] ",
                    y "Failed to parse unit root entry for dump_line: ",
                    y format!("{err:?}"),
                    w ".",
                );
                continue;
            }
        };
        match dump_line_program(id, &path_cache, &unit, dwarf) {
            Ok(program_file_attrs) => {
                file_attrs.extend(program_file_attrs);
            }
            Err(err) => {
                log::complex!(
                    w "[dwarf::dump_line_program] ",
                    y "Failed to dump line program: ",
                    y format!("{err:?}"),
                    w ".",
                );
            }
        }
        id += 1;
    }
    if path_cache.len() > 0 {
        log::complex!(
            w "[dwarf::dump_line] indexed ",
            g path_cache.len().to_string(),
            w " source files."
        );
    }
    Ok(file_attrs)
}

fn dump_line_program<R: Reader>(
    id: u64,
    path_cache: &InternMap<u64, Path>,
    unit: &gimli::Unit<R>,
    dwarf: &gimli::Dwarf<R>,
) -> Result<AddressMap<FileAttr>> {
    let mut file_attrs = AddressMap::default();
    if let Some(program) = unit.line_program.clone() {
        let mut rows = program.rows();
        let comp_dir = unit
            .comp_dir
            .as_ref()
            .map(|dir| dir.to_string_lossy().unwrap_or_default().into_owned())
            .map(PathBuf::from)
            .unwrap_or_default();

        while let Some((header, row)) = rows.next_row()? {
            let line = match row.line() {
                Some(line) => line.get() as usize,
                None => 0,
            };
            let column_start = match row.column() {
                gimli::ColumnType::Column(column) => column.get() as usize - 1,
                gimli::ColumnType::LeftEdge => 0,
            };
            let column_end = match row.column() {
                gimli::ColumnType::Column(column) => column.get() as usize - 1,
                gimli::ColumnType::LeftEdge => 0,
            };
            let file = match row.file(header) {
                Some(file) => file,
                None => continue,
            };

            // Try to use cached path if possible, prevents extra allocations.
            // A path is uniquely identified by it's comp_dir, directory id and it's file id.
            let key = id << 48 | row.file_index() << 24 | file.directory_index();
            let path = match path_cache.get(&key) {
                Some(cached) => cached,
                None => {
                    let mut path = comp_dir.clone();

                    if let Some(dir) = file.directory(header) {
                        if let Ok(path_comp) = dwarf.attr_string(unit, dir)?.to_string_lossy() {
                            path.push(&*path_comp);
                        }
                    }

                    if let Ok(path_comp) = dwarf.attr_string(unit, file.path_name())?.to_string_lossy() {
                        path.push(&*path_comp);
                    }

                    path_cache.add(key, &path)
                }
            };

            file_attrs.push(Addressed {
                addr: row.address() as usize,
                item: FileAttr {
                    path,
                    line,
                    column_start,
                    column_end,
                },
            });
        }
    }

    Ok(file_attrs)
}
