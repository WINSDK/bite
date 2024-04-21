use crate::RawSymbol;
use processor_shared::{AddressMap, Addressed, Section, SectionKind};
use object::macho::{self, DyldInfoCommand, DysymtabCommand, LinkeditDataCommand};
use object::read::macho::{MachHeader, MachOFile, SymbolTable};
use object::{Endianness, Object, ObjectSection, ObjectSegment, ReadRef, SectionFlags};
use std::mem::size_of;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct DyldChainedFixupsHeader {
    /// 0
    fixups_version: u32,
    /// Offset of dyld\_chained\_starts\_in\_image in chain\_data..
    starts_offset: u32,
    /// Offset of imports table in chain_data.
    imports_offset: u32,
    /// Offset of symbol strings in chain_data.
    symbols_offset: u32,
    /// Number of imported symbol names.
    imports_count: u32,
    /// DYLD_CHAINED_IMPORT*.
    imports_format: u32,
    /// 0 => uncompressed, 1 => zlib compressed.
    symbols_format: u32,
}

/// This struct is embedded in LC_DYLD_CHAINED_FIXUPS payload.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct DyldChainedStartsInImage {
    seg_count: u32,
    // Each entry is offset into this struct for that segment.
    // followed by pool of dyld\_chain\_starts\_in\_segment data.
    seg_info_offset: [u32; 1],
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct DyldChainedStartsInSegment {
    /// size of this (amount kernel needs to copy).
    size: u32,
    /// 0x1000 or 0x4000
    page_size: u16,
    /// DYLD_CHAINED_PTR_*.
    pointer_format: u16,
    /// Offset in memory to start of segment.
    segment_offset: u64,
    /// For 32-bit OS, any value beyond this is not a pointer.
    max_valid_pointer: u32,
    /// How many pages are in the array.
    page_count: u16,
    // Each entry is offset in each page of first element in chain
    // or DYLD_CHAINED_PTR_START_NONE if no fixups on page.
    page_start: [u16; 1],
}

unsafe impl object::Pod for DyldChainedFixupsHeader {}
unsafe impl object::Pod for DyldChainedStartsInImage {}
unsafe impl object::Pod for DyldChainedStartsInSegment {}

const DYLD_CHAINED_IMPORT: u32 = 1;
const DYLD_CHAINED_IMPORT_ADDEND: u32 = 2;
const DYLD_CHAINED_IMPORT_ADDEND64: u32 = 3;

/// stride 8, unauth target is vmaddr.
const DYLD_CHAINED_PTR_ARM64E: u16 = 1;
/// Target is vmaddr.
const DYLD_CHAINED_PTR_64: u16 = 2;
const DYLD_CHAINED_PTR_32: u16 = 3;
const DYLD_CHAINED_PTR_32_CACHE: u16 = 4;
const DYLD_CHAINED_PTR_32_FIRMWARE: u16 = 5;
/// Target is vm offset.
const DYLD_CHAINED_PTR_64_OFFSET: u16 = 6;
/// Stride 4, unauth target is vm offset.
const DYLD_CHAINED_PTR_ARM64E_KERNEL: u16 = 7;
const DYLD_CHAINED_PTR_64_KERNEL_CACHE: u16 = 8;
/// Stride 8, unauth target is vm offset.
const DYLD_CHAINED_PTR_ARM64E_USERLAND: u16 = 9;
/// Stride 1, x86_64 kernel caches.
const DYLD_CHAINED_PTR_X86_64_KERNEL_CACHE: u16 = 11;
/// Stride 8, unauth target is vm offset, 24-bit bind
const DYLD_CHAINED_PTR_ARM64E_USERLAND24: u16 = 12;

/// used in page_start[] to denote a page with no fixups.
const DYLD_CHAINED_PTR_START_NONE: u16 = 0xFFFF;
/// used in page_start[] to denote a page which has multiple starts.
const DYLD_CHAINED_PTR_START_MULTI: u16 = 0x8000;
/// used in chain_starts[] to denote last start in list for page.
const DYLD_CHAINED_PTR_START_LAST: u16 = 0x8000;

#[derive(Debug, Clone, Copy)]
enum ChainedFixupPointerGeneric {
    Generic32,
    Generic64,
    GenericArm64e,
    Firmware32,
}

impl ChainedFixupPointerGeneric {
    fn bind_and_stride(&self, ptr: u64) -> (bool, u64) {
        match self {
            Self::Generic32 => ((ptr >> 31) != 0, (ptr >> 21) & 0x3FF),
            Self::Generic64 => ((ptr >> 63) != 0, (ptr >> 51) & 0xFFF),
            Self::GenericArm64e => ((ptr >> 63) != 0, (ptr >> 52) & 0x7FF),
            Self::Firmware32 => (false, (ptr >> 26) & 0x3F),
        }
    }
}

pub struct MachoDebugInfo<'data, Mach: MachHeader> {
    /// Parsed Mach-O header.
    obj: &'data MachOFile<'data, Mach>,
    /// Where the first segment starts.
    base_addr: u64,
    /// Dynamic libraries found when parsing load commands.
    dylibs: Vec<&'data str>,
    /// Any parsed but not yet relocated symbols.
    pub syms: AddressMap<RawSymbol<'data>>,
    /// Parsed sections with extra metadata.
    pub sections: Vec<Section>,
    // ---- Required load commands ----
    chained_fixups: Option<&'data LinkeditDataCommand<Mach::Endian>>,
    symtab: Option<SymbolTable<'data, Mach>>,
    dysymtab: Option<&'data DysymtabCommand<Mach::Endian>>,
    dylid_info: Option<&'data DyldInfoCommand<Mach::Endian>>,
    // --------------------------------
}

impl<'data, Mach: MachHeader<Endian = Endianness>> MachoDebugInfo<'data, Mach> {
    pub fn parse(obj: &'data MachOFile<'data, Mach>) -> Result<Self, object::Error> {
        let mut this = Self {
            obj,
            base_addr: obj.segments().next().map(|seg| seg.address()).unwrap_or(0),
            syms: AddressMap::default(),
            sections: Vec::new(),
            dylibs: Vec::new(),
            chained_fixups: None,
            symtab: None,
            dysymtab: None,
            dylid_info: None,
        };
        this.sections = parse_sections(obj);
        this.parse_base_addr()?;
        this.parse_load_cmds()?;
        this.parse_global_syms();
        if let Some(chained_fixups) = this.chained_fixups {
            parse_chained_fixups::<Mach>(
                this.base_addr,
                &mut this.syms,
                &this.dylibs,
                chained_fixups,
                this.obj.data(),
                this.obj.endian(),
            );
        }
        this.parse_dylid_info()?;
        Ok(this)
    }

    fn parse_load_cmds(&mut self) -> Result<(), object::Error> {
        let header = self.obj.raw_header();
        let endian = self.obj.endian();

        let twolevel = header.flags(endian) & macho::MH_TWOLEVEL != 0;
        if twolevel {
            self.dylibs.push("");
        }

        let mut load_cmds_iter = header.load_commands(endian, self.obj.data(), 0)?;
        while let Some(lcmd) = load_cmds_iter.next()? {
            if let Some(cmd) = lcmd.symtab()? {
                self.symtab = Some(cmd.symbols(endian, self.obj.data())?);
            }
            if let Some(cmd) = lcmd.dysymtab()? {
                self.dysymtab = Some(cmd);
            }
            if let Some(dylib_info) = lcmd.dyld_info()? {
                self.dylid_info = Some(dylib_info);
            }
            if let Some(dylib) = lcmd.dylib()? {
                let dylib = lcmd.string(endian, dylib.dylib.name)?;
                let dylib = std::str::from_utf8(dylib).unwrap_or("");
                self.dylibs.push(dylib);
            }
            if lcmd.cmd() == macho::LC_DYLD_CHAINED_FIXUPS {
                self.chained_fixups = Some(lcmd.data()?);
            }
        }

        Ok(())
    }

    fn parse_dylid_info(&mut self) -> Result<(), object::Error> {
        let endian = self.obj.endian();
        let dylib_info = match self.dylid_info {
            Some(dylib_info) => dylib_info,
            None => return Ok(()),
        };

        let rebase_off = dylib_info.rebase_off.get(endian) as u64;
        let rebase_size = dylib_info.rebase_size.get(endian) as u64;
        match self.obj.data().read_bytes_at(rebase_off, rebase_size) {
            Ok(bytes) => parse_dynamic_table(bytes, &mut self.syms)?,
            Err(()) => log::complex!(
                w "[macho::parse_dylid_info] ",
                y "Failed to read rebase when parsing import at offset ",
                g format!("{rebase_off:#x}"),
                y "."
            ),
        }

        let bind_off = dylib_info.bind_off.get(endian) as u64;
        let bind_size = dylib_info.bind_size.get(endian) as u64;
        match self.obj.data().read_bytes_at(bind_off, bind_size) {
            Ok(bytes) => parse_dynamic_table(bytes, &mut self.syms)?,
            Err(()) => log::complex!(
                w "[macho::parse_dylid_info] ",
                y "Failed to read bind when parsing import at offset ",
                g format!("{rebase_off:#x}"),
                y "."
            ),
        }

        let lazy_bind_off = dylib_info.lazy_bind_off.get(endian) as u64;
        let lazy_bind_size = dylib_info.lazy_bind_size.get(endian) as u64;
        match self.obj.data().read_bytes_at(lazy_bind_off, lazy_bind_size) {
            Ok(bytes) => parse_dynamic_table(bytes, &mut self.syms)?,
            Err(()) => log::complex!(
                w "[macho::parse_dylid_info] ",
                y "Failed to read lazy bind when parsing import at offset ",
                g format!("{rebase_off:#x}"),
                y "."
            ),
        }

        Ok(())
    }

    fn parse_base_addr(&mut self) -> Result<(), object::Error> {
        // Macho addresses are relative to the __TEXT segment.
        for segment in self.obj.segments() {
            if let Some(b"__TEXT") = segment.name_bytes()? {
                self.base_addr = segment.address();
                break;
            }
        }

        Ok(())
    }

    fn parse_global_syms(&mut self) {
        self.syms.extend(crate::parse_symbol_table(self.obj));
        let entrypoint = self.obj.entry() + self.base_addr;
        self.syms.push(Addressed {
            addr: entrypoint as usize,
            item: RawSymbol {
                name: "entry",
                module: None,
            },
        });
    }
}

/// Common Mach-O dwarf section names I've found so far.
const DWARF_SECTIONS: [&str; 20] = [
    "__debug_abbrev",
    "__debug_addr",
    "__debug_aranges",
    "__debug_cu_index",
    "__debug_frame",
    "__debug_info",
    "__debug_line",
    "__debug_line_str",
    "__debug_loc",
    "__debug_loclists",
    "__debug_macinfo",
    "__debug_macro",
    "__debug_pubnames",
    "__debug_pubtypes",
    "__debug_ranges",
    "__debug_rnglists",
    "__debug_str",
    "__debug_str_offsets",
    "__debug_tu_index",
    "__debug_types",
];

fn parse_sections<'data, Mach: MachHeader>(obj: &'data MachOFile<'data, Mach>) -> Vec<Section> {
    let mut sections = Vec::new();
    for section in obj.sections() {
        let (name, bytes, start, end) = crate::parse_section_generics(&section);
        let section_flags = match section.flags() {
            SectionFlags::MachO { flags } => flags,
            _ => unreachable!()
        };

        let (mut kind, ident) = match section_flags & macho::SECTION_TYPE {
            // Standard section containing regular data (e.g., code, data).
            macho::S_REGULAR => if section_flags & macho::S_ATTR_SOME_INSTRUCTIONS != 0 {
                (SectionKind::Code, "CODE")
            } else if section_flags & macho::S_ATTR_PURE_INSTRUCTIONS != 0 {
                (SectionKind::Code, "PURE_CODE")
            } else {
                (SectionKind::Raw, "REGULAR")
            },
            // Section that doesn't occupy file space but is
            // zero-filled at runtime, used for uninitialized data.
            macho::S_ZEROFILL => (SectionKind::Raw, "ZEROFILL"),
            // Section containing C string literals. Null-terminated ASCII strings.
            macho::S_CSTRING_LITERALS => (SectionKind::CString, "CSTRING_LITERALS"),
            // Section containing 4-byte literals.
            macho::S_4BYTE_LITERALS => (SectionKind::Raw4, "4BYTE_LITERALS"),
            // Section containing 8-byte literals.
            macho::S_8BYTE_LITERALS => (SectionKind::Raw8, "8BYTE_LITERALS"),
            // Section containing 16-byte literals.
            macho::S_16BYTE_LITERALS => (SectionKind::Raw16, "16BYTE_LITERALS"),
            // Section with pointers to literals.
            macho::S_LITERAL_POINTERS => if obj.is_64() {
                (SectionKind::Ptr64, "LITERAL_POINTERS")
            } else {
                (SectionKind::Ptr32, "LITERAL_POINTERS")
            },
            // Section containing GOT indirect pointers.
            macho::S_NON_LAZY_SYMBOL_POINTERS => if obj.is_64() {
                (SectionKind::Got64, "NON_LAZY_SYMBOL_POINTERS")
            } else {
                (SectionKind::Got32, "NON_LAZY_SYMBOL_POINTERS")
            },
            // Section with stubs for symbols, used to handle imports from dynamic libraries.
            macho::S_SYMBOL_STUBS => (SectionKind::Code, "SYMBOL_STUBS"),
            // Section containing function pointers for module initialization.
            // These functions are called at module load.
            macho::S_MOD_INIT_FUNC_POINTERS => if obj.is_64() {
                (SectionKind::Ptr64, "MOD_INIT_FUNC_POINTERS")
            } else {
                (SectionKind::Ptr32, "MOD_INIT_FUNC_POINTERS")
            },
            // Section containing function pointers for module termination.
            // These functions are called at module unload.
            macho::S_MOD_TERM_FUNC_POINTERS => if obj.is_64() {
                (SectionKind::Ptr64, "MOD_TERM_FUNC_POINTERS")
            } else {
                (SectionKind::Ptr32, "MOD_TERM_FUNC_POINTERS")
            },
            // Section contains symbols that are coalesced by the static linker
            // and possibly the dynamic linker
            macho::S_COALESCED => (SectionKind::Raw, "COALESCED"),
            // Section containing pairs of function pointers for interposing.
            // Used to intercept and redirect calls for patching or hooking.
            macho::S_INTERPOSING => if obj.is_64() {
                (SectionKind::Ptr64, "INTERPOSING")
            } else {
                (SectionKind::Ptr32, "INTERPOSING")
            },
            // Section dedicated to DTrace's dynamically generated Object Format.
            macho::S_DTRACE_DOF => (SectionKind::Raw, "DTRACE_DOF"),
            // Section containing lazy symbol pointers specifically for dynamic libraries.
            // Bound on first access.
            macho::S_LAZY_DYLIB_SYMBOL_POINTERS => if obj.is_64() {
                (SectionKind::Ptr64, "LAZY_DYLIB_SYMBOL_POINTERS")
            } else {
                (SectionKind::Ptr32, "LAZY_DYLIB_SYMBOL_POINTERS")
            },
            // Section containing thread-local data that is not zero-filled and does need initialization.
            macho::S_THREAD_LOCAL_REGULAR => (SectionKind::Raw, "THREAD_LOCAL_REGULAR"),
            // Section containing thread-local data that is zero-filled at runtime.
            macho::S_THREAD_LOCAL_ZEROFILL => (SectionKind::Raw, "THREAD_LOCAL_ZEROFILL"),
            // Similar to S_THREAD_LOCAL_REGULAR.
            macho::S_THREAD_LOCAL_VARIABLES => (SectionKind::Raw, "THREAD_LOCAL_VARIABLES"),
            // Section containing pointers to thread-local variables.
            macho::S_THREAD_LOCAL_VARIABLE_POINTERS => if obj.is_64() {
                (SectionKind::Ptr64, "THREAD_LOCAL_VARIABLE_POINTERS")
            } else {
                (SectionKind::Ptr32, "THREAD_LOCAL_VARIABLE_POINTERS")
            },
            // Section containing pointers to initialization functions for thread-local data
            macho::S_THREAD_LOCAL_INIT_FUNCTION_POINTERS => if obj.is_64() {
                (SectionKind::Ptr64, "THREAD_LOCAL_INIT_FUNCTION_POINTERS")
            } else {
                (SectionKind::Ptr32, "THREAD_LOCAL_INIT_FUNCTION_POINTERS")
            },
            _ => (SectionKind::Raw, "UNKNOWN")
        };

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

#[allow(dead_code)]
fn parse_dynamic_table<'data>(
    _bytes: &'data [u8],
    _symbols: &mut AddressMap<RawSymbol<'data>>,
) -> Result<(), object::Error> {
    log::complex!(
        w "[macho::parse_dynamic_table] ",
        y "Missing an implementation",
        w ".",
    );
    Ok(())
}

struct ImportEntry<'data> {
    lib_ordinal: u64,
    #[allow(dead_code)]
    addend: u64,
    #[allow(dead_code)]
    weak: bool,
    name: &'data str,
}

fn parse_chained_import<'data>(
    imports_addr: u64,
    symbols_addr: u64,
    idx: u64,
    data: &'data [u8],
    imports: &mut Vec<ImportEntry<'data>>,
) {
    let entry_off = imports_addr + idx * size_of::<u32>() as u64;
    let raw: u32 = match data.read_at(entry_off) {
        Ok(raw) => *raw,
        Err(()) => {
            log::complex!(
                w "[macho::parse_chained_fixups] ",
                y "Invalid import at ordinal ",
                g idx.to_string(),
                y ".",
            );
            return;
        }
    };

    let sym_name_addr = symbols_addr + (raw >> 9) as u64;
    let sym_name_range = sym_name_addr..data.len() as u64;
    let sym_name = data.read_bytes_at_until(sym_name_range, 0).unwrap_or(&[]);
    if let Ok(name) = std::str::from_utf8(sym_name) {
        imports.push(ImportEntry {
            lib_ordinal: (raw & 0xff) as u64,
            addend: 0,
            weak: (raw >> 8) != 0,
            name,
        });
    }
}

fn parse_chained_import_addend<'data>(
    imports_addr: u64,
    symbols_addr: u64,
    idx: u64,
    data: &'data [u8],
    imports: &mut Vec<ImportEntry<'data>>,
) {
    let entry_off = imports_addr + idx * size_of::<[u32; 2]>() as u64;
    let raw: u32 = match data.read_at(entry_off) {
        Ok(raw) => *raw,
        Err(()) => {
            log::complex!(
                w "[macho::parse_chained_fixups] ",
                y "Invalid import at ordinal ",
                g idx.to_string(),
                y ".",
            );
            return;
        }
    };

    let addend: u32 = *data.read_at(entry_off).unwrap_or(&0);
    let sym_name_addr = symbols_addr + (raw >> 9) as u64;
    let sym_name_range = sym_name_addr..data.len() as u64;
    let sym_name = data.read_bytes_at_until(sym_name_range, 0).unwrap_or(&[]);
    if let Ok(name) = std::str::from_utf8(sym_name) {
        imports.push(ImportEntry {
            lib_ordinal: (raw & 0xff) as u64,
            addend: addend as u64,
            weak: (raw >> 8) != 0,
            name,
        });
    }
}

fn parse_chained_import_addend64<'data>(
    imports_addr: u64,
    symbols_addr: u64,
    idx: u64,
    data: &'data [u8],
    imports: &mut Vec<ImportEntry<'data>>,
) {
    let entry_off = imports_addr + idx * size_of::<[u64; 2]>() as u64;
    let raw: u64 = match data.read_at(entry_off) {
        Ok(raw) => *raw,
        Err(()) => {
            log::complex!(
                w "[macho::parse_chained_fixups] ",
                y "Invalid import at ordinal ",
                g idx.to_string(),
                y ".",
            );
            return;
        }
    };

    let addend: u64 = *data.read_at(entry_off).unwrap_or(&0);
    let sym_name_addr = symbols_addr + (raw >> 17) as u64;
    let sym_name_range = sym_name_addr..data.len() as u64;
    let sym_name = data.read_bytes_at_until(sym_name_range, 0).unwrap_or(&[]);
    if let Ok(name) = std::str::from_utf8(sym_name) {
        imports.push(ImportEntry {
            lib_ordinal: raw & 0xffff,
            addend,
            weak: (raw >> 16) != 0,
            name,
        });
    }
}

fn parse_page_starts_table_starts(page_starts: u64, page_count: u64, data: &[u8]) -> Vec<Vec<u16>> {
    let mut page_start_offs = Vec::new();
    for idx in 0..page_count {
        let start: u16 = match data.read_at(page_starts + size_of::<u16>() as u64 * idx) {
            Ok(start) => *start,
            Err(()) => {
                log::complex!(
                    w "[macho::parse_page_table] ",
                    y "Failed to read page offset at offset ",
                    g idx.to_string(),
                    y "."
                );
                continue;
            }
        };

        if start & DYLD_CHAINED_PTR_START_MULTI != 0 && start != DYLD_CHAINED_PTR_START_NONE {
            let overflow_idx = (start & !DYLD_CHAINED_PTR_START_MULTI) as u64;
            let mut sub_page_addr = page_starts + size_of::<u16>() as u64 * overflow_idx;
            let mut page_start_sub_starts = Vec::new();
            loop {
                let sub_page_start: u16 = match data.read_at(sub_page_addr) {
                    Ok(page_start) => *page_start,
                    Err(()) => continue,
                };
                if sub_page_start & DYLD_CHAINED_PTR_START_LAST == 0 {
                    page_start_sub_starts.push(sub_page_start);
                    page_start_offs.push(page_start_sub_starts.clone());
                    sub_page_addr += size_of::<u16>() as u64;
                } else {
                    page_start_sub_starts.push(sub_page_start & !DYLD_CHAINED_PTR_START_LAST);
                    page_start_offs.push(page_start_sub_starts);
                    break;
                }
            }
        } else {
            page_start_offs.push(vec![start]);
        }
    }

    page_start_offs
}

fn parse_chained_fixups<'data, Mach: MachHeader<Endian = Endianness>>(
    base_addr: u64,
    syms: &mut AddressMap<RawSymbol<'data>>,
    dylibs: &[&'data str],
    chained_fixups: &LinkeditDataCommand<Mach::Endian>,
    data: &'data [u8],
    endian: Endianness,
) {
    let data_off = chained_fixups.dataoff.get(endian) as u64;
    let fixups_header: &DyldChainedFixupsHeader = match data.read_at(data_off) {
        Ok(header) => header,
        Err(()) => {
            log::complex!(
                w "[macho::parse_chained_fixups] ",
                y "failed to read lazy bind when parsing import at offset ",
                g format!("{data_off:#x}"),
                y "."
            );
            return;
        }
    };

    let imports_addr = data_off + fixups_header.imports_offset as u64;
    let import_table_size = fixups_header.imports_count as u64;
    let chained_fixups_size = chained_fixups.datasize.get(endian) as u64;

    if import_table_size > chained_fixups_size {
        log::complex!(
            w "[macho::parse_chained_fixups] ",
            y "Binary is malformed.",
        );
        return;
    }

    let symbols_off = fixups_header.symbols_offset as u64;
    let symbols_addr = data_off + symbols_off;
    let import_count = fixups_header.imports_count as u64;
    let import_format = fixups_header.imports_format;

    let mut imports = Vec::new();
    match import_format {
        DYLD_CHAINED_IMPORT => {
            for idx in 0..import_count {
                parse_chained_import(imports_addr, symbols_addr, idx, data, &mut imports);
            }
        }
        DYLD_CHAINED_IMPORT_ADDEND => {
            for idx in 0..import_count {
                parse_chained_import_addend(imports_addr, symbols_addr, idx, data, &mut imports);
            }
        }
        DYLD_CHAINED_IMPORT_ADDEND64 => {
            for idx in 0..import_count {
                parse_chained_import_addend64(imports_addr, symbols_addr, idx, data, &mut imports);
            }
        }
        _ => {
            log::complex!(
                w "[macho::parse_chained_fixups] ",
                y "Unknown import format (might not be supported).",
            );
            return;
        }
    }

    let fixups_start_addr = data_off + fixups_header.starts_offset as u64;
    let segs: &DyldChainedStartsInImage = match data.read_at(fixups_start_addr) {
        Ok(segs) => segs,
        Err(()) => {
            log::complex!(
                w "[macho::parse_chained_fixups] ",
                y "Failed to read image starts.",
            );
            return;
        }
    };

    // Skip to seg_info_offset list.
    let seg_info_addr = fixups_start_addr + size_of::<u32>() as u64;

    for idx in 0..segs.seg_count as u64 {
        let off = match data.read_at::<u32>(seg_info_addr + idx * size_of::<u32>() as u64) {
            Ok(&seg_info_off) if seg_info_off != 0 => seg_info_off,
            _ => continue,
        };

        let mut chain_addr = fixups_start_addr + off as u64;
        let starts: &DyldChainedStartsInSegment = match data.read_at(chain_addr) {
            Ok(starts) => starts,
            Err(()) => {
                log::complex!(
                    w "[macho::parse_chained_fixups] ",
                    y "Failed to read segments starts.",
                );
                continue;
            }
        };

        let (stride_size, format) = match starts.pointer_format {
            DYLD_CHAINED_PTR_ARM64E
            | DYLD_CHAINED_PTR_ARM64E_USERLAND
            | DYLD_CHAINED_PTR_ARM64E_USERLAND24 => (8, ChainedFixupPointerGeneric::GenericArm64e),
            DYLD_CHAINED_PTR_ARM64E_KERNEL => (4, ChainedFixupPointerGeneric::GenericArm64e),
            // DYLD_CHAINED_PTR_ARM64E_FIRMWARE not supported anywhere by the looks of it.
            DYLD_CHAINED_PTR_64 | DYLD_CHAINED_PTR_64_OFFSET | DYLD_CHAINED_PTR_64_KERNEL_CACHE => {
                (4, ChainedFixupPointerGeneric::Generic64)
            }
            DYLD_CHAINED_PTR_32 | DYLD_CHAINED_PTR_32_CACHE => {
                (4, ChainedFixupPointerGeneric::Generic32)
            }
            DYLD_CHAINED_PTR_32_FIRMWARE => (4, ChainedFixupPointerGeneric::Firmware32),
            DYLD_CHAINED_PTR_X86_64_KERNEL_CACHE => (1, ChainedFixupPointerGeneric::Generic64),
            _ => {
                log::complex!(
                    w "[macho::parse_chained_fixups] ",
                    y "Unknown or unsupported pointer format ",
                    g starts.pointer_format.to_string(),
                    y "."
                );
                continue;
            }
        };

        // Skip to page_start list.
        chain_addr += size_of::<DyldChainedStartsInSegment>() as u64 - size_of::<u16>() as u64;

        let page_count = starts.page_count as u64;
        let page_start_offs = parse_page_starts_table_starts(chain_addr, page_count, data);

        for (jdx, page_starts) in page_start_offs.into_iter().enumerate() {
            let page_addr = starts.segment_offset + jdx as u64 * starts.page_size as u64;
            for start in page_starts {
                if start == DYLD_CHAINED_PTR_START_NONE {
                    continue;
                }

                let mut chain_entry_addr = page_addr + start as u64;
                let mut fixups_done = false;
                while !fixups_done {
                    let ptr = match format {
                        ChainedFixupPointerGeneric::Generic32
                        | ChainedFixupPointerGeneric::Firmware32 => {
                            data.read_at::<u32>(chain_entry_addr).map(|ptr| *ptr as u64)
                        }
                        ChainedFixupPointerGeneric::Generic64
                        | ChainedFixupPointerGeneric::GenericArm64e => {
                            data.read_at::<u64>(chain_entry_addr).copied()
                        }
                    };
                    let ptr = match ptr {
                        Ok(ptr) => ptr,
                        Err(()) => {
                            log::complex!(
                                w "[macho::parse_chained_fixups] ",
                                y "Couldn't read fixup pointer at offset ",
                                g format!("{chain_entry_addr:#x}"),
                                y "."
                            );
                            continue;
                        }
                    };

                    let (bind, next_entry_stride_count) = format.bind_and_stride(ptr);

                    if bind {
                        let ordinal = match starts.pointer_format {
                            DYLD_CHAINED_PTR_64 | DYLD_CHAINED_PTR_64_OFFSET => ptr & 0xFFFFF,
                            DYLD_CHAINED_PTR_ARM64E
                            | DYLD_CHAINED_PTR_ARM64E_KERNEL
                            | DYLD_CHAINED_PTR_ARM64E_USERLAND24 => {
                                if starts.pointer_format == DYLD_CHAINED_PTR_ARM64E_USERLAND24 {
                                    ptr & 0xFFFFFF
                                } else {
                                    ptr & 0xFFFF
                                }
                            }
                            DYLD_CHAINED_PTR_32 => ptr & 0xFFFFF,
                            _ => {
                                log::complex!(
                                    w "[macho::parse_chained_fixups] ",
                                    y "Unknown bind format at ",
                                    g format!("{chain_entry_addr:#x}"),
                                    y "."
                                );
                                chain_entry_addr += next_entry_stride_count * stride_size;
                                if next_entry_stride_count == 0 {
                                    fixups_done = true;
                                }
                                continue;
                            }
                        };

                        if let Some(entry) = imports.get(ordinal as usize) {
                            let target_addr = base_addr + chain_entry_addr;

                            if !entry.name.is_empty() {
                                let module = dylibs.get(entry.lib_ordinal as usize).map(|lib| {
                                    // Strip path prefix.
                                    lib
                                        .rsplit_once('/')
                                        .map(|x| x.1)
                                        .filter(|x| !x.is_empty())
                                        .unwrap_or(lib)
                                });

                                syms.push(Addressed {
                                    addr: target_addr as usize,
                                    item: RawSymbol {
                                        name: entry.name,
                                        module,
                                    }
                                });
                            } else {
                                log::complex!(
                                    w "[macho::parse_chained_fixups] ",
                                    y "Import table entry at ",
                                    g format!("{target_addr:#x}"),
                                    y " has no entries.",
                                );
                            }
                        } else {
                            log::complex!(
                                w "[macho::parse_chained_fixups] ",
                                y "Ordinal ",
                                g ordinal.to_string(),
                                y " has no matching import.",
                            );
                        }
                    } else {
                        let _entry_addr = match starts.pointer_format {
                            DYLD_CHAINED_PTR_ARM64E
                            | DYLD_CHAINED_PTR_ARM64E_KERNEL
                            | DYLD_CHAINED_PTR_ARM64E_USERLAND
                            | DYLD_CHAINED_PTR_ARM64E_USERLAND24 => {
                                let auth = ptr & 1 != 0;
                                let mut entry_addr = if auth { ptr & 0xFFFF } else { ptr & 0xFFFA };
                                if starts.pointer_format != DYLD_CHAINED_PTR_ARM64E || auth {
                                    entry_addr += base_addr;
                                }
                                entry_addr
                            }
                            DYLD_CHAINED_PTR_64 => ptr & 0x7FFFFFFFFFF,
                            DYLD_CHAINED_PTR_64_OFFSET => (ptr & 0x7FFFFFFFFFF) + base_addr,
                            DYLD_CHAINED_PTR_64_KERNEL_CACHE
                            | DYLD_CHAINED_PTR_X86_64_KERNEL_CACHE => ptr & 0x3FFFFFFF,
                            DYLD_CHAINED_PTR_32
                            | DYLD_CHAINED_PTR_32_CACHE
                            | DYLD_CHAINED_PTR_32_FIRMWARE => ptr & 0x3FFFFFF,
                            _ => {
                                log::complex!(
                                    w "[macho::parse_chained_fixups] ",
                                    y "Unknown bind format at ",
                                    g format!("{chain_entry_addr:#x}"),
                                    y "."
                                );
                                chain_entry_addr += next_entry_stride_count * stride_size;
                                if next_entry_stride_count == 0 {
                                    fixups_done = true;
                                }
                                continue;
                            }
                        };

                        let _reloc_addr = base_addr + chain_entry_addr;
                        // FIXME: we just don't handle relocations.
                        // Relocation from {reloc_addr:#x} to {entry_addr:#x}.
                    }

                    chain_entry_addr += next_entry_stride_count * stride_size;

                    if chain_entry_addr > page_addr + starts.page_size as u64 {
                        log::complex!(
                            w "[macho::parse_chained_fixups] ",
                            y "Pointer at ",
                            g format!("{chain_entry_addr:#x}"),
                            y " left page."
                        );
                        fixups_done = true;
                    }

                    if next_entry_stride_count == 0 {
                        fixups_done = true;
                    }
                }
            }
        }
    }
}
