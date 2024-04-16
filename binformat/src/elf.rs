use std::fmt;
use crate::{datastructure, RawSymbol};
use processor_shared::{AddressMap, Addressed, Section, SectionKind};
use object::elf;
use object::read::elf::{ElfFile, FileHeader, SectionHeader};
use object::{
    Endian, Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationKind,
    RelocationTarget,
};

pub struct ElfDebugInfo<'data, Elf: FileHeader> {
    /// Parsed ELF header.
    obj: &'data ElfFile<'data, Elf>,
    /// Parsed sections with extra metadata.
    pub sections: Vec<Section>,
    /// Any parsed but not yet relocated symbols.
    pub syms: AddressMap<RawSymbol<'data>>,
}

impl<'data, Elf: FileHeader> ElfDebugInfo<'data, Elf> {
    pub fn parse(obj: &'data ElfFile<'data, Elf>) -> Result<Self, object::Error> {
        let mut this = Self {
            obj,
            syms: AddressMap::default(),
            sections: Vec::new(),
        };
        this.sections = parse_sections(obj);
        this.parse_symbols();
        this.parse_imports();
        Ok(this)
    }

    pub fn parse_imports(&mut self) {
        let relocations = match self.obj.dynamic_relocations() {
            Some(relocations) => relocations,
            None => return,
        };

        let dyn_syms = match self.obj.dynamic_symbol_table() {
            Some(dyn_syms) => dyn_syms,
            None => return,
        };

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
                        RelocationKind::Elf(elf::R_X86_64_GLOB_DAT) => r_offset as usize,
                        RelocationKind::Elf(elf::R_X86_64_COPY) => r_offset as usize,
                        // address in .got.plt section which contains an address to the function
                        RelocationKind::Elf(elf::R_X86_64_JUMP_SLOT) => {
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
                    self.syms.push(Addressed {
                        addr,
                        item: RawSymbol { name, module: None },
                    });
                }
            }
        }
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

/// Common ELF dwarf section names I've found so far.
const DWARF_SECTIONS: [&str; 20] = [
    ".debug_abbrev",
    ".debug_addr",
    ".debug_aranges",
    ".debug_cu_index",
    ".debug_frame",
    ".debug_info",
    ".debug_line",
    ".debug_line_str",
    ".debug_loc",
    ".debug_loclists",
    ".debug_macinfo",
    ".debug_macro",
    ".debug_pubnames",
    ".debug_pubtypes",
    ".debug_ranges",
    ".debug_rnglists",
    ".debug_str",
    ".debug_str_offsets",
    ".debug_tu_index",
    ".debug_types",
];

fn parse_sections<'data, Elf: FileHeader>(obj: &'data ElfFile<'data, Elf>) -> Vec<Section> {
    let mut sections = Vec::new();
    let endian = obj.endian();
    let section_headers = obj.raw_header().sections(endian, obj.data()).unwrap();

    for (header, section) in section_headers.iter().zip(obj.sections()) {
        let sh_flags = header.sh_flags(endian).into();
        let (name, bytes, start, end) = crate::parse_section_generics(&section);

        let (mut kind, ident) = match header.sh_type(endian) {
            // Section header table entry is unused.
            elf::SHT_NULL => (SectionKind::Raw, "NULL"),
            // Program data.
            elf::SHT_PROGBITS => (SectionKind::Raw, "PROGBITS"),
            // Symbol table.
            elf::SHT_SYMTAB => (SectionKind::Raw, "SYMTAB"),
            // String table.
            elf::SHT_STRTAB => (SectionKind::CString, "STRTAB"),
            // Relocation entries with explicit addends.
            elf::SHT_RELA => (SectionKind::Raw, "RELA"),
            // Symbol hash table.
            elf::SHT_HASH => (SectionKind::Raw, "HASH"),
            // Dynamic linking information.
            elf::SHT_DYNAMIC => if obj.is_64() {
                (SectionKind::Elf64Dyn, "DYNAMIC")
            } else {
                (SectionKind::Elf32Dyn, "DYNAMIC")
            },
            // Notes.
            elf::SHT_NOTE => (SectionKind::Raw, "NOTE"),
            // Program space with no data (bss).
            elf::SHT_NOBITS => (SectionKind::Raw, "NOBITS"),
            // Relocation entries without explicit addends.
            elf::SHT_REL => (SectionKind::Raw, "REL"),
            // Reserved section type.
            elf::SHT_SHLIB => (SectionKind::Raw, "SHLIB"),
            // Dynamic linker symbol table.
            elf::SHT_DYNSYM => if obj.is_64() {
                (SectionKind::Elf64Sym, "DYNSYM")
            } else {
                (SectionKind::Elf32Sym, "DYNSYM")
            },
            // Array of constructors.
            elf::SHT_INIT_ARRAY => (SectionKind::Raw, "INIT_ARRAY"),
            // Array of destructors.
            elf::SHT_FINI_ARRAY => (SectionKind::Raw, "FINI_ARRAY"),
            // Array of pre-constructors.
            elf::SHT_PREINIT_ARRAY => (SectionKind::Raw, "PREINIT_ARRAY"),
            // Section group.
            elf::SHT_GROUP => (SectionKind::Raw, "GROUP"),
            // Extended section indices for a symbol table.
            elf::SHT_SYMTAB_SHNDX => (SectionKind::Raw4, "SYMTAB_SHNDX"),
            // Start of OS-specific section types.
            elf::SHT_LOOS => (SectionKind::Raw, "LOOS"),
            // Object attributes.
            elf::SHT_GNU_ATTRIBUTES => (SectionKind::Raw, "GNU_ATTRIBUTES"),
            // GNU-style hash table.
            elf::SHT_GNU_HASH => (SectionKind::Raw, "GNU_HASH"),
            // Prelink library list
            elf::SHT_GNU_LIBLIST => (SectionKind::Raw, "GNU_LIBLIST"),
            // Checksum for DSO content.
            elf::SHT_CHECKSUM => (SectionKind::Raw, "CHECKSUM"),
            // Sun-specific low bound.
            elf::SHT_LOSUNW => (SectionKind::Raw, "LOSUNW"),
            elf::SHT_SUNW_COMDAT => (SectionKind::Raw, "SUNW_COMDAT"),
            elf::SHT_SUNW_syminfo => (SectionKind::Raw, "SUNW_syminfo"),
            // Version definition section.
            elf::SHT_GNU_VERDEF => (SectionKind::Raw, "GNU_VERDEF"),
            // Version needs section.
            elf::SHT_GNU_VERNEED => (SectionKind::Raw, "GNU_VERNEED"),
            // Version symbol table.
            elf::SHT_GNU_VERSYM => (SectionKind::Raw, "GNU_VERSYM"),
            // Start of processor-specific section types.
            elf::SHT_LOPROC => (SectionKind::Raw, "LOPROC"),
            // End of processor-specific section types.
            elf::SHT_HIPROC => (SectionKind::Raw, "HIPROC"),
            // Start of application-specific section types.
            elf::SHT_LOUSER => (SectionKind::Raw, "LOUSER"),
            // End of application-specific section types.
            elf::SHT_HIUSER => (SectionKind::Raw, "HIUSER"),
            _ => (SectionKind::Raw, "UNKNOWN")
        };

        // GOT entries.
        if name == ".got" {
            if obj.is_64() {
                kind = SectionKind::Got64;
            } else {
                kind = SectionKind::Got32;
            }
        }

        // File path to gdb startup script.
        if name == ".debug_gdb_scripts" {
            kind = SectionKind::CString;
        }

        // Section contains code.
        if sh_flags as u32 & elf::SHF_EXECINSTR != 0 {
            kind = SectionKind::Code;
        }

        // Section contains null-terminated strings.
        if sh_flags as u32 & elf::SHF_STRINGS != 0 {
            kind = SectionKind::CString;
        }

        // Section that isn't loaded into memory at all.
        if sh_flags as u32 & elf::SHF_ALLOC == 0 {
            kind = SectionKind::Unloaded;
        }

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

#[repr(u64)]
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug)]
pub enum DynTag {
    DT_NULL = 0x0,
    DT_NEEDED = 0x1,
    DT_PLTRELSZ = 0x2,
    DT_PLTGOT = 0x3,
    DT_HASH = 0x4,
    DT_STRTAB = 0x5,
    DT_SYMTAB = 0x6,
    DT_RELA = 0x7,
    DT_RELASZ = 0x8,
    DT_RELAENT = 0x9,
    DT_STRSZ = 0xa,
    DT_SYMENT = 0xb,
    DT_INIT = 0xc,
    DT_FINI = 0xd,
    DT_SONAME = 0xe,
    DT_RPATH = 0xf,
    DT_SYMBOLIC = 0x10,
    DT_REL = 0x11,
    DT_RELSZ = 0x12,
    DT_RELENT = 0x13,
    DT_PLTREL = 0x14,
    DT_DEBUG = 0x15,
    DT_TEXTREL = 0x16,
    DT_JMPREL = 0x17,
    DT_BIND_NOW = 0x18,
    DT_INIT_ARRAY = 0x19,
    DT_FINI_ARRAY = 0x1a,
    DT_INIT_ARRAYSZ = 0x1b,
    DT_FINI_ARRAYSZ = 0x1c,
    DT_RUNPATH = 0x1d,
    DT_FLAGS = 0x1e,
    DT_ENCODING = 0x1f,
    DT_PREINIT_ARRAY = 0x20,
    DT_PREINIT_ARRAYSZ = 0x21,
    DT_LOOS = 0x6000000d,
    DT_SUNW_RTLDINF = 0x6000000e,
    DT_HIOS = 0x6ffff000,
    DT_VALRNGLO = 0x6ffffd00,
    DT_CHECKSUM = 0x6ffffdf8,
    DT_PLTPADSZ = 0x6ffffdf9,
    DT_MOVEENT = 0x6ffffdfa,
    DT_MOVESZ = 0x6ffffdfb,
    DT_FEATURE_1 = 0x6ffffdfc,
    DT_POSFLAG_1 = 0x6ffffdfd,
    DT_SYMINSZ = 0x6ffffdfe,
    DT_SYMINENT = 0x6ffffdff,
    DT_ADDRRNGLO = 0x6ffffe00,
    DT_GNU_HASH = 0x6ffffef5,
    DT_CONFIG = 0x6ffffefa,
    DT_DEPAUDIT = 0x6ffffefb,
    DT_AUDIT = 0x6ffffefc,
    DT_PLTPAD = 0x6ffffefd,
    DT_MOVETAB = 0x6ffffefe,
    DT_SYMINFO = 0x6ffffeff,
    DT_RELACOUNT = 0x6ffffff9,
    DT_RELCOUNT = 0x6ffffffa,
    DT_FLAGS_1 = 0x6ffffffb,
    DT_VERDEF = 0x6ffffffc,
    DT_VERDEFNUM = 0x6ffffffd,
    DT_VERNEED = 0x6ffffffe,
    DT_VERNEEDNUM = 0x6fffffff,
    DT_VERSYM = 0x6ffffff0,
    DT_MIPS_RLD_VERSION = 0x70000001,
    DT_MIPS_TIME_STAMP = 0x70000002,
    DT_MIPS_ICHECKSUM = 0x70000003,
    DT_MIPS_IVERSION = 0x70000004,
    DT_MIPS_FLAGS = 0x70000005,
    DT_MIPS_BASE_ADDRESS = 0x70000006,
    DT_MIPS_CONFLICT = 0x70000008,
    DT_MIPS_LIBLIST = 0x70000009,
    DT_MIPS_LOCAL_GOTNO = 0x7000000a,
    DT_MIPS_CONFLICTNO = 0x7000000b,
    DT_MIPS_LIBLISTNO = 0x70000010,
    DT_MIPS_SYMTABNO = 0x70000011,
    DT_MIPS_UNREFEXTNO = 0x70000012,
    DT_MIPS_GOTSYM = 0x70000013,
    DT_MIPS_HIPAGENO = 0x70000014,
    DT_MIPS_RLD_MAP = 0x70000016,
    DT_MIPS_RLD_MAP_REL = 0x70000035
}

impl fmt::LowerHex for DynTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

datastructure! {
    pub struct Elf32Sym {
        st_name: u32,
        st_value: u32,
        st_size: u32,
        st_info: u8,
        st_other: u8,
        st_shndx: u16,
    }
}

datastructure! {
    pub struct Elf64Sym {
        st_name: u32,
        st_info: u8,
        st_other: u8,
        st_shndx: u16,
        st_value: u64,
        st_size: u64,
    }
}

datastructure! {
    pub struct Elf64Dyn {
        d_tag: DynTag,
        d_val: u64,
    }
}

datastructure! {
    pub struct Elf32Dyn {
        d_tag: u32,
        d_val: u32,
    }
}
