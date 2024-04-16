use crate::RawSymbol;
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
            elf::SHT_SYMTAB => (SectionKind::Raw, "SYMTAB"), // array of Elf64_Sym
            // String table.
            elf::SHT_STRTAB => (SectionKind::CString, "STRTAB"),
            // Relocation entries with explicit addends.
            elf::SHT_RELA => (SectionKind::Raw, "RELA"),
            // Symbol hash table.
            elf::SHT_HASH => (SectionKind::Raw, "HASH"),
            // Dynamic linking information.
            elf::SHT_DYNAMIC => (SectionKind::Raw, "DYNAMIC"), // array of Elf64_Dyn
            // Notes.
            elf::SHT_NOTE => (SectionKind::Raw, "NOTE"),
            // Program space with no data (bss).
            elf::SHT_NOBITS => (SectionKind::Raw, "NOBITS"),
            // Relocation entries without explicit addends.
            elf::SHT_REL => (SectionKind::Raw, "REL"), // array of Elf64_Dyn
            // Reserved section type.
            elf::SHT_SHLIB => (SectionKind::Raw, "SHLIB"),
            // Dynamic linker symbol table.
            elf::SHT_DYNSYM => (SectionKind::Raw, "DYNSYM"),
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
