use crate::symbols::Index;
use object::{Object, ObjectSection, SectionKind};

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

#[derive(Debug)]
pub enum DecodeError {
    /// Unexpected read of binary failed.
    ReadFailed(std::io::Error),

    /// Failed to find a section with the given entrypoint.
    NoEntrypoint,

    /// Failed to decompress a given section section.
    DecompressionFailed(object::Error),

    /// Failed to parse object.
    IncompleteObject(object::Error),

    /// Failed to parse import table.
    IncompleteImportTable(object::Error),

    /// Failed to parse symbols table.
    IncompleteSymbolTable(pdb::Error),

    /// Decoder support for this platform doesn't yet exist.
    UnknownArchitecture,
}

pub struct Disassembly {
    /// Where the cursor is currently.
    pub current_addr: usize,

    /// Processor which holds information related to each instruction.
    pub proc: Box<dyn disassembler::InspectProcessor + Send>,

    /// Symbol lookup by absolute address.
    pub symbols: Index,
}

impl Disassembly {
    pub fn new<P: AsRef<std::path::Path>>(
        path: P,
        show_donut: Arc<AtomicBool>,
    ) -> Result<Self, DecodeError> {
        let now = tokio::time::Instant::now();
        show_donut.store(true, Ordering::Relaxed);

        let binary = std::fs::read(&path).map_err(DecodeError::ReadFailed)?;
        let obj = object::File::parse(&binary[..]).map_err(DecodeError::IncompleteObject)?;

        let entrypoint = obj.entry();
        let section = obj
            .sections()
            .filter(|s| s.kind() == SectionKind::Text)
            .find(|t| (t.address()..t.address() + t.size()).contains(&entrypoint))
            .ok_or(DecodeError::NoEntrypoint)?;

        let raw = section
            .uncompressed_data()
            .map_err(DecodeError::DecompressionFailed)?
            .into_owned();

        let section_base = section.address() as usize;
        let mut symbols = Index::new();

        symbols.parse_debug(&obj).map_err(DecodeError::IncompleteSymbolTable)?;

        if obj.format() == object::BinaryFormat::Pe {
            if obj.is_64() {
                symbols
                    .parse_imports::<object::pe::ImageNtHeaders64>(&binary[..])
                    .map_err(DecodeError::IncompleteImportTable)?;
            } else {
                symbols
                    .parse_imports::<object::pe::ImageNtHeaders32>(&binary[..])
                    .map_err(DecodeError::IncompleteImportTable)?;
            }
        }

        let proc: Box<dyn disassembler::InspectProcessor + Send> = match obj.architecture() {
            object::Architecture::Riscv32 => {
                let decoder = disassembler::riscv::Decoder { is_64: false };

                let mut proc: disassembler::Processor<disassembler::riscv::Decoder> =
                    disassembler::Processor::new(raw, section_base, obj.entry() as usize, decoder);

                proc.recurse();
                Box::new(proc)
            }
            object::Architecture::Riscv64 => {
                let decoder = disassembler::riscv::Decoder { is_64: true };

                let mut proc: disassembler::Processor<disassembler::riscv::Decoder> =
                    disassembler::Processor::new(raw, section_base, obj.entry() as usize, decoder);

                proc.recurse();
                Box::new(proc)
            }
            object::Architecture::Mips | object::Architecture::Mips64 => {
                let decoder = disassembler::mips::Decoder::default();

                let mut proc: disassembler::Processor<disassembler::mips::Decoder> =
                    disassembler::Processor::new(raw, section_base, obj.entry() as usize, decoder);

                proc.recurse();
                Box::new(proc)
            }
            object::Architecture::X86_64_X32 => {
                let decoder = disassembler::x86::Decoder::default();

                let mut proc: disassembler::Processor<disassembler::x86::Decoder> =
                    disassembler::Processor::new(raw, section_base, obj.entry() as usize, decoder);

                proc.recurse();
                Box::new(proc)
            }
            object::Architecture::X86_64 => {
                let decoder = disassembler::x64::Decoder::default();

                let mut proc: disassembler::Processor<disassembler::x64::Decoder> =
                    disassembler::Processor::new(raw, section_base, obj.entry() as usize, decoder);

                proc.recurse();
                Box::new(proc)
            }
            _ => return Err(DecodeError::UnknownArchitecture),
        };

        println!("took {:#?} to parse {:?}", now.elapsed(), path.as_ref());
        Ok(Self {
            current_addr: entrypoint as usize,
            proc,
            symbols,
        })
    }
}
