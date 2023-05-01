use crate::symbols::Index;
use disassembler::ToTokens;
use object::{Object, ObjectSection, SectionKind};
use tokenizing::colors;

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

#[allow(clippy::large_enum_variant)]
pub enum LineKind {
    Newline,
    Label(std::sync::Arc<demangler::TokenStream>),
    Instruction(disassembler::TokenStream),
}

pub struct Disassembly {
    /// Address of last instruction in the object.
    pub address_space: AtomicUsize,

    /// Lines of disassembly e.i. labels and instructions.
    lines: Mutex<Vec<LineKind>>,

    /// Symbol lookup by RVA.
    pub symbols: Mutex<Index>,
}

fn tokenize_lines<D: disassembler::Decodable>(
    proc: disassembler::Processor<D>,
    symbols: &Index,
) -> Vec<LineKind> {
    let mut lines = Vec::with_capacity(1024);

    for (&addr, inst) in proc.instructions.iter() {
        let rva = addr - proc.base_addr;

        if let Some(label) = symbols.get_by_addr(addr) {
            lines.push(LineKind::Newline);
            lines.push(LineKind::Label(label));
        }

        let mut line = disassembler::TokenStream::new();

        // memory address
        line.push_owned(
            format!("0x{addr:0>10X}  "),
            &colors::GRAY40,
        );

        // instruction's bytes
        let bytes = disassembler::encode_hex_bytes_truncated(
            proc.get_instruction_bytes(rva, &inst),
            proc.decoder.max_width() * 3 + 1
        );

        line.push_owned(bytes, &colors::GREEN);

        // instruction or decoding error
        match inst {
            Ok(inst) => inst.tokenize(&mut line),
            Err(err) => line.push_owned(format!("{err:?}"), &tokenizing::colors::RED),
        }

        lines.push(LineKind::Instruction(line));
    }

    lines
}

impl Disassembly {
    pub fn new() -> Self {
        Self {
            address_space: AtomicUsize::new(0),
            lines: Mutex::new(Vec::new()),
            symbols: Mutex::new(Index::new()),
        }
    }

    pub async fn load<P>(self: Arc<Self>, path: P, show_donut: Arc<AtomicBool>)
    where
        P: AsRef<std::path::Path> + Sync + Send + 'static,
    {
        let now = tokio::time::Instant::now();
        let path_fmt = format!("{:?}", path.as_ref());
        let showing_donut = Arc::clone(&show_donut);

        let task = tokio::spawn(async move {
            show_donut.store(true, Ordering::Relaxed);

            let binary = tokio::fs::read(&path)
                .await
                .map_err(|_| "Unexpected read of binary failed.")?;

            // SAFETY: tokio::spawn's in this scope require a &'static
            let binary: &'static [u8] = unsafe { std::mem::transmute(&binary[..]) };

            let obj = object::File::parse(binary).map_err(|_| "Not a valid object.")?;

            let entrypoint = obj.entry();
            let section = obj
                .sections()
                .filter(|s| s.kind() == SectionKind::Text)
                .find(|t| (t.address()..t.address() + t.size()).contains(&entrypoint))
                .ok_or("Failed to find a section with the given entrypoint.")?;

            self.address_space.store(
                (obj.relative_address_base() + section.size()) as usize,
                Ordering::Relaxed,
            );

            // SAFETY: tokio::spawn's in this scope require a &'static
            let section: object::Section<'static, '_> = unsafe { std::mem::transmute(section) };

            let raw = section
                .uncompressed_data()
                .map_err(|_| "Failed to decompress .text section.")?;

            let symbols = Index::parse(&obj).map_err(|_| "Failed to parse symbols table.")?;
            let section_base = section.address() as usize;

            let lines = match obj.architecture() {
                object::Architecture::Riscv32 => {
                    let decoder = disassembler::riscv::Decoder { is_64: false };

                    let mut proc: disassembler::Processor<disassembler::riscv::Decoder> =
                        disassembler::Processor::new(
                            &raw[..],
                            section_base,
                            obj.entry() as usize,
                            decoder,
                        );

                    proc.recurse();
                    tokenize_lines(proc, &symbols)
                }
                object::Architecture::Riscv64 => {
                    let decoder = disassembler::riscv::Decoder { is_64: true };

                    let mut proc: disassembler::Processor<disassembler::riscv::Decoder> =
                        disassembler::Processor::new(
                            &raw[..],
                            section_base,
                            obj.entry() as usize,
                            decoder,
                        );

                    proc.recurse();
                    tokenize_lines(proc, &symbols)
                }
                object::Architecture::Mips | object::Architecture::Mips64 => {
                    let decoder = disassembler::mips::Decoder::default();

                    let mut proc: disassembler::Processor<disassembler::mips::Decoder> =
                        disassembler::Processor::new(
                            &raw[..],
                            section_base,
                            obj.entry() as usize,
                            decoder,
                        );

                    proc.recurse();
                    tokenize_lines(proc, &symbols)
                }
                object::Architecture::X86_64_X32 => {
                    let decoder = disassembler::x86::Decoder::default();

                    let mut proc: disassembler::Processor<disassembler::x86::Decoder> =
                        disassembler::Processor::new(
                            &raw[..],
                            section_base,
                            obj.entry() as usize,
                            decoder,
                        );

                    proc.recurse();
                    tokenize_lines(proc, &symbols)
                }
                object::Architecture::X86_64 => {
                    let decoder = disassembler::x64::Decoder::default();

                    let mut proc: disassembler::Processor<disassembler::x64::Decoder> =
                        disassembler::Processor::new(
                            &raw[..],
                            section_base,
                            obj.entry() as usize,
                            decoder,
                        );

                    proc.recurse();
                    tokenize_lines(proc, &symbols)
                }
                err => todo!("decoder doesn't exist for {err:?}"),
            };

            *self.symbols.lock().unwrap() = symbols;
            *self.lines.lock().unwrap() = lines;

            Ok::<(), &str>(())
        });

        match task.await {
            Err(err) => {
                showing_donut.store(false, Ordering::Relaxed);
                eprintln!("{err:?}");
            }
            Ok(Err(err)) => {
                showing_donut.store(false, Ordering::Relaxed);
                crate::error!("{err}");
            }
            _ => println!("took {:#?} to parse {}", now.elapsed(), path_fmt),
        };
    }

    pub fn lines(&self) -> Option<std::sync::MutexGuard<Vec<LineKind>>> {
        if let Ok(lines) = self.lines.try_lock() {
            if !lines.is_empty() {
                return Some(lines);
            }
        }

        None
    }

    pub fn clear(&self) {
        self.lines.lock().unwrap().clear();
        self.symbols.lock().unwrap().clear();
    }
}
