use crate::symbols::Index;
use object::{Object, ObjectSection, SectionKind};

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

pub struct Disassembly {
    /// Where the cursor is currently.
    pub current_addr: AtomicUsize,

    /// Processor which holds information related to each instruction.
    pub proc: Mutex<Option<Box<dyn disassembler::InspectProcessor + Send>>>,

    /// Symbol lookup by absolute address.
    pub symbols: Mutex<Index>,
}

impl Disassembly {
    pub fn new() -> Self {
        Self {
            current_addr: AtomicUsize::new(0),
            proc: Mutex::new(None),
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

            // SAFETY: tokio::spawn's in this scope require a &'static
            let section: object::Section<'static, '_> = unsafe { std::mem::transmute(section) };

            let raw = section
                .uncompressed_data()
                .map_err(|_| "Failed to decompress .text section.")?
                .into_owned();

            let symbols = Index::parse(&obj).map_err(|_| "Failed to parse symbols table.")?;
            let section_base = section.address() as usize;

            match obj.architecture() {
                object::Architecture::Riscv32 => {
                    let decoder = disassembler::riscv::Decoder { is_64: false };

                    let mut proc: disassembler::Processor<disassembler::riscv::Decoder> =
                        disassembler::Processor::new(
                            raw,
                            section_base,
                            obj.entry() as usize,
                            decoder,
                        );

                    proc.recurse();
                    *self.proc.lock().unwrap() = Some(Box::new(proc));
                }
                object::Architecture::Riscv64 => {
                    let decoder = disassembler::riscv::Decoder { is_64: true };

                    let mut proc: disassembler::Processor<disassembler::riscv::Decoder> =
                        disassembler::Processor::new(
                            raw,
                            section_base,
                            obj.entry() as usize,
                            decoder,
                        );

                    proc.recurse();
                    *self.proc.lock().unwrap() = Some(Box::new(proc));
                }
                object::Architecture::Mips | object::Architecture::Mips64 => {
                    let decoder = disassembler::mips::Decoder::default();

                    let mut proc: disassembler::Processor<disassembler::mips::Decoder> =
                        disassembler::Processor::new(
                            raw,
                            section_base,
                            obj.entry() as usize,
                            decoder,
                        );

                    proc.recurse();
                    *self.proc.lock().unwrap() = Some(Box::new(proc));
                }
                object::Architecture::X86_64_X32 => {
                    let decoder = disassembler::x86::Decoder::default();

                    let mut proc: disassembler::Processor<disassembler::x86::Decoder> =
                        disassembler::Processor::new(
                            raw,
                            section_base,
                            obj.entry() as usize,
                            decoder,
                        );

                    proc.recurse();
                    *self.proc.lock().unwrap() = Some(Box::new(proc));
                }
                object::Architecture::X86_64 => {
                    let decoder = disassembler::x64::Decoder::default();

                    let mut proc: disassembler::Processor<disassembler::x64::Decoder> =
                        disassembler::Processor::new(
                            raw,
                            section_base,
                            obj.entry() as usize,
                            decoder,
                        );

                    proc.recurse();
                    *self.proc.lock().unwrap() = Some(Box::new(proc));
                }
                err => crate::warning!("decoder doesn't exist for {err:?}"),
            }

            self.current_addr
                .store(entrypoint as usize, Ordering::Relaxed);
            *self.symbols.lock().unwrap() = symbols;

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

    pub fn clear(&self) {
        self.current_addr.store(0, Ordering::SeqCst);
        self.symbols.lock().unwrap().clear();
    }
}
