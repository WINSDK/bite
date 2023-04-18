use crate::symbols::Index;
use crate::warning;
use object::{Object, ObjectSection, SectionKind};

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

#[allow(clippy::large_enum_variant)]
pub enum LineKind {
    Newline,
    Label(std::sync::Arc<demangler::TokenStream>),
    Instruction(disassembler::Line),
}

pub struct Disassembly {
    /// Address of last instruction in the object.
    pub address_space: AtomicUsize,

    /// Lines of disassembly e.i. labels and instructions.
    lines: Mutex<Vec<LineKind>>,

    /// Symbol lookup by RVA.
    pub symbols: Mutex<Index>,
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

        let task = tokio::spawn(async move {
            show_donut.store(true, Ordering::Relaxed);

            let binary = tokio::fs::read(&path)
                .await
                .map_err(|_| "Unexpected read of binary failed.")?;

            // SAFETY: tokio::spawn's in this scope require a &'static
            let binary: &'static [u8] = unsafe { std::mem::transmute(&binary[..]) };

            let obj = object::File::parse(binary).map_err(|_| "Not a valid object.")?;
            let arch = obj.architecture();

            let section = obj
                .sections()
                .filter(|s| s.kind() == SectionKind::Text)
                .find(|t| t.name() == Ok(".text"))
                .ok_or("Failed to find `.text` section.")?;

            self.address_space.store(
                (obj.relative_address_base() + section.size()) as usize,
                Ordering::Relaxed,
            );

            // SAFETY: tokio::spawn's in this scope require a &'static
            let section: object::Section<'static, '_> = unsafe { std::mem::transmute(section) };

            let raw = section
                .uncompressed_data()
                .map_err(|_| "Failed to decompress .text section.")?;

            let symbols = Index::parse(&obj)
                .await
                .map_err(|_| "Failed to parse symbols table.")?;

            // TODO: optimize for lazy chunk loading
            let base_offset = section.address() as usize;
            let stream = disassembler::InstructionStream::new(&raw[..], arch, base_offset)
                .map_err(|_| "Failed to disassemble: UnsupportedArchitecture.")?;

            let mut lines = Vec::with_capacity(1024);

            for line in stream {
                if let Some(label) = symbols.get_by_line(&line) {
                    lines.push(LineKind::Newline);
                    lines.push(LineKind::Label(label));
                }

                lines.push(LineKind::Instruction(line));
            }

            *self.symbols.lock().unwrap() = symbols;
            *self.lines.lock().unwrap() = lines;

            Ok::<(), &str>(())
        });

        match task.await.unwrap() {
            Err(err) => warning!("{err:?}"),
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
