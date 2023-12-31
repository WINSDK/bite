use std::io::{self, Read};
use std::path::Path;

fn read_to_end<R: Read>(
    r: &mut R,
    buf: &mut Vec<u8>,
    size_hint: Option<usize>,
) -> io::Result<usize> {
    const DEFAULT_BUF_SIZE: usize = 8 * 1024;

    let start_len = buf.len();

    // optionally read in chunks of 16
    let max_read_size = size_hint.and_then(|s| {
        s.checked_div(16)?
            .checked_add(1024)?
            .checked_next_multiple_of(DEFAULT_BUF_SIZE)
    });

    if let Some(size) = size_hint {
        log::PROGRESS.set("Reading binary", size);
    }

    loop {
        if buf.len() == buf.capacity() {
            // buf is full, need more space
            buf.reserve(32);
        }

        let mut spare = buf.spare_capacity_mut();
        if let Some(size) = max_read_size {
            let len = std::cmp::min(spare.len(), size);
            spare = &mut spare[..len]
        }

        let spare = unsafe { std::mem::transmute::<_, &mut [u8]>(spare) };
        match r.read(spare) {
            Ok(bytes_read) => {
                if bytes_read == 0 {
                    return Ok(buf.len() - start_len);
                }

                unsafe { buf.set_len(buf.len() + bytes_read) }
                log::PROGRESS.step_n(bytes_read);
            }
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        }
    }
}

pub fn read<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    fn inner(path: &Path) -> io::Result<Vec<u8>> {
        let mut file = std::fs::File::open(path)?;
        let size = file.metadata().map(|m| m.len() as usize).ok();
        let mut bytes = Vec::with_capacity(size.unwrap_or(0));
        read_to_end(&mut file, &mut bytes, size)?;
        Ok(bytes)
    }

    inner(path.as_ref())
}
