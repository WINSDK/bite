use radix_trie::TrieKey;
use std::sync::Arc;

pub fn parallel_compute<In, Out, F>(items: Vec<In>, output: &mut Vec<Out>, transformer: F)
where
    F: FnOnce(&In) -> Out,
    F: Send + Copy,
    In: Sync,
    Out: Send + Sync,
{
    let thread_count = std::thread::available_parallelism().unwrap().get();

    // for small item counts, perform single-threaded
    if items.len() < thread_count {
        for item in items.iter() {
            output.push(transformer(item));
        }

        return;
    }

    // multithreaded
    std::thread::scope(|s| {
        let chunks = items.chunks(items.len() / thread_count);
        let mut threads = Vec::with_capacity(thread_count);

        for chunk in chunks {
            let thread = s.spawn(move || {
                let mut result = Vec::with_capacity(chunk.len());
                for item in chunk {
                    result.push(transformer(item));
                }
                result
            });

            threads.push(thread);
        }

        for thread in threads {
            let chunk = thread.join().unwrap();
            output.extend(chunk);
        }
    });
}

#[derive(Debug)]
pub struct AddressMap<T> {
    pub (crate) mapping: Vec<Addressed<T>>,
}

impl<T> AddressMap<T> {
    /// Binary search, assumes [`Self`] is sorted.
    #[inline]
    pub fn search(&self, addr: usize) -> Result<usize, usize> {
        self.mapping.binary_search_by_key(&addr, |item| item.addr)
    }

    #[inline]
    pub fn extend(&mut self, other: Self) {
        self.mapping.extend(other.mapping)
    }
}

impl<T> Default for AddressMap<T> {
    fn default() -> Self {
        Self { mapping: Vec::new() }
    }
}

impl<T> std::ops::Deref for AddressMap<T> {
    type Target = Vec<Addressed<T>>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.mapping
    }
}

impl<T> std::ops::DerefMut for AddressMap<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mapping
    }
}

#[derive(Debug)]
pub struct Addressed<T> {
    pub addr: usize,
    pub item: T,
}

impl<T> PartialEq for Addressed<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr
    }
}

impl<T> Eq for Addressed<T> {}

impl<T> PartialOrd for Addressed<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.addr.cmp(&other.addr))
    }
}

impl<T> Ord for Addressed<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.addr.cmp(&other.addr)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArcStr {
    inner: Arc<str>,
}

impl ArcStr {
    #[inline]
    pub fn new(s: &str) -> Self {
        Self {
            inner: Arc::from(s),
        }
    }
}

impl std::ops::Deref for ArcStr {
    type Target = Arc<str>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl TrieKey for ArcStr {
    #[inline]
    fn encode_bytes(&self) -> Vec<u8> {
        self.as_bytes().encode_bytes()
    }
}
