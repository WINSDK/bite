use once_cell::sync::Lazy;

static THREAD_COUNT: Lazy<usize> = Lazy::new(|| {
    let fallback = std::num::NonZeroUsize::new(1).unwrap();

    std::thread::available_parallelism()
        .unwrap_or(fallback)
        .get()
});

// FIXME: somehow return a non-static type
pub async fn spawn_threaded<T, R, F>(tasks: Vec<T>, f: F) -> Vec<R>
where
    T: Send,
    R: Send + 'static,
    F: (Fn(T) -> R) + Send,
{
    if tasks.is_empty() {
        return Vec::new();
    }

    let mut idx = 0;
    let mut drained = false;
    let mut handles = Vec::with_capacity((tasks.len() / *THREAD_COUNT) + 1);

    while !drained {
        let mut next = idx + (tasks.len() / *THREAD_COUNT);

        if next > tasks.len() {
            next = tasks.len();
            drained = true;
        }

        let task_count = tasks.len();
        let tasks = tasks.as_ptr() as usize;
        let f = &f as *const F as usize;

        handles.push(tokio::task::spawn_blocking(move || unsafe {
            let mut collection = Vec::with_capacity(next - idx);

            let tasks: &[T] = std::slice::from_raw_parts(tasks as *const T, task_count);
            let func: &F = std::mem::transmute(f);

            for item in &tasks[idx..next] {
                let item = std::ptr::read(item);
                let item = func(item);

                collection.push(item);
            }

            collection
        }));

        idx += task_count / *THREAD_COUNT;
    }

    let mut results = Vec::with_capacity(tasks.len());
    for handle in handles {
        let result_chunk = handle
            .await
            .expect("Some thread failed in 'spawn_threaded'");

        results.extend(result_chunk);
    }

    results
}
