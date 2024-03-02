static MSG: &str = "Hello, world!\n";

#[inline(never)]
fn black_box<T>(v: T) {
    unsafe {
        std::ptr::read_volatile(&v);
    }
}

pub fn main() {
    let val = 16u64;
    black_box(val);
    println!("{}{}", MSG, val);
}
