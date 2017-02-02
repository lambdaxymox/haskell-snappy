extern crate libc;

mod snappy;


fn main() {
    let x = unsafe {
        snappy::snappy_max_compressed_length(100)
    };
    println!("max compressed length of a 100 byte buffer: {}", x);
}