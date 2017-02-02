extern crate libc;

mod snappy;


pub use snappy::SnappyStatus;
pub use snappy::compress;
pub use snappy::uncompress;
pub use snappy::max_compressed_length;
pub use snappy::uncompressed_length;
pub use snappy::validate_compressed_buffer;

fn main() {
    let x = snappy::max_compressed_length(100);
    println!("max compressed length of a 100 byte buffer: {}", x);
}