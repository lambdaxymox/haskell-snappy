extern crate libc;

mod snappy;


pub use snappy::SnappyStatus;
pub use snappy::compress;
pub use snappy::uncompress;
pub use snappy::max_compressed_length;
pub use snappy::uncompressed_length;
pub use snappy::validate_compressed_buffer;


pub fn get_unchecked(src: &[u8], index: usize) -> u8 {
    unsafe {
        *src.get_unchecked(index)
    }
}

pub fn set_unchecked(src: &mut [u8], index: usize, elem: u8) {
    src[index] = elem;
}