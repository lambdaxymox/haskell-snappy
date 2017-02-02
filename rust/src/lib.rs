extern crate libc;

mod snappy;


pub use snappy::SnappyStatus;
pub use snappy::compress;
pub use snappy::uncompress;
pub use snappy::max_compressed_length;
pub use snappy::uncompressed_length;
pub use snappy::validate_compressed_buffer;
