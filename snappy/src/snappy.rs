#![allow(dead_code)]
use libc::{c_int, size_t};


#[link(name = "snappy")]
extern {
    fn snappy_compress(input: *const u8,
                       input_length: size_t,
                       compressed: *mut u8,
                       compressed_length: *mut size_t) -> c_int;
    fn snappy_uncompress(compressed: *const u8,
                         compressed_length: size_t,
                         uncompressed: *mut u8,
                         uncompressed_length: *mut size_t) -> c_int;
    fn snappy_max_compressed_length(source_length: size_t) -> size_t;
    fn snappy_uncompressed_length(compressed: *const u8,
                                  compressed_length: size_t,
                                  result: *mut size_t) -> c_int;
    fn snappy_validate_compressed_buffer(compressed: *const u8,
                                         compressed_length: size_t) -> c_int;
}

pub enum SnappyStatus {
    SnappyOk = 0,
    SnappyInvalidInput = 1,
    SnappyBufferTooSmall = 2,
}

#[no_mangle]
pub extern fn compress(source: &[u8]) -> Vec<u8> {
    unsafe {
        let source_len = source.len() as size_t;
        let source_ptr = source.as_ptr();

        let mut dest_len = snappy_max_compressed_length(source_len);
        let mut dest = Vec::with_capacity(dest_len as usize);
        let dest_ptr = dest.as_mut_ptr();

        snappy_compress(source_ptr, source_len, dest_ptr, &mut dest_len);
        dest.set_len(dest_len as usize);
        dest
    }
}

#[no_mangle]
pub extern fn uncompress(source: &[u8]) -> Option<Vec<u8>> {
    unsafe {
        let source_len = source.len() as size_t;
        let source_ptr = source.as_ptr();

        let mut dest_len: size_t = 0;
        snappy_uncompressed_length(source_ptr, source_len, &mut dest_len);

        let mut dest = Vec::with_capacity(dest_len as usize);
        let dest_ptr = dest.as_mut_ptr();

        if snappy_uncompress(source_ptr, source_len, dest_ptr, &mut dest_len) == 0 {
            dest.set_len(dest_len as usize);
            Some(dest)
        } else {
            None // SNAPPY_INVALID_INPUT
        }
    }
}

#[no_mangle]
pub extern fn max_compressed_length(source_length: usize) -> usize {
    unsafe {
        snappy_max_compressed_length(source_length)
    }
}

#[no_mangle]
pub extern fn uncompressed_length(compressed: &[u8]) -> Result<usize, SnappyStatus> {
    unsafe {
        let mut result: usize = 0;
        let snappy_status = 
            snappy_uncompressed_length(compressed.as_ptr(), compressed.len(), &mut result);
        
        match snappy_status {
            0 => Ok(result),
            1 => Err(SnappyStatus::SnappyInvalidInput),
            2 => Err(SnappyStatus::SnappyBufferTooSmall),
            _ => Err(SnappyStatus::SnappyInvalidInput)
        }
    }
}

#[no_mangle]
pub extern fn validate_compressed_buffer(src: &[u8]) -> bool {
    unsafe {
        snappy_validate_compressed_buffer(src.as_ptr(), src.len() as size_t) == 0
    }
}


#[cfg(test)]
mod tests {
    #[test]
    fn test_valid_buffer() {
        let uncompressed = vec![0xde, 0xad, 0xd0, 0x0d];
        let compressed: &[u8] = &super::compress(&uncompressed);

        assert!(super::validate_compressed_buffer(compressed));
        assert!(super::uncompress(compressed) == Some(uncompressed));
    }

    #[test]
    fn test_invalid_buffer() {
        let buf = vec![0, 0, 0, 0];

        assert!(!super::validate_compressed_buffer(&buf));
        assert!(super::uncompress(&buf).is_none());
    }

    #[test]
    fn test_empty() {
        let buf = vec![];
        assert!(!super::validate_compressed_buffer(&buf));
        assert!(super::uncompress(&buf).is_none());

        let compressed_buf = super::compress(&buf);
        assert!(super::validate_compressed_buffer(&compressed_buf));
        assert!(super::uncompress(&compressed_buf) == Some(buf));
    }
}
