{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances #-}

module Snappy
    (
        maxCompressedLength,
        validateCompressedBuffer,
        compress,
    )
    where 

import qualified Data.ByteString    as BS
import           Control.Monad
import           Foreign
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Storable

-- Here we are using the opaque struct pattern to 
-- represent Rust's slice type.
data RawRustSlice

foreign import ccall "snappy::get_ptr"
    rust_getRawPtr :: Ptr RawRustSlice -> Ptr Word8

foreign import ccall "snappy::get_unchecked"
    rust_rawSliceGet :: Ptr RawRustSlice -> Int -> IO Word8

foreign import ccall "snappy::set_unchecked"
    rust_rawSliceSet :: Ptr RawRustSlice -> Int -> Word8 -> IO ()

type RustSlice = ForeignPtr RawRustSlice


data RawRustVec
type RustVec = ForeignPtr RawRustVec

type RawSnappyStatus = Int

data SnappyStatus = SnappyOk | SnappyInvalidInput | SnappyBufferTooSmall


data RustSnappyResult
type SnappyResult = ForeignPtr RustSnappyResult

toSnappyStatus :: RawSnappyStatus -> Maybe SnappyStatus
toSnappyStatus status = 
    case status of
        0 -> Just SnappyOk
        1 -> Just SnappyInvalidInput
        2 -> Just SnappyBufferTooSmall
        _ -> Nothing


data RawRustOptionalVec
type RustOptionalVec = ForeignPtr RawRustOptionalVec


foreign import ccall safe "max_compressed_length" 
    rust_maxCompressedLength :: CUInt -> CUInt

foreign import ccall safe "validate_compressed_buffer"
    rust_validateCompressedBuffer :: Ptr RawRustSlice -> Bool

foreign import ccall safe "compress"
    rust_compress :: Ptr RawRustSlice -> IO (Ptr RawRustVec)

foreign import ccall safe "uncompressed_length"
    rust_uncompressedLength :: Ptr RawRustSlice -> IO (Ptr RustSnappyResult)

foreign import ccall safe "uncompress"
    rust_uncompress :: Ptr RawRustSlice -> IO (Ptr RawRustOptionalVec)


maxCompressedLength :: Integral a => a -> a
maxCompressedLength = fromIntegral . rust_maxCompressedLength . fromIntegral

validateCompressedBuffer :: RustSlice -> IO Bool
validateCompressedBuffer fptr = 
    withForeignPtr fptr (\ptr -> return $ rust_validateCompressedBuffer ptr)

wrapper f = \ptr -> join $ newForeignPtr_ <$> f ptr

withForeignFunc f = \bytes -> withForeignPtr bytes (wrapper f)

compress :: RustSlice -> IO RustVec
compress = withForeignFunc rust_compress

uncompressedLength :: RustSlice -> IO SnappyResult
uncompressedLength = withForeignFunc rust_uncompressedLength

uncompress :: RustSlice -> IO RustOptionalVec
uncompress = withForeignFunc rust_uncompress
