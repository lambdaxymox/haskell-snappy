{-# LANGUAGE ForeignFunctionInterface #-}

module Snappy
    (
        maxCompressedLength,
        validateCompressedBuffer,
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
-- TODO: Add a proper interface for interacting with foreign byte slices.
data RawRustSlice

type RustSlice = ForeignPtr RawRustSlice

-- TODO: Add a storable interface for RustVec.
data RawRustVec
type RustVec = ForeignPtr RawRustVec

type RawSnappyStatus = Int

data SnappyStatus = SnappyOk | SnappyInvalidInput | SnappyBufferTooSmall

-- TODO: Add a storable interface for RustVec.
data RustSnappyResult
type SnappyResult = ForeignPtr RustSnappyResult

toSnappyStatus :: RawSnappyStatus -> Maybe SnappyStatus
toSnappyStatus status = 
    case status of
        0 -> Just SnappyOk
        1 -> Just SnappyInvalidInput
        2 -> Just SnappyBufferTooSmall
        _ -> Nothing

-- TODO: Add a storable interface for RawRustOptionalVec.
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

compress :: RustSlice -> IO RustVec
compress bytes = 
    withForeignPtr bytes (\ptr -> join $ newForeignPtr_ <$> rust_compress ptr)

uncompressedLength :: RustSlice -> IO SnappyResult
uncompressedLength bytes = 
    withForeignPtr bytes (\ptr -> join $ newForeignPtr_ <$> rust_uncompressedLength ptr)

uncompress :: RustSlice -> IO RustOptionalVec
uncompress bytes = 
    withForeignPtr bytes (\ptr -> join $ newForeignPtr_ <$> rust_uncompress ptr)
