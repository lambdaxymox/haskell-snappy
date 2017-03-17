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

-- Here we are using the opaque struct pattern to 
-- represent Rust's slice type.
-- TODO: Add a proper interface for interacting with foreign byte slices.
data RawRustSlice
type RustSlice = ForeignPtr RawRustSlice

-- TODO: Add a storable interface for RustVec.
data RawRustVec
type RustVec = ForeignPtr RawRustVec

foreign import ccall safe "max_compressed_length" 
    rust_maxCompressedLength :: CUInt -> CUInt

foreign import ccall safe "validate_compressed_buffer"
    rust_validateCompressedBuffer :: Ptr RawRustSlice -> Bool

foreign import ccall safe "compress"
    rust_compress :: Ptr RawRustSlice -> IO (Ptr RawRustVec)

maxCompressedLength :: Integral a => a -> a
maxCompressedLength = fromIntegral . rust_maxCompressedLength . fromIntegral

validateCompressedBuffer :: RustSlice -> IO Bool
validateCompressedBuffer fptr = 
    withForeignPtr fptr (\ptr -> return $ rust_validateCompressedBuffer ptr)

compress :: RustSlice -> IO RustVec
compress bytes = 
    withForeignPtr bytes (\ptr -> join $ newForeignPtr_ <$> rust_compress ptr)

