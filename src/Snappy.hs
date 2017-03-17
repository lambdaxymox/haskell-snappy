{-# LANGUAGE ForeignFunctionInterface #-}

module Snappy
    (
        maxCompressedLength,
        validateCompressedBuffer,
    )
    where 

import qualified Data.ByteString    as BS
import           Foreign
import           Foreign.C.Types
import           Foreign.ForeignPtr

-- Here we are using the opaque struct pattern to 
-- represent Rust's slice type.
data RustSlice
type RustByteString = ForeignPtr RustSlice

foreign import ccall "max_compressed_length" 
    rust_maxCompressedLength :: CUInt -> CUInt

foreign import ccall "validate_compressed_buffer"
    rust_validateCompressedBuffer :: Ptr RustSlice -> Bool

maxCompressedLength :: Integral a => a -> a
maxCompressedLength = fromIntegral . rust_maxCompressedLength . fromIntegral

validateCompressedBuffer :: RustByteString -> IO Bool
validateCompressedBuffer ptr = 
    withForeignPtr fptr (\ptr -> return $ rust_validateCompressedBuffer ptr)
