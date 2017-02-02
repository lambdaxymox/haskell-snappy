{-# LANGUAGE ForeignFunctionInterface #-}

module Snappy
    (
        maxCompressedLength,
    )
    where 

import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr

foreign import ccall "max_compressed_length" 
    c_maxCompressedLength :: CUInt -> CUInt


maxCompressedLength :: Integral a => a -> a
maxCompressedLength = fromIntegral . c_maxCompressedLength . fromIntegral