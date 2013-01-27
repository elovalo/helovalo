module Elovalo where

import Data.Word

type Frame = [Word8]

data DisplayType = Greyscale

data Elovalo = Elovalo { sendFrame    :: Frame -> IO ()
                       , geometry     :: (Word8,Word8,Word8)
                       , bitsPerVoxel :: Word8
                       , displayType  :: DisplayType
                       }
