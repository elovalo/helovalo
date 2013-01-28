module Elovalo where

import Data.Word
import Data.ByteString.Lazy (ByteString)

type Frame = ByteString

data DisplayType = Greyscale

data Elovalo = Elovalo { sendFrame    :: Frame -> IO ()
                       , geometry     :: (Word8,Word8,Word8)
                       , bitsPerVoxel :: Int -- ^ Bits per voxel, range: 1-16
                       , displayType  :: DisplayType
                       }
