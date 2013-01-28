module Elovalo where

import Data.Word
import Data.ByteString.Lazy (ByteString)

type Frame = ByteString

data DisplayType = Greyscale

data Elovalo = Elovalo { sendFrame    :: Frame -> IO ()
                       , geometry     :: (Int,Int,Int)
                       , bitsPerVoxel :: Int -- ^ Bits per voxel, range: 1-16
                       , displayType  :: DisplayType
                       }
