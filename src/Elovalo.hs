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

-- |Counts total number of voxels in given cube
totalVoxels :: Elovalo -> Int
totalVoxels e = (\(x,y,z) -> x*y*z) (geometry e)

-- |Number of bytes per frame. This assumes that the bit count is
-- dividable by 8. That's the case with any common cube geometry.
frameBytes :: Elovalo -> Int
frameBytes e = (bitsPerVoxel e * totalVoxels e) `div` 8
