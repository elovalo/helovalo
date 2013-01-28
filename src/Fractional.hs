{-# LANGUAGE RecordWildCards #-}
-- |Fractional value to Frame conversions
module Floating where

import Data.Binary.Put (runPut)
import Data.Binary.Bits.Put
import Data.Word
import Elovalo

-- |Converts values in range [0,1] to raw values
fractionalToFrame :: RealFrac a => Elovalo -> [a] -> Frame
fractionalToFrame Elovalo{..} xs = runPut $
                                   runBitPut $
                                   mapM_ (putFractional bitsPerVoxel) xs

-- |Put a single fractional value in correctly scaled format.
putFractional :: RealFrac a => Int -> a -> BitPut ()
putFractional bits = putWord16be bits . scale bits

-- |Scales a value between 0 and 1 to given bit precision integer.
scale :: (RealFrac a, Integral b) => Int -> a -> b
scale bits x = round $ clamp (0,high) $ high * x
  where high = 2^bits-1
  
-- |Clamps x between a and b, inclusive. Assuming a < b.
clamp :: Ord a => (a, a) -> a -> a
clamp (a,b) x = max a $ min b x
