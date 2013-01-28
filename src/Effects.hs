-- |This contains some simple frame renderers
module Effects where

import Elovalo
import Fractional

-- |Counts total number of voxels in given cube
totalVoxels :: Elovalo -> Int
totalVoxels e = (\(x,y,z) -> x*y*z) (geometry e)

-- |Produces frame with a static intensity
singleIntensity :: RealFrac a => Elovalo -> a -> Frame
singleIntensity e intensity = fractionalToFrame e $
                              replicate (totalVoxels e) intensity

