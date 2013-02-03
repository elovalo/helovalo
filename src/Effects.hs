-- |This contains some simple frame renderers
module Effects where

import Elovalo
import Fractional

-- |Produces frame with a static intensity
singleIntensity :: RealFrac a => Elovalo -> a -> Frame
singleIntensity e intensity = fractionalToFrame e $
                              replicate (totalVoxels e) intensity
