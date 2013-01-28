-- |Deep evaluation support for lazy ByteStrings
module ByteStringDeep (force) where

import Control.DeepSeq
import Data.ByteString.Lazy.Internal

-- Thanks to Bas van Dijk for the idea:
-- http://www.haskell.org/pipermail/libraries/2011-November/017062.html

instance NFData ByteString where
    rnf Empty       = ()
    rnf (Chunk _ b) = rnf b
