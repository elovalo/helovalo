-- |Deep evaluation support for lazy ByteStrings
module ByteStringDeep (force) where

import Control.DeepSeq
import Data.ByteString.Lazy.Internal

instance NFData ByteString where
    rnf Empty       = ()
    rnf (Chunk _ b) = rnf b
