module Elovalo where

import Data.Word

type Frame = [Word8]

data Elovalo = Elovalo { sendFrame :: Frame -> IO ()
                       }
