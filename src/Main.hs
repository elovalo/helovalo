module Main where

import Elocmd
import Elovalo
import Effects
import Interface.JsonRpc

-- This is a test only

main = do
  elo <- initElocmd "/dev/ttyUSB0"
  sendFrame elo $ singleIntensity elo 0.5
  runJsonRpc 8080 elo
