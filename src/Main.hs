module Main where

import Elocmd
import Elovalo
import Effects

-- This is a test only

main = do
  elo <- initElocmd "/dev/ttyUSB0"
  sendFrame elo $ singleIntensity elo 0.5
  putStrLn "hi-hii!"
