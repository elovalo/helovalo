-- |Module for accessing Elocmd variant of Elovalo
module Elocmd where

import qualified Data.ByteString as B
import Control.Monad (unless)
import System.IO
import Serial

data Elovalo = Elovalo

initElovalo :: FilePath -> IO ()
initElovalo f = do
  -- Open connection and enter frame mode
  h <- openSerialRaw f 250000
  hPutStr h "~F"
  hFlush h
  dropUntilM (hGetChar h) '%'
  -- Insert frame data
  B.hPut h $ B.replicate 768 255
  putStrLn "jee-hee"
  -- Return back to original operating mode
  hPutStr h "~*"
  hClose h

-- |Performs given action until it returns the same value than c.
dropUntilM :: (Eq a, Monad m) => m a -> a -> m ()
dropUntilM a c = do
  x <- a
  unless (x == c) (dropUntilM a c)
