-- |Module for accessing Elocmd variant of Elovalo
module Elocmd (initElocmd) where

import Data.ByteString (ByteString, hPut, pack)
import Control.Monad (unless,forever)
import Control.Concurrent (forkIO)
import System.IO
import Data.String
import Data.Word
import Serial
import Elovalo
import ElovaloInternal

-- Geometry and bitsPerVoxel are hardcoded, too.
frameLen = 768

initElocmd :: FilePath -> IO Elovalo
initElocmd f = do
  -- Open connection and enter frame mode. TODO listen geometry
  -- information, too.
  h <- openSerialRaw f 250000
  hPutStr h "~F"
  hFlush h
  waitFlip h
  -- Create receiver
  var <- newEmptyDataIO
  forkIO $ forever $ do
    -- Wait for new data
    frameData <- peekData var
    -- Write it
    hPut h frameData
    hFlush h
    waitFlip h
    -- Enable receiver
    emptyData var
  return Elovalo{ sendFrame = sendEloFrame var
                , geometry = (8,8,8)
                , bitsPerVoxel = 12
                , displayType = Greyscale
                }

waitFlip h = dropUntilM (hGetChar h) '%'

-- |Performs given action until it returns the same value than c.
dropUntilM :: (Eq a, Monad m) => m a -> a -> m ()
dropUntilM a c = do
  x <- a
  unless (x == c) (dropUntilM a c)

-- |Does frame escaping
escape :: Int -> [Word8] -> [Word8]
escape 0 []        = []
escape n (0x7e:xs) = 0x7e:0x00:escape (n-1) xs
escape n (x:xs)    = x:escape (n-1) xs
escape _ _ = error "Bad frame length" -- FIXME use exception instead

-- |Sends frame. Blocks if another frame is in transmit.
sendEloFrame :: DataVar ByteString -> Frame -> IO ()
sendEloFrame var frame = pushData var $
                         pack $
                         escape frameLen frame
