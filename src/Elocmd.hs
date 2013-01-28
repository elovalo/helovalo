-- |Module for accessing Elocmd variant of Elovalo
module Elocmd (initElocmd) where

import Data.ByteString.Lazy as B hiding (hPutStr)
import Control.Monad (unless,forever)
import Control.Concurrent (forkIO)
import System.IO
import Data.String
import Data.Word
import Serial
import Elovalo
import ElovaloInternal
import ByteStringDeep

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

-- |Does frame escaping by replacing hex byte 7E with bytes 7E 00
escape :: ByteString -> ByteString
escape xs = intercalate (pack [0x7e,0x00]) $ split 0x7e xs

ensureLength n xs = if B.length xs == n
                    then xs
                    else error "Bad frame length" -- FIXME use exception instead

-- |Sends frame. Blocks if another frame is in transmit.
sendEloFrame :: DataVar ByteString -> Frame -> IO ()
sendEloFrame var frame = pushData var $
                         force $
                         escape $
                         ensureLength frameLen frame
