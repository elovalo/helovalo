-- |Module for accessing Elocmd variant of Elovalo
module Elocmd where

import qualified Data.ByteString as B
import Control.Monad (unless,forever)
import Control.Monad.STM
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import System.IO
import Data.Word
import Serial

frameLen = 768

type Frame = [Word8]
type Elovalo = TVar (Maybe B.ByteString)

initElovalo :: FilePath -> IO Elovalo
initElovalo f = do
  -- Open connection and enter frame mode
  h <- openSerialRaw f 250000
  hPutStr h "~F"
  hFlush h
  waitFlip h
  -- Create receiver
  var <- newTVarIO Nothing
  forkIO $ forever $ do
    -- Wait for new data
    frameData <- atomically $ peekData var
    -- Write it
    B.hPut h frameData
    hFlush h
    waitFlip h
    -- Enable receiver
    atomically $ emptyData var
  return var

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
sendFrame :: Elovalo -> Frame -> IO ()
sendFrame var frame = atomically $
                      pushData var $
                      B.pack $
                      escape frameLen frame

-- |Pushes frame data to TVar if it is empty.
pushData :: Elovalo -> B.ByteString -> STM ()
pushData var new = do
  cur <- readTVar var 
  case cur of
    Nothing -> writeTVar var $ Just new
    Just _  -> retry

-- |Reads frame data and does not take it away
peekData :: Elovalo -> STM B.ByteString
peekData var = readTVar var >>= maybe retry return

-- |Cleans frame data and allows receiving a new one
emptyData :: Elovalo -> STM ()
emptyData = flip writeTVar Nothing
