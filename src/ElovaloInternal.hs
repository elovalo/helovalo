-- |Some functions for STM operations
module ElovaloInternal where

import Control.Monad.STM
import Control.Concurrent.STM.TVar

type DataVar a = TVar (Maybe a)

-- |Creates new, empty data box
newEmptyDataIO :: IO (DataVar a)
newEmptyDataIO = newTVarIO Nothing

-- |Cleans frame data and allows receiving a new one
emptyData :: DataVar a -> IO ()
emptyData = atomically . (flip writeTVar Nothing)

-- |Reads frame data and does not take it away
peekData :: DataVar a -> IO a
peekData var = atomically $ readTVar var >>= maybe retry return

pushData var new = atomically $ pushData' var new

-- |Pushes frame data to TVar if it is empty.
pushData' :: DataVar a -> a -> STM ()
pushData' var new = do
  cur <- readTVar var 
  case cur of
    Nothing -> writeTVar var $ Just new
    Just _  -> retry

hasData :: DataVar a -> a -> IO Bool
hasData var new = atomically $ (pushData' var new >> return True) `orElse` (return False)
