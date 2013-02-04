{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Interface.JsonRpc where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import System.IO.Unsafe (unsafePerformIO) -- TEMPORARY HACK
import Data.Conduit
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (ok200,badRequest400,methodPut)
import Data.Aeson.Parser (json)
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Attoparsec as A
import Blaze.ByteString.Builder (copyByteString, copyLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson

import Elovalo
import Fractional

runJsonRpc :: Port -> Elovalo -> IO ()
runJsonRpc p = run p . process

process :: Elovalo -> Request -> ResourceT IO Response
process elo r = case process' elo r of
  Left x  -> return $ notOk x
  Right p -> do
    frame <- requestBody r $$ sinkParser p
    --liftIO $ sendFrame elo frame
    (unsafePerformIO $ sendFrame elo frame) `seq` return $ ok

-- |Validates headers and returns parser for the payload
process' :: Elovalo -> Request -> Either String (A.Parser Frame)
process' elo Request{..} = do
  unless (pathInfo == ["frame"]) $ Left "Requested service is not implemented"
  unless (requestMethod == methodPut) $ Left "Only PUT is allowed"
  ct <- maybe (Left "Content type not given") return $ lookup "Content-type" requestHeaders
  case ct of
    ":application/json" -> return $ do
      val <- json
      case fromJSON val of
        Success x -> return $ fractionalToFrame elo (x::[Float])
        _ -> fail "Not a JSON array"
    ":application/octet-stream" -> return $ do
      A.takeLazyByteString
    _ -> Left "Unsupported content type"

-- |OK response in JSON
ok :: Response
ok = ResponseBuilder ok200 [("Content-Type","application/json")] $ copyLazyByteString $ encode True

-- |Misc error response in JSON
notOk :: String -> Response
notOk msg = ResponseBuilder badRequest400 [("Content-Type","application/json")] $ copyLazyByteString $ encode msg

-- |Debugging
test x = ResponseBuilder ok200 [("Content-Type","application/json")] $ copyLazyByteString x
