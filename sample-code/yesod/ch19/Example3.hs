#!/usr/bin/env stack
-- stack script --resolver lts-14.19
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder (fromByteString)
import qualified Data.ByteString          as S
import           Data.Conduit             (Flush (Chunk), ($=))
import           Data.Conduit.Binary      (sourceHandle)
import qualified Data.Conduit.List        as CL
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, responseStream)
import           Network.Wai.Handler.Warp (run)
import           System.IO                (IOMode (ReadMode), withFile)

main :: IO ()
main = run 3000 app

app :: Application
app _req sendResponse = withFile "index.html" ReadMode $ \handle ->
    sendResponse $ responseStream
        status200
        [("Content-Type", "text/html")]
        $ \send _flush ->
            let loop = do
                    bs <- S.hGet handle 4096
                    if S.null bs
                        then return ()
                        else send (fromByteString bs) >> loop
             in loop