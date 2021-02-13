#!/usr/bin/env stack
-- stack script --resolver lts-16.12
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (Value (Object, String))
import           Data.Aeson              (encode, object, (.=))
import           Data.Aeson.Parser       (json)
import           Conduit (runResourceT)
import           Data.Conduit            (runConduit, (.|))
import           Data.Conduit.Attoparsec (sinkParser)
import           Network.HTTP.Conduit    (RequestBody (RequestBodyLBS),
                                          Response (..), http, method, parseUrl,
                                          requestBody)
import           Network.HTTP.Client (parseUrlThrow)
import           Network.HTTP.Client.TLS (getGlobalManager)

main :: IO ()
main = do
    manager <- getGlobalManager
    value <- liftIO makeValue
    -- We need to know the size of the request body, so we convert to a
    -- ByteString
    let valueBS = encode value
    req' <- liftIO $ parseUrlThrow "http://localhost:3000/"
    let req = req' { method = "POST", requestBody = RequestBodyLBS valueBS }
    res <- runResourceT $ http req manager
    resValue <- runResourceT $ runConduit $ responseBody res .| sinkParser json
    liftIO $ handleResponse resValue

-- Application-specific function to make the request value
makeValue :: IO Value
makeValue = return $ object
    [ ("foo" .= ("bar" :: String))
    ]

-- Application-specific function to handle the response from the server
handleResponse :: Value -> IO ()
handleResponse = print