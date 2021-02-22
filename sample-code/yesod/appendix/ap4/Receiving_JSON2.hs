#!/usr/bin/env stack
-- stack script --resolver lts-17.3
import           Data.Aeson.Parser           (json)
import           Data.Conduit                ((.|))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Types.Status   (statusCode)
import           Data.Conduit (runConduit)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "http://httpbin.org/get"

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        value <- runConduit $ bodyReaderSource (responseBody response)
              .| sinkParser json
        print value