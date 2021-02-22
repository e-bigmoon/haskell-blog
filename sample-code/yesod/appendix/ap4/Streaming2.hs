#!/usr/bin/env stack
-- stack script --resolver lts-17.3
import qualified Data.ByteString           as S
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           System.IO                 (stdout)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "http://httpbin.org/get"

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        let loop = do
                bs <- brRead $ responseBody response
                if S.null bs
                    then putStrLn "\nFinished response body"
                    else do
                        S.hPut stdout bs
                        loop
        loop