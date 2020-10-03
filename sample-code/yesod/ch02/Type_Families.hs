#!/usr/bin/env stack
-- stack script --resolver lts-16.12
{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
import Data.Word (Word8)
import qualified Data.ByteString as S
import Data.ByteString.Char8 () -- get an orphan IsString instance

class SafeHead a where
    type Content a
    safeHead :: a -> Maybe (Content a)

instance SafeHead [a] where
    type Content [a] = a
    safeHead [] = Nothing
    safeHead (x:_) = Just x

instance SafeHead S.ByteString where
    type Content S.ByteString = Word8
    safeHead bs
        | S.null bs = Nothing
        | otherwise = Just $ S.head bs

main :: IO ()
main = do
    print $ safeHead ("" :: String)
    print $ safeHead ("hello" :: String)

    print $ safeHead ("" :: S.ByteString)
    print $ safeHead ("hello" :: S.ByteString)