#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package blaze-builder
    --package http-types
    --package text
    --package yesod
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Blaze.ByteString.Builder.Char.Utf8 (fromText)
import           Control.Arrow                      ((***))
import           Data.Monoid                        (mappend)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import           Network.HTTP.Types                 (encodePath)
import           Yesod

data Slash = Slash

mkYesod "Slash" [parseRoutes|
/ RootR GET
/foo FooR GET
|]

instance Yesod Slash where
    joinPath _ ar pieces' qs' =
        fromText ar `mappend` encodePath pieces qs
      where
        qs = map (TE.encodeUtf8 *** go) qs'
        go "" = Nothing
        go x  = Just $ TE.encodeUtf8 x
        pieces = pieces' ++ [""]

    -- We want to keep canonical URLs. Therefore, if the URL is missing a
    -- trailing slash, redirect. But the empty set of pieces always stays the
    -- same.
    cleanPath _ [] = Right []
    cleanPath _ s
        | dropWhile (not . T.null) s == [""] = -- the only empty string is the last one
            Right $ init s
        -- Since joinPath will append the missing trailing slash, we simply
        -- remove empty pieces.
        | otherwise = Left $ filter (not . T.null) s

getRootR :: Handler Html
getRootR = defaultLayout
    [whamlet|
        <p>
            <a href=@{RootR}>RootR
        <p>
            <a href=@{FooR}>FooR
    |]

getFooR :: Handler Html
getFooR = getRootR

main :: IO ()
main = warp 3000 Slash
