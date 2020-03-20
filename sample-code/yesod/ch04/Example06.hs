#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package shakespeare
    --package text
-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Data.Text         (Text)
import qualified Data.Text.Lazy.IO as TLIO
import           Text.Lucius       (CssUrl, luciusFile, luciusFileReload,
                                    renderCss)

data MyRoute = Home | Time | Stylesheet

render :: MyRoute -> [(Text, Text)] -> Text
render Home _       = "/home"
render Time _       = "/time"
render Stylesheet _ = "/style.css"

template :: CssUrl MyRoute
#if PRODUCTION
template = $(luciusFile "template.lucius")
#else
template = $(luciusFileReload "template.lucius")
#endif

main :: IO ()
main = TLIO.putStrLn $ renderCss $ template render
