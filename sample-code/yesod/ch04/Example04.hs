#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package shakespeare
    --package text
-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.Text.Lazy.IO as TLIO
import           Text.Lucius

-- Dummy render function.
render = undefined

-- Our mixin, which provides a number of vendor prefixes for transitions.
transition val =
    [luciusMixin|
        -webkit-transition: #{val};
        -moz-transition: #{val};
        -ms-transition: #{val};
        -o-transition: #{val};
        transition: #{val};
    |]

-- Our actual Lucius template, which uses the mixin.
myCSS =
    [lucius|
        .some-class {
            ^{transition "all 4s ease"}
        }
    |]

main = TLIO.putStrLn $ renderCss $ myCSS render
