#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package blaze-html
    --package shakespeare
-}
-- Just ignore the quasiquote stuff for now, and that shamlet thing.
-- It will be explained later.
{-# LANGUAGE QuasiQuotes #-}
import           Data.Char                       (toLower)
import           Data.List                       (sort)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Hamlet                     (shamlet)

data Person = Person
    { name :: String
    , age  :: Int
    }

main :: IO ()
main = putStrLn $ renderHtml [shamlet|
<p>Hello, my name is #{name person} and I am #{show $ age person}.
<p>
    Let's do some funny stuff with my name: #
    <b>#{sort $ map toLower (name person)}
<p>Oh, and in 5 years I'll be #{show ((+) 5 (age person))} years old.
|]
  where
    person = Person "Michael" 26
