#!/usr/bin/env stack
{- stack repl --resolver lts-15.4
    --package shakespeare
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Control.Monad         (forM_)
import           Data.Text             (Text)
import qualified Data.Text.Lazy.IO     as TLIO
import           Text.Shakespeare.Text

data Item = Item
    { itemName :: Text
    , itemQty  :: Int
    }

items :: [Item]
items =
    [ Item "apples" 5
    , Item "bananas" 10
    ]

main :: IO ()
main = forM_ items $ \item -> TLIO.putStrLn
    [lt|You have #{show $ itemQty item} #{itemName item}.|]
