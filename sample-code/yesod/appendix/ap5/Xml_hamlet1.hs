#!/usr/bin/env stack
-- stack script --resolver lts-16.29
{-# LANGUAGE OverloadedStrings #-}
import           Data.Map (empty)
import           Prelude  hiding (writeFile)
import           Text.XML

main :: IO ()
main =
    writeFile def "test3.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "html" empty
        [ NodeElement $ Element "head" empty
            [ NodeElement $ Element "title" empty
                [ NodeContent "My "
                , NodeElement $ Element "b" empty
                    [ NodeContent "Title"
                    ]
                ]
            ]
        , NodeElement $ Element "body" empty
            [ NodeElement $ Element "p" empty
                [ NodeContent "foo bar baz"
                ]
            ]
        ]