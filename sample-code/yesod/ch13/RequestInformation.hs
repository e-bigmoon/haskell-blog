#!/usr/bin/env stack
-- stack script --resolver lts-12.14

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.List (sortOn)
import           Data.Text (Text)
import           Yesod

data Person = Person
    { personName :: Text
    , personAge  :: Int
    }

people :: [Person]
people =
    [ Person "Miriam" 25
    , Person "Eliezer" 3
    , Person "Michael" 26
    , Person "Gavriella" 1
    ]

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            <a href="?sort=name">Sort by name
            |
            <a href="?sort=age">Sort by age
            |
            <a href="?">No sort
        ^{showPeople}
    |]

showPeople :: Widget
showPeople = do
    msort <- runInputGet $ iopt textField "sort"
    let people' =
            case msort of
                Just "name" -> sortOn personName people
                Just "age"  -> sortOn personAge  people
                _           -> people
    [whamlet|
        <dl>
            $forall person <- people'
                <dt>#{personName person}
                <dd>#{show $ personAge person}
    |]

main :: IO ()
main = warp 3000 App
