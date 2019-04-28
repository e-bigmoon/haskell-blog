{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Synopsis
    ( mainFunc
    ) where

import           Yesod

data App = App

mkMessage "App" "messages" "en"

plural :: Int -> String -> String -> String
plural 1 x _ = x
plural _ _ y = y

showInt :: Int -> String
showInt = show

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "App" [parseRoutes|
/     HomeR GET
/buy  BuyR  GET
/lang LangR POST
|]

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <h1>_{MsgHello}
        <form action=@{BuyR}>
            _{MsgEnterItemCount}
            <input type=text name=count>
            <input type=submit value=_{MsgPurchase}>
        <form action=@{LangR} method=post>
            _{MsgSwitchLanguage}
            <select name=lang>
                <option value=en>English
                <option value=he>Hebrew
            <input type=submit value=_{MsgSwitch}>
    |]

getBuyR :: Handler Html
getBuyR = do
    count <- runInputGet $ ireq intField "count"
    defaultLayout [whamlet|<p>_{MsgItemCount count}|]

postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField "lang"
    setLanguage lang
    redirect HomeR

mainFunc :: IO ()
mainFunc = warp 3000 App

