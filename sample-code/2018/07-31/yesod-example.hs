#!/usr/bin/env stack
-- stack script --resolver lts-12.4
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE BangPatterns #-}
import           Yesod
import Text.Julius
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import qualified Data.Text as T
import Numeric (showHex)
import Data.Text.Lazy.Builder (singleton, fromString)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  mname <- lookupGetParam "name"

  [whamlet|
    $maybe name <- mname
      <img onload="init('#{renderJavascript $ toJavascript $ rawJS $ string name}')" src="https://www.yesodweb.com/static/logo-home2-no-esod-smaller2.png">
    $nothing
      パラメータが設定されていません。
  |]

  toWidget [julius|
    function init(text) {
      console.log(text)
      // 何かしらの初期化処理
    }
  |]

main :: IO ()
main = warp 3000 App

string :: T.Text -> Builder
string s = {-# SCC "string" #-} singleton '"' <> quote s <> singleton '"'
  where
    quote q = case T.uncons t of
                Nothing      -> fromText h
                Just (!c,t') -> fromText h <> escape c <> quote t'
        where (h,t) = {-# SCC "break" #-} T.break isEscape q
    isEscape c = c == '\"' ||
                 c == '\\' ||
                 c == '<'  ||
                 c == '>'  ||
                 c == '&'  ||
                 c == '\'' ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape '<' = "\\u003c"
    escape '>' = "\\u003e"
    escape '&' = "\\u0026"
    escape '\'' = "\\\'"

    escape c
        | c < '\x20' = fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = singleton c
        where h = showHex (fromEnum c) ""