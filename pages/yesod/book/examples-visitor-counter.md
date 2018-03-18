---
title: Visitor counter
date: 2018/03/18
---

## 訪問者のカウント

"あなたは32番目の訪問者です"のようなものなしでは完璧でなかったような, インターネットの古き良き時代を思い出してみよう. その頃は良い時代だった! Yesodを用いて, その素晴らしい経験を再構築してみよう!

さあ, これを正しく行う場合, 情報が多数の水平スケール化したWebサーバで共有され, かつ, アプリケーション再起動時に情報が残るよう,  データベースのような永続的なストレージレイヤに保存しなければならない.

しかし, ここでの目的はよい練習素材を実演することではない(結局のところ, もしよい練習素材のためのものであれば, ビジタカウンタを実演しないであろう). 代わりに, これは複数のハンドラ間で, 何らかのステートを共有するための, 簡単な例を与えることを意図した. 現実世界での使用例としては, 同じリクエスト情報をキャッシュすることである. 今示す技術を使う場合, 複数のアプリケーションサーバと, アプリケーション再起動に注意する必要があることを, 念頭に置かねばならない.

その技術は簡単である: まず, ファウンデーションデータ型の中にミュータブル参照データのためのフィールドを作る. そして, 各ハンドラにおいて, それにアクセスする. 方法は簡単なため, コードを掘り下げる価値はある:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.IORef
import           Yesod

data App = App
    { visitors :: IORef Int
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    visitorsRef <- fmap visitors getYesod
    visitors <-
        liftIO $ atomicModifyIORef visitorsRef $ \i ->
        (i + 1, i + 1)
    defaultLayout
        [whamlet|
            <p>Welcome, you are visitor number #{visitors}.
        |]

main :: IO ()
main = do
    visitorsRef <- newIORef 0
    warp 3000 App
        { visitors = visitorsRef
        }
```

ここで, `IORef`を用いた. それが提供してくれる以上のものは不要であるが, 同様に`MVars`や`TVars`も自由に使える. 実際に, 上のプログラムを変更して代わりに`TVar`にビジターカウントを保存できるようにすることは, 読者のよい練習問題になるだろう.