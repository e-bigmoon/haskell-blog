---
title: 訪問者数カウンタ
date: 2018/09/16
---

遠い昔のインターネットを思い返してみてください。あの頃の Web サイトは、"あなたは32番目の訪問者です" というような、ちょっとしたものが無ければ完璧な web サイトとして認められませんでしたよね？あぁ、その頃は本当に良い時代でした！あの頃のインターネットを Yesod を使ってもう一度体験してみましょう！

さて、訪問者数を適切にカウントするためには、カウント数を水平にスケールする Web サーバ間で共有し、アプリケーションの再起動時に情報が残るようしなければなりません。そのため、データベースのような何らかの永続的なストレージレイヤに情報を保存する必要があります。

しかし、ここではそこまで複雑な問題は考えません (だって、もしそういうことなら訪問者カウンタを題材に選びませんよね？)。その代わり、今回の例では複数のハンドラ間で何らかの状態を共有する方法について解説しようと思います。実際のアプリケーションでは、リクエストをまたいで情報をキャッシュする時などに利用できます。ここで紹介するテクニックを使う場合は、複数のアプリケーションサーバやアプリケーションの再起動について、各自で適切な処理を追加で考慮する必要があります。

やり方としては、まずはファウンデーション型にミュータブルにデータを参照する新しいフィールドを作ります。そして、それぞれのハンドラでそのフィールドにアクセスします。このテクニックは簡単なのでコードを見てみましょう。

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

上記のコードでは `IORef` を使っていますが、必要であれば `MVar` や `TVar` なども自由に利用可能です。実際に、このプログラムを `IORef` から `TVar` に変更することは、読者への良い練習問題になるでしょう。

## 本書のコード

- [Example01.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/examples/ex02/Example01.hs)