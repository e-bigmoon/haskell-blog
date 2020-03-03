---
title: ファウンデーション型の初期化
published: 2018/03/18
updated: 2018/09/16
---

この例では、ファウンデーション型の初期化という、比較的わかりやすい概念を解説します。データを初期化したい理由は様々ですが、以下の2つは特に重要です。

- 効率性: 起動時にデータを初期化することで、リクエスト毎に同じ値するというような無駄な再計算を避けることができます。
- 永続性: 何らかの情報を各リクエスト間で共有できるようにミュータブルな場所に保存したいと思うことがあります。こういった場合、外部データベースが良く利用されますが、メモリ内のミュータブル変数を使うこともあります。

<div class="yesod-book-notice">
ミュータブル変数はストレージとして便利な仕組みですが、デメリットについても理解しておくべきです。もし、プロセスが止まってしまったらデータを失ってしまいます。また、2つ以上のプロセスに水平スケールする場合、プロセス間でデータを同期するための方法が必要になります。今回の例ではこれらの問題を取り扱いませんが、現実的な問題です。この問題は Yesod が永続性のために外部データベースの使用を強く推奨する理由の1つです。
</div>

説明のためにとてもシンプルな Web サイトを実装したいと思います。どのぐらいシンプルかと言うと、

- ルートは1つだけです。
- Markdown ファイルに保存されたコンテンツを使います。
- コンテンツを表示します。
- 昔流行ったと思いますが、サイトにどのぐらいの人数が来ているかわかるように訪問者数を表示します。

という程度にシンプルです。

## Step1: アプリケーションのファウンデーションを定義しよう

最初の説明を良く読めば、今回のアプリケーションで初期化しておくべきものは、Web サイトに表示する Markdown コンテンツと訪問者数を保存するためのミュータブル変数だとわかるでしょう。これから私達がやりたいことは、初期化処理で可能な限り多くのことを行い、ハンドラ内で同じ作業の繰り返しを避けることです。そのため、前処理で Markdown コンテンツを HTML 変換します。訪問者カウンタは単に `IORef` で十分です。まとめると、ファウンデーション型は以下のようになります。

```haskell
data App = App
    { homepageContent :: Html
    , visitorCount    :: IORef Int
    }
```

## Step2: ファウンデーションの使い方

例を非常に単純にするためにルートはトップページしか考えません。やりたいことは以下の通りです。

1. 訪問者数をインクリメントする。
1. 新しい訪問者数を取得する。
1. 訪問者数と共に Markdown コンテンツを表示する。

コードを多少短くするためのテクニックとしてレコードワイルドカード構文 `App{..}` を採用します。この言語拡張はデータ型にいくつもの異なるフィールドがある場合に便利です。

```haskell
getHomeR :: Handler Html
getHomeR = do
    App {..} <- getYesod
    currentCount <- liftIO $ atomicModifyIORef visitorCount
        $ \i -> (i + 1, i + 1)
    defaultLayout $ do
        setTitle "Homepage"
        [whamlet|
            <article>#{homepageContent}
            <p>You are visitor number: #{currentCount}.
        |]
```

## Step3: ファウンデーション型の値を作る

アプリケーションを初期化するときに、Step1 で定義した `App` 型の2つのフィールドに値を与える必要があります。この処理はただの `IO` なので必要に応じて任意のアクションを実行できます。今回は以下のアクションを実行します。

1. Markdown をファイルから読み込む。
1. Markdown を HTML に変換する。
1. 訪問者数をカウントするための変数を作る。

コードは上記のステップを Haskell に落とし込むだけです。

```haskell
go :: IO ()
go = do
    rawMarkdown <- TLIO.readFile "homepage.md"
    countRef <- newIORef 0
    warp 3000 App
        { homepageContent = markdown def rawMarkdown
        , visitorCount    = countRef
        }
```

## まとめ

今回の例に関して言えば、特に難しい概念や用語は登場していません。ただのプログラミングです。この章の目的は、アプリケーションに必要なものを揃えるための、一般的に利用されているベストプラクティスを紹介することです。実際のアプリケーションに関して言えば、初期化ステップはもっと複雑になるでしょう。例えば、データベースのコネクションプールをセットアップしたり、ビッグデータをバッチ処理するためにバックグラウンドジョブを起動したりします。この章を読み終わった読者は、アプリケーション毎に必要となる初期化処理を記述できるようになります。

以下は、今回の例の完全なソースコードです。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.IORef
import qualified Data.Text.Lazy.IO as TLIO
import           Text.Markdown
import           Yesod

data App = App
    { homepageContent :: Html
    , visitorCount    :: IORef Int
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]
instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    App {..} <- getYesod
    currentCount <- liftIO $ atomicModifyIORef visitorCount
        $ \i -> (i + 1, i + 1)
    defaultLayout $ do
        setTitle "Homepage"
        [whamlet|
            <article>#{homepageContent}
            <p>You are visitor number: #{currentCount}.
        |]

main :: IO ()
main = do
    rawMarkdown <- TLIO.readFile "homepage.md"
    countRef <- newIORef 0
    warp 3000 App
        { homepageContent = markdown def rawMarkdown
        , visitorCount    = countRef
        }
```

## 本書のコード

- [Example01.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/examples/ex01/Example01.hs)