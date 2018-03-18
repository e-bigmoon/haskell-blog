---
title: Initializing data in the foundation datatype
date: 2018/03/18
---

## ファウンデーションデータ型のデータを初期化

この例は, 比較的簡単な概念の例証を意図している: それは, ファウンデーションデータ型に保存されているデータの初期化を行うことである. これは様々な理由により行われるが, そのうちの2つは最も重要である:

- 効率性 : データをスタートアッププロセスにおいて一度に初期化することで, 各リクエスト毎に同じ値を再計算する必要がなくなる.
- 永続性 : ある情報を, 各リクエスト間で永続的なミュータブル位置に保存したい. これはしばしば外部データベースを用いて行われるが, メモリ内のミュータブル変数によっても行われる.

> ミュータブル変数は便利な, 保存のための仕組みであるが, 短所が存在することも覚えておくべきである. もし, プロセスが止まったら, データも失ってしまうことになる. また, 1つ以上のプロセスに水平スケールする場合, プロセス間でデータを同期するための方法が必要となる. この例においては, これら両者の問題は扱わないが, 問題は現実的である. これは, Yesodが永続性のために, 外部データベースの使用を強く推奨する理由の1つである.

実証のために, 非常に簡単なウェブサイトを実装する. それは単一のルートを含み, Markdownファイルに保存されたコンテンツを与える. また, そのコンテンツを表示するだけでなく, 何人の訪問者がサイトを訪れたかを表す, 古流の訪問者数も表示する.

### Step1: ファウンデーションの定義

先ほど, 初期化されるべき2つの情報について確認した: それは, ホームページに表示するMarkdownコンテンツと, 訪問者数を保持するミュータブル変数である. 目的は, 初期化フェーズにおいてできるだけ多くのことを行い, ハンドラにおいて同一作業の繰り返しを避けること, であることを念頭に置くべきである. 従って, まずMarkdownコンテンツをHTMLに, 前もって処理する. ビジタカウンタについては, 簡単な`IORef`で十分である. すると, ファウンデーションデータ型は次のようになる.

```haskell
data App = App
    { homepageContent :: Html
    , visitorCount    :: IORef Int
    }
```

### Step2: ファウンデーションを用いる

この単純な例のために, 1つのルートのみ持つことにする: それは, homepageである. なすべきこととしては:

1. 訪問者数をインクリメントする.
1. 新しい訪問者数をゲットする.
1. 訪問者数と共にMarkdownコンテンツを表示する.

コードを多少短くするために用いる1つのトリックとしては, レコードワイルドカード構文を用いることである: `App{..}`. これは, データ型において, いくつもの異なるフィールドを扱う場合に便利である.

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

### Step3: ファウンデーション型を作る

アプリケーションを初期化する際, 上で挙げた2つのフィールドに値を与える必要がある. これは通常の`IO`コードであり, 任意の必要なアクションを実行できる. 今回は, 次が必要となる:

1. Markdownをファイルから読み込む.
1. MarkdownをHTMLに変換する.
1. ビジタカウンタ変数を作る.

コードはこれらのステップが示すように, 非常に単純なものである.

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

### 結論

この例においてはロケットサイエンスは存在せず, 非常に単純なプログラミングである. この章の目的は, ここで出てきたようなしばしば必要となる実例を達成するために, 一般的に採られる最良の方法を実証することである. 実際のアプリケーションにおいては, 初期化ステップはもっと複雑であろう: データベースのコネクションプールをセットアップしたり, 巨大データをバッチ処理するためのバックグラウンドジョブを行ったりする. この章を読めば, アプリケーション特有の初期化コードをどこに置くべきかがわかるだろう.
以下は, 上の例に関するフルソースコードである.

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