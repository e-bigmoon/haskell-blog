---
title: Case Study: Single process pub-sub
published: 2021/02/13
---

前の例は明らかにとても単純であった. そのファウンデーションに基づいて(しゃれのつもりですが), もう少し面白いものを作ってみよう. サイトにおいて次のようなワークフローがあるとしよう. 

1. ページXに情報を入力し, 提出する.

2. 提出することでバックグラウンドジョブが始まり, ユーザは別のベージにリダイレクトされ, ジョブのステータスを見る.

3. 2つ目のページでは, バックグラウンドジョブからの更新を読み, それらをユーザに見せる.

ここでの中心的原則は, 1つのスレッドに更新を発行させ, 他のスレッドにこれらの更新を受け取ることに同意させる能力である. これは一般的にpub/subとして知られ, 幸いなことにHaskellにおいてSTMを用いることで非常に容易に成し遂げられる. 

前の例のように注意点から始めさせてください: この方法は単一のウェブアプリケーションプロセスでのみ機能する. もし2つの異なるサーバと1つのロードバランサがある場合, スティッキーセッションかあるいは, 他の方法で単一のユーザからのリクエストが同じマシーンに行くようにしなければならない. このような場合, Redisのような外部pubsubを用いた方法を考慮するかもしれない. 

注意点は置いといて, 始めましょう.

## Foundationデータ型

今回のfoundationでは2つの可変参照が必要になる. 1つ目は次に与える"job id"を追跡するものである. バックグラウンドジョブはそれぞれ一意の識別子で表され, それはURLで用いられる. 2つ目は, job iDからブロードキャストチャンネルへのマップであり, 更新を発行するために用いられる. コードでは以下のようになる:

``` haskell
data App = App
    { jobs    :: TVar (IntMap (TChan (Maybe Text)))
    , nextJob :: TVar Int
    }
```

`TChan`は`Maybe Text`値を含むことに注意してください. `Maybe`でラップする理由は, `Nothing`値を与えることで, チャンネルが完了したことを指摘するためである. 

## ジョブを割り当てる

ジョブを割り当てるために, 必要なことは以下のようになる:

1. job IDを作る.

2. 新しいブロードキャストチャンネルを作る.

3. チャンネルをチャンネルマップに追加する. 

SMTのおかげで, これはかなり簡単である. 

``` haskell
(jobId, chan) <- liftIO $ atomically $ do
    jobId <- readTVar nextJob
    writeTVar nextJob $! jobId + 1
    chan <- newBroadcastTChan
    m <- readTVar jobs
    writeTVar jobs $ IntMap.insert jobId chan m
    return (jobId, chan)
```

## バックグランドジョブをフォークする.

これを行うために多くの異なる方法が存在し, それらのバックグランドジョブが何であるかに完全に依存している. ここに示すのは最小のバックグランドジョブの例であり, それぞれのメッセージ間に1秒の間隔を置き, メッセージをプリントする. 最後のメッセージの後に`Nothing`値をブロードキャストし, チャンネルをチャンネルマップから除去する方法に注意してください. 

``` haskell
liftIO $ forkIO $ do
    threadDelay 1000000
    atomically $ writeTChan chan $ Just "Did something\n"
    threadDelay 1000000
    atomically $ writeTChan chan $ Just "Did something else\n"
    threadDelay 1000000
    atomically $ do
        writeTChan chan $ Just "All done\n"
        writeTChan chan Nothing
        m <- readTVar jobs
        writeTVar jobs $ IntMap.delete jobId m
```


## 進行を見る

このデモのために, 非常に単純な進行ビューを選んだ: ストリームレスポンスを持つプレーンなテキストページである. 他にもいくつかの可能性がある. X秒ごとに自動的に更新するHTMLページや, eventsourceやwebsocketを用いることである. これらについても目を通すことを推奨するが, ここでは考えうる最も単純な実装を挙げる.

``` haskell
getViewProgressR jobId = do
    App {..} <- getYesod
    mchan <- liftIO $ atomically $ do
        m <- readTVar jobs
        case IntMap.lookup jobId m of
            Nothing -> return Nothing
            Just chan -> fmap Just $ dupTChan chan
    case mchan of
        Nothing -> notFound
        Just chan -> respondSource typePlain $ do
            let loop = do
                    mtext <- liftIO $ atomically $ readTChan chan
                    case mtext of
                        Nothing -> return ()
                        Just text -> do
                            sendChunkText text
                            sendFlush
                            loop
            loop
```

まずマップにおけるチャンネルを探すことから始める. もし見つけることができなければ, ジョブが存在しなかったか, すでに完了してしまったかを表す. どちらの場合も, 404を返す. (可能な改善点としては, 前に完了したジョブの情報を保持し, それが終了したのかをユーザに知らせることである.)

チャンネルが存在すると仮定して, `respondSource`を用いストリーミングレスポンスを開始する. 次に, 繰り返し`readTChan`を`Nothing`値を得るまで呼び出す. そして, (`return ()`)により終了する. 各相互作用において, `sendChunkText`と`sendFlush`を用いていることに注意してください. その2つ目の呼び出しがないと, 出力バッファが完全にいっぱいになるまでユーザは更新を受け取れなくなる. これはリアルタイムの更新システムにおいて求めているものではない. 

### 完全なアプリケーション

完全性のために, このアプリケーションにける完全なソースコードを挙げる:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap
import           Data.Text              (Text)
import           Yesod

data App = App
    { jobs    :: TVar (IntMap (TChan (Maybe Text)))
    , nextJob :: TVar Int
    }

mkYesod "App" [parseRoutes|
/ HomeR GET POST
/view-progress/#Int ViewProgressR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "PubSub example"
    [whamlet|
        <form method=post>
            <button>Start new background job
    |]

postHomeR :: Handler ()
postHomeR = do
    App {..} <- getYesod
    (jobId, chan) <- liftIO $ atomically $ do
        jobId <- readTVar nextJob
        writeTVar nextJob $! jobId + 1
        chan <- newBroadcastTChan
        m <- readTVar jobs
        writeTVar jobs $ IntMap.insert jobId chan m
        return (jobId, chan)
    liftIO $ forkIO $ do
        threadDelay 1000000
        atomically $ writeTChan chan $ Just "Did something\n"
        threadDelay 1000000
        atomically $ writeTChan chan $ Just "Did something else\n"
        threadDelay 1000000
        atomically $ do
            writeTChan chan $ Just "All done\n"
            writeTChan chan Nothing
            m <- readTVar jobs
            writeTVar jobs $ IntMap.delete jobId m
    redirect $ ViewProgressR jobId

getViewProgressR :: Int -> Handler TypedContent
getViewProgressR jobId = do
    App {..} <- getYesod
    mchan <- liftIO $ atomically $ do
        m <- readTVar jobs
        case IntMap.lookup jobId m of
            Nothing -> return Nothing
            Just chan -> fmap Just $ dupTChan chan
    case mchan of
        Nothing -> notFound
        Just chan -> respondSource typePlain $ do
            let loop = do
                    mtext <- liftIO $ atomically $ readTChan chan
                    case mtext of
                        Nothing -> return ()
                        Just text -> do
                            sendChunkText text
                            sendFlush
                            loop
            loop

main :: IO ()
main = do
    jobs <- newTVarIO IntMap.empty
    nextJob <- newTVarIO 1
    warp 3000 App {..}
```








[Sphinx](http://sphinxsearch.com/)は検索エンジンであり, 多くのサイトにおいて検索面を強化している. YesodとSphinxを統合するために必要な実際のコードは比較的短いが, おおくの複雑なトピックが存在し, それ故Yesodにおいて水面化で起こっていることの扱い方を学ぶ上でよいケーススタディになる. 

ここでは本質的に3つの物事が展開される:

- 検索したいものを格納する. これはかなり単純なPersistentコードであり, この章ではあまり長々とは触れない.

- Yesod内部からSphinxサーチにアクセスする. sphinxパッケージのおかげで, これは実際に非常に簡単である.

- ドキュメントのコンテンツをsphinxに与える. これは面白い所であり, どのようにデータベースから直接XMLへコンテンツのストリーミングするかについて示す. そして, そのXLMは 直接回線の上を通ってクライアントに送られるのである.

この例の完全なコードは[FP Haskell Center](https://www.fpcomplete.com/user/snoyberg/yesod/case-study-sphinx)にある.

## Sphinxの設定

他の多くの例と異なり, ここではまず実際に外部Sphinxサーバを設定し実行する必要がある. ここではSphinxの詳細には踏み込まない. なぜならばそれはここでの要点と関連しないし, 最も大きな理由としては私がSphinxの専門家ではないからである. 

Sphinxは3つの主なコマンドラインユーティリティを持つ: `searchd`は実際の検索コマンドであり, クライアント(この場合, webアプリ)からリクエストを受け取り, 検索結果を返す. `indexer`はドキュメントの集合をパーズし, 検索インデックスを作る. `search`はデバッグのためのユーティリティであり, Sphinxに対する簡単なクエリを実行する.

2つの重要な設定が存在する: それは, ソースとインデックスである. ソースはSphinxにどこからドキュメント情報を読むかを伝える. これは直接MySQl, PostgreSQLをサポートし, より一般できなXML形式もxmlpipe2としてサポートする. ここでは最後のものを用いる. これはPersistentのバックエンドを選ぶのにより柔軟性を与えるだけでなく, より強力なYesodの概念を見せる. 

2つ目の設定はインデックスである. Sphinxは複数のインデックスを同時に扱うことができる. これにより, 一度に複数のサービスに対し検索をかけることが可能になる. 各インデックスはそれぞれのソースを持つ. 

今回の場合, アプリケーション(/search/xmlpipe)からURLを与え, Sphinxが必要なXMLファイルを与える. そして, それをインデクサーにパイプする. したがって, 次のコードをSphinxの設定ファイルに追加することになる.

```
source searcher_src
{
        type = xmlpipe2
        xmlpipe_command = curl http://localhost:3000/search/xmlpipe
}

index searcher
{
        source = searcher_src
        path = /var/data/searcher
        docinfo = extern
        charset_type = utf-8
}

searchd
{
        listen                  = 9312
        pid_file                = /var/run/sphinxsearch/searchd.pid
}
```

検索インデックスをビルドするために, `indexer searcher`を実行する. 明らかにこれはwebアプリケーションを実行するまでは動かない. 製品サイトにおいては, cron jobによりこのコマンドを実行し, インデックスを常に更新しておくのは重要である.

## 基本的なYesodの設定

基本的なYesodの設定を始めよう. データテーブルには単一のテーブルを持ち, その中にドキュメントを保存することにする. ドキュメントはタイトルとコンテンツから構成される. これをSQLiteデータベースに格納し, 検索や, ドキュメントの追加, ドキュメントの閲覧, xmlpipeファイルをSphinxに与えるためのルートを与える. 


``` haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Doc
    title Text
    content Textarea
|]

data Searcher = Searcher
    { connPool :: ConnectionPool
    }

mkYesod "Searcher" [parseRoutes|
/ HomeR GET
/doc/#DocId DocR GET
/add-doc AddDocR POST
/search SearchR GET
/search/xmlpipe XmlpipeR GET
|]

instance Yesod Searcher

instance YesodPersist Searcher where
    type YesodPersistBackend Searcher = SqlBackend

    runDB action = do
        Searcher pool <- getYesod
        runSqlPool action pool

instance YesodPersistRunner Searcher where -- see below
    getDBRunner = defaultGetDBRunner connPool

instance RenderMessage Searcher FormMessage where
    renderMessage _ _ = defaultFormMessage
```

幸いなことに今やこれら全てはとても見慣れたもに見える. ここで定義された1つの新しいものは, `YesodPersistRunner`のインスタンスである. これはストリーミングデータベースレスポンスを作るために必要な型クラスである. デフォルトの実装(`defaultGetDBRunner`)は大体の場合適切である. 

次にいくつかのフォームを定義する: 1つはドキュメントを作成するためで, もう1つは検索のためである;

``` haskell
addDocForm :: Html -> MForm Handler (FormResult Doc, Widget)
addDocForm = renderTable $ Doc
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Contents" Nothing

searchForm :: Html -> MForm Handler (FormResult Text, Widget)
searchForm = renderDivs $ areq (searchField True) "Query" Nothing
```

`searchField`の`True`パラメータは, フィールドをページのロードと同時に自動フォーカスにする. 最後に, ホームページ(ドキュメント追加フォームや検索フォームを示す), ドキュメント展開, ドキュメント追加のための標準的なハンドラを追加する. 

``` haskell
getHomeR :: Handler Html
getHomeR = do
    docCount <- runDB $ count ([] :: [Filter Doc])
    ((_, docWidget), _) <- runFormPost addDocForm
    ((_, searchWidget), _) <- runFormGet searchForm
    let docs = if docCount == 1
                then "There is currently 1 document."
                else "There are currently " ++ show docCount ++ " documents."
    defaultLayout
        [whamlet|
            <p>Welcome to the search application. #{docs}
            <form method=post action=@{AddDocR}>
                <table>
                    ^{docWidget}
                    <tr>
                        <td colspan=3>
                            <input type=submit value="Add document">
            <form method=get action=@{SearchR}>
                ^{searchWidget}
                <input type=submit value=Search>
        |]

postAddDocR :: Handler Html
postAddDocR = do
    ((res, docWidget), _) <- runFormPost addDocForm
    case res of
        FormSuccess doc -> do
            docid <- runDB $ insert doc
            setMessage "Document added"
            redirect $ DocR docid
        _ -> defaultLayout
            [whamlet|
                <form method=post action=@{AddDocR}>
                    <table>
                        ^{docWidget}
                        <tr>
                            <td colspan=3>
                                <input type=submit value="Add document">
            |]

getDocR :: DocId -> Handler Html
getDocR docid = do
    doc <- runDB $ get404 docid
    defaultLayout
        [whamlet|
            <h1>#{docTitle doc}
            <div .content>#{docContent doc}
        |]
```

## 検索

今や退屈な部分が終わったので, 実際の検索部分に進もう. 結果を展開するために, 3つの情報が必要になる: ドキュメントに紐づくID, ドキュメントのタイトル, 抜粋部分. 抜粋部分は検索語を含むハイライトされた部分のことである. 

![Search Result](https://www.yesodweb.com/book-1.6/image/search-results "Search Result")

結果のデータ型を作ることから始めよう.

``` haskell
data Result = Result
    { resultId      :: DocId
    , resultTitle   :: Text
    , resultExcerpt :: Html
    }
``` 

次に検索ハンドラを見る:

``` haskell
getSearchR :: Handler Html
getSearchR = do
    ((formRes, searchWidget), _) <- runFormGet searchForm
    searchResults <-
        case formRes of
            FormSuccess qstring -> getResults qstring
            _ -> return []
    defaultLayout $ do
        toWidget
            [lucius|
                .excerpt {
                    color: green; font-style: italic
                }
                .match {
                    background-color: yellow;
                }
            |]
        [whamlet|
            <form method=get action=@{SearchR}>
                ^{searchWidget}
                <input type=submit value=Search>
            $if not $ null searchResults
                <h1>Results
                $forall result <- searchResults
                    <div .result>
                        <a href=@{DocR $ resultId result}>#{resultTitle result}
                        <div .excerpt>#{resultExcerpt result}
        |]
```

ここでは何も特別なことはない. 単に上で定義された`searchForm`と`getResults`を用いているだけである. ただ, `getResults`関数はまだ定義されていない. この関数は検索文字列を取り, 結果のリストを返す. これはSphinx APIと最初に相互作用する部分である. 2つの関数が用いられる; `query`は一致したリストを返す. `buildExcerpts`はハイライトされた抜粋部分を返す. まず`getResult`を見てみよう.

``` 
getResults :: Text -> Handler [Result]
getResults qstring = do
    sphinxRes' <- liftIO $ S.query config "searcher" qstring
    case sphinxRes' of
        ST.Ok sphinxRes -> do
            let docids = map (toSqlKey . ST.documentId) $ ST.matches sphinxRes
            fmap catMaybes $ runDB $ forM docids $ \docid -> do
                mdoc <- get docid
                case mdoc of
                    Nothing -> return Nothing
                    Just doc -> liftIO $ Just <$> getResult docid doc qstring
        _ -> error $ show sphinxRes'
  where
    config = S.defaultConfig
        { S.port = 9312
        , S.mode = ST.Any
        }
``` 


`query`は3つのパラメータを取る: 設定オプション, 検索するインデックス(この場合searcher)と, 検索文字列である. そして, 検索文字列を含むドキュメントIDのリストを返す. ここで技巧的な部分は, それらのドキュメントは`Int64`値として返されるが, 実際は`DocIds`が必要なことである. 幸いなことに, SQL Persistバックエンドでは, `toSqlKey`関数を使うだけで変換できる. 

<div class=yesod-book-notice>
もしMondoDBのような数値でないIDを持つバックエンドを用いているのならば, これより少し工夫する必要がある.
</div>

そして, 結果として得られたIDをループし, `[Maybe Result]`値を得て, `catMaybes`を用いてそれを`[Result]`に変換する. whereの部分では, ローカル設定を定義し, デフォルトのポートを上書きし, どんな単語でもドキュメントに一致した部分があれば機能するように検索を設定する. 

最後に`getResult`関数を見てみよう.

``` haskell
getResult :: DocId -> Doc -> Text -> IO Result
getResult docid doc qstring = do
    excerpt' <- S.buildExcerpts
        excerptConfig
        [escape $ docContent doc]
        "searcher"
        qstring
    let excerpt =
            case excerpt' of
                ST.Ok texts -> preEscapedToHtml $ mconcat texts
                _ -> ""
    return Result
        { resultId = docid
        , resultTitle = docTitle doc
        , resultExcerpt = excerpt
        }
  where
    excerptConfig = E.altConfig { E.port = 9312 }

escape :: Textarea -> Text
escape =
    T.concatMap escapeChar . unTextarea
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar c   = T.singleton c
```

`buildExcerpts`は4つのパラメータを取る: 設定オプション, ドキュメントのテキストコンテンツ, 検索インデックスと検索単語である. 興味深い点は, テキストコンテンツをエンティティエスケープしている点である. 

同様にSphinxからの結果は`Text`のリストである. しかしもしろん, 実際にはHtmlとして持ちたい. そこで, そのリストを結合して単一の`Text`にして`preEscapedToHtml`を用い, マッチのために挿入したタグがエスケープされないようにする. このHTMLの例は以下のようになる.

```
&#8230; Departments.  The President shall have <span class='match'>Power</span> to fill up all Vacancies
&#8230;  people. Amendment 11 The Judicial <span class='match'>power</span> of the United States shall
&#8230; jurisdiction. 2. Congress shall have <span class='match'>power</span> to enforce this article by
&#8230; 5. The Congress shall have <span class='match'>power</span> to enforce, by appropriate legislation
&#8230;
```

## xmlpipe出力のストリーミング

最もよいものを最後に持ってきた. 大部分のYesodハンドラにおいて推奨される方法は, データベースの結果をメモリにロードし, それにもとづき結果のドキュメントを生成することである. これは簡単に取り組むことができるが, 重要なことはそれは例外に対し強いということである. もしデータベースからのロードに問題があれば, ユーザは適切に500レスポンスコードを得られる. 

<div class=yesod-book-notice>
"適切な500レスポンスコード"とは何を意味するのか? もしクライアントにレスポンスをストリーミングし始め, 途中でエラーに遭遇した際, そのステータスコードを変更することはできない; ユーザは途中で止まってしまったのに200レスポンスを見ることになる. この部分的なコンテンツは混乱を招くだけでなく, HTTP仕様の不適切な利用でもある. 
</div>

しかし, xmlpipe出力を生成することは代替案の良い例である. おそらく莫大な量のドキュメントがあり, 容易に数百キロバイトになる. もしストリーミングを用いない方法を用いるならば, これはかなりのメモリー浪費となり, レスポンス時間が遅延する. 

どのようにすれば正確にストリーミングレスポンスを作れるのであろうか? Yesodはこの場合のヘルパ関数を提供する: `responseSourceDB`. この関数は, 2つの引数を取る: コンテンツ型と, blaze-builderの`Builder`を与えるconduitの`Source`である. そしてYesodは, コネクションプールからデータベースコネクションを取ってきたり, トランズアクションを始めたり, レスポンスをユーザにストリーミングするようなあらゆる問題に対処する. 

今やXMLコンテンツから`Builder`のストリームを作りたいことを知っている. 幸いなことに, xml-conduitパッケージはこの「インターフェースを直接提供している. `xml-conduit`は高レベルのインターフェースを与え, ドキュメントを全体として扱う. しかし今回の場合, 低レベルの`Event`インターフェースを用い, メモリに最小限の影響しか与えないようにする. すると, 目的の関数としては以下のようになる:

``` haskell
renderBuilder :: Monad m => RenderSettings -> Conduit Event m Builder
```

簡単に説明すると, これは`renderBuilder`はある設定(今回はデフォルトを用いる)を取り, `Event`のストリームを`Builder`のストリームに変換する. これはとても素晴らしく, 必要なものは`Event`のストリームということになる. 

そういえば, XMLドキュメントはどのようであるべきであろうか? これはとても単純であり, `sphinx:docset`ルートエレメントと, 単一の`sphinx:field`(これはコンテンツフィールドを定義する)を含む`sphinx:schema`エレメント, そして`sphinx:document`が, データベースの各ドキュメントに存在する. 最後のエレメントは`id`属性と子`content`エレメントを持つ. 下にこのようなドキュメントの例を示す;

```
<sphinx:docset xmlns:sphinx="http://sphinxsearch.com/">
    <sphinx:schema>
        <sphinx:field name="content"/>
    </sphinx:schema>
    <sphinx:document id="1">
        <content>bar</content>
    </sphinx:document>
    <sphinx:document id="2">
        <content>foo bar baz</content>
    </sphinx:document>
</sphinx:docset>
```

<div class=yesod-book-notice>
もしXMLの名称空間に不慣れであれば, `xmlns:`構文と, `sphinx:`プレフィックスはかなり奇妙に見えるであろう. この章でXMLのチュートリアルに踏み込みたくはないので, その説明は省略する. もし興味があるならば, XML名称空間仕様について調べてください.
</div>

全てのドキュメントは同じイベントから始まり(docsetを始めたり, schemaを始めたり), 同じイベントで終了する(docsetを終了する). これらを定義するところから始めよう.

``` haskell
toName :: Text -> X.Name
toName x = X.Name x (Just "http://sphinxsearch.com/") (Just "sphinx")

docset, schema, field, document, content :: X.Name
docset = toName "docset"
schema = toName "schema"
field = toName "field"
document = toName "document"
content = "content" -- no prefix

startEvents, endEvents :: [X.Event]
startEvents =
    [ X.EventBeginDocument
    , X.EventBeginElement docset []
    , X.EventBeginElement schema []
    , X.EventBeginElement field [("name", [X.ContentText "content"])]
    , X.EventEndElement field
    , X.EventEndElement schema
    ]

endEvents =
    [ X.EventEndElement docset
    ]
```

今やドキュメントのシェルを手に入れたので, 各ドキュメントの`Event`を取得する必要がある. これは実際にかなり単純な関数である: 

``` haskell
entityToEvents :: Entity Doc -> [X.Event]
entityToEvents (Entity docid doc) =
    [ X.EventBeginElement document [("id", [X.ContentText $ toPathPiece docid])]
    , X.EventBeginElement content []
    , X.EventContent $ X.ContentText $ unTextarea $ docContent doc
    , X.EventEndElement content
    , X.EventEndElement document
    ]
```

ドキュメントエレメントは`id`属性から始まり, コンテンツを開始し, コンテンツを挿入する. そして両者のエレメントを閉じる. `toPathPiece`を用いて`DocId`を`Text`値に変換している. 次に, これらのエンティティのストリームをイベントのストリームに変換する必要がある. このために, `Data.conduit.List`に内蔵の`concatMap`関数を用いる: `CL.concatMap entityToEvents`.

しかし本当に望むものはこれらのイベントを直接データベースからストリームすることである. この本の大部分では, `selectList`関数を用いている. しかしPersistentは(より強力な)`selectSource`関数を提供する. よって次のような関数が作れる:

``` haskell
docSource :: Source (YesodDB Searcher) X.Event
docSource = selectSource [] [] $= CL.concatMap entityToEvents
```

`$=`演算子はsourceとconduitを結合し, 新しいsourceを作る. 今や`Event`ソースがあるので, それをドキュメントの開始と終了イベントで囲みさえすればよい. `Source`はモナドのインスタンスなので, もう簡単である:

``` haskell
fullDocSource :: Source (YesodDB Searcher) X.Event
fullDocSource = do
    mapM_ yield startEvents
    docSource
    mapM_ yield endEvents
```

今やそれを`getXmlpipeR`で結びつけさえすればよい. そうするために, 最初の方で言及した`respondSourceDB`関数を用いる. 必要な最後のトリックは, `Event`のストリームを`Chunk Builder`のストリームに変換することである. `Builder`のストリームへの変換は`renderbuilder`により達成され, 最後に各`Builder`を`Chunk`にラップしさえすればよい:

``` haskell
getXmlpipeR :: Handler TypedContent
getXmlpipeR =
    respondSourceDB "text/xml"
 $  fullDocSource
 $= renderBuilder def
 $= CL.map Chunk
```

## 完全なコード

``` haskell
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import           Control.Applicative                     ((<$>), (<*>))
import           Control.Monad                           (forM)
import           Control.Monad.Logger                    (runStdoutLoggingT)
import           Data.Conduit
import qualified Data.Conduit.List                       as CL
import           Data.Maybe                              (catMaybes)
import           Data.Monoid                             (mconcat)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Data.Text.Lazy.Encoding                 (decodeUtf8)
import qualified Data.XML.Types                          as X
import           Database.Persist.Sqlite
import           Text.Blaze.Html                         (preEscapedToHtml)
import qualified Text.Search.Sphinx                      as S
import qualified Text.Search.Sphinx.ExcerptConfiguration as E
import qualified Text.Search.Sphinx.Types                as ST
import           Text.XML.Stream.Render                  (def, renderBuilder)
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Doc
    title Text
    content Textarea
|]

data Searcher = Searcher
    { connPool :: ConnectionPool
    }

mkYesod "Searcher" [parseRoutes|
/ HomeR GET
/doc/#DocId DocR GET
/add-doc AddDocR POST
/search SearchR GET
/search/xmlpipe XmlpipeR GET
|]

instance Yesod Searcher

instance YesodPersist Searcher where
    type YesodPersistBackend Searcher = SqlBackend

    runDB action = do
        Searcher pool <- getYesod
        runSqlPool action pool

instance YesodPersistRunner Searcher where
    getDBRunner = defaultGetDBRunner connPool

instance RenderMessage Searcher FormMessage where
    renderMessage _ _ = defaultFormMessage

addDocForm :: Html -> MForm Handler (FormResult Doc, Widget)
addDocForm = renderTable $ Doc
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Contents" Nothing

searchForm :: Html -> MForm Handler (FormResult Text, Widget)
searchForm = renderDivs $ areq (searchField True) "Query" Nothing

getHomeR :: Handler Html
getHomeR = do
    docCount <- runDB $ count ([] :: [Filter Doc])
    ((_, docWidget), _) <- runFormPost addDocForm
    ((_, searchWidget), _) <- runFormGet searchForm
    let docs = if docCount == 1
                then "There is currently 1 document."
                else "There are currently " ++ show docCount ++ " documents."
    defaultLayout
        [whamlet|
            <p>Welcome to the search application. #{docs}
            <form method=post action=@{AddDocR}>
                <table>
                    ^{docWidget}
                    <tr>
                        <td colspan=3>
                            <input type=submit value="Add document">
            <form method=get action=@{SearchR}>
                ^{searchWidget}
                <input type=submit value=Search>
        |]

postAddDocR :: Handler Html
postAddDocR = do
    ((res, docWidget), _) <- runFormPost addDocForm
    case res of
        FormSuccess doc -> do
            docid <- runDB $ insert doc
            setMessage "Document added"
            redirect $ DocR docid
        _ -> defaultLayout
            [whamlet|
                <form method=post action=@{AddDocR}>
                    <table>
                        ^{docWidget}
                        <tr>
                            <td colspan=3>
                                <input type=submit value="Add document">
            |]

getDocR :: DocId -> Handler Html
getDocR docid = do
    doc <- runDB $ get404 docid
    defaultLayout
        [whamlet|
            <h1>#{docTitle doc}
            <div .content>#{docContent doc}
        |]

data Result = Result
    { resultId      :: DocId
    , resultTitle   :: Text
    , resultExcerpt :: Html
    }

getResult :: DocId -> Doc -> Text -> IO Result
getResult docid doc qstring = do
    excerpt' <- S.buildExcerpts
        excerptConfig
        [escape $ docContent doc]
        "searcher"
        qstring
    let excerpt =
            case excerpt' of
                ST.Ok texts -> preEscapedToHtml $ mconcat texts
                _ -> ""
    return Result
        { resultId = docid
        , resultTitle = docTitle doc
        , resultExcerpt = excerpt
        }
  where
    excerptConfig = E.altConfig { E.port = 9312 }

escape :: Textarea -> Text
escape =
    T.concatMap escapeChar . unTextarea
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar c   = T.singleton c

getResults :: Text -> Handler [Result]
getResults qstring = do
    sphinxRes' <- liftIO $ S.query config "searcher" qstring
    case sphinxRes' of
        ST.Ok sphinxRes -> do
            let docids = map (toSqlKey . ST.documentId) $ ST.matches sphinxRes
            fmap catMaybes $ runDB $ forM docids $ \docid -> do
                mdoc <- get docid
                case mdoc of
                    Nothing -> return Nothing
                    Just doc -> liftIO $ Just <$> getResult docid doc qstring
        _ -> error $ show sphinxRes'
  where
    config = S.defaultConfig
        { S.port = 9312
        , S.mode = ST.Any
        }

getSearchR :: Handler Html
getSearchR = do
    ((formRes, searchWidget), _) <- runFormGet searchForm
    searchResults <-
        case formRes of
            FormSuccess qstring -> getResults qstring
            _ -> return []
    defaultLayout $ do
        toWidget
            [lucius|
                .excerpt {
                    color: green; font-style: italic
                }
                .match {
                    background-color: yellow;
                }
            |]
        [whamlet|
            <form method=get action=@{SearchR}>
                ^{searchWidget}
                <input type=submit value=Search>
            $if not $ null searchResults
                <h1>Results
                $forall result <- searchResults
                    <div .result>
                        <a href=@{DocR $ resultId result}>#{resultTitle result}
                        <div .excerpt>#{resultExcerpt result}
        |]

getXmlpipeR :: Handler TypedContent
getXmlpipeR =
    respondSourceDB "text/xml"
 $  fullDocSource
 $= renderBuilder def
 $= CL.map Chunk

entityToEvents :: (Entity Doc) -> [X.Event]
entityToEvents (Entity docid doc) =
    [ X.EventBeginElement document [("id", [X.ContentText $ toPathPiece docid])]
    , X.EventBeginElement content []
    , X.EventContent $ X.ContentText $ unTextarea $ docContent doc
    , X.EventEndElement content
    , X.EventEndElement document
    ]

fullDocSource :: Source (YesodDB Searcher) X.Event
fullDocSource = do
    mapM_ yield startEvents
    docSource
    mapM_ yield endEvents

docSource :: Source (YesodDB Searcher) X.Event
docSource = selectSource [] [] $= CL.concatMap entityToEvents

toName :: Text -> X.Name
toName x = X.Name x (Just "http://sphinxsearch.com/") (Just "sphinx")

docset, schema, field, document, content :: X.Name
docset = toName "docset"
schema = toName "schema"
field = toName "field"
document = toName "document"
content = "content" -- no prefix

startEvents, endEvents :: [X.Event]
startEvents =
    [ X.EventBeginDocument
    , X.EventBeginElement docset []
    , X.EventBeginElement schema []
    , X.EventBeginElement field [("name", [X.ContentText "content"])]
    , X.EventEndElement field
    , X.EventEndElement schema
    ]

endEvents =
    [ X.EventEndElement docset
    ]

main :: IO ()
main = runStdoutLoggingT $ withSqlitePool "searcher.db3" 10 $ \pool -> liftIO $ do
    runSqlPool (runMigration migrateAll) pool
    warp 3000 $ Searcher pool
```
