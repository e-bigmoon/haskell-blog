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
