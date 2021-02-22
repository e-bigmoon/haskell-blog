---
title: 'http-conduit'
published: 2020/02/22
---

# http-client tutorial

http-clinentは比較的ローレベルAPIを持ち, TLS(HTTPS)をサポートしない最小限のパッケージである. このチュートリアルではhttp-conduitパッケージの`Network.HTTP.Simple`モジュールを扱う. これは高レベルのインターフェースを提供する. 

## API docs

API文書は以下にある:

- ['http-client'](https://www.stackage.org/lts-16.27/package/http-client-0.6.4.1)

- ['http-conduit'](https://www.stackage.org/lts-16.27/package/http-conduit-2.3.7.4)

## Tutorial exercise

学習の動機づけとして, チュートリアルを読み進める際, 以下の例に留意し, 解答を実装してみてください. 行毎に1つのURLを持つ入力ファイルを取るようなプログラムを書き, 各URLにリクエストすることでエラーでないステータスコードが返ることを確認しなさい.

## Basic Usage

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

`httpLBS`は与えられたURLに対しリクエストを行い, レスポンスボディをlazy `ByteString`として捕らえる. これはlazy `ByteString`であるが, リクエストをする際には完全にメモリに読み込むことに注意せよ. 効率的にメモリを使用するためだけにlazy `ByteString`を返しているだけであう. (詳細については下のストリーミングを参照せよ.)

一度レスポンスボディを得ることができれば, getter関数を用いて様々な詳細について見ることができる(ステータスコード, ヘッダ, そしてボディ).

## Receiving JSON

JSONメッセージを得るためにaesonを使うこともできる.

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpJSON "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

主な変更点は`httpLBS`の代わりに`httpJSON`を用いたことである. この関数は`FromJSON`のどんなインスタンスも返すことができ, あらゆる必要なパーズと変換を行う. もし何か問題があれば, 実行時例外を投げる(`httpJSONEither`を用いることで実行時例外を避け, `Either`を得よ).

戻り値は`FromJSON`のどのインスタンスにもなれるため, 戻り値を何らかの方法で制約する必要がある. この場合, 明確に(`::Value`)注釈を用いたが, たいていカスタムデータ型を用いることで制約を行う.

遊びのために, この例ではJSONボディをYAMLフォーマットで出力する. 

## Advanced Use

追加パッケージがあり, さらなる機能を与えてくれる, 例えば:

- ['http-client-tls'](https://www.stackage.org/package/http-client-tls)はHaskell-native tlsパッケージを通しTLSサポートを提供する. 

- ['http-conduit'](https://www.stackage.org/package/http-conduit)はconduitを用いたストリーミングリクエストとレスポンスを可能にする.

## Concepts

このライブラリは`OverloadedStrings`言語拡張を有効活用し, 文字列を`Request`, `ByteString`, そして大文字小文字を区別しない`ByteString`(ヘッダ名のために)に変換する. このライブラリにおいては, この言語拡張を用いることが強く推奨される.

## Caveats

このライブラリについて, 言及すべきいくつか重要な注意事項がある.

- デフォルトでは, 2xxでないステータスコードレスポンスは実行時例外にならず, これは以前のライブラリ挙動に反している(バージョン0.5より以前).

- デフォルトでは, http-clientは`http_proxy`と`https_proxy`環境変数を優先する. 下のプロキシの例を参照し, どのようにこれを迂回するかについての情報を得なさい.

## Request methods and parseRequest

URLの最初のリクエストメソッドを指定できる.

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpJSON "POST http://httpbin.org/post"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

実際に起こっていることは`Request`の`IsString`インスタンスが使われ, 文字列リテラルを`Request`にしていることである. しかし`parseRequest`を用いればより明瞭にできる:

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    request <- parseRequest "POST http://httpbin.org/post"
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

`parseRequest`は例外に対しより明瞭であるが, 文字列による方法ではもしコードに誤植があると実行時エラーになる. 一般的に`parseRequest`は実行時に生成されるURLをパーズする場合に選択されるべきである.

注意: もし有効でない文字列リテラルとして有効でないURLを与えた場合, ピュアな`Request`値を強要し実行時例外として表示される.

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "BAD URL"
    print response
```

は以下を生成する:

``` haskell
foo.hs: InvalidUrlException "BAD URL" "Invalid URL"
```

## Request building

リクエストには, ただのリクエストメソッド以外にもより多くが存在する. これらは様々なリクエストのセッタメソッドを用いて変換される.

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    request' <- parseRequest "POST http://httpbin.org/post"
    let request
            = setRequestMethod "PUT"
            $ setRequestPath "/put"
            $ setRequestQueryString [("hello", Just "world")]
            $ setRequestBodyLBS "This is my request body"
            $ setRequestSecure True
            $ setRequestPort 443
            $ request'
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

読者への練習問題: 上のコードを`parseRequest`を使わない形で書き換えよ.

実際に, もし望むのならば, URLパーズを用いずにリクエストを完全にプログラム的に書くことができる:

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do
    let request
            = setRequestPath "/get"
            $ setRequestHost "httpbin.org"
            $ defaultRequest
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

## Request bodies

レスポンスボディのように, 多くのヘルパ関数が存在し, 様々なリクエストボディのフォーマットを扱うことができる. 

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

data Person = Person String Int
instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age"  .= age
        ]

people :: [Person]
people = [Person "Alice" 30, Person "Bob" 35, Person "Charlie" 40]

main :: IO ()
main = do
    let request = setRequestBodyJSON people $ "POST https://httpbin.org/post"
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

あるいはファイルからのデータ:

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

data Person = Person String Int
instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age"  .= age
        ]

people :: [Person]
people = [Person "Alice" 30, Person "Bob" 35, Person "Charlie" 40]

main :: IO ()
main = do
    Yaml.encodeFile "people.yaml" people

    let request = setRequestBodyFile "people.yaml"
                $ setRequestHeader "Content-Type" ["application/x-yaml"]
                $ "PUT https://httpbin.org/put"
    response <- httpJSON request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
```

## Non-2XX responses

デフォルトでは, ライブラリのバージョン0.5からだが, もしただの文字列リテラルをリクエスト生成のために用いた場合, 2XXでないレスポンスを生成する全てのリクエストが, 実行時例外を投げるわけではない. しかし, 例外が2XXでないレスポンスステータスで返されるか否かは, 対応するリクエストの設定に依存しており, バージョン0.5では`checkResponse`, より古いバージョンでは`checkStatus`と呼ばれる. したがって, 文字列からリクエストを構築する方法は, ライブラリが2XXでないレスポンスステータスコードに対し, 例外を投げるかどうかを決定する. 

すべてのパージング関数を調べ, どれが`例外を投げる`リクエストであるか特定しましょう.

- `parseUrl`は廃止され, `parseUrlThrow`と同じである.

- `parseUrlThrow`は`checkResponse`アクションを持ったリクエストを生成し, レスポンスが2XXでないステータスコードを取った場合, 例外を投げる. 

- `parseRequest`は"安全な"リクエストを生成し, 2XXでないレスポンスステータスコードを取った場合, 例外を投げない(これはそれらが全く例外を投げないことを意味しているわけではない, リクエストを作るにおいて他の問題があるかもしれないためである).

- `parseRequest_`は`parseRequest`と同じであり, もし与えられた文字列が不正な形式の場合, 実行時に消滅するだけである. これはリクエストを`Request`の`IsString`インスタンスの文字列リテラルからリクエストをパーズするために用いられるものである. 

## Exceptions

コネクションが失敗した場合のように, このライブラリによって投げられる可能性のある例外は他にもある. これらをキャッチするために, `HttpException`例外型をキャッチすべきである. 

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception          (try)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    eresponse <- try $ httpLBS "http://does-not-exist"

    case eresponse of
        Left e -> print (e :: HttpException)
        Right response -> L8.putStrLn $ getResponseBody response
```

## Streaming

時々レスポンスボディ全体を一度にメモリに読みこむのを避けたいかもしれない. これらの場合, ストリーミングデータによる方法は役に立つ. 

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as S
import qualified Data.Conduit.List      as CL
import           Network.HTTP.Simple
import           System.IO              (stdout)

main :: IO ()
main = httpSink "http://httpbin.org/get" $ \response -> do
    liftIO $ putStrLn
           $ "The status code was: "
          ++ show (getResponseStatusCode response)

    CL.mapM_ (S.hPut stdout)
```

## Override proxy

デフォルトでは, `http_proxy`, あるいは`https_proxy`環境変数によって指定された何らかのプロキシサーバを用いる. これは上書き可能である:

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
    let request = setRequestProxy (Just (Proxy "127.0.0.1" 3128))
                $ "https://httpbin.org/get"
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

## Connection Manager

すべてのHTTPリクエストは`Manager`を通して作られる. `Manager`はサーバへのコネクションを作るために用いられる. これはサーバへのコネクションを再利用するようなことを扱う(同じホストに対し複数のリクエストを送る場合, 高度なTCPの負荷を避けるために). これはまた, 安全なコネクション(HTTPs)を確実にするために最も重要な, 様々な設定を構成できるようにする.

使いやすさと, アプリケーションにおける最大限のコネクション共有を確実にするために, `Network.HTTP.Simple`モジュールは共有のグローバルコネクション`Manager`をデフォルトで用いる. 望むのならば, 独自の`Manager`を生成しグローバルのものを上書きできる:

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.HTTP.Simple

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings

    let request = setRequestManager manager "http://httpbin.org/get"
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

問題:

1. 上のコードを変更し, 代わりにHTTPSコネクションを作れ. 何が起こるか?
2. 前のステップで生成されたエラーを以下を用いることで修正せよ. `Network.HTTP.Client.TLS.tlsManagerSettings`

例えばいくつかの設定を微調整したい場合もグローバルマネージャを上書きできる:

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple

main :: IO ()
main = do
    manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
    setGlobalManager manager

    let request = "http://httpbin.org/get"
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

目的のために`tlsManagerSettings`を用い, (全ての下の例がそうしているように)完全なHTTPとHTTPSのサポートを保証すべきである. 

## Lower level API

上の文書全ては`Network.HTTP.Simple` APIを扱っている. しかし, Network.HTTP.ClientにはローレベルのAPIが存在し, いくつかの場合においては有益である. この章の残りでは, このローレベルAPIのいくつかの例を与える. 

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager

    putStrLn $ "The status code was: " ++
               show (statusCode $ responseStatus response)
    print $ responseBody response
```

`newManager tlsManagerSettings`を用いて新しい`Manager`を得, `parseRequest`を行いテキスト形式のURLを`Request`に変換し, そして, `httpLbs`を用いてリクエストを行っている. 一度`Response`を得てしまえば, 標準的なアクセッサを用いてそのフィールドを調べられる. 

## Receiving JSON

このストリーミングをaesonを用いてJSONをパーズし構成することも, 容易である. 

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import           Data.Aeson.Parser           (json)
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Types.Status   (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "http://httpbin.org/get"

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        value <- bodyReaderSource (responseBody response)
              $$ sinkParser json
        print value
```

## Sending JSON

JSONの送信は, リクエストメソッドとボディの変更によってなされる:

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson                 (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    -- Create the request
    let requestObject = object
            [ "name" .= ("Alice" :: String)
            , "age"  .= (35 :: Int)
            ]
    initialRequest <- parseRequest "http://httpbin.org/post"
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode requestObject
            , requestHeaders =
                [ ("Content-Type", "application/json; charset=utf-8")
                ]
            }

    response <- httpLbs request manager
    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

他の一般的なリクエストボディ形式はURLエンコード化されたボディである. `urlEncodedBody`関数はこれを行うための便利な関数である. それは自動的にリクエストメソッドを`POST`にすることに注意せよ, また, もし望むのならそれを上書きできる.

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    initialRequest <- parseRequest "http://httpbin.org/put"
    let pairs =
            [ ("name", "Alice")
            , ("age", "35")
            ]
        request = (urlEncodedBody pairs initialRequest)
            { method = "PUT"
            }

    response <- httpLbs request manager
    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

## Non-2XX respinses

`checkeResponse`レコードセレクタ(http-clientのバージョン0.5から始まった)により, リクエストとレスポンスを調べ, もし何か間違っていれば例外を投げることができる. 0.5よりも古いバージョンでは2XXでないレスポンスステータスコードは例外を投げるが, 現在は変更され`checkResponse`はデフォルトでは何も行わない. 

## Proxy settings

デフォルトでは, http-clientは`http_proxy`と`https_proxy`環境変数を優先する. `Manager`を生成する際これを変更できる:

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager $ managerSetProxy noProxy tlsManagerSettings

    response <- httpLbs "http://httpbin.org/get" manager

    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

リクエスト毎にプロキシ設定を変更することもできる:

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    let request = "http://httpbin.org/get"
            { proxy = Just $ Proxy "127.0.0.1" 3128
            }
    response <- httpLbs request manager

    putStrLn $ "The status code was: "
            ++ show (statusCode $ responseStatus response)
    L8.putStrLn $ responseBody response
```

もしマネージャとリクエストボディのプロキシを上書きした場合, マネージャの設定が優先される. 

## Shareing the Manager

`Manager`を初期化するために多少のコストがかかる. さらに重要なことだが, 各`Manager`は自身のコネクションプールを持っている. アプリケーション全体を通して`Manager`値を共有することが非常に推奨される. これはTCPハンドシェイクの負荷を減らし, 一度に単一のサーバに対し多すぎるコネクションをするのを少なくしてくれる. 

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent.Async  (Concurrently (..))
import qualified Data.ByteString.Char8     as S8
import qualified Data.ByteString.Lazy      as L
import           Data.Foldable             (sequenceA_)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    runConcurrently $ sequenceA_ $ replicate 16
                    $ Concurrently $ doSomething manager

doSomething :: Manager -> IO ()
doSomething manager = do
    let request = "http://httpbin.org/get"

    response <- httpLbs request manager

    let msg = encodeUtf8 $ T.pack $ concat
            [ "Got a message with status code "
            , show $ statusCode $ responseStatus response
            , " with response body length "
            , show $ L.length $ responseBody response
            , "\n"
            ]

    -- Using bytestring-based output to avoid interleaving of string-based
    -- output
    S8.putStr msg
```

## Streaming

すでに見たconduitに基づいたストリーミングAPIの下に, ローレベルのストリーミングAPIが存在する:

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.21
import qualified Data.ByteString           as S
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           System.IO                 (stdout)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest "http://httpbin.org/get"

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        let loop = do
                bs <- brRead $ responseBody response
                if S.null bs
                    then putStrLn "\nFinished response body"
                    else do
                        S.hPut stdout bs
                        loop
        loop
```

## Exercise

ファイルにある全てのURLを確認し, それらがエラーでないステータスコードを返すことを保証せよ. 各行はそれ自身のURLである. 
- `http://...`と`https://...`URLの両方をサポートせよ.
- conduitを使ってファイルの中身をストリーミングせよ.
- チェックを並列に行え.
- 本当のボーナス: html-conduitをパーズに用い, ダウンロードされたHTMLページのURLを確認せよ.