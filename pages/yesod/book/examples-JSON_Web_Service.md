---
title: JSON Web Service
date: 2020/09/12
---

非常に単純なwebサービスを作りましょう: これは, JSONリクエストを取り, JSONレスポンスを返す. サーバーをWAI/Warpで書き, クライアントを`http-conduit`で書く. また, `aeson`をJSONのパージングとレンダリングに用いる. サーバーをYesod自身を用いて書くこともできるが, このような単純な例においては, Yesodの機能は余分であり不必要である.

## Server

WAIは`conduit`パッケージを用いて, ストリーミングリクエストボディを処理する. そして, `blaze-builder`を用いて効率的にレスポンスを生成する. `aeson`はパージングに`attoparsec`を用いる; `attoparsec-conduit`を用いることで, WAIと容易に相互運用できる. コードは次のようになる:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception        (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, encode, object, (.=))
import           Data.Aeson.Parser        (json)
import           Data.ByteString          (ByteString)
import           Data.Conduit             (($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, Response, responseLBS)
import           Network.Wai.Conduit      (sourceRequestBody)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3000 app

app :: Application
app req sendResponse = handle (sendResponse . invalidJson) $ do
    value <- sourceRequestBody req $$ sinkParser json
    newValue <- liftIO $ modValue value
    sendResponse $ responseLBS
        status200
        [("Content-Type", "application/json")]
        $ encode newValue

invalidJson :: SomeException -> Response
invalidJson ex = responseLBS
    status400
    [("Content-Type", "application/json")]
    $ encode $ object
        [ ("message" .= show ex)
        ]

-- Application-specific logic would go here.
modValue :: Value -> IO Value
modValue = return
```

## Client

http-conduitはWAIの仲間として書かれている. これも`conduit`と`blaze-builder`をいたる所で用いており, このことは再び`aeson`と相互運用が容易であることを意味する. `http-condit`に不慣れな人のために多少のコメントを付け加える:

- `Manager`はオープンコネクションを追跡するために存在する. その結果, 同じサーバに対する複数のリクエストで同じコネクションを用いることができる. 通常はグローバルコネクションマネージャを取得するために, `getGlobalManager`を用いる.

- リクエストボディのサイズを知る必要があるが, それは直接`Builder`からは決定されない. 代わりに, `Builder`をlazy `ButeString`に変換し, そこからサイズを取得する.

- リクエストを開始するための関数は数多く存在する. `http`を用いることで, 直接データストリームにアクセスできる. 他にも高度な関数(例えば`httpLbs`のような)が存在し, ソースに関する問題を考えずに, 直接ボディ全体を取得できる.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (Value (Object, String))
import           Data.Aeson              (encode, object, (.=))
import           Data.Aeson.Parser       (json)
import           Data.Conduit            (($$+-))
import           Data.Conduit.Attoparsec (sinkParser)
import           Network.HTTP.Conduit    (RequestBody (RequestBodyLBS),
                                          Response (..), http, method, parseUrl,
                                          requestBody, getGlobalManager)

main :: IO ()
main = do
    manager <- getGlobalManager
    value <- liftIO makeValue
    -- We need to know the size of the request body, so we convert to a
    -- ByteString
    let valueBS = encode value
    req' <- liftIO $ parseUrl "http://localhost:3000/"
    let req = req' { method = "POST", requestBody = RequestBodyLBS valueBS }
    res <- http req manager
    resValue <- responseBody res $$+- sinkParser json
    liftIO $ handleResponse resValue

-- Application-specific function to make the request value
makeValue :: IO Value
makeValue = return $ object
    [ ("foo" .= ("bar" :: String))
    ]

-- Application-specific function to handle the response from the server
handleResponse :: Value -> IO ()
handleResponse = print
```