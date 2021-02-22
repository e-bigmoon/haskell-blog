---
title: Web Application Interface
published: 2021/02/22
---

<div class="yesod-book-notice">
この章ではWAI version 3.0を扱うが, これは前のバージョンと多数の変更点がある. 
</div>

これはほとんど全てのウェブ開発に使われる言語が対処してきた問題である: ウェブサーバとアプリケーション間におけるローレベルのインターフェース. 初期における解決策としては立派で枯れたCommon Gateway Interface(CGI)であり, 言語によらないインターフェースを提供しており, 標準入力, 標準出力と環境変数のみを用いている. 

Perlが事実上のウェブプログラミング言語になりつつあった頃に遡ると, CGIの主な短所は明確になった: プロセスが各リクエスト毎に新たに開始される必要があった. インタープリタ型言語やデータベースコネクションを要するアプリケーションを扱う際, このオーバーヘッドは耐え難いものであった. FastCGI(後のSCGI)はCGIの後継者として登場したが, 多くのプログラミング界の大部分は間違った方向に進んでいるように見えた.

各言語はサーバと相互作用するための独自の基準を作り始めた. mod_perl. mod_python. mod_php. mod_ruby. 同じ言語内において, 複数のインターフェースが生じた. ある場合には, インターフェースの上にインターフェースがあった. そしてこれら全てはかなり重複した作業を要した: FastCGIと機能するように設計されたPythonアプリケーションはmod_pythonでは機能しなかった; mod_pythonはあるウェブサーバのためだけに存在した; そしてこれらのプログラミング言語固有のサーバ拡張が, 各プログラミング毎に書かれる必要があった. 

Haskellには独自の歴史がある. 元々cgiパッケージがあり, モナドインターフェースを提供していた. そしてfastcgiパッケージは同じインターフェースを与えた. 一方, Haskellウェブ開発の大部分はスタンドアロンサーバに焦点を当てた. 問題としては, 各サーバが独自のインターフェースを有したことであり, 特定のバックエンドを標的にする必要があったことを意味する. これは, GZIPエンコーディング, 開発サーバ, テストフレームワークのような共通の機能を共有することが不可能であることを意味する.

WAIはこれを解決しようとし, ウェブサーバとアプリケーション間における効率的で包括的なインターフェースを提供した. インターフェースをサポートする全てのハンドラが, あらゆるWAIアプリケーションにサーブでき, またそのインターフェースを用いるあらゆるアプリケーションが全てのハンドラで実行可能である. 

これを書いている時には, Warp, FastCGI, そして開発サーバのように様々なバックエンドがある. wai-handler-webkitのようにデスクトップアプリケーションを作るためのより秘伝のバックエンドも存在する. wai-extraはGZIP, JSON-P, そしてvirtual hostingのような多くの共通のミドルウェアコンポーネントを提供する. wai-testによってユニットテストを書くのが容易になり, wai-handler-develによりコンパイルするために止める心配をせずにアプリケーションを開発することが可能になる. YesodはScottyやMFlowのような他のHaskellウェブフレームワークと同様, WAIをターゲットにする. それはHoogleを含む, フレームワークを完全にスキップするアプリケーションによっても用いられている. 

<div class="yesod-book-notice">
Yesodはdevel serverに対し別のアプローチを取り, それはyesod develとして知られている. wai-handler-develとの相違としてはyesod develは実際にコードを毎回コンパイルし, cabalファイルのあらゆる設定を遵守することである. これは一般的なYesod開発において推奨される方法である. 
</div>

## インターフェース

インターフェース自身は非常に率直である: アプリケーションはリクエストを取り, レスポンスを返す. レスポンスはHTTPステータス, ヘッダのリストとリクエストボディである. リクエストは様々な情報を含む: リクエストされたパス, クエリストリング, リクエストボディ, HTTPバージョンなどである. 

リソースマネジメントを例外安全な方法で扱うために, 継続渡しスタイルを用いてレスポンスを返す. `bracket`関数の仕様と同様である. これによりアプリケーションの定義は次のようなる. 

``` haskell
type Application =
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
```

最初の引数は`Request`であり, これはあまり驚くべきことではない. 2つ目は継続, あるいは`Response`でなされるべきことである. 一般的にこれはだだクライアントに送っているだけである. 特別な`ResponseReceived`型を用いてアプリケーションが実際に継続を呼び出したかを確認できる. 

これは少し奇妙に見えるが, 使い方は非常に単純であり, それを以下に示す. 

## Response Body

Haskellはlazy bytestringとして知られるデータ型を持つ. 遅延性を利用することでメモリを浪費せずに大きな値を作ることができる. 遅延I/Oを用いることで, ファイルコンテンツ全体を表すが, 小さなメモリ領域しか占領しない. 理論的に, lazy bytestringはレスポンスボディのために必要な唯一の表現である. 

実践的には, lazy bytestringは"純粋"な値を生成するのには有用で, ファイルを読むのに必要な遅延I/Oはプログラムに対し, ある非決定性を導入する. 1秒で数千のファイルを扱うために, 制限となるのはメモリではなく, ファイルハンドルである. 遅延I/Oを用いることでファイルハンドルはすぐには開放されず, リソース浪費になる. これに対処するために, WAIは独自のストリーミングデータインターフェースを与える. 

このストリーミングインターフェースの中心は`Builder`である. `Builder`はバッファを`data`のバイトで満たすアクションを表現する. これは単純に`ByteString`を受け渡すよりも効率的である. なぜならば, データの複数コピーを避けられるためである. 多くの場合, アプリケーションは単一の`Builder`値を与える必要がある. そしてその単純な場合, `ResponseBuilder`コンストラクタがある.

しかし, `Application`がクライアントへのデータを生成するIOアクションを交互に挟む必要のある場合がある. その場合, `ResponseStream`を用いる. `ResponseStream`では, 関数を与える. この関数は2つのアクションを取る: "より多くのデータを生成する"アクションと, "バッファをフラッシュする"アクションである. これによりデータを生成し, IOアクションを行い, フラッシュすることを必要な回数だけ行い, 交互に挟む操作も必要なだけ持たせることができる. 

1つさらに最適化することができる: 多くのオペレーティングシステムはsendfileシステムコールがあり, これはファイルを直接ソケットに送り, より一般的なI/Oシステムコールに特有のメモリコピーを多く迂回する. この場合, `ResponseFile`を用いる. 

最後に, HTTPモードから完全に抜け出す必要のある場合がある. 2つの例として, WebSocketsでは半二重通信HTTP接続を完全な二重通信接続にアップグレードする必要がある. HTTPSプロキシでは, プロキシサーバが接続を確立し, ダムデータを運送する媒体になる必要がある. これらの場合, `ResponseRaw`コンストラクタを用いる. 必ずしも全てのWAIハンドラが実際に`ResponseRaw`をサポートをしている訳ではないことに注意せよ, しかし, 最も一般的に用いられているWarpハンドラはこれをサポートする. 

## Request Body

レスポンスボディと同様に, 理論的にはlazy ByteStringをリクエストボディに用いるが, 実践的には遅延I/Oは避けたい. 代わりに, リクエストボディはIO ByteStringにより表現される(ここでのByteStringはstrict ByteStringである). これはリクエストボディ全体でなく, データの次のチャンクを返すことに注意せよ. 一度リクエストボディ全体を消費したら, このアクションに対する次の呼び出しは空ByteStringを返す. 

次のことに注意せよ. レスポンスボディと異なり, リクエスト側で`Builder`を用いる必要がない. なぜならば, 目的としては単に読むだけだからである. 

理論的にはリクエストボディはどんな型のデータも含むことができるが, 最も一般的なのはURLエンコードされたデータとmultipart formデータである. wai-extraパッケージはこれらをメモリ上効率的な方法でパースする組み込みのサポートを含む.

## Hello World

WAIの単純さを示すために, hello worldの例を見てみよう. OverloadedString言語拡張を用いて, 明示的に文字列をbytestringにパックしなければならないのを避ける. 

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

application _ respond = respond $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main = run 3000 application
```

2行目から4行目ではインポートを行っている. Warpはwarpパッケージにより与えられ, 主要なWAIバックエンドである. WAIはまた, http-typesパッケージのトップにあり, 多くのデータ型と`status 200`を含む便利な値を含む. 

最初にアプリケーションを定義する. 特定のリクエストパラメータには関心がないため, 関数の最初の引数は無視する. これは, リクエスト値を含む. 2つ目の引数は"レスポンスを送る"関数であり, すぐに使うことになる. 送信するレスポンス値はlazy ByteString(よって respondLBS)で作られ, ステータスコード200("OK"), text/plainコンテンツタイプ, そして"Hello World"を含むボディを持つ. 非常に単純である. 

## Resource allocation

これをもう少し面白いものにして, レスポンスに対しリソースを割り当ててみよう. `MVar`を`main`関数に作りリクエスト数を追跡し, その`MVar`を各レスポンスを送る際に所持する.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder           (fromByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import           Control.Concurrent.MVar
import           Data.Monoid                        ((<>))
import           Network.HTTP.Types                 (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp           (run)

application countRef _ respond = do
    modifyMVar countRef $ \count -> do
        let count' = count + 1
            msg = fromByteString "You are visitor number: " <>
                  fromShow count'
        responseReceived <- respond $ responseBuilder
            status200
            [("Content-Type", "text/plain")]
            msg
        return (count', responseReceived)

main = do
    visitorCount <- newMVar 0
    run 3000 $ application visitorCount
```

これはWAIの継続渡しインターフェースが活躍する場である. 標準`modyfyMVar`関数を用いて`MVar`ロックを取得し, レスポンスを送る. どのようにresponseReceivedを挿入したかについて注意せよ, 実際にはその値は何にも使われていないが. これは単に, 実際にレスポンスを送ったことを明示しただけである. 

またどのように`Builder`を利用し`msg`値を構築するかについて注意せよ. 2つのByteStringを直接連結する代わりに, モノイドとして2つの異なる`Builder`値を連結する. これの利点は, 最初に一時的な`ByteString`バッファにコピーされ, 後に最終的なバッファにコピーされる代わりに, 結果が直接最終的な出力バッファにコピーされることである.

## Streaming Response

ストリーミングインターフェースを同様に試してみよう.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder (fromByteString)
import           Control.Concurrent       (threadDelay)
import           Network.HTTP.Types       (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)

application _ respond = respond $ responseStream status200 [("Content-Type", "text/plain")]
    $ \send flush -> do
        send $ fromByteString "Starting the response...\n"
        flush
        threadDelay 1000000
        send $ fromByteString "All done!\n"

main = run 3000 application
```

`responseStream`を用い, 3番目の引数は"ビルダを送信する", そして"バッファをフラッシュする"関数である. クライアントがすぐにデータを見れるようにするために, 最初のデータチャンクの後にどのようにデータをフラッシュするかについて注意せよ. しかし, レスポンスの最後にフラッシュする必要はない. WAIはハンドラが自動的にストリームの最後でフラッシュすることを要求する.

## Middleware

アプリケーションをコードの変更なしに複数のバックエンドで実行することを可能にすることの他に, WAIには他の利点がある: ミドルウェア. ミドルウェアは本質的にはアプリケーショントランスフォーマであり, あるアプリケーションを取り, 他のを返す.

ミドルウェアのコンポーネントは多くのサービスを提供するために使われる: URLの整理, 認証, キャッシュ, JSON-Pリクエスト. しかしおそらくもっとも有益で直感的なミドルウェアはgzip圧縮である. ミドルウェアは非常に単純に機能する: リクエストヘッダをパースし, クライアントが圧縮をサポートするか決定し, もしそうならばレスポンスボディを圧縮し, 適切なレスポンスヘッダを付け加える. 

ミドルウェアの素晴らしい点は, 目障りでないことである. どのようにgzipをhello worldアプリケーションに適応するかを見てみよう.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.HTTP.Types (status200)

application _ respond = respond $ responseLBS status200 [("Content-Type", "text/plain")]
                       "Hello World"

main = run 3000 $ gzip def application
```

インポート行を付け加え, 実際にミドルウェアへのアクセスを可能にし, 単純にgzipをアプリケーションに適応する. 複数のミドルウェアをつなぎ合わせることも可能である: `gzip False $ jsonp $ othermiddleware $ myapplication`のような行は完全に適切である. ひとつ警告がある: ミドルウェアが適応される順序は重要である. 例えば, jsonpは非圧縮データ上で機能する必要がある. よってもしそれをgzipの後に適応したら, 困難に陥ることになる.