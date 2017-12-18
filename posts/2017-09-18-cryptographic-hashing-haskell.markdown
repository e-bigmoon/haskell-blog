---
title: Haskell で暗号学的ハッシュを扱う
author: Michael Snoyman
translator: pythonissam
tags: fpcomplete
---

Great original post: [CRYPTOGRAPHIC HASHING IN HASKELL.](https://www.fpcomplete.com/blog/2017/09/cryptographic-hashing-haskell).

2017年 9月 18日 Michael Snoyman

[cryptonite](https://www.stackage.org/package/cryptonite) は現在、Haskell で暗号を扱う際のデファクトスタンダードです。
一般的に安全な乱数、共通鍵・公開鍵暗号、MAC (メッセージ認証符号)等をサポートしていて、その中には今日の話題、暗号学的ハッシュも含まれています。

まず、ハッシュ関数について軽く説明しましょう。ハッシュ関数とは、任意長のデータを固定長のデータに変換するものです。暗号学的ハッシュ関数は、暗号を扱う上で望ましい性質をそなえたハッシュ関数です (詳しくは [Wikipedia](https://en.wikipedia.org/wiki/Cryptographic_hash_function) を参照)。
一般的な暗号学的ハッシュ関数の使われ方の一例として、ダウンロードされたファイルが改ざんされていないことを保証するための、チェックサムを提供する、というものがあります。
今日使われている暗号学的ハッシュ関数には、SHA256、Skein512、そしてまぁ、ちょっと古いですが MD5 などがあります。

`cryptonite` は [memory](https://www.stackage.org/package/memory) というライブラリの一番上の階層にあります。この `memory` ライブラリは、Byte 配列を扱うための型クラスと便利な関数を提供しています。
「全て `ByteString` でいいのでは?」と思うかもしれませんが、後ほどこの型クラスの便利さを示します。

<!--more-->

一旦これら 2つのライブラリに慣れれば、簡単に使いこなすことができます。
ですが、API のドキュメントを見るだけでは、部分部分がどう組み合わさるのか理解するのは至難の技です。特に、どこで明示的な型シグネチャが必要になるのかの理解が難しい。
この記事では、理解に必要な部分について1つ1つの簡単な例を、実行可能なコードで紹介します。
読み進める中で、API のドキュメントについてざっくりと理解していってください。

この記事のコードの例は、全て `Stack` のスクリプトインタプリタ機能を使っています。
まず `Stack` を[インストール](https://haskell-lang.org/get-started)して、次の手順で実行してください。

- `Main.hs` にコードをコピペする
- `stack Main.hs` を実行

## 基本的な型クラス
文字列っぽい型を扱うのは慣れているでしょう? 正格・遅延評価される `ByteString` や `Text` や普通の `String` などです。
正格なバイトシーケンスを表現する際、`Data.ByteString.ByteString` を思い浮かべるのではないでしょうか。
しかし、これから見るように、バイトシーケンスとして扱いたい型はいろいろ見つかります。

`memory` はこの要望に答えるべく、以下の 2つの型クラスを定義しています。

- `ByteArrayAccess` ある型のバイトへ、読み取り専用のアクセスを提供
- `ByteArray` 読み / 書きのアクセスを提供する。`ByteArrayAccess` の子クラス

例えば、以下に示すコードは、意味もなく `ByteString` と `Byte` の変換をしています。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

main :: IO ()
main = do
  B.writeFile "test.txt" "This is a test"
  byteString <- B.readFile "test.txt"
  let bytes :: BA.Bytes
      bytes = BA.convert byteString
  print bytes
```

`bytestring` ライブラリを使ってファイルの入出力から話を始めたのは、[入出力は bytestring でやるべき](http://www.snoyman.com/blog/2016/12/beware-of-readfile) だからです。
`convert`関数を使うと、`ByteString` を `Bytes` に変換することができます。

**練習問題** 上の2つの型クラスの説明を踏まえて `convert` の型シグネチャは何だと思いますか? 答えは[こちら](https://www.stackage.org/haddock/lts-9.4/memory-0.14.7/Data-ByteArray.html#v:convert)。

`bytes` の値につけた、明示的な型シグネチャに気がつきましたか?
えーと、`memory` と `cryptonite` を使っていく上で、これは大事です。
こうして、よく GHC に、型推論についてのヒントを与えてやらないといけなくなります。
なぜなら、これらのライブラリの中のかなり多くの関数が、具体的な型ではなく、型クラスを使っているからです。

さて、`ByteArrayAccess` に属する型の例をお見せしましたが、それは`ByteArray` についての例ではありませんでした。
今は型クラスを分ける意味が分からないかもしれませんが、実際にハッシュを使う段階で、型クラスを分けることの利点が見えてくると思います。
ちょっと待ってくださいね。

### なぜ違う型があるのか
当然、`memory` の中になぜ `Bytes` という型があるのか、疑問に思う人もいるでしょうね。
`ByteString` と同じじゃないの? ってね。
まぁ違うんですけどね。
`Bytes` はメモリスライスのオフセットと長さを追跡しないことで、メモリのオーバーヘッドを小さくしています。
その代わりに、`Bytes` の値をスライスすることは許されません。
言い換えれば、`Bytes` に関する `drop`関数は、バッファの新しいコピーを作らなければならないということです。

まぁつまり、これはパフォーマンスの問題です。暗号を扱うライブラリは、一般的にパフォーマンスを重視する必要がありますからね。

また別の `memory` のおもしろい型に、[`ScrubbedBytes`](https://www.stackage.org/haddock/lts-9.4/memory-0.14.7/Data-ByteArray.html#t:ScrubbedBytes) というものがあります。
この型は、3つの特別な性質を有しています。
`Haddock` によると、

- スコープの範囲から出るとゴシゴシされる
- `Show` インスタンスは、中身を何1つとして出力しない
- 定数時間の `Eq` インスタンス

つまり、これらの性質は何か機密性の高いデータを扱うとき、ありふれた脆弱性をいくつも塞いでくれるものです。

うん、コードがあまりない説明になりましたね。
もっと楽しいことをしよう!

## Base16 エンコード/デコード
ユーザの入力を、Base16 (16進数) に変換してみましょう。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import qualified Data.ByteArray          as BA
import           Data.ByteArray.Encoding (convertToBase, Base (Base16))
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let encoded = convertToBase Base16 bs :: ByteString
  putStrLn $ "Converted to base 16: " ++ show encoded
```

`convertToBase` は、どんな `ByteArrayAccess` も、与えられた基数を使って `ByteArray` に変換することができます。Base16 以外の基数には、Base64 などがあります。

見てお分かりの通り、上の例では明示的に `ByteString` の型シグネチャを指定する必要がありました。なぜならそうしなければ、GHC が`ByteArrayAccess` のインスタンスの内、どれを使うべきなのか判断できないからです。

既にお分かりかもしれませんが、全く逆の変換を行う、`convertFromBase`関数も存在します。
この関数は、入力のフォーマットが正しくなかった場合にも対応できるように、`Either String ByteArray` の値を返します。

**練習問題** 入力に対して、Base16 のデコードを行うプログラムを書いてください
(解答はすぐ下)。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import qualified Data.ByteArray          as BA
import           Data.ByteArray.Encoding (convertFromBase, Base (Base16))
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some hexadecimal text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  case convertFromBase Base16 bs of
    Left e -> error $ "Invalid input: " ++ e
    Right decoded ->
      putStrLn $ "Converted from base 16: " ++ show (decoded :: ByteString)
```

*練習問題*
Base16 の入力を、Base64 のエンコードに変換するプログラムを書いてください。

## 正格な `ByteString` のハッシュ
よし、`memory` ライブラリに関する説明はもう十分でしょう。
これからは実際に暗号的なものをやっていきます。ユーザの入力を、SHA256 のハッシュ値 (ダイジェスト) に変換してみましょう。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import           Crypto.Hash             (hash, SHA256 (..), Digest)
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let digest :: Digest SHA256
      digest = hash bs
  putStrLn $ "SHA256 hash: " ++ show digest
```

たった今、`ByteString` (かもしくは `ByteArrayAccess` のインスタンス) を `Digest SHA256` に変換するために `hash` 関数を使いました。
実際、SHA256 以外のハッシュアルゴリズムも指定することができます。

今回の例では、`Digest SHA256` という型シグネチャが大事でした。GHC にどんなハッシュを使うのか知らせるためです。しかし次の例では、その代わりの関数が登場します。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import           Crypto.Hash             (hashWith, SHA256 (..))
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let digest = hashWith SHA256 bs
  putStrLn $ "SHA256 hash: " ++ show digest
```

`Digest` の `Show` インスタンスは、ダイジェストを16進数 (Base16) で表示します。これはいいですね。
しかし、これをBase64 で表示したい欲求にかられたらどうでしょう? 考えてみましょう。
`Digest` は `ByteArrayAccess` のインスタンスです。なので、`convertToBase` を使うことができます (そして、`Digest` は `ByteArray`のインスタンスではありません。そうしてしまったら問題が生じるのですが、それはなぜでしょうか? 行き詰まったら、[この関数のドキュメント](https://www.stackage.org/haddock/lts-9.3/cryptonite-0.23/Crypto-Hash.html#v:digestFromByteString)を読んでみましょう。答えが載っています。)。

**練習問題** ダイジェストを Base64 でエンコードされた文字列として出力してみましょう (答えはすぐ下)。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import           Crypto.Hash             (hashWith, SHA256 (..))
import           Data.ByteString         (ByteString)
import           Data.ByteArray.Encoding (convertToBase, Base (Base64))
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)

main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  let digest = convertToBase Base64 (hashWith SHA256 bs)
  putStrLn $ "SHA256 hash: " ++ show (digest :: ByteString)
```

`digest` が `ByteString` であることを明確にするために、型シグネチャが必要な理由を押さえてください。

## マッチするファイルがあるかどうか調べる
ここにちょっとしたプログラムがあります。ユーザはコマンドライン引数として、複数個のファイル名を渡します。そして、プログラムは同一の内容の全てのファイルのリストを表示します (少なくとも、SHA256 のハッシュ値がマッチするファイルを。それと、以下の定義には、メモリの効率がよろしくない部分があります。見つけてみてください。この点についてはまた後述します)。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import           Crypto.Hash             (Digest, SHA256, hash)
import qualified Data.ByteString         as B
import           Data.Foldable           (forM_)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           System.Environment      (getArgs)

readFile' :: FilePath -> IO (Map (Digest SHA256) [FilePath])
readFile' fp = do
  bs <- B.readFile fp
  let digest = hash bs -- notice lack of type signature :)
  return $ Map.singleton digest [fp]

main :: IO ()
main = do
  args <- getArgs
  m <- Map.unionsWith (++) <$> mapM readFile' args
  forM_ (Map.toList m) $ \(digest, files) ->
    case files of
      [] -> error "can never happen"
      [_] -> return () -- only one file
      --                                     unwords :: [String] -> String
      _ -> putStrLn $ show digest ++ ": " ++ unwords (map show files)
```

**練習問題** コマンドライン引数として与えられた全てのファイルの SHA256 ハッシュ値を表示するプログラムを書いてください。

**質問** 上のコードの、どこが非効率的なのでしょうか? 答えは次の章にあります。

## より効率的なファイルハッシュ
もしもハッシュ関数を使わなければ、さっきのプログラムの実装は、それぞれのファイルの中身をメモリ上に一度に展開するか、O(n^2) のペアの比較をするような変なものになっていたでしょう。
さっきのハッシュを使った実装は、それよりも良い実装です。しかし、まだ問題があります。
`Data.ByteString.readFile`を使っているので、際限なくメモリを使ってしまう可能性があります。
`cryptonite-conduit` を使うと、ファイルの内容全てをもっと効率良くハッシュできます。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import           Crypto.Hash         (Digest, SHA256, hash)
import           Crypto.Hash.Conduit (hashFile)
import           Data.Foldable       (forM_)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           System.Environment  (getArgs)

readFile' :: FilePath -> IO (Map (Digest SHA256) [FilePath])
readFile' fp = do
  digest <- hashFile fp
  return $ Map.singleton digest [fp]

main :: IO ()
main = do
  args <- getArgs
  m <- Map.unionsWith (++) <$> mapM readFile' args
  forM_ (Map.toList m) $ \(digest, files) ->
    case files of
      [] -> error "can never happen"
      [_] -> return () -- only one file
      _ -> putStrLn $ show digest ++ ": " ++ unwords (map show files)
```

かなりシンプルな変更です (というか、こっちの方が少し読みやすいのではないでしょうか)。
さらに、かなりメモリ効率の良いコードになりました(ファイル数に対して線形時間、ファイルサイズに対しては定数時間です)。

## ストリーム・ハッシュ
`conduit` と聞いて、耳か目が即座に反応したかもしれません。
質問された体で答えましょう。
はい、ハッシュに関してもストリーミング処理ができます。ここに、URL とファイルパスを受け取って、その URL の response body の中身をファイルパスに書きこみ、SHA256 でダイジェストを表示するプログラムがあります。
そして、それぞれのデータチャンクを 1回しか参照しないのがいいですね。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import Conduit
import Crypto.Hash         (Digest, SHA256, hash)
import Crypto.Hash.Conduit (sinkHash)
import Network.HTTP.Simple
import System.Environment  (getArgs)

main :: IO ()
main = do
  args <- getArgs
  (url, fp) <-
    case args of
      [x, y] -> return (x, y)
      _ -> error $ "Expected: URL FILEPATH"
  req <- parseRequest url
  digest <- runResourceT $ httpSink req $ \_res -> getZipSink $
    ZipSink (sinkFile fp) *>
    ZipSink sinkHash
  print (digest :: Digest SHA256)
```

`conduit` にできるなら、もちろんあなたにもできるはずです。
`conduit` を使わずに、`hashFile` を実装してみましょう。
こうすることで、ハッシュの API の内部がいくらか分かります。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import Crypto.Hash
import System.Environment  (getArgs)
import System.IO (withBinaryFile, IOMode (ReadMode))
import Data.Foldable (forM_)
import qualified Data.ByteString as B

hashFile :: HashAlgorithm ha => FilePath -> IO (Digest ha)
hashFile fp = withBinaryFile fp ReadMode $ \h ->
  let loop context = do
        chunk <- B.hGetSome h 4096
        if B.null chunk
          then return $ hashFinalize context
          else loop $! hashUpdate context chunk
   in loop hashInit

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \fp -> do
    digest <- hashFile fp
    putStrLn $ show (digest :: Digest SHA256) ++ "  " ++ fp
```

この実装では `Crypto.Hash` で提供されている純粋なハッシュ更新用関数を使っています。
今回の場合、いくつかバッファのコピーをスキップすることで、もう少し効率の良い実装を可能にする、可変ハッシュの関数を使うことができます。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import Crypto.Hash
import Crypto.Hash.IO
import System.Environment  (getArgs)
import System.IO (withBinaryFile, IOMode (ReadMode))
import Data.Foldable (forM_)
import qualified Data.ByteString as B

hashFile :: HashAlgorithm ha => FilePath -> IO (Digest ha)
hashFile fp = withBinaryFile fp ReadMode $ \h -> do
  context <- hashMutableInit
  let loop = do
        chunk <- B.hGetSome h 4096
        if B.null chunk
          then hashMutableFinalize context
          else do
            hashMutableUpdate context chunk
            loop
  loop

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \fp -> do
    digest <- hashFile fp
    putStrLn $ show (digest :: Digest SHA256) ++ "  " ++ fp
```

**練習問題** 遅延入出力と `hashlazy`関数を使って、`hashFile` を実装してください (遅延入出力を支持しているわけじゃないですよ。)。