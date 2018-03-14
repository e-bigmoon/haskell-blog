---
title: wizard モノイド
author: Gabriel Gonzalez
translator: pythonissam
tags: Haskell for all, 翻訳
---

## wizard モノイド (翻訳)

Original post: [The wizard monoid](http://www.haskellforall.com/2018/02/the-wizard-monoid.html)

最近の GHC 8.0 は、`IO` 用の `Monoid` インスタンスを提供しています。このブログ記事では、組み合わせ可能な "wizard" を作りつつ、なぜこのインスタンスが便利なのかを示す例をお見せします。

<!--more-->

### Wizard
ここで使う "wizard" とは、ユーザーに複数の入力を促し、全ての入力が完了したら、いくつかのアクションを実行するようなプログラムです。

簡単な wizard の例です:

```haskell
main :: IO ()
main = do
    -- First, we request all inputs:
    putStrLn "What is your name?"
    name <- getLine

    putStrLn "What is your age?"
    age <- getLine

    -- Then, we perform all actions:
    putStrLn ("Your name is: " ++ name)
    putStrLn ("Your age is: " ++ age)
```

... 実行例:

```plain
What is your name?
Gabriel<Enter>
What is your age?
31<Enter>
Your name is: Gabriel
Your age is: 31
```

... それで、以下はもう少し複雑な wizard の例です:

```haskell
import qualified System.Directory

main :: IO ()
main = do
    -- First, we request all inputs:
    files <- System.Directory.listDirectory "."
    let askFile file = do
            putStrLn ("Would you like to delete " ++ file ++ "?")
            response <- getLine
            case response of
                "y" -> return [file]
                _   -> return []

    listOfListOfFilesToRemove <- mapM askFile files
    let listOfFilesToRemove = concat listOfListOfFilesToRemove

    -- Then, we perform all actions:
    let removeFile file = do
            putStrLn ("Removing " ++ file)
            System.Directory.removeFile file
    mapM_ removeFile listOfFilesToRemove
```

... 実行例:

```plain
Would you like to delete file1.txt?
y<Enter>
Would you like to delete file2.txt?
n<Enter>
Would you like to delete file3.txt?
y<Enter>
Removing file1.txt
Removing file3.txt
```

以上に挙げた例では、ユーザーが要求された入力を全て入力し終えるまで、変更ができないアクションを実行するのは避けたいという要求があります。

### モジュール性
最初の例を見直してみましょう:

```haskell
main :: IO ()
main = do
    -- First, we request all inputs:
    putStrLn "What is your name?"
    name <- getLine

    putStrLn "What is your age?"
    age <- getLine

    -- Then, we perform all actions:
    putStrLn ("Your name is: " ++ name)
    putStrLn ("Your age is: " ++ age)
```

この例は、実質的には2つの独立した wizard を組み合わせています:

* 最初の wizard はユーザーの名前を要求し、表示している
* 2つ目の wizard はユーザーの年齢を要求し、表示している

しかし、アクションを実行する前に全ての入力が必要だったので、2つの wizard のロジックをそれぞれ混ぜる必要がありました。

これら2つの wizard を別々に定義し、より大きな wizard に合体させる方法があったらどうでしょう? `IO` の `Monoid` インスタンスの長所を活かせば可能です。こんな感じ:

```haskell
import Data.Monoid ((<>))

name :: IO (IO ())
name = do
    putStrLn "What is your name?"
    x <- getLine
    return (putStrLn ("Your name is: " ++ x))

age :: IO (IO ())
age = do
    putStrLn "What is your age?"
    x <- getLine
    return (putStrLn ("Your age is: " ++ x))

runWizard :: IO (IO a) -> IO a
runWizard request = do
    respond <- request
    respond

main :: IO ()
main = runWizard (name <> age)
```

このプログラムはさっきの例と完全に同じ動きをします。が、ユーザーの名前を扱うロジックは、ユーザーの年齢を扱うロジックと完全に分離されています。

この方法でうまくいくのは、それぞれの wizard を2つの部分に分けたからです:

* リクエストの部分 (ユーザーに入力を求める部分など)
* レスポンスの部分 (その入力に応じたアクションを実行する部分など)

... そしてそれぞれの wizard に `IO (IO ())` という型を与えることによって、型レベルでこれを実現しています:

```haskell
name :: IO (IO ())

age :: IO (IO ())
```

外側の `IO` アクションは"リクエスト"です。リクエストが終了したとき、外側の `IO` アクションは内側の `IO` アクション、つまり"レスポンス"を返します。例えば:

```haskell
--      ↓ リクエスト
name :: IO (IO ())
--          ↑ レスポンス
name = do
    putStrLn "What is your name?"
    x <- getLine
    -- ↑ ここから上の部分全てが、外側の `IO` アクションの一部 (例えばリクエスト)

    --      ↓ この return される値は、内側の `IO` アクション (例えばレスポンス)
    return (putStrLn ("Your name is: " ++ x))
```

wizard は `(<>)` 演算子を使って組み合わせることができます。`IO` アクションに限って言うなら、以下のような動作をします:

```haskell
ioLeft <> ioRight

= do resultLeft  <- ioLeft
     resultRight <- ioRight
     return (resultLeft <> resultRight)
```

言い換えるなら、`IO` アクションを2つ組み合わせるということは、それぞれの `IO` アクションを実行して結果を組み合わせるということなのです。これは、2つの `IO` アクションをネストさせると、アクションを実行して結果を組み合わせるという処理を2回実行する、ということも示しています:

```haskell
requestLeft <> requestRight

= do respondLeft  <- requestLeft
     respondRight <- requestRight
     return (respondLeft <> respondRight)

= do respondLeft  <- requestLeft
     respondRight <- requestRight
     return (do
         unitLeft  <- respondLeft
         unitRight <- respondRight
         return (unitLeft <> unitRight) )

-- Both `unitLeft` and `unitRight` are `()` and `() <> () = ()`, so we can
-- simplify this further to:

-- `unitLeft` も `unitRight` も `()` で、`() <> () = ()` なので、
-- 以下のように簡約化することができます:
= do respondLeft  <- requestLeft
     respondRight <- requestRight
     return (do
         respondLeft
         respondRight )
```

つまり、2つの wizard を組み合わせると、リクエストを組み合わせてレスポンスも組み合わせたことになるのです。

この方法は2つ以上の wizard でもうまくいきます。例えば:

```haskell
request0 <> request1 <> request2 <> request3

= do respond0 <- request0
     respond1 <- request1
     respond2 <- request2
     respond3 <- request3
     return (do
         respond0
         respond1
         respond2
         respond3 )
```

これをアクションの形で表現するために、さっきの例をもう一度見てみましょう:

```haskell
import Data.Monoid ((<>))

name :: IO (IO ())
name = do
    putStrLn "What is your name?"
    x <- getLine
    return (putStrLn ("Your name is: " ++ x))

age :: IO (IO ())
age = do
    putStrLn "What is your age?"
    x <- getLine
    return (putStrLn ("Your age is: " ++ x))

runWizard :: IO (IO a) -> IO a
runWizard request = do
    respond <- request
    respond

main :: IO ()
main = runWizard (name <> age)
```

...`name` と `age` はかなり似ているので、共通の関数を使うような実装にすることができますね:

```haskell
import Data.Monoid ((<>))

prompt :: String -> IO (IO ())
prompt attribute = do
    putStrLn ("What is your " ++ attribute ++ "?")
    x <- getLine
    return (putStrLn ("Your " ++ attribute ++ " is: " ++ x))

runWizard :: IO (IO a) -> IO a
runWizard request = do
    respond <- request
    respond

main :: IO ()
main = runWizard (prompt "name" <> prompt "age")
```

2つの wizard のロジックが混ざっていたとき、この共通化のロジックを使うことはできませんでした。しかしロジック毎に別々の wizard に分割すると、プログラムを小さくするための共通構造を突くことができます。

このプログラムの圧縮によって、簡単に新しい wizard を追加することができます:

```haskell
import Data.Monoid ((<>))

prompt :: String -> IO (IO ())
prompt attribute = do
    putStrLn ("What is your " ++ attribute ++ "?")
    x <- getLine
    return (putStrLn ("Your " ++ attribute ++ " is: " ++ x))

runWizard :: IO (IO a) -> IO a
runWizard request = do
    respond <- request
    respond

main :: IO ()
main = runWizard (prompt "name" <> prompt "age" <> prompt "favorite color")
```

... そして、モノイド関連の標準ライブラリ関数を活用しましょう。例えば `foldMap` を使えば wizard を大量に作ることができます。

```haskell
import Data.Monoid ((<>))

prompt :: String -> IO (IO ())
prompt attribute = do
    putStrLn ("What is your " ++ attribute ++ "?")
    x <- getLine
    return (putStrLn ("Your " ++ attribute ++ " is: " ++ x))

runWizard :: IO (IO a) -> IO a
runWizard request = do
    respond <- request
    respond

main :: IO ()
main = runWizard (foldMap prompt [ "name", "age", "favorite color", "sign" ])
```

より重要なのは、プログラムが何をしているのか一目瞭然になりました。読みやすさは書きやすさに比べ、大きな美徳です。

### 最後の例
ファイル削除の例も同じ観点から見直してみましょう:

```haskell
import qualified System.Directory

main :: IO ()
main = do
    -- 最初に全ての入力をリクエストする:
    files <- System.Directory.listDirectory "."
    let askFile file = do
            putStrLn ("Would you like to delete " ++ file ++ "?")
            response <- getLine
            case response of
                "y" -> return [file]
                _   -> return []

    listOfListOfFilesToRemove <- mapM askFile files
    let listOfFilesToRemove = concat listOfListOfFilesToRemove

    -- 次に全てのアクションを実行する
    let removeFile file = do
            putStrLn ("Removing " ++ file)
            System.Directory.removeFile file
    mapM_ removeFile listOfFilesToRemove
```

さっきと同じパターンで、シンプルにすることができます:

```haskell
import qualified System.Directory

main :: IO ()
main = do
    files <- System.Directory.listDirectory "."
    runWizard (foldMap prompt files)

prompt :: FilePath -> IO (IO ())
prompt file = do
    putStrLn ("Would you like to delete " ++ file ++ "?")
    response <- getLine
    case response of
        "y" -> return (do
            putStrLn ("Removing " ++ file)
            System.Directory.removeFile file )
        _   -> return (return ())

runWizard :: IO (IO a) -> IO a
runWizard request = do
    respond <- request
    respond
```

やるべきなのは、1つのファイルに対して処理を行う wizard を定義すること、そして `foldMap` を使って wizard を大量に生成することだけです。`IO` の `Monoid` インスタンスは、全てのリクエストを束ねて表示し、後で選択したファイルを削除してくれます。

### 結論
ユーザーが望む wizard の全てにこのパターンが適用できるわけではありません。例えば、wizard が互いに依存しているような状況では、このパターンはすぐに使い物にならなくなります。しかし、このパターンは `Monoid` の `IO` インスタンスを他の `Monoid` のインスタンスと (もしくは自分自身と!) 連結させ、新しい動作を生成するような一例にはなっています。
