---
title: wizard モノイド
author: Gabriel Gonzalez
translator: pythonissam
tags: Haskell for all, 翻訳
---

## wizard モノイド (翻訳)

Original post: [The wizard monoid](http://www.haskellforall.com/2018/02/the-wizard-monoid.html)

Recent versions of GHC 8.0 provides a Monoid instance for IO and this post gives a motivating example for why this instance is useful by building combinable "wizard"s.

最近の GHC 8.0 は、`IO` 用の `Monoid` インスタンスを提供しています。このブログ記事では、組み合わせ可能な "wizard" を作りつつ、なぜこのインスタンスが便利なのかを示す例をお見せします。

<!--more-->

### Wizard
I'll define a "wizard" as a program that prompts a user "up front" for multiple inputs and then performs several actions after all input has been collected.

ここで使う "wizard" とは、ユーザーに複数の入力を促し、全ての入力が完了したら、いくつかのアクションを実行するようなプログラムです。

Here is an example of a simple wizard:

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

... which produces the following interaction:

... 実行例:

```plain
What is your name?
Gabriel<Enter>
What is your age?
31<Enter>
Your name is: Gabriel
Your age is: 31
```

... and here is an example of a slightly more complex wizard:

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

In each example, we want to avoid performing any irreversible action before the user has completed entering all requested input.

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

This example is really combining two separate wizards:

* The first wizard requests and displays the user's name
* The second wizard requests and displays the user's age

この例は、実質的には2つの独立した wizard を組み合わせています:

* 最初の wizard はユーザーの名前を要求し、表示している
* 2つ目の wizard はユーザーの年齢を要求し、表示している

However, we had to interleave the logic for these two wizards because we needed to request all inputs before performing any action.

しかし、アクションを実行する前に全ての入力が必要だったので、2つの wizard のロジックをそれぞれ混ぜる必要がありました。

What if there were a way to define these two wizards separately and then combine them into a larger wizard? We can do so by taking advantage of the Monoid instance for IO, like this:

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

The way this works is that we split each wizard into two parts:

この方法でうまくいくのは、それぞれの wizard を2つの部分に分けたからです:

* the "request" (i.e. prompting the user for input)
* the "response" (i.e. performing an action based on that input)

* リクエストの部分 (ユーザーに入力を求める部分など)
* レスポンスの部分 (その入力に応じたアクションを実行する部分など)

... and we do so at the type-level by giving each wizard the type IO (IO ()):

... そしてそれぞれの wizard に `IO (IO ())` という型を与えることによって、型レベルでこれを実現しています:

```haskell
name :: IO (IO ())

age :: IO (IO ())
```

The outer IO action is the "request". When the request is done the outer IO action returns an inner IO action which is the "response". For example:

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

We combine wizards using the (<>) operator, which has the following behavior when specialized to IO actions:

wizard は `(<>)` 演算子を使って組み合わせることができます。`IO` アクションに限って言うなら、以下のような動作をします:

```haskell
ioLeft <> ioRight

= do resultLeft  <- ioLeft
     resultRight <- ioRight
     return (resultLeft <> resultRight)
```

In other words, if you combine two IO actions you just run each IO action and then combine their results. This in turn implies that if we nest two IO actions then we repeat this process twice:

言い換えるなら、`IO` アクションを2つ組み合わせるということは、それぞれの `IO` アクションを実行して結果を組み合わせるということなのです。これは、2つの `IO` アクションをネストさせると同じ処理を2回実行する、ということも示しています:

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

In other words, when we combine two wizards we combine their requests and then combine their responses.

つまり、2つの wizard を組み合わせると、リクエストを組み合わせてレスポンスも組み合わせたことになるのです。

This works for more than two wizards. For example:

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

To show this in action, let's revisit our original example once again:

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

... and this time note that name and age are awfully similar, so we can factor them out into a shared function:

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

We were not able to factor out this shared logic back when the logic for the two wizards were manually interleaved. Once we split them into separate logical wizards then we can begin to exploit shared structure to compress our program.

2つの wizard のロジックが混ざっていたとき、この共通化のロジックを使うことはできませんでした。しかしロジック毎に別々の wizard に分割すると、プログラムを小さくするための共通構造を突くことができます。

This program compression lets us easily add new wizards:

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

... and take advantage of standard library functions that work on Monoids, like foldMap so that we can mass-produce wizards:

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

More importantly, we can now easily see at a glance what our program does and ease of reading is a greater virtue than ease of writing.

より重要なのは、プログラムが何をしているのか一目瞭然になりました。読みやすさは書きやすさに比べ、大きな美徳です。

### 最後の例
Now let's revisit the file removal example through the same lens:

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

We can simplify this using the same pattern:

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

All we have to do is define a wizard for processing a single file, mass-produce the wizard using  foldMap and the Monoid instance for IO takes care of bundling all the requests up front and threading the selected files to be removed afterwards.

やるべきなのは、1つのファイルに対して処理を行う wizard を定義すること、そして `foldMap` を使って wizard を大量に生成することだけです。`IO` の `Monoid` インスタンスは、全てのリクエストを束ねて表示し、後で選択したファイルを削除してくれます。

### 結論
This pattern does not subsume all possible wizards that users might want to write. For example, if the wizards depend on one another then this pattern breaks down pretty quickly. However, hopefully this provides an example of you can chain the Monoid instance for IO with other Monoid instance (even itself!) to generate emergent behavior.

ユーザーが望む wizard の全てにこのパターンが適用できるわけではありません。例えば、wizard が互いに依存しているような状況では、このパターンはすぐに使い物にならなくなります。しかし、このパターンは `Monoid` の `IO` インスタンスを他の `Monoid` のインスタンスと (もしくは自分自身と!) 連結させ、新しい動作を生成するような一例にはなっています。
