---
title: script interpreter + stack script でスクリプティング！
published: 2018/03/19
updated: 2020/03/25
---

## stack script コマンドとは？

**stack** を使って自分で作成したプログラムを実行する方法はいくつかあります。

- `stack repl` (または `stack ghci`) コマンド
- `stack exec` コマンド
- `stack run` コマンド
- `stack script` コマンド

`stack script` コマンドは、利用する **resolver** を明示的に必ず指定するため、通常は利用するパッケージを指定することなく実行できます。

もちろん明示的にパッケージを指定することも可能です。

### 使い方

具体的には以下のように使います。ファイル名は自由ですが、`--resolver lts-15.5` のようにスナップショットの指定は必須です。

```shell
$ stack script --resolver lts-15.5 Main.hs
```

また、アプリケーションが引数を利用する場合はいつも通り `--` で区切ります。

```shell
$ stack script --resolver lts-15.5 -- Main.hs [1,2,3]
```

注意点として `stack script` で処理するファイルには `main` 関数が含まれている必要があります。

## script interpreter 形式とは？

一言で言えばシェルスクリプトの **Haskell** バージョンです。

`script interpreter` 形式のメリットは容易に実行可能な形式で配布できる点にあります。

例えば **ghci** に大量のオプションを渡す必要があるなど、毎回入力するのが面倒な場合に便利です。

### 使い方

通常の **Haskell** ファイルの先頭に次の1行を追加するだけです。

```hs
#!/usr/bin/env stack
```

実際には、そのすぐ次の行に `stack repl` や `stack script` などを指定して使うことになります。

## 組み合わせてみよう！

`script interpreter` と `stack script` を組み合わせると非常にポータブルな **Haskell** スクリプトを作ることができます。

個人的には以下のようなメリットを感じています。

- **スナップショット**が明示的に指定されるため、いつでも動くサンプルが簡単に作れる
- 実行時に必要なオプション等を全てファイルの中に含めることができるため、実行方法が可視化される
- ブログのサンプルコードなどに適した形式

### 使い方

```hs
#!/usr/bin/env stack
-- stack script --resolver lts-15.5
```

**lts-15.5** の部分は自分の好きな[スナップショット](https://www.stackage.org/)を指定しましょう。

実際に実行する場合はシェルスクリプト同様に実行権限を付与し、ファイルを実行します。

```shell
$ chmod u+x Main.hs
./Main.hs
```

### 明示的にパッケージを指定する場合

基本的にはパッケージを指定することなく動作しますが、ごく稀に明示的にパッケージを指定する必要があります。

その場合は、このように `--package` 引数を指定する事で実行できるようになります。

```hs
#!/usr/bin/env stack
-- stack script --resolver lts-15.5 --package yesod --package yesod-core
```

パッケージが多い場合は複数行コメントにすると見やすくなります。

```hs
#!/usr/bin/env stack
{- stack script
   --resolver lts-15.5
   --package yesod
   --package yesod-core
-}
```

## 具体例

以下のコードは **Yesod** のサンプルアプリケーションを `stack interpreter` + `stack script` にしたものです。

```hs
#!/usr/bin/env stack
-- stack script --resolver lts-15.5

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 HelloWorld
```

上記のファイルを `YesodEx.hs` として保存して実行してみましょう。

```shell
$ chmod u+x YesodEx.hs
$ ./YesodEx.hs
```

[http://localhost:3000](http://localhost:3000) にアクセスすると **Hello World** の文字が表示されたのではないでしょうか。

このようにとても簡単に実行できるため、シェルスクリプトの代わりに **Haskell** でスクリプトを書くことも普通にできるようになります。