---
title: Script interpreter 形式でアプリケーションを実行
date: 2018/03/19
---

## Script Interpreter 形式とは？

一言で言えばシェルスクリプトの `Haskell` バージョンです。

`stack` を使って自分で作成したプログラムを実行する方法はいくつかあります。

- `stack repl`
- `stack exec`
- `stack interpreter`

`script interpreter` 形式のメリットは容易に実行可能な形式で配布できる点にあります。また、大抵の場合は利用するパッケージを明示的に指定しなくても良いという点もあります。

例えば、ブログ記事で読者にコードを実行してもらいたい場合はこの形式が適しているでしょう。

逆に、アプリケーションの一部だけを見せたい場合などには向いていません。`stack interpreter` 形式は実行可能なファイルである必要があります。 (つまり `main` 関数が必要です)

## 使い方

通常の `Haskell` ファイルの先頭に以下のような行を追加するだけです。

```hs
#!/usr/bin/env stack
-- stack --resolver lts-11.1 script
```

`lts-11.1` の部分は自分の好きな[スナップショット](https://www.stackage.org/)を指定しましょう。

実際に実行する場合はシェルスクリプト同様に実行権限を付与し、ファイルを実行します。

```sh
$ chmod u+x Main.hs
./Main.hs
```

### 明示的にパッケージを指定する場合

基本的にはパッケージを省略して動作しますが、ごく稀に明示的にパッケージを指定する必要があります。

その場合は、このように `--package` 引数を指定する事で実行できるようになります。

```hs
#!/usr/bin/env stack
-- stack --resolver lts-11.1 script --package yesod --package yesod-core
```

パッケージが多い場合は複数行コメントにすると見やすくなります。

```hs
#!/usr/bin/env stack
{- stack --resolver lts-11.1 script
   --package yesod
   --package yesod-core
-}
```

## 具体例

以下のコードは `Yesod` のサンプルアプリケーションを `stack interpreter` 形式にしたものです。

```hs
#!/usr/bin/env stack
-- stack --resolver lts-11.1 script

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

```sh
$ chmod u+x YesodEx.hs
$ ./YesodEx.hs
```

[http://localhost:3000](http://localhost:3000) にアクセスすると `Hello World` の文字が表示されたのではないでしょうか。

このように、とても簡単に実行できるため、シェルスクリプトの代わりに `Haskell` でスクリプトを書くことも普通にできるようになります。