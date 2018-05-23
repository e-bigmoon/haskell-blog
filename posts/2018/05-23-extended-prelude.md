---
title: Prelude を カスタムPrelude で置き換える
author: Shinya Yamaguchi
tags: bigmoon, extensible
---

## はじめに

つい最近 haskell-jp で **皆さんPreludeは何を使っていますか？** という話がありました。

まとめるとだいたこんな感じです。

カスタム Prelude パッケージ | 利用しているプロジェクト
---------|---------
[base-prelude](https://github.com/nikita-volkov/base-prelude) | ???
[classy-prelude](https://github.com/snoyberg/mono-traversable/tree/master/classy-prelude) | [Yesod](https://github.com/yesodweb/yesod)
[protolude](https://github.com/sdiehl/protolude) | [purescript](https://github.com/purescript/purescript)
[universum](https://github.com/serokell/universum) | [cardano-sl](https://github.com/input-output-hk/cardano-sl)
[rio](https://github.com/commercialhaskell/rio) | [stack](https://github.com/commercialhaskell/stack)

カスタム Prelude を使うモチベーションは、自分のよく使う関数を Prelude に入れたいとか、**fromJust** みたいな部分関数を排除したいなど、色々あります。

実際にカスタム Prelude を使うためにはファイルの先頭に **NoImplicitPrelude** 言語拡張とカスタムPreludeの **import** 宣言を追加する必要があります。(具体例として **rio** パッケージを利用します)

```hs
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO

...
```

新しいファイルを作るたびに、ファイルの先頭に上記の宣言を書いても良いのですが、今回はこの作業なしにカスタム Prelude を使う方法をご紹介したいと思います。

<!--more-->

## default-extensions を利用する

`{-# LANGUAGE NoImplicitPrelude #-}` を自動的に有効化させることは意外と簡単です。

**package.yaml** や **cabal** ファイルの **default-extensions** に追加するだけです。

```yaml
# package.yaml
default-extensions:
- NoImplicitPrelude
```

```
# project.cabal
default-extensions: NoImplicitPrelude
```

この場合、全てのファイルで自動的に **NoImplicitPrelude** が有効になるため、カスタム Prelude の import 宣言のみが必要となります。

```hs
module Main where

import RIO

...
```

まだ `import RIO` が残っているので、こいつをなんとかしましょう。

## base-noprelude パッケージ

[Add -prelude-is flag](https://ghc.haskell.org/trac/ghc/ticket/9499?cversion=0&cnum_hist=8) というチケットで紹介されている方法を使えば、`import RIO` を記述することなく、**Prelude** のように利用できるようになります。

このチケットによれば **-prelude-is** というオプションを導入しようとしていたようですが、そんなことしなくても **Prelude** を置き換えれるよ！という話です。

具体的には [base-noprelude](https://github.com/haskell-hvr/base-noprelude) パッケージを利用します。

### 最小構成

この方法もかなり簡単で、**stack.yaml**, **package.yaml**, **src/Prelude.hs** をちょこっと書くだけで完成です。

```yaml
# stack.yaml
resolver: nightly-2018-05-23
packages:
- .
extra-deps:
- base-noprelude-4.11.1.0
```

```yaml
# package.yaml
name: extended-prelude

dependencies:
- base-noprelude
- rio

library:
  source-dirs: src
  other-modules:
  - Prelude
```

```hs
-- src/Prelude.hs
module Prelude (module RIO) where

import RIO
```

### 確認

ファイルに `import RIO` を書かなくても本当に良いのか確かめてみます。

```hs
-- src/Sample.hs
module Sample where

f :: Text
f = tshow "a"
```

当然、通常の **Prelude** には **Text** も **tshow** も無いので、`import RIO` が有効になっていなければ、ビルドエラーになるはずです。

**ghci** で確認してみましょう。

```hs
$ stack repl
ghci> f
"\"a\""

ghci> :t fromJust

<interactive>:1:1: error: Variable not in scope: fromJust

ghci> :t view
view :: MonadReader s m => Getting a s a -> m a
```

ちゃんと動いてそうですね。

## まとめ

実際にこの手法で開発を進めているわけではないので、もしかすると落とし穴があるかもしれませんが、思ったより使いやすくてびっくりしました。

元の Prelude に戻すことも簡単だと思うので、興味があれば試してみて下さい。(結構前のチケットなので、周知の事実だったらすみません・・・。)