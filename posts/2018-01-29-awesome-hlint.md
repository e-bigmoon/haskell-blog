---
title: 素晴らしき HLint を使いこなす
author: Shinya Yamaguchi
tags: bigmoon, hlint, 静的解析ツール, package
---

## はじめに

[HLint](https://github.com/ndmitchell/hlint) は [haskell-src-exts](https://www.stackage.org/package/haskell-src-exts) を使って実装されている静的解析ツールです。

`HLint` を使えば `github` などを使って `PR` ベースで開発する場合のコードレビューでこんな事を言わなくて済みます。

- `fromJust` とかの部分関数は使わないで！
- `maybe` 関数って知ってる？
- この言語拡張って本当に使ってるの？
- `undefined` まだ残ってるじゃん！

嬉しいことに `Travis CI` や `CircleCI` などで一度設定するだけなので導入もお手軽です！
また、最近知ったのですが、プロジェクト内で使って欲しくない関数なども `HLint` によって検出可能です。

さらに、独学で `Haskell` の学習を進めている人は `HLint` が素晴らしい教師役となってくれるでしょう。

- [利用したコード](https://github.com/waddlaw/blog-example-hlint)

<!--more-->

## HLint の参考記事

`HLint` は割と有名なので日本語の解説記事がいくつかありました。

- [Haskellの静的解析ツール HLint を使おう](https://qiita.com/suzuki-hoge/items/6d101e523620178c6f7b)
- [Haskellを書くときはstylish-haskellとhlintを使って労せずして綺麗なコードを書きましょう](https://www.ncaq.net/2017/10/07/)
- [OverloadedStringsとANNプラグマが干渉する場合の回避方法](https://qiita.com/VoQn/items/fe7953aec010d8f68a59)

ちゃんと使おうとすると上記の解説記事では少し物足りません。具体的には以下の点が不足しています。

- カスタムヒントの設定方法
- 関数の利用制限方法
- 関数・モジュール・ファイル単位でヒントを無視する方法
- `CI` で利用するための設定方法

本記事では、これらの内容について解説を行います。`HLint` でどんなことが出来るかについては、上記の記事または[公式リポジトリ](https://github.com/ndmitchell/hlint)をご参考ください。

また、内部の仕組みについては、作者の `Neil Mitchell` さんの解説記事が参考になります。

- [HLint のルールを理解する (和訳)](https://qiita.com/rounddelta/items/4584f5486c1061c93f0b)

## HLint の導入

`HLint` は以下のコマンドで簡単に導入できます。

```shell
$ stack install hlint

$ hlint --version
HLint v2.0.15, (C) Neil Mitchell 2006-2018
```

現在の最新版は `v2.0.15` となっています。`HLint` のバージョンによって出力内容が異なることが良くありますのでご注意ください。

また、お試しで使ってみたい人は以下のコマンドを実行してみましょう。カレントディレクトリ以下のファイルが検査されます。

```shell
$ curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
```

## HLint の実行方法

`HLint` はディレクトリを指定すると再帰的に解析を行ってくれます。

### プロジェクト全体に対して再帰的に実行

```shell
$ hlint .
```

### 特定のディレクトリ (src) に対して再帰的に実行

```shell
$ hlint src
```

### 複数のディレクトリ (src, test) に対して再帰的に実行

```shell
$ hlint src test
```

### 単一のファイル (app/Main.hs) にのみ実行

```shell
$ hlint app/Main.hs
```

## HLint のヒント

以下のように `stack new` で新規プロジェクトを作ってすぐの状態では `HLint` は何もヒントを出してくれません。つまり、とても良い状態ということです。

```shell
$ stacke new test-proj
$ cd test-proj

$ hlint .
No hints
```

ここで、ファイルを少し修正して `HLint` に働いてもらいましょう！

```haskell
-- src/Lib.hs
module Lib where

someFunc :: IO ()
someFunc = do
  let x1 = concat (map toUpper ['a' .. 'z'])
      x2 = maybe "" id "abc"
  putStrLn "someFunc"
```

上記のコードは以下のようなヒントを2つ提案してくれます。

```shell
$ hlint .
./src/Lib.hs:7:12: Warning: Use concatMap
Found:
  concat (map toUpper ['a' .. 'z'])
Why not:
  concatMap toUpper ['a' .. 'z']

./src/Lib.hs:8:12: Warning: Use fromMaybe
Found:
  maybe "" id
Why not:
  Data.Maybe.fromMaybe ""

2 hints
```

これは、こんな感じの意味になります。

ヒントレベル | ヒント | 出力の意味
:-----------:|--------|----------
警告 | Use concatMap | `concat (map toUpper ['a' .. 'z'])` を見つけたけど、どうして `concatMap toUpper ['a' .. 'z']` と書かないんだい？
警告 | Use fromMaybe | `maybe "" id` は `Data.Maybe` モジュールにある `fromMaybe` 関数を使えば `fromMaybe ""` と同じですよ

素晴らしいですね。とてもわかりやすいです。また、`--report` オプションを利用することで結果を `HTML` として出力することも可能です。

```shell
$ hlint . --report
```

とても素晴らしいのですが、`HLint` の提案するヒントに賛成できない時はどうしましょう? もし `HLint` の言うとおりにしかできないのであれば、とても使いづらいツールになってしまいます。

そういった場合のためにルールを無視する方法も用意されています。また、プロジェクト固有のカスタムヒントについても同様に設定方法が用意されています。

## HLint のヒントについて

デフォルトで適用されるヒントの一覧は [hlint.yaml](https://github.com/ndmitchell/hlint/blob/master/data/hlint.yaml) で確認できます。この中に無いヒントについては、自分でカスタムヒントを追加して対応することになります。

### カスタムヒントファイルの生成

まずは、カスタムヒントファイルの雛形を生成するために、プロジェクトのルートで以下のコマンドを実行しましょう。

```shell
$ hlint --default > .hlint.yaml
```

中身はこんな感じで、ヒントの書き方について具体例が載っています。

```yaml
# HLint configuration file
# https://github.com/ndmitchell/hlint
##########################

# This file contains a template configuration file, which is typically
# placed as .hlint.yaml in the root of your project


# Specify additional command line arguments
#
# - arguments: [--color, --cpp-simple, -XQuasiQuotes]


# Control which extensions/flags/modules/functions can be used
#
# - extensions:
#   - default: false # all extension are banned by default
#   - name: [PatternGuards, ViewPatterns] # only these listed extensions can be used
#   - {name: CPP, within: CrossPlatform} # CPP can only be used in a given module
#
# - flags:
#   - {name: -w, within: []} # -w is allowed nowhere
#
# - modules:
#   - {name: [Data.Set, Data.HashSet], as: Set} # if you import Data.Set qualified, it must be as 'Set'
#   - {name: Control.Arrow, within: []} # Certain modules are banned entirely
#
# - functions:
#   - {name: unsafePerformIO, within: []} # unsafePerformIO can only appear in no modules


# Add custom hints for this project
#
# Will suggest replacing "wibbleMany [myvar]" with "wibbleOne myvar"
# - error: {lhs: "wibbleMany [x]", rhs: wibbleOne x}


# Turn on hints that are off by default
#
# Ban "module X(module X) where", to require a real export list
# - warn: {name: Use explicit module export list}
#
# Replace a $ b $ c with a . b $ c
# - group: {name: dollar, enabled: true}
#
# Generalise map to fmap, ++ to <>
# - group: {name: generalise, enabled: true}


# Ignore some builtin hints
# - ignore: {name: Use let}
# - ignore: {name: Use const, within: SpecialModule} # Only within certain modules


# Define some custom infix operators
# - fixity: infixr 3 ~^#^~


# To generate a suitable file for HLint do:
# $ hlint --default > .hlint.yaml
```

`HLint` はデフォルトヒントが記述されている `hlint.yaml` と、カスタムヒントが記述されている `.hlint.yaml` の両方のヒント使って検査を行うため、プロジェクト固有のヒントについては、`.hlint.yaml` に記述していくことになります。

### カスタムヒントの追加

ここでは説明のため以下のような `tshow` という関数があるとしましょう。この関数は `show :: Show a => a -> String` の `Text` バージョンです。

```haskell
import Data.Text (pack)

tshow :: Show a => a -> Text
tshow = pack . show
```

目的としてはプロジェクトのコード中で `pack . show` となっている部分を `tshow` に直すようにヒントを出させることです。当然ながら現時点では `pack . show` というコードが使われていたとしても何も起こりません。

```haskell
-- src/Lib.hs
module Lib where

import Data.Text (pack)

intToText :: Int -> Text
intToText = pack . show
```

```shell
$ hlint .
No hints
```

それでは `.hlint.yaml` に `tshow = pack . show` を検出するためのヒントを追記しましょう。以下の1行を追記するだけです。

```yaml
- error: {lhs: pack (show x), rhs: tshow x}
```

`lhs`, `rhs` はそれぞれ `Left Hand Side (左辺)`, `Right Hand Side (右辺)` の略です。またヒントのレベルは `error` 以外にも `warm`, `suggest (hint キーワードはただのエイリアスです)` も指定できるため、好きなレベルを指定しましょう。(ヒントレベルの使い分けについては [What is the difference between error/warning/suggestion?](https://github.com/ndmitchell/hlint#what-is-the-difference-between-errorwarningsuggestion) をご参照ください)

では、実行してみましょう。

```shell
$ hlint .
./src/Lib.hs:14:13: Error: Use tshow
Found:
  pack . show
Why not:
  tshow

1 hint
```

無事に `Error` として `tshow` のためのヒントが表示されました！

ヒントの修正方法は、先程定義した `intToText` 関数の実装をヒント通りに書き換えるだけです。

```haskell
intToText :: Int -> Text
intToText = tshow
```

```shell
$ hlint .
No hints
```

### ヒントの定義方法について

さきほど定義したヒントはこのようにポイントフリーで書くこともできます。

```yaml
- error: {lhs: pack . show, rhs: tshow}
```

上記のヒント形式で次の内容を解析してみましょう。

```haskell
-- src/Lib.hs
module Lib where

import Data.Text (pack)

intToText :: Int -> Text
intToText = pack . show

intToText2  :: Int -> Text
intToText2 x = pack $ show x
```

```shell
$ hlint .
./src/Lib.hs:6:13: Error: Use tshow
Found:
  pack . show
Why not:
  tshow

1 hint
```

`intToText` と `intToText2` どちらも検出して欲しいですが `intToText` しか検出できていません。`HLint` では自動的に η-簡約 (eta-reduction) が行われるため `error: {lhs: pack (show x), rhs: tshow x}` というように定義しておいた方が良いです。

## プロジェクトで利用を禁止している関数を検出する

プロジェクト内で部分関数 (例: `fromJust`) を使わせないようにさせたり、`undefined` が残っていないかなどのチェックをレビュー時に人間が行っていたりしませんか？

人間が介入するということは必ずミスが起こります。人間が気をつければミスは起こらないと思っていたり、精神力でなんとかしようとしている場合は能力不足を疑われても仕方がありません。

また、そのようなつまらない間違い探しのような非クリエイティブな作業に大切な時間を割いてしまうのはとても良くないことです。

`HLint` を使えば、そのような関数を検出することが可能です。実際には `関数` だけでなく `言語拡張`, `フラグ`, `モジュール` も指定することができます。

### 関数を指定する方法

`.hlint.yaml` に以下の内容を追記します。今回は `undefined` を検出してみたいと思います。

```yaml
# .hlint.yaml
- functions:
  - {name: undefined, within: []}
```

現状はどこにも使われていないためヒントは表示されません。

```shell
$ hlint .
No hints
```

では、以下の関数を追加してみましょう。このように型レベルで設計して、実装を `undefined` にしておくことは良くあります。

```haskell
-- src/Lib.hs
doubleToText :: Double -> Text
doubleToText = undefined
```

忘れずに実装してしまえば問題無いのですが、たまには忘れることもあります。しかし、`HLint` があれば安心です。

```shell
$ hlint .
./src/Lib.hs:18:16: Warning: Avoid restricted function
Found:
  undefined
Note: may break the code

1 hint
```

## HLint のヒントを無視する方法

`HLint` のヒントを無視する方法には以下の2種類があります。

- `.hlint.yaml` ファイルで指定する (**全てのファイル**に影響)
- ファイルに直接 `{-# ANN -#}` アノテーションを記述する (**アノテーションの範囲**にのみ影響)

書式がちょっとわかりづらいので、実際に色々試してみましょう。

### 関数単位で全てのヒントを無視する

最初に定義した `someFunc` 関数はヒントを2つ提案してくれていました。

```haskell
-- src/Lib.hs
module Lib where

someFunc :: IO ()
someFunc = do
  let x1 = concat (map toUpper ['a' .. 'z'])
      x2 = maybe "" id "abc"
  putStrLn "someFunc"
```

```shell
$ hlint .
./src/Lib.hs:6:12: Warning: Use concatMap
Found:
  concat (map toUpper ['a' .. 'z'])
Why not:
  concatMap toUpper ['a' .. 'z']

./src/Lib.hs:7:12: Warning: Use fromMaybe
Found:
  maybe "" id
Why not:
  Data.Maybe.fromMaybe ""

2 hints
```

提案されているヒントは以下の2つです。

- Warning: Use `concatMap`
- Warning: Use `fromMaybe`

とりあえず `someFunc` のヒントを全て無視するようにしてしまいましょう。

こんな感じで `{-# ANN someFunc "HLint: ignore" #-}` というアノテーションをつけます。

```haskell
-- src/Lib.hs
module Lib where

{-# ANN someFunc "HLint: ignore" #-}
someFunc :: IO ()
someFunc = do
  let x1 = concat (map toUpper ['a' .. 'z'])
      x2 = maybe "" id "abc"
  putStrLn "someFunc"
```

```shell
$ hlint .
No hints
```

これで `src/Lib.hs` に記述されている `someFunc` 関数のみ `HLint` のヒントを無視できるようになりました。

### ヒントを無視する様々な方法

ヒントレベルは `ignore` 以外にも `suggest`, `warn`, `error` が利用可能です。これらの値を利用した場合は出力時のヒントレベルが強制的にそのレベルに上書きされます。つまり、ヒントファイルに `warn` で定義されていたとしても `error` や `ignore` として処理されることになります。

#### 関数単位で全てのヒントを無視する方法

`ANN` のあとに対象の関数名を書きます。

```haskell
{-# ANN someFunc "HLint: ignore" #-}
```

#### 関数単位で特定のヒントのみを無視する方法

`HLint: ignore` の後にヒント名を書きます。

```haskell
{-# ANN someFunc "HLint: ignore Use fromMaybe" #-}
{-# ANN someFunc "HLint: ignore Use concatMap" #-}
```

#### モジュール単位で無視する方法

`module` キーワードを使う場合はアノテーションを `import` 文の後に設置しないと上手く動かないので、その点のみ注意が必要です。

```haskell
{-# ANN module "HLint: ignore" #-}

{-# ANN module "HLint: ignore Use fromMaybe" #-}
{-# ANN module "HLint: ignore Use concatMap" #-}
```

### OverloadedStrings 言語拡張

言語拡張の `OverloadedStrings` を有効化している場合は上手く動かないため、明示的に `String` の型注釈を指定する必要があります。

```haskell
{-# ANN someFunc ("HLint: ignore" :: String) #-}
```

### ヒントファイルを使って無視する方法

プロジェクト全体で無視したいヒントについては `.hint.yaml` に追記します。

```yaml
# .hint.yaml
- ignore: {name: Use fromMaybe}
- ignore: {name: Use concatMap}
```

`within` キーワードでヒントを適用するモジュールを指定できます。

例として `Lib` モジュールのみを対象とする場合は次のようになります。(この場合は `ignore` が指定されているので `Lib` モジュールのみヒントを無視します)

```yaml
# .hlint.yaml
- ignore: {name: Use fromMaybe, within: [Lib]}
- ignore: {name: Use concatMap, within: [Lib]}
```

## CI を回す！

`Haskell` のプロジェクトでよく見る `CI` ツールといえば以下の2つでしょう。

- [Travis CI](https://travis-ci.org/)
- [CircleCI 2.0](https://circleci.com/)

個人的には以下の点で `CircleCI` が好きです。

- プライベートリポジトリも無料で使える
- `Docker`, `docker-compose` と親和性が高い

ここでは `HLint` の内容にしか言及しませんが、機会があれば `CI` については別途記事にしたいと思います。

### Travis CI

`.travis.yml` に以下の内容を記述するだけです。

```yaml
# .travis.yml
sudo: false
language: generic
jobs:
  include:
    - stage: Run hlint
      script: curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
```

### CircleCI 2.0

`.circleci/config.yml` に以下の内容を記述するだけです。

```yaml
version: 2
jobs:
  hlint:
    docker:
      - image: ubuntu:16.04
    steps:
      - checkout
      - run:
          name: Run hlint
          command: |
            apt update
            apt install -y curl
            curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
workflows:
  version: 2
  hlint:
    jobs:
      - hlint
```

## まとめ

今回は紹介していませんが `HLint` のヒントを自動的に適用してくれる [apply-refact](https://github.com/mpickering/apply-refact) というツールもあります。使い方については各種ドキュメントをご確認ください。

- [Automatically Applying Hints](https://github.com/ndmitchell/hlint#automatically-applying-hints)

今回は `Haskell` の静的解析ツール `HLint` について説明を行いました。需要があれば `LiquidHaskell` などの他の静的解析ツールについても、チュートリアル的な解説記事を書いていきたいところです。

