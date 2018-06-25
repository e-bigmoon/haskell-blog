---
title: HLint のカスタムヒント
date: 2018/05/16
---

## カスタムヒント

デフォルトで適用されるヒントの一覧は [hlint.yaml](https://github.com/ndmitchell/hlint/blob/master/data/hlint.yaml) で確認できます。

この中に無いヒントについては、自分でカスタムヒントを追加して対応することになります。

## カスタムヒントファイルの生成

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

**HLint** はデフォルトヒントが記述されている `hlint.yaml` と、カスタムヒントが記述されている `.hlint.yaml` の両方のヒント使って検査を行うため、プロジェクト固有のヒントについては、`.hlint.yaml` に記述していくことになります。

## カスタムヒントの追加

ここでは説明のため以下のような **tshow** という関数があるとしましょう。この関数は `show :: Show a => a -> String` の **Text** バージョンです。

```haskell
import Data.Text (pack)

tshow :: Show a => a -> Text
tshow = pack . show
```

目的としてはプロジェクトのコード中で `pack . show` となっている部分を **tshow** に直すようにヒントを出させることです。

まだヒントを追加していないため、当然ながら現時点では `pack . show` というコードが使われていたとしても何も起こりません。

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

それでは `.hlint.yaml` に `tshow = pack . show` を検出するためのヒントを追記しましょう。

以下の1行を追記するだけです。

```yaml
- error: {lhs: pack (show x), rhs: tshow x}
```

**lhs**, **rhs** はそれぞれ **Left Hand Side (左辺)**, **Right Hand Side (右辺)** の略です。またヒントのレベルは **error** 以外にも **warm**, **suggest** (**hint** キーワードはただのエイリアスです) も指定できるため、好きなレベルを指定しましょう。(ヒントレベルの使い分けについては [What is the difference between error/warning/suggestion?](https://github.com/ndmitchell/hlint#what-is-the-difference-between-errorwarningsuggestion) をご参照ください)

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

無事に **Error** として **tshow** のためのヒントが表示されました！

ヒントの修正方法は、先程定義した **intToText** 関数の実装をヒント通りに書き換えるだけです。

```haskell
intToText :: Int -> Text
intToText = tshow
```

```shell
$ hlint .
No hints
```

## ヒントの定義方法について

さきほど定義したヒントはこのようにポイントフリー形式で書くこともできます。

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

**intToText** と **intToText2** どちらも検出して欲しいですが **intToText** しか検出できていません。

**HLint** では自動的に η-簡約 (eta-reduction) が行われるため `error: {lhs: pack (show x), rhs: tshow x}` のように定義しておいた方が良いでしょう。

## hlint に渡す引数を指定する

具体的には、**QuasiQuotes** を **package.yaml** の **default-extensions** に指定する場合などで役に立ちます。

以下のように **arguments** に **-XQuasiQuotes** を指定します。

```yaml
# .hlint.yaml
- arguments: [ -XQuasiQuotes ]
```

**arguments** に指定できるのは言語拡張だけでなく `--color`, `--cpp-simple` など、`hlint` に渡せる引数であれば何でも指定することができます。
