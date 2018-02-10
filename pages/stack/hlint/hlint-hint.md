---
title: HLintのヒント
date: 2018/02/10
---


以下のように `stack new` で新規プロジェクトを作ってすぐの状態では `HLint` は何もヒントを出してくれません。

つまり、とても良い状態ということです。

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

これは、このような意味になります。

```yaml
# 1つ目のヒント
- ヒントレベル: 警告
- ヒント: Use concatMap
- 出力の意味: concat (map toUpper ['a' .. 'z']) を見つけたけど、どうして concatMap toUpper ['a' .. 'z'] と書かないんだい？

# 2つ目のヒント
- ヒントレベル: 警告
- ヒント: Use fromMaybe
- 出力の意味: maybe "" id は Data.Maybe モジュールにある fromMaybe 関数を使えば fromMaybe "" と同じですよ
```

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

目的としてはプロジェクトのコード中で `pack . show` となっている部分を `tshow` に直すようにヒントを出させることです。まだヒントを追加していないため、当然ながら現時点では `pack . show` というコードが使われていたとしても何も起こりません。

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

`Lib` モジュールのみを検査対象とする場合は `within` キーワードを次のようにします。

```yaml
# .hlint.yaml
- functions:
  - {name: undefined, within: [Lib]}
```
