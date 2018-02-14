---
title: HLint のヒント
date: 2018/02/12
---

## stack プロジェクトで HLint になれよう！

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

## ヒントの種類

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
```

```yaml
# 2つ目のヒント
- ヒントレベル: 警告
- ヒント: Use fromMaybe
- 出力の意味: maybe "" id は Data.Maybe モジュールにある fromMaybe 関数を使えば fromMaybe "" と同じですよ
```

素晴らしいですね。とてもわかりやすいです。

ここで紹介したヒント以外にもデフォルトでいくつも有用なヒントが用意されているため、実際に色々使ってみると楽しいです。

## HTML で結果を出力

`--report` オプションを利用することで結果を `HTML` として出力することも可能です。

```shell
$ hlint . --report
...
```

とても素晴らしいのですが、`HLint` の提案するヒントに賛成できない時はどうしましょう? もし `HLint` の言うとおりにしかできないのであれば、とても使いづらいツールになってしまいます。

そういった場合のためにルールを無視する方法も用意されています。また、プロジェクト固有のカスタムヒントについても同様に設定方法が用意されています。