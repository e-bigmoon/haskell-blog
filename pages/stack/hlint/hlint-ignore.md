---
title: HLintのヒントを無視する方法
date: 2018/02/10
---


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

- Warning: `Use concatMap`
- Warning: `Use fromMaybe`

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
