---
title: HLint
---

`hlint` は自分のコードをより良くしてくれる静的解析ツールです。`hlint` の指摘に従うことでより `Haskell` らしい書き方が身につきます。

インストールは `stack` があれば簡単です。

```shell-session
$ stack install hlint
$ hlint --version
HLint v2.0.11, (C) Neil Mitchell 2006-2017
```

使い方もとても簡単です。検査したいディレクトリを指定するだけで再帰的にチェックしてくれます。

```shell
$ hlint app/
No hints

$ hlint src
src/Minfree.hs:53:3: Warning: Use sequence_
Found:
  sequence [writeArray a x True | x <- xs, x <= n]
Why not:
  sequence_ [writeArray a x True | x <- xs, x <= n]

1 hint
```

上記のように、ヒントを表示してくれるため、これ通りに書き換えていくことで独学でも良いコードを書くことができます。

このヒントを修正することはとても簡単です。`src/Minfree.hs:53:3` を指摘通りに修正するだけです。

```haskell:src/Minfree.hs
-- | Data.Array.ST モジュールを使った checklist
checklist' :: [Int] -> Array Int Bool
checklist' xs = runSTArray $ do
  a <- newArray (0, n) False
  -- 修正
  sequence_ [writeArray a x True | x <- xs, x<=n]
  return a
  where n = length xs
```

このように修正したら `HLint` でもう一度確認してみましょう。

```shell-session
$ hlint src
No hints

# プロジェクト全体をチェックしたい場合はこうする
$ hlint .
...
5 hints
```

独自ルールの追加や、既存ルールの警告をOFFにすることもできるので、`travis-ci` などの継続的インテグレーションツールを使って `hlint` をビルドのたびに実行するとコードの品質をある程度保つことができると思います。

また `ghc-mod` や `haskell-ide-engine` などと組み合わせ使うことで `hlint` を自動的に実行しながらコードを書くこともできます。
