---
title: プロジェクトで禁止している関数を関数の検出
date: 2018/02/12
---

## HLint を導入するメリット

プロジェクト内で部分関数 (例: `fromJust`) を使わせないようにさせたり、`undefined` が残っていないかなどのチェックをレビュー時に人間が行っていたりしませんか？

人間が介入するということは必ずミスが起こります。人間が気をつければミスは起こらないと思っていたり、精神力でなんとかしようとしている場合は能力不足を疑われても仕方がありません。

また、そのようなつまらない間違い探しのような非クリエイティブな作業に大切な時間を割いてしまうのはとても良くないことです。

`HLint` を使えば、そのような関数を検出することが可能です。実際には `関数` だけでなく `言語拡張`, `フラグ`, `モジュール` なども指定することができます。

## 関数を指定する方法

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

## 特定のモジュールのみを対象とする方法

`Lib` モジュールのみを検査対象とする場合は `within` キーワードを次のようにします。

```yaml
# .hlint.yaml
- functions:
  - {name: undefined, within: [Lib]}
```