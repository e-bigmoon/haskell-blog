---
title: hoogle
---

開発を進めて行くとこんなことを良く思います。

- 型で関数を検索できないかな？
- 関数名の一部は覚えているんだけど、全部の名前は忘れた
- コードに書かれている関数がどのモジュールで定義されているか知りたい

`Haskell` に詳しい方であれば [hayoo](http://hayoo.fh-wedel.de/) や [hoogle](https://www.haskell.org/hoogle/) を使って上記の問題を解決していると思います。個人的には `hayoo` が好きです。

これらのツールで基本的には事足りるのですが、`stack hoogle` を使うと以下のようなメリットがあります。

- 現在指定しているプロジェクトの `lts` のバージョンに基づいて `hoogle` 検索が可能になる
- 自分で定義した関数も `hoogle` 検索の対象となる (エクスポートしていれば)
- オフラインで利用できる

実際に使う際も非常に簡単です。`stack hoogle --setup` をしていない場合は初回実行時に少し時間がかかります。

```shell-session
# グローバルデータベースの作成
$ stack exec -- hoogle generate
# プロジェクトデータベースの作成
$ stack hoogle

$ stack hoogle "con map"
Prelude concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
Data.List concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
Data.Foldable concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
GHC.OldList concatMap :: (a -> [b]) -> [a] -> [b]
...

$ stack hoogle "minfree"
Ch01.Minfree minfree :: [Int] -> Int
module Ch01.Minfree
Ch01.Minfree minfree' :: [Int] -> Int

$ stack hoogle "a -> a"
Prelude id :: a -> a
Data.Function id :: a -> a
GHC.Exts breakpoint :: a -> a
GHC.Exts lazy :: a -> a
...

$ stack hoogle -- "a -> a" --count=20
Prelude id :: a -> a
Data.Function id :: a -> a
GHC.Exts breakpoint :: a -> a
GHC.Exts lazy :: a -> a
...
```

##### hoogle
- [ndmitchell/hoogle](https://github.com/ndmitchell/hoogle)
