---
title: (^>>) と (>>^)
author: Shinya Yamaguchi
tags: bigmoon
---

## はじめに

[Control.Arrow](https://www.stackage.org/haddock/lts-12.5/base-4.11.1.0/Control-Arrow.html#) モジュールに [(^>>)](https://www.stackage.org/haddock/lts-12.5/base-4.11.1.0/Control-Arrow.html#v:-94--62--62-) と [(>>^)](https://www.stackage.org/haddock/lts-12.5/base-4.11.1.0/Control-Arrow.html#v:-62--62--94-) という演算子が定義されています。

ちょっとだけ面白かったので紹介しようと思います。

<!--more-->

## 準備

例として、文字列を全て**大文字**に変換する関数を考えてみましょう。

素朴に書けばこんな感じでしょうか。

```haskell
import Data.Char (toUpper)

toAllUpper :: String -> String
toAllUpper = map toUpper
```

この関数は以下のように期待通りに動きます。

```shell
*Main> toAllUpper "B|g0on"
"B|G0ON"
```

ではここで、同じように `Text` バージョンを作りたいと思います。何も考えずに pack と unpack を使えばすぐ定義できますね。

```haskell
import           Data.Char (toUpper)
import           Data.Text (Text)
import qualified Data.Text as T

toAllUpper :: String -> String
toAllUpper = map toUpper

toAllUpperText :: Text -> Text
toAllUpperText = T.pack . toAllUpper . T.unpack
```

この関数も同様にちゃんと動きます。

```haskell
*Main> toAllUpperText $ T.pack "B|g0on"
"B|G0ON"
```

## (^>>) と (>>^) を無理矢理使ってみる

先程定義した `toAllUpperText` 関数は `(^>>)` と `(>>^)` を使って、以下のように書き直すことができます。(`Control.Arrow` をインポートする必要があります。)

左から右バージョン

```haskell
toAllUpperText' :: Text -> Text
toAllUpperText' = T.unpack ^>> toAllUpper >>^ T.pack
```

右から左バージョン

```haskell
toAllUpperText'' :: Text -> Text
toAllUpperText'' = T.pack <<^ toAllUpper ^<< T.unpack
```

関数合成 (`.`) を単純に置き換えただけです・・・。

### 型

この演算子の何が面白いかと言うと、**入力**と**出力**をそれぞれ変換できるんですね！

```hs
(^>>) :: Arrow a => (b -> c) -> a c d -> a b d
(>>^) :: Arrow a => a b c -> (c -> d) -> a b d
```

```hs
(<<^) :: Arrow a => a c d -> (b -> c) -> a b d
(^<<) :: Arrow a => (c -> d) -> a b c -> a b d
```

先程の例は `Arrow a` の `a` を関数型 `(->)` として具体化したものになります。

```hs
(^>>) :: Arrow a => (b -> c) -> a c d -> a b d
  -- a = (->) として具体化した
    (b -> c) -> (->) c d -> (->) b d
  = (b -> c) -> (c -> d) -> (b -> d)
```

引数の順番を逆にすれば関数合成の型と同じですね。

```hs
  = (b -> c) -> (c -> d) -> (b -> d)
  -- 引数を逆にした
  (c -> d) -> (b -> c) -> (b -> d)
  -- 変数名の変更
  = (b -> c) -> (a -> b) -> (a -> c)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

## まとめ

- 現実世界で Arrow をバリバリ使っているコードをみかけることは (僕は) 全く無いです。(Hakyll は過去に Arrow ベースで定義されていたようですが、Monad ベースに切り替わりました)

たぶん使うことは無いですが、パズルみたいで楽しい。

以上です。