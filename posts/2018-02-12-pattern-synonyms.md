---
title: Pattern Synonyms で DEPRECATED
author: Shinya Yamaguchi
tags: bigmoon, 言語拡張
---

## はじめに

`persistent` ライブラリで `SomeField` を `CopyField` に置き換える際の[PR](https://github.com/yesodweb/persistent/pull/760)で、パターンシノニム (Pattern Synonyms) の面白い使い方を見つけました。

もしかしたら良く知られた技なのかもしれませんが、ご紹介したいと思います。

パターンシノニムが何かについての説明は、今回全くしていませんのでわかる人向けです。

<!--more-->

## モチベーション

パターンシノニムはパターンの再利用を可能にするための言語拡張です。詳しくは [User's Guide 10.7. Pattern synonyms](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pattern-synonyms) をご確認ください。

使いみちは色々あると思いますが、今回はコンストラクタを `DEPRECATED` にするために利用する例をご紹介します。（実際のコードは[このあたり](https://github.com/parsonsmatt/persistent/blob/c882203c9cc09ba28b2012b58b4cd0fdc415e6ce/persistent-mysql/Database/Persist/MySQL.hs#L1080)です)

`Persistent` ライブラリの `3.0.0` から `SomeField` が廃止され `HandleUpdateCollision` 型が導入されました。

`SomeField` 型は `Persistent` の中でも結構重要な型 (`SomeException` と同じような雰囲気の型) です。また `Yesod` のプロジェクトでは多数のユーザがいるため、このような変更に対しては、いきなり削除するのではなく `DEPRECATED` プラグマを利用して、新しい関数への移行を促しています。

今回の場合、**関数**ではなく**データ型**を変更する必要があるためパターンシノニムが利用されています。

## 実際のコード

コメント等を適宜削除しましたが、実際にはこんな感じで利用されています。

```haskell
data HandleUpdateCollision record where
  CopyField :: EntityField record typ -> HandleUpdateCollision record
  CopyUnlessEq :: PersistField typ => EntityField record typ -> typ -> HandleUpdateCollision record

type SomeField = HandleUpdateCollision

#if MIN_VERSION_base(4,8,0)
pattern SomeField :: EntityField record typ -> SomeField record
#endif
pattern SomeField x = CopyField x
{-# DEPRECATED SomeField "The type SomeField is deprecated. Use the type HandleUpdateCollision instead, and use the function copyField instead of the data constructor." #-}
```

重要な部分はここですね。

```haskell
pattern SomeField x = CopyField x
{-# DEPRECATED SomeField "..." #-}
```

`Haskell` には**型シノニム**があるのに**データシノニム**って無いんですね。。。って思った人はパターンシノニムを使えばこんな感じで実現可能です。

通常、パターンシノニムを利用するモチベーションはパターンマッチの再利用にあると思うのですが、単純にデータコンストラクタのエイリアスにしておいて `DEPRECATED` を促すやり方も結構使えるなーと思いました。

以上です。