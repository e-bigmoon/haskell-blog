---
title: extensible-0.4.9 がリリースされました。
author: Shinya Yamaguchi
tags: bigmoon, extensible
---

## はじめに

BIG MOON では、業務に必要なツールを自社開発しており、プログラミング言語に **Haskell** を採用しています。実用的に利用し始めて3年ぐらい？です。

僕らが **Haskell** を利用していて一番困った点はレコードの取り扱いです。

- 異なる型のフィールドラベルに同じ名前を付けたい
- フィールド全体対して関数を適用したい
- フィールド多相な関数を定義したい

このような問題に対して [extensible](https://github.com/fumieval/extensible) という、(当初は謎に包まれていた) パッケージの利用を検討し、実際に既存のシステムを **extensible** で置き換えました。(当時アルバイトしていた [matsubara0507](https://github.com/matsubara0507) さんが居なければ実現不可能だったと思います)

今回、縁あって作者の [fumieval](https://twitter.com/fumieval) さんと一緒に仕事できる機会に恵まれました。fumieval さんは簡単な質問でも、とても気さくに答えてくれます。

僕達のノウハウはまだまだとても少ないですが、この素晴らしいパッケージを広く知って欲しいと思い、まだまだ作成途中ではありますが [extensible 攻略Wiki](https://wiki.hask.moe/) という親しみやすい雰囲気で情報を発信していくことになりました。

この wiki もまた **Haskell** で作られており [apus](https://github.com/fumieval/apus) という名前で公開されています。

今回の extensible-0.4.9 の[アップデート](https://github.com/fumieval/extensible/blob/master/CHANGELOG.md)は、攻略wiki のコンテンツを拡充していく中で出てきたアイデアや、関数などがいくつか追加されました。

今回はその内容について簡単な例とともに解説を行いたいと思います。

<!--more-->

## アップデート内容

- MonadIO のインスタンスを一般化しました。

ベースモナドとして **ResourceT IO** などが使えるようになりました。

---

- 新しい制約コンビネータ [And](https://hackage.haskell.org/package/extensible-0.4.9/docs/Data-Extensible-Dictionary.html#t:And) を追加しました。

このコンビネータを利用することで **Forall** の制約を二つ以上指定することができます。

```hs
And :: (k -> Constraint) -> (k -> Constraint) -> k -> Constraint
```

以下は簡単な例ですが、拡張可能レコードの値が **Show** かつ [Typeable](https://www.stackage.org/haddock/lts-11.9/base-4.10.1.0/Data-Typeable.html) の両方を満たすという制約で [hfoldMapFor](https://hackage.haskell.org/package/extensible-0.4.9/docs/Data-Extensible-Product.html#v:hfoldMapFor) 関数を使うことができます。

```hs
debug :: Forall (ValueIs (And Show Typeable)) xs => Record xs -> IO ()
debug = hfoldMapFor poly (print . fork id typeOf . runIdentity . getField)
  where
    poly = Proxy @ (ValueIs (And Show Typeable))
    fork f g x = (f x, g x)
```

この関数は与えられた拡張可能レコードの **値** と **型** を表示することができます。

```hs
>>> debug person
("\"bigmoon\"",[Char])
("10",Int)
```

[完全なコード](https://github.com/waddlaw/extensible-example/blob/master/release-article/0.4.9/And.hs)

---

- [stringAssocKey](https://hackage.haskell.org/package/extensible-0.4.9/docs/Data-Extensible-Field.html#v:stringAssocKey) 関数を追加しました。

```hs
stringAssocKey :: (IsString a, KnownSymbol (AssocKey kv)) => proxy kv -> a
```

この関数を使えば、拡張可能レコードのキーを文字列として取得することができます。

例えば、拡張可能レコードのキーを全て集めてリストにして返す関数は以下のように作ることができます。

```hs
keys :: (IsString key, Forall (KeyIs KnownSymbol) xs) => proxy xs -> [key]
keys xs = henumerateFor (Proxy @ (KeyIs KnownSymbol)) xs ((:) . stringAssocKey) []
```

[IsString](https://www.stackage.org/haddock/lts-11.9/base-4.10.1.0/Data-String.html#t:IsString) のインスタンスであれば何でも良いので、**String** に限らず **Text**, **ByteString** などを返すことができます。

[完全なコード](https://github.com/waddlaw/extensible-example/blob/master/release-article/0.4.9/StringAssocKey.hs)

---

- [prettyprinter](https://hackage.haskell.org/package/prettyprinter) パッケージの **Pretty** のインスタンスを追加しました。

prettyprinter パッケージについては[過去のブログ記事](https://haskell.e-bigmoon.com/posts/2018/03-30-prettyprinter.html)で少し紹介しているので、興味ある方はそちらをご確認ください。

まだ `master` にはマージされていないようですが、以下のようになるとのことです。

```hs
[ name: DA-192H
  weight: 260.0
  price: 120
  featured: True
  description: High-quality (24bit 192kHz), lightweight portable DAC
  quantity: 20
, name: HHP-150
  weight: 200.0
  price: 330
  featured: False
  description: Premium wooden headphone
  quantity: 55 ]
```

[完全なコード](https://github.com/waddlaw/extensible-example/blob/master/release-article/0.4.9/Pretty.hs)

---

- [th-lift](https://hackage.haskell.org/package/th-lift) の **Lift** のインスタンスを追加しました。

**Lift** のインスタンスになったので例えば、**Data.Yaml.TH** モジュールの [decodeFile](https://www.stackage.org/haddock/lts-11.9/yaml-0.8.30/Data-Yaml-TH.html#v:decodeFile) 関数を使ってコンパイル時に **yaml** ファイルから一気に拡張可能レコードを作り上げることができます。

```hs
config :: Config
config = $$(Yaml.TH.decodeFile "config.yaml")
```

[完全なコード](https://github.com/waddlaw/extensible-example/blob/master/release-article/0.4.9/Lift.hs)

---

- [hmapWithIndexFor](https://hackage.haskell.org/package/extensible-0.4.9/docs/Data-Extensible-Product.html#v:hmapWithIndexFor) を追加しました。

[hmapWithIndex](https://hackage.haskell.org/package/extensible-0.4.9/docs/Data-Extensible-Product.html#v:hmapWithIndex) の多相バージョンです。

例えば以下のようにして拡張可能レコードから **aeson** パッケージの [Value](https://www.stackage.org/haddock/lts-11.9/aeson-1.2.4.0/Data-Aeson.html#t:Value) をラッパーとして持つ拡張可能レコードに変換できます。

```hs
toJSONRecord :: Forall (ValueIs ToJSON) xs => Record xs -> RecordOf (Const' Value) xs
toJSONRecord = hmapWithIndexFor poly $ \m ->
    Field . Const' . toJSON . runIdentity . getField
  where poly = Proxy @ (ValueIs ToJSON)
```

[完全なコード](https://github.com/waddlaw/extensible-example/blob/master/release-article/0.4.9/HmapWithIndexFor.hs)

---

- [Const'](https://hackage.haskell.org/package/extensible-0.4.9/docs/Data-Extensible-Wrapper.html#t:Const-39-) に Monoid のインスタンスを追加しました。

---

- [Wrapper](https://hackage.haskell.org/package/extensible-0.4.9/docs/Data-Extensible-Wrapper.html#t:Wrapper) に Either e のインスタンスを追加しました。