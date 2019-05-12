---
title: Megaparsec tutorial from IH book
author: Mark Karpov
translator: Wataru Yamada
tags: megaparsec, package, 翻訳
---

Great original post: [Megaparsec tutorial from IH book](https://markkarpov.com/megaparsec/megaparsec.html)

この Megaparsec のチュートリアルは、元々は [Intermediate Haskell](https://intermediatehaskell.com/) という本の１つの章のために書かれました。
ここ一年で本が進展していないため、
他の著者は私がスタンドアロンのチュートリアルとしてテキストを公表し、
人々が少なくとも私たちの仕事のこの部分から恩恵を受けることができるようにする
ことに同意しました。

<!--more-->

- [`ParsecT` と `Parsec` モナド](#ParsecT)
- [文字 とバイナリストリーム](#Character)
- [モナディック構文とアプリカティブ構文](#MonaAp)
- [`Eof` による入力の強制消費](#Eof)
- [選択肢を使った動作](#Alt)
- [`try` によるバックトラックの制御](#Try)
- [パーサのデバッグ](#Debug)
- [ラベル付けと隠蔽](#Label)
- [パーサの実行](#Run)
- [`MonadParsec` 型クラス](#MonadParsec)
- [字句解析](#Lexing)
  - [空白](#White)
  - [文字と文字列リテラル](#Char)
  - [数字](#Numbers)
- [`notFollowedBy` と `lookAhead`](#lookAhead)
- [式のパース](#Expr)
- Indentation-sensitive parsing
  - nonIndented and indentBlock
  - Parsing a simple indented list
  - Nested indented list
  - Adding line folds
- Writing efficient parsers
- Parse errors
  - Parse error definitions
  - How to signal a parse error
  - Displaying parse errors
  - Catching parse errors in running parser
- Testing Megaparsec parsers
- Working with custom input streams

「例：あなた自身のパーサコンビネータを書く」の章で開発されたトイパーサコンビネータは、実際の使用には適していないので、
同じ問題を解決するHaskellエコシステムのライブラリを見ていきましょう。
そして、それらがなすさまざまなトレードオフに注意してください。

- [parsec](https://hackage.haskell.org/package/parsec) は長い間 Haskellの「デフォルト」のパーサライブラリでした。このライブラリは、エラーメッセージの品質に焦点を当てていると言われています。ただし、テストカバレッジは良くなく、現在メンテナンスモードになっています。

- [attoparsec](https://hackage.haskell.org/package/attoparsec) は、パフォーマンスを重視した堅牢で高速なパーサライブラリです。このリストの中で、インクリメンタルパージングを完全にサポートしているのはこれだけです。欠点はエラーメッセージの質が悪いこと、モナド変換子として使用できないこと、および入力ストリームとして使用できる型の組み合わせが限られていることです。

- [trifecta](https://hackage.haskell.org/package/trifecta) は優れたエラーメッセージを特徴としていますが、あまり文書化されておらず、理解するのが難しいです。 `String` と `ByteString` はそのままではパースできますが、`Text` はパースできません。

- [megaparsec](https://hackage.haskell.org/package/megaparsec) は、ここ数年で積極的に開発されてきた `parsec` のフォークです。現在のバージョンは、速度、エラーメッセージの品質、そして柔軟性の間で素晴らしいバランスを取っています。 `parsec` の非公式の後継者として、`parsec` ライブラリを使用したことがあるか、チュートリアルを読んだことがあるユーザにとっては慣習的でなじみのあるものです。

これらすべてのライブラリを網羅しようとするのは実用的ではないため、 `megaparsec` に焦点を当てます。より正確には、この本が出版される時までにはほとんどどこでも古いバージョンに取って代わるであろうバージョン7をカバーするつもりです。

<a name="ParsecT"></a>

## ParsecT と Parsec モナド

`ParsecT`は、主要なパーサモナド変換子であり、`megaparsec` の中心的なデータ型です。
`ParsecT e s m a` は、次のようにパラメータ化されています。

- `e` はエラーメッセージのカスタムコンポーネントの型です。もし私たちが何もカスタムを望まないのであれば（そして今のところ私たちはしません）、 `Data.Void` モジュールの `Void` を使うだけです。

- `s` は入力ストリームの型です。 `megaparsec` は、`String`、正格または遅延 `Text` 、 正格または遅延 `ByteStrings` をそのまま使用して動作します。カスタム入力ストリームを扱うことも可能です。

- `m` は `ParsecT` モナド変換子の内部モナドです。

- `a` はパース結果であるモナディック値です。

ほとんどの場合、`m` は `Identity` に他ならないので、`Parsec` 型シノニムは非常に便利です。

```haskell
type Parsec e s a = ParsecT e s Identity a
```

`Parsec` は、単なる `ParsecT`の変換子を使わないバージョンです。

`megaparsec` のモナド変換子と MTL のモナド変換子およびクラスの間の類似点を示すこともできます。
確かに、`MonadState` や `MonadReader` などの型クラスと目的が似ている `MonadParsec` 型クラスがあります。
後で `MonadParsec` に戻り、詳細について説明します。

カスタムした型シノニムを定義することは `megaparsec` を使ってパーサを書くことを始める最も良い方法です。
これは次の2つの理由から良い考えです。

- あなたのパーサモナドとして `Parser` があれば、`Parser Int` のようなトップレベルのシグネチャを追加することがより簡単になります。シグネチャがないと、`e` のようなものが曖昧になることがよくあります。これはライブラリの多相APIの反面です。

- すべての型変数を具体的な方に固定して操作すると、GHCの最適化が大幅に向上します。パーサが多相性を保っている場合、GHCは最適化の観点からそれほど多くのことはできません。`megaparsec` APIは多相ですが、エンドユーザーは具体的にパーサモナドの型を固定することが予想されます。そのため、インライン展開と、ほとんどの関数の定義がインターフェイスファイルと呼ばれるファイルに出力されているという事実により、GHCは非常に効率的な非多相的なコードを生成できます。

次のように型シノニム（通常は `Parser` と呼ばれる）を定義しましょう。

```haskell
ype Parser = Parsec Void Text
--                   ^    ^
--                   |    ┗━━━━┓
-- カスタムエラーコンポーネント  入力ストリームの型
```

カスタムパースエラーを扱い始めるまでは、この章で `Parser`が表示されているときは、この型を想定してください。

<a name="Character"></a>

## 文字とバイナリストリーム

`megaparsec` は、5種類の入力ストリーム（`String`、正格または遅延 `Text` 、 正格または遅延 `ByteStrings`）をそのまま使用できると言われています。
これが可能なのは、ライブラリでこれらの型が 
`Stream` 型クラスのインスタンスになっているからです。
`Stream` 型クラスはそれぞれのデータ型が
`megaparsec` のパーサへの入力として使用するために必要な関数を抽象化したものです。

シンプルにしたバージョンの `Stream` は、次のようになります。

```haskell
class Stream s where
  type Token  s :: *
  type Tokens s :: *
  take1_ :: s -> Maybe (Token s, s) -- aka uncons
  tokensToChunk :: Proxy s -> [Token s] -> Tokens s
```

実際の `Stream` の定義にはもっと多くのメソッドがありますが、それらを知ることはライブラリを使うために必要ではありません。

型クラスには2つの型関数が関連付けられています。

- ストリーム `s` における `Token s` は単一トークンの型です。一般的な例は `Char` と `Word8` ですが、カスタムストリームのために何か他のものになるかもしれません。

- ストリーム `s` における `Tokens s` はストリームの「チャンク」の型です。チャンクの概念はパフォーマンス上の理由から導入されただけです。確かに、トークンのリスト `[Token s]` と同型であるストリームの一部のより効率的な表現が可能です。例えば、 `Text` 型の入力ストリームは `Tokens s ~ Text` であり、`Text` のチャンクは `Text` です。型の等価性 `Tokens s ~ s` はしばしば成り立ちますが、 `Tokens s` と `s` はカスタムストリームでは異なる可能性があるため、`megaparsec` ではこれらの型を分離します。

デフォルトの入力ストリームの型を以下の表に示します。

|`s`|`Token s`|`Tokens s`|
|:-|:-|:-|
|`String`|`Char`|`String`|
|正格 `Text`|`Char`|正格 `Text`|
|遅延 `Text`|`Char`|遅延 `Text`|
|正格 `ByteString`|`Word8`|正格 `ByteString`|
|遅延 `ByteString`|`Word8`|遅延 `ByteString`|

`Token` および `Tokens` 型関数は `megaparsec` API の型にて
多く出現するため、慣れることが重要です。

お気づきかもしれませんが、デフォルト入力ストリームを`Token`型でグループ化すると、2つのグループになります。

- `Token s ~ Char` となる文字ストリーム : `String` および 正格・遅延 `Text`。

- `Token s ~ Word8` となるバイナリストリーム : 正格・遅延 `ByteString`。

`megaparsec` では、それぞれの型の入力ストリームに対して同じパーサをコーディングする必要はないことがわかりました。
(`attoparsec` ライブラリではする必要があります)
しかし、それぞれの `Token s` ごとに異なるコードが必要です。

- 文字ストリームの共通のコンビネータを得るには、`Text.Megaparsec.Char` モジュールをインポートしてください。

- バイナリストリームで同じようにするには、`Text.Megaparsec.Byte`をインポートします。

これらのモジュールには、次のような2つの類似したヘルパーパーサのセットが含まれています。

|**Name**|`Text.Megaparsec.Char`|`Text.Megaparsec.Byte`|
|:-|:-|:-|
|`newline`|`(MonadParsec e s m, Token s ~ Char) => m (Token s)`|`(MonadParsec e s m, Token s ~ Word8) => m (Token s)`|
|`eol`|`(MonadParsec e s m, Token s ~ Char) => m (Tokens s)`|`(MonadParsec e s m, Token s ~ Word8) => m (Tokens s)`|

このモジュールを構築するプリミティブをいくつか紹介しましょう。そうすれば、これから使用するツールを理解できます。

最初のプリミティブは`token`と呼ばれ、`Token s`を解析することができます。

```haskell
token :: MonadParsec e s m
  => (Token s -> Maybe a)
    -- ^ Matching function for the token to parse
  -> Set (ErrorItem (Token s))
     -- ^ Expected items (in case of an error)
  -> m a
```

`token` の最初の引数はパースしたいトークンのマッチング関数です。
関数が`Just`で何かを返す場合、その値はパース結果になります。
`Nothing` はパーサがトークンを受理しなかったことによりプリミティブが失敗したこと示します。

2番目の引数は、（`container`パッケージの）`Set` であり、失敗した場合にユーザーに表示されるすべての予想されるErrorItemを含みます。
パースエラーについて議論するときに、`ErrorItem` 型を詳しく調べます。

トークンがどのように機能するのかをよりよく理解するために、
`Text.Megaparsec` モジュールの定義を見てみましょう。
これにはあらゆる種類の入力ストリームで
機能するコンビネータが含まれています。
`satisfy`はかなり一般的なコンビネータです。
マッチさせたいトークンを与えると `True` を返す述語を与え、
パーサは結果を返します。

```haskell
satisfy :: MonadParsec e s m
  => (Token s -> Bool) -- ^ Predicate to apply
  -> m (Token s)
satisfy f = token testToken Set.empty
  where
    testToken x = if f x then Just x else Nothing
```

`testToken` は `Bool` を返す関数 `f` を
期待するトークン `Maybe (Token s)` を返す関数に変えます。
`satisfy` では、
一致すると予想される正確なトークン列がわからないため、
2番目の引数として `Set.empty` を渡します。

`satisfy` を理解するために、機能するか見てみましょう。
パーサで遊ぶためには、それを実行するヘルパー関数が必要です。
GHCiでテストするために `megaparsec` は `parseTest` を提供します。

まず、GHCiを起動していくつかのモジュールをインポートしましょう。

```bash
λ> import Text.Megaparsec
λ> import Text.Megaparsec.Char
λ> import Data.Text (Text)
λ> import Data.Void
```

パーサの型のあいまいさを解決するために、
使用する `Parser` 型シノニムを追加します。


```bash
λ> type Parser = Parsec Void Text
```

また、文字列リテラルを `Text` の値として使用できるように、`OverloadedStrings` 言語拡張を有効にする必要があります。

```bash
λ> :set -XOverloadedStrings

λ> parseTest (satisfy (== 'a') :: Parser Char) ""
1:1:
  |
1 | <empty line>
  | ^
unexpected end of input

λ> parseTest (satisfy (== 'a') :: Parser Char) "a"
'a'

λ> parseTest (satisfy (== 'a') :: Parser Char) "b"
1:1:
  |
1 | b
  | ^
unexpected 'b'

λ> parseTest (satisfy (> 'c') :: Parser Char) "a"
1:1:
  |
1 | a
  | ^
unexpected 'a'

λ> parseTest (satisfy (> 'c') :: Parser Char) "d"
'd'
```

`satisfy` の多相性により、
`parseTest` は `MonadParsec e s m` で
`e` と `s` に何を使うべきかわからないため、
アノテーション `:: Parser Char` は必要です
(`m` はこれらのヘルパーにより `Identity` と仮定されます)。
型シグネチャを持つ既存のパーサを使う場合、
パーサの型を明示的に説明する必要はありません。

うまくいきそうです。 `satisfy` の問題は、
それが失敗したときに何が期待されるのかを述べないということです。
なぜなら、
`satisfy` の呼び出し元が提供する関数を分析することができないからです。
あまり一般的ではないですが、
代わりにもっと有用なエラーメッセージを生成することができる
他のコンビネータがあります。
例えば、`single` (`Text.Megaparsec.Byte` と `Text.Megaparsec.Char` では `char` と呼ばれる型制約のあるシノニムを使用する) は特定のトークン値にマッチします。

```bash
single :: MonadParsec e s m
  => Token s           -- ^ Token to match
  -> m (Token s)
single t = token testToken expected
  where
    testToken x = if x == t then Just x else Nothing
    expected    = E.singleton (Tokens (t:|[]))
```

`Tokens` 値コンストラクタは、前に説明した型関数 `Tokens` 
と何の共通点もありません。
実際、 `Tokens` は `ErrorItem` のコンストラクタの1つであり、
一致すると予想される具体的なトークン列
を指定するために使用されます。

```bash
λ> parseTest (char 'a' :: Parser Char) "b"
1:1:
  |
1 | b
  | ^
unexpected 'b'
expecting 'a'

λ> parseTest (char 'a' :: Parser Char) "a"
'a'
```

以下のようにの改行を定義できます。

```bash
newline :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
newline = single '\n'
```

2つ目のプリミティブは`tokens`と呼ばれ、
`Tokens` をパースすることを可能にします。
つまり、入力の固定されたチャンクに一致させるために使用できます。

```bash
tokens :: MonadParsec e s m
  => (Tokens s -> Tokens s -> Bool)
    -- ^ チャンクの等価性をチェックする述語
  -> Tokens s
    -- ^ 入力にマッチさせたいチャンク
  -> m (Tokens s)
```

`tokens` に関して定義された2つのパーサがあります。

```haskell
-- from "Text.Megaparsec":
chunk :: MonadParsec e s m
  => Tokens s
  -> m (Tokens s)
chunk = tokens (==)

-- from "Text.Megaparsec.Char" and "Text.Megaparsec.Byte":
string' :: (MonadParsec e s m, CI.FoldCase (Tokens s))
  => Tokens s
  -> m (Tokens s)
string' = tokens ((==) `on` CI.mk)
```

それらは入力の一定のチャンクにマッチします。
`chunk` (`Text.Megaparsec.Byte` と `Text.Megaparsec.Char` では `string` と呼ばれる型制約のあるシノニムを使用する) は大文字と小文字を区別しますが、
`string'` は大文字と小文字を区別しません。
大文字と小文字を区別しない場合のマッチには
`case-insensitive` パッケージが使われているため、
`FoldCase` 制約があります。

新しいコンビネータも使ってみましょう。

```haskell
λ> parseTest (string "foo" :: Parser Text) "foo"
"foo"

λ> parseTest (string "foo" :: Parser Text) "bar"
1:1:
  |
1 | bar
  | ^
unexpected "bar"
expecting "foo"

λ> parseTest (string' "foo" :: Parser Text) "FOO"
"FOO"

λ> parseTest (string' "foo" :: Parser Text) "FoO"
"FoO"

λ> parseTest (string' "foo" :: Parser Text) "FoZ"
1:1:
  |
1 | FoZ
  | ^
unexpected "FoZ"
expecting "foo"
```

OK、単一のトークンと入力のチャンクをマッチできました。
次のステップは、より興味深いパーサを書くために
基本的なパーサを組み合わせる方法を学びます。

<a name="MonaAp"></a>

## モナディック構文とアプリカティブ構文

パーサを組み合わせる最も簡単な方法は、それらを逐次実行することです。
`ParsecT`と`Parsec`はモナドであり、
モナドでの束縛はパーサを連続で使用することとまったく同じです。

```haskell
mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)
```

これを実行すると、すべてが期待通りに機能することを確認できます。


```bash
λ> parseTest mySequence "abc"
('a','b','c')

λ> parseTest mySequence "bcd"
1:1:
  |
1 | bcd
  | ^
unexpected 'b'
expecting 'a'

λ> parseTest mySequence "adc"
1:2:
  |
1 | adc
  |  ^
unexpected 'd'
expecting 'b'
```

すべてのモナドがアプリカティブファンクターでもあることを覚えていれば、
逐次実行のための代替構文が可能であり、
アプリカティブ構文を使用できます。

```haskell
mySequence :: Parser (Char, Char, Char)
mySequence =
  (,,) <$> char 'a'
       <*> char 'b'
       <*> char 'c'
```

2番目のバージョンは最初のバージョンとまったく同じように機能します。
どちらのスタイルを使うかは、しばしば好みの問題です。
モナディックスタイルは間違いなく冗長で、ときにはより明確ですが、
一方アプリカティブスタイルはより簡潔です。
そうは言っても、モナドはアプリカティブファンクターよりも強力であるため、
モナディックスタイルはもちろんより強力です。

<a name="Eof"></a>

## eof による入力の強制消費

`Applicative` はとてもおもしろいことをするのに十分強力です。
単位元を持つ結合演算子を備えることで、
Haskell では `Alternative` 型クラスとして表現される
アプリカティブファンクタのモノイドを得ます。
`parser-combinators` パッケージは
`Applicative` と `Alternative` の概念に基づき構築された
かなりの数の抽象的なコンビネータを提供します。
`Text.Megaparsec` モジュールはそれらを
`Control.Applicative.Combinators` から再エクスポートします。

最も一般的なコンビネータの1つに、`many` と呼ばれるものがあります。
それは与えられたパーサを0回以上実行することができます。

```
λ> parseTest (many (char 'a') :: Parser [Char]) "aaa"
"aaa"

λ> parseTest (many (char 'a') :: Parser [Char]) "aabbb"
"aa"
```

2番目の結果は少し驚くかもしれません。
パーサは `a` が一致したとして消費しましたが、その後停止しました。
`many (char 'a')` の後に何をしたいのか何も言っていませんでした！

ほとんどの場合は、パーサに入力全体の消費を強制させ、
恥ずかしがり屋で黙ってやめるのではなく、
パースエラーを報告させたいです。
これは、入力の終わりに達することを
要求することによって行われます。
入力の終わりは概念にすぎませんが、
幸いにも `eof :: MonadParsec e m => m ()`と呼ばれるプリミティブがあり、
これは何も消費せず、入力の終わりでのみ成功します。
これをパーサに追加してもう一度試してみましょう。

```bash
λ> parseTest (many (char 'a') <* eof :: Parser [Char]) "aabbb"
1:3:
  |
1 | aabbb
  |   ^
unexpected 'b'
expecting 'a' or end of input
```

パーサで`b`について何も言わなかったことにより、
それらは確かに予想外となりました。

<a name="Alt"></a>

## 選択肢を使った動作

これから、次の形式のURIのパースが可能である実用的なパーサを開発します。

```
scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
```

角括弧 `[]` の中はオプションであり、それらは有効なURIに現れても現れなくてもよいことを覚えておくべきです。
`[]` は、ある可能性を別の可能性の中で表現するためにネストすることさえできます。私たちはこのすべてを処理します[^1]。

[^1]: RFC 3986 に従ってURIのパースが可能であり Megaparsec パーサを含む [`modern-uri`](https://hackage.haskell.org/package/modern-uri) パッケージが実際にあります。ただし、パッケージのパーサはここで説明したものよりもはるかに複雑です。


`scheme` から始めましょう。 `data`、`file`、`ftp`、`http`、`https`、`irc`、`mailto` など、私たちが知っているスキームのみを受け入れます。

一定の文字列と一致させるために、`string` を使います。
選択を表現するために、`Alternative` 型クラスの `(<|>)` メソッドを使います。
次のように書くことができます。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

pScheme :: Parser Text
pScheme = string "data"
  <|> string "file"
  <|> string "ftp"
  <|> string "http"
  <|> string "https"
  <|> string "irc"
  <|> string "mailto"
```

試してみましょう。

```bash
λ> parseTest pScheme ""
1:1:
  |
1 | <empty line>
  | ^
unexpected end of input
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"

λ> parseTest pScheme "dat"
1:1:
  |
1 | dat
  | ^
unexpected "dat"
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"

λ> parseTest pScheme "file"
"file"

λ> parseTest pScheme "irc"
"irc"
```

見栄えは良いですが、`pScheme`の定義は少し反復的です。 `choice` コンビネータを使って `pScheme` を書く方法があります。

```haskell
pScheme :: Parser Text
pScheme = choice
  [ string "data"
  , string "file"
  , string "ftp"
  , string "http"
  , string "https"
  , string "irc"
  , string "mailto" ]
```

`choice` は要素間に `(<|>)` を入れてリストを畳み込む操作である
`asum` の単なるシノニムであり、`pScheme` の2つの定義は実際には同じです。
`choice` を使用したほうがが少し良く見えるかもしれません。

スキームの後にはコロン`:`があるはずです。
何かした後で別の何かを要求するために、
モナドでの束縛またはdo記法を使います。

```haskell
data Uri = Uri
  { uriScheme :: Text
  } deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
  r <- pScheme
  _ <- char ':'
  return (Uri r)
```

`pUri`を実行しようとすると、スキーム名の後に `:` が必要であることがわかります。

```bash
λ> parseTest pUri "irc"
1:4:
  |
1 | irc
  |    ^
unexpected end of input
expecting ':'

λ> parseTest pUri "irc:"
Uri {uriScheme = "irc"}
```

しかし、このスキームのパーサは完成していません。
良い Haskell のプログラマーは、
正しくないデータを単純に表現できないように型を定義しようとします。
すべての `Text` の値が有効なスキームであるとは限りません。
スキームを表すためにデータ型を定義し、
`pScheme` パーサにその型の値を返させます。

```haskell
data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttp   <$ string "http"
  , SchemeHttps  <$ string "https"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

data Uri = Uri
  { uriScheme :: Scheme
  } deriving (Eq, Show)
```

`(<$)` 演算子は、左側にある値をただちに関数型コンテキストに入れて、
その時点で存在しているものはすべて置き換えます。
`a <$ f` は `const a <$> f` と同じですが、
関数によってはより効率的な場合があります。

引き続きパーサを使ってみましょう。

```
λ> parseTest pUri "https:"
1:5:
  |
1 | https:
  |     ^
unexpected 's'
expecting ':'
```

うーん、`https` は有効なスキームであるべきです。
何がいけないのかわかりますか？
パーサは選択肢を一つずつ試し、
`http` で一致し、 `https` を試すことはありません。
解決策は、`SchemeHttp <$ string "https"` という行を
`SchemeHttp <$ string "http"` という行の前に置くことです。
選択肢は順序が重要であることを覚えておいてください！

`pUri` は正しく動作するようになりました。

```
λ> parseTest pUri "http:"
Uri {uriScheme = SchemeHttp}

λ> parseTest pUri "https:"
Uri {uriScheme = SchemeHttps}

λ> parseTest pUri "mailto:"
Uri {uriScheme = SchemeMailto}

λ> parseTest pUri "foo:"
1:1:
  |
1 | foo:
  | ^
unexpected "foo:"
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
```

<a name="Try"></a>

## `try` によるバックトラックの制御

次に扱う部分は `[//[user:password@]host[:port]]` つまり認証情報です。
オプション部分のネストが含まれるので、
これを反映するように `Uri` 型 を更新しましょう。

```haskell
data Uri = Uri
  { uriScheme    :: Scheme
  , uriAuthority :: Maybe Authority
  } deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text) -- (user, password)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)
```

ここで、バックトラックと呼ばれる重要な概念について議論する必要があります。
バックトラックは、入力を「消費しない」処理により時間を遡る方法です。
これは主に分岐で重要です。ここに一例を示します。

```haskel
alternatives :: Parser (Char, Char)
alternatives = foo <|> bar
  where
    foo = (,) <$> char 'a' <*> char 'b'
    bar = (,) <$> char 'a' <*> char 'c'
```

合理的に見えますが、これを試してみましょう。

```
λ> parseTest alternatives "ab"
('a','b')

λ> parseTest alternatives "ac"
1:2:
  |
1 | ac
  |  ^
unexpected 'c'
expecting 'b'
```

ここで起きたことは、
`foo` の `char 'a'` の部分(これが最初に試行されます)の成功と、
入力ストリームからの `a` の消費です。
`char 'b'` は `'c'`とのマッチに失敗したため、エラーになりました。
ここで重要なことは、
`foo` が何らかの入力を消費しているので
`(<|>)` は `bar` を試していないということです！

これはパフォーマンス上の理由から行われており、
また、`foo` の残り物を `bar` に与えて実行するのは意味が無いです。
`bar` は `foo` と同じ場所の入力ストリームから実行したいです。
`megaparsec` は `attoparsec` や前の章のトイコンビネータとは異なり、
自動で戻りません。そのため、`try` と呼ばれるプリミティブを使用して、
明示的にバックトラックしたいという願望を表現する必要があります。
`try p` は、 `p` が入力の消費に失敗した場合、
入力が消費されていないかのように失敗します
(実際、パーサの状態全体をバックトラックします)。
これにより `(<|>)` で右側の選択肢を試すことが可能になります。

```haskell
alternatives :: Parser (Char, Char)
alternatives = try foo <|> bar
  where
    foo = (,) <$> char 'a' <*> char 'b'
    bar = (,) <$> char 'a' <*> char 'c'
```

```
λ> parseTest alternatives "ac"
('a','c')
```

実際に入力を消費するすべてのプリミティブ
（`try` などの既存のパーサの動作を変更するプリミティブもあります）は、
入力の消費という点で「アトミック」です。
これは、失敗した場合に自動的にバックトラックするため、
入力を消費して途中で失敗することはできないことを意味します。
これが、`pScheme` の選択肢のリストが機能する理由です。
つまり、`string` は `tokens` の上に定義され、`tokens` はプリミティブです。
文字列全体を `string` でマッチさせるか、
入力ストリームをまったく消費せずに失敗します。

URIのパースに戻ると、
`(<|>)` を使った `optional` という便利なコンビネータを作ることができます。

```haskell
optional :: Alternative f => f a -> f (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
```

`optional p` の `p` でマッチすれば、結果は `Just` になります。
そうでなければ `Nothing` が返されます。
ちょうど欲ほしかったものです！
`optional` を定義する必要はありません、
`Text.Megaparsec` はこのコンビネータを再エクスポートします。
これを `pUri` で使うことができます。

```haskell
pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme
  void (char ':')
  uriAuthority <- optional . try $ do            -- (1)
    void (string "//")
    authUser <- optional . try $ do              -- (2)
      user <- T.pack <$> some alphaNumChar       -- (3)
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal) -- (4)
    return Authority {..}                        -- (5)
  return Uri {..}                                -- (6)
```

ユーザー名とパスワードとして
任意の英数字の文字列を受け入れることができるようにし、
同様にホストの形式を単純化しました。

以下に重要な点を挙げます。

- (1) と (2) では、 `optional` の引数を `try` でラップする必要があります。
これは複合パーサであり、プリミティブではないためです。

- (3) `some` は `many` に似ていますが、その引数のパーサが少なくとも一度はマッチすることを要求します(`some p =（:) <$> p <*> many p`)。

- (4) 必要でない限り `try` を使わないでください！ここでは `char ':'` が成功すると (`token` の上に構築されているので, `try` は必要はありません)、そのあとに必ずポートが続かなければならないことがわかっているので、`L.decimal` により 10進数を要求します。`:` にマッチした後は、後戻りできないので、戻る方法は必要ありません。

- (5) と (6) では、`RecordWildCards` 言語拡張を使用して `Authority` と `Uri` の値を作り上げます。

GHCiで `pUri` を試し、それが機能することを確認してください。

```
λ> parseTest (pUri <* eof) "https://mark:secret@example.com"
Uri
  { uriScheme = SchemeHttps
  , uriAuthority = Just (Authority
    { authUser = Just ("mark","secret")
    , authHost = "example.com"
    , authPort = Nothing } ) }

λ> parseTest (pUri <* eof) "https://mark:secret@example.com:123"
Uri
  { uriScheme = SchemeHttps
  , uriAuthority = Just (Authority
    { authUser = Just ("mark","secret")
    , authHost = "example.com"
    , authPort = Just 123 } ) }

λ> parseTest (pUri <* eof) "https://example.com:123"
Uri
  { uriScheme = SchemeHttps
  , uriAuthority = Just (Authority
    { authUser = Nothing
    , authHost = "example.com"
    , authPort = Just 123 } ) }

λ> parseTest (pUri <* eof) "https://mark@example.com:123"
1:13:
  |
1 | https://mark@example.com:123
  |             ^
unexpected '@'
expecting '.', ':', alphanumeric character, or end of input
```

<a name="Debug"></a>

## パーサのデバッグ

面白いことが起こっていることに気付くかもしれません。

```haskell
λ> parseTest (pUri <* eof) "https://mark:@example.com"
1:7:
  |
1 | https://mark:@example.com
  |       ^
unexpected '/'
expecting end of input
```

パースエラーを改善できそうです。何をすればいいでしょうか？
何が起きているのかを知る最も簡単な方法は、
組み込みのヘルパー `dbg` を使うことです。

```haskell
dbg :: (Stream s, ShowToken (Token s), ShowErrorComponent e, Show a)
  => String            -- ^ デバッグ用のラベル
  -> ParsecT e s m a   -- ^ デバッグするパーサ
  -> ParsecT e s m a   -- ^ デバッグメッセージを出力するパーサ
```

これを `pUri` で使ってみましょう。

```
pUri :: Parser Uri
pUri = do
  uriScheme <- dbg "scheme" pScheme
  void (char ':')
  uriAuthority <- dbg "auth" . optional . try $ do
    void (string "//")
    authUser <- dbg "user" . optional . try $ do
      user <- T.pack <$> some alphaNumChar
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> dbg "host" (some (alphaNumChar <|> char '.'))
    authPort <- dbg "port" $ optional (char ':' *> L.decimal)
    return Authority {..}
  return Uri {..}
```

それでは、その不幸な入力に対してもう一度 `pUri` を実行してみましょう。

```
λ> parseTest (pUri <* eof) "https://mark:@example.com"
scheme> IN: "https://mark:@example.com"
scheme> MATCH (COK): "https"
scheme> VALUE: SchemeHttps

user> IN: "mark:@example.com"
user> MATCH (EOK): <EMPTY>
user> VALUE: Nothing

host> IN: "mark:@example.com"
host> MATCH (COK): "mark"
host> VALUE: "mark"

port> IN: ":@example.com"
port> MATCH (CERR): ':'
port> ERROR:
port> 1:14:
port> unexpected '@'
port> expecting integer

auth> IN: "//mark:@example.com"
auth> MATCH (EOK): <EMPTY>
auth> VALUE: Nothing

1:7:
  |
1 | https://mark:@example.com
  |       ^
unexpected '/'
expecting end of input
```

`megaparsec` の内部で何が起こっているのか正確にわかります。

- `scheme` のマッチに成功します。

- `user` は失敗します。`mark` の所にユーザー名がありますが、`:` の後にパスワードはありません（ここではパスワードを空にしないことを要求します）。失敗し、`try` のおかげでバックトラックします。

- `host` は `user` と同じ場所から開始し、入力をホスト名として解釈しようとします。これは成功し、ホスト名として `mark` を返すことがわかります。

- `host` の後にポート番号があるかもしれないので、`port` は機会を得ます。それは `:` を見ますが、その後に整数がないので `port` は失敗します。

- そのため、`auth` パーサ全体が失敗します（`port` は `auth` の内側にあり、失敗しました）。

- `auth` パーサは、何もパースできなかったため、`Nothing`を返します。`eof` は入力の終わりに達したことを要求しますが、そうではないので、最終的なエラーメッセージが表示されます。

何をすべきでしょうか？これは、`try` を使用してコードの大部分を囲むと、
パースエラーが悪化する可能性がある場合の例です。
パースしたい構文をもう一度見てみましょう。

```
scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
```

私たちは何を探していますか？
パースのある特定の分岐にコミットできるようにするための何か。
`:` を見たときにポート番号が続かなければならないポートのように。
注意深く見れば、二重スラッシュ`//`が、URIに認証情報の部分があることを示す記号であることがわかります。
`//` のマッチはアトミックパーサ（`string`）が使われていることにより、
マッチは自動的にバックトラックするので、
`//` にマッチした後は恐れずに、認証情報の部分を要求することができます。
最初の`try`を`pUri`から削除しましょう。

```haskell
pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme
  void (char ':')
  uriAuthority <- optional $ do -- この行から try を削除した
    void (string "//")
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal)
    return Authority {..}
  return Uri {..}
```

これで、より良いパースエラーを得られるようになりました。

```
λ> parseTest (pUri <* eof) "https://mark:@example.com"
1:14:
  |
1 | https://mark:@example.com
  |              ^
unexpected '@'
expecting integer
```

まだ少し誤解を招くようですが、まあ、それは私が選んだトリッキーな例です。
たくさんの`optional`。

<a name="Label"></a>

## ラベル付けと隠蔽

時には期待されるアイテムのリストがかなり長くなるかもしれません。
認識されていないスキームを使用しようとしたときに得られるものを覚えていますか？

```
λ> parseTest (pUri <* eof) "foo://example.com"
1:1:
  |
1 | foo://example.com
  | ^
unexpected "foo://"
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
```

`megaparsec` は、一般的に*ラベル*と呼ばれるカスタムで、期待されるアイテムを上書きする方法を提供します。これは、`label` プリミティブ(`(<?>)` 演算子の形式のシノニムを持つ)を使用して行われます。

```
pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme <?> "valid scheme"
  -- 残りの部分は同じ
```

```
λ> parseTest (pUri <* eof) "foo://example.com"
1:1:
  |
1 | foo://example.com
  | ^
unexpected "foo://"
expecting valid scheme
```

エラーメッセージを読みやすくするために、ラベルを追加します。

```
pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme <?> "valid scheme"
  void (char ':')
  uriAuthority <- optional $ do
    void (string "//")
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar <?> "username"
      void (char ':')
      password <- T.pack <$> some alphaNumChar <?> "password"
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
    authPort <- optional (char ':' *> label "port number" L.decimal)
    return Authority {..}
  return Uri {..}
```

例:

```
λ> parseTest (pUri <* eof) "https://mark:@example.com"
1:14:
  |
1 | https://mark:@example.com
  |              ^
unexpected '@'
expecting port number
```

もう1つのプリミティブは `hidden` と呼ばれます。
`label` が名前の変更であることに対し、
hiddenは単にそれらを完全に削除します。
比較しましょう。

```
λ> parseTest (many (char 'a') >> many (char 'b') >> eof :: Parser ()) "d"
1:1:
  |
1 | d
  | ^
unexpected 'd'
expecting 'a', 'b', or end of input

λ> parseTest (many (char 'a') >> hidden (many (char 'b')) >> eof :: Parser ()) "d"
1:1:
  |
1 | d
  | ^
unexpected 'd'
expecting 'a' or end of input
```

エラーメッセージのノイズを少なくすることが望ましい場合は、
`hidden` を使用してください。
例えば、プログラミング言語をパースするときは、通常、各トークンの後に空白文字がある可能性があるため、"expecting white space" というメッセージを削除することをお勧めします。

演習 : `pUri` パーサを完成させることは読者のための課題として残されています。完成に必要なすべてのツールは説明されました。

<a name="Run"></a>

## パーサの実行

パーサを構築する方法を詳細に調べました。
しかし、 `parseTest` を除いて、
それらを実行できる関数を調べていませんでした。

慣習的に、あなたがプログラムしたパーサを実行するための
"デフォルト"の関数は `parse` でした。
しかし、`parse` は実際には `runParser` のシノニムです。

```haskell
runParser
  :: Parsec e s a -- ^ 実行するパーサ
  -> String     -- ^ ソースファイルの名前
  -> s          -- ^ パーサへの入力
  -> Either (ParseErrorBundle s e) a
```

2番目の引数は、生成されたパースエラーに含まれる単なるファイル名です。
実際の入力は関数の3番目の引数として渡されるため、
`megaparsec` はそのファイルから何も読みません。

`runParser` を使用すると、`Parsec` モナドを実行できます。
これは、既にご存知のとおり、変換子を使わないバージョンの `ParsecT` です。

```haskell
type Parsec e s = ParsecT e s Identity
```

`runParser` には、`runParser'`、`runParserT`、
および `runParserT'` の3つの姉妹がいます。
接尾辞 `T` の付いたバージョンは `PrasecT` モナド変換子を実行し、
「プライム」バージョンはパーサの状態を受け取り、返します。
すべての関数を表にまとめましょう。

| 引数             | `Parsec` の実行 | `ParsecT` の実行 |
|:-----------------|:----------------|:-----------------|
| 入力とファイル名 | `runParser`     | `runParserT`     |
| カスタム初期状態 | `runParser'`    | `runParserT'`    |


タブの幅を標準以外の値(デフォルトの値は8)に設定したい場合など、
カスタム初期状態が必要な場合があります。
`runParser'` はこのようになっています。

```
runParser'
  :: Parsec e s a -- ^ 実行するパーサ
  -> State s    -- ^ 初期状態
  -> (State s, Either (ParseErrorBundle s e) a)
```

手動で状態を変更することはライブラリの高度な使用法であり、
ここでは説明しません。

`ParseErrorBundle` とは何かについて疑問に思う場合は、
[この後の章のいずれか](#Error)で説明します。

<a name="MonadParsec"></a>

## `MonadParsec` 型クラス

`megaparsec` のすべてのツールは、
`MonadParsec` 型クラスの任意のインスタンスと連携します。
型クラスは、プリミティブコンビネータ、
つまりすべての `megaparsec` のパーサの基本的な構成要素、
他のコンビネータでは表現できないコンビネータを抽象化します。

プリミティブコンビネータを型クラスに持つことで、
`megaarsec` の `ParsecT` の主要なモナド変換子を、
MTL系のよく知られている変換子にラップして、
モナドスタックのレイヤー間でさまざまな相互作用を実現することができます。
動機をよりよく理解するために、
モナドスタック内のレイヤーの順序が重要であることを思い出してください。
このように `ReaderT` と `State` を組み合わせると、

```haskell
type MyStack a = ReaderT MyContext (State MyState) a
```
外側のレイヤー `ReaderT` はその下のレイヤー `m` の内部構造を検査できません。
`ReaderT` の `Monad` インスタンスはバインディングの戦略を記述しています。

```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Monad (ReaderT r m) where
  m >>= k = ReaderT $ \r -> do
    a <- runReaderT m r
    runReaderT (k a) r
```

実際、`m` について私たちが知っている唯一のことは、
それが `Monad`のインスタンスであり、
したがって `m` の状態はモナディックバインドを介してのみ
`k` に渡すことができるということです。
とにかくそれが `ReaderT` の `(>>=)` から私たちが通常欲しいものです。

`Alternative` 型クラスの `(<|>)` メソッドは異なった働きをします。
それは状態を「分割」し、パーサの2つの分岐はもう接触しません。
そのため、最初の分岐が破棄されるとその状態への変更も破棄され、
2番目の分岐に影響を与えることはできないという意味で
バックトラックした状態になります
(最初の分岐が失敗したときの状態を「バックトラック」します。)。

説明のために、ReaderTのAlternativeの定義を見てみましょう。

```haskell
instance Alternative m => Alternative (ReaderT r m) where
  empty = liftReaderT empty
  ReaderT m <|> ReaderT n = ReaderT $ \r -> m r <|> n r
```

`ReaderT` は「ステートレス」なモナド変換子であり、
(何を持っていない) `ReaderT` 自体に関連するモナドの状態を組み合わせる必要なしに(ここで `m` の `Alternative` インスタンスが役に立ちます。)
実際の作業を内部のモナドに委任するのは簡単なので、
これはすべて非常に素晴らしいことです。

それでは、Stateを見てみましょう。
`State s a` は `StateT s Identity a` の単なるシノニムなので、
`StateT s m` 自体の `Alternative` インスタンスを見てください。

```haskell
instance (Functor m, Alternative m) => Alternative (StateT s m) where
  empty = StateT $ \_ -> empty
  StateT m <|> StateT n = StateT $ \s -> m s <|> n s
```

ここでは、reader のコンテキスト `r` の共有を見たように、
状態 `s` の分割を見ることができます。
ただし、`m s` と `n s` の式はステートフルな結果を生成するため、
モナディック値と一緒に、新しい状態をタプルで返すという違いがあります。
ここでは、`m s` か `n s` のどちらかで進み、
自然にバックトラックを達成します。

`ParsecT` はどうですか？
`State` を `ParsecT` の中に次のように置くことを考えてみましょう。

```
type MyStack a = ParsecT Void Text (State MyState) a
```

`ParsecT` は `ReaderT` よりも複雑で、`(<|>)`の実装にはもっと多くのことが必要です。

- パーサ自体の状態管理
- 起こるべき（適切な）パースエラーのマージ。

`ParsecT` の `Alternative` のインスタンスへの`(<|>)`の実装は、
その基盤となる `State MyState` モナドの `Alternative`インスタンスに
その作業を委任することができないので、`MyState`の分割は発生せず、
バックトラックはありません。

例を挙げて説明しましょう。

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)

type Parser = ParsecT Void Text (State String)

parser0 :: Parser String
parser0 = a <|> b
  where
    a = "foo" <$ put "branch A"
    b = get   <* put "branch B"

parser1 :: Parser String
parser1 = a <|> b
  where
    a = "foo" <$ put "branch A" <* empty
    b = get   <* put "branch B"

main :: IO ()
main = do
  let run p          = runState (runParserT p "" "") "initial"
      (Right a0, s0) = run parser0
      (Right a1, s1) = run parser1

  putStrLn  "Parser 0"
  putStrLn ("Result:      " ++ show a0)
  putStrLn ("Final state: " ++ show s0)

  putStrLn  "Parser 1"
  putStrLn ("Result:      " ++ show a1)
  putStrLn ("Final state: " ++ show s1)
```

これがプログラムを実行した結果です。

```
Parser 0
Result:      "foo"
Final state: "branch A"
Parser 1
Result:      "branch A"
Final state: "branch B"
```

`parser0` を使うと、分岐 `b` が試行されていないことがわかります。
しかしparser1では、`empty` によって失敗し、
成功したのは分岐 `b` であるにもかかわらず、
最終結果（`get` によって返される値）が分岐 `a`から得られることは明らかです。
(パーサの文脈では `empty` は
「即座に失敗し、何が起こったのかについての情報がない」
という意味です。)
バックトラックは発生しません。

パーサーでカスタム状態をバックトラックしたい場合はどうしますか？
`ParsecT` を `StateT` 内にラップすることを許可するならば、それを提供することができます。

```haskell
type MyStack a = StateT MyState (ParsecT Void Text Identity) a
```

`MyStack` で `(<|>)` を使用すると、
使用されるインスタンスは `StateT` のインスタンスになります。

```haskell
StateT m <|> StateT n = StateT $ \s -> m s <|> n s
```

これは状態をバックトラックさせ、
それから残りの作業をその内部モナド`ParsecT` の `Alternative` インスタンスに委任します。
この動作はまさに私たちが望むものです。

```
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)

type Parser = StateT String (ParsecT Void Text Identity)

parser :: Parser String
parser = a <|> b
  where
    a = "foo" <$ put "branch A" <* empty
    b = get   <* put "branch B"

main :: IO ()
main = do
  let p            = runStateT parser "initial"
      Right (a, s) = runParser p "" ""
  putStrLn ("Result:      " ++ show a)
  putStrLn ("Final state: " ++ show s)
```

プログラムは次のように出力します。

```
Result:      "initial"
Final state: "branch B"
```

このアプローチを実行可能にするために、
`StateT` はプリミティブパーサのセット全体をサポートするべきであり、
そうすることで `ParsecT` と同じようにそれを扱うことができます。
言い換えれば、
内部モナドが(MTLの)`MonadWriter`のインスタンスである場合は
`MonadState` だけではなく、`MonadWriter`も
`MonadParsec`のインスタンスである必要があります。

```haskell
instance MonadWriter w m => MonadWriter w (StateT s m) where …
```

確かに、`MonadParsec` の内部インスタンスから `StateT` に
プリミティブを持ち上げることができます。

```haskell
instance MonadParsec e s m => MonadParsec e s (StateT st m) where …
```

`megaparsec`は MTL のすべてのモナド変換子に対して
`MonadParsec` のインスタンスを定義しているので、
ユーザーは変換子を `ParsecT` の内側に挿入したり、
それらの変換子で `ParsecT` をラップしたりして、
モナドスタックの層間で異なる種類の相互作用を実現できます。

<a name="Lexing"></a>

## 字句解析

字句解析は、入力ストリームを整数、キーワード、シンボルなどのトークン
のストリームに変換するプロセスです。
これらは、生の入力を直接パースするよりもパースが容易であるか、
またはパーサジェネレータで生成されたパーサへの入力として期待されます。
字句解析は、`alex`などの外部ツールを使用して別のパスで実行できますが、
`megaparsec`はパーサの一部として、シームレスに字句解析プログラムを書くことを簡単にする関数も提供します。

文字ストリーム用の `Text.Megaparsec.Char.Lexer` と
バイトストリーム用の `Text.Megaparsec.Byte.Lexer` の
2つの字句解析モジュールがあります。
正格な `Text` を入力ストリームとして扱うので
`Text.Megaparsec.Char.Lexer` を使いますが、
`ByteStrings` を使いたい場合はほとんどの関数は
`Text.Megaparsec.Byte.Lexer` にも反映されます。

<a name="White"></a>

## 空白

最初に取り上げる必要があるトピックは、空白の扱いです。
すべてのトークンの前またはすべてのトークンの後に、
一貫した方法で空白を消費することが役立ちます。
Megaparsec の字句解析モジュールは、
「トークンの前に空白を入れず、トークンの後にすべての空白を消費する」
という戦略に従います。

空白を消費するには、*スペースコンシューマ*
と呼ばれる特別なパーサが必要です。
`Text.Megaparsec.Char.Lexer` モジュールは、
一般的なスペースコンシューマを構築するためのヘルパーを提供します。

```haskell
space :: MonadParsec e s m
  => m () -- ^ 空の入力を受け入れない空白文字のパーサ
          -- (e.g. 'space1')
  -> m () -- ^ 行コメントのパーサ (e.g. 'skipLineComment')
  -> m () -- ^ ブロックコメントのパーサ (e.g. 'skipBlockComment')
  -> m ()
```

`space` 関数のドキュメンテーションはそれ自体で非常に包括的ですが、
例を挙げて補足しましょう。

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)
```

いくつかのメモ：

- `Text.Megaparsec.Char.Lexer`は、修飾付きでインポートされることを意図されています。なぜなら、Text.Megaparsec.Charの`space`のように衝突する名前を含んでいるからです。

- `L.space` の最初の引数は空白を拾うために使用されることになっているパーサであるべきです。重要な点は、`L.space`が無限ループに入ってしまうので、空の入力を受け入れてはいけないということです。`space1`は、要件を完全に満たす `Text.Megaparsec.Char` のパーサです。

- `L.space` の2番目の引数は、行コメント、すなわち、与えられたトークンのシーケンスで始まり行の終わりで終わるコメントをスキップする方法を定義します。`skipLineComment` ヘルパーを使用すると、行コメント用の補助パーサを簡単に作成できます。

- `L.space` の3番目の引数は、ブロックコメント、つまりトークンの開始シーケンスと終了シーケンスの間のすべてのものをを受け取る方法を定義します。`skipBlockComment` ヘルパーは、ネストされていないブロックコメントを扱うことができます。ネストされたブロックコメントをサポートする場合は、代わりに `skipBlockCommentNested` を使用してください。

操作的には、`L.space` は3つすべてのパーサを適用できなくなるまで、順番に試行します。つまり、空白がすべて消費されたことになります。
これを知っていれば、文法にブロックコメントや行コメントが含まれていない場合は、`L.space`の2番目または3番目の引数として`empty`を渡すことができます。
`(<|>)` の単位元である `empty` は、`L.space` が次の空白要素のためにパーサを試みるようにします。まさに望んでいたことです。

スペースコンシューマ `sc` を持っているなら、
さまざまな空白関連のヘルパーを定義できます。

```haskell
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc -- (1)

symbol :: Text -> Parser Text
symbol = L.symbol sc -- (2)
```

- `lexeme` は、供給されたスペースコンシューマを使用してすべての末尾の空白を取る、語彙素のラッパーです。

- `symbol` は内部で `string` を使って与えられたテキストにマッチさせ、そして同様にすべての末尾の空白を拾うパーサです。

私たちはすぐにそれがすべて一緒に動作する方法を見ますが、
最初に `Text.Megaparsec.Char.Lexer` からさらに2,3のヘルパーを
導入する必要があります。

<a name="Chara"></a>

## 文字と文字列リテラル

エスケープ規則はさまざまなので、
文字リテラルおよび文字列リテラルのパースは難しい場合があります。
簡単にするために、`megaparsec` は `charLiteral` パーサを提供します。

```haskell
charLiteral :: (MonadParsec e s m, Token s ~ Char) => m Char
```

`charLiteral` の仕事は、Haskellレポートに記述されている文字リテラルの構文に従ってエスケープされる可能性がある単一の文字をパースすることです。
ただし、次の2つの理由から、リテラルを囲む引用符はパースされません。

- ユーザーは文字リテラルの引用方法を制御できる

- そのため、charLiteralを使用して文字列リテラルもパースできる

`charLiteral`の上に構築されたパーサの例をいくつか示します。

```haskell
charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')
```

- `L.charLiteral` を文字リテラルのパーサに変えるには、囲む引用符を追加するだけです。ここではHaskellの構文に従い、シングルクオートを使います。`between`コンビネータは、単純に`between open close p = open *> p <* close`ように定義されます。

- `stringLiteral`はダブルクオートで囲まれた文字列リテラル内の個々の文字をパースするために `L.charLiteral` を使用します。

2番目の関数も、`manyTill` コンビネータを使用しているため興味深いです。

```haskell
manyTill :: Alternative m => m a -> m end -> m [a]
manyTill p end = go
  where
    go = ([] <$ end) <|> ((:) <$> p <*> go)
```

`manyTill` は繰り返しごとにパーサ `end` を適用しようとし、
失敗するとパーサ `p` を実行して `p` の結果をリストに蓄積します。

少なくとも1つのアイテムが存在することを要求する `someTill` もあります。

<a name="Numbers"></a>

## 数字

最後に、非常に一般的なニーズは数値をパースすることです。
整数の場合、10進数、8進数、および16進数の表現で
値をパースできる3つのヘルパーがあります。

```haskell
decimal, octal, hexadecimal
  :: (MonadParsec e s m, Token s ~ Char, Integral a) => m a
```

それらを使うのは簡単です。

```haskell
integer :: Parser Integer
integer = lexeme L.decimal
```

```
λ> parseTest (integer <* eof) "123  "
123

λ> parseTest (integer <* eof) "12a  "
1:3:
  |
1 | 12a
  |   ^
unexpected 'a'
expecting end of input or the rest of integer
```

`scientific` と `float` は整数と小数の文法を受け入れます。
`scientific` は`scientific`パッケージの `Scientific`型を返しますが、
`float` はその結果の型が多相的であり、
`RealFloat` の任意のインスタンスを返すことができます。

```haskell
scientific :: (MonadParsec e s m, Token s ~ Char)              => m Scientific
float      :: (MonadParsec e s m, Token s ~ Char, RealFloat a) => m a
```

例：

```haskell
float :: Parser Double
float = lexeme L.float
```

```
λ> parseTest (float <* eof) "123"
1:4:
  |
1 | 123
  |    ^
unexpected end of input
expecting '.', 'E', 'e', or digit

λ> parseTest (float <* eof) "123.45"
123.45

λ> parseTest (float <* eof) "123d"
1:4:
  |
1 | 123d
  |    ^
unexpected 'd'
expecting '.', 'E', 'e', or digit
```

これらすべてのパーサは符号付き数値をパースしないことに注意してください。
符号付き数値用のパーサを作成するには、
既存のパーサを `signed` コンビネータでラップする必要があります。

```haskell
signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float
```

`signed` の最初の引数(スペースコンシューマ)は、符号と実際の数字の間の空白の消費を制御します。スペースを入れたくない場合は、代わりに`return ()`を渡してください。

<a name="lookAhead"></a>

## `notFollowedBy` と `lookAhead`

実際にはパーサの位置を進めずに入力ストリームの
先読みを実行できる(`try` に加えて)さらに2つのプリミティブがあります。

１つめは `notFollowedBy` と呼ばれるものです。

```
notFollowedBy :: MonadParsec e s m => m a -> m ()
```

引数のパーサが失敗したときにのみ成功し、
入力を消費したり、パーサの状態を変更することはありません。

`notFollowedBy` を使いたいと思うかもしれない例として、
キーワードの構文解析を考えます。

```
pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword)
```

このパーサは問題を抱えています。
マッチさせるキーワードが単に識別子の接頭辞であるならどうでしょうか？
その場合、それは間違いなくキーワードではありません。
したがって、`notFollowedBy` を使用して
そのようなケースを排除する必要があります。

```
pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)
```

もう1つのプリミティブは`lookAhead`です。

```
lookAhead :: MonadParsec e s m => m a -> m a
```

`lookAhead` の引数 `p` が成功すると、`lookAhead p` 全体も成功しますが、
入力ストリーム（およびパーサの状態全体）はそのまま残ります。
つまり、何も消費されません。

これが有用である可能性がある場所の一例は、
すでにパースされた入力に対してチェックを実行し、
失敗または正常に継続することです。
慣用表現として、次のようなコードで表すことができます。

```
withPredicate1
  :: (a -> Bool)       -- ^ パース後の入力の振る舞いの確認
  -> String            -- ^ チェックが失敗した時に表示するメッセージ
  -> Parser a          -- ^ 実行するパーサ
  -> Parser a          -- ^ 振る舞いを確認したパーサの結果
withPredicate1 f msg p = do
  r <- lookAhead p
  if f r
    then p
    else fail msg
```

これは `lookAhead` の使用した例ですが、
チェックが成功した場合に2回解析を実行しており、
良くないことにも注意してください。
ここに `getOffset` 関数を使用した代わりの解決方法があります。

```
withPredicate2
  :: (a -> Bool)       -- ^ パース後の入力の振る舞いの確認
  -> String            -- ^ チェックが失敗した時に表示するメッセージ
  -> Parser a          -- ^ 実行するパーサ
  -> Parser a          -- ^ 振る舞いを確認したパーサの結果
withPredicate2 f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else do
      setOffset o
      fail msg
```

このようにして、入力ストリームの `offset` を
`p` を実行する前の状態に設定してから失敗します。
未消費の残りとオフセットの位置に不一致がありますが、
`fail` を呼び出してすぐに構文解析を終了するので、
この場合は問題になりません。


<a name="Expr"></a>

## 式のパース

「式」とは、
項とそれらの項に適用される演算子から形成される構造を意味します。
演算子は、
異なる優先順位で、前置、中置、後置、左と右の結合にすることができます。
このような構文の例として、学校でよく知られている算術式があります。

```
a * (b + 2)
```

2種類の項、変数(`a` と `b`)と整数(`2`)を見ることができます。
2つの演算子、`*`と`+`もあります。

式のパーサを書くには時間がかかるかもしれません。
これ手助けするために、
`megaparsec` には `Text.Megaparsec.Expr` モジュールが付属しています。
これは、`Operator`データ型と`makeExprParser`ヘルパーの
2つのだけをエクスポートします。

両方ともよくドキュメント化されているので、
このセクションでは文書化を繰り返すことはせず、
代わりに単純だが完全に機能する式のパーサを書くつもりです。

式を表すデータ型を
[AST](https://ja.wikipedia.org/wiki/%E6%8A%BD%E8%B1%A1%E6%A7%8B%E6%96%87%E6%9C%A8)として定義することから始めましょう。

```
data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)
```

`makeExprParser` を使用するには、
項のパーサと演算子テーブルを指定する必要があります。

```
makeExprParser :: MonadParsec e s m
  => m a               -- ^ 項のパーサ
  -> [[Operator m a]]  -- ^ 演算子テーブル, 'Operator'を参照
  -> m a               -- ^ s式のパーサの結果
```

それでは項パーサから始めましょう。
結合性や優先順位のようなものを扱う場合、式をパースするアルゴリズムでは、
項をボックスとみなし、分割できない全体として考えることをお勧めします。
この場合、このカテゴリに分類されるものが3つあります。
変数、整数、および括弧内の式全体です。
前の章の定義を使用して、項のパーサを次のように定義できます。

```
pVariable :: Parser Expr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable = undefined -- TODO
```

`pVariable`、`pInteger`、および`parens`の定義は、
ここまでで問題なく進むはずです。
文法が重ならないので `pTerm` に `try` が必要ないという点も、
ここではとてもラッキーです。

- 開き括弧 `(` がある場合は、括弧内に式が続くことを知っているので、そのブランチにコミットします。

- 文字を見れば、それが識別子の始まりであることがわかります。

- 数字が見えれば、それが整数の始まりであることがわかります。

最後に、`pExpr` を終了するために、
`operatorTable` を定義する必要があります。
型からネストされたリストであることがわかります。
すべての内部リストはサポートしたい演算子のリストです。
それらはすべて同じ優先順位を持っています。
外側のリストは優先順位の降順で並べられているので、
高い位置に演算子のグループを配置するほど、
それらはより強く結合されます。

```
data Operator m a -- N.B.
  = InfixN  (m (a -> a -> a)) -- ^ 非結合の中置
  | InfixL  (m (a -> a -> a)) -- ^ 左結合の中置
  | InfixR  (m (a -> a -> a)) -- ^ 右結合の中置
  | Prefix  (m (a -> a))      -- ^ 前置
  | Postfix (m (a -> a))      -- ^ 後置

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation
    , prefix "+" id ]
  , [ binary "*" Product
    , binary "/" Division ]
  , [ binary "+" Sum
    , binary "-" Subtr ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
```

`binary` で `InfixL` 内部の `Parser (Expr -> Expr -> Expr)` を
どのように配置し、
同様に `Parser (Expr -> Expr)` を `prefix` と `postfix`
に配置するか注意してください。
すなわち、`symbol name` を実行し、
`Expr` 型の最終結果を得るために項に適用する関数を返します。

これでパーサーを試すことができます。準備は完了です！

```
λ> parseTest (pExpr <* eof) "a * (b + 2)"
Product (Var "a") (Sum (Var "b") (Int 2))

λ> parseTest (pExpr <* eof) "a * b + 2"
Sum (Product (Var "a") (Var "b")) (Int 2)

λ> parseTest (pExpr <* eof) "a * b / 2"
Division (Product (Var "a") (Var "b")) (Int 2)

λ> parseTest (pExpr <* eof) "a * (b $ 2)"
1:8:
  |
1 | a * (b $ 2)
  |        ^
unexpected '$'
expecting ')' or operator
```

`Text.Megaparsec.Expr` モジュールのドキュメントには、
あまり標準的ではない状況で役立つヒントがいくつか含まれているので、
それを読むことをお勧めします。

<!-- <a name=""></a> -->

<!-- ##  -->

<!-- <a name="Error"></a> -->

<!-- ## Parse errors -->







演習の回答例(`pUri` を完成させる)

```
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Applicative hiding (some,many)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Data.Char
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Uri = Uri
  { uriScheme    :: Scheme
  , uriAuthority :: Maybe Authority
  , uriPath      :: [Text]
  , uriQuery     :: Maybe Text
  , uriFlagment  :: Maybe Text
  } deriving (Eq, Show)

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text) -- (user, password)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

alternatives :: Parser (Char, Char)
alternatives = try foo <|> bar
  where
    foo = (,) <$> char 'a' <*> char 'b'
    bar = (,) <$> char 'a' <*> char 'c'

pPath :: Parser [Text]
pPath = choice
  [ pPathAbempty
  , pPathAbsolute
  , pPathNoScheme
  , pPathRootless
  , pPathEmpty
  ]

pPathAbempty :: Parser [Text]
pPathAbempty = many (char '/' *> pSegment)

pPathAbsolute :: Parser [Text]
pPathAbsolute = do
  void (char '/')
  option [] $ do
    seg <- pSegmentNz
    segs <-  many (char '/' *> pSegment)
    return (seg:segs)

pPathNoScheme :: Parser [Text]
pPathNoScheme = do
  seg <- pSegmentNzNc
  segs <- many (char '/' *> pSegment)
  return (seg:segs)

pPathRootless :: Parser [Text]
pPathRootless = do
  seg <- pSegmentNz
  segs <- many (char '/' *> pSegment)
  return (seg:segs)

pPathEmpty :: Parser [Text]
pPathEmpty = return []

pSegment :: Parser Text
pSegment = T.pack <$> many pPchar

pSegmentNz :: Parser Text
pSegmentNz = T.pack <$> some pPchar

pSegmentNzNc :: Parser Text
pSegmentNzNc = T.pack <$> some (pUnreserved <|> pPctEncoded <|> pSubDelims <|> char '@')

pPchar :: Parser Char
pPchar = pUnreserved <|> pPctEncoded <|> pSubDelims <|> char ':' <|> char '@'

pUnreserved :: Parser Char
pUnreserved = alphaNumChar <|> char '-' <|> char '.' <|> char '_' <|> char '~'

pPctEncoded :: Parser Char
pPctEncoded = do
  void (char '%')
  a <- hexDigitChar
  b <- hexDigitChar
  return . chr $ (digitToInt a)*16 + digitToInt b

pSubDelims :: Parser Char
pSubDelims = choice $ map char "!$&'()*+,;="

pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme <?> "valid scheme"
  void (char ':')
  uriAuthority <- optional $ do
    void (string "//")
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar <?> "username"
      void (char ':')
      password <- T.pack <$> some alphaNumChar <?> "password"
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
    authPort <- optional (char ':' *> label "port number" L.decimal)
    return Authority {..}
  uriPath <- pPath
  uriQuery <- optional (T.pack <$> (char '?' *> many (pPchar <|> char '/' <|> char '?')))
  uriFlagment <- optional (T.pack <$> (char '#' *> many (pPchar <|> char '/' <|> char '?')) <?> "flagment")
  return Uri {..}
```
