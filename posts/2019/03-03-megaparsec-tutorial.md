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
- Debugging parsers
- Labelling and hiding things
- Running a parser
- The MonadParsec type class
- Lexing
  - White space
  - Char and string literals
  - Numbers
- notFollowedBy and lookAhead
- Parsing expressions
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

<!-- <a name=""></a> -->

<!-- ##  -->

