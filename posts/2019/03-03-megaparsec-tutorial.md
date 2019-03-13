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
- Monadic and applicative syntax
- Forcing consumption of input with eof
- Working with alternatives
- Controlling backtracking with try
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

- すべての型変数を具体的な方に固定して操作すると、GHCの最適化が大幅に向上します。パーサーが多相性を保っている場合、GHCは最適化の観点からそれほど多くのことはできません。`megaparsec` APIは多相ですが、エンドユーザーは具体的にパーサモナドの型を固定することが予想されます。そのため、インライン展開と、ほとんどの関数の定義がインターフェイスファイルと呼ばれるファイルに出力されているという事実により、GHCは非常に効率的な非多相的なコードを生成できます。

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
パーサーは結果を返します。

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
例えば、`single` は特定のトークン値にマッチします。

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

`tokens` に関して定義された2つのパーサーがあります。

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
`string` は大文字と小文字を区別しますが、
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

<!-- <a name=""></a> -->

<!-- ##  -->

