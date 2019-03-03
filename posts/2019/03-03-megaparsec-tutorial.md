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

## 文字 とバイナリストリーム


`megaparsec` は、5種類の入力ストリーム（`String`、正格または遅延 `Text` 、 正格または遅延 `ByteStrings`）をそのまま使用できると言われています。
これが可能なのは、ライブラリでこれらの型が 
`Stream` 型クラスのインスタンスにしているからです。
`Stream` 型クラスはそれぞれのデータ型が
`megaparsec` のパーサへの入力として使用されるために
サポートすべき関数を抽象化したものです。

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


<!-- <a name=""></a> -->

<!-- ##  -->

