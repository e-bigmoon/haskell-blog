---
title: Haskell
published: 2018/09/11
updated: 2021/02/13
---

Haskell は強力で速く、型安全なプログラミング言語です。本書は、Haskell の基礎についてある程度理解している読者を対象としています。Haskell に馴染みが無い読者には、オンラインで読むことができる素晴らしい入門書が2冊あるので、そちらをご紹介します。

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/read/)

さらに [School of Haskell](https://www.fpcomplete.com/introduction-to-haskell) にも良い記事がいくつもあります。

Yesod を使うためには、少なくとも Haskell の基本的な部分について理解しておく必要があります。また、それだけでなく、入門書では扱わないような Haskell の機能についても Yesod では扱うことになります。この章では、本書で想定している Haskell の基礎知識と読者の知識とのギャップを確認しておこうと思います。

もし、Haskell にすごく詳しければ、この章は完全にスキップしてもらって構いません。また、とりあえず Yesod を触ってみたいと思う人は、後からこの章に戻ってこれば良いので、好きに動かしてみてください。

## ことばについて

自分の手足のように Haskell を扱う人でさえも、Haskell の用語についてはたまに混乱してしまうことがあります。まずは、本書で扱う用語について確認していきましょう。

### データ型

これはHaskellのような, 強く型付けされた言語における中心的な構成要素の1つである. `Int`のようなデータ型は, プリミティブな値として扱われ, 他の型はこれらに基づきより複雑な値を生成する. 例えば, personを次のように表現できる:

``` haskell
data Person = Person Text Int
```

ここでは, `Text`はpersonの名前を表し, `Int`はpersonの年齢を与える. 簡便性により, この特別な型の例は本全体で用いられる. 本質的には新しいデータ型を作るための3つの方法がある:

- `type GearCount = Int`のような`type`宣言は単に既存の型の同義語である. 型システムは`GearCount`を求められるところで`Int`を用いることを妨げはしない. これを用いることで, コードをより自己文書化できる.

- `newtype Make = Make Text`のような`newtype`宣言. この場合, `Make`の場所で誤って`Text`を用いることはできない; コンパイラに止められてしまう. `newtype`ラッパはコンパイル時には常に消えてしまうので, オーバーヘッドはない.

- 上の`Person`のような`data`宣言. `data Vehicle = Bicycle GearCount | Car Make Model`のような代数的データ型(ADTs)も作ることができる. 

### データコンストラクタ

上記の例だと、`Person`、`Make`、`Bicycle`、`Car` などがデータコンストラクタです。

### 型コンストラクタ

上記の例だと、`Person`、`Make`、`Vehicle` が型コンストラクタです。

### 型変数

`data Maybe a = Just a | Nothing` で考えると、`a` が型変数です。

<div class="yesod-book-notice">
<code>Person</code> と <code>Make</code>型はデータ型とデータコンストラクタで同じ名前を共有しています。これはデータコンストラクタが1つしかない場合の良くある慣習のようなものです。言語が強制するものではないので、データ型とデータコンストラクタに異なる名前を付けることもできます。
</div>

## ツール

2015年7月から, Yesodのツール推奨は非常に単純になった: [stack](https://github.com/commercialhaskell/stack#readme)を用いてください. stackはHaskellにおける完全なビルドツールであり, コンパイラ(Glasgow Haskell Compiler, 別名 GHC), ライブラリ(Yesodを含む), 追加的なビルドツール(alexやhappy), そしてより多くのものを扱っている. Hakellには他にも利用可能なビルドツールが存在し, 大部分はYesodを非常によくサポートしている. しかし最も容易にするためには, stackを堅持することを推奨する. yesodウェブサイトには最新の[クイックスタートガイド](http://www.yesodweb.com/page/quickstart)があり, stackをインストールしたり新しいscaffoldサイトを開始するための説明がある. 

一度道具一式を正しくセットアップした後は, 多くのHaskellライブラリをインストールする必要がある. この本の大部分では, 次のコマンドにより必要なすべてのライブラリがインストールされる::

```
stack build classy-prelude-yesod persistent-sqlite
```

本書の例を実行するために, それを例えばyesod-example.hsのようなファイルに保存し, 次のように実行してください:

```
stack runghc yesod-example.hs
```

## 言語プラグマ

GHCはデフォルトではHaskell98に非常に近いモードで実行される. それは多くの言語拡張も備え付けており, より強力な型クラス, 構文変更, そしてより多くのことを可能にしてくれる. GHCにこれらの拡張を有効にする方法は数多く存在する. この本における大部分のコードにおいては, 次のような言語プラグマを見るであろう:

```
{-# LANGUAGE MyLanguageExtension #-}
```

これらは常にソースファイルの先頭にあるべきである. さらに, 他にも2つの他の一般的な方法がある:

- GHCコマンドラインで, 追加的な引数`-XMyLanguageExtension`を渡す. 

- `cabal`ファイルにおいて, `default-extensions`ブロックを追加する.

個人的にはGHCコマンドライン引数による方法は決して用いない. これは個人的は趣向だが, ファイルの中で設定を明示的に宣言したいためである. 一般的に拡張を`cabal`ファイルに書かないことが推奨される; しかし, この規則は大方パブリックに利用可能なライブラリに対し当てはまる. あなたとチームが作業するためのアプリケーションを書く際は, すべての言語拡張を1つの場所に定義しておくことはとても有益なことである. Yesodのscaffoldサイトは特にこの方法を用いて, 同じ言語プラグマをそれぞれのソースファイルで指定するというボイラープレートを回避している. この本ではかなり多くの言語拡張を用いることになる(scaffoldingを書く際には, 13個用いる). これらの全ての意味について触れるわけではない. 代わりに, [GHC文書](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/)を見てください. 

### Overloaded Strings

"hello"の型は何であろうか? 伝統的には, それは`String`であり, `type String = [Char]`として定義される. 残念なことに, これに関しては多くの限界がある:

- これは非常に非効率的なテキストデータの実装である. 各consセルのための余剰メモリに加えて, それぞれの文字自身が完全な機械言語になる.

- 時に`ByteString`やHTMLのように, 実際にはテキストではないstringのようなデータが存在する. 

これらの制限を対処するために, GHCは`OverloadedStrings`という言語拡張を持つ. これを有効にすればリテラル文字列はもはや単層型`String`ではない; 代わりに, それは`IsString a => a`という型を持ち, `IsString`は次のように定義される:

``` haskell
class IsString a where
    fromString :: String -> a
```

`Text`(ずっと効率的にパックされた`String`型), `ByteString`, そして`Html`などのように, Haskellの多くの型で利用可能な`IsString`インスタンスがある. 事実上この本における全ての例では, この言語拡張が有効にされていると仮定する. 

残念なことに, この拡張に関しては1つの欠点が存在する: 時々GHCの型チェックを混乱させるのである. 次のように想定しましょう: 

```
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
import Data.Text (Text)

class DoSomething a where
    something :: a -> IO ()

instance DoSomething String where
    something _ = putStrLn "String"

instance DoSomething Text where
    something _ = putStrLn "Text"

myFunc :: IO ()
myFunc = something "hello"
```
プログラムは`String`あるいは`Text`のどちらを出力するのでしょうか? それは明らかではない. したがって, 明示的な型注釈を与え, "`hello`"が`String`あるいは`Text`として扱われるのかを指定する必要がある. 

<div class=yesod-book-notice>
ある場合にはこれらの問題は`ExtendedDefaultRules`言語拡張を用いることで克服される. しかしこの本においては代わりに明示的な方法を用い, デフォルトに頼らないことにする.
</div>

### Type Families

タイプファミリの基本的なアイデアとしては2つの異なる型に対する関連を述べることである. リストの最初の要素を安全に取るための関数を書くことしましょう. しかしリストだけに制限したくはない; `ByteString`も`Word8`のリストとして扱って欲しい. そうするために, 関連型を導入し, ある型のコンテンツが何であるかを指定したい. 

``` haskell
{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
import Data.Word (Word8)
import qualified Data.ByteString as S
import Data.ByteString.Char8 () -- get an orphan IsString instance

class SafeHead a where
    type Content a
    safeHead :: a -> Maybe (Content a)

instance SafeHead [a] where
    type Content [a] = a
    safeHead [] = Nothing
    safeHead (x:_) = Just x

instance SafeHead S.ByteString where
    type Content S.ByteString = Word8
    safeHead bs
        | S.null bs = Nothing
        | otherwise = Just $ S.head bs

main :: IO ()
main = do
    print $ safeHead ("" :: String)
    print $ safeHead ("hello" :: String)

    print $ safeHead ("" :: S.ByteString)
    print $ safeHead ("hello" :: S.ByteString)
```

新しい構文としては, `class`や`instance`の中に`type`を置くことができることです. 代わりに`deta`を用いることもでき, その場合は既存のものを参照する代わりに新しいデータを作ることができる. 

<div class=yesod-book-notice>
型クラスの文脈の外で関連型を用いる方法も存在する. より詳細については, [Haskell wiki page](http://www.haskell.org/haskellwiki/GHC/Type_families)を参照してください.
</div>

### Template Haskell

Template Haskell[TH]はコード生成のための方法である. Yesodにおいては多くの場所で用いられ, ボイラープレートを減らしたり, 生成されたコードが正確なことを保証してくれる. Template Haskellは本質的にはHaskell Abstract Syntax Tree(AST)を生成するためのHaskellである. 

<div class=yesod-book-notice>
実際にはTHにはそれ以上に強力な力がある. なぜならばそれは実際にコードをイントロスペクトできるためである. しかしYesodにおいてこれらの機能は用いない.
</div>

THコードを書くことは技巧的であり, 残念なことにあまり型安全性は関係しない. コンパイルできないコードを生成するTHを書きがちである. これは単にYesod開発者の問題であり, ユーザの問題ではない. 開発中は, かなり多くの単体テストを用いて生成されたコードの正確性を確認する. ユーザとして必要なことは, これらのすでに存在する関数を呼び出すことである. 例えば, 外部に定義されたHamletテンプレートをインクルードするために, 次のように書ける:

```
$(hamletFile "myfile.hamlet")
```

(HamletはShakespeareの章で議論される)ドルマークのすぐ後には括弧が続き, GHCに次に来るものがTemplate Haskell関数であることを伝える. コードの内側はコンパイラにより実行され, Haskell ASTを生成し, さらにコンパイルされる. そうです, [これに関しメタ](http://www.yesodweb.com/blog/2010/09/yo-dawg-template-haskell)することも可能です. 

ここでのよいトリックとしては, THコードは任意の`IO`アクションが可能であり, したがって入力を外部ファイルに書き, それをコンパイル時にパーズすることができる. 1つの使い方の例としては, HTML, CSS, そしてJavascriptテンプレートのコンパイル時チェックを行うことである. 

もしTemplate Haskellコードが宣言を生成するために用いられ, ファイルのトップレベルに置かれていたとしたら, ドルマークと括弧を省略できる. つまり:

``` haskell
{-# LANGUAGE TemplateHaskell #-}

-- Normal function declaration, nothing special
myFunction = ...

-- Include some TH code
$(myThCode)

-- Or equivalently
myThCode
```

Template Haskellにおいてどんなコードが生成されているかを見ることは有益である. そのために, `-ddump-splices` GHCオプションを使ってください:

<div class=yesod-book-notice>
Template Haskellにはここでは扱われない多くの特性がある. 詳細については, <a href=http://www.haskell.org/haskellwiki/GHC/Type_families>Haskell wiki page</a>を参照してください. 
</div>

Template Haskellはstage restrictionとよばれるものを導入している. これは本質的にはTemplate Haskell spliceの前に来るコードは, Template Haskllやその後のコードを参照できないことを意味する. これによりしばしばコードを少し再配備する必要がある. 同じ制約はQuasiQuoteにも当てはまる.

独創的であるが, Yesodはコード生成を用いてボイラープレートを避けるのに本当に適している. YesodをTemplate Haskellを用いない方法で使うことも全く問題ない. それについては"Yesod for Haskeller"の章に詳細がある. 

### QuasiQuotes

QuasiQuote(QQ)はTemplate Haskellのちょっとした拡張であり, Haskellソースファイル内の任意コンテンツを埋め込み可能にする. 例えば, 前に`hamletFile TH`関数について述べたが, それは外部ファイルからテンプレートコンテンツを読む込む. また, 準クォートで囲まれた`hamlet`はコンテンツをインラインで取る:

``` haskell
{-# LANGUAGE QuasiQuotes #-}

[hamlet|<p>This is quasi-quoted Hamlet.|]
```

構文は鍵括弧とパイプで始める. 準クォートの名前は括弧と最初のパイプの間に入り, コンテンツはパイプ間に入る.

この本を通し, しばしばTHによって外部出力される方法よりも, QQによる方法を用いる. なぜならばこの方がコードのコピー&ペーストが楽だからである. しかし, 製品版においては, 短いコンテンツを除きほとんどの場合で外部ファイルが推奨される. なぜならば, それによりHaskellでない構文とHaskellコードがしっかり分離されるためである. 

## API ドキュメント

標準的なAPIドキュメントプログラムはHaddockと呼ばれている. 標準Haddock検索ツールはHoogleである. 個人的には[StackageのHoogle検索](https://www.stackage.org/)と付随するHaddockを検索やドキュメントの閲覧に推奨する. 理由としては, Stackage Hoogleデータベースは非常に多くのオープンソースHaskellパッケージを含み, 提供されるドキュメントは常に完璧に生成され, 他のHaddockへのリンクもあるためである. 

もしこの本を読んでいる際に分からない型や関数に出くわしたら, Hoogle検索をHoogleで行い, 詳細を調べてください.

## まとめ

Yesod を使うために Haskell のエキスパートになる必要はありません。基本的なことについて、ちょっとだけわかっていれば大丈夫です。この章の内容が本書をこれから楽しく読み進めるために役に立つことでしょう.