---
title: 'xml-conduit'
published: 2021/02/23
---

# xml-conduit

多くの開発者はXMLファイルを扱わなければならないということに対しうんざりしている. XMLは複雑なデータモデルを持ち, わかりにくいライブラリ, 莫大な何層にも重なる複雑性が目標との間にあるという評判がある. 多くの苦痛は実際には言語とライブラリの問題であって, XML固有の問題でないと言いたい.

再び, Haskellの型システムにより問題を最も根本的な形式に分解できる. xml-typesパッケージはXMLデータモデルをきちんと分解し(ストリーミングかつDOMに基づいた方法), 単純なADTにする. Haskellの標準普遍データ構造により, ドキュメントに変更を加えることが容易になり, 単純な関数の集合により, パージング, レンダリングが楽になる. 

xml-conduitパッケージを扱う予定である. 表面下では, このパッケージはYesodが一般的に高パフォーマンスのために行う多くの方法を用いる: blaze-builder, text, conduit, そして, attoparsec. しかし, ユーザの観点からはそれは最も単純なAPI(`readFile`/`writeFile`)からXMLイベントストリームの完全な制御に至るまで, あらゆるものを与えてくれる. 

`xml-conduit`に加え, xml-hamletやxml2htmlなどの関連するパッケージがいくつかある. これら全てのパッケージをどのように使うかについてと, いつ使うべきかについての両者を扱う.

## Synopsis

``` haskell
<!-- Input XML file -->
<document title="My Title">
    <para>This is a paragraph. It has <em>emphasized</em> and <strong>strong</strong> words.</para>
    <image href="myimage.png"/>
</document>
```

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import qualified Data.Map        as M
import           Prelude         hiding (readFile, writeFile)
import           Text.Hamlet.XML
import           Text.XML

main :: IO ()
main = do
    -- readFile will throw any parse errors as runtime exceptions
    -- def uses the default settings
    Document prologue root epilogue <- readFile def "input.xml"

    -- root is the root element of the document, let's modify it
    let root' = transform root

    -- And now we write out. Let's indent our output
    writeFile def
        { rsPretty = True
        } "output.html" $ Document prologue root' epilogue

-- We'll turn out <document> into an XHTML document
transform :: Element -> Element
transform (Element _name attrs children) = Element "html" M.empty
    [xml|
        <head>
            <title>
                $maybe title <- M.lookup "title" attrs
                    \#{title}
                $nothing
                    Untitled Document
        <body>
            $forall child <- children
                ^{goNode child}
    |]

goNode :: Node -> [Node]
goNode (NodeElement e) = [NodeElement $ goElem e]
goNode (NodeContent t) = [NodeContent t]
goNode (NodeComment _) = [] -- hide comments
goNode (NodeInstruction _) = [] -- and hide processing instructions too

-- convert each source element to its XHTML equivalent
goElem :: Element -> Element
goElem (Element "para" attrs children) =
    Element "p" attrs $ concatMap goNode children
goElem (Element "em" attrs children) =
    Element "i" attrs $ concatMap goNode children
goElem (Element "strong" attrs children) =
    Element "b" attrs $ concatMap goNode children
goElem (Element "image" attrs _children) =
    Element "img" (fixAttr attrs) [] -- images can't have children
  where
    fixAttr mattrs
        | "href" `M.member` mattrs  = M.delete "href" $ M.insert "src" (mattrs M.! "href") mattrs
        | otherwise                 = mattrs
goElem (Element name attrs children) =
    -- don't know what to do, just pass it through...
    Element name attrs $ concatMap goNode children
```

``` haskell
<?xml version="1.0" encoding="UTF-8"?>
<!-- Output XHTML -->
<html>
    <head>
        <title>
            My Title
        </title>
    </head>
    <body>
        <p>
            This is a paragraph. It has
            <i>
                emphasized
            </i>
            and
            <b>
                strong
            </b>
            words.
        </p>
        <img src="myimage.png"/>
    </body>
</html>
```

## Types

型を分析するためにボトムアップアプローチを取ろう. この章はXMLデータモデル自身の入門書としての役割も果たす. これに完全には慣れていなくても心配しないでください.

Haskellが本当にその力を示す最初の場所は`Name`データ型であると思う. 多くの言語(Javaのような)はnameを適切に表現するのに苦戦している. 問題はnameには実際には3つの要素があることである: ローカルネーム, 名前空間 (任意), 接頭辞 (これもまた任意). 説明のためにいくつかのXMLを見てみましょう:

```
<no-namespace/>
<no-prefix xmlns="first-namespace" first-attr="value1"/>
<foo:with-prefix xmlns:foo="second-namespace" foo:second-attr="value2"/>
```

最初のタグは`no-namespace`のローカルネームを持っているまたは, 名前空間を持たないか接頭辞を持っている. 2つ目のタグ(local name: `no prefix`)もまた, 接頭辞を持たないが, (`first-namespace`)を持つ.しかし, `first-attr`はその名前空間を継承しない: 属性名前空間は常に明示的に接頭辞を設定される必要がある. 

<div class="yesod-book-notice">
名前空間はほとんど常に何らかのURIであるが, そうである必要があるという指定はない何もない.
</div>

3つ目のタグは`with-prefix`のローカルネーム, `foo`接頭辞, `second-namespace`の名前空間を持つ. その属性は`second-attr`ローカルネームと, 同じ接頭辞, 名前空間を持つ. `xmlns`と`xmlns:foo`属性は名前空間仕様の一部であり, 各エレメントの属性とは考えられない. 

そこでネームから必要なものを調べよう: 全てのネームはローカルネームを持ち, 任意に接頭辞と名前空間を持てる. レコード型に単純に当てはまるように見える:

``` haskell
data Name = Name
    { nameLocalName :: Text
    , nameNamespace :: Maybe Text
    , namePrefix    :: Maybe Text
    }
```

XML名前空間の標準では, 2つのネームは同じローカルネームと名前空間を持つ場合, 同一と考えられる. 言い換えると, 接頭辞は重要でないのである. したがって, `xml-types`は接頭辞を無視する`Eq`と`Ord`インスタンスを定義する.

言及すべき最後のクラスインスタンスは`IsString`である. 毎回文章を作るたびに, 手動で`Name "p" Nothing Nothing`と打たなければならないのは非常につまらない. もし, `OverloadedStrings`をオンにすれば, "`p`"は自動的にそれに変換される. さらに, `IsString`インスタンスはClark表記と呼ばれるものを認識し, 中括弧で囲まれた名前空間を前に置くことが可能になる. 言い換えると:

``` haskell
"{namespace}element" == Name "element" (Just "namespace") Nothing
"element" == Name "element" Nothing Nothing
```

## The Four Types of Nodes

XML ドキュメントはネストされたノードのツリーである. 実際に4つの異なる型のノードが許容されている: エレメント, コンテント(すなはち, テキスト), コメント, そしてプロセッシングインストラクションである.

<div class="yesod-book-notice">
最後のものに不慣れかもしれない, それはあまり一般的には用いられない. それは次のようにマークアップされる:

```
<?target data?>
```

プロセシングインストラクション(PIs)には2つの驚くべき事実がある:

- PIsは属性を持たない. しばしば属性を持っているようにみえるプロセシングインストラクションを見るが, 実際にはインストラクションにおいてそのデータに関する規則はない. 

- ドキュメントの最初にある`<?xml ...?>`はプロセシングインストラクションではない. それは単にドキュメントの始まり(XML宣言として知られている)であり, たまたま非常にPIに類似しているだけである. 違いは, `<?xml ...?>`ラインはパースされたコンテンツには現れない点である. 
</div>

プロセシングインストラクションはそれに関係する2つのテキストを持つため(ターゲットとデータ), 単純なデータ型で表せる:

``` haskell
data Instruction = Instruction
    { instructionTarget :: Text
    , instructionData :: Text
    }
```

コメントは特別なデータ型は存在しない, なぜならばそれらはただのテキストであるからである. しかし, コンテントは興味深いものである: それはプレーンテキスト, あるいは未解決エンティティ(例えば, `&copyright-statement`;)を持てる. xml-typesはすべてのデータ型におけるこれらの未解決エンティティを保持し, スペックに適合するようにしている. しかし, 実践的にはそれらのデータ型に対しプログラムを組むことは非常につまらない. そして, 大部分の使用において, 未解決エンティティは結局エラーになるだけである. 

したがって, `Text.XML`モジュールはノード, エレメント, そしてドキュメントに対し, 全ての未解決エンティティを除いた独自のデータ型セットを持っている. もし代わりに未解決エンティティを扱いたければ, `Text.XML.Unresolved`モジュールを用いるべきである. 今から, `Text.XML`データ型のみに焦点を当てることにする. しかし, それらはxml-typesのバージョンとほとんど同じである.

ところで, 回り道の後: コンテントはただのテキストであり, したがってそれも特別なデータ型を持たない. 最後のノード型はエレメントであり, 3つの情報を含む: ネーム, 属性名/値ペアのマップ, そして子ノードのリストである.(`xml-types`において, この値も同様に未解決エンティティを含みうる) したがって, `Element`は次のように定義される:

``` haskell
data Element = Element
    { elementName :: Name
    , elementAttributes :: Map Name Text
    , elementNodes :: [Node]
    }
```

これはもちろん質問をされる: `Node`はどのようなものなのか? これはHaskellが最も輝く部分である: その直和型はxmlデータモデルを完璧にモデル化する.

``` haskell
data Node
    = NodeElement Element
    | NodeInstruction Instruction
    | NodeContent Text
    | NodeComment Text
```

## Document

したがって今やエレメントとノードがあるが, ドキュメント全体についてはどうであろうか? データ型を並べてみよう:

``` haskell
data Document = Document
    { documentPrologue :: Prologue
    , documentRoot :: Element
    , documentEpilogue :: [Miscellaneous]
    }

data Prologue = Prologue
    { prologueBefore :: [Miscellaneous]
    , prologueDoctype :: Maybe Doctype
    , prologueAfter :: [Miscellaneous]
    }

data Miscellaneous
    = MiscInstruction Instruction
    | MiscComment Text

data Doctype = Doctype
    { doctypeName :: Text
    , doctypeID :: Maybe ExternalID
    }

data ExternalID
    = SystemID Text
    | PublicID Text Text
```

XMLスペックによるとドキュメントは単一のルートエレメント(`documentRoot`)を持つ. それは任意のdoctype宣言も持つ. doctypeとルートエレメント両方の前と後に, コメントとプロセシングインストラクションを持てる. (空白も用いることができるが, それはパースにおいて無視される.)

したがって, doctypeについてはどうであろうか? ええ, それはドキュメントのルートエレメントと, 任意のパブリックなシステム識別子を指定する. これらはDTDファイルを参照するために使われ, ファイルに関しより多くの情報を与える(例えば, 検証規則, デフォルト属性, エンティティ解決): いくつかの例を見てみよう:

``` haskell
<!DOCTYPE root> <!-- no external identifier -->
<!DOCTYPE root SYSTEM "root.dtd"> <!-- a system identifier -->
<!DOCTYPE root PUBLIC "My Root Public Identifier" "root.dtd"> <!-- public identifiers have a system ID as well -->
``` 

そして, それはXMLデータモデル全体である. 多くのパーズ目的のために, 単純に`Document`型全体を無視し, すぐに`documentRoot`に進むことができる. 

## Events

ドキュメントAPIに加え, `xml-type`はEventデータ型を定義する. これはストリーミングツールを構築するために用いられ, ある種の処理(例えば, 追加属性を全てのエレメントに与える)において, 非常にメモリ効率が良くなる. 今はストリーミングAPIについては言及しないが, ドキュメントAPIを分析した後であれば, 非常に馴染み深いものになるであろう.

<div class="yesod-book-notice">
ストリーミングAPIの例については, Sphinxケーススタディで見れる.
</div>


## Text.XML

xml-conduitへの推奨される入り口は, Text.XMLモジュールである. このモジュールはXMLをDOM形式で操作するために必要な全てのデータ型と同様に, XMLコンテントをパーズしレンダリングするための多くの異なる方法をエクスポートする. 最も簡単なものから始めよう:

``` haskell
readFile  :: ParseSettings  -> FilePath -> IO Document
writeFile :: RenderSettings -> FilePath -> Document -> IO ()
``` 

これは`ParseSettings`と`RenderSettings`データ型を導入する. これらを使って, パーサやレンダラの挙動を変更し, 例えば文字エンティティを追加したり, プリティ(すなはち, インデントされた)出力にしたりできる. しかしこれらの型はDefault型クラスのインスタンであるため, 必要な場合は単純にdefを使えばいい. 残りの章を通して, そのようにしてこれらの値を与えることにする; より詳細についてはAPI docsを参照せよ.

ファイルに基づいたAPIの他に, テキストやバイト文字列に基づいたAPIもあることを指摘することは有益である. バイト文字列により関数はすべて賢いエンコード探知を行い, UTF-8, UTF-16そしてUTF-32をbig endianでもlittle endianでも, Byte-Order Marker(BOM)があってもなくてもサポートする. 

複雑なデータ検索においては, 高レベルのカーソルAPIを用いる. 標準Text.XML APIはその高レベルの基礎を形成するだけでなく, 単純なXML変換やXML生成のための素晴らしいAPIである. 例はsynopsisを見よ.

## A note about file paths

上の型注釈において, `FilePath`型がある. しかし, これはPrelude.FilePathではない. 標準`Prelude`は型シノニム `type FilePath = [Char]`を定義する. 残念なことに, このような方法を取ることには, ファイル名の文字エンコーディングやパスセパレータの相違を含む多くの制限がある. 
代わりに, xml-conduitはsystem-filepathパッケージを用い, これは抽象的な`FilePath`型を定義する. 個人的にはこれは作業するのにずっとよい方法であると思う. そのパッケージは非常に使いやすいので, ここでは詳細には触れない. しかし, 使い方について少し簡単な説明を与えたい:

- `FilePath`は`IsString`のインスタンスであるため, 通常の文字列でタイプでき, `OverloadedString`拡張が有効である限り, それらは適切に扱われる. (とりあえずそれを有効にすることをかなり推奨する. なぜならばそれにより`Text`値をずっと快適に扱えるようになるからである. )

- もし明確に`Prelude`の`FilePath`へ, あるいはそこから変換する必要がある場合, それぞれencodeString, decodeStringを用いるべきである. これはファイルパスエンコーディングを考慮する. 

- 手動でディレクトリ名や拡張子を持ったファイル名をつなぎ合わせる代わりに, `Filesystem.Path.CurrentOS`モジュールのオペレータを用いよ, 例えば, `myfolder </> filename <.> extension`.

## Cursor

XHTMLドキュメントからタイトルを抜き出したいとしよう. ちょうど説明した`Text.XML`インターフェースを用いて, エレメントの子に対しパターンマッチを行えば可能である. しかしそれはすぐに, 非常につまらないものになる. この種の検索における定石はXPathであり, `/html/head/title`と書ける. そしてこれはまさにText.XML.Cursorのコンビネータの設計の動機づけとなったものである. 

カーソルはXMLのノードでありツリーにおけるその場所を把握している: それは上, 横, 下に横断することができる. (表面化ではこれは[tying the knot](https://wiki.haskell.org/Tying_the_Knot)により達成される.) カーソルを`Text.XML`型から作るのに必要な2つの関数がある: `fromDocument`と`fromNode`である.

Axisという概念もあり, `type Axis = Cursor -> [Cursor]`で定義される. それはaxisの例を見るところから始めるのが最も簡単である: 子は現在の子の0以上のカーソルを返す, 親は入力に対する単一の親カーソルを返すか, 入力がルートエレメントであれば空リストを返す, などである.

さらに, 述語を取るaxisも存在する. `element`は一般的に用いられる関数であり, 与えられた名前に一致する唯一のエレメントまでフィルタリングする. 例えば, `element "title"` はもしその名前が"title"であれば入力エレメントを返し, そうでなければ空リストを返す. 

他の一般的に用いられる関数でaxisでないものは`content :: Cursor -> [Text]`である. すべてのコンテンツノードのうち, 含まれるテキストを返す: そうでなければ, 空リストを返す.

そして, リストのモナドインスランスにより, これら全てをつなぎ合わせることが可能である. 例えば, タイトルの検索を行うために, 次のようなプログラムを書く:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T

main :: IO ()
main = do
    doc <- readFile def "test.xml"
    let cursor = fromDocument doc
    print $ T.concat $
            child cursor >>= element "head" >>= child
                         >>= element "title" >>= descendant >>= content
```

これが言っていることは次のようである:

1. ルートエレメントのすべての子ノードを取得する.

2. "head"と名付けられたエレメントだけをフィルタリングする.

3. それらすべてのヘッドエレメントから全ての子を取得する.

4. "title"と名付けられたエレメントだけフィルタリングする. 

5. そのタイトルエレメントの全ての子孫を得る. (子孫は子, あるいは子の子孫である. ええ, これは再帰的な定義である. )

6. テキストノードのみを取得する.

よって, 次のような入力ドキュメントに対し:

``` 
<html>
    <head>
        <title>My <b>Title</b></title>
    </head>
    <body>
        <p>Foo bar baz</p>
    </body>
</html>
```

出力は`My Title`を得る. これはすべて問題なく良いのだが, XPathによる解決法よりもずっと冗長である. この冗長さに対応するために, Aristid Breitkreuzは一連の演算子をCursorモジュールに追加し, 多くの共通のケースを処理した. その結果, 例を次のように書き直せる:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T

main :: IO ()
main = do
    doc <- readFile def "test.xml"
    let cursor = fromDocument doc
    print $ T.concat $
        cursor $/ element "head" &/ element "title" &// content
```

`$/`は右のaxisを左のcursorの子に適用することを意味する. `&/`はほとんど同じであるが, 代わりに2つのaxisを結合するために用いられる. これは, `Text.XML.Cursor`における一般的な規則である: $で始まる演算子はaxisを適用する. 一方, `&`は2つを結合する. `&//`はaxisを全ての子孫に適用するために用いられる.

より複雑で技巧的な例を見てみましょう. 次のようなドキュメントがあるとする:

```
<html>
    <head>
        <title>Headings</title>
    </head>
    <body>
        <hgroup>
            <h1>Heading 1 foo</h1>
            <h2 class="foo">Heading 2 foo</h2>
        </hgroup>
        <hgroup>
            <h1>Heading 1 bar</h1>
            <h2 class="bar">Heading 2 bar</h2>
        </hgroup>
    </body>
</html>
```

全ての`h1`タグにおけるコンテンツで, `h2`タグに先行し, `class`属性の"bar"を持つものを取得したいとする. この畳み込みの検索を行うために, 次のように書く:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T

main :: IO ()
main = do
    doc <- readFile def "test2.xml"
    let cursor = fromDocument doc
    print $ T.concat $
        cursor $// element "h2"
               >=> attributeIs "class" "bar"
               >=> precedingSibling
               >=> element "h1"
               &// content
```

段階的に見てみよう. まずドキュメントにおける全てのh2エレメントを取得する. (`$//`はルートエレメントにおける全ての子孫を取得する.) そして, `class=bar`のもののみをフィルタリングする. `>=>`演算子は実際にはControl.Monadからの標準演算子である; リストのモナドインスタンスであることのさらに別の利点である. `precedingSibling`はノードの前に来て同じ親を共有する全てのノードを見つける. (また`preceding`axisはツリーにおける全ての前のノードを取る.) そして`h1`エレメントのみを取得し, コンテンツを取る.

<div class="yesod-book-notice">
同等のXpathは比較のため, `//h2[@class = 'bar']/preceding-sibling::h1//text()`である.
</div>

cursor APIはXPathにおいてはあまり簡潔ではないが, 標準Haskellコードであることと, 型安全であるというメリットがある. 

## xml-hamlet

Haskellのデータ型の簡潔性により, `Text.Xml API`を持つコンテンツを作ることは, 多少冗長だが, 容易である. 次のコードは:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import           Data.Map (empty)
import           Prelude  hiding (writeFile)
import           Text.XML

main :: IO ()
main =
    writeFile def "test3.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "html" empty
        [ NodeElement $ Element "head" empty
            [ NodeElement $ Element "title" empty
                [ NodeContent "My "
                , NodeElement $ Element "b" empty
                    [ NodeContent "Title"
                    ]
                ]
            ]
        , NodeElement $ Element "body" empty
            [ NodeElement $ Element "p" empty
                [ NodeContent "foo bar baz"
                ]
            ]
        ]
```
は以下を生じる.

``` 
<?xml version="1.0" encoding="UTF-8"?>
<html><head><title>My <b>Title</b></title></head><body><p>foo bar baz</p></body></html>
```

これは跳躍であり, 命令的で可変変数に基づいたAPI(Javaなど)を扱うよりも容易に結合できるが, 快適からは程遠く, 実際に達成しようとしていることを不明瞭にする. 物事を簡単にするために, xml-hamletパッケージがあり, これはQuasi-Quotationを用いることで, XMLを自然な構文でタイプすることが可能になる. 例えば, 上は次のように書き直せる:

``` hasekll
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Data.Map        (empty)
import           Prelude         hiding (writeFile)
import           Text.Hamlet.XML
import           Text.XML

main :: IO ()
main =
    writeFile def "test3.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "html" empty [xml|
<head>
    <title>
        My #
        <b>Title
<body>
    <p>foo bar baz
|]
```

いくつかポイントを示しましょう:

- 構文はURL展開(@{..})を除き, 通常のHamletとほとんど同一である.

 - 閉じタグがない.

 - 空白を認識する.

 - 行の最後に空白を持ちたい場合, #を最後に用いる. 最初の場合, バックスラッシュを用いる.

- xml展開は`Node`のリストを返す. したがって, 全ての通常の`Document`とルート`Element`コンストラクタを全てラップする必要がある. 

- 特別な`.class`と`#id`属性形式はサポートされていない.

そして通常のHamletと同様に, 変数展開と制御構造を使うことができる. そこで少し複雑な例としては:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Text.XML
import Text.Hamlet.XML
import Prelude hiding (writeFile)
import Data.Text (Text, pack)
import Data.Map (empty)

data Person = Person
    { personName :: Text
    , personAge :: Int
    }

people :: [Person]
people =
    [ Person "Michael" 26
    , Person "Miriam" 25
    , Person "Eliezer" 3
    , Person "Gavriella" 1
    ]

main :: IO ()
main =
    writeFile def "people.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "html" empty [xml|
<head>
    <title>Some People
<body>
    <h1>Some People
    $if null people
        <p>There are no people.
    $else
        <dl>
            $forall person <- people
                ^{personNodes person}
|]

personNodes :: Person -> [Node]
personNodes person = [xml|
<dt>#{personName person}
<dd>#{pack $ show $ personAge person}
|]
```

少しの注意点:

- キャレット展開(^{...})はノードのリストを取り, その結果容易に他の`xml-`引用を埋め込むことができる. 

- Hamletと異なり, ハッシュ展開(#{...})は多相的ではない, そして`Text`値のみ許容する. 

## xml2html

この章では今までのところ, 例ではXHTML中心に展開した. そうしたのは, 単純にそれが大部分の読者にとって, 最も馴染み深いXML形式であるためである. しかしこれに関しては認めざるおえない欠点がある: 全てのXHTMLが正しいHTMLとは限らない. 次の相違点が存在する:

- HTMLにはvoidタグ(例えば`img`, `br`)が存在し, これらは閉じタグを必要とせず, そして実際には許容されていない.

- HTMLはself-closingタグを理解できない. そのため, `<script></script>`や`<script/>`は非常に異なるものを意味する.

- 前の2つの点を組み合わせる: 自由にvoidタグをself-closeできるが, ブラウザに対しては何も意味しない. 

- 互換モードを避けるために, HTMLドキュメントを`DOCTYPE`宣言で開始すべきである.

- HTMLページのトップにXML宣言`<?xml ...?>`は来て欲しくない.

- XHTMLは完全に名前空間で制御されているが, HTMLでは名前空間を使用したくない.

- `<style>`と`<script>`タグのコンテンツはエスケープすべきでない. 

幸いなことに, `xml-conduit`は`Node`, `Document`, そして`Element`に対し`ToHtml`インスタンスを与え, これらの相違点を尊重する. したがって, 単に`toHtmlを使うだけで, 正しい出力を得ることができる. 

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import           Data.Map                        (empty)
import           Text.Blaze.Html                 (toHtml)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Hamlet.XML
import           Text.XML

main :: IO ()
main = putStr $ renderHtml $ toHtml $ Document (Prologue [] Nothing []) root []

root :: Element
root = Element "html" empty [xml|
<head>
    <title>Test
    <script>if (5 < 6 || 8 > 9) alert("Hello World!");
    <style>body > h1 { color: red }
<body>
    <h1>Hello World!
|]
```

出力: (空白が追加されている)

``` 
<!DOCTYPE HTML>
<html>
    <head>
        <title>Test</title>
        <script>if (5 < 6 || 8 > 9) alert("Hello World!");</script>
        <style>body > h1 { color: red }</style>
    </head>
    <body>
        <h1>Hello World!</h1>
    </body>
</html>
```