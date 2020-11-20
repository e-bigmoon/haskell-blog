---
title: Basics
published: 2018/03/18
updated: 2020/11/20
---

どんな新しい技術でも、まずはとりあえず動かしてみましょう。

この章の目的は簡単な Yesod アプリケーションを作り、基本的な概念や用語を理解することです。

## Hello World

まずは Hello World を表示する簡単な Web ページの例を見てみましょう。

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 HelloWorld
```

このコードを `helloworld.hs` というファイル名で保存した後に `runhaskell helloworld.hs` コマンドを実行することで、ポート3000番で動く Web サーバが立ち上がります。クイックスタートガイドに従って `stack` で yesod をインストールした場合は `runhaskell` コマンドの代わりに `stack runghc helloworld.hs` コマンドを実行してください。そして、ブラウザで [http://localhost:3000](http://localhost:3000) にアクセスすれば、次の HTML コードが得られるでしょう。

```html
<!DOCTYPE html>
<html><head><title></title></head><body>Hello World!</body></html>
```

本章の残りの部分で、この例を何度か参照します。

## ルーティング

多くのモダンな Web フレームワークと同様に Yesod は [Front controllerパターン](https://en.wikipedia.org/wiki/Front_controller) を採用しています。これは、Yesod アプリケーションへのあらゆるリクエストは一旦同じポイントに入り、そこからルーティングが行われることを意味します。それとは対照的に、PHP や ASP においては、たいてい複数の異なるファイルを作成し、Web サーバが自動的に関連ファイルを直接リクエストします。

さらに、Yesod は宣言的な方法でルート (route) を指定します。先ほどの例では以下の部分が該当します。

```haskell
mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]
```

<div class="yesod-book-notice">
`mkYesod` は Template Haskell 関数、 `parseRoutes` は準クォート (QuasiQuoter) です。
</div>

これを日本語で説明すれば、「Hello World アプリケーションはルートを1つ作成します。 `HomeR` は `/` (アプリケーションのルート (root)) へのリクエストを待機し、そのうちの `GET` リクエストに答えます。」という意味になります。`HomeR` をリソースと呼び、そのため接尾辞 **"R"** はリソースのRです。

<div class="yesod-book-notice">
リソース名における接尾辞Rは単なる慣習ですが、かなり一般的に使われています。これにより、コードを読んだり理解することが少しだけ楽になります。
</div>

`mkYesod` のTH関数は、ルートデータ型、パーサー/レンダー関数、ディスパッチ関数、いくつかの補助的な型など、かなり多くのコードを生成します。ルーティングの章で、それらの詳細について説明しますが、 GHC オプションの `-ddump-splices` を使えば、生成されるコードをすぐに確認することもでき、それを見やすく整理すると次のようになります。

```haskell
instance RenderRoute HelloWorld where
    data Route HelloWorld = HomeR
        deriving (Show, Eq, Read)
    renderRoute HomeR = ([], [])

instance ParseRoute HelloWorld where
    parseRoute ([], _) = Just HomeR
    parseRoute _       = Nothing

instance YesodDispatch HelloWorld where
    yesodDispatch env req =
        yesodRunner handler env mroute req
      where
        mroute = parseRoute (pathInfo req, textQueryString req)
        handler =
            case mroute of
                Nothing -> notFound
                Just HomeR ->
                    case requestMethod req of
                        "GET" -> getHomeR
                        _     -> badMethod

type Handler = HandlerT HelloWorld IO
```

<div class="yesod-book-notice">
`-ddump-splices` オプションを利用するだけでなく、アプリケーションの Haddock ドキュメントを生成して、どんな関数やデータ型が生成されたか確認するテクニックも良く使われます。
</div>

これを見ると、 `RenderRoute` クラスは関連データ型 (***associated data type***) を定義し、アプリケーションのルートを定義していることが分かります。この単純な例では、ルートは `HomeR` １つしかありませんが、現実的なアプリケーションではより多くのルートが生成され、それぞれのインスタンスにおける処理は `HomeR`よりもずっと複雑になるでしょう。

`renderRoute` はルートをパス断片とクエリ文字列パラメータに変換します。今回の例は単純なので、パス断片とクエリ文字列パラメータはどちらも空リストです。

`ParseRoute` クラスは `renderRoute` の逆関数 `parseRoute` を提供します。これが Template Haskell に依存する最初の大きなモチベーションです。なぜなら、ルートのパーシングやレンダリングが互いに正しく対応することを保証するからです。このようなコードを手で書く場合、両者の関数の同期を取ることがすぐに難しくなるため、Template Haskell のコード生成を利用して、コンパイラ (とYesod) にそれらを肩代わりしてもらいます。

`YesodDispatch` はインプットリクエストを受け取り、適切なハンドラ関数に渡す手段を与えます。その処理は本質的には、次のようになります。

1. リクエストをパースする
1. ハンドラー関数を選択する
1. ハンドラー関数を実行する

コード生成は、ルートをハンドラ関数名にマッチさせる単純な形式に従いますが、詳細は次章で説明します。

最後に、単純な型シノニム `Handler` はコードの記述量をほんの少し減らしてくれます。

実際にはここに書いたよりずっと多くのことが起こっています。生成されたディスパッチコードは、効率性のためビューパターン言語拡張を利用し, さらに多くの型クラスのインスタンスが生成されます。そして、その他にサブサイトなどを制御するためのコードも同様に生成されます。これらの詳細は本書全体で解説していますが、特に "リクエストを理解する" の章が詳しいです。

## ハンドラ関数

`HomeR` という名前のルートは `GET` リクエストに応じます。では、どうやってレスポンスを定義すれば良いのでしょうか？それは、ハンドラ関数を書けば良いのです。Yesod はハンドラ関数に標準命名規則を定義しており、小文字のメソッド名 (例えば `GET` は `get` になる) の後にルート名が続くという規則になっています。この場合のハンドラ関数の名前は `getHomeR` となります。

これから書いていく Yesod コードの大部分はハンドラ関数に定義することになります。ハンドラ関数ではユーザの入力処理やデータベースクエリの実行、さらにはレスポンスを作ります。先ほどの例では `defaultLayout` 関数を用いてレスポンスを作りました。この関数はサイトのテンプレートに与えられたコンテンツをラップします。デフォルトでは `doctype`、`html`、`head`、`body` タグから成る HTML ファイルを生成します。Yesod 型クラスの章で詳しく説明しますが、この関数をオーバーライドすることで、より複雑な HTML を生成することもできます。

今回の例では `[whamlet|Hello World!|]` を `defaultLayout` に渡しています。`whamlet` は Hamlet 構文をウィジェット (Widget) に変換する準クォートです。Hamlet は Yesod のデフォルト HTML テンプレートエンジンです。また Hamlet 以外にも Cassius、Lucius、Julius があり、これらもまた、完全な型安全かつコンパイル時チェックの方法でそれぞれ HTML、CSS、Javascript を生成します。これらの詳細についてはシェイクスピアの章を参照してください。

ウィジェットは Yesod の中でとても基本的な概念の1つでです。サイトを構成するための HTML、CSS、Javascript をモジュラーコンポーネントとして生成でき、それらはサイト全体で再利用できるようになります。この詳細についてはウィジェットの章で確認することにしましょう。

## ファウンデーション

'HelloWorld' という単語は本書の例で何度も現れます。あらゆる Yesod アプリケーションはファウンデーションデータ型を持ちます。このデータ型は `Yesod` 型クラスのインスタンスでなければなりません。その結果、アプリケーションの実行を制御するために必要ないくつかの異なる設定を宣言するための中央広場のような役割を与えることができます。

今回の場合 HelloWorld 型は何の情報も含んでいないため面白くありません。しかし、そのファウンデーションが、今回の例の実行において核となります。実際にそれはルートをインスタンス宣言と結びつけ、実行につなげる役割を果たします。ファウンデーションは本書全体を通して所々に現れます。

ファウンデーションの面白い例はいくつも考えられるでしょう。例えば、プログラムのスタート時に初期化され、あらゆる部分で用いられるような、多くの有益な情報を格納するために利用する場合などです。一般的な例は以下の通りです。

- データベースコネクションプール
- 設定ファイルから読み込まれる設定値
- HTTPコネクションマネージャー
- 乱数生成器

<div class="yesod-book-notice">
ところで Yesod という言葉はヘブライ語でファウンデーションを意味します。
</div>

## 実行

main 関数における `Hello World` についてもう少し説明しておきます。ファウンデーションはアプリケーションをルーティングし、リクエストに応答するために必要なすべての情報を含んでいます。あとは、ファウンデーションを実際に動く何かに変換するだけです。このための Yesod における有用な関数は `warp` です。この関数は指定されたポート上で動作する Warp ウェブサーバをいくつかのデフォルト設定で起動します。(ここでのポート番号は 3000)。

Yesod の特徴の1つに、単一のデプロイメント戦略に縛られることがないという利点があります。Yesod はウェブアプリケーションインターフェース (WAI) のトップレベルに組み込まれているため、FastCGI、SCGI、Warp またはWebkitライブラリを用いたデスクトップアプリケーションでも利用できるようになっています。これらのオプションについてはデプロイメントの章で確認します。また、本章の終わりで開発用サーバーについて説明します。

Warp は Yesod における最も重要なデプロイメントオプションです。Warp はとりわけ Yesod をホストするために開発された、軽量で高効率なウェブサーバです。また Yesod に限らず Warp は利用可能です。例えば、別の Haskell 開発 (フレームワーク、非フレームワークアプリケーション) や、いくつかのプロダクション環境の標準ファイルサーバとして利用することも可能です。

## リソースと型安全 URL

hello world の例では、リソース (`HomeR`) を1つだけ定義しました。しかし、ウェブアプリケーションでは普通はページが1つ以上ありますよね。次の例はトップページを含め、ページが3つある例です。

```haskell
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data Links = Links

mkYesod "Links" [parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
|]

instance Yesod Links

getHomeR  = defaultLayout [whamlet|<a href=@{Page1R}>Go to page 1!|]
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Go to page 2!|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go home!|]

main = warp 3000 Links
```

全体的にこれは Hello World の例と非常に類似しています。今回のファウンデーションは `HelloWorld` の代わりに `Links` となっています。また `HomeR` リソースの他に `Page1R` と `Page2R` リソース、さらに2つのハンドラ関数 `getPage1R` と `getPage2R` が追加されました。

唯一本当に新しい部分としては `whamlet` 準クォートの中身です。構文に関しては "シェイクスピア (Shakespeare)" の章で掘り下げますが、

```haskell
<a href=@{Page1R}>Go to page 1!
```

上記のコードが `Page1R` リソースへのリンクを作成するということは読み取れるでしょう。ここで注意すべき重要な点としては `Page1R` はデータコンストラクタだという点です。それぞれのリソースをデータコンストラクタにすることで、型安全URL (***type-safe URLs***) の機能を実現できます。URLを生成するために文字列を操作するのではなく、単純にただの Haskell の値を生成します。アットマーク記号の展開 (`@{...}`) を利用することで Yesod はユーザに情報を送る前に、これら Haskell の値を自動的にテキスト形式の URL に置き換えます。型安全URLの実装方法ついては、再び `-ddump-splices` の出力結果を見ることで理解できるでしょう。

```haskell
instance RenderRoute Links where
    data Route Links = HomeR | Page1R | Page2R
      deriving (Show, Eq, Read)

    renderRoute HomeR  = ([], [])
    renderRoute Page1R = (["page1"], [])
    renderRoute Page2R = (["page2"], [])
```

`Links` と関連付けられた型 `Route` は `Page1R` と `Page2R` のルートを扱うための追加的なコンストラクタを持ちます。今回の場合 `renderRoute` の戻り値は HelloWorld の例よりも多くのことを教えてくれます。タプルの第一要素は与えられたルートにおけるパス断片であり、第二要素はクエリ文字列パラメータです。多くの場合、クエリ文字列パラメータは基本的には空リストとなるでしょう。

型安全URLを過剰に評価しているわけではありません。なぜなら、型安全URLはリンクを切らさずに URL を変更することが可能になるといった、アプリケーション開発においてかなりの柔軟さと堅牢さを与えるからです。ルーティングの章では、ルートがブログポスト ID とブログエントリ URL の関係のようなパラメータを取る例を学びます。

もしかすると、数値表現のブログポスト ID のルーティングから 年/月/スラグ 形式に切り替えたいと思う読者がいるかもしれません。従来のウェブフレームワークでは、ブログポストを単一参照している全てのルートを適切に更新していました。もし, 何かしらの手違いがあると、実行時に 404 などのエラーが起きてしまいます。Yesod では、ルートが更新されたときにコンパイルが行われ GHC は修正する必要のあるすべての行を正確に指摘します。

## HTML 以外のレスポンス

Yesod はどんなコンテンツに対しても対応可能であり、一般的なレスポンス形式の多く関しては Yesod でサポートしています。 HTML については既に述べた通りですが、例えば JSON データなら aeson パッケージを利用する事で容易に対応できます。

```haskell
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR  = return $ object ["msg" .= "Hello World"]

main = warp 3000 App
```

JSON レスポンスに関しては `Accept` リクエストヘッダーによって自動的に HTML と JSON の表現を切り替える方法を含めて、後の章において詳細を述べます。

## The scaffolded site

`stack` を使って Yesod ライブラリと `yesod` 補助実行ファイルをインストールし、`stack new` コマンドで scaffolded site を生成した場合、デフォルトの scaffolded site を含むフォルダが生成されていることでしょう。そのフォルダの中で、追加の依存関係 (バックエンドデータベースなど) を構築するために `stack install --only-dependencies` を実行し、さらに `yesod devel` コマンドでサイトを動かすことができます。

<div class="yesod-book-notice">
パッケージ環境のセットアップは [quick start guide](http://www.yesodweb.com/page/quickstart) を参照してください。
</div>

scaffolded site はファイルのセットアップや依存関係など、プロダクション Yesod サイトにおいて培われてきたベストプラクティスをまとめたものです。しかしながら、これらの利便性は実際に Yesod を学習することで得られる知見なので、本書では scaffolding ツールの利用を避け、代わりにライブラリとして Yesod を直接扱います。しかし、実際のサイトを立ち上げる場合はこのツール用いることを強くおすすめします。

scaffolded site の構造については、scaffolding の章で説明します。

## 開発用サーバー

インタプリタ言語にはコンパイラ言語には無い、「変更をファイルに保存し, 再読み込みすれば良い」という迅速なプロトタイピングというメリットがあります。上述の Yesod アプリケーションに何らかの変更を加える場合は `runhaskell` コマンドを最初からもう一度実行する必要があり、それは少し面倒です。

しかし、幸いなことに解決策があります。`yesod devel` はコードを自動的にリビルド、リロードしてくれます。これは Yesod プロジェクトの開発方法として素晴らしく、プロダクションへ移行する際にかなり効率的なコードのコンパイルを行うことができます。Yesod scaffolding は自動的にこれらの作業をセットアップし、その結果、迅速なプロトタイピングと高速なプロダクションコードが共存する最適な世界が実現できます。

`yesod devel` を利用してコードをセットアップするためにはもう少し深入りする必要があるため、本書の例では `warp` だけを利用しています。しかし、 scaffolded site は開発サーバを利用するための十分な設定がされているため、本書を読み終わって、すぐに現実世界のアプリケーション開発へ飛び込むことができるでしょう。

## まとめ

あらゆる Yesod アプリケーションはファウンデーションデータ型を中心に構築されます。リソースをファウンデーションデータ型に関連付け、ハンドラ関数を定義することで、Yesod がすべてのルーティングを行います。リソースもデータコンストラクタなので、型安全URLを持つことが可能となります。

Yesod は WAI のトップレベルに組み込まれているため、Yesod アプリケーションは多くの異なるバックエンドで動作します。単純なアプリケーションにおいては `warp` 関数は Warp ウェブサーバを利用するための手頃な方法です。また、迅速な開発を行うために `yesod devel` を利用することは良い選択です。そして、プロダクション環境へ移行するに当たり、自分たちが必要とする Warp (あるいは他のWAIハンドラ) を構成するための十分な力と柔軟性があります。

Yesod の開発では、準クォート or 外部ファイル、`warp` or `yesod devel` などのコーディングスタイルについて多数の選択肢があります。本書における例は、コピー&ペーストしやすい選択肢を利用する傾向がありますが、実際に Yesod アプリケーションを構築する時には、より強力なオプションの選択が可能です。

## 本書のコード

- [Example01.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch03/Example01.hs)
- [Example02.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch03/Example02.hs)
- [Example03.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/ch03/Example03.hs)
