---
title: Literate Markdown
author: Shinya Yamaguchi
tags: bigmoon, ghc
---

## はじめに

今回は `Literate Markdown` をご紹介します。(最近見つけて面白そうだったので使ってみました。)

モチベーションとしては `Bird` 記法や `TeX` 記法だけではなく、`Markdown` 記法も使いたいよね！という感じです。

また、`HaskelL` の文学的 (Literate) プログラミングについては、すでに多くのWeb・書籍等で言及されているため、必要最低限の説明に留めたいと思います。

本記事の内容は以下を参考としています。

- [Haskell 2010 Language Report](https://www.haskell.org/definition/haskell2010.pdf)
- [Literate Markdown](https://ghc.haskell.org/trac/ghc/wiki/LiterateMarkdown)
- [Flexible Literate Haskell File Extensions](https://ghc.haskell.org/trac/ghc/wiki/FlexibleLiterateExtension)
- [sol/markdown-unlit](https://github.com/sol/markdown-unlit)

<!--more-->

## 文芸的プログラミング

文芸的プログラミングは `GHC` 固有の機能ではなく `Haskell 2010` の仕様 (10.4 Literate comments) で規定されています。

また、拡張子は通常の `hs` ではなく `lhs` となります。

スタイルについては `Bird` 記法と `LaTeX` 記法があり、好きな方を使うと良いでしょう。

### Bird 記法

`Brid` 記法は先頭に `>` を入れて表現するスタイルです。

`>` から始まる行が `Haskell` コードとして認識されます。

```haskell
This is a comment.  Lines starting with '>' are the actual code.

> average xs = sum xs / length xs
```

### LaTeX 記法

`LaTeX` 記法は通常の `LaTeX` のように `\begin{code} - \end{code}` のコードブロックで囲まれた部分を `Haskell` コードとして認識します。

```haskell
This is a comment.

\begin{code}
average = sum xs / length xs
\end{code}
```

## Literate Markdown

`Literate Markdown` は2014年頃に議論があったようです。実装も[提案](https://github.com/elliottt/ghc/tree/literate-markdown)されていたようですが、現状の `GHC` には組み込まれていません。

どうしようかなと思っていましたが、色々と調べた結果、[sol/markdown-unlit](https://github.com/sol/markdown-unlit) という素晴らしいパッケージを見つけました。

このパッケージを利用することで当初の目的が達成できそうです！

## markdown-unlit

### インストール

`version 0.5.0` を利用したかったので `--resolver=nightly` としました。

```shell
$ stack install markdown-unlit --resolver=nightly
$ which markdown-unlit
/home/bm12/.local/bin/markdown-unlit
```

### 実際に使ってみる

以下の内容を `test.lhs` というファイル名で保存しましょう。

<div class="sourceCode"><pre class="sourceCode markdown"><code class="sourceCode markdown hljs"># nifty-library: Do nifty things (effortlessly!)

Here is a basic example:

```haskell
main :: IO ()
main = putStrLn "That was easy!"
```
</code></pre></div>

この時、拡張子は `.lhs` となることに注意してください。(`.md` としたい場合はシンボリックリンクを作成します)

では実行してみます。

```shell
$ stack ghci --ghc-options="-pgmL markdown-unlit" test.lhs
[1 of 1] Compiling Main             ( /home/bm12/Desktop/test.lhs, interpreted )
Ok, modules loaded: Main.
Loaded GHCi configuration from /tmp/ghci19999/ghci-script
*Main> main
That was easy!
```

問題無さそうですね。これで `Literate Markdown` 形式のファイルを作成することができました。

`-pgmL markdown-unlit` オプションは `.ghci` ファイルを設定することで省略可能です。

```shell
$ echo ':set -pgmL markdown-unlit' >> ~/.ghci
```

もし、読み込み時に `*** WARNING: ~/.ghci is writable by someone else, IGNORING!` のようなエラーが出る場合は以下のようにして、ファイルのパーミッションを変更します。

```shell
$ chmod go-w ~/.ghci
```

これで、基本的な使い方はわかりました。次に実際のプロジェクトにおいてどのように利用されているか確認してみたいと思います。

## stack プロジェクトで markdown-unlit を使う

より実践的な例として `markdown-unlit` を使ってプロジェクトの `README.md` を `Literate Markdown` 形式で記述してみましょう。

この手法は実際にいくつかのプロジェクトで利用されています。

- [yesodweb/wai](https://github.com/yesodweb/wai/tree/master/wai#readme)
- [tfausak/strive](https://github.com/tfausak/strive)
- [sol/attoparsec-parsec](https://github.com/sol/attoparsec-parsec#readme)
- [hspec/hspec-expectations](https://github.com/hspec/hspec-expectations#readme)

### 準備

```shell
$ stack new literate-markdown
$ cd literate-markdown
```

`LTS-10.1` に含まれる `markdown-unlit` のバージョンは `0.4.1` ですが、最新版の `0.5.0` を使いたいので `resolver` に `nightly` を指定します。ついでに `package.yaml` に依存関係を追加しておきます。

`stack.yaml` と `package.yaml` はそれぞれこんな感じです。

```yaml
resolver: nightly-2017-12-28
packages:
- .
```

```yaml
name:                literate-markdown
version:             0.1.0.0
github:              "waddlaw/literate-markdown"

extra-source-files:
- README.md
- ChangeLog.md

dependencies:
- base >= 4.7 && < 5
- markdown-unlit
```

とりあえず `stack build` しておきます。

```shell
$ stack build
```

### markdown-unlit の設定

まずは `package.yaml` に以下の内容を追記します。

```yaml
ghc-options: -pgmL markdown-unlit
```

次に `README.md` の内容を以下のように変更します。

<div class="sourceCode"><pre class="sourceCode markdown"><code class="sourceCode markdown hljs"># nifty-library: Do nifty things (effortlessly!)

Here is a basic example:

```haskell
main :: IO ()
main = putStrLn "That was easy!"
```
</code></pre></div>

ここが少し面倒ですが、シンボリックリンクを作成します。

```shell
$ ln -s README.md README.lhs
```

### テストの設定

`package.yaml` にテストを追加しましょう。以下の内容を追記します。

```yaml
tests:
  readme:
    main: README.lhs
```

これで `stack test` を実行してみましょう。

```shell
$ stack test
literate-markdown-0.1.0.0: test (suite: readme)

That was easy!

literate-markdown-0.1.0.0: Test suite readme passed
```

ちゃんと `That was easy!` が表示されています。

## 終わりに

- `Haskell` で利用可能な文芸的プログラミングについて少しだけ解説しました
- `Markdown` 形式で文芸的プログラミングを行うための `markdown-unlit` について解説しました

実際にいくつかのプロジェクトで利用されているように、`README.md` で `main` 関数を実行する例がある場合は、この方法を採用してみても良いのではないでしょうか。

本記事で利用したコード: [github](https://github.com/waddlaw/example-literate-markdown)