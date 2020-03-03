---
title: イントロダクション
published: 2017/12/25
updated: 2019/09/15
next: ./why-stack.html
---

## はじめに

[Pearls of Functional Algorithm Design](https://www.amazon.co.jp/dp/0521513383) (訳本: [関数プログラミング 珠玉のアルゴリズムデザイン](https://www.amazon.co.jp/dp/4274050645)) の第1章を題材に **Haskell** プロジェクトの作り方を学びます。

プログラムの内容を理解できなくても、記事を最後まで読むことで **Haskell** プロジェクトの作り方を学ぶことができると思います。

**Ubuntu 18.04 LTS** or **Mac** で動作確認を行っています。

### 本チュートリアルで取り扱う内容

1. [イントロダクション](./)
    - [なぜ stack を使うのか？](./why-stack.html)
    - [Stackage とは何か？](./stackage.html)
    - [stack のインストールと設定](./stack-install.html)
    - [hpack とは何か？](./hpack.html)
    - [stack 以外の選択肢について](./alt-stack.html)
1. [プロジェクトの作成](./create-prj.html)
1. [ライブラリの作成](./create-lib.html)
    - [repl 環境の使い方](./repl.html)
    - [GHC について](./ghc.html)
    - [パッケージと依存関係](./package-and-deps.html)
    - [extra-deps の指定方法](./extra-deps.html)
1. [アプリケーションの作成](./create-app.html)
1. [ドキュメントの作成](../doc/index.html)
    - [Haddock の基礎知識](../doc/haddock-intro.html)
    - [Haddock コメント形式](../doc/haddock-comment.html)
    - [Haddock の設定](../doc/haddock-settings.html)
1. [テストの作成](../test/)
    - [テストフレームワーク (hspec)](../test/hspec.html)
    - [【基礎】ランダムテスト (QuickCheck)](../test/quickcheck.html)
    - [【実践】ランダムテスト (QuickCheck)](../test/quickcheck2.html)
    - [ドキュメントのテスト (doctest)](../test/doctest.html)
    - [テストフレームワーク (tasty)](../test/tasty.html)
1. [ベンチマークの作成](../bench/)
    - [criterion パッケージ](../bench/criterion.html)
    - [gauge パッケージ](../bench/gauge.html)
1. [アプリケーションの配布](../dist/index.html)
    - [docker integration](../dist/docker.html)
    - [stack script](../dist/stack-script.html)
1. より良いソフトウェアを作るために
    - Linter (hlint)
    - フォーマッター (ormolu)
    - CI
      - Travis CI
      - Circle CI
      - AppVeyor
      - GitHub Actions

### 動作環境

環境 | バージョン
-----|--------
OS | Ubuntu, macOS High Sierra
stack | version 2.1.3
hpack | 0.31.2

### 参考サイト

**stack** 並びに各ツールに関して参考にしたサイトは [Links](/stack/etc/links.html) にまとめていますので、随時ご確認ください。
