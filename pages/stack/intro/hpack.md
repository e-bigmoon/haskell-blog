---
title: hpack について
date: 2018/01/08
prev: stack-install.html
next: create-prj.html
---

## hpack の役割

`hpack` は `yaml` 形式の `package.yaml` から `.cabal` を自動生成するためのツールです。

`hpack` を使うメリットとしては以下が挙げられます。

- `yaml` 形式なのでわかりやすい (`cabal` 形式を覚えなくて良い)
- `exposed-modules` を書けば、`other-modules` を自動推論してくれる
- 共通する `dependencies` や `ghc-options` を一箇所に集約できる

## インストール & 使い方

`stack` にバンドルされているため、別途 `hpack` をインストールする必要はありません。

そのため `stack build` コマンドを実行すれば自動的に `package.yaml` から `.cabal` を生成してくれます。

自分の使っているバージョンを確認する場合は `stack --version` を使います。一番最後に表示されます。(僕の環境では `hpack-0.20.0`)

```shell
$ stack --version
Version 1.7.0, Git revision 98c51cafb038bf09b58d5607956fb199d3660735 (5570 commits) x86_64 hpack-0.20.0
```

もし、最新版の `hpack` を利用したい場合は `--with-hpack=<PATH>` オプションを利用する必要があるのでご注意ください。

また、手動で `.cabal` を変更した場合は `hpack` が変更を検知して `.cabal` を生成しないので、そちらもご注意ください。

## 既存の .cabal プロジェクトを hpack に変換する方法

既存のプロジェクトを `hpack` に変換するための便利ツールとして [hpack-convert](https://github.com/yamadapc/hpack-convert) があります。

このツールを使えばプロジェクトの `.cabal` から `.package.yaml` を生成することができます。

`stack` もこのツールを使って `hpack` に移行しました。([Use hpack package.yaml to build Stack #3506](https://github.com/commercialhaskell/stack/pull/3506))

そのため、基本的には大丈夫だと思いますが、僕が使った時は少し変換がおかしい部分もあったのでご注意ください。

## 従来の .cabal 形式を使いたい場合

`hpack` は `package.yaml` がプロジェクトのルートに存在する場合のみ `package.yaml` から `.cabal` ファイルを生成します。

そのため `cabal` を使いたい場合はプロジェクトから `package.yaml` を削除するだけで大丈夫です。

## まとめ

`stack new` した際のデフォルトテンプレートは `hpack` に切り替わりました。([Switch new-template to use hpack #112](https://github.com/commercialhaskell/stack-templates/pull/112))

利用可能なテンプレートは `stack templates` コマンドで確認できます。

```shell
$ stack templates
Template                    Description
chrisdone
foundation                - Project based on an alternative prelude with batteries and no dependencies.
franklinchen
ghcjs                     - Haskell to JavaScript compiler, based on GHC
ghcjs-old-base
hakyll-template           - a static website compiler library
haskeleton                - a project skeleton for Haskell packages
hspec                     - a testing framework for Haskell inspired by the Ruby library RSpec
new-template
protolude                 - Project using a custom Prelude based on the Protolude library
quickcheck-test-framework - a library for random testing of program properties
readme-lhs                - small scale, quick start, literate haskell projects
rubik
scotty-hello-world
scotty-hspec-wai
servant                   - a set of packages for declaring web APIs at the type-level
servant-docker
simple
simple-hpack
simple-library
spock                     - a lightweight web framework
tasty-discover            - a project with tasty-discover with setup
tasty-travis
unicode-syntax-exe
unicode-syntax-lib
yesod-minimal
yesod-mongo
yesod-mysql
yesod-postgres
yesod-postgres-fay
yesod-simple
yesod-sqlite
```

本チュートリアルも `hpack` を利用します。
