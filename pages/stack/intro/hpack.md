---
title: hpack について
date: 2018/05/05
prev: stack-install.html
next: create-prj.html
---

## hpack の役割

[hpack](https://github.com/sol/hpack) は **yaml** 形式の **package.yaml** という名前のファイルから `.cabal` を自動生成するためのツールです。

**hpack** を使うメリットとしては以下が挙げられます。

- **yaml** 形式なのでわかりやすい (**cabal** 形式を覚えなくて良い)
- **exposed-modules** と **other-modules** を明示的に書かなくても自動的に推論してくれる
- 共通する **dependencies** や **ghc-options** を一箇所に集約できる

## インストール & 使い方

**stack** と一緒にインストールされるため、別途 **hpack** をインストールする必要はありません。

**stack build** コマンドを実行すれば自動的に **package.yaml** から `.cabal` ファイルを生成してくれます。

自分の使っているバージョンを確認する場合は `stack --version` を使います。一番最後に表示されます。

```shell
$ stack --version
Version 1.7.1, Git revision 681c800873816c022739ca7ed14755e85a579565 (5807 commits) x86_64 hpack-0.28.2
```

もし、開発版の **hpack** を利用したい場合は `--with-hpack=<PATH>` オプションを利用します。

また、手動で `.cabal` を変更した場合は **hpack** が変更を検知して `.cabal` を生成しないので、そちらもご注意ください。

### .gitignore ファイルへの追加

**stack new** で作ったプロジェクトの場合 `.gitignore` に `<project>.cabal` が追加されていると思います。

これは、異なる **hpack** (**stack**) のバージョンでビルドしようとしたときの問題を回避するために設定されています。

**github** などで公開しているアプリケーションの場合、 `.cabal` ファイルが含まれている場合は、同じバージョンの **hpack** を利用するか、`.cabal` ファイルを削除してからビルドする必要があります。

また、そのファイルを **push** したら同じ事が繰り返し起きてしまいます。

そのため **hpack** を利用する場合は `.gitignore` ファイルに `.cabal` を追加するようにしておきましょう。

## hpack を使いたくない場合

**hpack** は **package.yaml** がプロジェクトのルートに存在する場合のみ **package.yaml** から `.cabal` ファイルを生成します。

そのため、**hpack** を利用したくない場合はプロジェクトから **package.yaml** を削除するだけで大丈夫です。