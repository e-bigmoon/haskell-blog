---
title: hpack とは何か？
date: 2019/09/14
prev: stack-install.html
next: alt-stack.html
---

[hpack](https://github.com/sol/hpack) は **yaml** 形式の **package.yaml** という名前のファイルから `.cabal` を自動生成するためのツールです。

**hpack** を使うメリットとしては以下が挙げられます。

- **yaml** 形式なのでわかりやすい (**cabal** 形式を覚えなくて良い)
- **exposed-modules** と **other-modules** を明示的に書かなくても自動的に推論してくれる

**stack** で管理されているプロジェクトは、**hpack** を利用していることが多いです。そのため、本記事でも **hpack** を利用します。

ただ、最近では **hpack** を使わないプロジェクトも多いので、慣れてきたら **package.yaml** を削除して **cabal** だけでも良いかもしれません。

## インストール & 使い方

**stack** はライブラリとしての **hpack** に依存しているため、別途 **hpack** をインストールする必要はありません。**stack build** コマンドを実行すれば自動的に **package.yaml** から `.cabal` ファイルを生成してくれます。

自分の使っているバージョンを確認する場合は `stack --hpack-numeric-version` を使います。

```shell
$ stack --hpack-numeric-version 
0.31.2
```

手動で `.cabal` を変更した場合は **hpack** が変更を検知して `.cabal` を生成しないので、そちらもご注意ください。その場合は単純に **cabal** ファイルを削除し、もう一度ビルドすれば良いです。

### .gitignore ファイルへの追加

**stack new** で作ったプロジェクトの場合 `.gitignore` に `<project>.cabal` が追加されていると思います。

これは、異なる **hpack** (**stack**) のバージョンでビルドしようとしたときの問題を回避するために設定されています。

**github** などで公開しているアプリケーションの場合、 `.cabal` ファイルが含まれている場合は、同じバージョンの **hpack** を利用するか、`.cabal` ファイルを削除してからビルドする必要があります。

また、そのファイルを **push** したら同じ事が繰り返し起きてしまいます。

そのため **hpack** を利用する場合は `.gitignore` ファイルに `.cabal` を追加するようにしておきましょう。

## hpack を使いたくない場合

**hpack** は **package.yaml** がプロジェクトのルートに存在する場合のみ **package.yaml** から `.cabal` ファイルを生成します。

そのため、**hpack** を利用したくない場合はプロジェクトから **package.yaml** を削除するだけで大丈夫です。

```shell
$ rm package.yaml
```
