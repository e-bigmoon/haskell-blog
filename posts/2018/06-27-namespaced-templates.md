---
title: namespaced templates
author: Shinya Yamaguchi
tags: bigmoon, stack
updated: 2018/06/27
---

## はじめに

つい先日 [namespaced templates](https://github.com/commercialhaskell/stack/pull/4103) という、PRが **master** にマージされました。([Namespaced templates #4039](https://github.com/commercialhaskell/stack/issues/4039) が該当する isssue です)

通常、**stack new** コマンドを実行すると、ローカルに保存されている [new-template](https://github.com/commercialhaskell/stack-templates/blob/master/new-template.hsfiles) の内容に基づいてプロジェクトの雛形が作成されます。

もし、ローカルに保存されているテンプレートの内容を確認したい場合は以下のようなコマンドを実行すれば良いでしょう。

```shell
$ cat $(stack path --stack-root)/templates/new-template.hsfiles
```

**config.yaml** を変更すれば、このデフォルトテンプレートを変更することもできます。(詳しくは [config.yaml のよくある設定](https://haskell.e-bigmoon.com/stack/tips/config-yaml.html) をご確認ください)

今回新たに実装された namespaced templates 機能は、これらのテンプレート機能をより強化するものとなっています。

具体的には **username/foo** という形式でリモートリポジトリから自分のプロジェクトテンプレートをダウンロードし、それをもとにプロジェクトを作成できるようになります。

それでは使い方を見ていきたいと思います。

```shell
$ stack --version
Version 1.8.0, Git revision 466da22ce21cddc20323fc2611cd2a2d3cc56ec6 (dirty) (5998 commits) x86_64 hpack-0.28.2
```

<!--more-->

## リポジトリを用意しよう！

namespaced templates 機能は以下のサービスで利用可能です。

- github
- gitlab
- bitbucket

まずは、**stack-templates** という名前でリポジトリを作っておいてください。(今のところは任意の名前に変更する方法はありません。)

この名前のリポジトリに存在するテンプレートファイルを参照することができます。

## テンプレートファイルを作ってみよう！

- テンプレートの参考になるのは [commercialhaskell/simple.hsfiles](https://github.com/commercialhaskell/stack-templates/blob/master/simple.hsfiles) です。
- テンプレートエンジンは [mustache](https://mustache.github.io/mustache.1.html) を使っているそうです。
- 今から作るテンプレートファイルは [e-bigmoon/sample.hsfiles](https://github.com/e-bigmoon/stack-templates/blob/master/sample.hsfiles) にあります。

### 最小のプロジェクトテンプレート

とりあえず **Readme.md** ぐらいは欲しいですよね。

```mastache
{-# START_FILE README.md #-}
# {{name}}
```

- `{-# START_FILE README.md #-}` はそれ以降に続く内容で **README.md** というファイルを作ります
ｰ `{{name}}` には、**stack new** の引数で与えたプロジェクト名が挿入されます。

ここまでで実際に動かして確認してみます。

```shell
$ cd /tmp
$ stack new test-proj e-bigmoon/sample
Downloading template "e-bigmoon/sample" to create project "test-proj" in test-proj/ ...
The template "e-bigmoon/sample" is invalid and could not be used. The error was: Template does not contain a .cabal or package.yaml file
```

怒られました。どうやら **cabal** ファイルか **package.yaml** が含まれている必要があるみたいです。

**package.yaml** ファイルも追加して作成するために、少し編集しましょう。

```mastache
{-# START_FILE README.md #-}
# {{name}}

{-# START_FILE package.yaml #-}
name: {{name}}
dependencies:
  - base
library: {}
```

もう一度実行してみます。

```shell
$ stack new test-proj e-bigmoon/sample
...
All done.

$ tree test-proj/
test-proj/
├── package.yaml
├── README.md
├── stack.yaml
└── test-proj.cabal
```

今度は成功しました。簡単でいいですね。

ダウンロードしたテンプレートファイルは `~/.stack/templates/<username>/<template>` というパスで保存されています。

## stack new コマンド

先程は github リポジトリ上のテンプレートファイルを取得したので以下のコマンドを実行しました。

```shell
$ stack new test-proj e-bigmoon/sample
```

このコマンドは次のコマンドの省略形です。

```shell
$ stack new test-proj github:e-bigmoon/sample.hsfiles
```

そのため gitlab や bitbucket にあるテンプレートファイルをダウンロードする場合は以下のようになります。

```shell
# gitlab
$ stack new test-proj gitlab:e-bigmoon/sample.hsfiles

# bitbucket
$ stack new test-proj bitbucket:e-bigmoon/sample.hsfiles
```

また、テンプレート名を省略した場合は公式のテンプレートが利用されるため、次のコマンドを実行していることと同じです。

```shell
$ stack new test-proj commercialhaskell/new-template
```

完全なURLで別のサーバーにあるテンプレートファイルを指定することも可能です。

```shell
$ stack new test-proj https://my-site.com/content/template9.hsfiles
```

github のテンプレートファイルを完全なURLで指定する際、上記のURLは invalid なので注意。ちゃんと raw の URL を指定しましょう。

```shell
# NG
$ stack new test-proj https://github.com/commercialhaskell/stack-templates/blob/master/hspec.hsfiles

# OK
$ stack new test-proj https://raw.githubusercontent.com/commercialhaskell/stack-templates/master/new-template.hsfiles
```

## stack.yaml が含まれる場合

プロジェクトテンプレートに **stack.yaml** が含まれる場合は初期化処理がスキップされます。([v1.7.1](https://haskell.e-bigmoon.com/posts/2018/05-04-stack171.html) で導入された修正ですね)

```shell
$ stack new test-proj e-bigmoon/sample
Downloading template "e-bigmoon/sample" to create project "test-proj" in test-proj/ ...
Initialized empty Git repository in /tmp/test-proj/.git/
```

## もっと複雑なテンプレート

`~/.stack/config.yaml` に変数を定義しておき、プロジェクトテンプレートから参照することもできます。

**config.yaml** ファイルの内容が以下のようになっているとしましょう。

```yaml
templates:
  params:
    author-email: chrisdone@gmail.com
    author-name: Chris Done
    copyright: 2018 Chris Done
    github-username: chrisdone
    category: Development
```

テンプレートファイル内から単純に `{{author-email}}` の形式で変数を展開できます。

```
{-# START_FILE {{name}}.cabal #-}
name:                {{name}}
version:             0.1.0.0
-- synopsis:
-- description:
homepage:            https://github.com/{{github-username}}{{^github-username}}githubuser{{/github-username}}/{{name}}#readme
license:             BSD3
license-file:        LICENSE
author:              {{author-name}}{{^author-name}}Author name here{{/author-name}}
maintainer:          {{author-email}}{{^author-email}}example@example.com{{/author-email}}
copyright:           {{copyright}}{{^copyright}}{{year}}{{^year}}2018{{/year}} {{author-name}}{{^author-name}}Author name here{{/author-name}}{{/copyright}}
category:            {{category}}{{^category}}Web{{/category}}
```

## まとめ

今までも自分で作ったプロジェクトテンプレートを使うことはできましたが、**new-template** を置き換えるか **config.yml** を設定する必要がありました。

今回の機能追加により、**stack new** コマンドで直接プロジェクトテンプレートを簡単に指定できるため、自作テンプレート作りがはかどりそうです。

以上です。