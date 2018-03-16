---
title: stack について
date: 2018/01/08
prev: ./stackage.html
next: ./hpack.html
---

## インストール

お好きな方法で `stack` をインストールしてください。僕のおすすめは、[公式ドキュメント](https://docs.haskellstack.org/en/stable/README/#how-to-install)で推奨されている `curl` か `wget` を使った方法です。

```shell
$ curl -sSL https://get.haskellstack.org/ | sh

$ wget -qO- https://get.haskellstack.org/ | sh
```

## パスの追加

このタイミングで `~/.local/bin` を `PATH` に追加しておくことをおすすめします。

`stack install` コマンドで `~/.local/bin/` 以下に実行ファイルが保存されます。そのため、パスを通すことで、どこからでもコマンドを実行できるようになります。

```shell
$ export PATH=~/.local/bin:$PATH

# 必要に応じて .bashrc や .bash_profile などに追記してください
$ echo 'export PATH=~/.local/bin:$PATH' >> ~/.bashrc
```

## コマンドの自動補完設定

これを設定しておくと、`stack` のサブコマンドを補完してくれるので、何かと便利です。

```shell
$ eval "$(stack --bash-completion-script stack)"
```

`zsh` ユーザは [for ZSH users](https://docs.haskellstack.org/en/stable/shell_autocompletion/#for-zsh-users) をご確認ください。

## ~/.stack/ のディレクトリ構造

一番最初だけ `stack update` を行い、グローバルプロジェクト用のディレクトリを生成しましょう。

```shell
$ stack update
...

$ tree ~/.stack/
/home/bm12/.stack/
|-- config.yaml
`-- indices
    `-- Hackage
        |-- 00-index.tar
        |-- 00-index.tar.gz
        |-- 00-index.tar.idx
        |-- 01-index.tar
        |-- mirrors.json
        |-- root.json
        |-- snapshot.json
        `-- timestamp.json
```

`indices` フォルダを触ることは基本的にありません。

### config.yaml

`config.yaml` には `stack new` でプロジェクトを生成する際の設定を記述します。

僕はいつもこんな感じに設定しています。

```yaml
default-template: new-template
templates:
  scm-init: git
  params:
    author-name: <author>
    author-email: <email>
    copyright: 'Copyright (c) 2017 <author>'
    github-username: <github_username>
```

詳しくは[YAML Configuration](https://github.com/commercialhaskell/stack/blob/master/doc/yaml_configuration.md)をご確認ください。
