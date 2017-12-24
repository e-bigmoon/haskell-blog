---
title: stackのインストール
---

お好きな方法で `stack` をインストールしてください。僕のおすすめは、公式が推奨している `curl` か `wget` を使った方法です。

```shell-session
$ curl -sSL https://get.haskellstack.org/ | sh

$ wget -qO- https://get.haskellstack.org/ | sh
```

このタイミングで `~/.local/bin` を `PATH` に追加しておくことをおすすめします。このディレクトリに `stack install` コマンドでインストールされた実行ファイルが保存されるため、パスを通しておくとどこからでもコマンドを実行できるようになります。

```shell-session
$ export PATH=~/.local/bin:$PATH

# 必要に応じて .bashrc などに追記してください
$ echo 'export PATH=~/.local/bin:$PATH' >> ~/.bashrc
```

###### config.yaml

一番最初だけ `stack update` を行い、グローバルプロジェクト用のフォルダを生成しましょう。

```shell-session
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

`config.yaml` には `stack new` でプロジェクトを生成する際の設定を記述します。僕はいつもこんな感じに設定しています。

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

詳しくは[ドキュメント](https://github.com/commercialhaskell/stack/blob/master/doc/yaml_configuration.md)をご確認ください。

##### hpack について

- [hpack](https://github.com/sol/hpack)

`hpack` は `package.yaml` が存在する場合のみ `package.yaml` から `.cabal` ファイルを生成するためのツールです。`stack` にバンドルされているため、別途 `hpack` をインストールする必要はありませんが、最新版の `hpack` を利用したい場合は `--with-hpack=<PATH>` オプションを利用する必要があるのでご注意ください。

個人的には `yaml` 形式なので、読みやすいというのと `other-modules` を明示的に記載しなくても良いという点に惹かれて導入しました。今後より便利になっていくと思っています。`stack` も `hpack` に移行しましたので普通の人は使っても大丈夫だと思います。

既存のプロジェクトを `hpack` に変換するための便利ツールとして [hpack-convert](https://github.com/yamadapc/hpack-convert) があります。`stack` はこのツールを使って `hpack` に移行しました。僕が使った時は少し変換がおかしい部分もあったので、ご注意ください。

また、`stack new` した際のデフォルトテンプレートも `hpack` に切り替わりました。([Switch new-template to use hpack #112](https://github.com/commercialhaskell/stack-templates/pull/112))。利用可能なテンプレートは `stack templates` コマンドで確認できます。

本チュートリアルも `hpack` を利用します。

##### Bash Auto-completion
設定しておくと、`stack` のサブコマンドを補完してくれるので、何かと便利です。

```shell-session
$ eval "$(stack --bash-completion-script stack)"
```
`zsh` ユーザは別途マニュアルを参照してください。

- [Shell Auto-completion](https://docs.haskellstack.org/en/stable/shell_autocompletion/)


##### hpack
- [sol/hpack](https://github.com/sol/hpack)
- [Cleaning Up Our Projects with Hpack!](https://mmhaskell.com/blog/2017/7/17/cleaning-up-our-projects-with-hpack)
- [yamadapc/hpack-convert](https://github.com/yamadapc/hpack-convert)
