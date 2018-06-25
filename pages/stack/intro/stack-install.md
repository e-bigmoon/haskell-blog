---
title: stack について
date: 2018/05/05
prev: ./stackage.html
next: ./hpack.html
---

## インストール

お好みの方法で **stack** をインストールしてください。

僕のおすすめは、[公式ドキュメント](https://docs.haskellstack.org/en/stable/README/#how-to-install)で推奨されている **curl** か **wget** を使った方法です。

```shell
$ curl -sSL https://get.haskellstack.org/ | sh

$ wget -qO- https://get.haskellstack.org/ | sh
```

## パスの追加

このタイミングで `~/.local/bin` を環境変数 **PATH** に追加しておくことをおすすめします。

**stack install** コマンドでインストールされる実行ファイルは `~/.local/bin/` 以下に保存されます。

そのため、`~/.local/bin/` にパスを通すことで、どこからでもコマンドを実行できるようになります。

```shell
$ export PATH=~/.local/bin:$PATH

# 必要に応じて .bashrc や .bash_profile などに追記してください
$ echo 'export PATH=~/.local/bin:$PATH' >> ~/.bashrc
```

## コマンドの自動補完設定

これを設定しておくと、**stack** のサブコマンドを補完してくれるので、何かと便利です。

```shell
$ echo 'eval "$(stack --bash-completion-script stack)"' >> ~/.bashrc
```

**zsh** ユーザは [for ZSH users](https://docs.haskellstack.org/en/stable/shell_autocompletion/#for-zsh-users) をご確認ください。

## ~/.stack/ のディレクトリ構造

**stack** をダウンロードしたら、初回だけ **stack update** を行い、グローバルプロジェクト用のディレクトリを生成しましょう。

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

- **config.yaml** にプロジェクトで共通する設定を記述しておくことができます。
- **indices** フォルダ以下を触ることはありません。

### config.yaml

**config.yaml** には **stack new** でプロジェクトを生成する際の設定を記述します。

僕は設定している項目は以下のとおりです。

```yaml
templates:
  scm-init: git
  params:
    author-name: Your Name
    author-email: youremail@example.com
    github-username: yourusername
```

詳しくは [config.yaml のよくある設定](../tips/config-yaml.html) をご確認ください。
