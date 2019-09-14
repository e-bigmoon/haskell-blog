---
title: stack のインストールと設定
date: 2019/09/14
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

これで **stack** が使えるようになったと思います。以下のようにバージョン情報が表示されれば大丈夫です。

```shell
$ stack --version
Version 2.1.3, Git revision 0fa51b9925decd937e4a993ad90cb686f88fa282 (7739 commits) x86_64 hpack-0.31.2
```

### パスの追加

このタイミングで `~/.local/bin` を環境変数 **PATH** に追加しておくことをおすすめします。

**stack install** コマンドでインストールされる実行ファイルは `~/.local/bin/` 以下に保存されます。

そのため、`~/.local/bin/` にパスを通すことで、どこからでもコマンドを実行できるようになります。

```shell
$ export PATH=~/.local/bin:$PATH

# 必要に応じて .bashrc や .bash_profile などに追記してください
$ echo 'export PATH=~/.local/bin:$PATH' >> ~/.bashrc
```

### コマンドの自動補完設定

これを設定しておくと、**stack** のサブコマンドを補完してくれるので、何かと便利です。

```shell
$ echo 'eval "$(stack --bash-completion-script stack)"' >> ~/.bashrc
```

**zsh** ユーザは [for ZSH users](https://docs.haskellstack.org/en/stable/shell_autocompletion/#for-zsh-users) をご確認ください。

## ~/.stack/ のディレクトリ構造

**stack** をインストールしたら、初回だけ **stack update** を行い、グローバルプロジェクト用のディレクトリを生成しましょう。

```shell
$ stack update
...

$ tree ~/.stack/
~/.stack/
├── config.yaml
├── pantry
│   ├── hackage
│   │   ├── 00-index.tar
│   │   ├── 00-index.tar.gz
│   │   ├── 00-index.tar.idx
│   │   ├── hackage-security-lock
│   │   ├── mirrors.json
│   │   ├── root.json
│   │   ├── snapshot.json
│   │   └── timestamp.json
│   ├── pantry.sqlite3
│   └── pantry.sqlite3.pantry-write-lock
├── stack.sqlite3
└── stack.sqlite3.pantry-write-lock
```

**pantry** フォルダや **stack.sqlite3**, **stack.sqlite3.pantry-write-lock** ファイルを触ることは基本的にありません。

### config.yaml

**config.yaml** には **stack new** でプロジェクトを生成する際の設定を記述します。

僕が設定している項目は以下のとおりです。

```yaml
templates:
  scm-init: git
  params:
    author-name: Your Name
    author-email: youremail@example.com
    github-username: yourusername
```

詳しくは [config.yaml のよくある設定](../tips/config-yaml.html) をご確認ください。
