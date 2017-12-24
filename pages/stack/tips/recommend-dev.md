---
title: おすすめの開発方法
---

その時によって違うといえば違うのですが僕は `vscode` のターミナルに以下のコマンドを打ち込んで自動的にビルドさせています。

```shell-session
$ stack test --fast --file-watch --pedantic

# ラフに開発する時はこっちを使ってます
$ stack test --fast --file-watch
```

各オプションの意味は以下の通りです。

オプション | 意味
---------|-------
--fast | 最適化を無効にする (`-O0`)
--file-watch | ファイルの変更を検知するとリビルドする
--pedantic | `-Wall` による警告をエラーとして扱う
