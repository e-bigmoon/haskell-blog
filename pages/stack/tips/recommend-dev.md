---
title: おすすめの開発方法
date: 2017/12/24
---

## vscode (Visual Studio Code)

僕は `vscode` のターミナルに以下のコマンドを打ち込んで自動的にビルドさせています。

```shell
$ stack test --fast --file-watch --pedantic
```

プロジェクトの初期など、ラフに開発する時は以下のコマンドを良く使います。

```shell
$ stack test --fast --file-watch
```

## オプションの紹介

各オプションの意味は以下の通りです。

オプション | 効果
---------|-------
`--fast` | 最適化を無効にする (`-O0`)
`--file-watch` | ファイルの変更を検知すると自動的にリビルドを行う
`--pedantic` | `-Wall` による警告をエラーとして扱う
