---
title: おすすめの開発方法
date: 2018/05/05
---

## 開発環境

弊社で主に利用しているエディタとツールは以下の通りです。

- [VS CODE](https://code.visualstudio.com/)
- [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)
- [stylish-haskell](https://github.com/jaspervdj/stylish-haskell)

**haskell-ide-engine** は [Language Server Protocol (LSP)](https://github.com/jaspervdj/stylish-haskell) が利用できれば、どのエディタでも利用可能です。

そのた、エディタについては **Emacs**, **NeoVim**, **Atom**, **Sublime Text** などと連携させて自由に利用しています。

## 開発中のビルド方法

僕は [VS CODE](https://code.visualstudio.com/) のターミナルに以下のコマンドを打ち込んで自動的にビルドさせています。

```shell
$ stack test --fast --file-watch
```

## 最後の仕上げ

基本的に **GHC** と **hlint** の警告を全て消したのち **stylish-haskell** でコードを整えています。

`--pedantic` オプションを利用すれば **GHC** の警告が出た段階でビルドがストップするので、効率的に修正できます。

```shell
# GHC 警告チェック
$ stack clean
$ stack test --fast --file-watch --pedantic

# hlint チェック
$ curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
```

## オプションの紹介

各オプションの意味は以下の通りです。

オプション | 効果
---------|-------
`--fast` | 最適化を無効にする (`-O0`)
`--file-watch` | ファイルの変更を検知すると自動的にリビルドを行う
`--pedantic` | `-Wall` による警告をエラーとして扱う
