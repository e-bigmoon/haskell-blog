---
title: VS Code で Ghcid を使う
author: Neil Mitchell
translator: pythonissam
tags: Neil Mitchell’s Haskell Blog, 翻訳
---

Great original post: [Ghcid with VS Code](http://neilmitchell.blogspot.jp/2017/11/ghcid-with-vs-code.html).

2017年 11月 10日 Neil Mitchell

*概要: 新しいバージョンの Ghcid と VS Code拡張がリリースされ、よりいい感じに動くようになった。*

<!--more-->

[Ghcid v0.6.8](https://hackage.haskell.org/package/ghcid) と関連する VS Code拡張 [haskell-ghcid v0.2.0](https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid) をリリースしました。
一緒に使うと、Ghcid + VS Code 環境をシンプルなものにしてくれます。

## .ghcid ファイルの読み込み (Ghcid)
Ghcid に、カレントディレクトリに `.ghcid` ファイルがあった場合はそれを追加の引数としてロードする機能が追加されました。
例えば、Shake レポジトリには [.ghcid](https://github.com/ndmitchell/shake/blob/master/.ghcid) ファイルがあって:

```
-c "ghci -fno-code -ferror-spans"
```

`ghcid` に、コマンドでこれを解釈させずに (例えば `.stack-work` があれば `stack` でこれを解釈させずに) 常に `ghci -fno-code -ferror-spans` を実行させています。
このコマンドは、[`.ghci` ファイル](https://github.com/ndmitchell/shake/blob/master/.ghci) があって、必要なファイルを全てロードしているのでうまく動いています。
ちなみに `-fno-code` はコンパイル速度を上げるために、`-ferror-spans` はエラーのハイライトをより良くするために指定しています。

## ghcid を開始 (Ghcid VS Code)
`Start Ghcid` アクションという VS Code 拡張の新機能は、新しい `ghcid` 端末を起動します。
起動した後、出力は一時ファイルに保存され、Problems ペインに表示されます。
この拡張は `ghcid` を追加の引数なしで実行するため、引数を渡したい場合は `.ghcid` でこの引数を指定する必要があります。

このような変更をしたのは、VS Code からより少ないキーで `ghcid` を開始するためです。
以前は、ファイルを開いたり、特別なフラグを追加したり、コマンドの実行などが必要でした。
