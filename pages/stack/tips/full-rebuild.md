---
title: 完全なリビルド
published: 2017/12/24
updated: 2018/05/05
---

## リビルド方法

通常 **stack build** でリビルドした際はキャッシュが利用されます。

しかし、場合によって (警告をもう一度みたいなど) はキャッシュを無視してリビルドしたい時があります。

`--force-dirty` や `--ghc-options=-fforce-recomp` などを使う方法もあるのですが、一番確実な方法は **stack clean** することです。

```shell
$ stack clean
$ stack build
```

これでもダメな場合は、以下の方法を試してみてください。

## 何をやってもだめな時

基本的には **stack clean** で上手く行くことが多いのですがどうしてもだめな場合は `--full` オプションを追加します。

```shell
$ stack clean --full
$ stack build
```

このオプションは以下のコマンドと同じ結果となります。

```shell
$ rm -rf .stack-work/
```

## それでもダメな時

ここまでする必要があるのは非常に稀ですが、どうしても上手く行かない時は試してみてください。

**precompiled** ディレクトリと **snapshots** ディレクトリを削除してもう一度ビルドしてみましょう。

```shell
$ stack path --stack-root
~/.stack

$ rm -rf $(stack path --stack-root)/precompiled
$ rm -rf $(stack path --stack-root)/snapshots
```