---
title: 完全なリビルド
---


通常 `stack build` でリビルドした際はキャッシュが利用されます。しかし、場合によって (警告をもう一度みたいなど) はキャッシュを無視してリビルドしたい時があります。

`--force-dirty` や `--ghc-options=-fforce-recomp` などを使う方法もあるのですが、一番確実なのは `stack clean` することです。

```shell-session
$ stack clean
$ stack build

# 上記でもダメな場合
$ stack clean --full
$ stack build
```
