---
title: stack path
date: 2018/07/30
---

## 目的

基本的にはあまり使いません。

`stack` が参照している各種パスを表示することができます。`stack` で問題が発生した際、この内容も教えてあげると解決しやすくなります。

## 使い方

全てのパスを表示

```shell
$ stack path
....
```

`stack` のルートパスのみを表示

```shell
$ stack path --stack-root
...
```