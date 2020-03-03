---
title: stack exec
published: 2018/07/30
# updated: 2019/09/15
---

## 普通の使い方

`stack exec` は通常、ビルドしたバイナリを実行するために使います。

```shell
$ stack build
$ stack exec -- <binary_name>
```

`--` はオプションを渡すときには必須です。また、バイナリの名前は `package.yaml` に記載した名前になります。

## ビルドしたバイナリのパスの取得

また、あまり知られていませんが `stack exec` コマンドは `shell` で利用できるコマンドがそのまま使えます。

```shell
$ stack exec -- ls -l
...

$ stack exec env
...
```

これを少し応用すると、ビルドしたバイナリのパスを簡単に取得することができます。

```shell
$ stack exec -- which <binary_name>
...
```

デプロイスクリプトなどで使うと便利です。