---
title: cabal-install の更新
date: 2019/09/14
---

## cabal のアップグレード方法

### ghcup

一番おすすめの方法です。バイナリが落ちてくるためすぐに更新が終わります。

```shell
$ ghcup install-cabal
Installing cabal-install-3.0.0.0 into "/Users/gupi/.ghcup/bin"
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 4904k  100 4904k    0     0  7007k      0 --:--:-- --:--:-- --:--:-- 7006k
Successfully installed cabal-install into
  /Users/gupi/.ghcup/bin
```

### cabal

`ghcup` が最新版に対応していない場合はこちらで更新すると良いでしょう。ただ、ビルドに時間がかかります。

```shell
$ cabal update
$ cabal install cabal-install
```

### stack

`stack` を使う方法では最新版が落ちてこない可能性もあるので、あまりおすすめしません。

```shell
$ stack install cabal-install
```
