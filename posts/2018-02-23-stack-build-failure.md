---
title: stack でどうしてもビルドできないとき
author: Shinya Yamaguchi
tags: bigmoon, stack
---

## はじめに

僕は `stack`, `hlint`, `liquidhaskell` などのパッケージを毎日なんとなくビルドしてインストールしているのですが、ある時 `stack` のビルドで `GHC panic` が発生し、ビルドができなくなってしまいました。

数日放置していたのですが、直らず・・・。そればかりか別のPCでは問題なくビルドができているため `stack` の問題だな！と意気揚々と [issue](https://github.com/commercialhaskell/stack/issues/3876) を作ったのですが、結局は自分の環境の問題でした・・・。

僕は英語が得意ではありませんが、こういった時に批判だけするのは良くないと思っているので、頑張って `issue` や `PR` を投げるようにしています。Google 翻訳とかもありますし、誰か助けてくれますよ、きっと。 (放置されたり却下されることもありますが、気にせずコツコツ続けると楽しいです)

<!--more-->

## Stack のビルドがどうしてもできない時

ちゃんと `stack clean --full` を行った上でビルドコマンドを叩いたら、以下のような `ghc panic` エラーになりました。

```shell
$ stack build
...

ghc: panic! (the 'impossible' happened)
      (GHC version 8.2.2 for x86_64-apple-darwin):
    	Loading temp shared object failed: dlopen(/var/folders/ky/8g7fv32j4js337c85sy5gy0h0000gn/T/ghc43457_0/libghc_437.dylib, 5): Symbol not found: _hackagezmsecurityzm0zi5zi2zi2zm9BfzzLHvNB6mEIMD9YTAK2zz_HackageziSecurityziUtilziChecked_zdwthrowChecked_closure
      Referenced from: /var/folders/ky/8g7fv32j4js337c85sy5gy0h0000gn/T/ghc43457_0/libghc_437.dylib
      Expected in: flat namespace
     in /var/folders/ky/8g7fv32j4js337c85sy5gy0h0000gn/T/ghc43457_0/libghc_437.dylib

    Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
```

`ghc panic` は、言語拡張とか最新の機能とかを触っていれば良く見るエラーですが、なぜこれが起きたのか全くわかりません・・・。

`issue` のアドバイス通り `stack exec -- ghc-pkg unregister hackage-security` も試してみたのですがダメでした。

なので最終手段の `~/.stack` を削除することになったんですが、今までどのフォルダを削除したら良いのかイマイチわかっていませんでした。

しかし `stack` の中の人は違います！ `precompiled`, `snapshots` を削除しなよ！と優しく教えてくれたので、指示通りこんな感じで削除したおところ、無事にビルドが通るようになりました！

```shell
$ stack path --stack-root
~/.stack

$ rm -rf $(stack path --stack-root)/precompiled
$ rm -rf $(stack path --stack-root)/snapshots
```

もう知ってるよ・・・。という話かもしれませんが、どうしてもビルドできない人は試してみてはいかがでしょうか。

以上です。