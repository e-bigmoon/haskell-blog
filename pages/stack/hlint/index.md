---
title: HLint
published: 2018/02/10
updated: 2018/02/12
---

## はじめに

[HLint](https://github.com/ndmitchell/hlint) は [haskell-src-exts](https://www.stackage.org/package/haskell-src-exts) を使って実装されている静的解析ツールです。

`HLint` を使えば `github` などを使って `PR` ベースで開発する場合のコードレビューでこんな事を言わなくて済みます。

- `fromJust` とかの部分関数は使わないで！
- `maybe` 関数って知ってる？
- この言語拡張って本当に使ってるの？
- `undefined` まだ残ってるじゃん！

嬉しいことに `Travis CI` や `CircleCI` などで一度設定するだけなので導入もお手軽です！
また、最近知ったのですが、プロジェクト内で使って欲しくない関数なども `HLint` によって検出可能です。

さらに、独学で `Haskell` の学習を進めている人は `HLint` が素晴らしい教師役となってくれるでしょう。

## HLint の参考記事

`HLint` は割と有名なので日本語の解説記事がいくつかありました。

- [Haskellの静的解析ツール HLint を使おう](https://qiita.com/suzuki-hoge/items/6d101e523620178c6f7b)
- [Haskellを書くときはstylish-haskellとhlintを使って労せずして綺麗なコードを書きましょう](https://www.ncaq.net/2017/10/07/)
- [OverloadedStringsとANNプラグマが干渉する場合の回避方法](https://qiita.com/VoQn/items/fe7953aec010d8f68a59)

本格的に利用しようとすると、上記の解説記事では少し物足りません。具体的には以下の点が不足しています。

- [カスタムヒントの設定方法](/stack/hlint/hlint-customhint.html)
- [関数の利用制限方法](/stack/hlint/forbidden-functions.html)
- [関数・モジュール・ファイル単位でヒントを無視する方法](/stack/hlint/hlint-ignore.html)
- [CIで利用するための設定方法](/stack/hlint/hlint-ci.html)

本記事では、これらの内容について解説を行います。`HLint` でどんなことが出来るかについては、上記の記事または[公式リポジトリ](https://github.com/ndmitchell/hlint)をご参考ください。

また、内部の仕組みについては、作者の `Neil Mitchell` さんの解説記事が参考になります。

- [HLint のルールを理解する (和訳)](https://qiita.com/rounddelta/items/4584f5486c1061c93f0b)

## まとめ

今回は紹介していませんが `HLint` のヒントを自動的に適用してくれる [apply-refact](https://github.com/mpickering/apply-refact) というツールもあります。

使い方については各種ドキュメントをご確認ください。

- [Automatically Applying Hints](https://github.com/ndmitchell/hlint#automatically-applying-hints)
