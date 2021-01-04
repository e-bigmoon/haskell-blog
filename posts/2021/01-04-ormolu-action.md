---
title: ormolu-action
author: Shinya Yamaguchi
tags: bigmoon
updated: 2021/01/04
---

へいしゃでは、ソースコードのフォーマッターに [Ormolu][hkg-ormolu] を利用しています。

インデント時のスペース数すらカスタマイズ不可なので好き嫌いが分かれるところですが、プロジェクト全体のコードを統一しておくとコードレビューの際にも有用なので入れています。

ちなみにスペース幅を4にしたくてフォークされた [fourmolu][github-fourmolu] というフォーマッターもあります。現在ではスペース数以外の設定もできるみたいです。

どちらも [hls][github-hls] から利用可能なので、興味がある人は試してみると良いと思います。

また、**ormolu** のフォーマット結果で末尾カンマが気に入らない場合は [google/ormolu][google-ormolu] の **gfork** ブランチを使ってみると良いかもしれません ([フォーマット結果](https://github.com/google/ormolu/commit/5a36b8b6ef85b587bbf6e8cd5ecb7754fed7461d))。**fourmolu** も [Add option for leading commas (and expand test suite) #17][fourmolu-pr-17] で設定できるようになってました。

今回はプロジェクトのコードがフォーマットされていることを CI (Github Action) でチェックする簡単な方法を紹介します。

<!--more-->

## ormolu-action

既に [ormolu-action][github-ormolu-actioin] というアクションが提供されているので、これを利用すると簡単です。

オプションを指定しなければ以下の内容を `.github/workflows/format.yml` のような名前で保存すれば完了です。

```yaml
name: format

jobs:
  ormolu:
    runs-on: ubuntu-18.04
    steps:
    - uses: actions/checkout@v2
    - uses: mrkkrp/ormolu-action@v1
```

フォーマットされていない場合は CI のログにフォーマット前後の diff が表示されます。

## 実例

このブログでも導入しており、オプションもいくつか利用しています。

```yaml
name: format

jobs:
  ormolu:
    runs-on: ubuntu-18.04
    steps:
    - uses: actions/checkout@v2
    - uses: mrkkrp/ormolu-action@v1
      with:
        pattern: |
          **/app/**/*.hs
          !quiz
          !sample-code
        extra-args: '-o -XTypeApplications'
```

- `pattern` を使うと **含めたい/除外したい** ディレクトリやファイルを指定できるため、より細かい制御が可能です
- `extra-args` は **ormolu** にそのまま渡されるオプションになります

[hkg-ormolu]: https://hackage.haskell.org/package/ormolu
[github-fourmolu]: https://github.com/parsonsmatt/fourmolu
[github-hls]: https://github.com/haskell/haskell-language-server
[google-ormolu]: https://github.com/google/ormolu
[github-ormolu-actioin]: https://github.com/mrkkrp/ormolu-action
[fourmolu-pr-17]: https://github.com/parsonsmatt/fourmolu/pull/17