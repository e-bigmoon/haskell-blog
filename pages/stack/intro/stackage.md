---
title: Stackage とは何か？
date: 2019/09/14
prev: why-stack.html
next: stack-install.html
---

[Stackage](https://www.stackage.org/) は **Stable Hackage** の略で、どんな組み合わせでも依存関係でエラーが起きないように調整されたパッケージの集合 (スナップショット) を提供しています。

スナップショットには以下の2種類があります。

- 長期サポート (3ヶ月 ~ 6ヶ月) の **lts (Long Term Support)**
- 日々のスナップショット **nightly**

## スナップショットのバージョン規則

**lts** のバージョンは `X.Y` という形式になっており、`X` をメジャーバージョン, `Y` をマイナーバージョンと呼びます。また、**nightly** のバージョンは `nightly-YYYY-MM-DD` という形式になっています。

バージョンの上がるタイミングと内容は以下のようになっています。

バージョン | タイミング | 例 | 備考
:-----|:-----|:------|:------
メジャーバージョン | 3 ~ 6 ヶ月に一度 | lts-13.0 → lts-14.0 | API の破壊的変更, パッケージの追加, パッケージの削除が行われる
マイナーバージョン | 1週間に一度 (主に日曜日) | lts-14.0 → lts-14.1 | 互換性のある API の変更, パッケージの追加が行われる
nightly | (原則) 毎日 | nightly-2019-09-13 → nightly-2019-09-14 | API の変更, パッケージの追加

**lts** で一度対応してしまえば、マイナーバージョンを上げた際にコードが壊れることは基本的にありません。なので、互換性を維持したまま新しい関数やバグフィックスされた関数などを使うことができます。

これは Haskell のパッケージが [Haskell Package Versioning Policy](https://pvp.haskell.org/) に従っているためです。バージョンの決め方は以下の図の通りです。

![](https://pvp.haskell.org/pvp-decision-tree.svg)

**Stackage** は [Hackage](https://hackage.haskell.org/) にアップロードされたパッケージをミラーしているので、自分の作ったパッケージを **Hackage** にアップロードすると、自動的に **Stackage** にも反映されます。この段階では **lts** にも **nightly** にも含まれません。

もし、自分の作ったパッケージを **lts** や **nightly** に含めたい場合は自分で申請する必要があります。申請方法は簡単で、[stackage リポジトリ](https://github.com/fpco/stackage/pulls)にプルリクエストを投げて、承認されれば完了です。

詳しいことは、[MAINTAINERS.md](https://github.com/fpco/stackage/blob/master/MAINTAINERS.md) にまとまっているため、興味がある方はどうぞ。

## スナップショットに追加されるタイミング

スナップショットに追加される基本的な流れとしては以下の通りです。

1. **Hackage** にパッケージをアップロードする
2. **Stackage** に追加される
3. **nightly** の申請を行い、**nightly** に追加される
4. **lts** のマイナーバージョンアップデートで **lts** に追加される

## スナップショットの指定

スナップショットを **stack.yaml** の **resolver** に指定する (後述) ことで、プロジェクトで利用するパッケージのバージョンを固定することができます。

```yaml
resolver: lts-14.5
```

また、オプションとして渡すこともできます。その場合は `--resolver` オプションを利用します。

```shell
$ stack repl --resolver lts-14.5
```

また、上記では紹介していませんが `ghc-X.Y.Z` のように GHC のバージョンを指定したスナップショットもあります。

```shell
# resolver のスナップショットに lts-14.5 を指定して ghci を起動
$ stack ghci --resolver lts-14.5

# GHC-8.8.1 で ghci を起動
$ stack ghci --resolver ghc-8.8.1

# GHC-8.6.5 で ghci を起動
$ stack ghci --resolver ghc-8.6.5

# 最新の resolver で ghci を起動
$ stack ghci --resolver nightly
$ stack ghci --resolver lts
```

利用可能なスナップショットの一覧は **stack ls snapshots** コマンドで確認することができます。

```shell
$ stack ls snapshots remote
a month ago

Resolver name: lts-14.1
LTS Haskell 14.1 (ghc-8.6.5)

Resolver name: nightly-2019-08-12
Stackage Nightly 2019-08-12 (ghc-8.6.5)
...
```

## カスタムスナップショット

自分でカスタマイズしたスナップショットを定義して利用することもできます。

詳しくは [カスタムスナップショットの紹介](/posts/2017/12-23-stack161.html) をご確認ください。
