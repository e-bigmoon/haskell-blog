---
title: extra-deps に github の短縮形が指定できるようになります
author: Shinya Yamaguchi
tags: bigmoon, stack
updated: 2018/03/14
---

## はじめに

今日マージされた [Allow 'github' shorthand for extra-deps (fixes #3873) #3890](https://github.com/commercialhaskell/stack/pull/3890) がとても便利だと思いますのでご紹介します。

```sh
$ stack --version
Version 1.7.0, Git revision 4a140342f9b28005bf2fdd5335bdcd32c9370265 (5702 commits) x86_64 hpack-0.21.2
```

<!--more-->

`stack.yaml` に記述する `extra-deps` は主に2つの理由で記述することが多いです。

- `lts` に含まれていないパッケージのバージョンを指定するため
- そもそも `Hackage` に上がっていないパッケージを使うため

今回は `extra-deps` の指定方法に `github` 専用の短縮形が導入されました。

こんな感じで指定可能です。(コミットの短縮形はいつから使えていたのかわかりませんが、使えます)

```yaml
extra-deps:
- github: haskell/text
  commit: 9fac5d
```

1.6.5 ではまだ利用できない機能となっております。

## 試し方

```sh
$ stack upgrade --git
# コンパイルするので時間かかります

$ stack --version
Version 1.7.0, Git revision 4a140342f9b28005bf2fdd5335bdcd32c9370265 (5702 commits) x86_64 hpack-0.21.2
```

遊んでから、元の `stack` のバージョンに戻す時は以下のようにするだけです。

```sh
$ stack upgrade --binary-version 1.6.5
...

$ stack --version
Version 1.6.5, Git revision 24ab0d6ff07f28276e082c3ce74dfdeb1a2ca9e9 (5514 commits) x86_64 hpack-0.20.0
# バイナリが落ちてくるのですぐ終わります
```

## まとめ

- `stack 1.7.0` ぐらいから利用できるようになるはず
- `github` が新たに追加され `user/repo` の短縮形で記述できるようになった
- `commit` もいつからか短縮形で記述できるようになっていた
- `subdirs` を指定すれば `Yesod` のような mega-repo でも指定できる

以上です。
