---
title: Lambdabot for Slack
author: Shinya Yamaguchi
tags: bigmoon
---

## はじめに

最近 slack で [Lambdabot](https://github.com/lambdabot/lambdabot) というツールを知りました。こいつを導入するとこんな事ができます。

![Lambdabot と遊んでいる図](/images/2019/02-11/1.png)

実務で本格的に使えるものではありませんが、入れてみると楽しいですよ。(カスタマイズして改良すれば、かなり使えるかもしれない)

<!--more-->

## Lambdabot の導入

導入はとても簡単で slack app から Lambdabot をインストールするだけです！

![Slack app で検索](/images/2019/02-11/2.png)

![Lambdabot のインストール画面](/images/2019/02-11/3.png)

検索するのが面倒な人は [Add Lambdabot to Slack](https://lambdabot.brianmckenna.org/slack/install.html) の App ボタンからもインストール可能です。

アプリのソースコード等は [markandrus/slack-lambdabot](https://github.com/markandrus/slack-lambdabot) にありますので、気になる方はこちらもご参照ください。

自分でビルドしてホスティングする場合は fork の [ezoerner/slack-lambdabot](https://github.com/ezoerner/slack-lambdabot) の方が良いかもしれません。(要検証)

## Lambdabot for Slack でできること

どうやら本家の Lambdabot の機能全てが使えるわけではないようです。

また、利用可能な機能のうち `type` や `hoogle`等はちゃんと使えるようですが、`run` や `check` などは結果がおかしい場合があります。

### hoogle

どうやら結果は3件しか返さないようです。

![関数名で検索](/images/2019/02-11/hoogle-1.png)

![型名で検索](/images/2019/02-11/hoogle-2.png)

![型で検索 (型変数有り)](/images/2019/02-11/hoogle-3.png)

![型で検索 (型変数無し)](/images/2019/02-11/hoogle-4.png)

### type

どのパッケージが含まれているかわかりませんが、lens の演算子を試したところだめでした。

![関数](/images/2019/02-11/type-1.png)

![メソッド](/images/2019/02-11/type-2.png)

![複雑な式](/images/2019/02-11/type-3.png)

![演算子](/images/2019/02-11/type-4.png)

![セクション](/images/2019/02-11/type-5.png)

![エラー](/images/2019/02-11/type-6.png)

### run

式を実行することができるようですが、全然動かないです。

![print はエラー](/images/2019/02-11/run-1.png)

![奇跡的に動いた例](/images/2019/02-11/run-2.png)

![文字列を含む場合にはエラーになる](/images/2019/02-11/run-3.png)

![無限リストは適当に打ち切り](/images/2019/02-11/run-4.png)

### check

QuickCheck も実行可能ですが、上手く行く例が作れません・・・。

![実行時間の制約に引っかかってるっぽい](/images/2019/02-11/check-1.png)

### free

たぶん実装は [FreeTheorem.hs](https://github.com/lambdabot/lambdabot/blob/0b26cb6ca0e0389bcff68d360c18775a787c9d52/lambdabot-haskell-plugins/src/Lambdabot/Plugin/Haskell/Free/FreeTheorem.hs) です。

良くわからないので解説できません。実行結果だけ貼っておきます。

![reverse](/images/2019/02-11/free-1.png)

![const](/images/2019/02-11/free-2.png)

![id](/images/2019/02-11/free-3.png)

![flip](/images/2019/02-11/free-4.png)

![filter](/images/2019/02-11/free-5.png)

### pl

ポイントフリーにしてくれます。また fusion もいくつか可能っぽいです。

![ポイントフリー化1](/images/2019/02-11/pl-1.png)

![ポイントフリー化2](/images/2019/02-11/pl-2.png)

![f . id = id](/images/2019/02-11/pl-3.png)

![map f . map g = map (f . g)](/images/2019/02-11/pl-4.png)

## 終わりに

ちゃんと作り込んだら結構使えそうな気がします。