---
title: stack path
date: 2018/09/14
---

## 目的

基本的にはあまり使いませんが、シェルスクリプトから呼び出すことはたまにあります。

`stack` が参照している各種パスを表示することができます。`stack` で問題が発生した際、この内容も教えてあげると解決しやすくなります。

## 使い方

コマンド | 意味 | 実行結果の具体例
---------|---------|--------
`stack path` | 全てのパスを表示 | 長いので省略
`stack path --stack-root` | stack のルートパス | /home/bm12/.stack
`stack path --local-bin` | stack install の保存先 | /home/bm12/.local/bin
