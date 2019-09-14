---
title: Haddock の基礎知識
date: 2019/09/14
prev: ./index.html
next: ./haddock-comment.html
---

## Haddock の生成

**stack** には **Haddock** 形式と呼ばれる形式でコメントを残すことで、そのコメントを自動的にドキュメントに変換する **stack haddock** コマンドがあります。

まずは特に何もせずに **Haddock** を生成してみましょう。

```shell-session
$ stack haddock
...

Running Haddock on library for PFAD-0.1.0.0..
Haddock coverage:
   0% (  0 /  3) in 'Minfree'
  Missing documentation for:
    Module header
    minfree (src/Minfree.hs:6)
    minfree' (src/Minfree.hs:36)
Documentation created:
.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/doc/html/PFAD/index.html,
.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/doc/html/PFAD/PFAD.txt

...
```

これでドキュメントは生成されましたが、ログによると `.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/doc/html/PFAD/index.html` に生成されているようです。生成されるドキュメントはデフォルトで `.stack-work` 以下の奥深くになっているため、ちょっと確認しづらいです。

そういう場合は、自動的にブラウザで開いてくれる `--open` オプションを使ってみましょう。

```shell
$ stack haddock --open
```

### 出力先ディレクトリの変更

次に、`--haddock-arguments --odir=haddock` オプションで、出力先ディレクトリを変更してみましょう。

```shell-session
$ stack haddock --haddock-arguments --odir=haddock
$ tree -L 1
.
├── ChangeLog.md
├── LICENSE
├── PFAD.cabal
├── README.md
├── Setup.hs
├── app
├── package.yaml
├── src
├── stack.yaml
├── stack.yaml.lock
└── test
```

**haddock** ディレクトリができると思いきや、何も起こりませんね。これはビルドキャッシュが残っているためです。一度 **stack clean** をしてからもう一度実行してみましょう。

```shell-session
$ stack clean
$ stack haddock --haddock-arguments --odir=haddock
$ tree -L 1
.
├── ChangeLog.md
├── LICENSE
├── PFAD.cabal
├── README.md
├── Setup.hs
├── app
├── haddock
├── package.yaml
├── src
├── stack.yaml
├── stack.yaml.lock
└── test
```

**haddock/index.html** をブラウザで確認すると、こんな感じで **Hackage** と同じようなドキュメントが生成されているはずです。

![](/images/haddock01.jpeg)
![](/images/haddock02.jpeg)

ここまでで基本的なドキュメントの生成方法はわかりました。

気にしておいて欲しい点としては公開 (**expose**) されている関数のみがドキュメント化されるという点です。**Haddock** 形式のコメントを使っていなくても **expose** されている関数や型は、自動的に情報が公開されることを理解しておきましょう。

次は **Haddock** 形式のコメントを追加してどんどんドキュメントをリッチにしていきましょう！
