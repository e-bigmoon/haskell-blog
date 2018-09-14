---
title: stack build
date: 2018/09/14
---

## stack build 時に良く利用するオプション

基本的には `--fast` と `--file-watch` を常用していることが多い

```shell
$ stack build --fast --file-watch
```

CI の設定では以下のように `--pedantic` を使ってチェックしています。(`pedantic` オプションを常時使っていると効率が悪いため、CIのみにしています)

```shell
$ stack buld --pedantic --fast
```

### オプションの簡単な説明

オプション | 説明
-----------|--------
`--fast` | 最適化を無効にする
`--file-watch` | ファイルの変更を検知して自動的に再ビルドする
`--pedantic` | GHCの警告をエラーとして報告する

## フラグの指定方法

あまり利用することが無いかもしれませんが、パッケージによってはフラグが指定されていることがあります。

基本的には以下の書式でフラグを有効化できます。

```shell
$ stack build --flag <package_name>:<flag_name>
```

## 並列ビルドについて

`stack` では自動的にビルドが並列化される (現在のCPUのコア数を自動的に設定する) ため、明示的に `-j` オプションを渡す必要はありません。(むしろ、現状は間違ったコア数を渡してしまうとパフォーマスに悪影響を及ぼすバグがあるため、現状は**非推奨**です。)

並列化オプションとして `-jN` が提供されていますが、以下のようにコア数を**抑制したい**場合にのみ、利用した方が良いでしょう。(`-j` はエラーになります)

### 並列化してビルド

```shell
$ stack build
```

### コア数1でビルド

```shell
$ stack build -j1
```

### stack.yaml に設定する方法

```yaml
# stack.yaml
jobs: 1
```

## --docker オプション

自分のプロジェクトを `Docker` 環境内でビルドするためには `--docker` オプションを追加します。

```shell
$ stack build --docker
Error: No such object: fpco/stack-build:lts-12.0
Received ExitFailure 1 when running
Raw command: /usr/bin/docker inspect fpco/stack-build:lts-12.0
Standard output:

[]
```

エラーになってしまいました。これはベースイメージに `fpco/stack-build:lts-12.0` を指定したけど、イメージが見つからなかったのでビルドできなかったよ。というエラーです。

この場合 `stack docker pull` コマンドでイメージを `pull` します。今回は `lts-12.0` のタグがついていますが、利用している `resolver` によって変わります。

```shell
$ stack docker pull
Pulling image from registry: 'fpco/stack-build:lts-12.0'
lts-12.0: Pulling from fpco/stack-build
b234f539f7a1: Pull complete
55172d420b43: Pull complete
5ba5bbeb6b91: Pull complete
43ae2841ad7a: Pull complete
f6c9c6de4190: Pull complete
52b26204f9af: Pull complete
916b6abf261c: Pull complete
048ddb5a0825: Pull complete
db66cbec7a84: Pull complete
7b52cdfbea02: Pull complete
Digest: sha256:fdbabc6df1135ab640041c966a2f8ced3bdaff7226de4a41f52d8c08fc9d64c7
Status: Downloaded newer image for fpco/stack-build:lts-12.0
```

では、もう一度ビルドを試してみましょう。

```shell
$ stack build --docker
...
```

今度は成功しました！しかし、Docker 環境内でビルドできて何が嬉しいの？と思うかもしれません。

[stack image container](./image-container.html) コマンドと組み合わせることにによって、手軽に素早く Docker イメージを作ることができます。