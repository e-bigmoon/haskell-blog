---
title: stack image container
date: 2018/09/14
---

このコマンドを利用すると、アプリケーション入りの Docker イメージをコマンド1発で作ることができます。

`--docker` オプションについては [stack build](./build.html) をご確認ください。

## stack.yaml の設定

まずは stack.yaml に以下の設定を追記しましょう。

```yaml
image:
  container:
    name: myapp
    base: "fpco/stack-run"
```

`base` に指定するのは Docker イメージです。

上記の例では、以下のような Dockerfile を生成して、ビルドした Haskell アプリケーションをイメージに追加する感じです。

```dockerfile
FROM fpco/stack-run
ADD ./ /
```

## イメージの作成

以下のコマンドで Docker イメージを作成します。

```shell
$ stack image container --docker
...
Sending build context to Docker daemon  2.014MB
Step 1/2 : FROM fpco/stack-run
 ---> cdabad604832
Step 2/2 : ADD ./ /
 ---> 33e6900547e1
Successfully built 33e6900547e1
Successfully tagged pfad:latest
```

これでアプリケーション入りの Docker イメージが完成しました。

一応出来上がったイメージを確認してみます。

```shell
$ docker images myapp
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
myapp               latest              33e6900547e1        25 seconds ago      1.52GB
```

イメージのサイズが結構やばいですが、これは `stack.yaml` に `base: "fpco/stack-run"` として [fpco/stack-run](https://hub.docker.com/r/fpco/stack-run/tags/) イメージを指定したからです。基本的には自分でカスタマイズしたイメージを利用することになります。

生成されたバイナリファイルはデフォルトでは `/usr/local/bin` に保存されます。

```shell
$ docker run --rm -it myapp ls -l /usr/local/bin
total 1968
-rwxr-xr-x 1 root root 1004528 Jul 16 08:28 myapp
```

Docker イメージ内にバイナリファイルがあるので普通にコマンドを実行できます。

```shell
$ docker run --rm -it myapp
...
```

あとは、この `Docker` イメージを [Docker Hub](https://hub.docker.com/) なりで公開すれば、誰でもすぐに実行できます。

Haskell 製の web アプリケーション をこのような方法で Docker イメージにして `kubernetes` にデプロイするスタイルにすると、めちゃめちゃ楽ですよ。