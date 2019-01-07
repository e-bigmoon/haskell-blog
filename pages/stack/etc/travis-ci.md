---
title: Travis CI
date: 2019/01/07
---

## テンプレート

以下の内容は汎用的な `travis` の設定です。`.travis.yml` としてプロジェクトのルートに保存すれば動きます。

```yaml
language: haskell
dist: xenial

cache:
  directories:
    - $HOME/.cabal/packages
    - $HOME/.cabal/store
    - $HOME/.stack
    - $TRAVIS_BUILD_DIR/.stack-work

cabal: "2.4"
env:
  global:
    - STACK_VERSION=1.9.3
    - HPACK_VERSION=0.31.1

matrix:
  fast_finish: true
  include:
    - env: BUILD=cabal
      ghc: "8.6.3"

    - env: BUILD=cabal
      ghc: "8.4.4"

    - env: BUILD=cabal
      ghc: "8.2.2"

      # --system-ghc is used to take advantage of language: haskell and save build time. If the stack resolver ghc and system-ghc become different, stack will download the right ghc
    - env: BUILD=stack ARGS="--resolver lts-11 --system-ghc"
      ghc: "8.2.2"

    - env: BUILD=stack ARGS="--resolver lts-12 --system-ghc"
      ghc: "8.4.4"

    - env: BUILD=stack ARGS="--system-ghc"
      ghc: "8.6.3"

      # nightly build
    - env: BUILD=stack ARGS="--resolver nightly --system-ghc"
      ghc: "8.6.3"

    - env: BUILD=style
      ghc: "8.6.3"

    - env: BUILD=pedantic ARGS="--system-ghc --pedantic"
      ghc: "8.6.3"

  allow_failures:
    - env: BUILD=stack ARGS="--resolver nightly --system-ghc"
    - env: BUILD=style
    - env: BUILD=pedantic ARGS="--system-ghc --pedantic"

before_install:
  - case "$BUILD" in
      style)
        export PATH="$TRAVIS_BUILD_DIR"/hlint:$PATH
        ;;
      cabal)
        export PATH="$HOME"/.cabal/bin:$PATH
        ;;
      *)
        export PATH="$HOME"/.local/bin:$PATH
        ;;
    esac

install:
- |
  set -ex
  case "$BUILD" in
    style)
      curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s -- --version
      ;;
    cabal)
      mkdir -p $HOME/.cabal/bin
      
      curl --progress-bar --location -o hpack.gz "https://github.com/sol/hpack/releases/download/$HPACK_VERSION/hpack_linux.gz"
      gzip -d hpack.gz
      chmod u+x ./hpack
      mv hpack $HOME/.cabal/bin/
      
      hpack .

      cabal --numeric-version
      hpack --version
      ghc --version
      ;;
    *)
      mkdir -p $HOME/.local/bin

      travis_retry curl -sL https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

      stack upgrade --binary-version=$STACK_VERSION

      stack --numeric-version
      stack --hpack-numeric-version
      ghc --version
      ;;
  esac
  set +ex

script:
- |
  set -ex
  case "$BUILD" in
    style)
      curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
      ;;
    cabal)
      cabal new-update
      cabal new-test all
      ;;
    stack)
      stack --no-terminal $ARGS test --haddock --no-haddock-deps
      ;;
    pedantic)
      stack --no-terminal $ARGS test --no-run-tests
      ;;
  esac
  set +ex

notifications:
  email: false
```

上記の設定内容でチェック出来る項目

- テスト (ビルド) が複数の GHC で通るかどうか (stack, cabal)
- GHC の警告が出ていないかどうか (pedantic)
- HLint のヒントが出ていないかどうか (style)

## エラー集

### ジョブ間で成果物が共有されない問題

`stack test` により実行ファイル `site` を作成した後、実行ファイルの動作確認を `Travis CI` で行うため、`.travis.yml` に以下の内容を追記しました。

```yaml
jobs:
  include:
    - stage: stack test
      script: stack --no-terminal test
    - stage: site build test
      script: stack exec site rebuild
```

このように記述した結果、以下のようなエラーで二番目の`stage`が失敗していました。

```shell
Executable named site not found on path: ["/home/travis/build/wataru86/haskell-blog/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/bin",(中略),"/opt/pyenv/bin","/home/travis/.yarn/bin"]

The command "stack exec site rebuild" exited with 1.
```

一番目の `stage` のログを見てみると、以下のようにキャッシュの処理が最大時間である180秒を超えていたため途中で止まってしまったことがわかりました。

```shell
running `casher push` took longer than 180 seconds and has been aborted.
You can extend the timeout with `cache.timeout`. See https://docs.travis-ci.com/user/caching/#Setting-the-timeout for details
```

キャッシュの保存はそれぞれの `stage` の最後に行われるので、 `.travis.yml` を以下のように `stage` の分割を行い、さらにキャッシュの最大時間を2倍(360秒)にすることで解決しました。

```yaml
cache:
  timeout: 360

（中略）

jobs:
  include:
    - stage: install cabal
      script: stack --no-terminal build -j 1 Cabal
    - stage: install pandoc
      script: stack --no-terminal build pandoc
    - stage: install deprndences
      script: stack --no-terminal test --only-dependencies
    - stage: stack test
      script: stack --no-terminal test
    - stage: rebuild site
    - script: stack exec site rebuild
```

今回のエラーの全体のログは[こちら](https://travis-ci.org/wataru86/haskell-blog/jobs/325956394)です。

## 参考リンク

- [stack document - Travis CI](https://docs.haskellstack.org/en/stable/travis_ci/)
- [stack document - simple example](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/travis-simple.yml)
- [stack document - complex example](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/travis-complex.yml)
- [tasty-travis.hsfiles](https://github.com/commercialhaskell/stack-templates/blob/master/tasty-travis.hsfiles)
- [.travis.yml (stack repo)](https://github.com/commercialhaskell/stack/blob/master/.travis.yml)
- [tonyday567/.travis.yml](https://gist.github.com/tonyday567/e6cf9f3e2010ca73b511bb175c4f7d98)
