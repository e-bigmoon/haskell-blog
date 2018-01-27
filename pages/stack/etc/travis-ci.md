---
title: Travis CI
date: 2018/01/20
---

## テンプレート

以下の内容は汎用的な `travis` の設定です。`.travis.yml` としてプロジェクトのルートに保存すれば動きます。

```yaml
sudo: false
language: generic
cache:
  directories:
  - "$HOME/.stack/"
  - "$HOME/.local/bin/"
  - ".stack-work/"
before_install:
- mkdir -p ~/.local/bin
- export PATH=$HOME/.local/bin:$PATH
- travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
install:
- stack --no-terminal test --only-dependencies
jobs:
  include:
    - stage: stack test
      script: stack --no-terminal test
```

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