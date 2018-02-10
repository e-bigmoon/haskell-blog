---
title: CIを回す!
date: 2018/02/10
---


`Haskell` のプロジェクトでよく見る `CI` ツールといえば以下の2つでしょう。

- [Travis CI](https://travis-ci.org/)
- [CircleCI 2.0](https://circleci.com/)

個人的には以下の点で `CircleCI` が好きです。

- プライベートリポジトリも無料で使える
- `Docker`, `docker-compose` と親和性が高い

ここでは `HLint` の内容にしか言及しませんが、機会があれば `CI` については別途記事にしたいと思います。

### Travis CI

`.travis.yml` に以下の内容を記述するだけです。

```yaml
# .travis.yml
sudo: false
language: generic
jobs:
  include:
    - stage: Run hlint
      script: curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
```

### CircleCI 2.0

`.circleci/config.yml` に以下の内容を記述するだけです。

```yaml
version: 2
jobs:
  hlint:
    docker:
      - image: ubuntu:16.04
    steps:
      - checkout
      - run:
          name: Run hlint
          command: |
            apt update
            apt install -y curl
            curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
workflows:
  version: 2
  hlint:
    jobs:
      - hlint
```

## まとめ

今回は紹介していませんが `HLint` のヒントを自動的に適用してくれる [apply-refact](https://github.com/mpickering/apply-refact) というツールもあります。使い方については各種ドキュメントをご確認ください。

- [Automatically Applying Hints](https://github.com/ndmitchell/hlint#automatically-applying-hints)

今回は `Haskell` の静的解析ツール `HLint` について説明を行いました。需要があれば `LiquidHaskell` などの他の静的解析ツールについても、チュートリアル的な解説記事を書いていきたいところです。
