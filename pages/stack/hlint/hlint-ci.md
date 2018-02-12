---
title: CI を回す!
date: 2018/02/12
---

## Haskell プロジェクトで良く使われている CI

`Haskell` のプロジェクトでよく見る `CI` ツールといえば以下の2つでしょう。

- [Travis CI](https://travis-ci.org/)
- [CircleCI 2.0](https://circleci.com/)

個人的には以下の点で `CircleCI` が好きです。

- プライベートリポジトリも無料で使える
- `Docker`, `docker-compose` と親和性が高い

ここでは `HLint` の内容にしか言及しませんが、機会があれば `CI` については別途記事にしたいと思います。

## Travis CI

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

## CircleCI 2.0

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