---
title: Circle CI 2.0 の設定
author: Shinya Yamaguchi
tags: bigmoon
updated: 2018/07/23
---

## はじめに

**Haskell** プロジェクトの多くは **Travis CI** を使って CI を回しています。

しかしここ最近、いくつかのプロジェクトで **Circle CI** の利用が進んでいるように思います。

僕も社内のプロジェクトでは **Circle CI** を使っています。実際に **Circle CI** を使っていて個人的に良いなと感じたのは以下の4点です。

- docker イメージを指定できる
- プライベートリポジトリで利用できる
- travis より速い気がする
- キャッシュが不変

キャッシュの動作に関しては **travis** とは逆なので少し違和感があるかもしれませんが、キャッシュでCIが失敗するということが無くなるので、非常に良いと思います。

今回は Haskell プロジェクトで汎用的に使える **Circle CI** の設定をご紹介したいと思います。ただ、開発が進むにつれて色々とカスタマイズする必要が出てくると思いますので、今回参考にした設定ファイル等のリンクを参考資料として載せておきます。現実的に利用されている設定なので非常に役立つと思います。

参考にした `config.yml`

- [ghc/ghc](https://github.com/ghc/ghc/blob/master/.circleci/config.yml)
- [haskell/haskell-ide-engine](https://github.com/haskell/haskell-ide-engine/blob/master/.circleci/config.yml)
- [haskell-jp/blog](https://github.com/haskell-jp/blog/blob/master/.circleci/config.yml)
- [restyled-io/restyled.io](https://github.com/restyled-io/restyled.io/blob/master/.circleci/config.yml)
- [pbrisbin/hs-shellwords](https://github.com/pbrisbin/hs-shellwords/blob/master/.circleci/config.yml)
- [haskell-works/stack-build](https://github.com/haskell-works/stack-build)

参考にした記事

- [Dockerizing our Haskell App](https://mmhaskell.com/blog/2018/4/25/dockerizing-our-haskell-app)
- [Configuring CircleCI](https://circleci.com/docs/2.0/configuration-reference/)
- [CircleCI2.0でHaskellのテストを実行する](https://tech.recruit-mp.co.jp/dev-tools/post-13981/)

<!--more-->

## 注意点

**Circle CI 2.0** の設定ファイルは `.circleci/config.yml` という名前でなければなりません。

config.**yaml** にすると認識しないので注意しましょう。(これで何時間か無駄にしたことがあります)

## シンプルな設定

[config.yml](https://github.com/waddlaw/circleci-sandobx/blob/simple-fast/.circleci/config.yml) にシンプルな設定例を置いてあります。

CI に含めたい内容は人それぞれだと思いますが、ここでは以下の4種類を検査できるようにします。

- stack test
- stack test --pedantic
- hlint
- stylish-haskell

また、以下の要件を満たすようにします。

- stack のバージョンを柔軟に切り替えたい
- キャッシュを強制的にクリアしたい
- 一度設定したら、できるだけ設定ファイルをいじらない

### config.yml

完全な **config.yml** は以下になります。これをコピペでプロジェクトの **.circleci/config.yml** に保存すればすぐに動きます！

```yaml
version: 2

aliases:
  - &default_env
    environment:
      CACHE_KEY: 1
      STACK_VERSION: 1.7.1
  - &create_cache_key_file
    run:
      name: Create cache control key file
      command: echo $CACHE_KEY > cache_key
  - &restore_build_results
    restore_cache:
      keys:
        - stack-{{ checksum "cache_key" }}-{{ checksum "stack.yaml" }}-{{ checksum "package.yaml" }}
        - stack-{{ checksum "cache_key" }}-{{ checksum "stack.yaml" }}
        - stack-{{ checksum "cache_key" }}
  - &save_build_results
      save_cache:
        key: stack-{{ checksum "cache_key" }}-{{ checksum "stack.yaml" }}-{{ checksum "package.yaml" }}
        paths:
          - ~/.stack
          - ~/.local/bin
          - .stack-work
  - &display_stack_version
    run:
      name: Display stack version
      command: |
        stack upgrade --binary-version=$STACK_VERSION
        stack --version
jobs:
  build:
    docker:
      - image: quay.io/haskell_works/stack-build-minimal
    <<: *default_env
    steps:
      - checkout
      - *create_cache_key_file
      - *restore_build_results
      - *display_stack_version
      - run:
          name: Install dependencies
          command: stack test -j 1 --only-dependencies --no-terminal --no-run-tests
          no_output_timeout: 120m
      - run:
          name: Run stack test
          command: stack test --fast
      - run:
          name: Run stack test --pedantic
          command: |
            stack clean
            stack test --pedantic --fast --no-run-tests
      - run:
          name: Run HLint
          command: |
            curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s -- --version
            curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
      - run:
          name: Run stylish-haskell
          command: |
            # TODO: https://github.com/jaspervdj/stylish-haskell/pull/218
            curl -sL https://raw.githubusercontent.com/waddlaw/stylish-haskell/master/scripts/latest.sh | sh -s -- -i $(find . -type f -name "*hs" -not -path '.git' -not -path '*.stack-work*')
            git --no-pager diff --exit-code
      - *save_build_results
```

それぞれの内容について個別に説明します。

#### default_env

```yaml
aliases:
  - &default_env
    environment:
      CACHE_KEY: 1
      STACK_VERSION: 1.7.1
```

**Circle CI** のキャッシュは不変なので一度作られると上書きできません。また、**travis** のようにキャッシュのクリアボタンもありません。

そのため、キャッシュキーの指定に **Circle CI** の環境変数を利用するというやり方がよく用いられます。個人的には管理画面でいちいち変更するのは面倒なので **config.yml** に含めてしまえば良いかなと思います。

なので **CACHE_KEY** は、明示的にキャッシュをクリアする際に利用します。

**STACK_VERSION** は利用する **stack** のバージョンを指定するためにあります。基本的に最新版を指定しておけば良いのですが、更新された直後など、古いバージョンを指定したい場合があるので環境変数として用意しています。

#### create_cache_key_file

```yaml
  - &create_cache_key_file
    run:
      name: Create cache control key file
      command: echo $CACHE_KEY > cache_key
```

このやり方は [haskell/haskell-ide-engine](https://github.com/haskell/haskell-ide-engine/blob/master/.circleci/config.yml) を参考にしました。

キャッシュキーに直接環境変数を指定できれば良いのですが、そのような方法が無いためこのように一度ファイルに書き出しています。

#### restore_build_results

```yaml
- &restore_build_results
    restore_cache:
      keys:
        - stack-{{ checksum "cache_key" }}-{{ checksum "stack.yaml" }}-{{ checksum "package.yaml" }}
        - stack-{{ checksum "cache_key" }}-{{ checksum "stack.yaml" }}
        - stack-{{ checksum "cache_key" }}
```

キャッシュの復元部分です。よくあるイディオムですが

1. cache_key
1. stack.yaml
1. package.yaml

の順番でキャッシュが残るようにしています。

また `stack-{{ checksum "cache_key" }}-{{ checksum "stack.yaml" }}-{{ checksum "package.yaml" }}` だけの設定ですと、**package.yaml** を更新した際にキャッシュがヒットしないため、依存関係のインストールから再度始めることになってしまいます。

#### save_build_results

```yaml
  - &save_build_results
      save_cache:
        key: stack-{{ checksum "cache_key" }}-{{ checksum "stack.yaml" }}-{{ checksum "package.yaml" }}
        paths:
          - ~/.stack
          - ~/.local/bin
          - .stack-work
```

ここがキャッシュを保存している部分です。依存関係を毎回インストールしなくても良いように `~/.stack` と `.stack-work` を指定しています。

`~/.local/bin` はどちらでも良いような気がします。

#### display_stack_version

```yaml
- &display_stack_version
    run:
      name: Display stack version
      command: |
        stack upgrade --binary-version=$STACK_VERSION
        stack --version
```

**stack** のバージョンを強制的に `$STACK_VERSION` にします。

また、ちゃんと期待しているバージョンが利用されているか確認するためにバージョンを表示しています。

#### build image

```yaml
  build:
    docker:
      - image: quay.io/haskell_works/stack-build-minimal
```

どのイメージを利用するかは人それぞれです。**fpco/stack-build:lts** でも良いですが、イメージの pull に3分かかるのでお勧めしません。

そのため、ここでは **quay.io/haskell_works/stack-build-minimal** を利用しています。hie でも利用しているので問題無いでしょう。

Docker ファイルの内容については以下のリポジトリで確認ができます。

- [haskell-works/stack-build](https://github.com/haskell-works/stack-build)

#### 依存関係のインストール

```yaml
      - run:
          name: Install dependencies
          command: stack test -j 1 --only-dependencies --no-terminal --no-run-tests
          no_output_timeout: 120m
```

コマンドのオプションはそれぞれ以下のために利用しています。

- `-j 1`: ビルド時のメモリ不足エラーを回避するため
- `--only-dependencies`: ここでは依存関係のみをインストールしたいので
- `--no-terminal`: 詳細表示は不必要なので
- `--no-run-tests`: そのままではテストが実行されるため

また `no_output_timeout: 120m` はデフォルトのビルド制限時間 (20分) を回避するために追加しています。

#### stack test

```yaml
      - run:
          name: Run stack test
          command: stack test --fast
```

単純にテストを実行します。なんとなく `--fast` をつけています。

#### pedantic

```yaml
      - run:
          name: Run stack test --pedantic
          command: |
            stack clean
            stack test --pedantic --fast --no-run-tests
```

`stack test --pedantic --fast` としておけばテストと両方できて良いんじゃないの？と思われるかもしれませんが、おすすめしません。実際にやってみればわかりますが、めっちゃイライラします。

**pedantic** は必ずテストと分離した方が良いです。`--no-run-tests` オプションを付けているので、テストの実行は行われません。

また、ビルドのキャッシュが残っていると **pedantic** の警告がスルーされることがあるので **stack clean** を事前に行っておいた方が良いでしょう。

#### hlint

```yaml
      - run:
          name: Run HLint
          command: |
            curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s -- --version
            curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
```

**hint** はバージョンによって動作が結構違うので、バージョンを表示させておくと便利です。

#### stylish-haskell

```yaml
      - run:
          name: Run stylish-haskell
          command: |
            curl -sL https://raw.githubusercontent.com/jaspervdj/stylish-haskell/master/scripts/latest.sh | sh -s -- -i $(find . -type f -name "*hs" -not -path '.git' -not -path '*.stack-work*')
            git --no-pager diff --exit-code
```

**stylish-haskell** もチェックしたい人向けです。必要なければ削除しましょう。

スクリプトがバグってたので修正しました。やっていることとしては、

1. 全ての **.hs** ファイルに対して **stylish-haskell** を実行
1. もし、整形の必要があれば **git diff** の結果として出力される

という感じです。

## ワークフローバージョン

- [config.yml](https://github.com/waddlaw/circleci-sandobx/blob/simple-fast-workflows/.circleci/config.yml)

個人的にはワークフローにしておく方が好きなので、ワークフローバージョンも掲載しておきます。シンプルバージョンと内容は同じです。

```yaml
version: 2

aliases:
  - &default_env
    environment:
      CACHE_KEY: 7
      STACK_VERSION: 1.7.1
  - &create_cache_key_file
    run:
      name: Create cache control key file
      command: echo $CACHE_KEY > cache_key
  - &restore_build_results
    restore_cache:
      keys:
        - stack-{{ checksum "cache_key" }}-{{ checksum "stack.yaml" }}-{{ checksum "package.yaml" }}
        - stack-{{ checksum "cache_key" }}-{{ checksum "stack.yaml" }}
        - stack-{{ checksum "cache_key" }}
  - &save_build_results
      save_cache:
        key: stack-{{ checksum "cache_key" }}-{{ checksum "stack.yaml" }}-{{ checksum "package.yaml" }}
        paths:
          - ~/.stack
          - ~/.local/bin
          - .stack-work
  - &display_stack_version
    run:
      name: Display stack version
      command: |
        stack upgrade --binary-version=$STACK_VERSION
        stack --version
jobs:
  build:
    docker:
      - image: quay.io/haskell_works/stack-build-minimal
    <<: *default_env
    steps:
      - checkout
      - *create_cache_key_file
      - *restore_build_results
      - *display_stack_version
      - run:
          name: Install dependencies
          command: stack test -j 1 --only-dependencies --no-terminal --no-run-tests
          no_output_timeout: 120m
      - run:
          name: Run stack test
          command: stack test --fast
      - *save_build_results

  pedantic:
    docker:
      - image: quay.io/haskell_works/stack-build-minimal
    <<: *default_env
    steps:
      - checkout
      - *create_cache_key_file
      - *restore_build_results
      - *display_stack_version
      - run:
          name: Run stack test --pedantic
          command: |
            stack clean
            stack test --pedantic --fast --no-run-tests
  check:
    docker:
      - image: quay.io/haskell_works/stack-build-minimal
    steps:
      - checkout
      - run:
          name: Run HLint
          command: |
            curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s -- --version
            curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s .
      - run:
          name: Run stylish-haskell
          command: |
            curl -sL https://raw.githubusercontent.com/jaspervdj/stylish-haskell/master/scripts/latest.sh | sh -s -- -i $(find . -type f -name "*hs" -not -path '.git' -not -path '*.stack-work*')
            git --no-pager diff --exit-code
workflows:
  version: 2
  test-check:
    jobs:
      - build
      - pedantic:
          requires:
            - build
      - check
```

## まとめ

Circle CI めっちゃ便利です。

**docker-compose** と連携させればデータベースと接続してテストすることもできたりするので良いですよ。

以上です。
