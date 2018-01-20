---
title: Travis CI
---

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

#### エラー集

`stack test`により実行ファイル`site`を作成した後、実行ファイルの動作確認を`Travis CI`で行うため、`.travis.yml`に以下の内容を追記しました。

```yaml
jobs:
  include:
    - stage: stack test
      script: stack --no-terminal test
    - stage: site build test
      script: stack exec site rebuild
```

このように記述した結果、以下のようなエラーで二番目の`stage`が失敗していました。

```
Executable named site not found on path: ["/home/travis/build/wataru86/haskell-blog/.stack-work/install/x86_64-linux/lts-10.1/8.2.2/bin",(中略),"/opt/pyenv/bin","/home/travis/.yarn/bin"]

The command "stack exec site rebuild" exited with 1.
```

Travisにおいてそれぞれの`stage`は独立していてキャッシュは共有されないので、一つ目の`stage`で作成された実行ファイル`site`は二番目の`stage`で参照できずにエラーとなりました。今回のように実行ファイルの作成と実行を行う場合は以下のように`script`を同一の`stage`内に記述しましょう。

```yaml
jobs:
  include:
    - stage: stack test and build site
      script:
        - stack --no-terminal test
        - stack exec site rebuild
```

今回のエラーの全体のログは[こちら](https://travis-ci.org/wataru86/haskell-blog/jobs/325956394)です。

#### 参考リンク
* [stack document - Travis CI](https://docs.haskellstack.org/en/stable/travis_ci/)
* [stack document - simple example](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/travis-simple.yml)
* [stack document - complex example](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/travis-complex.yml)
* [tasty-travis.hsfiles](https://github.com/commercialhaskell/stack-templates/blob/master/tasty-travis.hsfiles)
* [.travis.yml (stack repo)](https://github.com/commercialhaskell/stack/blob/master/.travis.yml)