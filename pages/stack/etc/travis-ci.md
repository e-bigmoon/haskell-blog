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

#### 参考リンク
* [stack document - Travis CI](https://docs.haskellstack.org/en/stable/travis_ci/)
* [stack document - simple example](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/travis-simple.yml)
* [stack document - complex example](https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/travis-complex.yml)
* [tasty-travis.hsfiles](https://github.com/commercialhaskell/stack-templates/blob/master/tasty-travis.hsfiles)
* [.travis.yml (stack repo)](https://github.com/commercialhaskell/stack/blob/master/.travis.yml)