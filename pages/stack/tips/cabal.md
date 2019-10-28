---
title: cabal コマンドとの対応表
date: 2019/03/03
---

## 注意点

このページの内容は `cabal HEAD` を追っているため、最新版の `cabal` ではまだ利用不可能な内容も含まれる場合があります。

## stack と cabal

```shell
$ stack --numeric-version
1.9.3

$ cabal --numeric-version
2.4.1.0
```

　| stack | cabal
-----|------|------
ドキュメント | [stack user guide](https://docs.haskellstack.org/en/stable/README/) | [Cabal User Guide](https://www.haskell.org/cabal/users-guide/)
repo | [commercialhaskell/stack](https://github.com/commercialhaskell/stack/tree/stable) | [haskell/cabal](https://github.com/haskell/cabal)
コマンド名 | stack | cabal (正式には cabal-install と呼ばれているもの)

## stack と cabal の違い

- `extra-lib-dirs` や `extra-include-dirs` などで依存するライブラリが変化した時
  - stack: `~/.stack/snapshots/` 以下を手動で削除し、リビルドする必要がある
  - cabal: `nix-style` を採用しているため、リビルドするだけで良い
- パッケージをリビルドするタイミング
  - stack: `resolver` が変化した際に、パッケージを新たに全てリビルドする必要がある
  - cabal: `compiler` が変化した際に、パッケージを新たに全てリビルドする必要がある
  
### 個人的な意見

- stack はエラーメッセージ等が初心者に優しい (ユーザフレンドリー)
- cabal は stack よりも明らかにビルドが速い

## v1-build と v2-build コマンドの違い

- `cabal v2-build` は [Nix-style Local Builds](https://cabal.readthedocs.io/en/latest/nix-local-build-overview.html)
- `cabal build` は `cabal v1-build` のエイリアス
- `cabal new-build` は `cabal v2-build` のエイリアス
- `cabal v3.0.0.0` からは `cabal v2-build` が `cabal build` のエイリアスになる予定 [#5800](https://github.com/haskell/cabal/pull/5800)

## コマンド対応表

stack | cabal | 備考
------|--------|-------
`stack init` | `cabal init -n --is-executable` <br> `cabal init --simple` <br> `cabal init --lib` <br> `cabal init --exe` <br> `cabal init --libandexe` | [#5707](https://github.com/haskell/cabal/pull/5707), [#5759](https://github.com/haskell/cabal/pull/5759), [#5864](https://github.com/haskell/cabal/pull/5864)
`stack build` | `cabal new-build`
`stack build --static` | `cabal new-build --enable-executable-static` | [#5446](https://github.com/haskell/cabal/pull/5446)
`stack test` | `cabal new-test --enable-tests` <br> `cabal new-test all` | [#5079](https://github.com/haskell/cabal/issues/5079)
`stack repl` <br> `stack ghci` | `cabal new-repl`
`stack repl --package <pkg1> <pkg2>` | `cabal new-repl --build-dep <pkg1>, <pkg2>` | [#5845](https://github.com/haskell/cabal/pull/5845)
`stack clean` | `cabal new-clean`
`stack run` | `cabal new-run`
`stack --version` | `cabal --version`
`stack --numeric-version` | `cabal --numeric-version`
`stack upgrade` | `cabal new-install cabal-install`<br>`cabal new-install cabal-install --overwrite-policy=always`
 ? | `cabal new-haddock`
 ? | `cabal new-sdist`
 ? | `cabal check`
 ? | `cabal upload` <br> `cabal upload --publish`
`stack update` | `cabal new-update`

### どちらか一方にしかないコマンドやオプション

stack | cabal | 備考
------|--------|-------
`stack build --file-watch` | [#5252](https://github.com/haskell/cabal/issues/5252)
- | `cabal format` | [#2460](https://github.com/haskell/cabal/issues/2460), [#5306](https://github.com/haskell/cabal/issues/5306), [#5734](https://github.com/haskell/cabal/issues/5734)
`stack path` | [#3850](https://github.com/haskell/cabal/issues/3850), [#4661](https://github.com/haskell/cabal/issues/4661) |
`stack setup` | _ | [ghcup](https://github.com/haskell/ghcup) を利用する

## 設定ファイル等のパス対応表 (初期値)

stack | cabal
------|-------
`~/.stack/` | `~/.cabal/`
`~/.stack/config.yaml`, `~/.stack/global-project/stack.yaml` | `~/.cabal/config`

## Tips

### コマンドの自動補完

#### stack

- [Shell Auto-completion](https://docs.haskellstack.org/en/stable/shell_autocompletion/#shell-auto-completion)

```shell
$ echo 'eval "$(stack --bash-completion-script stack)"' >> ~/.bashrc
```

#### cabal

- [bash-completion script](https://github.com/haskell/cabal/blob/master/cabal-install/bash-completion/cabal)

```shell
$ curl https://raw.githubusercontent.com/haskell/cabal/master/cabal-install/bash-completion/cabal -O
$ sudo mv cabal /usr/share/bash-completion/completions/
```

### マルチパッケージプロジェクト

#### stack

```shell
# stack.yaml
packages:
- .
- ./package1
- ./package2
- ./package3
```

実行方法

```shell
# 全てのパッケージ
$ stack build
$ stack repl

# 個別のパッケージ
$ stack build package1
$ stack repl package1
```

#### cabal

```shell
-- cabal.project
packages:
  ./
  package1
  package2
  package3
```

実行方法

```shell
# 全てのパッケージ
$ cabal new-build all
$ cabal new-repl

# 個別のパッケージ
$ cabal new-build package1
$ cabal new-repl basic
```

### リモートリポジトリ

#### stack

- [Git and Mercurial repos](https://docs.haskellstack.org/en/stable/yaml_configuration/#git-and-mercurial-repos)

```yaml
# stack.yaml
extra-deps:
- github: fumieval/extensible
  commit: 81ccac73f7480ea66e6008e660972bfee9e83976
```

複数パッケージになっている場合は `subdirs` を指定する

```yaml
# stack.yaml
extra-deps:
- github: gtk2hs/gtk2hs
  commit: 7bccd432e2f962d80b2b804fa2a59712e402753c
  subdirs:
  - cairo
```

複数のパッケージを指定する場合は `subdirs` に追加するだけで良い。

```yaml
# stack.yaml
extra-deps:
- github: gtk2hs/gtk2hs
  commit: 7bccd432e2f962d80b2b804fa2a59712e402753c
  subdirs:
  - cairo
  - gtk
```

#### cabal

- [Specifying Packages from Remote Version Control Locations](https://cabal.readthedocs.io/en/latest/nix-local-build.html#specifying-packages-from-remote-version-control-locations)
- リポジトリに `cabal` ファイルが含まれていない場合は `NoCabalFileFound` となる。リポジトリに含めるしかない。
  - [Specifying remote Git repositories without .cabal file? #5785](https://github.com/haskell/cabal/issues/5785)

```cabal
# cabal.project
source-repository-package
  type: git
  location: https://github.com/fumieval/extensible
  tag: 81ccac73f7480ea66e6008e660972bfee9e83976
```

リポジトリにパッケージが複数含まれる場合は `subdir` を指定する

```cabal
# cabal.project
source-repository-package
  type: git
  location: https://github.com/gtk2hs/gtk2hs
  tag: 7bccd432e2f962d80b2b804fa2a59712e402753c
  subdir: cairo
```

複数のパッケージを指定する場合は `stack` の `subdirs` のような書き方はできないため、`source-repository-package` を複数記述する。([#5472](https://github.com/haskell/cabal/issues/5472))

```cabal
# cabal.project
source-repository-package
  type: git
  location: https://github.com/gtk2hs/gtk2hs
  tag: 7bccd432e2f962d80b2b804fa2a59712e402753c
  subdir: cairo
  
source-repository-package
  type: git
  location: https://github.com/gtk2hs/gtk2hs
  tag: 7bccd432e2f962d80b2b804fa2a59712e402753c
  subdir: gtk
```

### プロファイリング

#### stack

```shell
$ stack build --profile
```

#### cabal

```
profiling: True
```

`cabal.project.local` に上記の内容を追加し、`cabal new-build` すれば良い。

- [How can I profile my library/application?](https://cabal.readthedocs.io/en/latest/nix-local-build.html#how-can-i-profile-my-library-application)

### プロジェクトで利用する ghc を強制する方法

#### cabal

`cabal.project` に `with-compiler` を追記する

```
with-compiler: ghc-8.6.2
```

### build-depends で利用するバージョン制約の指定方法

- [3.3.2.9. Build information](https://www.haskell.org/cabal/users-guide/developing-packages.html#build-information)

利用可能な演算子 | 記号
------|--------
比較演算子 | `==`, `>=`, `>`, `<`, `<=`, `^>=`
条件演算子 | `&&`, `\|\|`

#### 良くある指定方法

```cabal
pkgname >= n
pkgname ^>= n
pkgname >= n && < m
pkgname == n.*
```

#### 省略形

省略形 | 展開後
-------|--------
`foo == 4.*`      | `foo >= 4 && < 5` 
`foo == 1.2.*`    | `foo >= 1.2 && < 1.3`
`foo ^>= x`       | `foo >= x && < x.1`
`foo ^>= x.y`     | `foo >= x.y && < x.(y+1)`
`foo ^>= x.y.z`   | `foo >= x.y.z && < x.(y+1)`
`foo ^>= x.y.z.u` | `foo >= x.y.z.u && < x.(y+1)`

#### 備考

- ワイルドカードは `Cabal 1.6` から利用可能
- `^>=` は `Cabal 2.0` から利用可能 (キャレット)
- `foo == 1.0.*` は `foo-1` にはマッチしないので注意 (`^>=` の利用を推奨)

### 集合記法

- Cabal 3.0 から使える
- `==` と `^>=` 演算子が対応している
- [caret operator accepts a set of versions #5906](https://github.com/haskell/cabal/pull/5906)

```
tested-with: GHC == 8.6.3, GHC == 8.4.4, GHC == 8.2.2, GHC == 8.0.2,
             GHC == 7.10.3, GHC == 7.8.4, GHC == 7.6.3, GHC == 7.4.2

build-depends: network ^>= 2.6.3.6 || ^>= 2.7.0.2 || ^>= 2.8.0.0 || ^>= 3.0.1.0
```

こんな感じで書き直せる

```
tested-with: GHC == { 8.6.3, 8.4.4, 8.2.2, 8.0.2, 7.10.3, 7.8.4, 7.6.3, 7.4.2 }

build-depends: network ^>= { 2.6.3.6, 2.7.0.2, 2.8.0.0, 3.0.1.0 }
```

### cabal.project.local の設定

```shell
$ cabal new-configure -j
```

### スクリプト形式

#### stack

- [script interpreter + stack script でスクリプティング！](https://haskell.e-bigmoon.com/stack/tips/script-interpreter.html)

```haskell
#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 HelloWorld
```

実行方法

```shell
$ stack script --resolver lts-13.1 Script.hs
```

#### cabal

- [5.4.5. cabal new-run](https://www.haskell.org/cabal/users-guide/nix-local-build.html#cabal-new-run)
- [RFC: Add support for "#! /usr/bin/env cabal #3843](https://github.com/haskell/cabal/issues/3843)
- [Add cabal scripting support #5483](https://github.com/haskell/cabal/pull/5483)

```haskell
#!/usr/bin/env cabal
{- cabal:
build-depends: base   ^>= 4.12
             , yesod  ^>= 1.6.0
-}

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 HelloWorld
```

実行方法

```shell
$ cabal new-run Script.hs
$ cabal new-run Script.hs -- --arg1 # 引数有り
```

## 便利ツール

- [cabal-plan list-bins](http://hackage.haskell.org/package/cabal-plan)

## 参考

- [Announcing cabal new-build: Nix-style local builds](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/)
- [Introduction to Cabal](https://haskell-at-work.com/episodes/2018-05-13-introduction-to-cabal.html)
