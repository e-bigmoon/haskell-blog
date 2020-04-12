---
title: cabal コマンドとの対応表
published: 2018/11/30
updated: 2020/04/12
---

## 注意点

このページの内容は **cabal HEAD** を追っているため、最新版の **cabal** ではまだ利用不可能な内容も含まれる場合があります。

## 参考となるサイト&記事

- [AWESOME-CABAL](https://kowainik.github.io/projects/awesome-cabal)
- [Why Not Both?](https://medium.com/@fommil/why-not-both-8adadb71a5ed)
- [Announcing cabal new-build: Nix-style local builds](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/)
- [Introduction to Cabal](https://haskell-at-work.com/episodes/2018-05-13-introduction-to-cabal.html)
- [The Haskell Cabal | Overview](https://www.haskell.org/cabal/)
  - [haskell/cabal-website](https://github.com/haskell/cabal-website)

## stack と cabal

```shell
$ stack --version
Version 2.1.3, Git revision 636e3a759d51127df2b62f90772def126cdf6d1f (7735 commits) x86_64 hpack-0.31.2

$ cabal -V
cabal-install version 3.2.0.0
compiled using version 3.2.0.0 of the Cabal library
```

　| stack | cabal
-------|-----------|-------------------
ドキュメント | [stack user guide][stack-official] | [Cabal User Guide][cabal-user-guide] ([latest][cabal-user-guide-latest])
repo | [commercialhaskell/stack][stack-repo] | [haskell/cabal][cabal-repo]
コマンド名 | stack | cabal (正式には **cabal-install** と呼ばれているもの)

[stack-official]: https://docs.haskellstack.org/en/stable/README/
[stack-repo]: https://github.com/commercialhaskell/stack/tree/stable
[cabal-user-guide]: https://www.haskell.org/cabal/users-guide/
[cabal-user-guide-latest]: https://cabal.readthedocs.io/en/latest/
[cabal-repo]: https://github.com/haskell/cabal

## stack と cabal の違い

- [[RFC] Switch from `stack` to `cabal-install` for building Haskell code #3280][hasura-3280] に良くまとまっています。

[hasura-3280]: https://github.com/hasura/graphql-engine/issues/3280

## v1-build と v2-build コマンドの違い

- `cabal v2-build` は [Nix-style Local Builds](https://cabal.readthedocs.io/en/latest/nix-local-build-overview.html)
- `cabal build` は `cabal v1-build` のエイリアス
- `cabal new-build` は `cabal v2-build` のエイリアス
- `cabal v3.0.0.0` からは `cabal v2-build` が `cabal build` のエイリアス ([#5800][cabal-5800])

cabal version | cabal build | cabal new-build
--------------|-------------|-----------------
v3.2.0.0 | v2-build | v2-build
v3.0.0.0 | v2-build | v2-build
v2.4.1.0 | **v1**-build | v2-build

[cabal-5800]: https://github.com/haskell/cabal/pull/5800

## コマンド対応表

stack | cabal | 備考
------|--------|-------
`stack install` | `cabal install` | [cabal install コマンドについて](/posts/2020/03-25-cabal-install.html)
`stack init` | `cabal init -n --is-executable` <br> `cabal init --simple` <br> `cabal init --lib` <br> `cabal init --exe` <br> `cabal init --libandexe` | [#5707][cabal-5707], [#5759][cabal-5759], [#5864][cabal-5864], [#6676][cabal-6676]
`stack build` | `cabal build`
`stack test` | `cabal test`
`stack repl` <br> `stack ghci` | `cabal repl` | [cabal repl コマンドについて](/posts/2020/04-10-cabal-repl.html)
`stack repl --package <pkg1> <pkg2>` | `cabal repl --build-dep <pkg1>, <pkg2>` <br> `cabal repl -b <pkg1>, <pkg2>` | [#5845][cabal-5845]
`stack clean` | `cabal clean`
`stack run` | `cabal run`
`stack --version` | `cabal --version` <br> `cabal -V`
`stack --numeric-version` | `cabal --numeric-version`
`stack upgrade` | `cabal install cabal-install` | [ghcup][ghcup-official] を利用する
 ? | `cabal haddock`
 ? | `cabal sdist`
 ? | `cabal check`
 ? | `cabal upload` <br> `cabal upload --publish`
`stack update` | `cabal update`
 ? | `cabal list` | [#6681][cabal-6681]

[cabal-5707]: https://github.com/haskell/cabal/pull/5707
[cabal-5759]: https://github.com/haskell/cabal/pull/5759
[cabal-5845]: https://github.com/haskell/cabal/pull/5845
[cabal-5864]: https://github.com/haskell/cabal/pull/5864
[cabal-6676]: https://github.com/haskell/cabal/pull/6676
[cabal-6681]: https://github.com/haskell/cabal/pull/6681

### オプション対応表

stack | cabal | 備考
------|-------|-------
`local-bin-path` | `installdir`

### どちらか一方にしかないコマンドやオプション

stack | cabal | 備考
------|--------|-------
`stack build --file-watch` | [#5252][cabal-5252]
- | `cabal format` | [#2460][cabal-2460], [#5306][cabal-5306], [#5734][cabal-5734]<br>[cabal-fmt の紹介][cabal-fmt-article]
`stack path` | [#3850][cabal-3850], [#4661][cabal-4661] |
`stack setup` | [ghcup][ghcup-official] を利用する |
[#3420][stack-3420] | `cabal build --enable-executable-static` | [#5446][cabal-5446]
- | `-w` オプション。<br>`cabal repl --with-compiler ghc-8.8.3`<br>`cabal repl -w ghc-8.8.3` | コンパイラを指定できる

[stack-3420]: https://github.com/commercialhaskell/stack/issues/3420
[cabal-2460]: https://github.com/haskell/cabal/issues/2460
[cabal-3850]: https://github.com/haskell/cabal/issues/3850
[cabal-4661]: https://github.com/haskell/cabal/issues/4661
[cabal-5252]: https://github.com/haskell/cabal/issues/5252
[cabal-5306]: https://github.com/haskell/cabal/issues/5306
[cabal-5446]: https://github.com/haskell/cabal/pull/5446
[cabal-5734]: https://github.com/haskell/cabal/issues/5734
[cabal-fmt-article]: https://haskell.e-bigmoon.com/posts/2019/10-07-cabal-fmt.html
[ghcup-official]: https://www.haskell.org/ghcup/

## 設定ファイル等のパス対応表 (初期値)

stack | cabal
--------------|-------
`~/.stack/` | `~/.cabal/`
`~/.local/bin` | `~/.cabal/bin`
? | `~/.cabal/store`
`~/.stack/config.yaml`<br>`~/.stack/global-project/stack.yaml` | `~/.cabal/config`

## Tips

### index-state

- **Cabal-2.0** から利用可能
- デフォルト値は **HEAD**

**cabal.project** に追加する場合は以下のようにすれば良い。

```
index-state: 2020-03-28T06:23:15Z
```

その他にも指定方法としては **HEAD** と **UNIX timestamp format** が利用できる。

```
index-state: HEAD

-- UNIX timestamp format example
index-state: @1474739268

-- ISO8601 UTC timestamp format example
index-state: 2016-09-24T17:47:48Z
```

現在の **UNIX timestamp format** と **ISO8601 UTC timestamp format** は以下のコマンドで取得できる。

```shell
$ date +%s
1586488404

$ date -u +"%Y-%m-%dT%H:%M:%SZ"
2020-04-10T03:18:55Z
```

`cabal configure` で指定する場合は以下のようにすれば良い。

```shell
$ cabal configure --index-state=HEAD
$ cabal configure --index-state=$(date +%s)
$ cabal configure --index-state=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
```

- [index-state - Cabal User Guide][cabal-user-guide-index-state]
- [#6682][cabal-6682]

[cabal-user-guide-index-state]: https://www.haskell.org/cabal/users-guide/nix-local-build.html#cfg-field-index-state
[cabal-6682]: https://github.com/haskell/cabal/pull/6682

### Stackage のスナップショットから project.cabal.freeze ファイルを生成する方法

スナップショットの **URL** に `cabal.config` を追加すると [cabal.project.freeze](https://www.stackage.org/lts-15/cabal.config) ファイルが取得できる。

```shell
# lts-15 の最新版の例
$ curl https://www.stackage.org/lts-15/cabal.config > cabal.project.freeze

# lts-15.5 の例
$ curl https://www.stackage.org/lts-15.5/cabal.config > cabal.project.freeze

# nightly-2020-03-24 の例
$ https://www.stackage.org/nightly-2020-03-24/cabal.config > cabal.project.freeze
```

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
$ cabal build all
$ cabal repl

# 個別のパッケージ
$ cabal build package1
$ cabal repl basic
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

複数のパッケージを指定する場合、`cabal-3.2.0.0` からは以下のように `subdir` に複数のパッケージを並べて書けるようになった。

```cabal
# cabal.project
source-repository-package
  type: git
  location: https://github.com/gtk2hs/gtk2hs
  tag: 7bccd432e2f962d80b2b804fa2a59712e402753c
  subdir: cairo gtk
```

`cabal-3.0.0.0`以前は`stack` の `subdirs` のような書き方はできないため、`source-repository-package` を複数記述しなければならない。([#5472](https://github.com/haskell/cabal/issues/5472))

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

`cabal.project.local` に上記の内容を追加し、`cabal build` すれば良い。

- [How can I profile my library/application?](https://cabal.readthedocs.io/en/latest/nix-local-build.html#how-can-i-profile-my-library-application)

### プロジェクトで利用する ghc を強制する方法

#### cabal

`cabal.project` に `with-compiler` を追記する

```
with-compiler: ghc-8.8.3
```

### build-depends で利用するバージョン制約の指定方法

- [Cabal Version Calculator](https://cabal-version-calculator.netlify.com/)
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
$ cabal configure -j
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
$ stack script --resolver lts-15.5 Script.hs
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
$ cabal run Script.hs
$ cabal run Script.hs -- --arg1 # 引数有り
```

## 便利ツール

- [cabal-plan](http://hackage.haskell.org/package/cabal-plan)
  - [cabal-plan license-report 機能の紹介](https://haskell.e-bigmoon.com/posts/2020/03-30-cabal-plan-license-report.html)
- [cabal-fmt](https://hackage.haskell.org/package/cabal-fmt)
  - [cabal-fmt の紹介](https://haskell.e-bigmoon.com/posts/2019/10-07-cabal-fmt.html)
- cabal-env
  - [phadej/cabal-env](https://github.com/phadej/cabal-env)
  - [hvr/cabal-env](https://github.com/hvr/cabal-env)
