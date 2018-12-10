---
title: cabal コマンドとの対応表
date: 2018/12/07
---

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

## コマンド対応表

stack | cabal | 備考
------|--------|-------
`stack init` | `cabal init -n --is-executable` <br> `cabal init --simple` | [#5707](https://github.com/haskell/cabal/pull/5707)
`stack setup` | _ | [ghcup](https://github.com/haskell/ghcup) を利用する
`stack build` | `cabal new-build`
`stack test` | `cabal new-test`
`stack repl` | `cabal new-repl`
`stack clean` | `cabal new-clean`
`stack run` | `cabal new-run`
`stack --version` | `cabal --version`
`stack --numeric-version` | `cabal --numeric-version`
`stack upgrade` | `cabal new-install<br>cabal-install --overwrite-policy=always`
 ? | `cabal new-haddock`
 ? | `cabal new-sdist`
 ? | `cabal check`
 ? | `cabal upload` <br> `cabal upload --publish`
 `stack update` | `cabal new-update`

### どちらか一方にしかないコマンドやオプション

stack | cabal | 備考
------|--------|-------
`stack build --file-watch` | [#5252](https://github.com/haskell/cabal/issues/5252)

## 設定ファイル等のパス対応表 (初期値)

stack | cabal
------|-------
`~/.stack/` | `~/.cabal/`
`~/.stack/config.yaml`, `~/.stack/global-project/stack.yaml` | `~/.cabal/config`

## Tips

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

#### cabal

- [Specifying Packages from Remote Version Control Locations](https://cabal.readthedocs.io/en/latest/nix-local-build.html#specifying-packages-from-remote-version-control-locations)
- リポジトリに `cabal` ファイルが含まれていない場合は `NoCabalFileFound` となる。リポジトリに含めるしかない。

```cabal
# cabal.project
source-repository-package
  type: git
  location: https://github.com/fumieval/extensible
  tag: 81ccac73f7480ea66e6008e660972bfee9e83976
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

### cabal.project.local の設定

```shell
$ cabal new-configure -j
```

## 参考

- [Announcing cabal new-build: Nix-style local builds](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/)
- [Introduction to Cabal](https://haskell-at-work.com/episodes/2018-05-13-introduction-to-cabal.html)
