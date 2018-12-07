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
`stack repl` | `cabal new-repl`
`stack clean` | `cabal new-clean`
`stack run` | `cabal new-run`
`stack --version` | `cabal --version`
`stack --numeric-version` | `cabal --numeric-version`
`stack upgrade` | `cabal new-install cabal-install --overwrite-policy=always`
 ? | `cabal new-haddock`
 ? | `cabal new-sdist`
 ? | `cabal check`
 ? | `cabal upload` <br> `cabal upload --publish`

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

## 参考

- [Announcing cabal new-build: Nix-style local builds](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/)
- [Introduction to Cabal](https://haskell-at-work.com/episodes/2018-05-13-introduction-to-cabal.html)
