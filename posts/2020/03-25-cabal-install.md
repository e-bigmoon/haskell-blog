---
title: cabal install コマンドについて
author: Shinya Yamaguchi
tags: bigmoon, cabal
updated: 2020/04/03
---

- `stack install` と全く同じ動作をする **cabal** コマンドは `cabal install all:exes` です。

---

今回利用した **cabal** と **stack** のバージョンは以下の通りです。

```shell
$ cabal -V
cabal-install version 3.0.0.0
compiled using version 3.0.0.0 of the Cabal library 

$ stack --version
Version 2.1.3, Git revision 636e3a759d51127df2b62f90772def126cdf6d1f (7735 commits) x86_64 hpack-0.31.2
```

この記事では `stack install` コマンドと `cabal install` コマンドの挙動の違いなどについて調査した結果等をまとめています。

<!--more-->

## プロジェクトの構成

例えば、プロジェクトが以下のような構成になっていて、**executable** (mainExe1, mainExe2, subExe1, subExe2) が定義されているとしましょう。

```shell
$ tree .
.
├── app
│   ├── Main1.hs
│   └── Main2.hs
├── cabal.project
├── stack.yaml
├── subs
│   ├── pkg1
│   │   ├── app
│   │   │   └── Main.hs
│   │   └── pkg1.cabal
│   └── pkg2
│       ├── app
│       │   └── Main.hs
│       └── pkg2.cabal
└── app.cabal
```

[プロジェクトのコード][project-code]

[project-code]: https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/2020/03-25/

## stack install と cabal install の違い

　| `stack install` | `cabal install`
-------| ----------------|-------------------
インストール方法 | コピー | **シンボリックリンク**
インストール先ディレクトリの指定オプション | `local-bin-path` | `--installdir`

### cabal install

`cabal install` に指定可能なコマンドをいくつか試してみて、実際に何がインストールされるか一覧にしました。

コマンド | `mainExe1` | `mainExe2` | `subExe1` | `subExe2` | 備考
--------|:----------:|:----------:|:---------:|:---------:|--------
`cabal install`                  | O | O |   |
`cabal install .`                | O | O |   |
`cabal install app`              | O | O |   |
`cabal install pkg1`             |   |   | O |
`cabal install pkg2`             |   |   |   | O
`cabal install . pkg1 pkg2`      | O | O | O | O |
`cabal install app pkg1 pkg2`    | O | O | O | O |
`cabal install all`              | O | O | O | O | *注意点
`cabal install all:exes`         | O | O | O | O |

---

- `cabal install` や `stack install .` の挙動など、いくつか **stack** と異なる場合があるので注意が必要です。
- 注意点: 全てのパッケージに **executable** が含まれている場合に限り実行可能です。
  - `cabal install all` で **executable** が含まれていないパッケージがある場合は以下のようなエラーになります。

```shell
cabal: Cannot build the executables in the package pkg2 because it does not
contain any executables. Check the .cabal file for the package and make sure
that it properly declares the components that you expect.
```

- `stack install` に相当するコマンドは `cabal install all:exes` です。

### stack install 

同様に `stack install` も確認しました。

コマンド | `mainExe1` | `mainExe2` | `subExe1` | `subExe2` | 備考
--------|:----------:|:----------:|:---------:|:---------:|--------
`stack install`               | O | O | O | O
`stack install .`             | O | O | O | O
`stack install app`           | O | O |   |
`stack install pkg1`          |   |   | O |
`stack install pkg2`          |   |   |   | O
`stack install . pkg1 pkg2`   |   |   |   |  | *エラー1
`stack install app pkg1 pkg2` | O |O | O | O |
`stack install all`           |   |   |   |  | *エラー2
`stack install all:exes`      |   |   |   |  | *エラー2

エラー 1

```shell
$ stack install . pkg1 pkg2
The following errors occurred while parsing the build targets:
- The package pkg1 was specified in multiple, incompatible ways: . pkg1
- The package pkg2 was specified in multiple, incompatible ways: . pkg2
```

エラー 2

```shell
$ stack install all
Error: While constructing the build plan, the following exceptions were encountered:
Unknown package: all
Some different approaches to resolving this:
Plan construction failed.
```

### その他の指定方法

#### cabal

コマンド | `mainExe1` | `mainExe2` | 備考
--------|:----------:|:----------:|--------
`cabal install app:exe`          |   |   | *エラー1
`cabal install app:exes`         | O | O
`cabal install app:exe:mainExe1` | O | O
`cabal install app:exe:mainExe2` | O | O
`cabal install app:mainExe1`     | O | O
`cabal install app:mainExe2`     | O | O
`cabal install exes`             | O | O
`cabal install exe:mainExe1`     | O | O
`cabal install exe:mainExe2`     | O | O
`cabal install mainExe1`         | O | O
`cabal install mainExe2`         | O | O

---

エラー1

```shell
$ cabal install app:exe
cabal: Unknown target 'app:exe'.
The package app has no component 'exe'.
```

#### stack

コマンド | `mainExe1` | `mainExe2` | 備考
--------|:----------:|:----------:|--------
`stack install app:exe`          |   |   | *エラー1
`stack install app:exes`         |   |   | *エラー1
`stack install app:exe:mainExe1` | O |  
`stack install app:exe:mainExe2` |   | O
`stack install app:mainExe1`     | O |  
`stack install app:mainExe2`     |   | O
`stack install exes`             |   |   | *エラー2
`stack install exe:mainExe1`     |   |   | *エラー2
`stack install exe:mainExe2`     |   |   | *エラー2
`stack install mainExe1`         |   |   | *エラー3
`stack install mainExe2`         |   |   | *エラー3

---

エラー1

```shell
$ stack install app:exe
Error parsing targets: Component exe does not exist in package app
```

エラー2

```shell
$ stack install exe:mainExe1
Error parsing targets: Unknown local package: exe
```

エラー3

```shell
$ stack install mainExe1
Error: While constructing the build plan, the following exceptions were encountered:
Unknown package: mainExe1
Some different approaches to resolving this:
Plan construction failed.
```

## cabal install で良く使うオプション

オプション | 内容
----------|-----------
`--installdir=<path>` | インストール先のパスを指定
`--install-method=copy` | シンボリックリンクではなく、実体がコピーされる。<br>**Docker** にバイナリをコピーする際や **Windows** 環境などで利用することがあります。
`--overwrite-policy=always` | すでに実行ファイルが存在する場合でも、常に上書きします。

---

`--overwrite-policy=always` を毎回指定するのが面倒な場合は

```shell
$ cabal user-config update -a overwrite-policy:always
```

というコマンドで `~/.cabal/config` に設定できます。

## 3.2.0.0 で変更になる点など

- [Copy on windows #6519](https://github.com/haskell/cabal/pull/6519/files)

**Windows** 環境において `cabal install` の際のデフォルトの挙動が変更されます。シンボリックリンクが作成できない場合は自動的にコピーになります。

---

- [Implement cabal install -z #6428](https://github.com/haskell/cabal/pull/6428)

ローカルのプロジェクト設定を無視するオプション `-z`, `--ignore-project` が追加されます。

---

- [Resolve #6369 and #6393: Allow cabal v2-install pkgname:exename or http://example.com/package.tar.gz(#sha256=abcde...) #6576](https://github.com/haskell/cabal/pull/6576)

```shell
$ cabal install 'https://hackage.haskell.org/package/cabal-fmt-0.1.2/cabal-fmt-0.1.2.tar.gz#sha256=aae556efbcaddfd65c6a1c1811b122b0d8c8d00624c8c2e36aabb5e9f9ea9840'
```