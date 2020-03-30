---
title: cabal-plan license-report 機能の紹介
author: Shinya Yamaguchi
tags: bigmoon, package
# updated: 2020/03/30
---

[cabal-plan](https://hackage.haskell.org/package/cabal-plan) を使って、プロジェクトが依存するパッケージのライセンスを列挙する方法について紹介します。

本記事は [Is there a tool to collect all LICENSE files of used dependencies?](https://www.reddit.com/r/haskell/comments/8vhkwv/is_there_a_tool_to_collect_all_license_files_of/) を参考にしています。

```shell
$ cabal -V
cabal-install version 3.0.0.0
compiled using version 3.0.0.0 of the Cabal library
```

<!--more-->

## インストール方法

```shell
$ cabal update
$ cabal install cabal-plan -f license-report
$ cabal-plan --version
cabal-plan 0.6.2.0
```

今回の機能を使うためには **license-report** フラグを有効にする必要があります。デフォルトでは無効になっています。

## ライセンスの列挙

具体例として、このブログで試してみましょう。

- [e-bigmoon/haskell-blog](https://github.com/e-bigmoon/haskell-blog)

ライセンスファイルを抽出する場合は `--licensedir` オプションで出力先ディレクトリを指定します。

```shell
$ cabal-plan license-report --licensedir=licenses exe:site > blog.md
WARNING: couldn't find metadata for base-4.13.0.0
WARNING: couldn't find metadata for hakyll-sass-0.3
WARNING: license files for array-0.5.4.0 (global/GHC bundled) not copied
WARNING: license files for binary-0.8.7.0 (global/GHC bundled) not copied
WARNING: license files for bytestring-0.10.10.0 (global/GHC bundled) not copied
WARNING: license files for containers-0.6.2.1 (global/GHC bundled) not copied
WARNING: license files for deepseq-1.4.4.0 (global/GHC bundled) not copied
WARNING: license files for directory-1.3.6.0 (global/GHC bundled) not copied
WARNING: license files for filepath-1.4.2.1 (global/GHC bundled) not copied
WARNING: couldn't find metadata for ghc-boot-th-8.8.3
WARNING: license files for ghc-prim-0.5.3 (global/GHC bundled) not copied
WARNING: license files for integer-gmp-1.0.2.0 (global/GHC bundled) not copied
WARNING: license files for mtl-2.2.2 (global/GHC bundled) not copied
WARNING: license files for parsec-3.1.14.0 (global/GHC bundled) not copied
WARNING: license files for pretty-1.1.3.6 (global/GHC bundled) not copied
WARNING: license files for process-1.6.8.0 (global/GHC bundled) not copied
WARNING: license files for stm-2.5.0.0 (global/GHC bundled) not copied
WARNING: license files for template-haskell-2.15.0.0 (global/GHC bundled) not copied
WARNING: license files for text-1.2.4.0 (global/GHC bundled) not copied
WARNING: license files for time-1.9.3 (global/GHC bundled) not copied
WARNING: license files for transformers-0.5.6.2 (global/GHC bundled) not copied
WARNING: license files for unix-2.7.2.2 (global/GHC bundled) not copied
```

## 出力結果

`licenses` ディレクトリはだいたいこんな感じでライセンスファイルが格納されています。

```shell
$ tree licenses | head -n 20
licenses
├── Glob-0.10.0
│   └── LICENSE.txt
├── HTTP-4000.3.14
│   └── LICENSE
├── HsYAML-0.2.1.0
│   ├── LICENSE.GPLv2
│   └── LICENSE.GPLv3
├── HsYAML-aeson-0.2.0.0
│   └── LICENSE.GPLv2
├── JuicyPixels-3.3.5
│   └── LICENSE
├── Only-0.1
│   └── LICENSE
├── QuickCheck-2.13.2
│   └── LICENSE
├── SHA-1.6.4.4
│   └── LICENSE
├── StateVar-1.2
│   └── LICENSE
```

----

`blog.md` の内容を一部を以下に貼り付けます。完全なコードは[こちら](https://github.com/e-bigmoon/haskell-blog/sample-code/2020/03-30/blog.md)。

# Dependency License Report

Bold-faced **`package-name`**s denote standard libraries bundled with `ghc-8.8.3`.

## Direct dependencies of `bigmoon-haskellers-blog:exe:site`

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Also depended upon by |
| --- | --- | --- | --- | --- |
| **`base`** | [`4.13.0.0`](http://hackage.haskell.org/package/base-4.13.0.0) |  *MISSING* | *MISSING* | *(core library)* |
| `extensible` | [`0.8`](http://hackage.haskell.org/package/extensible-0.8) | [`BSD-3-Clause`](licenses/extensible-0.8/LICENSE) | Extensible, efficient, optics-friendly data types and effects |  |
| `hakyll` | [`4.13.2.0`](http://hackage.haskell.org/package/hakyll-4.13.2.0) | [`BSD-3-Clause`](licenses/hakyll-4.13.2.0/LICENSE) | A static website compiler library | `hakyll-sass` |
| `hakyll-sass` | [`0.3`](http://hackage.haskell.org/package/hakyll-sass-0.3) |  *MISSING* | *MISSING* |  |
| `html-entities` | [`1.1.4.3`](http://hackage.haskell.org/package/html-entities-1.1.4.3) | [`MIT`](licenses/html-entities-1.1.4.3/LICENSE) | A codec library for HTML-escaped text and HTML-entities |  |
| `rio` | [`0.1.14.1`](http://hackage.haskell.org/package/rio-0.1.14.1) | [`MIT`](licenses/rio-0.1.14.1/LICENSE) | A standard library for Haskell |  |
| `yaml` | [`0.11.3.0`](http://hackage.haskell.org/package/yaml-0.11.3.0) | [`BSD-3-Clause`](licenses/yaml-0.11.3.0/LICENSE) | Support for parsing and rendering YAML documents. | `hakyll`, `pandoc-citeproc` |

## Indirect transitive dependencies

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Depended upon by |
| --- | --- | --- | --- | --- |
| `Glob` | [`0.10.0`](http://hackage.haskell.org/package/Glob-0.10.0) | [`BSD-3-Clause`](licenses/Glob-0.10.0/LICENSE.txt) | Globbing library | `pandoc` |
| `HTTP` | [`4000.3.14`](http://hackage.haskell.org/package/HTTP-4000.3.14) | [`BSD-3-Clause`](licenses/HTTP-4000.3.14/LICENSE) | A library for client-side HTTP | `pandoc` |

## まとめ

今回は **cabal-plan** を使ってみましたが、同様のツールとして [fossas/fossa-cli](https://github.com/fossas/fossa-cli/blob/master/docs/integrations/haskell.md#haskell) や [github/licensed](https://github.com/github/licensed/blob/master/docs/sources/cabal.md) なども利用できるようです。
