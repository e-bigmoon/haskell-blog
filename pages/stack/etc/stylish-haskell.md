---
title: stylish-haskell
date: 2019/03/08
---

## 何をするためのツールか？

- [jaspervdj/stylish-haskell - GitHub](https://github.com/jaspervdj/stylish-haskell)

ソースコードを綺麗に整形するためのツールです。[brittany](https://github.com/lspitzner/brittany) などのソースコードフォーマットツールとの大きな違いは、整形しすぎない点かなと思います。

[README](https://github.com/jaspervdj/stylish-haskell/blob/master/README.markdown) に動作結果が載っています。

### stylish-haskell 適用前

```haskell
{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving,
            ViewPatterns,
    ScopedTypeVariables #-}

module Bad where

import Control.Applicative ((<$>))
import System.Directory (doesFileExist)

import qualified Data.Map as M
import      Data.Map    ((!), keys, Map)

data Point = Point
    { pointX, pointY :: Double
    , pointName :: String
    } deriving (Show)
```

### stylish-haskell 適用後

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Bad where

import           Control.Applicative ((<$>))
import           System.Directory    (doesFileExist)

import           Data.Map            (Map, keys, (!))
import qualified Data.Map            as M

data Point = Point
    { pointX, pointY :: Double
    , pointName      :: String
    } deriving (Show)
```

## インストール方法

```shell
$ stack install stylish-haskell

$ stylish-haskell --version
stylish-haskell 0.9.2.0
```

## 初期設定ファイルの生成方法

```shell
$ stylish-haskell --defaults > .stylish-haskell.yaml
```

生成される設定ファイルの内容は [stylish-haskell.yaml](https://github.com/jaspervdj/stylish-haskell/blob/master/data/stylish-haskell.yaml) で確認できます。

## default-extensions

以下のように **package.yaml** の **default-extensions** にデフォルトの言語拡張を指定する場合があります。

```yaml
default-extensions:
- TypeOperators
- DataKinds
- OverloadedLabels
- OverloadedStrings
- TypeApplications
```

このように指定するということは、ソースコード上からは上記の言語拡張を削除すると思います。

その結果、**stylish-haskell** 言語拡張を認識できずにエラーを返してしまいます。

```shell
$ stylish-haskell app/Main.hs
Language.Haskell.Stylish.Parse.parseModule: could not parse app/Main.hs: ParseFailed (SrcLoc "<unknown>.hs" 10 3) "Improper character constant or misplaced '"
```

この場合は、`.stylish-haskell.yaml` (設定ファイル) に言語拡張を指定すれば上手くいきます。

```yaml
language_extensions:
- TypeOperators
- DataKinds
- OverloadedLabels
- OverloadedStrings
- TypeApplications
```

## ワンライナー

haskell-jp slack から借用しました。ありがとうございます！

### 指定したディレクトリ以下に対して再帰的に適用する。

ファイル名やディレクトリ名に特殊な文字（空白や引用符など）が含まれていても動作するバージョン。

```shell
$ find . -type f -name "*hs" -not -path '.git' -not -path '*.stack-work*' -print0 | xargs -0 stylish-haskell -i
```

### git 管理の場合

git で管理している `hs` ファイルにのみに適用する場合

```shell
$ git ls-files -z | egrep -z 'hs$' | xargs -0 stylish-haskell -i
```