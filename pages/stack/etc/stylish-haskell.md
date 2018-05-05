---
title: stylish-haskell
date: 2018/05/05
---

## 何をするためのツールか？

- [github](https://github.com/jaspervdj/stylish-haskell)

ソースコードを綺麗に整形するためのツールです。個人的には **brittany** と併用しています。

**README.md** に動作結果が載っています。

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
stylish-haskell 0.9.0.2
```

## 初期設定ファイルの生成方法

```shell
$ stylish-haskell --defaults > .stylish-haskell.yaml
```

生成される設定ファイルの内容は [stylish-haskell.yaml](https://github.com/jaspervdj/stylish-haskell/blob/master/data/stylish-haskell.yaml) で確認できます。

## default-extensions で指定している言語拡張

以下のように **package.yaml** の **default-extensions** にデフォルトの言語拡張を指定する場合があります。

```yaml
language-extensions:
- TypeOperators
- DataKinds
- OverloadedLabels
- OverloadedStrings
- TypeApplications
```

デフォルトで有効なのでソースファイルには上記の言語拡張は出現しません。そのため **stylish-haskell** はエラーを返してしまいます。

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