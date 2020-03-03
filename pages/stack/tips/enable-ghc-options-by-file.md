---
title: ファイル単位で GHC_OPTIONS を指定する方法
published: 2018/05/16
# updated: 2020/02/22
---

## やり方

ファイルの先頭に次のようなプラグマを書きます。

```hs
{-# OPTIONS_GHC <option> #-}
```

**<option>** は有効・無効にしたい **ghc-option** です。

## 具体例

特定のファイルで **Orphan** インスタンスの警告を無効にするためには、ファイルの先頭に以下の行を追記します。

```hs
{-# OPTIONS_GHC -fno-warn-orphans #-}
```