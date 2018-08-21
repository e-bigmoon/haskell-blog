---
title: path & path-io パッケージ
author: Shinya Yamaguchi
tags: bigmoon, package
---

## はじめに

Haskell でファイルやディレクトリを扱うプログラムを書く時によく使うパッケージとして [filepath](https://www.stackage.org/lts-12.7/package/filepath-1.4.2) パッケージや [directory](https://www.stackage.org/lts-12.7/package/directory-1.3.1.5) パッケージがあります。(Haskell入門の「4.4 ファイルシステム」に **directory** パッケージの話が少し載っています。)

これらのパッケージは結局のところただの文字列操作なので、バグを出さないためにはパッケージ利用者がかなり注意深く使わなければなりません。

例えば、以下のようなパスは型レベルでは同じ文字列 (**FilePath**) ですが

```hs
-- 相対パス
pathRel :: FilePath
pathRel = ./aaa/bbb/ccc

-- 絶対パス
pathAbs :: FilePath
pathAbs = /home/user/aaa/bbb/ccc

-- ファイルへのパス
pathFile :: FilePath
pathFile = ./aaa/a.png

-- ディレクトリへのパス
pathDir :: FilePath
pathDir = ./aaa
```

このように、**FilePath** 型では**相対パス**なのか**絶対パス**なのか型レベルで判断する方法が無かったり、そもそもパスが**ファイル**なのか**ディレクトリ**なのかすらわからなかったりします。

今回紹介するのは、型レベルでこれらをちゃんと分類できるようにしている [path](https://github.com/commercialhaskell/path) と [path-io](https://github.com/mrkkrp/path-io) パッケージです。

型レベルで **相対パス** or **絶対パス** と **ファイル** or **ディレクトリ** を表現するため、不正な操作はコンパイル時にチェックできるようになります。

また、**stack** の内部でも利用していたので、実用しても大丈夫だと思います。

パッケージのバージョンは以下のとおりです

- path-0.6.1
- path-io-1.3.3

まだまだ更新が活発なパッケージなので、path-0.7 では破壊的変更を含む更新があるようです。([CHANGELOG](https://github.com/commercialhaskell/path/blob/master/CHANGELOG))

<!--more-->

## path パッケージ

ドキュメントが充実しているので [Readme](https://github.com/commercialhaskell/path/blob/master/README.md) を読めば使い方はすぐにわかると思います。

### データ型

`Path` の型は `FilePath` を幽霊型 (Phantom type) を使ってラップしているだけです。(幽霊型については [ElmでPhantom Type (幽霊型)入門](https://qiita.com/nobkz/items/5926257a375a4a181dde) や [で、出たー！幽霊型だー！(Phantom Type)](https://qiita.com/HirotoShioi/items/3444e215070144b8ca0f) などが日本語のわかりやすい解説だと思います)

```hs
newtype Path b t = Path FilePath
  deriving (Data, Typeable, Generic)
```

ここで2つの型変数の意味は以下の通りです。

- `b` - 相対パス or 絶対パス
- `t` - ファイル or ディレクトリ

型変数 `b` は実際には以下の型のどちらかを取ります。

```hs
data Abs deriving (Typeable)
data Rel deriving (Typeable)
```

同様に型変数 `t` は以下の型を取ります。

```hs
data File deriving (Typeable)
data Dir deriving (Typeable)
```

具体的なパスの型は以下の4種類のどれかになります。

```hs
Path Abs File -- ファイルへの絶対パス
Path Abs Dir  -- ディレクトリへの絶対パス
Path Rel File -- ファイルへの相対パス
Path Rel Dir  -- ディレクトリへの相対パス
```

型を見るだけでどんなパスなのか一目瞭然なので、めっちゃ良いですね。

### 値の作り方

型については説明したので、次は実際に `Path` 型の値を作っていきましょう！

#### パースする方法

`Path` 型は4種類あるので、パーズする関数も4種類あります。

```hs
parseAbsDir  :: MonadThrow m => FilePath -> m (Path Abs Dir)
parseRelDir  :: MonadThrow m => FilePath -> m (Path Rel Dir)
parseAbsFile :: MonadThrow m => FilePath -> m (Path Abs File)
parseRelFile :: MonadThrow m => FilePath -> m (Path Rel File)
```

`MonadThrow m` がついていますが、この `m` は `IO` だと思えば以下の型と同じですし

```hs
parseAbsDir  :: FilePath -> IO (Path Abs Dir)
parseRelDir  :: FilePath -> IO (Path Rel Dir)
parseAbsFile :: FilePath -> IO (Path Abs File)
parseRelFile :: FilePath -> IO (Path Rel File)
```

`Maybe` であれば、以下の型と同じです。

```hs
parseAbsDir  :: FilePath -> Maybe (Path Abs Dir)
parseRelDir  :: FilePath -> Maybe (Path Rel Dir)
parseAbsFile :: FilePath -> Maybe (Path Abs File)
parseRelFile :: FilePath -> Maybe (Path Rel File)
```

難しいことはあまり気にせず、([MonadThrow](https://www.stackage.org/haddock/lts-12.7/exceptions-0.10.0/Control-Monad-Catch.html#t:MonadThrow) 型クラスのインスタンスになっている) 色んなモナドで使えるんだなと思えば良いと思います。

実際に `ghci` を使って動作を確認してみましょう！

```shel
$ stack repl --package path
> import Path

# 型のチェック
> :t parseAbsDir "/"
parseAbsDir "/" :: MonadThrow m => m (Path Abs Dir)
> :t parseAbsDir "./"
parseAbsDir "./" :: MonadThrow m => m (Path Abs Dir)

# IO モナドの文脈
> parseAbsDir "/"
"/"
> parseAbsDir "./"
*** Exception: InvalidAbsDir "./"

# Maybe モナドの文脈
> parseAbsDir "/" :: Maybe (Path Abs Dir)
Just "/"
> parseAbsDir "./" :: Maybe (Path Abs Dir)
Nothing

# 以下のような "../" を含むパスはパーズできない
> parseAbsDir "./../a/b/"
*** Exception: InvalidAbsDir "./../a/b/"
> parseRelDir "./../a/b/"
*** Exception: InvalidAbsDir "./../a/b/"
```

これで文字列から `Path` 型に変換する方法がわかりましたね！結構簡単です。

#### Template Haskell & QuasiQuotes

コンパイル時にすでにファイルパスが決まっている時はテンプレートHaskellや準クォートを使うこともできます。

```haskell
$(mkAbsDir "/home/chris")
$(mkRelDir "chris")
$(mkAbsFile "/home/chris/x.txt")
$(mkRelFile "chris/x.txt")
```

```haskell
[absdir|/home/chris|]
[reldir|chris|]
[absfile|/home/chris/x.txt|]
[relfile|chris/x.txt|]
```

これで不正なパスはコンパイル時エラーとなるため、かなり安全ですね。

### Path から FilePath への変換

`Path` 型の値を `FilePath` に変換するためには `toFilePath` 関数を利用します。

```shell
> toFilePath <$> parseRelDir "./a/b"
"a/b/"

> toFilePath <$> parseRelDir "./a/b/"
"a/b/"

> toFilePath <$> parseRelDir "./a////b//////"
"a/b/"
```

こんな感じで期待している文字列に変換されているか確かめることができます。

### パスの等価性

2つの `Path` の等しさは単純に文字列の等価性として定義されています。

```hs
instance Eq (Path b t) where
  (==) (Path x) (Path y) = x == y
```

実際にいくつか試してみます。

```shell
> (==) <$> parseRelDir "./a/b" <*> parseRelDir "./a/b"
True

> (==) <$> parseRelDir "./a/b" <*> parseRelDir "./a/b/c"
False

> (==) <$> parseRelDir "./a/b" <*> parseRelDir "./a/b/"
True
```

### パスの操作

関数と実行結果のみを紹介していきます。

#### 2つのパスの結合

```haskell
(</>) :: Path b Dir -> Path Rel t -> Path b t
```

第一引数は **Dir** で第二引数は **Rel** が指定されている点に注意してください。そのため、第一引数にファイルへのパスを与えようとするとコンパイルエラーになります。

```shell
> (</>) <$> parseRelDir "a/b/c" <*> parseRelFile "a.png"
"a/b/c/a.png"

> (</>) <$> parseRelDir "a/b/c" <*> parseRelDir "d"
"a/b/c/d/"
```

#### パスの先頭部分から、ディレクトリパスを除去

Data.List の [stripPrefix](https://www.stackage.org/haddock/lts-12.7/base-4.11.1.0/Data-List.html#v:stripPrefix) 関数と同じように利用できます。

```haskell
stripProperPrefix :: MonadThrow m => Path b Dir -> Path b t -> m (Path Rel t)
```

```shell
> join $ stripProperPrefix <$> parseAbsDir "/usr/local/bin/" <*> parseAbsFile "/usr/local/bin/stack"
"stack"

> join $ stripProperPrefix <$> parseAbsDir "/local/bin/" <*> parseAbsFile "/usr/local/bin/stack"
*** Exception: NotAProperPrefix "/local/bin/" "/usr/local/bin/stack"
```

#### パスから親ディレクトリパスを取得

```haskell
parent :: Path b t -> Path b Dir
```

```shell
> parent <$> parseRelFile "ab"
"./"

> parent <$> parseRelFile "./a/b/c/d"
"a/b/c/"
```

#### ディレクトリパスから、相対ディレクトリパスを取得

```haskell
dirname :: Path b Dir -> Path Rel Dir
```

```shell
> dirname <$> parseAbsDir "/a/b/c/d"
"d/"

> dirname <$> parseRelDir "./a/b/c/d"
"d/"
```

#### ファイルパスから相対ファイルパスを取得

```haskell
filename :: Path b File -> Path Rel File
```

```shell
> filename <$> parseAbsFile "/a/b/c/d.png"
"d.png"

> filename <$> parseRelFile "./a/b/c/d.png"
"d.png"
```

#### ファイルパスから拡張子を取得

```haskell
fileExtension :: Path b File -> String
```

```shell
> fileExtension <$> parseAbsFile "/a/b/c.png"
".png"

> fileExtension <$> parseRelFile "a/b/c.png"
".png"
```

#### ファイルパスに拡張子を追加

```haskell
addFileExtension :: MonadThrow m => String -> Path b File -> m (Path b File)

-- 演算子バージョンとして (<.>) が定義されている
(<.>) :: MonadThrow m => Path b File -> String -> m (Path b File)
```

```shell
> join $ addFileExtension "hs" <$> parseAbsFile "/a/b/c"
"/a/b/c.hs"

> join $ addFileExtension ".hs" <$> parseAbsFile "/a/b/c"
"/a/b/c.hs"

> join $ addFileExtension ".hs" <$> parseRelFile "a/b/c"
"a/b/c.hs"

> join $ addFileExtension ".hs" <$> parseRelFile "a/b/c.rs"
"a/b/c.rs.hs"

> join $ (<.> ".hs") <$> parseRelFile "a/b/c.rs"
"a/b/c.rs.hs"
```

既に拡張子があっても、追加する点に注意。

#### ファイルパスに拡張子を追加 (既に拡張子がある場合は置き換える)

```haskell
setFileExtension :: MonadThrow m => String -> Path b File -> m (Path b File)

-- 演算子バージョンとして (-<.>) が定義されている
(-<.>) :: MonadThrow m => Path b File -> String -> m (Path b File)
```

```shell
> join $ setFileExtension "hs" <$> parseAbsFile "/a/b/c"
"/a/b/c.hs"

> join $ setFileExtension ".hs" <$> parseAbsFile "/a/b/c"
"/a/b/c.hs"

> join $ setFileExtension ".hs" <$> parseRelFile "a/b/c"
"a/b/c.hs"

> join $ setFileExtension ".hs" <$> parseRelFile "a/b/c.rs"
"a/b/c.hs"

> join $ (-<.> ".hs") <$> parseRelFile "a/b/c.rs"
"a/b/c.hs"
```

## path-io

ここまでで `Path` 型の定義や値の作り方、操作する関数などを見てきました。

しかしながら、これだけでは実際にファイルを作ったり削除したりすることはできません。文字列に変換して `directory` パッケージを利用することもできますが、やはり `Path` 型のまま使いたいですよね。

そのためには [path-io](https://github.com/mrkkrp/path-io) パッケージを利用すると良いです。内部的には `directory` パッケージを再利用していますが、`Path` 型で使えるようにラップしてくれています。(また、便利な関数もいくつか追加されています)

### サンプルプログラム

例えばこんな感じで使えます。以下の例はコマンドライン引数から受け取った文字列に拡張子 `.hs` を追加して適当な内容で保存し、最後にディレクトリを再帰的にコピーする例です。

```hs
#!/usr/bin/env stack
-- stack script --resolver lts-12.7
{-# LANGUAGE TemplateHaskell #-}

import Path
import Path.IO

import Control.Monad (when)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  when (length args == 1) $ do
    let src  = $(mkRelDir "./src")
        dest = $(mkRelDir "./.backup")

    -- 安全にディレクトリを作成
    mapM_ ensureDir [src, dest]

    rawName <- parseRelFile $ head args
    fn <- (src </> rawName) -<.> "hs"

    writeFile (toFilePath fn) "main :: IO ()\nmain = undefined\n"

    -- ディレクトリを再帰的にコピー
    copyDirRecur' src dest
```

実行結果

```shell
$ ./Sample.hs aaa

$ tree -a .
.
├── .backup
│   └── aaa.hs
├── Sample.hs
└── src
    └── aaa.hs

2 directories, 3 files

$ cat src/aaa.hs
main :: IO ()
main = undefined

$ cat .backup/aaa.hs
main :: IO ()
main = undefined
```

動いているようです。

## まとめ

- `filepath` や `directory` パッケージでは文字列の操作となってしまうため、コンパイル時に不正な利用方法をチェックできない
- `path` や `path-io` は幽霊型を使って不正な利用をコンパイル時にチェックする
- 実際に `stack` でも利用されているパッケージ

以上です。