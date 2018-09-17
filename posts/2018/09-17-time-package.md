---
title: time パッケージの使い方
author: Shinya Yamaguchi
tags: bigmoon, package
---

## はじめに

Haskell で時間や日付を扱う際に良く利用されるのは [time](https://www.stackage.org/lts-12.9/package/time) パッケージです。

このパッケージが使いやすいかどうかは人それぞれですが、使い方を知っておくと便利なのでよく使いそうな関数を簡単に解説しようと思います。

これからの例は以下のコマンドを実行していると仮定して話を進めます。

```shell
$ stack repl --package time --resolver lts-12.9
$ import Data.Time
```


<!--more-->

## Time パッケージのモジュール構造

基本的には [Data.Time](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time.html) を import して使います。

```haskell
import Data.Time
```

Data.Time は以下のモジュールを再エクスポートしています。

モジュール名 | 用途
-------------|---------
[Data.Time.Calendar](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Calendar.html) | 日付
[Data.Time.Clock](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Clock.html) | 全然使わないので良くわからない
[Data.Time.LocalTime](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-LocalTime.html) | 日本の現在時刻を取得など
[Data.Time.Format](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Format.html) | 出力の整形

### rio を利用している場合

[rio](https://www.stackage.org/lts-12.9/package/rio) を利用している場合は [RIO.Time](https://www.stackage.org/haddock/lts-12.9/rio-0.1.5.0/RIO-Time.html) を import します。

```haskell
import RIO.Time
```

## Data.Time.LocalTime

現在時刻を取得する場合にこのモジュールを使います。現在時刻を取得したいからと言って [getCurrentTime](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Clock.html#v:getCurrentTime) を利用すると日本時間にならないので注意してください。

### getZonedTime

システムのタイムゾーンに応じた現在時刻を返します。

```shell
> :t getZonedTime
getZonedTime :: IO ZonedTime

> getZonedTime
2018-09-17 13:41:05.512522063 JST
```

### getCurrentTimeZone

システムのタイムゾーンを取得します。このタイムゾーンに基づいて `getZonedTime` が計算されます。

```shell
> :t getCurrentTimeZone
getCurrentTimeZone :: IO TimeZone

> getCurrentTimeZone
JST
```

### zonedTimeToUTC

`ZonedTime` を `UTCTime` に変換するために使います。

```shell
> :t zonedTimeToUTC
zonedTimeToUTC :: ZonedTime -> UTCTime

> zonedTimeToUTC <$> getZonedTime
2018-09-17 04:41:27.907476307 UTC
```

### utcToZonedTime

`zonedTimeToUTC` の逆で `UTCTime` を `ZonedTime` に変換する関数です。タイムゾーンのための引数を余分に取ります。

```shell
> :t utcToZonedTime
utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime

> utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime
2018-09-17 13:41:37.955641567 JST
```

## 1日後の時間を計算するには？

ここで、取得した時間の1日後を計算してみましょう。

そのためには Data.Time.Clock で定義されている [addUTCTime](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Clock.html#v:addUTCTime) を使います。

```haskell
addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
```

第一引数に `NominalDiffTime` という謎の型を取りますが、`nominalDay` の実装を見れば `60 * 60 * 24` っぽいことがわかるので、そんな感じで値を作ります。

```haskell
nominalDay :: NominalDiffTime
nominalDay = 86400
```

ちなみに、上記の実装でなぜ `NominalDiffTime` の値になるかと言うと、`NominalDiffTime` は `Num` クラスのインスタンスになっているため、自動的に `fromInteger` が呼ばれて変換されるという仕組みです。

実際に試してみましょう。1日後を計算してみます。

```shell
> t = addUTCTime nominalDay . zonedTimeToUTC <$> getZonedTime
2018-09-17 10:32:56.880362453 UTC

> getZonedTime
2018-09-17 13:49:09.279378323 JST

> utcToZonedTime <$> getCurrentTimeZone <*> t
2018-09-18 13:49:16.211737218 JST
```

同様に1時間後も計算してみましょう。

```shell
> t = addUTCTime (60 * 60) . zonedTimeToUTC <$> getZonedTime

> getZonedTime
2018-09-17 13:49:33.169797528 JST

> t
2018-09-17 05:49:36.757498845 UTC

> utcToZonedTime <$> getCurrentTimeZone <*> t
2018-09-17 14:49:40.930944714 JST
```

上手くいってますね！

## Data.Time.LocalTime

時刻の取得・計算ができたら、あとは整形して出力するだけです！

Data.Time.LocalTime モジュールの関数を使って出力を整形してみましょう！

### formatTime

[formatTime](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Format.html#v:formatTime) 関数の使い方がわかれば、任意の形式で出力できるようになります。

```shell
> :t formatTime
formatTime :: FormatTime t => TimeLocale -> String -> t -> String
```

ここで `FormatTime t` の `t` は `UTCTime` や `ZonedTime`、`Day` などの型が使えます。

```haskell
formatTime :: TimeLocale -> String -> ZonedTime -> String
formatTime :: TimeLocale -> String -> UTCTime -> String
formatTime :: TimeLocale -> String -> Day -> String
```

型に応じて第三引数が変わるということです。

実際に使えばすぐに慣れます。(第一引数の値は [defaultTimeLocale](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Format.html#v:defaultTimeLocale) を指定しておけば良いのですが、自分でカスタマイズしたものを使うこともあります)

第二引数がフォーマット文字列なので、空文字列を与えれば当然結果も空になります。

```shell
> formatTime defaultTimeLocale "" <$> getZonedTime
""
```

フォーマットの指定方法については [haddock](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Format.html#v:formatTime) を参照してください。

```shell
> formatTime defaultTimeLocale "%D" <$> getZonedTime
"09/17/18"

> formatTime defaultTimeLocale "%F" <$> getZonedTime
"2018-09-17"

> formatTime defaultTimeLocale "%x" <$> getZonedTime
"09/17/18"

> formatTime defaultTimeLocale "%Y/%m/%d-%T" <$> getZonedTime
"2018/09/17-13:52:21"

> formatTime defaultTimeLocale rfc822DateFormat <$> getZonedTime
"Sun, 16 Sep 2018 19:53:10 JST"

> formatTime defaultTimeLocale (iso8601DateFormat Nothing) <$> getZonedTime
"2018-09-16"
```

## 文字列をパーズして ZonedTime や Day の値を作る

ここまでは現在時刻を元に時刻の計算や出力結果の整形を行いました。

しかし、実際のプログラムでは文字列をパースして `ZonedTime` や `Day` の値に変換したいこともあるでしょう。そのような場合は [parseTimeM](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Format.html#v:parseTimeM) を使うと便利です。

```shell
> :t parseTimeM
parseTimeM
  :: (Monad m, ParseTime t) =>
     Bool -> TimeLocale -> String -> String -> m t
```

型がわかりづらいですが、具体的にはこんな型で利用することができます。

```haskell
parseTimeM :: Bool -> TimeLocale -> String -> String -> IO Day
parseTimeM :: Bool -> TimeLocale -> String -> String -> IO ZonedTime
parseTimeM :: Bool -> TimeLocale -> String -> String -> Maybe Day
parseTimeM :: Bool -> TimeLocale -> String -> String -> Maybe ZonedTime
```

- 第一引数は **空白** を許容するかどうかのフラグです (True だと空白OK)
- 第二引数は気にせず **defaultTimeLocale** を指定しておきましょう
- 第三引数は **パーズで利用するフォーマット** を指定します
- 第四引数は **入力の文字列** です

### 具体例

実際にいくつか使ってみましょう。以下の通り `%F` は `YYYY-MM-DD` の書式になります。

```shell
> formatTime defaultTimeLocale "%F" <$> getZonedTime
"2018-09-17"
```

モナドを `IO` や `Maybe` などに変化させた基本的な例。

```shell
> parseTimeM True defaultTimeLocale "%F" "2018-09-17" :: IO ZonedTime
2018-09-17 00:00:00 +0000

> parseTimeM True defaultTimeLocale "%F" "2018-09-17" :: Maybe ZonedTime
Just 2018-09-17 00:00:00 +0000
```

第一引数を変化させて、入力文字列の空白の有無について確認する例。

```shell
> parseTimeM True defaultTimeLocale "%F" " 2018-09-17 " :: IO ZonedTime
2018-09-17 00:00:00 +0000

> parseTimeM False defaultTimeLocale "%F" " 2018-09-17 " :: IO ZonedTime
*** Exception: user error (parseTimeM: no parse of "2018-09-17 ")
```

入力文字列とパーズの書式がマッチしない例

```shell
> parseTimeM False defaultTimeLocale "%x" " 2018-09-17 " :: IO ZonedTime
*** Exception: user error (parseTimeM: no parse of " 2018-09-17 ")
```

Day 型の値をとしてパーズする例

```shell
> parseTimeM True defaultTimeLocale "%F" "2018-09-17" :: IO Day
2018-09-17
```

このようにして日付を取得できれば、今回は説明していませんが [Data.Time.Calendar](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Calendar.html) の [addDays](https://www.stackage.org/haddock/lts-12.9/time-1.8.0.2/Data-Time-Calendar.html#v:addDays) 関数などを使って日付の計算を行うこともできるようになります。

```shell
> d = parseTimeM True defaultTimeLocale "%F" "2018-09-17" :: IO Day

> addDays 1 <$> d
2018-09-18

> addDays 35 <$> d
2018-10-22
```

## まとめ

- time パッケージを使うと時刻や日付の計算ができる
- 現在の**日本**時間を取得した場合は **getCurrentTime** ではなく、**getZonedTime** を使う
- 整形には **formatTime** を使う
- 文字列から **ZonedTime** や **Day** に変換する際は **parseTimeM** を使う

Haskell入門の **7.7 日付・時刻を扱う** にも3ページほど **time** パッケージの解説があるので、気になる人はそちらも確認してみると良いかもしれません。

以上です。

## おまけ

`getZonedTime` に対して `formatTime defaultTimeLocale <フォーマット文字>` の対応表です。

```shell
> getZonedTime
2018-09-17 14:44:52.052040178 JST
```

<div class="narrow-table">
文字 | 出力結果
-------------|---------
`%-z` | `+900`
`%_z` | `+ 900`
`%0z` | `+0900`
`%^z` | `+0900`
`%#z` | `+0900`
`%8z` | `+00000900`
`%_12z` | `+         900`
`%%` | `%`
`%t` | `\t`
`%n` | `\n`
`%z` | `+0900`
`%Z` | `JST`
`%c` | `Mon Sep 17 14:39:34 JST 2018`
`%R` | `14:39`
`%T` | `14:40:12`
`%X` | `14:40:31`
`%r` | `02:40:55 PM`
`%P` | `pm`
`%p` | `PM`
`%H` | `14`
`%k` | `14`
`%I` | `02`
`%l` | ` 2`
`%M` | `43`
`%S` | `49`
`%q` | `903244678000`
`%Q` | `.28084722`
`%s` | `1537163079`
`%D` | `09/17/18`
`%F` | `2018-09-17`
`%x` | `09/17/18`
`%Y` | `2018`
`%y` | `18`
`%C` | `20`
`%B` | `September`
`%b` | `Sep`
`%h` | `Sep`
`%m` | `09`
`%d` | `17`
`%e` | `17`
`%j` | `260`
`%f` | `20`
`%V` | `38`
`%u` | `1`
`%a` | `Mon`
`%A` | `Monday`
`%U` | `37`
`%w` | `1`
`%W` | `38`
</div>