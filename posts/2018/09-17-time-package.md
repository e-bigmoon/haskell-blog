---
title: time パッケージの使い方
author: Shinya Yamaguchi
tags: bigmoon, package
updated: 2020/06/20
---

## はじめに

Haskell で時間や日付を扱う際に良く利用されるのは [time][pkg-time] パッケージです。

このパッケージが使いやすいかどうかは人それぞれですが、使い方を知っておくと便利なのでよく使いそうな関数を簡単に解説しようと思います。

これからの例は以下のコマンドを実行していると仮定して話を進めます。

```shell
$ cabal repl -b time==1.10
```

[pkg-time]: https://hackage.haskell.org/package/time

<!--more-->

## Time パッケージのモジュール構造

基本的には [Data.Time][mod-time] を **import** して使います。

```haskell
import Data.Time
```

[mod-time]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time.html

`Data.Time` は以下のモジュールを再エクスポートしています。

モジュール名 | 用途
-------------|---------
[Data.Time.Calendar][mod-cal] | 日付
[Data.Time.Clock][mod-clock] | 全然使わないので良くわからない
[Data.Time.LocalTime][mod-localtime] | 日本の現在時刻を取得など
[Data.Time.Format][mod-format] | 出力の整形

[mod-cal]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Calendar.html
[mod-clock]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Clock.html
[mod-localtime]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-LocalTime.html
[mod-format]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Format.html

### rio を利用している場合

[rio][pkg-rio] を利用している場合は [RIO.Time][mod-rio-time] を **import** します。

```haskell
import RIO.Time
```

[pkg-rio]: https://hackage.haskell.org/package/rio
[mod-rio-time]: https://hackage.haskell.org/package/rio-0.1.17.0/docs/RIO-Time.html

## Data.Time.LocalTime

現在時刻を取得する場合にこのモジュールを使います。現在時刻を取得したいからと言って [getCurrentTime][func-getCurrentTime] を利用すると日本時間にならないので注意してください。

[func-getCurrentTime]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Clock-POSIX.html#v:getCurrentTime

### getZonedTime

システムのタイムゾーンに応じた現在時刻を返します。

```shell
> :t getZonedTime
getZonedTime :: IO ZonedTime

> getZonedTime
2020-06-20 13:18:40.677811323 JST
```

### getCurrentTimeZone

システムのタイムゾーンを取得します。このタイムゾーンに基づいて [getZonedTime][func-getZonedTime] が計算されます。

```shell
> :t getCurrentTimeZone
getCurrentTimeZone :: IO TimeZone

> getCurrentTimeZone
JST
```

[func-getZonedTime]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-LocalTime.html#v:getZonedTime

### zonedTimeToUTC

`ZonedTime` を `UTCTime` に変換するために使います。

```shell
> :t zonedTimeToUTC
zonedTimeToUTC :: ZonedTime -> UTCTime

> zonedTimeToUTC <$> getZonedTime
2020-06-20 04:20:14.529514141 UTC
```

### utcToZonedTime

`zonedTimeToUTC` の逆で `UTCTime` を `ZonedTime` に変換する関数です。タイムゾーンのための引数を余分に取ります。

```shell
> :t utcToZonedTime
utcToZonedTime :: TimeZone -> UTCTime -> ZonedTime

> utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime
2020-06-20 13:20:28.011749783 JST
```

## 1日後の時間を計算するには？

ここで、取得した時間の1日後を計算してみましょう。

そのためには `Data.Time.Clock` で定義されている [addUTCTime][func-addUTCTime] を使います。

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
> t1 = addUTCTime nominalDay . zonedTimeToUTC <$> getZonedTime
> getZonedTime
2020-06-20 13:22:26.700694373 JST

> utcToZonedTime <$> getCurrentTimeZone <*> t1
2020-06-21 13:22:33.553973172 JST
```

同様に1時間後も計算してみましょう。

```shell
> t2 = addUTCTime (60 * 60) . zonedTimeToUTC <$> getZonedTime
> getZonedTime
2020-06-20 13:22:58.351335073 JST

> t2
2020-06-20 05:23:04.594425732 UTC

> utcToZonedTime <$> getCurrentTimeZone <*> t2
2020-06-20 14:23:12.834203921 JST
```

上手くいってますね！

[func-addUTCTime]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Clock.html#v:addUTCTime

## Data.Time.LocalTime

時刻の取得・計算ができたら、あとは整形して出力するだけです！

`Data.Time.LocalTime` モジュールの関数を使って出力を整形してみましょう！

### formatTime

[formatTime][func-formatTime] 関数の使い方がわかれば、任意の形式で出力できるようになります。

```shell
> :t formatTime
formatTime :: FormatTime t => TimeLocale -> String -> t -> String
```

ここで `FormatTime t` の `t` は `UTCTime` や `ZonedTime`、`Day` などの型が使えます。

```haskell
formatTime :: TimeLocale -> String -> ZonedTime -> String
formatTime :: TimeLocale -> String -> UTCTime   -> String
formatTime :: TimeLocale -> String -> Day       -> String
```

型に応じて第三引数が変わるということです。

実際に使えばすぐに慣れます。(第一引数の値は [defaultTimeLocale][func-defaultTimeLocale] を指定しておけば良いのですが、自分でカスタマイズしたものを使うこともあります)

第二引数がフォーマット文字列なので、空文字列を与えれば当然結果も空になります。

```shell
> formatTime defaultTimeLocale "" <$> getZonedTime
""
```

フォーマットの指定方法については [haddock][func-formatTime] を参照してください。

```shell
> formatTime defaultTimeLocale "%D" <$> getZonedTime
"06/20/20"

> formatTime defaultTimeLocale "%F" <$> getZonedTime
"2020-06-20"

> formatTime defaultTimeLocale "%x" <$> getZonedTime
"06/20/20"

> formatTime defaultTimeLocale "%Y/%m/%d-%T" <$> getZonedTime
"2020/06/20-13:26:44"

> formatTime defaultTimeLocale rfc822DateFormat <$> getZonedTime
"Sat, 20 Jun 2020 13:26:50 JST"
```

[func-formatTime]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Format.html#v:formatTime
[func-defaultTimeLocale]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Format.html#v:defaultTimeLocale

## Data.Time.Format.ISO8601

**ISO8601** の書式は [Data.Time.Format.ISO8601][mod-iso] モジュールの [iso8601Show][func-iso8601] を利用します。

```shell
> iso8601Show <$> getZonedTime
"2020-06-20T13:31:08.7048868+09:00"
```

[mod-iso]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Format-ISO8601.html
[func-iso8601]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Format-ISO8601.html#v:iso8601Show

## 文字列をパーズして ZonedTime や Day の値を作る

ここまでは現在時刻を元に時刻の計算や出力結果の整形を行いました。

しかし、実際のプログラムでは文字列をパーズして `ZonedTime` や `Day` の値に変換したいこともあるでしょう。そのような場合は [parseTimeM][func-parseTimeM] を使うと便利です。

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
"2020-06-20"
```

モナドを `IO` や `Maybe` などに変化させた基本的な例。

```shell
> parseTimeM True defaultTimeLocale "%F" "2020-06-20" :: IO ZonedTime
2020-06-20 00:00:00 +0000

> parseTimeM True defaultTimeLocale "%F" "2020-06-20" :: Maybe ZonedTime
Just 2020-06-20 00:00:00 +0000
```

第一引数を変化させて、入力文字列の空白の有無について確認する例。

```shell
> parseTimeM True defaultTimeLocale "%F" " 2020-06-20 " :: IO ZonedTime
2020-06-20 00:00:00 +0000

> parseTimeM False defaultTimeLocale "%F" " 2020-06-20 " :: IO ZonedTime
*** Exception: user error (parseTimeM: no parse of " 2020-06-20 ")
```

入力文字列とパーズの書式がマッチしない例

```shell
> parseTimeM False defaultTimeLocale "%x" " 2020-06-20 " :: IO ZonedTime
*** Exception: user error (parseTimeM: no parse of " 2020-06-20 ")
```

`Day` 型の値をとしてパーズする例

```shell
> parseTimeM True defaultTimeLocale "%F" "2020-06-20" :: IO Day
2020-06-20
```

このようにして日付を取得できれば、今回は説明していませんが [Data.Time.Calendar][mod-cal] の [addDays][func-addDays] 関数などを使って日付の計算を行うこともできるようになります。

```shell
> d = parseTimeM True defaultTimeLocale "%F" "2020-06-20" :: IO Day

> addDays 1 <$> d
2020-06-21

> addDays 35 <$> d
2020-07-25
```

[func-parseTimeM]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Format.html#v:parseTimeM
[func-addDays]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Calendar.html#v:addDays

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
2020-06-20 13:41:37.314698155 JST
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
