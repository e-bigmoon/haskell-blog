---
title: Yesod には脆弱性があるのかな？
author: Shinya Yamaguchi, pythonissam
tags: bigmoon, yesod, security
---

## はじめに

[Yesod](https://www.yesodweb.com/) は Haskell で書かれた Web アプリケーションフレームワークです。

WordPress や Drupal と違って、セキュリティはかなり万全です。(ユーザが気にしなければならない部分が非常に少ないです)

最近、[体系的に学ぶ 安全なWebアプリケーションの作り方 第2版 (通称: 徳丸本)](https://www.sbcr.jp/products/4797393163.html) が発売されました。

勉強のため、本書の内容を Yesod で確認しているのですが、その中で面白い例を見つけました。

今回はその内容について紹介したいと思います。(セキュリティの専門家ではないので間違いがあればご指摘ください)

<!--more-->

## 脆弱性のあるコード

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-12.4
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  mname <- lookupGetParam "name"

  [whamlet|
    $maybe name <- mname
      <img onload="init('#{name}')" src="https://www.yesodweb.com/static/logo-home2-no-esod-smaller2.png">
    $nothing
      パラメータが設定されていません。
  |]

  toWidget [julius|
    function init(text) {
      // 何かしらの処理
    }
  |]

main :: IO ()
main = warp 3000 App
```

このコードには脆弱性があります。

**Yesod** の変数展開 `#{..}` は、このような **JavaScript** の動的生成でも大丈夫だろうと思っていたのですが、そうではありませんでした。

### 正常系

期待する動作として、例えば `http://localhost/?name=bigmoon` いう形でパラメータを渡すと、以下のような URL が組み立てられます。

```html
<img onload="init('bigmoon')" src="...">
```

これは予定通りです。

### 異常系

パラメータを `http://localhost/?name=%27),alert(XSS)//` とするとインジェクションが発生します。

```html
<img onload="init('%27),alert(1)//')" src="...">
```

読みやすさのため、パーセントエンコーディングを元に戻します。

```html
<img onload="init(''),alert(XSS)//')" src="...">
```

`alert` が出てきてしまいましたね・・・。

余談ですが **Yesod** ではパラメータ中に出現する `;` をパラメータの区切り文字として認識するようです。そのため `http://localhost/?name=%27);alert(XSS)//` ではインジェクションは発生しません。

## 原因

問題はどこにあるのでしょうか？

パラメータを取得しているコードはこの部分です。

```hs
mname <- lookupGetParam "name"
```

[lookupGetParam](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core-Handler.html#v:lookupGetParam) の型は以下の通りです。

```hs
lookupGetParam :: MonadHandler m => Text -> m (Maybe Text)
```

つまり、`mname :: Maybe Text` 型となってしまいます。ここが問題の原因です。

ただの **Text** 型なので変数展開時に **HTMLのエスケープ処理** が行われます。本来ならば **JavaScript** 用のエスケープ処理が必要なのです。

## 展開部分を改良してみる

では **Javascript** 型に変換すれば問題は解決するのでしょうか？

試しに以下のようにコードを変更してみました。

```hs
<img onload="init('#{renderJavascript $ toJavascript $ rawJS $ name}')" src="...">
```

ですが、やはり結果は同じです。

## エスケープ処理

ソースコードを追いかけてみるとどうやら **Value** の値については [string](https://www.stackage.org/haddock/lts-12.4/shakespeare-2.0.15/src/Text.Julius.html#string) というサニタイザーが適用されるようです。

```hs
string :: T.Text -> Builder
string s = {-# SCC "string" #-} singleton '"' <> quote s <> singleton '"'
  where
    quote q = case T.uncons t of
                Nothing      -> fromText h
                Just (!c,t') -> fromText h <> escape c <> quote t'
        where (h,t) = {-# SCC "break" #-} T.break isEscape q
    isEscape c = c == '\"' ||
                 c == '\\' ||
                 c == '<'  ||
                 c == '>'  ||
                 c == '&'  ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape '<' = "\\u003c"
    escape '>' = "\\u003e"
    escape '&' = "\\u0026"

    escape c
        | c < '\x20' = fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = singleton c
        where h = showHex (fromEnum c) ""
```

なので、先程のコードを少し修正してこの `string` を適用してみました。

```hs
<img onload="init('#{renderJavascript $ toJavascript $ rawJS $ string $ name}')" src="...">
```

しかし、これでもやっぱりだめです。なぜなら `'` に対してはサニタイズしていないからだと思います。

### string 関数の修正

以下のように `'` のエスケープ処理を追加したところ、インジェクションが発生しなくなりました。

```hs
isEscape c = c == '\"' ||
             c == '\\' ||
             c == '<'  ||
             c == '>'  ||
             c == '&'  ||
             c == '\'' || -- 追記
             c < '\x20'

escape '\"' = "\\\""
escape '\\' = "\\\\"
escape '\n' = "\\n"
escape '\r' = "\\r"
escape '\t' = "\\t"
escape '<' = "\\u003c"
escape '>' = "\\u003e"
escape '&' = "\\u0026"
-- 追記
escape '\'' = "\\\'"
```

## まとめ

- **JvasSript** を動的に組み立てない
- ユーザが気をつけなけば **Yesod** でも脆弱性が発生する
- 変数展開で全てがエスケープされると思っていたが、場所によっては問題が発生することがある

以上です。