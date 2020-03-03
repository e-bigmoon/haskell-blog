---
title: XSS に対するセキュリティ
published: 2018/08/04
updated: 2020/03/03
---

## サンプルの基本形式

```hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/check CheckR GET
|]

instance Yesod App

getCheck1R :: Handler Html
getCheck1R = defaultLayout $ do
  mParam <- lookupGetParam "p"

  [whamlet|
    ここに各種処理を書きます。
  |]

main :: IO ()
main = warp 3000 App
```

雛形は上記を使います。

## HTML のエスケープ

### テンプレート

```hs
[whamlet|
  #{maybe "" id mParam}
|]
```

### URL

[`http://localhost:3000/check1?p=<script>alert(document.cookie)</script>`](http://localhost:3000/check1?p=<script>alert(document.cookie)</script>)

### 結果

上記のURLでアクセスした場合は以下のように `<` と `>` がエスケープされる。

```html
&lt;script&gt;alert(document.cookie)&lt;/script&gt;
```

そのため **XSS** は発生しない。(エスケープについての詳しい流れは[変数展開処理の流れ](./07-variable-interpolation.html)を参照してください)

## 画面の書き換え

### テンプレート

```hs
[whamlet|
  <form action="" method="POST">
    氏名<input name="name" value="#{maybe "" id mParam}"><br>
|]
```

### URL

[`http://localhost:3000/check2?p="></form>XSS"`](http://localhost:3000/check2?p="></form>XSS")

### 結果

以下のようにエスケープ処理 (`"`, `<`, `>`) が行われるため、安全

```hs
value="&quot;&gt;&lt;/form&gt;XSS&quot;"
```

## 引用符で囲まれない属性値のXSS

### テンプレート

```hs
[whamlet|
  <input type=text name=mail value=#{maybe "" id mParam}>
|]
```

### URL

[`http://localhost:3000/check3?p=1+onmouseover%3dalert(document.cookie)`](http://localhost:3000/check3?p=1+onmouseover%3dalert(document.cookie))

### 結果

以下のように `"` が無い場合は自動的に追加するため、安全

```hs
<input type="text" name="mail" value="1 onmouseover=alert(document.cookie)">
```

## href属性やsrc属性のXSS

### テンプレート

```hs
[whamlet|
  <a href=#{maybe "" id mParam}>
    ブックマーク
|]
```

### URL

[`http://localhost:3000/check4?p=javascript:alert(document.cookie)`](http://localhost:3000/check4?p=javascript:alert(document.cookie))

### 結果

以下のように JavaScript のエスケープは何も行われないため、XSS が発生する。

```hs
<a href="javascript:alert(document.cookie)">ブックマーク</a>
```

## JavaScript の動的生成

### テンプレート

```hs
[whamlet|
  <body onload="init('#{maybe "" id mParam}')">
|]

toWidget [julius|
  function init(text) {
    // 適当な処理
  }
|]
```

### URL

[`http://localhost:3000/check5?p='),alert(document.cookie)//`](http://localhost:3000/check5?p='\),alert(document.cookie)//)

### 結果

JavaScript のエスケープ処理が行われないため、XSS が発生する。

```html
<body onload="init('&#39;),alert(document.cookie)//')"></body>
<script>
  function init(text) {
    // 適当な処理
  }
</script>
```