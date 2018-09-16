---
title: ルート属性
date: 2018/09/16
---

ルート属性はルート毎にメタデータをセットするための機能です。構文はびっくりマークの後に続けて属性値を記述するだけです。また、使い方は `routeAttrs` 関数を使うだけです。

この機能を理解するのはとても簡単ですし、実際に私は管理用ルートの注釈を追加するためにこの機能を利用しています。約12個の異なる管理アクションを持った Web サイトを考えてみて下さい。手動で `requireAdmin` の呼び出しを追加したり、各アクションの最初にそういった処理を追加することもできると思いますが、以下のようなデメリットあります。

1. この作業はとても面倒です。
1. そういった手作業による追加・削除というのは間違いのもとになりやすいです。
1. 最悪なのは、ミスに気付くのが難しいということです。

管理用ルートの明示的なリストを持つように `isAuthorized` メソッドを変更すれば少しは良くなるかもしれませんが、ミスをした時にすぐに見つけるのはやはり難しいです。

このような理由があるので、ルート属性を利用した方が良いでしょう。具体的には、管理用ルートにしたいルート定義に単語を1つ追加し、`isAuthorized` でその属性を確認するだけです。コードを見てみてましょう！

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Set         (member)
import           Data.Text        (Text)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/unprotected UnprotectedR GET
/admin1 Admin1R GET !admin
/admin2 Admin2R GET !admin
/admin3 Admin3R GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    authRoute _ = Just $ AuthR LoginR
    isAuthorized route _writable
        | "admin" `member` routeAttrs route = do
            muser <- maybeAuthId
            case muser of
                Nothing -> return AuthenticationRequired
                Just ident
                    -- Just a hack since we're using the dummy module
                    | ident == "admin" -> return Authorized
                    | otherwise -> return $ Unauthorized "Admin access only"
        | otherwise = return Authorized

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Hacky YesodAuth instance for just the dummy auth plugin
instance YesodAuth App where
    type AuthId App = Text

    loginDest _ = HomeR
    logoutDest _ = HomeR
    getAuthId = return . Just . credsIdent
    authPlugins _ = [authDummy]
    maybeAuthId = lookupSession credsKey

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Route attr homepage"
    [whamlet|
        <p>
            <a href=@{UnprotectedR}>Unprotected
        <p>
            <a href=@{Admin1R}>Admin 1
        <p>
            <a href=@{Admin2R}>Admin 2
        <p>
            <a href=@{Admin3R}>Admin 3
    |]

getUnprotectedR, getAdmin1R, getAdmin2R, getAdmin3R :: Handler Html
getUnprotectedR = defaultLayout [whamlet|Unprotected|]
getAdmin1R = defaultLayout [whamlet|Admin1|]
getAdmin2R = defaultLayout [whamlet|Admin2|]
getAdmin3R = defaultLayout [whamlet|Admin3|]

main :: IO ()
main = warp 3000 App
```

これは素晴らしいね。`Admin3R` のセキュリティホールも見つけやすいでしょ？

## もう1つの方法: 階層的ルート

いくつかのケースで利用される別の方法として、階層的ルート (***hierarchical routes***) があります。階層的ルートを使えば、多数の関連するルートを1つの親の下にまとめることができます。例えば、全ての管理用ルートを1つの URL 構造 (例: `/admin`) の下層に配置したい場合などに適しています。階層的ルートの使い方はとても簡単です。パス、名前、コロンという規則のルート宣言を1行追加するだけです。例:

```haskell
/admin AdminR:
```

そして、全ての子ルートをその行の下に少なくとも1つのスペースでインデントして配置します。

```haskell
  /1 Admin1R GET
  /2 Admin2R GET
  /3 Admin3R GET
```

また、子ルートを `AdminR` コンストラクタでラップすることで、型安全URLから参照できます。例: `AdminR Admin1R`。以下のコードは先程のルート属性の例を階層的ルートで書き直したものです。

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Set         (member)
import           Data.Text        (Text)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Dummy

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/unprotected UnprotectedR GET
/admin AdminR:
    /1 Admin1R GET
    /2 Admin2R GET
    /3 Admin3R GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    authRoute _ = Just $ AuthR LoginR
    isAuthorized (AdminR _) _writable = do
        muser <- maybeAuthId
        case muser of
            Nothing -> return AuthenticationRequired
            Just ident
                -- Just a hack since we're using the dummy module
                | ident == "admin" -> return Authorized
                | otherwise -> return $ Unauthorized "Admin access only"
    isAuthorized _route _writable = return Authorized

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Hacky YesodAuth instance for just the dummy auth plugin
instance YesodAuth App where
    type AuthId App = Text

    loginDest _ = HomeR
    logoutDest _ = HomeR
    getAuthId = return . Just . credsIdent
    authPlugins _ = [authDummy]
    maybeAuthId = lookupSession credsKey

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Route attr homepage"
    [whamlet|
        <p>
            <a href=@{UnprotectedR}>Unprotected
        <p>
            <a href=@{AdminR Admin1R}>Admin 1
        <p>
            <a href=@{AdminR Admin2R}>Admin 2
        <p>
            <a href=@{AdminR Admin3R}>Admin 3
    |]

getUnprotectedR, getAdmin1R, getAdmin2R, getAdmin3R :: Handler Html
getUnprotectedR = defaultLayout [whamlet|Unprotected|]
getAdmin1R = defaultLayout [whamlet|Admin1|]
getAdmin2R = defaultLayout [whamlet|Admin2|]
getAdmin3R = defaultLayout [whamlet|Admin3|]

main :: IO ()
main = warp 3000 App
```

## 属性付き階層的ルート

もちろん、2つの方法を合わせることも可能です。階層的ルートの子は親の属性を継承します。例えば:

```haskell
/admin AdminR !admin:
    /1 Admin1R GET !1
    /2 Admin2R GET !2
    /3 Admin3R GET !3
```

`AdminR Admin1R` は属性として `admin` と `1` を持ちます。

このテクニックを使えば、最初の例のように `admin` 属性を `isAuthorized` 関数で利用できます。`Admin3R` に属性を付け忘れてしまうようなこともありません。今回のような例では、オリジナルの階層的ルートのコードと比較して、上記の属性付き階層的ルートの方が優れているということはありません。`(AdminR _)` のパターンマッチを ``"admin" `member` routeAttrs route`` で置き換えただけなので、全く同じです。しかし、管理ページを全て同じ URL 構造の下に配置するのではなく、異なるサブツリーにする場合はこちらの方が良いでしょう。

```haskell
/admin AdminR !admin:
    /1 Admin1R GET
    /2 Admin2R GET
    /3 Admin3R GET

/a AR !a:
  /1 A1R GET
  /2 A2R GET
  /admin AAdminR !admin:
    /1 AAdmin1R GET
    /2 AAdmin2R GET
```

`/admin` と `/a/admin` の下のページは全て `admin` 属性を持ち、 ``"admin" `member` routeAttrs route`` でチェックできます。しかし、`(AdminR _)` によるパターンマッチでは、この例を上手く扱えません。なぜなら、`/admin/*` ルートにはマッチしますが、`/a/admin/*` にはマッチしないためです。

## 本書のコード

- [Example01.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/examples/ex09/Example01.hs)
- [Example02.hs](https://github.com/e-bigmoon/haskell-blog/tree/master/sample-code/yesod/examples/ex09/Example02.hs)