---
title: Route attributes
date: 2018/03/18
---

## ルート属性

ルート属性 (Route attributes) により, ルート記載の中にルート毎のメタデータをセットできる. 構文は単純であり, エクスクラメーションマークの後に値を入れるだけでよい. 使い方もまた単純である: `routeAttrs` 関数を使えば良いだけである.

それらすべてがどのように組み合わさるかを理解するのは容易である. また, 望むのなら, モチベーションに富んだ例もある. 個人的にルート属性を最も用いる場面は, 管理ルートに注釈をつける時である. 約12個の異なる管理アクションを持ったウェブサイトがあると想定しよう. 手動で `requireAdmin` の呼び出しを付け足したり, これに似たものを各アクションの最初に付け足すことができるが, 以下のようなデメリットがある.

1. 面倒である.
1. 間違いのもとになる. また, 付け足すことを忘れやすい.
1. さらに悪いことに, それを忘れたことに気付くのが困難である.

`isAuthorized` メソッドを管理ルートの明確なリストと共に修正することは少しはましであるが, 忘れた際に一目で理解するのは難しい.

このような理由により, ルート属性を利用した方が良い. 具体的には, ルート定義におけるそれぞれの関連部分に単一ワードを加え, `isAuthorized` の中でその属性を確認するだけでよい. コードを見てみよう!

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
    authHttpManager = error "no http manager provided"

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

実際のところ, これはかなりひどいものである. さらに `Admin3R` におけるセキュリティホールも見つかると思う.

## 代替方法: 階層的ルート

ある場合において用いられる他の方法としては階層的ルート (hierarchical routes) がある. これにより, 多数の関連するルートを1つの親の下にまとめられる. 仮に, 全ての管理ルートを1つのURL構造 (例: `/admin`) に保持したいのであれば, これは適した解決策である. 階層的ルートの使い方はかなり単純であり, パス, 名前, コロンから成るルート宣言を1行加えるだけで良い. 例: `/admin AdminR:`

そして, 全ての子ルートをその行の下に置き, 少なくとも1つのスペースでインデントする.

```haskell
  /1 Admin1R GET
  /2 Admin2R GET
  /3 Admin3R GET
```

型安全URLを用いてこれらのルートを参照するためには, 単純に `AdminR` コンストラクタでラップするだけでよい.例: `AdminR Admin1R`. 以下のコードは前述のルート属性の例を階層的ルートで書き直したものである.

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
    authHttpManager = error "no http manager provided"

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

## 属性を持つ階層的ルート

もちろん, 2つの方法を合わせることができる. 階層的ルートの子は親の属性を継承する. 例えば:

```haskell
/admin AdminR !admin:
    /1 Admin1R GET !1
    /2 Admin2R GET !2
    /3 Admin3R Get !3
```

`AdminR Admin1R` は `admin` と `1` の属性を持つ.

このテクニックを用いて, 最初の例のように `admin` 属性を `isAuthorized` 関数で用いることができる. `Admin3R` でしてしまったように, 属性の付加を忘れることがない. 階層的ルートに対応するオリジナルのコードと比較し, この手法には実際の利点は存在しない. 両方の手法はともかく, 同値である. 我々は `(AdminR _)` のパターンマッチを ``"admin" `member` routeAttrs route`` で置き換えただけである. しかし, 管理ページが全て同じurl構造に集約されず, 異なるサブツリーに属する場合, 長所がより明確になる.

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

`/admin` と `/a/admin` の下のページは全て `admin` 属性を持ち, ``"admin" `member` routeAttrs route`` を用いて確認できる. `(AdminR _)` によるパターンマッチは, この例ではうまくいかず, `/a/admin/*` ではなく `/admin/*` ルートにのみにマッチする.