---
title: Cookie のセキュリティ
date: 2018/08/04
---

## セッションを保存しているクッキー

Yesod では `_SESSION` というクッキーにセッションIDが格納されます。この名前は [clientSessionBackend](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/src/Yesod.Core.Class.Yesod.html#clientSessionBackend) 関数内部でハードコーディングされている。

もし変更したい場合は `Yesod` 型クラスの [makeSessionBackend](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/src/Yesod.Core.Class.Yesod.html#defaultClientSessionBackend) を自分で実装する。その際は [defaultClientSessionBackend](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/src/Yesod.Core.Class.Yesod.html#defaultClientSessionBackend) が参考になる。

## セッションに指定されているデフォルトの属性

[loadClientSession](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/src/Yesod.Core.Class.Yesod.html#loadClientSession) の中に定義がある。

```hs
[AddCookie defaultSetCookie
  { setCookieName = sessionName
  , setCookieValue = encodeClientSession key iv date host sess'
  , setCookiePath = Just "/"
  , setCookieExpires = Just (csdcExpires date)
  , setCookieDomain = Nothing
  , setCookieHttpOnly = True
  }
]
```

この結果

- `Domain`: 設定無し
- `HttpOnly`: 設定有り

ということがわかる。

## クッキーを追加した時のデフォルト値

[setCookie](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core-Handler.html#v:setCookie) を使うことでクッキーを追加できる。

[SetCookie](https://www.stackage.org/haddock/lts-12.4/cookie-0.4.4/Web-Cookie.html#t:SetCookie) 型の値は [defaultSetCookie](https://www.stackage.org/haddock/lts-12.4/cookie-0.4.4/Web-Cookie.html#v:defaultSetCookie) を使って、必要な部分のみを変更すると良い。

```hs
defaultSetCookie :: SetCookie
defaultSetCookie = SetCookie
    { setCookieName     = "name"
    , setCookieValue    = "value"
    , setCookiePath     = Nothing
    , setCookieExpires  = Nothing
    , setCookieMaxAge   = Nothing
    , setCookieDomain   = Nothing
    , setCookieHttpOnly = False
    , setCookieSecure   = False
    , setCookieSameSite = Nothing
    }
```

その際、デフォルト値は上記のようになっている。