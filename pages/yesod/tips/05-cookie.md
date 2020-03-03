---
title: Cookie のセキュリティ
published: 2018/08/04
updated: 2020/03/03
---

## セッションを保存しているクッキー

Yesod では `_SESSION` というクッキーにセッションIDが格納されます。この名前は [clientSessionBackend][1] 関数内部でハードコーディングされている。

もし変更したい場合は `Yesod` 型クラスの [makeSessionBackend][2] を自分で実装する。その際は [defaultClientSessionBackend][3] が参考になる。

[1]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core.html#v:clientSessionBackend
[2]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core.html#v:makeSessionBackend
[3]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core.html#v:defaultClientSessionBackend

## セッションに指定されているデフォルトの属性

[loadClientSession][4] の中に定義がある。

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

[4]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core.html#v:loadClientSession

## クッキーを追加した時のデフォルト値

[setCookie][5] を使うことでクッキーを追加できる。

[SetCookie][6] 型の値は [defaultSetCookie][7] を使って、必要な部分のみを変更すると良い。

[5]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core-Handler.html#v:setCookie
[6]: https://hackage.haskell.org/package/cookie-0.4.5/docs/Web-Cookie.html#t:SetCookie
[7]: https://hackage.haskell.org/package/cookie-0.4.5/docs/Web-Cookie.html#v:defaultSetCookie

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
