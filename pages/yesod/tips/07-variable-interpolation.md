---
title: 変数展開処理の流れ
published: 2018/08/04
updated: 2020/03/03
---

## 変数展開記法

`hamlet` では `#{...}` という形式で変数展開を行うことができます。

```hs
[hamlet|
  #{var}
|]
```

HTML のエスケープなどもこの段階で行われます。

今回はこの処理の流れを追ってみようと思います。

## Text -> Html 型への変換部分

今回は `Text` 型について処理を追うが、その他の型についても大筋の流れは同じです。

### 【ステップ1】 toHtml (in yesod-core)

`#{var}` は実際のところ **yesod-core** パッケージが再エクスポートしている `blaze-html` の [toHtml][1] 関数を適用するだけです。

つまり、以下の式と同じです。

```hs
toHtml var
```

[1]: https://hackage.haskell.org/package/blaze-html-0.9.1.2/docs/Text-Blaze-Html.html#v:toHtml

### 【ステップ2】 toHtml (in blaze-html)

**toHtml** の実装は以下のようになっています。

```hs
toHtml :: ToMarkup a => a -> Html
toHtml = toMarkup
```

[toMarkup][2] メソッドは **blaze-markup** パッケージで定義されています。

[2]: https://hackage.haskell.org/package/blaze-markup-0.8.2.3/docs/Text-Blaze.html#v:toMarkup

### 【ステップ3】 toMarkup (in blaze-markup)

**toMarkup** メソッドは [ToMarkup][3] 型クラスのメソッドなので、実装は型によって異なります。

今回は `Text` 型に着目しているため、`Text` 型のインスタンス定義を確認しましょう。

```hs
instance ToMarkup Text where
  toMarkup = text
  preEscapedToMarkup = preEscapedText
```

**text** 関数は [Text.Blaze.Internal][4] モジュールで定義されています。

[3]: https://hackage.haskell.org/package/blaze-markup-0.8.2.3/docs/Text-Blaze.html#t:ToMarkup
[4]: https://hackage.haskell.org/package/blaze-markup-0.8.2.3/docs/Text-Blaze-Internal.html#v:text

### 【ステップ4】 text (in blaze-markup)

`text` 関数の実装は以下の通りです。

```hs
text :: Text    -- ^ Text to render.
     -> Markup  -- ^ Resulting HTML fragment.
text = content . Text
```

ここで出現する `Text` データコンストラクタは [ChoiceString][5] 型の値です。

**content** 関数もまた `Text.Blaze.Internal` モジュールで定義されています。(export はされていません)


[5]: https://hackage.haskell.org/package/blaze-markup-0.8.2.3/docs/Text-Blaze-Internal.html#t:ChoiceString

### 【ステップ5】 content (in blaze-markup)

`content` 関数の実装は以下の通りです。

```hs
content :: ChoiceString -> Markup
content cs = Content cs ()
```

ここで出現する `Content` は [MarkupM][6] 型の値です。

つまりここまでの流れとまとめると、以下のようになります。

```hs
#{var}
= toHtml var
= toMarkup var
= text var
= content $ Text var
= Content (Text var) ()
```

次は、ディスパッチの処理を追っていきましょう。

[6]: https://hackage.haskell.org/package/blaze-markup-0.8.2.3/docs/Text-Blaze-Internal.html#t:MarkupM

## ディスパッチ処理

典型的なハンドラは以下のような定義とすることが多いでしょう。

```hs
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  mParam <- lookupGetParam "p"

  [whamlet|
    #{maybe "" id mParam}
  |]
```

ここで注意する点は `getHomeR` の型は `Handler Text` ではなく `Handler Html` とした点です。

つまり [defaultLayout][7] を適用した結果はまだエスケープ処理に移る前段階ということです。

ではどこでエスケープ処理が行われているのでしょう？確認するためにはハンドラが実際に呼び出される場所を特定すれば良いでしょう。

具体的には [YesodDispatch][8] 型クラスがハンドラを実際に呼び出しています。

[7]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core.html#v:defaultLayout
[8]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core.html#t:YesodDispatch

### 【ステップ1】YesodDispatch 型クラス

ここで少し問題となるのが、`YesodDispatch` のインスタンスは `TH` によって自動生成されるという点です。

[TH で生成されるコードの確認方法](./08-TH.html) を参考に生成されるコードを確認してみましょう。

```hs
data App = App

mkYesod "App" [parseRoutes|
/check1 Check1R GET
|]

instance Yesod App

getCheck1R :: Handler Html
getCheck1R = defaultLayout $ do
  mParam <- lookupGetParam "p"

  [whamlet|
    #{maybe "" id mParam}
  |]
```

生成されるコードは次のようになります。(読みやすいように、少々整形しています)

```hs
mkYesod "App" $
  [ ResourceLeaf
      Resource
        { resourceName = "Check1R"
        , resourcePieces = [Static "check1"]
        , resourceDispatch =
            Methods
              { methodsMulti = Nothing
              , methodsMethods = ["GET"]
              }
        , resourceAttrs = []
        , resourceCheck = True
        }
  ]
======>
instance ParseRoute App where
  parseRoute (["check1"], _) = Just Check1R
  parseRoute (_, _) = Nothing

instance RenderRoute App where
  data Route App = Check1R
    deriving (Show, Eq, Read)
  renderRoute Check1R = ([pack "check1")], [])

instance RouteAttrs App where
  routeAttrs Check1R {} = fromList []

resourcesApp :: [ResourceTree String]
resourcesApp =
  [ ResourceLeaf
      Resource
        { resourceName = "Check1R"
        , resourcePieces = [Static "check1"]
        , resourceDispatch =
            Methods
              { methodsMulti = Nothing
              , methodsMethods = ["GET"]
              }
        , resourceAttrs = []
        , resourceCheck = True
        }
  ]

type Handler = HandlerFor App
type Widget = WidgetFor App ()

instance YesodDispatch App where
  yesodDispatch env req = helper (pathInfo req)
    where
      helper ["check1"] =
        case requestMethod req of
          "GET" -> yesodRunner getCheck1R env (Just Check1R) req
          _     -> yesodRunner (void badMethod) env (Just Check1R) req
      helper _ = yesodRunner (void notFound) env Nothing req
```

色々と生成されているが、ここで着目するのは `yesodDispatch` メソッドです。

その中で `yesodRunner getCheck1R env (Just Check1R) req` という形で [yesodRunner][9] が呼ばれていることがわかります。

[9]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core.html#v:yesodRunner

### 【ステップ2】yesodRunner (in yesod-core)

yesodRunner の実装は以下の通りです。(長いので必要な部分のみ抜き出しています)

```hs
yesodRunner handler' YesodRunnerEnv {..} route req sendResponse
...
        yar <- runInternalState (runHandler rhe handler yreq') is
...
    handler = yesodMiddleware handler'
```

まずは [yesodMiddleware][10] に適用されますが、型が変化しないためここではエスケープ処理が行われていないことがわかります。

```hs
yesodMiddleware :: ToTypedContent res => HandlerFor site res -> HandlerFor site res
```

次に [Yesod.Core.Internal.Run][11] モジュールで定義されている [runHandler][12] 関数に制御がうつります。

[10]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core.html#v:yesodMiddleware
[11]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/src/Yesod.Core.Internal.Run.html
[12]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/src/Yesod.Core.Internal.Run.html#runHandler

### 【ステップ3】runHandler

`runHandler` の実装は以下の通りです。(ここでも必要な部分のみを掲載しています)

```hs
runHandler rhe@RunHandlerEnv {..} handler yreq = withInternalState $ \resState -> do
    (state, contents0) <- basicRunHandler rhe handler yreq resState
...
```

次は [basicRunHandler][13] が呼ばれます。

[13]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/src/Yesod.Core.Internal.Run.html#basicRunHandler

### 【ステップ4】basicRunHandler

`basicRunHandler` の実装は以下の通りです。(ここでも必要な部分のみを掲載しています)

```hs
basicRunHandler rhe handler yreq resState = do
...
            res <- unHandlerFor handler (hd istate)
            tc <- evaluate (toTypedContent res)
...
```

次に `unHandlerFor` が呼ばれます。

### 【ステップ5】unHandlerFor

[unHandlerFor][14] は [HandlerFor][15] 型を剥がすための関数で、実装は以下のようになっています。

```hs
newtype HandlerFor site a = HandlerFor
    { unHandlerFor :: HandlerData site site -> IO a
    }
    deriving Functor
```

ここで `HandlerFor site a` が `IO a` に変化します。

つまり先程の `res` の型が `Html` だということがわかります。

```hs
res <- unHandlerFor handler (hd istate)
tc <- evaluate (toTypedContent res)
```

そのため、次は [toTypedContent][16] を見ていく必要があります。

[14]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core-Types.html#v:unHandlerFor
[15]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core-Types.html#t:HandlerFor
[16]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core-Content.html#v:toTypedContent

### 【ステップ6】toTypedContent

`toTypedContent` は [ToTypedContent][17] 型クラスのメソッドです。

`Html` 型のインスタンス定義は次のようになっています。

```hs
instance ToTypedContent Html where
  toTypedContent h = TypedContent typeHtml (toContent h)
```

`toContent` を見てみましょう。

[17]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core-Content.html#t:ToTypedContent

### 【ステップ7】toContent

`toContent` は [ToContent][18] 型クラスのメソッドであり `Html` 型のインスタンス定義は以下のようになっています。

```hs
instance ToContent Html where
  toContent bs = ContentBuilder (renderHtmlBuilder bs) Nothing
```

ここでやっとレンダリング関数の [renderHtmlBuilder][19] が出現しました。

[18]: https://hackage.haskell.org/package/yesod-core-1.6.17.3/docs/Yesod-Core-Content.html#t:ToContent
[19]: https://hackage.haskell.org/package/blaze-html-0.9.1.2/docs/Text-Blaze-Html-Renderer-Utf8.html#v:renderHtmlBuilder

## レンダリング処理

### 【ステップ1】renderHtmlBuilder

[renderHtmlBuilder][20] の実装は以下の通りです。

```hs
import qualified Text.Blaze.Renderer.Utf8 as R

renderHtmlBuilder :: Html -> Builder
renderHtmlBuilder = R.renderMarkupBuilder
```

[20]: https://hackage.haskell.org/package/blaze-html-0.9.1.2/docs/Text-Blaze-Html-Renderer-Utf8.html#v:renderHtmlBuilder

### 【ステップ1】renderMarkupBuilder

[renderMarkupBuilder][21] の実装は以下のようになっています。

ここで、この関数に渡される値は `Content (Text var) ()` のような形になっていたことを思い出そう。該当するパターンマッチのみを掲載します。

```hs
renderMarkupBuilder :: Markup     -- ^ Markup to render
                  -> Builder  -- ^ Resulting builder
renderMarkupBuilder = go mempty
  where
    go :: Builder -> MarkupM b -> Builder
    ...
    go _ (Content content _) = fromChoiceString content
```

単純に `fromChoiceString` を適用するだけのようです。

[21]: https://hackage.haskell.org/package/blaze-markup-0.8.2.3/docs/Text-Blaze-Renderer-Utf8.html#v:renderMarkupBuilder

### 【ステップ2】fromChoiceString

[fromChoiceString][22] の実装は以下のようになっています。

```hs
import qualified Blaze.ByteString.Builder           as B
import qualified Blaze.ByteString.Builder.Html.Utf8 as B
...

fromChoiceString :: ChoiceString  -- ^ String to render
                 -> Builder       -- ^ Resulting builder
fromChoiceString (Static s)     = B.copyByteString $ getUtf8ByteString s
fromChoiceString (String s)     = B.fromHtmlEscapedString s
fromChoiceString (Text s)       = B.fromHtmlEscapedText s
fromChoiceString (ByteString s) = B.fromByteString s
fromChoiceString (PreEscaped x) = case x of
    String s -> B.fromString s
    Text   s -> B.fromText s
    s        -> fromChoiceString s
fromChoiceString (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then mempty else B.fromString s
    Text   s     -> if "</" `T.isInfixOf` s then mempty else B.fromText s
    ByteString s -> if "</" `S.isInfixOf` s then mempty else B.fromByteString s
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x `mappend` fromChoiceString y
fromChoiceString EmptyChoiceString = mempty
```

今回関係するのは `fromChoiceString (String s) = B.fromHtmlEscapedText s` なので `fromHtmlEscapedText` を確認しましょう。

[22]: https://hackage.haskell.org/package/blaze-markup-0.8.2.3/docs/src/Text.Blaze.Renderer.Utf8.html#fromChoiceString

### 【ステップ3】fromHtmlEscapedText

[fromHtmlEscapedString][23] 関数の実装は以下の通りです。

```hs
charUtf8HtmlEscaped :: P.BoundedPrim Char
charUtf8HtmlEscaped =
    condB (>  '>' ) (condB (== '\DEL') P.emptyB P.charUtf8) $
    condB (== '<' ) (fixed4 ('&',('l',('t',';')))) $        -- &lt;
    condB (== '>' ) (fixed4 ('&',('g',('t',';')))) $        -- &gt;
    condB (== '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $  -- &amp;
    condB (== '"' ) (fixed6 ('&',('q',('u',('o',('t',';')))))) $  -- &#quot;
    condB (== '\'') (fixed5 ('&',('#',('3',('9',';'))))) $  -- &#39;
    condB (\c -> c >= ' ' || c == '\t' || c == '\n' || c == '\r')
          (P.liftFixedToBounded P.char7) $
    P.emptyB
  where
    {-# INLINE fixed4 #-}
    fixed4 x = P.liftFixedToBounded $ const x >$<
      P.char7 >*< P.char7 >*< P.char7 >*< P.char7

    {-# INLINE fixed5 #-}
    fixed5 x = P.liftFixedToBounded $ const x >$<
      P.char7 >*< P.char7 >*< P.char7 >*< P.char7 >*< P.char7

    {-# INLINE fixed6 #-}
    fixed6 x = P.liftFixedToBounded $ const x >$<
      P.char7 >*< P.char7 >*< P.char7 >*< P.char7 >*< P.char7 >*< P.char7
```

つまり、この関数内で実際のエスケープ処理が走っていることがわかります。

[23]: https://hackage.haskell.org/package/blaze-builder-0.4.1.0/docs/Blaze-ByteString-Builder-Html-Utf8.html#v:fromHtmlEscapedString

## まとめ

- `<`, `>`, `&`, `"`, `'` の5文字がエスケープされます。
