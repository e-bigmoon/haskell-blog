---
title: 変数展開処理の流れ
date: 2018/08/04
---

## 変数展開記法

`hamlet` では `#{...}` という形式で変数展開を行うことができる。

```hs
[hamlet|
  #{var}
|]
```

HTML のエスケープなどもこの段階で行われる。

今回はこの処理の流れを追ってみようと思う。

## Text -> Html 型への変換部分

今回は `Text` 型について処理を追うが、その他の型についても大筋の流れは同じである。

### 【ステップ1】 toHtml (in yesod-core)

`#{var}` は実際のところ **yesod-core** パッケージの [toHtml](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core.html#v:toHtml) 関数を適用している。

つまり、以下の式と同じである。

```hs
toHtml var
```

また **yesod-core** で定義されている **toHtml** は **blaze-html** パッケージの [toHtml](https://www.stackage.org/haddock/lts-12.4/blaze-html-0.9.1.1/Text-Blaze-Html.html#v:toHtml) を再エクスポートしているだけである。

### 【ステップ2】 toHtml (in blaze-html)

**toHtml** の[実装](https://www.stackage.org/haddock/lts-12.4/blaze-html-0.9.1.1/src/Text.Blaze.Html.html#toHtml) は以下のとおりである。

```hs
toHtml :: ToMarkup a => a -> Html
toHtml = toMarkup
```

[toMarkup](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/Text-Blaze.html#v:toMarkup) メソッドは **blaze-markup** パッケージで定義されている。

### 【ステップ3】 toMarkup (in blaze-markup)

**toMarkup** メソッドは [ToMarkup](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/Text-Blaze.html#t:ToMarkup) のメソッドなので、実装は型によって異なる。

今回は `Text` 型に着目している。`Text` 型の[インスタンス定義](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/src/Text.Blaze.html#line-131) は以下の通りである。

```hs
instance ToMarkup Text where
  toMarkup = text
  preEscapedToMarkup = preEscapedText
```

**text** 関数は [Text.Blaze.Internal](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/src/Text.Blaze.Internal.html) モジュールで定義されている。

### 【ステップ4】 text (in blaze-markup)

[text](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/src/Text.Blaze.Internal.html) 関数の実装は以下の通り。

```hs
text :: Text    -- ^ Text to render.
     -> Markup  -- ^ Resulting HTML fragment.
text = content . Text
```

ここで出現する `Text` は [ChoiceString](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/Text-Blaze-Internal.html#t:ChoiceString) 型の値である。

**content** 関数もまた、[Text.Blaze.Internal](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/src/Text.Blaze.Internal.html) モジュールで定義されている。

### 【ステップ5】 content (in blaze-markup))

[content](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/src/Text.Blaze.Internal.html#content) 関数の実装は以下の通り。

```hs
content :: ChoiceString -> Markup
content cs = Content cs ()
```

ここで出現する `Content` は [MarkupM](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/Text-Blaze-Internal.html#t:MarkupM) 型の値である。

つまりここまでの流れとまとめると、以下のようになる

```hs
#{var}
= toHtml var
= toMarkup var
= text var
= content $ Text var
= Content (Text var) ()
```

次は、ディスパッチの処理を追っていこう。

## ディスパッチ処理

典型的なハンドラは以下のような定義とすることが多いだろう。

```hs
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  mParam <- lookupGetParam "p"

  [whamlet|
    #{maybe "" id mParam}
  |]
```

ここで注意する点は `getHomeR` の型は `Handler Text` ではなく `Handler Html` だという事実である。

つまり [defaultLayout](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core.html#v:defaultLayout) を適用した結果はまだエスケープ処理に移る前段階ということである。

ではどこでエスケープ処理が行われているのだろうか？

それはハンドラが実際に呼び出される場所を特定すれば良い。具体的に言えば [YesodDispatch](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core.html#t:YesodDispatch) 型クラスがハンドラを実際に呼び出している。

### 【ステップ1】YesodDispatch 型クラス

ここで少し問題となるのが、`YesodDispatch` のインスタンスは `TH` によって自動生成されるという点だ。

[TH で生成されるコードの確認方法](./08-TH.html) を参考に生成されるコードを確認してみよう。

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

生成されるコードは次のようになる。(読みやすいように、少々整形している)

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

色々と生成されているが、ここで着目するのは `yesodDispatch` メソッドである。

その中で `yesodRunner getCheck1R env (Just Check1R) req` という形で [yesodRunner](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core.html#v:yesodRunner) が呼ばれていることがわかる。

### 【ステップ2】yesodRunner (in yesod-core)

yesodRunner の[実装](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/src/Yesod.Core.Internal.Run.html#yesodRunner) は以下の通りである。(長いので必要な部分のみ抜き出した)

```hs
yesodRunner handler' YesodRunnerEnv {..} route req sendResponse
...
        yar <- runInternalState (runHandler rhe handler yreq') is
...
    handler = yesodMiddleware handler'
```

まずは [yesodMiddleware](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core.html#v:yesodMiddleware) に適用されるが、型が変化しないため、ここではエスケープ処理が行われていないことがわかる。

```hs
yesodMiddleware :: ToTypedContent res => HandlerFor site res -> HandlerFor site res
```

次に [Yesod.Core.Internal.Run](https://github.com/yesodweb/yesod/blob/master/yesod-core/Yesod/Core/Internal/Run.hs) モジュールで定義されている [runHandler](https://github.com/yesodweb/yesod/blob/master/yesod-core/Yesod/Core/Internal/Run.hs#L185) 関数に制御がうつる。

### 【ステップ3】runHandler

runHadler の実装は以下の通り。(ここでも必要な部分のみを掲載)

```hs
runHandler rhe@RunHandlerEnv {..} handler yreq = withInternalState $ \resState -> do
    (state, contents0) <- basicRunHandler rhe handler yreq resState
...
```

次は [basicRunHandler](https://github.com/yesodweb/yesod/blob/master/yesod-core/Yesod/Core/Internal/Run.hs#L61) が呼ばれる。

### 【ステップ4】basicRunHandler

basicRunHandler の実装は以下の通り。(ここでも必要な部分のみを掲載)

```hs
basicRunHandler rhe handler yreq resState = do
...
            res <- unHandlerFor handler (hd istate)
            tc <- evaluate (toTypedContent res)
...
```

次に unHandlerFor に適用される。

### 【ステップ5】unHandlerFor

[unHandlerFor](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core-Types.html#v:unHandlerFor) は [HandlerFor](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core-Types.html#t:HandlerFor) 型を剥がすための関数である。

[実装](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/src/Yesod.Core.Types.html#HandlerFor) は以下のようになっている。

```hs
newtype HandlerFor site a = HandlerFor
    { unHandlerFor :: HandlerData site site -> IO a
    }
    deriving Functor
```

ここで `HandlerFor site a` が `IO a` に変化する。

つまり先程の `res` の型が `Html` だということがわかる。

```hs
res <- unHandlerFor handler (hd istate)
tc <- evaluate (toTypedContent res)
```

そのため、次は [toTypedContent](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core-Content.html#v:toTypedContent) を見ていく必要がある。

### 【ステップ6】toTypedContent

toTypedContent は [ToTypedContent](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core-Content.html#t:ToTypedContent) 型クラスのメソッドである。

[Html 型のインスタンス定義](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/src/Yesod.Core.Content.html#line-287) は次のようになっている。

```hs
instance ToTypedContent Html where
  toTypedContent h = TypedContent typeHtml (toContent h)
```

toContent を見てみよう。

### 【ステップ7】toContent

toContent は [ToContent](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/Yesod-Core-Content.html#t:ToContent) 型クラスのメソッドであり [Html 型のインスタンス定義](https://www.stackage.org/haddock/lts-12.4/yesod-core-1.6.6/src/Yesod.Core.Content.html#line-102) は以下のようになっている。

```hs
instance ToContent Html where
  toContent bs = ContentBuilder (renderHtmlBuilder bs) Nothing
```

ここでやっとレンダリング関数の [renderHtmlBuilder](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/Text-Blaze-Renderer-Utf8.html#v:renderHtmlBuilder) が出現した。

## レンダリング処理

renderHtmlBuilder は DEPRECATE になっており、[renderMarkupBuilder](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/Text-Blaze-Renderer-Utf8.html#v:renderMarkupBuilder) の利用が推奨されている。

### 【ステップ1】renderMarkupBuilder

[renderMarkupBuilder](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/src/Text.Blaze.Renderer.Utf8.html#renderMarkupBuilder) の実装は以下のようになっている。

ここで、この関数に渡される値は `Content (Text var) ()` のような形になっていたことを思い出そう。該当するパターンマッチのみを掲載する。

```hs
renderMarkupBuilder
  :: Markup     -- ^ Markup to render
  -> Builder  -- ^ Resulting builder
renderMarkupBuilder = go mempty
  where
    go :: Builder -> MarkupM b -> Builder
    ...
    go _ (Content content _) = fromChoiceString content
```

単純に [fromChoiceString](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/Text-Blaze-Renderer-String.html#v:fromChoiceString) を適用するだけのようだ。

### 【ステップ2】fromChoiceString

`fromChoiceString` の[実装](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/src/Text.Blaze.Renderer.String.html#fromChoiceString)は以下のようになっている。

```hs
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> String        -- ^ String to append
                 -> String        -- ^ Resulting string
fromChoiceString (Static s)     = getString s
fromChoiceString (String s)     = escapeMarkupEntities s
fromChoiceString (Text s)       = escapeMarkupEntities $ T.unpack s
fromChoiceString (ByteString s) = (SBC.unpack s ++)
fromChoiceString (PreEscaped x) = case x of
    String s -> (s ++)
    Text   s -> (\k -> T.foldr (:) k s)
    s        -> fromChoiceString s
fromChoiceString (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then id else (s ++)
    Text   s     -> if "</" `T.isInfixOf` s then id else (\k -> T.foldr (:) k s)
    ByteString s -> if "</" `S.isInfixOf` s then id else (SBC.unpack s ++)
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x . fromChoiceString y
fromChoiceString EmptyChoiceString = id
```

今回関係するのは `fromChoiceString (String s) = escapeMarkupEntities s` なので [escapeMarkupEntities](https://www.stackage.org/haddock/lts-12.4/blaze-markup-0.8.2.1/src/Text.Blaze.Renderer.String.html#escapeMarkupEntities) を確認しよう。

### 【ステップ2】escapeMarkupEntities

escapeMarkupEntities 関数はエクスポートされていないが、実装は以下のようになっている。

```hs
escapeMarkupEntities :: String  -- ^ String to escape
                   -> String  -- ^ String to append
                   -> String  -- ^ Resulting string
escapeMarkupEntities []     k = k
escapeMarkupEntities (c:cs) k = case c of
    '<'  -> '&' : 'l' : 't' : ';'             : escapeMarkupEntities cs k
    '>'  -> '&' : 'g' : 't' : ';'             : escapeMarkupEntities cs k
    '&'  -> '&' : 'a' : 'm' : 'p' : ';'       : escapeMarkupEntities cs k
    '"'  -> '&' : 'q' : 'u' : 'o' : 't' : ';' : escapeMarkupEntities cs k
    '\'' -> '&' : '#' : '3' : '9' : ';'       : escapeMarkupEntities cs k
    x    -> x                                 : escapeMarkupEntities cs k
```

つまり、この関数内で実際のエスケープ処理が走っていることがわかる。

## まとめ

`<`, `>`, `&`, `"`, `'` の5文字がエスケープされる。