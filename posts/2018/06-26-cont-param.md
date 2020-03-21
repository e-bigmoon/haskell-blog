---
title: ContT を使ってコードを綺麗にしよう！
author: Shinya Yamaguchi
tags: bigmoon
updated: 2018/06/26
---

## はじめに

Continuation (継続) について全く勉強したことが無いので [Control.Monad.Cont](https://www.stackage.org/haddock/lts-11.15/mtl-2.2.2/Control-Monad-Cont.html) で定義されている ContT とかいつ使うんだろうなーと思っていましたが、ついに利用機会がありました！！！

僕が考えたんじゃなくて fumieval さんに相談して、教えてもらったんですけどね。

<!--more-->

## 問題のコード

**Yesod** の **Handler** ではパラメータの取得するために [lookupGetParam](https://www.stackage.org/haddock/lts-11.15/yesod-core-1.6.5/Yesod-Core-Handler.html#v:lookupGetParam) や [lookupPostParam](https://www.stackage.org/haddock/lts-11.15/yesod-core-1.6.5/Yesod-Core-Handler.html#v:lookupPostParam) を利用すると思います。

実際にはこんな感じでパラメータを取得していくつか処理を行います。

```haskell
deleteTestR :: Handler Html
deleteTestR = do
  mParam <- lookupPostParam "key"

  case mParam of
    Nothing -> returnJson $ String "パラメータが不正です。"
    Just param ->
      case textToSqlKey param of
        Nothing -> returnJson $ String "キーが見つかりませんでした。"
        Just key -> do
          mRecord <- runDB $ get key
          case mRecord of
            Nothing -> returnJson $ String "削除対象のデータが見つかりませんでした。"
            Just _  -> returnJson $ String "success"
```

このコード、どう考えても嫌な感じですよね・・・。ネストやばいし。

**do** で書くと **Maybe** 型なので値を返せないし、ベースに **Handler** モナドがあるので **Either** で置き換えるのも良くわかんないな・・・。と思って、結構放置してました。

## ContT を使ってリファクタリング！

先程のプログラムを [ContT](https://www.stackage.org/haddock/lts-11.15/mtl-2.2.2/Control-Monad-Cont.html#t:ContT) で置き換えるとこうなります。

```haskell
deleteTestR :: Handler Html
deleteTestR = do
  mParam <- lookupPostParam "key"

  evalContT $ do
    param <- mParam !? "パラメータが不正です。"
    key <- textToSqlKey param !? "キーが見つかりませんでした。"
    mRecord <- lift $ runDB $ get key
    deletedBrand <- mRecord !? "削除対象のデータが見つかりませんでした。"
    lift $ returnJson $ String "success"
  where
    Nothing !? e = ContT $ const $ returnJson $ String e
    Just a  !? _ = ContT ($ a)
```

感動しましたね。継続すごいな！って。

## まとめ

継続勉強しよ。