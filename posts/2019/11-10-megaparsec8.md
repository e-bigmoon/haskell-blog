---
title: Megaparsec 8
author: Mark Karpov
translator: Wataru Yamada
tags: megaparsec, package, 翻訳
---

Great original post: [Megaparsec 8](https://markkarpov.com/post/megaparsec-8.html)

一年が経ち、Megaparsecの新しいメジャーバージョンが再び登場する時がきました。
今回の変更は、
これまでのメジャーリリースの中で最も破壊的ではない変更です。
実際、
ほとんどのユーザはアップグレードのために何もする必要はないと思います。

<!--more-->

これには次の理由があります。

- それほど多くのissue が開かれておらず、バグも報告されていません。それはMegaparsecが最近、そしてほとんど満足のいく方法で「うまくいく」という事実と関係があると思います。

- ライブラリは現在幅広く使用されています。この記事の執筆時点で、Megaparsecに直接依存する[Hackageのパッケージは146個](https://packdeps.haskellers.com/reverse/megaparsec)あります。Megaparsecの上に構築することを選択した[新しい刺激的なライブラリ](https://hackage.haskell.org/package/replace-megaparsec)も現れました。[Idris](https://github.com/idris-lang/Idris-dev)や[Dhall](https://github.com/dhall-lang/dhall-haskell)などのプロジェクトでは、Megaparsecを使用してパースの問題を解決しています。

これらはライブラリが枯れ、成熟したことを示しているので、
動作しているものを壊さないようにしましょう。
とはいえ、常に改善の余地があります。

## Nixによる品質保証

バージョン8の作業を開始する前に、
Ni​​xを使用して品質保証を強化することにしました。
現在のMegaparsecに依存するプロジェクトの数を理解し、
Nixを使用して[Ormolu](https://github.com/tweag/ormolu)のバグ発見に成功した経験を思い出したことから、
依存パッケージによって引き起こされる破壊的変更、
パフォーマンスの変更、およびバグのチェックにNixを用いることにしました。

結果をMegaparsecのリポジトリにある[HACKING.md](https://github.com/mrkkrp/megaparsec/blob/master/HACKING.md)ファイルにドキュメント化しました。
開発時のシェルとは別に、Nixの式は以下のターゲットグループを提供します。

- `base` は `parser-combinators` や `hspec-megaparsec` などの密接に関連したパッケージとそのテストです。`nix-build -A base --no-out-link` を実行することにより、開発者はこれらすべてをビルドし、テストに通すことができます。

- `deps` は選択された依存関係の集合のもとでビルドの破壊とテストスイートの失敗が起きないことを確認します。

- `benches` はベンチマークのコレクションです。これには、Megaparsecのマイクロベンチマークと、ライブラリが実際のタスクでどのように実行されるかを示すいくつかのパッケージが含まれます。

これらコマンドのそれぞれで、
特定のパッケージまたはベンチマークに「ズームイン」するための
アクセスができます。
たとえば、`nix-build -A benches.parsers-bench` を実行して、
`parsers-bench` のベンチマークを確認できます。
要するに、ほとんどのパッケージは新しい変更でも引き続き動作し、
修正が容易ではないものでも動作します。
実際、システムの使用を続けるには、
失敗したパッケージにパッチを適用する必要があったため、
[アップグレード用のパッチ](https://github.com/mrkkrp/megaparsec/tree/31b917b1297950c22925f9ee7f7a588834293103/nix/patches)を入手できます。

ロジックやパフォーマンスの低下は見つかりませんでした。

## パースエラーの位置の制御

新機能について話しましょう。
プリミティブ `failure` と `fancyFailure` は
`parseError` に置き換えられました。

```haskell
parseError :: MonadParsec e s m => ParseError s e -> m a

-- 現在の 'failure' と 'fancyFailure' は普通の関数:

failure
  :: MonadParsec e s m
  => Maybe (ErrorItem (Token s)) -- ^ 期待しないアイテム (あれば)
  -> Set (ErrorItem (Token s)) -- ^ 期待するアイテム
  -> m a
failure us ps = do
  o <- getOffset
  parseError (TrivialError o us ps)

fancyFailure
  :: MonadParsec e s m
  => Set (ErrorFancy e) -- ^ Fancy error components
  -> m a
fancyFailure xs = do
  o <- getOffset
  parseError (FancyError o xs)
```

これはプリミティブの数を減らすことではありません
(減らすことも良いことですが)。
`parseError` の主な特徴は、
パーサの状態からの現在のオフセットは必要なく、
任意のオフセットでパースエラーを報告できることです。
これは、既にその位置を超えて移動した場合でも、
入力の特定の部分を指すようにパースエラーを作成する場合に重要です。
これまでは、まず `getOffset` を介して正しいオフセットを取得し、
次にパースエラーを報告する直前に `setOffset` で
オフセットを設定することでしか達成できませんでした。
これは見苦しいだけでなく、エラーが発生しやすくなり、
正しいオフセットの復元を忘れることがあります。
`mmark`の[実例](https://github.com/mmark-md/mmark/blob/8f5534d8068c2b7a139b893639ee5920bcaedd84/Text/MMark/Parser.hs#L787-L790)を次に示します。

```haskell
  o' <- getOffset
  setOffset o
  (void . hidden . string) "[]"
  -- ↑ これが失敗した場合、これをオフセット「o」で報告する必要があります
  setOffset (o' + 2)
```
ここでは完全な状況を説明しませんが、
`"[]"`(`+ 2`の部分)のパース後にオフセットの増分を考慮するのを忘れたため、
このコードにはしばらくバグがあったと言えば十分です。
次のように書けば、同じことをよりうまく表現できます。

```haskell
  region (setErrorOffset o) $
    (void . hidden . string) "[]"

-- 備考

region :: MonadParsec e s m
  => (ParseError s e -> ParseError s e)
     -- ^ 'ParseError' の処理方法
  -> m a
     -- ^ 処理を適用する「領域」
  -> m a
```

`getOffset` / `setOffset` ハックと同じように行うために`region`が使用され、
副作用として、もしエラーが起きたらパースエラーを更新する関数によって
現在のオフセットが変更されます。
`region` は `parseError` を使用して、古いハックを廃止できます。

```haskel
region f m = do
  r <- observing m
  case r of
    Left err -> parseError (f err)
    Right x -> return x
```

いいね.

## マルチエラーパーサのより良いストーリー

プロジェクトの最初期から、
マルチエラーパーサをサポートする方向にゆっくりと動いていました。
バージョン7では、`ParseError`の代わりに
`ParseErrorBundle`を返すようになりました。
マルチエラーレボリューションのすべてが整っていましたが、
複数のパースエラーを報告するちゃんとした方法がありませんでした。


マルチエラーパーサを使用するための1つの前提条件は、
入力に問題のある部分をスキップして、
正常であることがわかっている位置からパースを再開できることです。
この部分は、`withRecovery`プリミティブ
（Megaparsec 4.4.0以降で使用可能）を使用して実現されます。

```haskell
-- | @'withRecovery' r p@ は、パーサー @p@ が失敗した場合でも解析を続行できます。
-- この場合、実際の 'ParseError' を引数とする @r@ が呼び出されます。
-- よくある使い方として、特定のオブジェクトのパースの失敗を意味する値を返すことで、
-- その入力の一部を消費し次のオブジェクトの開始位置に移動する。
--
-- @r@ が失敗すると、元のエラーメッセージが 'withRecovery' なしで報告されることに注意してください。
-- パーサ @r@ を回復してもエラーメッセージに影響することはありません。


withRecovery
  :: (ParseError s e -> m a) -- ^ 失敗の回復方法
  -> m a             -- ^ オリジナルのパーサ
  -> m a             -- ^ 失敗から回復できるパーサ
```

Megaparsec 8 までのユーザーは、
成功と失敗の可能性を含む直和型になるように型`a`を選択する必要がありました。
たとえば、`Either (ParseError s e) Result` です。
パースエラーを収集し、
後で表示する前に手動で`ParseErrorBundle`に追加する必要がありました。
言うまでもなく、これらはすべて、
ユーザーフレンドリーではない高度な使用例です。

Megaparsec 8 は、遅延パースエラーのサポートを追加します。

```haskell
-- | 後で報告するために 'ParseError'を登録します。
-- このアクションはパースを終了せず、パースの最後に考慮される
-- 「遅延」'ParseError'のコレクションに特定の「ParseError」を
-- 追加する以外は効果がありません。 このコレクションが空の場合のみ、
-- パーサは成功します。 これは、複数のパースエラーを一度に報告する
-- 主な方法です。

registerParseError :: MonadParsec e s m => ParseError s e -> m ()

-- | 'failure'に似ていますが、 遅延'ParseError'のためのものです。

registerFailure
  :: MonadParsec e s m
  => Maybe (ErrorItem (Token s)) -- ^ 期待しないアイテム (あれば)
  -> Set (ErrorItem (Token s)) -- ^ 期待するアイテム
  -> m ()

-- | 'fancyFailure'に似ていますが、 遅延'ParseError'のためのものです。

registerFancyFailure
  :: MonadParsec e s m
  => Set (ErrorFancy e) -- ^ Fancy error components
  -> m ()
```

これらのエラーは `withRecovery` のエラー処理コールバックに登録でき、
結果の型は `Maybe Result` になります。
これにより、遅延エラーが最終的な `ParseErrorBundle` に含まれるようになり、
遅延エラーのコレクションが空でない場合に
パーサが最終的に失敗するようになります。

以上のことから、
マルチエラーパーサを書く習慣が
ユーザ間でより一般的になることを願っています。

## その他

- いつものように、変更の完全なリストについては、[chagelog](https://github.com/mrkkrp/megaparsec/blob/master/CHANGELOG.md) を参照してください。
- [公式チュートリアル](https://markkarpov.com/megaparsec/megaparsec.html)を含むすべてのテキストをバージョン8と互換性があるように更新しました。新しい機能の使用方法を説明するセクションを含めるように拡張しました。
- `hspec-megaparsec` などのサテライトパッケージが更新され、バージョン8で動作するようになりました。
