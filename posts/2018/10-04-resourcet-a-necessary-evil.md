---
title: 必要悪の ResourceT (翻訳)
author: pythonissam
tags: fpcomplete, 翻訳
---

[原文](https://www.fpcomplete.com/blog/2018/10/resourcet-necessary-evil)

![NECESSARY EVUL](https://www.fpcomplete.com/hubfs/download.jpeg?t=1539658811949)

言い換えると、「ResourceT は害悪だ」ということです。

要約を書いておきます。ResourceT はすばらしいツールで、制約付きのリソースと実行時例外を扱うときの、実際に出てくる問題を解決するのに使われます。しかし、野生の世界では、こいつは必要ではない状況でもよく使われているようです。ResourceT についての情報をもっと仕入れたいのなら、[README.md](https://github.com/snoyberg/conduit/tree/master/resourcet#readme) を見てみてください。

Haskell でファイルをコピーするときにはどうしていますか? 明らかな答えは無視して (`System.Directory.copyFile`)、イラっとくる答えを出してみましょう:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import System.Exit
import System.Process
main = rawSystem "cp" ["src", "dest"] >>= exitWith
```

もちろん、[バイナリの I/O 関数](https://www.snoyman.com/blog/2016/12/beware-of-readfile)が欲しくなりますよね? 1つの方法として、正格な `ByteString` バージョンの `readFile` と `writeFile` を使ってみましょう:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import qualified Data.ByteString as B
main = B.readFile "src" >>= B.writeFile "dest"
```

残念ながら、こいつは入力のファイルが大きかったとき、メモリを無制限に使ってしまいます。そのため、遅延 (???) IO を使いましょう:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import qualified Data.ByteString.Lazy as BL
main = BL.readFile "src" >>= BL.writeFile "dest"
```

しかし、ここにも別の問題があるのです。リソースの使い方が決定的ではありません。もしも `dest` に書き込んでいる途中に例外が投げられたら、`src` のファイルディスクリプタがクローズされる保証ができなくなります。このような小さなプログラムならまぁ問題ありませんが、実行時間の長いマルチスレッドを持つアプリケーションなら、ファイルディスクリプタが枯渇することで、プロセス全体が落ちる可能性があります。

まぁ、これはストリーミングデータ関連のライブラリに精通している人にとっては古臭い話題でしょう。同じように、この問題の解決策として私が書いたライブラリ ([conduit](https://haskell-lang.org/library/conduit)) を使った方法を示しても、別に驚くことでもないでしょう (たぶん):

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import Conduit
main = runConduit $ sourceFile "src" .| sinkFile "dest"
```

これは全く問題なく動きそうですが、コンパイルが失敗します:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import Conduit
main = runConduit $ sourceFile "src" .| sinkFile "dest"
```

目を凝らして脳力を発揮すれば、次第に理解できると思います。さっきの例の正格な I/O バージョンは、無制限のメモリを使うことで、ファイルディスクリプタがリークすることを防いでいました。このおかげでファイルディスクリプタをすぐに閉じることができます。遅延型 (???) の I/O は、ファイルディスクリプタを長くオープンしておくことで、メモリの問題を解決していますが、リークする可能性もあります。Conduit は、型レベルでどちらの問題も解決することを強制しています。Conduit そのものはメモリの使い方について言及するものですが、そこで使われている別の登場人物がいます。それが ResourceT です。ResourceT は例外が起こった場合でも、ファイルディスクリプタが閉じられることを保証するためのものです。

嬉しいことに、この問題を解決するのは簡単です。`runResourceT` を使いましょう:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import Conduit
main = runResourceT
     $ runConduit
     $ sourceFile "src" .| sinkFile "dest"
```

このパターンは conduit を使う上でかなりよく見かけるので、ビルトインのヘルパー関数が存在しています:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import Conduit
main = runConduitRes $ sourceFile "src" .| sinkFile "dest"
```

こんな感じのコードは conduit の世界のいたるところに存在しています。えぇ、私のドキュメントの中にもよく出てきますよ! 今日はその罪滅ぼしをしましょうか。

# ResourceT はなぜ必要なの?
上の例ではちょろまかしていましたが、私は型が ResourceT を使うように強制する、と説明しました。これは事実なのですが、論理的に考えてなぜこの概念が必要なのでしょうか? 説明しましょう:

* Conduit はコルーチンベース
* コルーチンベースのコードは、例外のハンドラを適切に設置することができない
  * この理由は直感的ではないですが、説明してみます。コルーチンベースのシステムでは、yield したときや await したとき (???)、実行のコントロールを別のコンポーネントに渡していることになります。他のコンポーネントが実行しているアクションに、例外ハンドラを設置することはできません。
* これをどうにかするために、`ResourceT` を持つこの resourcet というライブラリを使います。これで例外が起きた場合でも、リソースを解放するようなアクションを登録することができます。

訳者注: コルーチンはおそらく、jmp 命令や goto みたいなものです (???)

よし、なので `sourceFile` や `sinkFile` を使うためには ResourceT を使う必要があるわけです。これらの関数は couduit のパイプラインでファイルディスクリプタを確保しますが、それを解放するアクションが実行されることを担保できないため、`ResourceT` を使わないといけないのですね。うん、いいんじゃないでしょうか。

# ResourceT なんていらないぜ
しかし、ResourceT はかなり強力なツールです。思い通りに新しく解放処理をするアクションを動的に登録することができます。私たちの場合、そんな力は必要ありません! (より簡単な方法を後でお見せしますが) 例を見てみましょう:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import Conduit
import System.IO
main =
  withBinaryFile "src" ReadMode $ \src ->
  withBinaryFile "dest" WriteMode $ \dest ->
  runConduit $ sourceHandle src .| sinkHandle dest
```

リソースの確保をしていますが、ここに動的な部分はありませんね。2つファイルを開いており、1つは読み、もう1つは書き込みのためです。私たちは、例外のイベントが発生したとき、これらのファイルディスクリプタがクローズされることを保証しなければなりません (ついでに言えば、正常に終了すればいいんですけどね)。この種のワークフローはよく知られていて、Haskell の世界でも使われています。そのため、`withBinaryFile` のような、以上言及したこと全てをやってくれるような、標準関数が存在しているわけです。もっと一般的な言葉を使うなら、これは**ブラケットパターン**と表現することができます。`withBinaryFile` のような関数の実装には、`bracket` 関数が使われているからです。

もちろん、上のコードは冗長なだけではなく、エラーを生み出す原因にもなります。間違えて `ReadMode` と `WriteMode` を入れ変えてしまったりとか。これを聞いてありえないと思った方、原因は私にあります。ここまでのチュートリアルでは `ResourceT` ベースのアプローチを書いていて気持ちが良かったのですが... とはいえ、couduit にはこれを簡単に解決し、エラーも簡単に分かるヘルパー関数があります:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import Conduit
main =
  withSourceFile "src" $ \src ->
  withSinkFile "dest" $ \dest ->
  runConduit $ src .| dest
```

`sourceFile`/`sinkFile` のアプローチよりもまだ冗長です。しかし、必要のない重量級のアプローチを導入することを考えれば、採算の取れるコストではあります。自分でコードを描くときはもちろんですが、何か書いたり教えたりする場では、この方向でいこうと思っています。

# ResourceT を使いすぎることの欠点
さて、ResourceT が**重量級**であることは何回も言及してきましたが、これは実際に問題になるのでしょうか? まぁ問題になるんですけど、いくつか理由を挙げてみましょう:

1. **パフォーマンス** ResourceT にはわずかながら、パフォーマンス上のオーバーヘッドがあります。一般的にこれはさほど問題にはならないのですが、以下の理由から最初に挙げることにしました:
  * 世間の人はパフォーマンスについて語るのが大好き
  * ここで挙げる理由の中では、一番明確な指標になる
2. **複雑度** ResourceT はモナドトランスフォーマで、これは多くの人の知るところとなったわけですが、私はますますこいつに懐疑的になっています。また、ResourceT の中の値の生存期間について誤解が見られますが(???)、これはブラケットパターンには見られなかったものです。
3. **リソースの寿命が長すぎる** ResourceT から作られた値がすでに解放されているにもかかわらず、その値を使ってしまったために、プロダクションのコードでも多くのバグが出てくるのを見てきました。これはブラケットパターンでも起こりうるものですが、どういうわけか ResourceT の方がバグを覆い隠す能力が高いようです。不自然な例かもしれませんが、以下のコードを考えてみましょう:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import Conduit
import Control.Monad.Trans.Resource
import System.IO
main = do
  (_, src) <- runResourceT $ allocate (openFile "src" ReadMode) hClose
  (_, dest) <- runResourceT $ allocate (openFile "dest" WriteMode) hClose
  runConduit $ sourceHandle src .| sinkHandle dest
```

この場合、`src` と `dest` のどちらも、

* `allocate` によって作られた
* `hClose` を呼ぶ解放のためのアクション (???) が登録されている
* `runResourceT` の実行が終わり、解放処理が実行される
* にも関わらず、ファイルハンドルが `runResourceT` の外で返されている

次はもっとありうる例です。`transPipe` を使ってこれを正しくやろうとしたとき、以下のようなバグが生まれるのを私は今まで何回も見てきました:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-12.10 script
import Conduit
import Control.Monad.Trans.Resource
import System.IO
main = runConduit
  $ transPipe runResourceT (sourceFile "src")
 .| transPipe runResourceT (sinkFile "dest")
```

この最後の例は、最近トランスフォーマに恐怖を覚える理由の一部を示しています。

これらの問題をうまく解決してくれる型ベースの方法はあります。[regions](https://github.com/basvandijk/regions#readme) です。(もちろん) [Oleg によって開発](http://okmij.org/ftp/Haskell/regions.html#light-weight)されました。動くことには動きますが、このアイディアが広く受け入れられることはありませんでした。私が思うに、型をうまくやりとりするコストが高すぎたのではないかと思います。

おもしろいことに、regions は Rust の生存期間 (???) の概念とそこまで大きな違いはありません。よりおもしろいのは、この領域は [RAII](https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization) (Resource Acquisition Is Initialization) というアプローチで、C++ と Rust は早すぎるクローズを避けることで、 Haskell のブラケットパターンよりも良い感じの解決策を取っていることです。
4. 私は ResourceT が Haskell における非同期例外関連のバグを取り除く、素晴らしい方法であるかのような宣伝を見かけてきました。理論はこうです。ResourceT さえ使えば、非同期例外について考える必要すら無くなる、`allocate` を適切に使えば完璧だ!

私の意見は違います。実際のところ、必要になるころには、リソースが長生きしすぎていて終わると思います (???)。非同期例外についての学習を避けている内は、絶対にうまく扱うことはできません。私がおすすめするのは、

* [unliftio](https://www.stackage.org/package/unliftio) や [safe-exceptions](https://www.stackage.org/package/safe-exceptions) など、非同期例外を正しく扱うようにデザインされたライブラリを使うこと
* リソースを確保するためには、`bracket` のような正しい関数を使うこと
* 非同期例外の詳細について、時間をかけて理解すること。私が書いた[超詳しいブログ記事](https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell)や、[ウェブの発表](https://www.fpcomplete.com/blog/how-to-handle-asynchronous-exceptions-in-haskell)があります。

訳者注: 超詳しいです。[リンク]()

これは本当に思うのですが、**必要のないところでは resourcet を使わないで**ください。でもはい、もちろん、重要な質問が1つ残っていますね。

# なんで ResourceT なんてものがあるのか
