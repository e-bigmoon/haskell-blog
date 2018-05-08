# 正格性のすべて

12 Sep 2017 Michael Snoyman

Haskell は (もしかすると、評判のよろしくない？) 遅延言語です。遅延性の基本的なアイデアは「値は必要となったときにのみ計算される」という、たった一言で説明できるぐらい簡単なものです。しかし、この裏には様々なことが隠れています。特に、メモリと時間について効率的なコードを書こうとしたときに、必要不可欠なトピックがいくつもあります。

- 弱頭部標準形 (WHNF) と 標準形 (NF)
- `seq` と `deepseq` 関数の使い方 (と関連する概念)
- データ型の正格性アノテーション
- バンパターン
- 遅延、スパイン(spine)-正格、値-正格などのデータ構造の正格性
- 適切な補助関数の選択 (特に、fold)

この記事は効率的な [conduit](https://haskell-lang.org/library/conduit) コードを書くためのいくつかの質問にインスパイアされたものであり、記事の最後でそれらについて本気で取り組んでみようと思います。ここで紹介する概念は一般的なものであり、ストリーミングライブラリ一般に限定されるものではありません。

**ノート**
この記事は現実とは逆に、遅延性を解決すべき問題として取り扱います。遅延性は有利にも不利にもなります。私たちの目標は遅延性の問題の大枠とその回避策を理解することなので、悪い点にのみ焦点を当てることにします。遅延性にはとても大きなメリットが数多くありますが、ここで取り上げることはしません。なぜなら、私の読者はコメントで遅延性の素晴らしさについて紹介している記事へのリンクをいくつも追加してくれるでしょうから :)

# 遅延性入門
さて、私は先ほど

> 値は必要となった時にのみ計算される

と言いました。詳細を知るために正格言語であるC言語と比較してみましょう。

```c
#include <stdio.h>

int add(int x, int y) {
  return x + y;
}

int main() {
  int five = add(1 + 1, 1 + 2);
  int seven = add(1 + 2, 1 + 3);

  printf("Five: %d\n", five);
  return 0;
}
```

私たちの関数 `add` は `x` と `y` の両方の引数で正格です。そして、結果もまた正格になります。つまり

- 初めて `add` が呼ばれる前に `1 + 1` と `1 + 2` の両方の結果を計算します。
- `2` と `3` で `add` 関数を呼び出した結果 `5` が得られ、それは変数 `five` によって指し示されるメモリーの値となります。
- 同じように `1 + 2`, `1 + 3` も計算し `seven` に `7` が格納されます。
- 完全に計算された `five` の値で `printf` を呼び出します。

さて、これと同じ Haskell コードを比較してみましょう。

```haskell
add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
  let five = add (1 + 1) (1 + 2)
      seven = add (1 + 2) (1 + 3)

  putStrLn $ "Five: " ++ show five
```

正格性解析と呼ばれる仕組みにより、説明よりも効率的な結果となることがありますが、意味的には以下の通りです。

- `1 + 1` や `1 + 2` の計算をすぐに行うのではなく、コンパイラはこれらの計算のサンク (契約として考えることができます)  を生成し、`add` 関数にサンクを渡します。
- `add` 関数をすぐに呼び出すという例外を除けば、`five` は `add` 関数を `1 + 1` と `1 + 2` のサンクに適用するというサンクです。
- `seven` に関しても同様に、`add` 関数を異なる2つのサンクに適用するというサンクです。
- 最終的に `five` を表示しようとする際に実際の数を知る必要があります。このことを強制評価 (forcing evaluation) と言い、あとで詳しく、いつ・どのように強制評価が起こるか説明しますが、今のところは `putStrLn` が実行された時と理解すれば十分です。`1 + 1` と `1 + 2` の強制評価を行う `five` の強制評価が行われ、サンクが実際の数 (`2`, `3`, 最終的に `5`) に変換されます。
- `seven` は一度も使われず、サンクとして残ったままとなりますが、このサンクを評価するための時間はかかっていません。

C の (正格) 評価と比較すると、使われることのない `seven` の値を評価するという無意味な処理を行わないという恩恵があります。これにより、処理を3つスキップできます！現実的な場面では3つではなく、もっとひどいコストのかかる処理かもしれません。

*だけども*、全てが素晴らしいものではありません。サンクはタダじゃないんです。我々はサンクのためにスペースを確保する必要があり、その確保と後にメモリ解放のために行われる GC を引き起こすコストの両方がかかります。たぶん一番大切なことは、式がサンク化されたものは、評価されたものよりもずっとコストがかかる可能性があるということです。データコストラクタのオーバーヘッドとの混乱を避ける (それだけで複雑になる) ために `five` の2つの表現方法を比較してみましょう。C において `five` は正確に1つのマシンワード\*を消費します。それに対して Haskell の `five` サンクはだいたい以下のようになります。

\* またはそれよりも少ないです `int` は32ビットですが、たぶんあなたは64ビットのマシンを使っているでしょう。しかし、整列問題によりレジスタを1つのマシンワードと言っても良いでしょう。

- 1つのマシンワードが "私はサンクです" と主張します
- サンクの中は `add` 関数と `1 + 1` と `1 + 2` のサンク (それぞれ1つのマシンワード) へのポインタとなっています。そのため合計で3つのマシンワードです。
- `1 + 1` のサンクはサンクのための1つのマシンワードと `+` 演算子と `1` の値へのポインタです。(GHC は int 自身の余分なオーバーヘッドを避けるためにメモリの専用部分に小さな int の値を保持する最適化を行いますが、理論的にはそれぞれの余分なマシンワードが追加される)。ここでもまた、少なくとも3つのマシンワードが必要となります。
- 同じことが `1 + 2` のサンクにも言えるので、3つのマシンワードとなります。
- 最終的な合計は **10マシンワード** となり、C のメモリ利用量と比較して10倍の差があります！

実際のところ、そういうわけではありません。私は正格性解析のステップについて少しお話しましたが、これは "やぁ、ちょっと待って、これは後で使うからサンクを確保するよりも2つの数字を加算する方が絶対良いって、じゃあまたね！" という感じです。しかしこれは、Haskell を書いて、遅延性やサンクがどこで発生するのか理解するときに重要になってきます。

## バン!
さて、どうすれば Haskell の評価をより正格にできるのでしょうか。一番簡単なのはバンパターンを使う方法です。まずはコードを先に確認してみましょう。

```haskell
{-# LANGUAGE BangPatterns #-}
add :: Int -> Int -> Int
add !x !y = x + y

main :: IO ()
main = do
  let !five = add (1 + 1) (1 + 2)
      !seven = add (1 + 2) (1 + 3)

  putStrLn $ "Five: " ++ show five
```

このコードは正格な C のコードと全く同じ動作をします。先ほどのコードとの違いは `add` 関数の `x` と `y` の前にバン (`!`) があることです。これによって GHC は `add` を評価する前に `x` と `y` の値を評価しなければならないと判断します。同様に `five` と `seven` の前にも `!` があるため、 GHC は `putStrLn` を評価する前にこれらの値を評価します。

Haskell には多くのものがありますが、バンパターンはただの `seq` 関数を使ったシンタックスシュガーです。`seq` 関数は以下のような型です。

```haskell
seq :: a -> b -> b
```

この型シグネチャを見ると、`a` の値を無視するような以下の実装にしたくなるでしょう。

```haskell
badseq :: a -> b -> b
badseq a b = b
```

けれども、 `seq` は GHC が提供しているプリミティブな操作を使って`b` が評価されるときには既に `a` が評価されていることを保証する関数です。ここで先ほどの `add` 関数をバンパターンの代わりに `seq` を使って書き換えてみましょう。

```haskell
add :: Int -> Int -> Int
add x y =
  let part1 = seq x part2
      part2 = seq y answer
      answer = x + y
   in part1
-- Or more idiomatically
add x y = x `seq` y `seq` x + y
```

これはこのような意味です。

- `part1` は `x` を評価した後に `part2` の値を評価します
- `part2` は `y` を評価した後に `answer` の値を評価します
- `answer` はそのまま `x + y` です

もちろん、`let` と `in` を使ってこんなに長いコードを書くのは大変なので、大抵のプログラマは最後の行のように `seq` を中置記法で使います。

**練習問題** `in part1` の代わりに `in part2` とした場合どうなるでしょうか？また `in answer` ではどうでしょうか？

バンパターンから `let` を使う方法への変換はどんな場合でも可能です。先ほどの `main` 関数は次のように書き換えても同じことです。

```haskell
main :: IO ()
main = do
  let five = add (1 + 1) (1 + 2)
      seven = add (1 + 2) (1 + 3)

  five `seq` seven `seq` putStrLn ("Five: " ++ show five)
```

これは `seq` の動作を理解するために非常に重要な例ですが、コードが読みやすくなるという点を除けばバンパターンを使ったものと同一です。なので、自分が読みやすい好きな方を使ってください。たぶん大半はバンパターンを使うでしょうけども。

## 評価を追ってみよう

今まで、サンクの評価について私の説明が全てでした。これから、評価について、もっと直接的に観測するための方法について説明します。`Debug.Trace` モジュールで定義されている `trace` 関数は評価された時にメッセージを表示します。以下のプログラムの出力を予想してください。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
import Debug.Trace

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
  let five = trace "five" (add (1 + 1) (1 + 2))
      seven = trace "seven" (add (1 + 2) (1 + 3))

  putStrLn $ "Five: " ++ show five
```

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
{-# LANGUAGE BangPatterns #-}
import Debug.Trace

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
  let !five = trace "five" (add (1 + 1) (1 + 2))
      !seven = trace "seven" (add (1 + 2) (1 + 3))

  putStrLn $ "Five: " ++ show five
```

それぞれのプログラムで表示される内容は・・・

もうわかりましたね。答えは以下の通りです。

- 最初のプログラムは `five` と `Five: 5` の両方を表示します。`seven` は式を評価する必要が無いため、表示されません。(出力のバッファリングによっては、これらの値の表示順序が入れ替わるという奇妙な現象が起きることがあります。)
- 2つ目のプログラムは `five` と `seven` の両方を表示します。なぜなら、バンパターンによってこれらの評価が強制されるからです。しかし、この表示順についてはあなたが期待するものと異なっていたのではないでしょうか。現に、私のシステムでは `five` が表示される前に `seven` が表示されました。なぜなら、この場合において GHC は評価順を並び替えることができるからです。
- 逆に ``five `seq` seven `seq` putStrLn ("Five: " ++ show five)`` としていたら、表示される順序は `five`, `seven`, `"Five: 5"` となっていたでしょう。これはバンパターンが単に `seq` に変換されるという先ほどの説明でほんの少しだけ嘘をついたからです。しかし、 ``x `seq` y`` という式は実際のところ、GHC からすれば `x` と `y` のどちらを先に評価したとしても、式の評価が終わった時に `x` と `y` の両方が評価されていることが保証されていれば良いのです。

とはいえ、あたなの式が本当に純粋であれば、`x` と `y` の評価がどちらから行われるかということは観測できなかったはずです。純粋では無い `trace` 関数を利用したからこそ、評価順を観測することができたんです。

**質問** もし、全ての `add` 関数に `!` をつけたら結果はどう変わるでしょうか？ なぜ `!` を付けるだけで出力に影響したり (しなかったり) するのでしょうか？

## ボトムの値
今までの例もちゃんと動きますが、評価の順番をデモするのに、より標準的な方法があります。ボトム、すなわち `undefined` を使うことです。`undefined` は、評価されたときに実行時例外を投げる点で特別です。 (他の特別な関数や値のように、`error` 関数も同じ動きをします。) バンなしで `seven` が評価されないことを再現するために、以下の2つのプログラムを比較してみましょう:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
{-# LANGUAGE BangPatterns #-}

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
  let five = add (1 + 1) (1 + 2)
      seven = add (1 + 2) undefined -- (1 + 3)

  putStrLn $ "Five: " ++ show five
```

と:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
{-# LANGUAGE BangPatterns #-}

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
  let five = add (1 + 1) (1 + 2)
      !seven = add (1 + 2) undefined -- (1 + 3)

  putStrLn $ "Five: " ++ show five
```

です。最初の例は問題なく実行することができます。これは `seven` が評価されることがないからですね。しかし、後者では、`seven` にバンパターンが付いています。ここで、GHC はこんなことをしています:

- `add (1 + 2) undefined` と言う式を評価する
- これは `(1 + 2) + undefined` に簡約化される
- しかしまだこれは値ではなく式なので、さらに評価が必要になる
- `+` という演算子を評価するために、ただのサンクではなく2つの引数の実際の値が必要になる。`+` の引数がバンパターンを持っている、という見方をしてもいい。正しい言い方をすると、「`+` は2つの引数のどちらに対しても正格」
- GHC は `1 + 2` と `undefined` のどちらも最初に評価してもいい。ここでは `1 + 2` を最初に評価しているものとするが、2つの評価済みの値 (`1` と `2`) を `+` に渡し、`3` を得る。全て順調。
- しかし、`undefined` を評価しようとして、実行時例外が投げられる。

**問題** 上の質問に戻りますが: `add` 関数の内部にバンパターンを持たせたら、何か変わるでしょうか? このプログラムの出力が何になるか考えてみてください:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
{-# LANGUAGE BangPatterns #-}

add :: Int -> Int -> Int
add !x !y = x + y

main :: IO ()
main = do
  let five = add (1 + 1) (1 + 2)
      seven = add (1 + 2) undefined -- (1 + 3)

  putStrLn $ "Five: " ++ show five
```

この動作を正格な言語と比較するために、実行時例外のようなものを持つ言語が必要ですね。Rust のパニックを使うことにしましょう:

```rust
fn add(x: isize, y: isize) -> isize {
    println!("adding: {} and {}", x, y);
    x + y
}

fn main() {
    let five = add(1 + 1, 1 + 2);
    let seven = add(1 + 2, panic!());

    println!("Five: {}", five);
}
```

まず Rust をフォローするために書いておきましょう。Rust は、このプログラムがどんなにバカげたことをしているのか、多くの警告を出してくれます。確かにそれはそうなんですが、これらの警告は無視して突っ走りましょう。このプログラムは、まず最初に `add(1 + 1, 1 + 2)` という式を評価します (`adding: 2 and 3` という出力で確認できます)。そして、2回目の add 関数に入る前に、`1 + 2` と　`panic!()` のどちらも評価する必要があります。前者はいいですが、後者ではパニックが発生し、そこでショートします。

Haskell の遅延性を獲得したいのなら、簡単な方法があります。クロージャを使いましょう。クロージャは本質的にはサンクです。Rust の構文でクロージャを書くと、`|args| body` のようになります。引数なしのクロージャを作ると、サンクのような振る舞いをします。こんな感じです:

```rust
fn add<X, Y>(x: X, y: Y) -> isize
    where X: FnOnce() -> isize,
          Y: FnOnce() -> isize {
    let x = x();
    let y = y();
    println!("adding: {} and {}", x, y);
    x + y
}

fn main() {
    let five = || add(|| 1 + 1, || 1 + 2);
    let seven = || add(|| 1 + 2, || panic!());

    println!("Five: {}", five());
}
```

繰り返しますが、Rust のコンパイラは使われていない `seven` について文句を言ってきます。が、`seven` のクロージャを使うことはないので、このプログラムを実行することはできます。

まだ Rust についてあまり知らない? それなら、みんな大好き Javascript を使ってみましょう:

```javascript
function add(x, y) {
    return x() + y();
}

function panic() {
    throw "Panic!"
}

var five = ignored => add(ignored => 1 + 1, ignored => 1 + 2);
var seven = ignored => add(ignored => 1 + 2, panic);
console.log("Five: " + five());
```

よし、今までの話をまとめてみます:

- Haskell はデフォルトで lazy
- バンパターンと `seq` を使うことで、正格にすることができる
- 一方、正格な言語ではクロージャを使うことで lazy にすることができる
- 関数が引数に対して正格かどうか、ボトム (`undefined`) を渡して、目の前で爆発するかどうかで確認することができる
- `trace` 関数を使っても同じことを確認できる

ここまでは全て順調ですね。先に進む前に、これらの概念を確実に理解しておいてください。上のセクションを読み直すのもいいかもしれません。

## 平均
まだ言及してないことがありました: 具体的に、「評価する」とか「値であることを強要する」というのはどういうことなのでしょう? この問題を考えるために、average 関数を実装してみましょう。`RunningTotal` というヘルパー型を使って、これまでに出てきた累積合計と要素数を取得することにします。

```haskell
data RunningTotal = RunningTotal
  { sum :: Int
  , count :: Int
  }

printAverage :: RunningTotal -> IO ()
printAverage (RunningTotal sum count)
  | count == 0 = error "Need at least one value!"
  | otherwise = print (fromIntegral sum / fromIntegral count :: Double)

-- | A fold would be nicer... we'll see that later
printListAverage :: [Int] -> IO ()
printListAverage =
  go (RunningTotal 0 0)
  where
    go rt [] = printAverage rt
    go (RunningTotal sum count) (x:xs) =
      let rt = RunningTotal (sum + x) (count + 1)
       in go rt xs

main :: IO ()
main = printListAverage [1..1000000]
```

実行時に統計を取って、メモリ使用を見てみます:

```bash
$ stack ghc average.hs && ./average +RTS -s
```

なんということでしょう。メモリ使用量がぶっ飛んだことになっています!

```bash
[1 of 1] Compiling Main             ( average.hs, average.o )
Linking average ...
500000.5
     258,654,528 bytes allocated in the heap
     339,889,944 bytes copied during GC
      95,096,512 bytes maximum residency (9 sample(s))
       1,148,312 bytes maximum slop
             164 MB total memory in use (0 MB lost due to fragmentation)
```

トータルで 258MB も確保していて、一度に 95MB も確保しています。ただの局所関数の再帰にしては、ばかみたいに多いですね。

### バン!
あなたは今、「`seq` とかバンパターンのようなものを使うべきじゃないの?」と考えていると思います。確かに、それは筋が通るやり方です。実際、1つバンパターンを加えて `go` の再帰に入る前に新しい `rt` の評価を強制することで、普通にこの問題は解けそうです。例えば、`{-# LANGUAGE BangPatterns #-}` をファイルの先頭に追加して、`go` をこんな風に定義したらどうでしょう:

```haskell
go !rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

しかし、こうしてもメモリ使用率は全く変わりません。なぜこんなことになってしまうのでしょう。これを理解するためには、weak head normal form (弱頭部正規形) というものを理解する必要があります。

### Weak Head Normal Form
まず最初に、このトピックに関して [Stack Overflow に素晴らしい回答があることを](https://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form/6889335#6889335)示しておきます。

私たちは値であることを強制し、式を評価することについて話し合ってきましたが、それが実際に何を意味しているのかは全く明らかにしませんでした。まず簡単な例から始めましょう。このプログラムの出力はどうなるでしょうか?

```haskell
main = putStrLn $ undefined `seq` "Hello World"
```

`undefined` をプリントするときにエラーが出る、と予測したかもしれませんが、正解です。`"Hello World"` を評価する前に `undefined` を評価しようとし、それは `putStrLn` が引数に対して正格だからです。これは正解でしょう。しかし、少し違う例を試してみましょう:

```haskell
main = putStrLn $ Just undefined `seq` "Hello World"
```

「評価する」という言葉を、「サンクがない状態の何かになるまで完全に評価する」という意味で取っている人は、今回も `undefined` についてエラーを吐く、と答えるでしょう。しかし実際は、例外を吐かずにうまく "Hello World" と表示してくれます。一体どうなっているんでしょう?

実は、`seq` を使って評価を強制するとき、*weak head normal form (WHNF)* のみ評価しているのです。ほとんどのデータ型において、これは 1つコンストラクタの層を引き剥がす、という意味になります。`Just undefined` の場合、`Just` というデータコンストラクタを引き剥がすだけで、その中の `undefined` に触れることはありません。(すぐ下でこれに対処する別々の方法をお見せします。)

標準データコンストラクタ*を扱う場合、`seq` の使用はパターンマッチングで一番外側のコンストラクタでマッチさせるようなものです。単層化させたいのなら、例えば、`seqMaybe :: Maybe a -> b -> b` という関数を実装して、上の `main` で使うことができます。やってみてください。答えは下にあります。

* 説明はお待ちください。後で `newtype` の話を読めば、この変なネーミングの意味を理解できるでしょう。

```haskell
seqMaybe :: Maybe a -> b -> b
seqMaybe Nothing b = b
seqMaybe (Just _) b = b

main :: IO ()
main = do
  putStrLn $ Just undefined `seqMaybe` "Hello World"
  putStrLn $ undefined `seqMaybe` "Goodbye!"
```

Let's up the ante again. What do you think this program will print?

もう一度掛け金を上げてみましょう。このプログラムは何を表示すると思いますか?

```haskell
main = do
  putStrLn $ error `seq` "Hello"
  putStrLn $ (\x -> undefined) `seq` "World"
  putStrLn $ error "foo" `seq` "Goodbye!"
```

``error `seq` ...`` が問題になると思うかもしれません。最終的に `error` が例外を吐くんじゃないの? ってね。しかし、`error` は関数です。`error` が `String` を引数に与えられるまで、例外が吐かれることも、ボトムの値が返されることもないのです。結果的に、実は、これを評価してもエラーを生成することはありません。ルールとしては、引数よりも少ない値に適用された関数は、自動的に WHNF になります。

同じようなロジックは、`(\x -> undefined)` にも適用できます。これはラムダ式ですが、型としては全ての引数に値が適用されていない関数です。したがって、これが評価されても例外を吐くことはありません。言い換えると、この式はすでに WHNF になっています。

しかし、`error "foo"` は引数が完全に適用された関数です。これはもう関数ではなく、値です。そして WHNF に評価しようとするときに、例外が爆発して顔面に飛んできます。

**演習** 次の式は、評価されたときに例外を吐くでしょうか?

- `(+) undefined`
- `Just undefined`
- `undefined 5`
- `(error "foo" :: Int -> Double)`

### 平均を直す
WHNF を理解したところで、例に戻って最初のバンパターンが何もしてくれなかったのか見てみましょう:

```haskell
go !rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

WHNF では、評価を強制することはコンストラクタを剥がすのと同じことです。これはさっきの節でもうやりましたね! 問題は、`RunningTotal` データコンストラクタの中に含まれる値が評価されていないこと、それが原因でサンクが蓄積されていることです。これを解決する方法は2つあります:

```haskell
go rt [] = printAverage rt
go (RunningTotal !sum !count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

`RunningTotal` の値にバンを置くのはやめて、コンストラクタの中の値に置いて、ループの度に評価させるようにしています。巨大なサンクの連鎖は積まれなくなり、maximum residency は 44kb にまで減少しています。(全体の使用量はまだ 192mb 辺りになっていますが。トータルの使用量をどうにかするためには、この記事のものとは別の最適化をする必要があります。なので、この値はこの記事の例では全て無視することにします。) もう1つのアプローチは:

```haskell
go rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let !sum' = sum + x
      !count' = count + 1
      rt = RunningTotal sum' count'
   in go rt xs
```

これは新しい `RunningTotal` を作る前に、新しい sum と count を強制的に評価しています。私はこのバージョンの方がちょっと好きです。というのも、次のループで値をくずすのではなく、正しい場所、つまり値を作るときに評価を強制しているからです。

この話のポイント: コンテナではなく、実際に評価する必要があるものを評価していることを確認すべし

#### 翻訳者追記
実行結果を比較してみました。

##### 1
```haskell
go rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

##### 2
```haskell
go !rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

##### 3
```haskell
go rt [] = printAverage rt
go (RunningTotal !sum !count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

##### 4
```haskell
go rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let !sum' = sum + x
      !count' = count + 1
      rt = RunningTotal sum' count'
   in go rt xs
```

##### 5
原文にはありませんが、`RunningTotal` の定義でバンパターンをつけています。

```haskell
data RunningTotal = RunningTotal
  { sum :: !Int
  , count :: !Int
  }

go rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

##### 6
これも原文にはありませんが、`StrictData` 拡張を使っています。

```haskell
{-# LANGUAGE StrictData #-}

go rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

##### 7
deepseq のセクションのものです

```haskell
data RunningTotal = RunningTotal
  { sum :: Int
  , count :: Int
  }
instance NFData RunningTotal where
  rnf (RunningTotal sum count) = sum `deepseq` count `deepseq` ()
```

|| 1 | 2 | 3 | 4 | 5 | 6 | 7 |
|---|---|---|---|---|---|---|---|
| allocated in the heap | 258,654,520 bytes | 258,654,520 bytes | 192,102,712 bytes | 216,102,712 bytes | 176,102,712 bytes | 176,102,712 bytes | 256,135,480 bytes |
| copied during GC | 339,889,944 bytes | 339,889,944 bytes | 173,080 bytes | 142,896 bytes | 164,400 bytes | 164,400 bytes | 168,640 bytes |
| maximum residency | 95,096,512 bytes | 95,096,512 bytes | 44,384 bytes | 44,384 bytes | 44,384 bytes | 44,384 bytes | 44,384 bytes |
| maximum slop | 1,148,312 byte | 1,148,312 bytes | 25,248 bytes | 25,248 bytes | 25,248 bytes | 25,248 bytes | 25,248 bytes |
| total memory in use | 164 MB | 164 MB | 1 MB | 1 MB | 1 MB | 1 MB | 1 MB

### deepseq
`seq` が weak head normal form にしか評価してくれないのはイライラしますよね。normal form (NF) にまで完全に評価したいような状況はいくらでもあります。つまり、値の中の全てのサンクを評価したいということですね。これをどうにかする言語にビルトインの方法はないですが、半分標準の (GHC についてくるということ) ライブラリはあります。`deepseq` です。`deepseq` は `rnf`メソッドを使って定義されます。`rnf`メソッドは `NFData`型クラスに存在し、値を normal form にする定義になっています。

`NFData` 型クラスで定義される `rnf`メソッドを使うことで、値をどうやって normal form にするのか定義した型クラスを提供することで、それを達成します (`rnf` メソッド経由で)。

```haskell
import Control.DeepSeq

data RunningTotal = RunningTotal
  { sum :: Int
  , count :: Int
  }
instance NFData RunningTotal where
  rnf (RunningTotal sum count) = sum `deepseq` count `deepseq` ()

printAverage :: RunningTotal -> IO ()
printAverage (RunningTotal sum count)
  | count == 0 = error "Need at least one value!"
  | otherwise = print (fromIntegral sum / fromIntegral count :: Double)

-- | A fold would be nicer... we'll see that later
printListAverage :: [Int] -> IO ()
printListAverage =
  go (RunningTotal 0 0)
  where
    go rt [] = printAverage rt
    go (RunningTotal sum count) (x:xs) =
      let rt = RunningTotal (sum + x) (count + 1)
       in rt `deepseq` go rt xs

main :: IO ()
main = printListAverage [1..1000000]
```

もう一度言いますが、これの maximum residency は 44kb です。ここでは `NFData` のインスタンスを定義して、`rnf` メソッドも含んでいます。単純にデータコンストラクタ中の全ての値を `deepseq` するのは、`NFData` インスタンスを定義するときにほとんどいつも使う方法です。これは常套手段なので、実は `Generic` 導出を使うだけで GHC に同じ仕事をさせることができます:

```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic)
import Control.DeepSeq

data RunningTotal = RunningTotal
  { sum :: Int
  , count :: Int
  }
  deriving Generic
instance NFData RunningTotal
```

`NFData` のインスタンスにすることの一番の魅力は、多くのデータ型に対する抽象化の能力です。(ここでやっているように) スペースリークを避けるだけではなく、値の中のサンクに例外が誤って含まれてしまうのを防ぐことができます。例として、[safe-exceptions library](https://haskell-lang.org/library/safe-exceptions) の [tryAnyDeep](https://www.stackage.org/haddock/lts-9.3/safe-exceptions-0.1.6.0/Control-Exception-Safe.html#v:tryAnyDeep) 関数を見てみてください。

**演習** `rnf` と `seq` に関して、 `deepseq` を自分で定義してみてください。

### 正格なデータ
これらのアプローチはうまくいきましたが、最適解ではありません。問題は、`RunningTotal` の定義に存在します。ここで私たちが考えているのは、`RunningTotal` 型の値があるとき、実は2つの `Int` が存在しているということです。しかし遅延性のせいで、`RunningTotal` の値は2つの `Int` か `Int` に評価することができるサンク、もしくは例外を投げるサンクを持つことができる、といった意味合いになってしまいます。

そうではなく、`RunningTotal` の値に遅延性が入りこむ余地を無くしたいものですね。定義のデータ型で正格性注釈をつけることでそれを実現できます:

```haskell
data RunningTotal = RunningTotal
  { sum :: !Int
  , count :: !Int
  }
  deriving Generic

printAverage :: RunningTotal -> IO ()
printAverage (RunningTotal sum count)
  | count == 0 = error "Need at least one value!"
  | otherwise = print (fromIntegral sum / fromIntegral count :: Double)

-- | A fold would be nicer... we'll see that later
printListAverage :: [Int] -> IO ()
printListAverage =
  go (RunningTotal 0 0)
  where
    go rt [] = printAverage rt
    go (RunningTotal sum count) (x:xs) =
      let rt = RunningTotal (sum + x) (count + 1)
       in go rt xs

main :: IO ()
main = printListAverage [1..1000000]
```

`RunningTotal` の中で `Int` の前にバンを置いただけです。他に正格性や評価を指定するようなものはありません。しかし、これらのフィールドに正格性注釈を置くことで、簡単ですが重要なことを言うことができます:

**`RunningTotal` 型の値を評価するときは、その中に含まれる2つの `Int` も評価しなければならない**

上で言及したように、2つ目の `go` は コンストラクタを剥ぎ取ることで、`RunningTotal` の値の評価を強制しています。今、この動きは自動的に `sum` と `count` の評価を強制します。前回はバンパターンを使う必要がありましたね。

これ以外のアドバンテージも1つあります。少し話からは脱線しますが、それでも言及しておく価値はあります。 `Int` のような小さな値を扱うとき、GHC は自動的に正格なフィールドをアンボックス化します。これは、`RunningTotal` の中で `Int` へのポインタを持ち続けるよりも、`Int` そのものを持つようになるという意味です。こうすることで、もっとメモリ使用量を減らすことができます。

こういうすごく良い質問をしてくれるかもしれません: 「データのフィールドで正格性注釈を使うかべきどうか、どうやったらわかるの?」この回答は少し議論の余地があるかもしれませんが、私のアドバイスとしては、フィールドに対して遅延性を持たせたいとき以外は、正格にすることです。フィールドを正格にすることで、以下のようなメリットが得られます:

- ここで私たちがやっているように、うっかりスペースリークを起こしてしまうのを避ける
- うっかりボトムの値を含んでしまうのを避ける
- レコード構文で値を生成するとき、正格なフィールドを忘れた時に GHC がエラーを出してくれる。正格ではないフィールドに対しては警告しか出してくれない。

### newtype の興味深いケース
3つ、よく似たデータ型を定義して見ましょう:

```haskell
data Foo = Foo Int
data Bar = Bar !Int
newtype Baz = Baz Int
```

Let's play a game, and guess the output of the following potential bodies for `main`. Try to work through each case in your head before reading the explanation below.

ゲームをしましょう。以下のコードを `main` 関数に置いたときの出力を推測してみてください。下にある説明を読む前に、それぞれのケースを頭の中で考えてくださいね。

1. `case undefined of { Foo _ -> putStrLn "Still alive!" }`
2. `case Foo undefined of { Foo _ -> putStrLn "Still alive!" }`
3. `case undefined of { Bar _ -> putStrLn "Still alive!" }`
4. `case Bar undefined of { Bar _ -> putStrLn "Still alive!" }`
5. `case undefined of { Baz _ -> putStrLn "Still alive!" }`
6. `case Baz undefined of { Baz _ -> putStrLn "Still alive!" }`

ケース (1) は比較的単純ですね。データコンストラクタを1層剥いで (`Foo`)、ボトムの値を見つけます。なので、これは例外を投げます。これは (3) にも当てはまります。

(2) は例外を投げません。`Foo` データコンストラクタがあって、それはボトムの値を含んでいます。しかし、`Foo` の中の `Int` に正格性注釈がないので、`Foo` を剥いでも `Int` を強制的に評価することはなく、例外が投げられることはありません。これとは対照的に、(4) では正格性注釈があるので、`Bar` のケースに通したら例外が投げられます。

`newtype` はどうでしょう? `newtype` について知っていることといえば、実行時に表現されることはないということでしょうか。ということは、`Baz` データコンストラクタがボトムの余分な層を隠すことは不可能です。言い換えれば、`Baz undefined` と `undefined` を区別することはできません。こう考えると、ぱっと見 `Bar` のようになりそうですが、おもしろいことに、そうではないんです。

`Baz` コンストラクタが実行時の動きになんら影響がないことはわかりますよね? そもそもそこには存在していないんだから。よって、(5) の中のパターンマッチは何の意味もありません。これは `case undefined of { _ -> putStrLn "Still alive!" }` と等しくなります。そして `undefined` について調べることはないので (データコンストラクタではなく、ワイルドカードパターンを使っているので)、例外が投げられることはありません。

同じように、ケース (6) でも `Baz` コンストラクタを `undefined` に適用していますが、実行時に表現されることはないので、これもまた存在しません。なので、ここでも例外が投げられることはありません。

**演習** ```main = Baz undefined `seq` putStrLn "Still alive!"``` の出力はどうなるでしょうか? そうなるのはなぜでしょう?

### 便利な演算子と関数
すでにお気づきかもしれませんが、`seq` と `deepseq` をあらゆるところで使うのは不都合なことがあります。バンパターンも助けにはなりますが、評価を強制する方法は他にもあります。おそらく、最も普通に使われているのは `$!` 演算子でしょう。例えば、以下のような感じです:

```haskell
mysum :: [Int] -> Int
mysum list0 =
  go list0 0
  where
    go [] total = total
    go (x:xs) total = go xs $! total + x

main = print $ mysum [1..1000000]
```

上の例では、`go` 関数の再帰に入る前に `total + x` の評価を強制しています。結果、スペースリークを防ぐことができます。(演習: 同じことを、バンパターンと `seq` 関数を使ってやってみてください。)

`$!!` 演算子も、`seq` ではなく `deepseq` を使っていること以外は同じです。そのため、この演算子を使うと normal form に評価されます。

```haskell
import Control.DeepSeq

average :: [Int] -> Double
average list0 =
  go list0 (0, 0)
  where
    go [] (total, count) = fromIntegral total / count
    go (x:xs) (total, count) = go xs $!! (total + x, count + 1)

main = print $ average [1..1000000]
```

他にも、いい感じのヘルパー関数に `force` というものがあります。これは、対象の式が WHNF に評価されるときに、実際には NF への評価を強制しています。例えば、上の `go` 関数は以下のように書き換えることができます:

```haskell
go [] (total, count) = fromIntegral total / count
go (x:xs) (total, count) = go xs $! force (total + x, count + 1)
```

**演習** これらの便利な関数と演算子を、`seq` と `deepseq` を使って自分で定義してみましょう。

## データ構造
はい、以上が一番複雑な部分でした。もしもそれら全てを理解していたら、残りは自然に理解できて、より深く理解するための用語を少し導入するだけになります。

ゆっくり始めましょう: このプログラムの出力はどうなるでしょうか:


```haskell
data List a = Cons a (List a) | Nil

main = Cons undefined undefined `seq` putStrLn "Hello World"
```

えー、上で紹介した法則を使うと、一番外側のコンストラクタがあるので、`Cons undefined undefined` はすでに WHNF になっています。なので、このプログラムは例外を吐くことなく "Hello World" と表示します。いいですね。さて、`Cons` は `:` データコンストラクタと等しいことを意識してください。そうすると、上の例は以下のプログラムと同一であることがわかります:

```haskell
main = (undefined:undefined) `seq` putStrLn "Hello World"
```

ということは、リストはレイジーなデータ構造だということですね。最初の要素はボトムで、残りのリストもボトムです。しかし全体としてはボトムではありません。少し違う例を試してみましょう:


```haskell
data List a = Cons a !(List a) | Nil

main = Cons undefined undefined `seq` putStrLn "Hello World"
```

これは顔面で爆発します! リストの tail が正格だからです。しかし、以下の例は大丈夫です:

```haskell
data List a = Cons a !(List a) | Nil

main = Cons undefined (Cons undefined Nil) `seq` putStrLn "Hello World"
```

このリストの定義では、リストそのものについては詳細の全てを知る必要があります。しかし、値は undefined のままでもいいのです。これは spine strict と呼ばれています。対照的に、値に対して正格にすることも可能です。やってみましょう:

```haskell
data List a = Cons !a !(List a) | Nil

main = Cons undefined (Cons undefined Nil) `seq` putStrLn "Hello World"
```

これは期待通り、顔面で爆発するでしょう。

お分かりかもしれませんが、もう1つリストの定義が残っています。値に対して正格で、残りはそうではないものです:

```haskell
data List a = Cons !a (List a) | Nil
```

実際のところ、このパターンを取る Haskell のデータ構造は知りません。よって名前もありません。(もしもこんなデータ構造があって名前があるのなら、教えてください。どんな使われ方をしているのか気になります。)

なので、標準のリストはレイジーです。他にもいくつかデータ型を見てみましょう:

### ベクター
`Data.Vector` の中のベクター (ボックスベクターとも) は、spine strict です。`import qualified Data.Vector as V` でインポートしたとして、以下のプログラムの結果はどうなるでしょうか?

1. ``main = V.fromList [undefined] `seq` putStrLn "Hello World"``
2. ``main = V.fromList (undefined:undefined) `seq` putStrLn "Hello World"``
3. ``main = V.fromList undefined `seq` putStrLn "Hello World"``

最初は成功します。ベクターの背骨部分は定義してあるからです。ボトムを含んでいるかどうかは無関係です。2番目は失敗します。背骨であるリスト後部が undefined なので、背骨部分は undefined になります。最後の例も (当然) 失敗します。リスト全体が undefined だからです。

さて、アンボックスベクターについても見てみましょう。推論の都合上、GHC を少し手助けしてやる必要があります。なので、この部分から始めましょう:

```haskell
import qualified Data.Vector.Unboxed as V

fromList :: [Int] -> V.Vector Int
fromList = V.fromList
```

この場合、さっきの例はどうなるでしょうか?

1. ``main = fromList [undefined] `seq` putStrLn "Hello World"``
2. ``main = fromList (undefined:undefined) `seq` putStrLn "Hello World"``
3. ``main = fromList undefined `seq` putStrLn "Hello World"``

ご想像の通り、(2) と (3) はボックスベクターのときと同じ動きをします。しかし、(1) も例外を投げるようになります。これは、アンボックスベクターが spine strict なだけではなく、値に対しても正格だからです。storable vector と primitive vector も同じ振る舞いをします。

残念ながら、私の知る限り、公開されているライブラリに正格なボックスベクターは存在しません。そのようなデータ型はスペースリーク対策に役立つと思うんですが (この記事を書くきっかけになった質問のように)。

### Set と Map
containers と unordered-containers パッケージを見てみれば、`Strict` と `Lazy` バリアントの中に、Set なんとかのようなモジュールは存在しないのに (`Data.IntSet` など)、Map なんとかは存在することに気がつくと思います (例えば、`Data.HashMap.Strict` と `Data.HashMap.Lazy`)。これは、これら全てのコンテナが spine strict で、キーに対して正格でなければならないからです。set は別に分けられた値を持たず、キーだけ持っているので、値に対して正格でなければいけません。

対照的に、map はキーと値の両方を持っています。map なんとかモジュールのレイジーなバリアントは spine strict で、値についてレイジーです。対して、正格なバリアントは spine と値の両方について正格です。

**演習** `Data.Sequence.Seq` データ型を調べて、lazy、spine strict、value strict のいずれかに分類してください。

## 関数の引数
ある関数がある引数に渡されたボトムの値に適用されるとき、結果がボトムになるのなら、その関数は引数の1つに対して正格だと考えられます。上の例で見たように、`Int` に対して `+` を適用するときはどちらの引数に対しても正格ですが、`undefined + x` と `x + undefined` はボトムです。

対して、`const` 関数は `const a b = a` と定義されますが、これは最初の引数に対して正格で、2番目の引数に対してはレイジーです。

リストの `:` データコンストラクタは、第1引数と第2引数に対してレイジーです。しかし、`data List = Cons !a !(List a) | Nil` という定義では、`Cons` はどちらの引数に対しても正格になります。

## Fold
遅延性を扱う上でつまづきやすいポイントは、fold です。もっとも悪名高い例は `foldl` 関数でしょう。こいつは偽りの安心感をいざない、夢と希望をぶち壊してくれます:

```haskell
mysum :: [Int] -> Int
mysum = foldl (+) 0

main :: IO ()
main = print $ mysum [1..1000000]
```

これは限りなく正解に近いですが、53mb も resident memory を使っています! 答えはチョンっとつけて正格な左畳み込み `foldl` 関数を使うことです。

```haskell
import Data.List (foldl')

mysum :: [Int] -> Int
mysum = foldl' (+) 0

main :: IO ()
main = print $ mysum [1..1000000]
```

`Prelude` はなんでこんな常に使うのが間違っているような関数 (`foldl`) を提供しているのでしょうか?

![Hysterical Raisins](https://www.fpcomplete.com/static/hysterical-raisins.jpg)

ただ、ほとんど全ての正格であると称している関数は、実際のところ weak head normal form に対してのみ正格であることに留意しなければいけません。前の `average` 関数の例を見てみると、まだスペースリークがあります:

```haskell
import Data.List (foldl')

average :: [Int] -> Double
average =
  divide . foldl' add (0, 0)
  where
    divide (total, count) = fromIntegral total / count
    add (total, count) x = (total + x, count + 1)

main :: IO ()
main = print $ average [1..1000000]
```

私のアドバイスは、正格なフィールドを持つヘルパーデータ型を使うことです。しかしあなたはそうしたくないかもしれませんし、normal form に評価するような `foldl'` がないことにイライラしているかもしれません。そんなあなたに朗報です。`force` を使うだけで、簡単に WHNF fold を NF fold にアップグレードすることができます:

```haskell
import Data.List (foldl')
import Control.DeepSeq (force)

average :: [Int] -> Double
average =
  divide . foldl' add (0, 0)
  where
    divide (total, count) = fromIntegral total / count
    add (total, count) x = force (total + x, count + 1)

main :: IO ()
main = print $ average [1..1000000]
```

腕のいい水道工事業者のように、`force` はすぐにリークを止めてくれます!

### ストリーミングデータ
conduit のようなストリーミングデータライブラリの主張の1つに、メモリ使用が定数のオーダーになる、というものがあります。この主張を聞くと、スペースリークについて心配することなく、こいつとおさらばできるという印象を受けます。が、WHNF vs NF 問題はここでも当てはまります。私の言い分を証明するために、conduit で average をひどいやり方で計算してみましょう:

```haskell
import Conduit

average :: Monad m => ConduitM Int o m Double
average =
  divide <$> foldlC add (0, 0)
  where
    divide (total, count) = fromIntegral total / count
    add (total, count) x = (total + x, count + 1)

main :: IO ()
main = print $ runConduitPure $ enumFromToC 1 1000000 .| average
```

こうやってメモリ使用率を確かめることができます:

```haskell
$ stack --resolver lts-9.3 ghc --package conduit-combinators -- Main.hs -O2
$ ./Main +RTS -s
```

**演習** 以下のものを使って、このプログラムを定数メモリ使用率で実行してください:

1. `force` 関数
2. バンパターン
3. 正格なフィールドを持つカスタムデータ型


## 連鎖反応
この超正格なプログラムをご覧ください。値に正格な特別なデータ型を持っています。私はふんだんにバンパターンを散りばめ、至るところで `seq` を呼び出しています。`$!` を使いました。メモリ使用率はどうなるでしょうか?

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
{-# LANGUAGE BangPatterns #-}

data StrictList a = Cons !a !(StrictList a) | Nil

strictMap :: (a -> b) -> StrictList a -> StrictList b
strictMap _ Nil = Nil
strictMap f (Cons a list) =
  let !b = f a
      !list' = strictMap f list
   in b `seq` list' `seq` Cons b list'

strictEnum :: Int -> Int -> StrictList Int
strictEnum low high =
  go low
  where
    go !x
      | x == high = Cons x Nil
      | otherwise = Cons x (go $! x + 1)

double :: Int -> Int
double !x = x * 2

evens :: StrictList Int
evens = strictMap double $! strictEnum 1 1000000

main :: IO ()
main = do
  let string = "Hello World"
      string' = evens `seq` string
  putStrLn string
```

よーく見て、コードをよく読んで、予想して見てください。準備はいいですか? いきましょう。

メモリ利用率は 44kb です。「なんで!?」と叫びたくなるかもしれません。「100万回正格なリンクトリストを回しているじゃないか!」ってね。惜しい。このプログラムは `evens` の値を評価を強制した直後に、死ぬほど評価を繰り返すでしょう。これは正しい。そして、`main` の中の `string'` という値の評価を強制した直後に `evens` は評価されます。

しかし、このプログラムはどちらの評価も強制することはありません! 注意深く見てみれば、プログラムの最後の行は `string` という値を使っています。`string` も `evens` も使うことはないんですね。プログラムを実行するとき、GHC は `main` 関数によって指定された `IO` アクションを実行することにのみ関心を持ちます。そして、その `main` は `putStrLn string` ということしか言っていないわけです。

このことの理解は極めて重要です。プログラム中で `seq` や `deepseq` を使い、好きなだけ評価の連鎖を組み立てることができます。しかし結局は、`IO` アクション経由で連鎖の最初に値を評価してやらないと、評価されないサンクのままなのです。

**演習**

1. `putStrLn string` を `putStrLn string'` にして、メモリ使用率がどうなるか観察してください (終わったら戻してください)
2. `main` のどこかにバンパターンを置くと、メモリ利用率がはね上がります。それはどこでしょう?
3. `putStrLn string` の行のどこかに `seq` を置くと、メモリ利用率が大きくなります。それはどこでしょう?

## もっと先に
Sebastian Graf は[このブログ記事を分析する](http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html)というタイトルで、素晴らしいブログ記事を書いています。このブログ記事は、正格性のケース毎に GHC がどのように解析、最適化をしているのかというところまでもっと踏み込んだ解説をしています。作者である彼の言葉を引用します。

「このブログ記事では、スペースリークを防ぐための、コンパイラと連携できるようなより局所的なアプローチを解説したいと思います」

もしも興味が湧いたのなら、一読してみることをおすすめします。
