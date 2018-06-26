---
title: 正格性のすべて (翻訳)
author: Michael Snoyman
translator: pythonissam, Shinya Yamaguchi
tags: fpcomplete, 翻訳
---

Original post: [ALL ABOUT STRICTNESS](https://www.fpcomplete.com/blog/2017/09/all-about-strictness)

Haskell は (もしかすると、評判のよろしくない？) 遅延 (*lazy*) 言語です。遅延性の基本的なアイデアは「値は必要となったときにのみ計算される」という、たった一言で説明できるぐらい簡単なものです。しかし、この裏には様々なことが隠れています。特に、メモリと時間について効率的なコードを書こうとしたときに、必要不可欠なトピックがいくつもあります。

- 弱頭部正規形 (WHNF) と 正規形 (NF)
- `seq` と `deepseq` の使い方 (と関連する概念)
- データ型の正格性注釈
- バンパターン
- 遅延、スパイン(spine)-正格、値-正格などのデータ構造の正格性
- 適切な補助関数の選択 (特に、fold 系)

この記事は効率的な [conduit](https://haskell-lang.org/library/conduit) コードを書くためのいくつかの質問にインスパイアされたものであり、記事の最後でそれらについて本気で取り組んでみようと思います。ここで紹介する概念は汎用的なものであり、ストリーミングライブラリに限定されるものではありません。

**ノート**
この記事は現実とは逆に、遅延性を解決すべき問題として取り扱います。遅延性は有利にも不利にもなります。私たちの目標は遅延性の問題の大枠とその回避策を理解することなので、悪い点にのみ焦点を当てることにします。遅延性にはとても大きなメリットが数多くありますが、ここで取り上げることはしません。なぜなら、私の読者はコメントで遅延性の素晴らしさについて紹介している記事へのリンクをいくつも追加してくれるでしょうから :)

<!--more-->

## 遅延性入門

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

私たちの関数 `add` は `x` と `y` の両方の引数で*正格*です。そして、結果もまた正格になります。つまり

- 初めて `add` が呼ばれる前に `1 + 1` と `1 + 2` の両方の結果を計算します。
- `2` と `3` を引数として `add` 関数を呼び出した結果 `5` が得られます。それは変数 `five` によって指し示されるメモリの値となります。
- 同様に `1 + 2`, `1 + 3` を計算し `seven` に `7` が格納されます。
- 完全に計算された `five` の値で `printf` を呼び出します。

さて、これと等価な Haskell コードと比較してみましょう。

```haskell
add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
  let five = add (1 + 1) (1 + 2)
      seven = add (1 + 2) (1 + 3)

  putStrLn $ "Five: " ++ show five
```

正格性解析 (*strictness analysis*) と呼ばれる仕組みにより、説明よりも効率的な結果となることがありますが、意味的には以下の通りです。

- `1 + 1` や `1 + 2` の計算をすぐに行うのではなく、コンパイラはこれらの計算のサンク (*thunk*) (プロミス (*promise*) として考えることができます)  を生成し、`add` 関数にサンクを渡します。
- `add` 関数をすぐに呼び出すという例外を除けば、`five` は `add` 関数を `1 + 1` と `1 + 2` のサンクに適用するというサンクです。
- `seven` に関しても同様に、`add` 関数を異なる2つのサンクに適用するというサンクです。
- 最終的に `five` を表示しようとする際に実際の数を知る必要があります。このことを強制評価 (*forcing evaluation*) と言い、あとで詳しく、いつ・どのように強制評価が起こるか説明しますが、今のところは `putStrLn` が実行された時に起こると理解すれば十分です。`1 + 1` と `1 + 2` の強制評価を行う `five` の強制評価が行われ、サンクが実際の数 (`2`, `3`, 最終的に `5`) に変換されます。
- `seven` は一度も使われず、サンクとして残ったままとなりますが、このサンクを評価するための時間はかかっていません。

C の (正格) 評価と比較すると、使われることのない `seven` の値を評価するという無意味な処理を行わないという恩恵があります。これにより、処理を3つスキップできます！現実的な場面では3つではなく、もっとひどいコストのかかる処理かもしれません。

*だけども*、全てが素晴らしいものではありません。サンクはタダじゃないんです。我々はサンクのためにスペースを確保する必要があり、その確保と後にメモリ解放のために行われる GC を引き起こすコストの両方がかかります。たぶん一番大切なことは、式がサンク化されたものは、評価されたものよりもずっとコストがかかる可能性があるということです。(問題がより複雑になって) 混乱してしまわないように、データコストラクタのオーバーヘッドは一旦無視して、`five` の2つの表現方法を比較してみましょう。C において `five` は正確に1つのマシンワード\*を消費します。それに対して Haskell の `five` サンクはだいたい以下のようになります。

\* またはそれよりも少ないです。`int` は32ビットですが、たぶんあなたは64ビットのマシンを使っているでしょう。しかし、整列問題によりレジスタを1つのマシンワードと言っても良いでしょう。

- 1つのマシンワードが "私はサンクです" と主張します
- サンクの中は `add` 関数と `1 + 1` と `1 + 2` のサンク (それぞれ1つのマシンワード) へのポインタとなっています。そのため合計で3つのマシンワードです。
- `1 + 1` のサンクはサンクのための1つのマシンワードと `+` 演算子と `1` の値へのポインタです。(GHC は int 自身の余分なオーバーヘッドを避けるためにメモリの専用部分に小さな int の値を保持する最適化を行いますが、理論的にはそれぞれの余分なマシンワードが追加されます)。ここでもまた、少なくとも3つのマシンワードが必要となります。
- 同じことが `1 + 2` のサンクにも言えるので、3つのマシンワードとなります。
- 最終的な合計は**10マシンワード**となり、C のメモリ使用量と比較して10倍の差があります！

実際のところ、こんなに上手くはいきません。なぜなら、正格性解析によって "やぁ、ちょっと待って、これは後で使うからサンクを確保するよりも2つの数字を加算する方が絶対良いって、じゃあまたね！" という感じで最適化が行われるためです。しかし、遅延性やサンクが発生する可能性のある全ての場所を理解しておくことは、Haskell を書く上で非常に重要なことです。

### バン!

さて、どうすれば Haskell の評価をより正格にできるのでしょうか。一番簡単なのはバンパターンを使う方法です。まずはコードを確認してみましょう。

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

このコードは正格な C のコードと全く同じように振る舞います。先ほどのコードとの違いは `add` 関数の `x` と `y` の前にバン (`!`) があることです。これによって GHC は `add` を評価する前に `x` と `y` の値を評価しなければならないと判断します。同様に `five` と `seven` の前にも `!` があるため、 GHC は `putStrLn` を評価する前にこれらの値を評価します。

Haskell には多くのものがありますが、バンパターンはただの `seq` 関数を使ったシンタックスシュガーです。`seq` 関数の型は以下の通りです。

```haskell
seq :: a -> b -> b
```

この型シグネチャを見ると、`a` の値を無視するような以下の実装にしたくなるでしょう。

```haskell
badseq :: a -> b -> b
badseq a b = b
```

けれども、 `seq` は GHC が提供しているプリミティブな操作を使って`b` が評価されるときには、既に `a` が評価されていることを保証します。ここで先ほどの `add` 関数をバンパターンの代わりに `seq` を使って書き換えてみましょう。

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

このコードは以下のような意味になります。

- `part1` は `x` を評価した後に `part2` の値を評価します
- `part2` は `y` を評価した後に `answer` の値を評価します
- `answer` はそのまま `x + y` です

もちろん、`let` と `in` を使ってこんなに長いコードを書くのは大変なので、多くのプログラマは最後の行のように `seq` を中置記法で使います。

**演習** `in part1` の代わりに `in part2` とした場合どうなるでしょうか？また `in answer` ではどうでしょうか？

バンパターンから `let` を使う方法への変換はどんな場合でも可能です。先ほどの `main` 関数は次のように書き換えても同じことです。

```haskell
main :: IO ()
main = do
  let five = add (1 + 1) (1 + 2)
      seven = add (1 + 2) (1 + 3)

  five `seq` seven `seq` putStrLn ("Five: " ++ show five)
```

上記のプログラムはバンパターンを使ったものと等しく、`seq` の動作を理解するために非常に重要な例です。ですが、バンパターンのコードよりも少し読みづらくなってしまったと感じる読者もいると思います。なので、自分が読みやすい好きな方を使ってください。たぶん、ほとんどの人はバンパターンを使うでしょうけども。

### 評価を追ってみよう

今まで、サンクの評価について私の説明が全てでした。これから、評価について、もっと直接的に観測するための方法について説明します。`Debug.Trace` モジュールで定義されている `trace` 関数は評価された時にメッセージを表示します。以下の2つのプログラムの出力を予想してみてください。

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

- 最初のプログラムは `five` と `Five: 5` の両方を表示します。`seven` は式が評価されないため、表示されません。(出力のバッファリングによっては、これらの値の表示順序が入れ替わるという奇妙な現象が起きることがあります。)
- 2つ目のプログラムは `five` と `seven` の両方を表示します。なぜなら、バンパターンによってこれらの評価が強制されるからです。しかし、この表示順についてはあなたが期待するものと異なっていたのではないでしょうか。現に、私のシステムでは `five` が表示される前に `seven` が表示されました。なぜなら、この場合において GHC は評価順を並び替えることができるからです。
- 逆に ``five `seq` seven `seq` putStrLn ("Five: " ++ show five)`` としていたら、表示される順序は `five`, `seven`, `"Five: 5"` となっていたでしょう。これはバンパターンが単に `seq` に変換されるという先ほどの説明でほんの少しだけ嘘をついたからです。しかし、 ``x `seq` y`` という式は実際のところ、GHC からすれば `x` と `y` のどちらを先に評価したとしても、式の評価が終わった時に `x` と `y` の両方が評価されていることが保証されていれば良いのです。

とはいえ、あたなの式が本当に純粋であれば、`x` と `y` の評価がどちらから行われるかということは観測できなかったはずです。純粋では無い `trace` 関数を利用したからこそ、評価の順序を観測することができたんです。

**質問** もし、全ての `add` 関数に `!` をつけたら結果はどう変わるでしょうか？ なぜ `!` を付けるだけで出力に影響したり (しなかったり) するのでしょうか？

### ボトムの値

今までの例もちゃんと動くので良いのですが、評価の順番を確認する方法として、よりスタンダードな方法があります。それはボトム、すなわち `undefined` を使うことです。`undefined` は、評価されたときに実行時例外を投げる点で特別です。 (他の特別な関数や値のように、`error` 関数も同じ動きをします。) バンが無い場合は `seven` が評価されないことを確認するために、以下の2つのプログラムを比較してみましょう:

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

最初の例は問題なく実行することができます。これは `seven` が評価されないからですね。しかし、2つ目の例では、`seven` にバンパターンが付いています。ここで、GHC はこんなことをしています:

- `add (1 + 2) undefined` と言う式を評価します。
- この式は `(1 + 2) + undefined` に簡約されます。
- しかし、簡約した結果は値ではなく式なので、さらに評価が必要になります。
- `+` という演算子を評価するためには、2つの引数にサンクではなく実際の値が必要になります。このことは `+` の引数がバンパターンとなっているという見方をしても良いでしょう。より正確には、「`+` は2つの引数のどちらに対しても正格」と言います。
- GHC は `1 + 2` と `undefined` の評価順を自由に選択できます。ここでは `1 + 2` を最初に評価することにしましょう。そうすると、2つの評価済みの値 (`1` と `2`) を `+` に渡すので、`3` が返ってきます。全て順調ですね。
- しかし、次に `undefined` を評価しようとして、実行時例外が投げられます。

**質問** 上の質問に戻りますが: `add` 関数の内部にバンパターンを持たせたら、何か変わるでしょうか? 以下のプログラムの出力が何になるか考えてみてください:

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

この動作を正格な言語と比較するためには、実行時例外のようなものを持つ言語が必要ですね。Rust の panic を使うことにしましょう:

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

まず、Rust の名誉のために一言断っておくと、Rust は、このプログラムがどんなにバカげたことをしているのか、多くの警告を出してくれます。確かにそれはそうなんですが、これらの警告は無視して突っ走りましょう。このプログラムは、まず最初に `add(1 + 1, 1 + 2)` という式を評価します (`adding: 2 and 3` という出力で確認できます)。そして、2回目の `add` 関数に入る前に、`1 + 2` と `panic!()` のどちらも評価する必要があります。前者はいいですが、後者ではパニックが発生し、そこでショートします。

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

- Haskell の評価戦略はデフォルトで遅延評価です。
- バンパターンと `seq` を使うことで、正格評価にすることができます。
- 一方、正格な言語ではクロージャを使うことで遅延評価にすることができます。
- ボトム (`undefined`) を関数の引数に渡して、目の前で爆発すれば、関数の引数が正格だとわかります。
- `trace` 関数を使っても同じことを確認できます。

ここまでは全て順調ですね。先に進む前に、これらの概念を確実に理解しておいてください。前の章を読み直すのもいいかもしれません。

## 平均

まだ言及してないことがありました。「評価する」とか「値であることを強要する」というのは具体的にどういうことなのでしょう? この問題を考えるために、average 関数を実装してみましょう。`RunningTotal` というデータ型を使って、平均値の合計と要素数を取得することにします。

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

実行時に統計を取って、メモリ使用量を見てみます:

```shell
$ stack ghc average.hs && ./average +RTS -s
```

なんということでしょう。メモリ使用量がぶっ飛んだことになっています!

```shell
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

あなたは今、「`seq` とかバンパターンのようなものを使うべきじゃないの?」と考えていると思います。確かに、それも良いでしょう。実際、1つバンパターンを加えて `go` の再帰に入る前に新しい `rt` を強制評価すれば、この問題は簡単に解決できそうです。例えば、`{-# LANGUAGE BangPatterns #-}` をファイルの先頭に追加して、`go` をこんな風に定義したらどうでしょう:

```haskell
go !rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

しかし、こうしてもメモリ使用量は*全く*変わりません。なぜこんなことになってしまうのでしょう。これを理解するためには、weak head normal form (弱頭部正規形) というものを理解する必要があります。

### 弱頭部正規形

まず最初に、このトピックに関して [Stack Overflow に素晴らしい回答があることを](https://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form/6889335#6889335)示しておきます。

私たちは値であることを強制し、式を評価することについて話し合ってきましたが、それが実際に何を意味しているのかは全く明らかにしませんでした。まず簡単な例から始めましょう。このプログラムの出力はどうなるでしょうか?

```haskell
main = putStrLn $ undefined `seq` "Hello World"
```

文字列を表示しようとしたときに `undefined` が原因でエラーになるだろうと予測した方は正解です。このエラーは `putStrLn` が引数に対して正格であり、また `"Hello World"` を評価する前に `undefined` を評価しようとするために起こります。では、少し違う例を試してみましょう:

```haskell
main = putStrLn $ Just undefined `seq` "Hello World"
```

「評価する」という言葉を、「サンクがない状態の何かになるまで完全に評価する」という意味で取っている人は、今回も `undefined` についてエラーを吐く、と答えるでしょう。しかし実際は、例外を吐かずにうまく "Hello World" と表示してくれます。一体どうなっているんでしょう?

実は、`seq` で強制評価について話しているときは、*弱頭部正規形 (weak head normal form) (WHNF)* へ評価するという意味で話しています。ほとんどのデータ型において、これは 1つコンストラクタの層を引き剥がす、という意味になります。`Just undefined` の場合、`Just` というデータコンストラクタを引き剥がすだけで、その中の `undefined` に触れることはありません。(すぐ下でこれに対処する別々の方法をお見せします。)

標準データコンストラクタ*を扱う場合、`seq` を利用するということは一番外側のコンストラクタでパターンマッチさせるようなものです。単相化させたいのなら、例えば、`seqMaybe :: Maybe a -> b -> b` という関数を実装して、上の `main` で使うことができます。やってみてください。答えは下にあります。

- 説明はお待ちください。後で `newtype` の話を読めば、この変なネーミングの意味を理解できるでしょう。

```haskell
seqMaybe :: Maybe a -> b -> b
seqMaybe Nothing b = b
seqMaybe (Just _) b = b

main :: IO ()
main = do
  putStrLn $ Just undefined `seqMaybe` "Hello World"
  putStrLn $ undefined `seqMaybe` "Goodbye!"
```

では、続けましょう。このプログラムは何を表示すると思いますか?

```haskell
main = do
  putStrLn $ error `seq` "Hello"
  putStrLn $ (\x -> undefined) `seq` "World"
  putStrLn $ error "foo" `seq` "Goodbye!"
```

``error `seq` ...`` が問題になると思うかもしれません。最終的に `error` が例外を吐くんじゃないの? ってね。しかし、`error` は関数です。`error` が `String` の値を引数に与えられるまで、例外が吐かれることも、ボトムの値が返されることもないのです。よって、これを評価してもエラーを生成することはありません。ルールとしては、引数よりも少ない値に適用された関数は、自動的に弱頭部正規形になります。

同じようなロジックは、`(\x -> undefined)` にも適用できます。これはラムダ式ですが、型としては全ての引数に値が適用されていない関数です。したがって、このラムダ式が評価されても例外を吐くことはありません。言い換えると、この式はすでに弱頭部正規形になっています。

しかし、`error "foo"` は引数が完全に適用された関数です。これはもう関数ではなく、値です。そして弱頭部正規形に評価しようとするときに、例外が爆発して顔面に飛んできます。

**演習** 次の式は、評価されたときに例外を投げるでしょうか?

- `(+) undefined`
- `Just undefined`
- `undefined 5`
- `(error "foo" :: Int -> Double)`

### average 関数を修正する

弱頭部正規形を理解したところで、例に戻って最初のバンパターンが、なぜ何もしてくれなかったのか見てみましょう:

```haskell
go !rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

弱頭部正規形では、強制評価はコンストラクタを剥がすことと同じです。これはさっきの節でもうやりましたね! 問題は、`RunningTotal` データコンストラクタの中に含まれる値が評価されていないこと、それが原因でサンクが蓄積されていることです。これを解決する方法は2つあります。

```haskell
go rt [] = printAverage rt
go (RunningTotal !sum !count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

バンを `RunningTotal` に置くのはやめて、コンストラクタの*中*の値に置いて、ループの度に強制的に評価させるようにしています。巨大なサンクの連鎖は無くなり、最大メモリ常駐量は 44kb にまで減少しています。(全体としては、まだ約 192mb 使用しています。これをどうにかするためには、今回の例とは異なる別の最適化を行う必要があります。なので、この値はこの例では全て無視することにします。) もう1つのアプローチは:

```haskell
go rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let !sum' = sum + x
      !count' = count + 1
      rt = RunningTotal sum' count'
   in go rt xs
```

このアプローチでは新しい `RunningTotal` の値を作る*前*に、新しい sum と count を強制評価します。私はこのバージョンの方がちょっと好きです。というのも、次の繰り返しで値を分解した時ではなく、正しい場所、つまり値を構築するときに評価を強制しているからです。

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

##### 5 原文にはありませんが、`RunningTotal` の定義で正格性注釈をつけています。

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

##### 6 これも原文にはありませんが、`StrictData` 拡張を使っています。

```haskell
{-# LANGUAGE StrictData #-}

go rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

##### 7 deepseq のセクションのものです

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

`seq` が弱頭部正規形にしか評価してくれないのはイライラしますよね。*正規形* (NF) にまで完全に評価したいという状況はいくらでもあります。つまり、値の中の全てのサンクを評価したいということですね。言語レベルでこれを制御する方法はありませんが、半分標準の (GHC についてくるということ) ライブラリに `deepseq` 関数があります。`deepseq` は `NFData` 型クラスの `rnf` (**r**educe a value to **n**ormal **f**orm) メソッドを使って定義されています。`rnf` メソッドは値を正規形に簡約する方法を提供します。

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

もう一度言いますが、このプログラムの最大メモリ常駐量は 44kb です。ここでは `rnf` を含む　`NFData` 型クラスのインスタンスを定義します。単純にデータコンストラクタ中の全ての値を `deepseq` するという方法は、`NFData` インスタンスを定義するときに良く用いられます。これは常套手段なので、実は `Generic` 導出を使うだけで GHC は自動的にインスタンスを定義します。

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

`NFData` 型クラスのインスタンスにすることの一番の魅力は、多くのデータ型に対する抽象化の能力です。(ここでやっているように) スペースリークを避けるだけではなく、値の中のサンクに例外が誤って含まれてしまうというようなことも防げます。例として、[safe-exceptions library](https://haskell-lang.org/library/safe-exceptions) の [tryAnyDeep](https://www.stackage.org/haddock/lts-9.3/safe-exceptions-0.1.6.0/Control-Exception-Safe.html#v:tryAnyDeep) 関数を見てみてください。

**演習** `rnf` と `seq` を使って `deepseq` を自分で定義してみてください。

#### 翻訳者追記

演習の解答例

```haskell
deepseq :: NFData a => a -> b -> b
deepseq x y = rnf x `seq` y
```

### 正格なデータ

これらのアプローチはうまくいきましたが、最適解ではありません。問題は `RunningTotal` の定義に存在します。ここで私たちが考えているのは、`RunningTotal` 型の値があるとき、実は2つの `Int` が存在しているということです。しかし遅延性のせいで `RunningTotal` の値には、2つの `Int`、`Int` に評価することができるサンク、もしくは例外を投げるサンクを持つことができる、という値を含むことができてしまいます。

そのため、`RunningTotal` の値に遅延性が入りこむ余地を無くしたいものですね。これは、データ型の定義に*正格性注釈* (*strictness annotations*) をつけることで実現できます。

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

`RunningTotal` の定義で `Int` の前にバンを置いただけです。他に正格性や評価を指定するようなものはありません。しかし、これらのフィールドに正格性注釈を置くことで、簡単ですが重要なことを言うことができます:

**`RunningTotal` 型の値を評価するときは、必ずその中に含まれる2つの `Int` も評価しなければならない**

さきほど説明したように、2つ目の `go` は コンストラクタを剥ぎ取ることで、`RunningTotal` の値が強制評価されます。前回まではバンパターンを使う必要があった `sum` と `count` の強制評価が、ここでは自動化されています。

これ以外のアドバンテージも1つあります。少し話からは脱線しますが、それでも言及しておく価値はあります。 `Int` のような小さな値を扱うとき、GHC は自動的に正格なフィールドを*アンボックス化* (*unbox*) します。これは、`RunningTotal` の中で `Int` へのポインタを持ち続けるよりも、`Int` そのものを持つようになるという意味です。こうすることで、もっとメモリ使用量を減らすことができます。

こういうすごく良い質問をしてくれるかもしれません: 「自分のデータのフィールドで正格性注釈を使うかべきどうか、どうやったらわかるの?」この回答は少し議論の余地があるかもしれませんが、私のアドバイスとして、ベストプラクティスはフィールドに対して遅延性を持たせたいとき以外は、正格にすることです。フィールドを正格にすることで、以下のようなメリットが得られます:

- ここで私たちがやっているように、うっかりスペースリークを起こしてしまうのを避ける
- うっかりボトムの値を含んでしまうのを避ける
- レコード構文で値を生成するとき、正格なフィールドを忘れた時に GHC がエラーを出してくれる。正格ではないフィールドに対しては警告しか出してくれない。

### newtype の興味深いケース

よく似たデータ型を3つ定義してみましょう。

```haskell
data Foo = Foo Int
data Bar = Bar !Int
newtype Baz = Baz Int
```

ゲームをしましょう。以下のコードを `main` 関数に置いたときの出力を推測してみてください。以下の説明を読む前に、それぞれのケースを頭の中で考えてくださいね。

1. `case undefined of { Foo _ -> putStrLn "Still alive!" }`
2. `case Foo undefined of { Foo _ -> putStrLn "Still alive!" }`
3. `case undefined of { Bar _ -> putStrLn "Still alive!" }`
4. `case Bar undefined of { Bar _ -> putStrLn "Still alive!" }`
5. `case undefined of { Baz _ -> putStrLn "Still alive!" }`
6. `case Baz undefined of { Baz _ -> putStrLn "Still alive!" }`

ケース (1) は比較的単純ですね。データコンストラクタ (`Foo`) を1層剥いで、ボトムの値を見つけます。なので、これは例外を投げます。これは (3) にも当てはまります。

\(2) は例外を投げません。`Foo` データコンストラクタがあって、それはボトムの値を含んでいます。しかし、`Foo` の中の `Int` に正格性注釈がないので、`Foo` を剥いでも `Int` の強制評価は起こらず、例外が投げられることはありません。これとは対照的に、(4) では正格性注釈があるので、`Bar` のケースでは例外が投げられます。

`newtype` はどうでしょう? `newtype` について知っていることといえば、実行時表現が無いということでしょうか。ということは、`Baz` データコンストラクタがボトムの余分な層を隠すことは不可能です。つまり、`Baz undefined` と `undefined` を区別することはできません。こう考えると、ぱっと見 `Bar` のようになりそうですが、おもしろいことに、そうではないんです。

`Baz` コンストラクタを剥がすことが実行時の動作になんら影響がないことはわかりますよね? そもそもそこには存在していないんだから。よって、(5) の中のパターンマッチは何の意味もありません。これは `case undefined of { _ -> putStrLn "Still alive!" }` と等しくなります。そして `undefined` について調べることはない (データコンストラクタではなく、ワイルドカードパターンを使っている) ので、例外が投げられることはありません。

同様に、ケース (6) でも `Baz` コンストラクタを `undefined` に適用していますが、実行時表現はないので、これもまた存在しません。なので、ここでも例外が投げられることはありません。

**演習** ```main = Baz undefined `seq` putStrLn "Still alive!"``` の出力はどうなるでしょうか? そうなるのはなぜでしょう?

#### 翻訳者追記

演習の解答例: エラーを吐く。`seq` によって `Baz undefined` 、つまり `undefined` が評価されるため。

### 便利な演算子と関数

すでにお気づきかもしれませんが、`seq` と `deepseq` をあらゆるところで使うのは不都合なことがあります。バンパターンも助けにはなりますが、強制評価を行う方法は他にもあります。おそらく、最も良くに使われているのは `$!` 演算子でしょう。例えば、以下のように利用します。

```haskell
mysum :: [Int] -> Int
mysum list0 =
  go list0 0
  where
    go [] total = total
    go (x:xs) total = go xs $! total + x

main = print $ mysum [1..1000000]
```

上の例では、`go` 関数の再帰に入る前に `total + x` を強制評価しています。結果、スペースリークを防ぐことができます。(演習: 同じことを、バンパターンと `seq` 関数を使ってやってみてください。)

`$!!` 演算子も同様ですが、`seq` ではなく `deepseq` を利用しています。そのため、この演算子を使うと正規形に評価されます。

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

他にも、いい感じの補助関数に `force` というものがあります。これは、対象の式が弱頭部正規形に評価されるとき、実際には正規形に評価します。例えば、上記の `go` 関数はこのように書き換えることができます。

```haskell
go [] (total, count) = fromIntegral total / count
go (x:xs) (total, count) = go xs $! force (total + x, count + 1)
```

**演習** これらの便利な関数と演算子を、`seq` と `deepseq` を使って自分で定義してみましょう。

#### 翻訳者追記

演習の解答例

```haskell
force :: (NFData a) => a -> a
force x = x `deepseq` x
```

## データ構造

はい、以上が一番複雑な部分でした。もしもそれら全てを理解していたら、残りは自然に理解できて、より深く理解するための用語を少し導入するだけになります。

このプログラムの出力はどうなるでしょうか:

```haskell
data List a = Cons a (List a) | Nil

main = Cons undefined undefined `seq` putStrLn "Hello World"
```

えー、これまでに紹介した原理を使うと、一番外側のコンストラクタがあるので、`Cons undefined undefined` は既に弱頭部正規形になっています。なので、このプログラムは例外を吐くことなく "Hello World" と表示します。いいですね。さて、`Cons` は `:` データコンストラクタと等しいことを思い出してください。そうすると、上記の例はこうなります。

```haskell
main = (undefined:undefined) `seq` putStrLn "Hello World"
```

ということは、リストは遅延データ構造だということですね。最初の要素はボトムで、残りの要素もボトムです。しかし全体としてはボトムではありません。少し違う例を試してみましょう。

```haskell
data List a = Cons a !(List a) | Nil

main = Cons undefined undefined `seq` putStrLn "Hello World"
```

これは顔面で爆発します! 後続のリストが正格だからです。しかし、以下の例は大丈夫です。

```haskell
data List a = Cons a !(List a) | Nil

main = Cons undefined (Cons undefined Nil) `seq` putStrLn "Hello World"
```

このリストの定義では、リストそのものについては詳細の全てを知る必要があります。しかし、値は undefined のままでもいいのです。これは*スパイン正格* (*spine strict*) と呼ばれています。対照的に、値に対して正格な*値正格* (*value strict}*) にすることも可能です。やってみましょう。

```haskell
data List a = Cons !a !(List a) | Nil

main = Cons undefined (Cons undefined Nil) `seq` putStrLn "Hello World"
```

これは期待通り、顔面で爆発するでしょう。

お分かりかもしれませんが、もう1つリストの定義が残っています。値に対して正格で、残りはそうではないものです。

```haskell
data List a = Cons !a (List a) | Nil
```

実際のところ、Haskell このパターンのデータ構造を知りません。よって名前もありません。(もしもこんなデータ構造があって名前があるのなら、教えてください。どんな使われ方をしているのか気になります。)

なので、通常のリストは遅延リストです。他にもいくつかデータ型を見てみましょう。

### ベクター

`Data.Vector` の中のベクター (*ボックスベクター* (*boxed vectors*)とも) は、スパイン正格です。`import qualified Data.Vector as V` でインポートしたとして、以下のプログラムの結果はどうなるでしょうか?

1. ``main = V.fromList [undefined] `seq` putStrLn "Hello World"``
2. ``main = V.fromList (undefined:undefined) `seq` putStrLn "Hello World"``
3. ``main = V.fromList undefined `seq` putStrLn "Hello World"``

最初は成功します。ベクターは完全なスパインとして定義されるからです。そのため、ボトムを含んでいるかどうかは無関係です。2番目は失敗します。後続のリストのスパインが undefined だからです。最後の例も (当然) 失敗します。リスト全体が undefined だからです。

さて、*アンボックスベクター* (*unboxed vectors*) についても見てみましょう。推論の都合上、GHC を少し手助けしてやる必要があります。なので、プログラムの先頭に以下のような `fromList` が定義されているとしましょう。

```haskell
import qualified Data.Vector.Unboxed as V

fromList :: [Int] -> V.Vector Int
fromList = V.fromList
```

この場合はどうなるでしょうか？

1. ``main = fromList [undefined] `seq` putStrLn "Hello World"``
2. ``main = fromList (undefined:undefined) `seq` putStrLn "Hello World"``
3. ``main = fromList undefined `seq` putStrLn "Hello World"``

ご想像の通り、(2) と (3) はボックスベクターのときと同じ動きをします。しかし、(1) も例外を投げるようになります。これは、アンボックスベクターがスパイン正格なだけではなく、値に対しても正格だからです。storable vector と primitive vector も同じ振る舞いをします。

残念ながら、私の知る限り、公開されているライブラリに正格なボックスベクターは存在しません。そのようなデータ型はスペースリーク対策に役立つと思うんですが (この記事を書くきっかけになった質問のように)。

### Set と Map

containers, unordered-containers パッケージの、Map から始まるモジュールには `Strict` と `Lazy` (例えば、`Data.HashMap.Strict` と `Data.HashMap.Lazy`) が用意されているのに対して、Set から始まるモジュールには存在しないことに (`Data.IntSet` など) 気がつくと思います 。これは、これら全てのコンテナがスパイン正格で、キーに対して正格でなければならないからです。集合は分離された値を持たず、キーだけ持っているので、値に対して正格でなければいけません。

それに対してマップはキーと値の両方を持っています。Map.HashMap.Lazy のようなモジュールはスパイン正格ですが、値は遅延です。対して、Map.HashMap.Strict のようなモジュールはスパインと値の両方について正格です。

**演習** `Data.Sequence.Seq` データ型を調べて、遅延、スパイン正格、値正格のいずれかに分類してみましょう。

#### 翻訳者追記

演習の解答例:

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-11.2 --package containers

import Data.Sequence (fromList)

main = fromList [0,0,0,undefined,0,0] `seq` putStrLn "Alive!"
```

これを実行すると `Alive!` と表示されるのでスパイン正格。

## 関数の引数

関数の引数にボトムの値が適用されたとき、結果がボトムになれば、関数はボトムが与えられた引数に対して正格であると言います。上の例で見たように、`Int` に対して `+` を適用する場合、 `undefined + x` と `x + undefined` の結果は両方ともボトムとなるため、`+` はどちらの引数に対しても正格です。

それに対して `const a b = a` と定義される `const` 関数は、最初の引数に対して正格で、2番目の引数に対しては非正格です。

リストの `:` データコンストラクタは、第1引数と第2引数に対して、どちらも非正格です。しかし、`data List a = Cons !a !(List a) | Nil` という定義では、`Cons` はどちらの引数に対しても正格になります。

## Fold

遅延性を扱う上でつまづきやすいポイントは、fold です。もっとも悪名高い例は `foldl` 関数でしょう。こいつは偽りの安心感をいざない、夢と希望をぶち壊してくれます。

```haskell
mysum :: [Int] -> Int
mysum = foldl (+) 0

main :: IO ()
main = print $ mysum [1..1000000]
```

翻訳者追記 (上記プログラムの結果)

```plain
     169,311,296 bytes allocated in the heap
     230,806,408 bytes copied during GC
      53,397,048 bytes maximum residency (8 sample(s))
         903,624 bytes maximum slop
             106 MB total memory in use (0 MB lost due to fragmentation)
```

これは限りなく正解に近いですが、最大メモリ常駐量は 53mb にもなっています! 答えはチョンっとつけて正格な左畳み込み `foldl'` 関数を使うことです。

```haskell
import Data.List (foldl')

mysum :: [Int] -> Int
mysum = foldl' (+) 0

main :: IO ()
main = print $ mysum [1..1000000]
```

`Prelude` はなんでこんな絶対におかしい関数 (`foldl`) を提供しているのでしょうか?

![Hysterical Raisins](https://www.fpcomplete.com/static/hysterical-raisins.jpg)

ただ、ほとんど全ての正格であると称している関数は、実際のところ弱頭部正規形に対してのみ正格であることに留意しなければいけません。前の `average` 関数の例を見てみると、まだスペースリークがあります:

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

翻訳者追記 (上記プログラムの結果)

```plain
     306,654,600 bytes allocated in the heap
     390,431,392 bytes copied during GC
      88,082,496 bytes maximum residency (10 sample(s))
       1,160,496 bytes maximum slop
             181 MB total memory in use (0 MB lost due to fragmentation)
```

私のアドバイスは、正格なフィールドを持つ補助的なデータ型を使うことです。しかしあなたはそうしたくないかもしれませんし、正規形に評価するような `foldl'` がないことにイライラしているかもしれません。そんなあなたに朗報です。`force` を使うだけで、簡単に弱頭部正規形へ評価する fold を正規形へ評価する fold にアップグレードすることができます。

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

翻訳者追記 (上記プログラムの結果)

```plain
     240,102,848 bytes allocated in the heap
          54,552 bytes copied during GC
          44,384 bytes maximum residency (2 sample(s))
          21,152 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)
```

腕のいい水道工事業者のように、`force` はすぐにリークを止めてくれます!

### ストリーミングデータ

conduit のようなストリーミングデータライブラリの主張の1つに、メモリ使用量が定数オーダーになる、というものがあります。この主張を聞くと、スペースリークについて心配することなく、こいつとおさらばできるという印象を受けます。しかし、弱頭部正規形 vs 正規形の問題はここでも当てはまります。私の言い分を証明するために、conduit で average をひどいやり方で計算してみましょう。

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

以下のコマンドでメモリ使用量を確かめることができます。

```haskell
$ stack --resolver lts-9.3 ghc --package conduit-combinators -- Main.hs -O2
$ ./Main +RTS -s
```

翻訳者追記 (上記プログラムの結果)

```plain
     265,361,840 bytes allocated in the heap
     205,193,384 bytes copied during GC
      50,201,912 bytes maximum residency (8 sample(s))
         686,792 bytes maximum slop
              98 MB total memory in use (0 MB lost due to fragmentation)
```

**演習** 以下のものを使って、このプログラムを定数メモリ使用量で実行してみましょう。

1. `force` 関数
2. バンパターン
3. 正格なフィールドを持つカスタムデータ型

## 連鎖反応

この超正格なプログラムをご覧ください。特別な値正格リストデータ型です。私はふんだんにバンパターンを散りばめ、至るところで `seq` を呼び出しています。`$!` も使ってます。メモリ使用量はどうなるでしょうか?

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

よーく見て、コードをよく読んで、予想してみてください。準備はいいですか? いきましょう。

翻訳者追記 (上記プログラムの結果)

```plain
          51,912 bytes allocated in the heap
           3,408 bytes copied during GC
          44,504 bytes maximum residency (1 sample(s))
          25,128 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)
```

メモリ利用量は 44kb です。「なんで!?」と叫びたくなるかもしれません。「100万回正格な `Int` のリンクトリストを回しているじゃないか!」ってね。惜しい。このプログラムは `evens` の値の評価を強制した直後に、死ぬほど評価を繰り返すでしょう。これは正しい。そして、`main` の中の `string'` という値の評価を強制した直後に `evens` は評価されます。

しかし、このプログラムはどちらも強制評価が起こることはありません! 注意深く見てみれば、プログラムの最後の行は `string` という値を使っています。`string'` も `evens` も使うことはないんですね。プログラムを実行するとき、GHC は `main` 関数によって指定された `IO` アクションを実行することにのみ関心を持ちます。そして、その `main` は `putStrLn string` ということしか言っていないわけです。

このことの理解は極めて重要です。プログラム中で `seq` や `deepseq` を使い、好きなだけ評価の連鎖を組み立てることができます。しかし結局は、`IO` アクション経由で連鎖の最初に値を評価してやらないと、評価されないサンクが残ったままなのです。

**演習**

1. `putStrLn string` を `putStrLn string'` にして、メモリ使用量がどうなるか観察してください (終わったら戻してください)
2. `main` のどこかにバンパターンを置くと、メモリ利用量がはね上がります。それはどこでしょう?
3. `putStrLn string` の行のどこかに `seq` を置くと、メモリ利用率が大きくなります。それはどこでしょう?

#### 翻訳者追記

演習の解答例

1. 略
2. `string'` の前
3. 略

## もっと先に

Sebastian Graf は[このブログ記事を分析する](http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html)というタイトルで、素晴らしいブログ記事を書いています。このブログ記事は、正格性のケース毎に GHC がどのように解析、最適化をしているのかというところまでもっと踏み込んだ解説をしています。作者である彼の言葉を引用します。

「このブログ記事では、スペースリークを防ぐための、コンパイラと連携できるようなより局所的なアプローチを解説したいと思います」

もしも興味が湧いたのなら、一読してみることをおすすめします。
