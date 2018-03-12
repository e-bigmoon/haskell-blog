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

このコードは正格な C のコードと全く同じ動作をします。先ほどのコードとの違いは `add` 関数の `x` と `y` の前にバン (`!`) があるからです。これによって GHC は `add` を評価する前に `x` と `y` の値を評価しなければならないと判断します。同様に `five` と `seven` の前にも `!` があるため、 GHC は `putStrLn` を評価する前にこれらの値を評価します。

Haskell には多くのものがありますが、バンパターンはただの `seq` 関数を使ったシンタックスシュガーです。`seq` 関数は以下のような型です。

```haskell
seq :: a -> b -> b
```

この型シグネチャを見ると、`a` の値を無視するような以下の実装にしたくなるでしょう。

```haskell
badseq :: a -> b -> b
badseq a b = b
```

けれども、 `seq` は GHC が提供しているプリミティブな操作を使って`b` が評価されてから `a` が評価されることを保証する関数です。ここで先ほどの `add` 関数をバンパターンの代わりに `seq` を使って書き換えてみましょう。

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

もちろん、`let` と `in` を使ってこんなに長いコードを書くには大変なので、大抵のプログラマは最後の行のように `seq` を中置記法で使います。

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

## The value of bottom
This is all well and good, but the more standard way to demonstrate evaluation order is to use bottom values, aka `undefined`. `undefined` is special in that, when it is evaluated, it throws a runtime exception. (The `error` function does the same thing, as do a few other special functions and values.) To demonstrate the same thing about `seven` not being evaluated without the bangs, compare these two programs:

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

Versus:

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

The former completes without issue, since `seven` is never evaluated. However, in the latter, we have a bang pattern on `seven`. What GHC does here is:

- Evaluate the expression `add (1 + 2) undefined`
- This reduces to `(1 + 2) + undefined`
- But this is still an expression, not a value, so more evaluation is necessary
- In order to evaluate the `+` operator, it needs actual values for the two arguments, not just thunks. This can be seen as if `+` has bang patterns on its arguments. The correct way to say this is "`+` is strict in both of its arguments."
- GHC is free to choose to either evaluate `1 + 2` or `undefined` first. Let's assume it does `1 + 2` first. It will come up with two evaluated values (`1` and `2`), pass them to `+`, and get back `3`. All good.
- However, it then tries to evaluate `undefined`, which triggers a runtime exception to be thrown.

**QUESTION** Returning to the question above: does it look like bang patterns inside the `add` function actually accomplish anything? Think about what the output of this program will be:

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

To compare this behavior to a strict language, we need a language with something like runtime exceptions. I'll use Rust's panics:

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

Firstly, to Rust's credit: it gives me a bunch of warnings about how this program is dumb. Fair enough, but I'm going to ignore those warnings and charge ahead with it. This program will first evaluate the `add(1 + 1, 1 + 2)` expression (which we can see in the output of `adding: 2 and 3`). Then, before it ever enters the add function the second time, it needs to evaluate both `1 + 2` and `panic!()`. The former works just fine, but the latter results in a panic being generated and short-circuiting the rest of our function.

If we want to regain Haskell's laziness properties, there's a straightforward way to do it: use a closure. A closure is, essentially, a thunk. The Rust syntax for creating a closure is `|args| body`. We can create closures with no arguments to act like thunks, which gives us:

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

Again, the Rust compiler complains about the unused `seven`, but this program succeeds in running, since we never run the `seven` closure.

Still not up to speed with Rust? Let's use everyone's favorite language: Javascript:

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

Alright, to summarize until now:

- Haskell is lazy by default
- You can use bang patterns and `seq` to make things strict
- By contrast, in strict languages, you can use closures to make things lazy
- You can see if a function is strict in its arguments by passing in bottom (`undefined`) and seeing if it explodes in your face
- The `trace` function can help you see this as well

This is all good, and make sure you have a solid grasp of these concepts before continuing. Consider rereading the sections above.

## Average
Here's something we didn't address: what, exactly, does it mean to evaluate or force a value? To demonstrate the problem, let's implement an average function. We'll use a helper datatype, called `RunningTotal`, to capture both the cumulative sum and the number of elements we've seen so far.

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

We're going to run this with run time statistics turned on so we can look at memory usage:

```bash
$ stack ghc average.hs && ./average +RTS -s
```

Lo and behold, our memory usage is through the roof!

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

We're allocating a total of 258MB, and keeping 95MB in memory at once. For something that should just be a tight inner loop, that's ridiculously large.

### Bang!
You're probably thinking right now "shouldn't we use that `seq` stuff or those bang patterns?" Certainly that makes sense. And in fact, it looks really trivial to solve this problem with a single bang to force evaluation of the newly constructed `rt` before recursing back into `go`. For example, we can add `{-# LANGUAGE BangPatterns #-}` to the top of our file and then define `go` as:

```haskell
go !rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

Unfortunately, this results in exactly the same memory usage as we had before. In order to understand why this is happening, we need to look at something called weak head normal form.

### Weak Head Normal Form
Note in advance that [there's a great Stack Overflow answer](https://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form/6889335#6889335) on this topic for further reading.

We've been talking about forcing values and evaluating expressions, but what exactly that means hasn't been totally clear. To start simple, what will the output of this program be?

```haskell
main = putStrLn $ undefined `seq` "Hello World"
```

You'd probably guess that it will print an error about `undefined`, since it will try to evaluate `undefined` before it will evaluate `"Hello World"`, and because `putStrLn` is strict in its argument. And you'd be correct. But let's try something a little bit different:

```haskell
main = putStrLn $ Just undefined `seq` "Hello World"
```

If you assume that "evaluate" means "fully evaluate into something with no thunks left," you'll say that this, too, prints an `undefined` error. But in fact, it happily prints out "Hello World" with no exceptions. What gives?

It turns out that when we talk about forcing evaluation with `seq`, we're only talking about evaluating to *weak head normal form (WHNF)*. For most data types, this means unwrapping one layer of constructor. In the case of `Just undefined`, it means that we unwrap the `Just` data constructor, but don't touch the `undefined` within it. (We'll see a few ways to deal with this differently below.)

It turns out that, with a standard data constructor*, the impact of using `seq` is the same as pattern matching the outermost constructor. If you want to monomorphise, for example, you can implement a function of type `seqMaybe :: Maybe a -> b -> b` and use it in the `main` example above. Go ahead and give it a shot... answer below.

* Hold your horses, we'll talk about `newtypes` later and then you'll understand this weird phrasing.

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

```haskell
main = do
  putStrLn $ error `seq` "Hello"
  putStrLn $ (\x -> undefined) `seq` "World"
  putStrLn $ error "foo" `seq` "Goodbye!"
```

You might think that ``error `seq` ...`` would be a problem. After all, isn't `error` going to throw an exception? However, `error` is a function. There's no exception getting thrown, or no bottom value being provided, until `error` is given its `String` argument. As a result, evaluating does not, in fact, generate an error. The rule is: any function applied to too few values is automatically in WHNF.

A similar logic applies to `(\x -> undefined)`. Although it's a lambda expression, its type is a function which has not been applied to all arguments. And therefore, it will not throw an exception when evaluated. In other words, it's already in WHNF.

However, `error "foo"` is a function fully applied to its arguments. It's no longer a function, it's a value. And when we try to evaluate it to WHNF, its exception blows up in our face.

**EXERCISE** Will the following throw exceptions when evaluated?

- `(+) undefined`
- `Just undefined`
- `undefined 5`
- `(error "foo" :: Int -> Double)`

### Fixing average
Having understood WHNF, let's return to our example and see why our first bang pattern did nothing to help us:

```haskell
go !rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

In WHNF, forcing evaluation is the same as unwrapping the constructor, which we are already doing in the second clause! The problem is that the values contained inside the `RunningTotal` data constructor are not being evaluated, and therefore are accumulating thunks. Let's see two ways to solve this:

```haskell
go rt [] = printAverage rt
go (RunningTotal !sum !count) (x:xs) =
  let rt = RunningTotal (sum + x) (count + 1)
   in go rt xs
```

Instead of putting the bangs on the `RunningTotal` value, I'm putting them on the values within the constructor, forcing them to be evaluated at each loop. We're no longer accumulating a huge chain of thunks, and our maximum residency drops to 44kb. (Total allocations, though, are still up around 192mb. We need to play around with other optimizations outside the scope of this post to deal with the total allocations, so we're going to ignore this value for the rest of the examples.) Another approach is:

```haskell
go rt [] = printAverage rt
go (RunningTotal sum count) (x:xs) =
  let !sum' = sum + x
      !count' = count + 1
      rt = RunningTotal sum' count'
   in go rt xs
```

This one instead forces evaluation of the new sum and count before constructing the new `RunningTotal` value. I like this version a bit more, as it's forcing evaluation at the correct point: when creating the value, instead of on the next iteration of the loop when destructing it.

Moral of the story: make sure you're evaluating the thing you actually need to evaluate, not just its container!

### deepseq
The fact that `seq` only evaluates to weak head normal form is annoying. There are lots of times when we would like to fully evaluate down to normal form (NF), meaning all thunks have been evaluated inside our values. While there is nothing built into the language to handle this, there is a semi-standard (meaning it ships with GHC) library to handle this: `deepseq`. It works by providing an `NFData` type class the defines how to reduce a value to normal form (via the `rnf` method).

```haskell
{-# LANGUAGE BangPatterns #-}
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

This has a maximum residency, once again, of 44kb. We define our `NFData` instance, which includes an `rnf` method. The approach of simply `deepseq`ing all of the values within a data constructor is almost always the approach to take for `NFData` instances. In fact, it's so common, that you can get away with just using `Generic` deriving and have GHC do the work for you:

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

The true beauty of having `NFData` instances is the ability to abstract over many different data types. We can use this not only to avoid space leaks (as we're doing here), but also to avoid accidentally including exceptions inside thunks within a value. For an example of that, check out the [tryAnyDeep](https://www.stackage.org/haddock/lts-9.3/safe-exceptions-0.1.6.0/Control-Exception-Safe.html#v:tryAnyDeep) function from the [safe-exceptions library](https://haskell-lang.org/library/safe-exceptions).

**EXERCISE** Define the `deepseq` function yourself in terms of `rnf` and `seq`.

### Strict data
These approaches work, but they are not ideal. The problem lies in our definition of `RunningTotal`. What we want to say is that, whenever you have a value of type `RunningTotal`, you in fact have two `Int`s. But because of laziness, what we're actually saying is that a `RunningTotal` value could contain two `Int`s, or it could contain thunks that will evaluate to `Int`s, or thunks that will throw exceptions.

Instead, we'd like to make it impossible to construct a `RunningTotal` value that has any laziness room left over. And to do that, we can use strictness annotations in our definition of the data type:

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

All we've done is put bangs in front of the `Int`s in the definition of `RunningTotal`. We have no other references to strictness or evaluation in our program. However, by placing the strictness annotations on those fields, we're saying something simple and yet profound:

**Whenever you evaluate a value of type `RunningTotal`, you must also evaluate the two `Int`s it contains**

As we mentioned above, our second `go` clause forces evaluation of the `RunningTotal` value by taking apart its constructor. This act now automatically forces evaluation of `sum` and `count`, which we previously needed to achieve via a bang pattern.

There's one other advantage to this, which is slightly out of scope but worth mentioning. When dealing with small values like an `Int`, GHC will automatically unbox strict fields. This means that, instead of keeping a pointer to an `Int` inside `RunningTotal`, it will keep the `Int` itself. This can further reduce memory usage.

You're probably asking a pretty good question right now: "how do I know if I should use a strictness annotation on my data fields?" This answer is slightly controversial, but my advice and recommended best practice: unless you know that you want laziness for a field, make it strict. Making your fields strict helps in a few ways:

- Avoids accidental space leaks, like we're doing here
- Avoids accidentally including bottom values
- When constructing a value with record syntax, GHC will give you an error if you forget a strict field. It will only give you a warning for non-strict fields.

### The curious case of newtype
Let's define three very similar data types:

```haskell
data Foo = Foo Int
data Bar = Bar !Int
newtype Baz = Baz Int
```

Let's play a game, and guess the output of the following potential bodies for `main`. Try to work through each case in your head before reading the explanation below.

1. `case undefined of { Foo _ -> putStrLn "Still alive!" }`
2. `case Foo undefined of { Foo _ -> putStrLn "Still alive!" }`
3. `case undefined of { Bar _ -> putStrLn "Still alive!" }`
4. `case Bar undefined of { Bar _ -> putStrLn "Still alive!" }`
5. `case undefined of { Baz _ -> putStrLn "Still alive!" }`
6. `case Baz undefined of { Baz _ -> putStrLn "Still alive!" }`

Case (1) is relatively straightforward: we try to unwrap one layer of data constructor (the `Foo`) and find a bottom value. So this thing throws an exception. The same thing applies to (3).

(2) does not throw an exception. We have a `Foo` data constructor in our expression, and it contains a bottom value. However, since there is no strictness annotation on the `Int` in `Foo`, uwnrapping the `Foo` does not force evaluation of the `Int`, and therefore no exception is thrown. By contrast, in (4), we do have a strictness annotation, and therefore `case`ing on `Bar` throws an exception.

What about `newtype`s? What we know about `newtype`s is that they have no runtime representation. Therefore, it's impossible for the `Baz` data constructor to be hiding an extra layer of bottomness. In other words, `Baz undefined` and `undefined` are indistinguishable. That may sound like `Bar` at first, but interestingly it's not.

You see, unwrapping a `Baz` constructor can have no effect on runtime behavior, since it was never there in the first place. The pattern match inside (5), therefore, does nothing. It is equivalent to `case undefined of { _ -> putStrLn "Still alive!" }`. And since we're not inspecting the `undefined` at all (because we're using a wildcard pattern and not a data constructor), no exception is thrown.

Similarly, in case (6), we've applied a `Baz` constructor to `undefined`, but since it has no runtime representation, it may as well not be there. So once again, no exception is thrown.

**EXERCISE** What is the output of the program ``main = Baz undefined `seq` putStrLn "Still alive!"? Why``?

### Convenience operators and functions
It can be inconvenient, as you may have noticed already, to use `seq` and `deepseq` all over the place. Bang patterns help, but there are other ways to force evaluation. Perhaps the most common is the `$!` operator, e.g.:

```haskell
mysum :: [Int] -> Int
mysum list0 =
  go list0 0
  where
    go [] total = total
    go (x:xs) total = go xs $! total + x

main = print $ mysum [1..1000000]
```

This forces evaluation of `total + x` before recursing back into the `go` function, avoiding a space leak. (EXERCISE: do the same thing with a bang pattern, and with the `seq` function.)

The `$!!` operator is the same, except instead of working with `seq`, it uses `deepseq` and therefore evaluates to normal form.

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

Another nice helper function is `force`. What this does is makes it that, when the expression you're looking at is evaluated to WHNF, it's actually evaluated to NF. For example, we can rewrite the `go` function above as:

```haskell
go [] (total, count) = fromIntegral total / count
go (x:xs) (total, count) = go xs $! force (total + x, count + 1)
```

**EXERCISE** Define these convenience functions and operators yourself in terms of `seq` and `deepseq`.

## Data structures
Alright, I swear that's all of the really complicated stuff. If you've absorbed all of those details, the rest of this just follows naturally and introduces a little bit more terminology to help us understand things.

Let's start off slowly: what's the output of this program:

```haskell
data List a = Cons a (List a) | Nil

main = Cons undefined undefined `seq` putStrLn "Hello World"
```

Well, using our principles from above: `Cons undefined undefined` is already in WHNF, since we've got the outermost constructor available. So this program prints "Hello World", without any exceptions. Cool. Now let's realize that `Cons` is the same as the `:` data constructor for lists, and see that the above is identical to:

```haskell
main = (undefined:undefined) `seq` putStrLn "Hello World"
```

This tells me that lists are a lazy data structure: I have a bottom value for the first element, a bottom value for the rest of the list, and yet this first cell is not bottom. Let's try something a little bit different:

```haskell
data List a = Cons a !(List a) | Nil

main = Cons undefined undefined `seq` putStrLn "Hello World"
```

This is going to explode in our faces! We are now strict in the tail of the list. However, the following is fine:

```haskell
data List a = Cons a !(List a) | Nil

main = Cons undefined (Cons undefined Nil) `seq` putStrLn "Hello World"
```

With this definition of a list, we need to know all the details about the list itself, but the values can remain undefined. This is called spine strict. By contrast, we can also be strict in the values and be value strict:

```haskell
data List a = Cons !a !(List a) | Nil

main = Cons undefined (Cons undefined Nil) `seq` putStrLn "Hello World"
```

This will explode in our faces, as we'd expect.

There's one final definition of list you may be expecting, one strict in values but not in the tail:

```haskell
data List a = Cons !a (List a) | Nil
```

In practice, I'm aware of no data structures in Haskell that follow this pattern, and therefore it doesn't have a name. (If there are such data structures, and this does have a name, please let me know, I'd be curious about the use cases for it.)

So standard lists are lazy. Let's look at a few other data types:

### Vectors
The vectors in `Data.Vector` (also known as boxed vectors) are spine strict. Assuming an import of `import qualified Data.Vector as V`, what would be the results of the following programs?

1. ``main = V.fromList [undefined] `seq` putStrLn "Hello World"``
2. ``main = V.fromList (undefined:undefined) `seq` putStrLn "Hello World"``
3. ``main = V.fromList undefined `seq` putStrLn "Hello World"``

The first succeeds: we have the full spine of the vector defined. The fact that it contains a bottom value is irrelevant. The second fails, since the spine of the tail of the list is undefined, making the spine undefined. And finally the third (of course) fails, since the entire list is undefined.

Now let's look at unboxed vectors. Because of inference issues, we need to help out GHC a little bit more. So starting with this head of a program:

```haskell
import qualified Data.Vector.Unboxed as V

fromList :: [Int] -> V.Vector Int
fromList = V.fromList
```

What happens with the three cases above?

1. ``main = fromList [undefined] `seq` putStrLn "Hello World"``
2. ``main = fromList (undefined:undefined) `seq` putStrLn "Hello World"``
3. ``main = fromList undefined `seq` putStrLn "Hello World"``

As you'd expect, (2) and (3) have the same behavior as with boxed vectors. However, (1) also throws an exception, since unboxed vectors are value strict, not just spine strict. The same applies to storable and primitive vectors.

Unfortunately, to my knowledge, there is no definition of a strict, boxed vector in a public library. Such a data type would be useful to help avoid space leaks (such as the original question that triggered this blog post).

### Sets and Maps
If you look at the containers and unordered-containers packages, you may have noticed that the Map-like modules come in `Strict` and `Lazy` variants (e.g., `Data.HashMap.Strict` and `Data.HashMap.Lazy`) while the Set-like modules do not (e.g., `Data.IntSet`). This is because all of these containers are spine strict, and therefore must be strict in the keys. Since a set only has keys, no separate values, it must also be value strict.

A map, by contrast, has both keys and values. The lazy variants of the map-like modules are spine-strict, value-lazy, whereas the strict variants are both spine and value strict.

**EXERCISE** Analyze the `Data.Sequence.Seq` data type and classify it as either lazy, spine strict, or value strict.

## Function arguments
A function is considered strict in one of its arguments if, when the function is applied to a bottom value for that argument, the result is bottom. As we saw way above, `+` for `Int` is strict in both of its arguments, since: `undefined + x` is bottom, and `x + undefined` is bottom.

By contrast, the `const` function, defined as `const a b = a`, is strict in its first argument and lazy in its second argument.

The `:` data constructor for lists is lazy in both its first and second argument. But if you have `data List = Cons !a !(List a) | Nil`, `Cons` is strict in both its first and second argument.

## Folds
A common place to end up getting tripped up by laziness is dealing with folds. The most infamous example is the `foldl` function, which lulls you into a false sense of safety only to dash your hopes and destroy your dreams:

```haskell
mysum :: [Int] -> Int
mysum = foldl (+) 0

main :: IO ()
main = print $ mysum [1..1000000]
```

This is so close to correct, and yet uses 53mb of resident memory! The solution is but a tick away, using the strict left fold `foldl'` function:

```haskell
import Data.List (foldl')

mysum :: [Int] -> Int
mysum = foldl' (+) 0

main :: IO ()
main = print $ mysum [1..1000000]
```

Why does the `Prelude` expose a function (`foldl`) which is almost always the wrong one to use?



But the important thing to note about almost all functions that claim to be strict is that they are only strict to weak head normal form. Pulling up our `average` example from before, this still has a space leak:

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

My advice is to use a helper data type with strict fields. But perhaps you don't want to do that, and you're frustrated that there is no `foldl'` that evaluates to normal form. Fortunately for you, by just throwing in a call to `force`, you can easily upgrade a WHNF fold into a NF fold:

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

Like a good plumber, `force` patches that leak right up!

### Streaming data
One of the claims of streaming data libraries (like conduit) is that they promote constant memory usage. This may make you think that you can get away without worrying about space leaks. However, all of the comments about WHNF vs NF mentioned above apply. To prove the point, let's do average badly with conduit:

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

You can test the memory usage of this with:

```haskell
$ stack --resolver lts-9.3 ghc --package conduit-combinators -- Main.hs -O2
$ ./Main +RTS -s
```

**EXERCISE** Make this program run in constant resident memory, by using:

1. The `force` function
2. Bang patterns
3. A custom data type with strict fields


## Chain reaction
Look at this super strict program. It's got a special value-strict list data type. I've liberally sprinkled bang patterns and calls to `seq` throughout. I've used `$!`. How much memory do you think it uses?

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

Look carefully, read the code well, and make a guess. Ready? Good.

It uses 44kb of memory. "What?!" you may exclaim. "But this thing has to hold onto a million `Int`s in a strict linked list!" Ehh... almost. It's true, our program is going to do a hell of a lot of evaluation as soon as we force the `evens` value. And as soon as we force the `string'` value in `main`, we'll force `evens`.

However, our program never actually forces evaluation of either of these! If you look carefully, the last line in the program uses the `string` value. It never looks at `string'` or `evens`. When executing our program, GHC is only interested in performing the `IO` actions it is told to perform by the `main` function. And `main` only says something about `putStrLn string`.

This is vital to understand. You can build up as many chains of evaluation using `seq` and `deepseq` as you want in your program. But ultimately, unless you force evaluation via some `IO` action of the value at the top of the chain, it will all remain an unevaluated thunk.

**EXERCISES**

1. Change `putStrLn string` to `putStrLn string'` and see what happens to memory usage. (Then undo that change for the other exercises.)
2. Use a bang pattern in `main` somewhere to get the great memory usage.
3. Add a `seq` somewhere in the `putStrLn string` line to force the greater memory usage.
