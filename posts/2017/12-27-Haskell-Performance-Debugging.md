---
title: Haskell のパフォーマンスをデバッグする
author: Matt Parsons
translator: Kotaro Ohsugi
github: pythonissam
tags: To Overcome, 翻訳
---

Great original post: [Haskell Performance Debugging](http://www.parsonsmatt.org/2017/12/18/haskell_performance_debugging.html).

2017年 12月 18日 Matt Parsons

誰かが reddit に [Treap の実装が遅い](https://www.reddit.com/r/haskell/comments/7km60k/optimization_ideas_in_treap_implementation/) と投稿していました。それを分析して、何が起きているのかを考えてみましょう。

レポジトリは[ここ](https://github.com/parsonsmatt/performance-debugging)にあります。

## 最初の実行
Cabalプロジェクトを作り、makefile を作ります。そして、最初のプロファイルを取ります。コードとプロファイル結果は GitHub の `master`ブランチにあります。

実行されているコードやプロファイル結果を見る前に、質問のデータ構造の定義を確認しておきましょう:

```haskell
data Node  v d = Node { val :: v, info :: d, prior :: Int }
    deriving (Eq, Show)
data Treap v d = Leaf | Tree {node :: Node v d, left :: Treap v d, right :: Treap v d}
    deriving Show
```

<!--more-->

注釈つきの2分木ですね。 spine と値は、リスト同様に遅延評価されます。

以下が `main` 関数で、この出力を調べます。

```haskell
main = do
    g <- getStdGen
    let nulls = repeat ()
        n = 100000
        rxs = take n $ randomRs (1,100000) g  :: [Int]
        nodeList = feedFold (zip rxs nulls) g buildNode
        treap = insertMany empty nodeList

    print $ heightTreap treap

    print $ map (\Node{val = v} -> v) $ inOrder treap
```

これをプロファイリングしつつビルドして、`-p` と `-s` をつけて実行します。これは時間とメモリ確保についてのプロファイリングをしてくれます。
以下が `-s` の出力です:

```
   1,691,027,808 bytes allocated in the heap
   1,179,783,328 bytes copied during GC
      42,694,944 bytes maximum residency (25 sample(s))
       8,493,296 bytes maximum slop
             121 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      3033 colls,     0 par    0.716s   0.752s     0.0002s    0.0008s
  Gen  1        25 colls,     0 par    0.544s   0.560s     0.0224s    0.0460s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    1.088s  (  1.140s elapsed)
  GC      time    1.260s  (  1.312s elapsed)
  RP      time    0.000s  (  0.000s elapsed)
  PROF    time    0.000s  (  0.000s elapsed)
  EXIT    time    0.000s  (  0.003s elapsed)
  Total   time    2.404s  (  2.455s elapsed)

  %GC     time      52.4%  (53.4% elapsed)

  Alloc rate    1,554,253,500 bytes per MUT second

  Productivity  47.6% of total user, 46.5% of total elapsed
```

GC に 52% の時間が割かれているのはよろしくないですね。

プロファイル結果によると、かなり大部分の時間を `splitTreap`関数で消費してしまっているようです。なので、そこで何が起きているのか確認しましょう:

```haskell
splitTreap :: (Ord v) => Treap v d -> v -> (Treap v d, Treap v d)
splitTreap Leaf _ = (Leaf, Leaf)
splitTreap (tree @ Tree {node = Node { val = x }, left = l, right = r})   v
    | x < v  =
        let (lt, rt) = splitTreap r v
         in ( Tree { node = node tree, left = l, right = lt }
            , rt
            )
    | v <= x =
        let (lt, rt) = splitTreap l v
         in ( lt
            , Tree { node = node tree, left = rt, right = r}
            )
```

私には気になる点が2つ見つかりました:

* タプル
* 再帰

タプルはしばしば、意図しない遅延やスペースリークの原因になります。GHC がタプルのデータ構造を見て、それを完全にアンボックス化し、オーバーヘッドを 0 にできる場合もあります。しかし、それができないときもあります。結果割り当てられまくって、サンクリークが始まります。

再帰は GHC のインライン化能力を完全に上回っており、パフォーマンスを台無しにしてしまいます。`map` や `foldr` などはクレバーな最適化を受けることができますが、単純な再帰関数には大抵、インライン化において問題が存在します。

これらが実験を始める前の私の印象です。私のタプルメモリ割り当て仮説を検証するために、ヒープのプロファイリングを行いましょう。`-hd`フラグを使って確保されたデータコンストラクタを取得します:

![-hd フラグを使ったプロファイル](http://www.parsonsmatt.org/treap-base-hd.png)

いい感じですね! さて、このグラフは `Tree` コンストラクタの割り当て前に、大量のノード、タプル、`I#` (`Int` のコンストラクタ) を割り当てていることを示しています。対象の `main` 関数だと、この挙動は完全に非合理的というわけではありません。

## 実験1: データ構造を正格化する
このセクション関連のコードは `strictify-treap` にあります。

データ構造のところどころにバンパターンを差し込んでみました:

```haskell
data Node v d
    = Node
    { val :: !v
    , info :: d
    , prior :: !Int
    } deriving (Eq, Show)

data Treap v d
    = Leaf
    | Tree
    { node :: !(Node v d)
    , left :: Treap v d
    , right :: Treap v d
    } deriving Show
```

こうすることで `Node` 型の `val` フィールドと `prior` フィールドが正格になり、`Treap` 型は `node` フィールドに正格になります。`info` フィールドはたいていのコンテナのように、lazy のままにしてあります。データ構造の spine も lazy のままです。

`-s` の出力です:

```
1,659,050,200 bytes allocated in the heap
   1,144,049,696 bytes copied during GC
      43,890,168 bytes maximum residency (33 sample(s))
       8,508,680 bytes maximum slop
             102 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      2905 colls,     0 par    0.676s   0.696s     0.0002s    0.0007s
  Gen  1        33 colls,     0 par    0.544s   0.567s     0.0172s    0.0409s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.920s  (  0.996s elapsed)
  GC      time    1.220s  (  1.263s elapsed)
  RP      time    0.000s  (  0.000s elapsed)
  PROF    time    0.000s  (  0.000s elapsed)
  EXIT    time    0.000s  (  0.001s elapsed)
  Total   time    2.196s  (  2.260s elapsed)

  %GC     time      55.6%  (55.9% elapsed)

  Alloc rate    1,803,315,434 bytes per MUT second

  Productivity  44.4% of total user, 44.1% of total elapsed
```

20MB も使用しているメモリが減っています。これはいいですね。そして全体の使用時間も少なくなっています (2.4秒 vs 2.2秒)。これもいいことです! しかし GC が使っている時間は 55% です。前より悪くなってるじゃなイカ!

以下はヒーププロファイリングの結果です:

![ヒーププロファイリングの結果](http://www.parsonsmatt.org/treap-strict-nodes.png)

大きな違いはありませんが、確かに少し良い結果です。時間と割り当てのプロファイリングは全く違う結果を証明していています。後々、プログラムの実行時間は 2.49秒から 0.97秒になります。

この結果にかなり励まされつつ、木の spine も正格にしようと思います。

```haskell
data Node v d
    = Node
    { val :: !v
    , info :: d
    , prior :: !Int
    } deriving (Eq, Show)

data Treap v d
    = Leaf
    | Tree
    { node :: !(Node v d)
    , left :: !(Treap v d)
    , right :: !(Treap v d)
    } deriving Show
```

`-s` の出力結果によると、合計でまだ 94MB 程度のメモリを使っているようです。

```
  1,161,437,656 bytes allocated in the heap
     449,893,272 bytes copied during GC
      43,890,328 bytes maximum residency (24 sample(s))
       8,520,808 bytes maximum slop
              94 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      2143 colls,     0 par    0.152s   0.166s     0.0001s    0.0006s
  Gen  1        24 colls,     0 par    0.188s   0.203s     0.0085s    0.0272s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.556s  (  0.644s elapsed)
  GC      time    0.320s  (  0.345s elapsed)
  RP      time    0.000s  (  0.000s elapsed)
  PROF    time    0.020s  (  0.024s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.952s  (  0.989s elapsed)

  %GC     time      33.6%  (34.9% elapsed)

  Alloc rate    2,088,916,647 bytes per MUT second

  Productivity  64.3% of total user, 62.6% of total elapsed
```

これはかなり良い結果です。GC が 33% なのは良くないですが、それでも前よりもだいぶ良い結果です。2.4秒から 0.95秒になりました。これはかなりの改善です。

今度はヒープの出力も見てみましょう:

![ヒープの出力](http://www.parsonsmatt.org/treap-strict-spine.png)

もう少しですね! 大きなメモリの山が生成され、それが回収されています。何かが起きていることの兆候です。タプルコンストラクタに多くの割り当てをしてしまっています。つらいですね。

## Split を正格化する
まだ `splitTreap` という最大の犯罪者がいます。こいつはプログラムの実行時間のほぼ半分を占めています。タプルを割り当て、それを捨てていることはわかっているので、そこにスペースリークがある可能性があります。タプルの中にバンパターンを追加し、結果を見てみます。

これが変更点です:

```haskell
splitTreap :: (Ord v) => Treap v d -> v -> (Treap v d, Treap v d)
splitTreap Leaf _ = (Leaf, Leaf)
splitTreap (tree @ Tree {node = Node { val = x }, left = l, right = r})   v
    | x < v  = let (!lt, !rt) = splitTreap r v in
                (   Tree { node = node tree, left = l, right = lt },
                    rt  )
    | v <= x = let (!lt, !rt) = splitTreap l v in
                (   lt,
                    Tree { node = node tree, left = rt, right = r}  )
```

元のコードがタプルを直ちに破棄して `lt` と `rt` 変数を lazy なままにしているのに対して、これらの変数を WHNF に強制しています。

新しい `-s` の出力結果です:

```
   1,331,896,120 bytes allocated in the heap
     497,880,136 bytes copied during GC
      43,890,328 bytes maximum residency (25 sample(s))
       8,516,712 bytes maximum slop
              94 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      2245 colls,     0 par    0.188s   0.186s     0.0001s    0.0007s
  Gen  1        25 colls,     0 par    0.212s   0.251s     0.0100s    0.0386s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.636s  (  0.756s elapsed)
  GC      time    0.360s  (  0.394s elapsed)
  RP      time    0.000s  (  0.000s elapsed)
  PROF    time    0.040s  (  0.043s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    1.084s  (  1.151s elapsed)

  %GC     time      33.2%  (34.2% elapsed)

  Alloc rate    2,094,176,289 bytes per MUT second

  Productivity  63.1% of total user, 62.0% of total elapsed
```

これはさっきの実行からほとんど変わっていません。ヒーププロファイルも変更されませんでした。これらのタプルがどこに割り当てられるのかを見るために、`-hc` をつけて実行してみます。`-hc` は実際にどの関数がデータを生成しているのかを記録してくれるので、どこに注目するべきかが分かります。

![-hc を使ったプロファイル](http://www.parsonsmatt.org/treap-strict-tuple-hc.png)

**あぁナッツ!** `splitTreap` がほんの少ししかメモリ割り当てがされなくなってるぞ。`buildNode`, `feedFold`, `insertMany` でほとんどの割り当てを行っているみたいです。これは、`splitTreap` に多くの時間と割り当てをしているという `-p` オプションの結果に反しているように見えます。

今は `insertMany` に集中するべきでしょう。

## insertMany
このセクションのコードは、GitHub の`insert-many`ブランチにあります。

`mergeTreap` はなぜかしらカリー化されていましたが:

```haskell
mergeTreap :: (Treap v d, Treap v d) -> Treap v d
```

これはうざかったので、上のようにアンカリー化しました。これはパフォーマンスには関係ありません。

ここで、`insertMany` を実際に見てみます:

```haskell
insertMany :: (Ord v) => Treap v d -> [Node v d] -> Treap v d
insertMany = foldl insertTreap
```

あぁ。`foldl` がまた来やがりました。

```haskell
insertMany :: (Ord v) => Treap v d -> [Node v d] -> Treap v d
insertMany = foldl' insertTreap
```

さぁ、GHC のアメージングな最適化力とプライムボーイ、どちらが勝つのでしょうか? `-s` の結果です:

```plain
   1,115,162,944 bytes allocated in the heap
     245,033,472 bytes copied during GC
      12,088,896 bytes maximum residency (22 sample(s))
         306,112 bytes maximum slop
              32 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      2134 colls,     0 par    0.116s   0.128s     0.0001s    0.0003s
  Gen  1        22 colls,     0 par    0.080s   0.105s     0.0048s    0.0136s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.596s  (  0.700s elapsed)
  GC      time    0.180s  (  0.216s elapsed)
  RP      time    0.000s  (  0.000s elapsed)
  PROF    time    0.016s  (  0.018s elapsed)
  EXIT    time    0.000s  (  0.001s elapsed)
  Total   time    0.852s  (  0.916s elapsed)

  %GC     time      21.1%  (23.6% elapsed)

  Alloc rate    1,871,078,765 bytes per MUT second

  Productivity  77.0% of total user, 74.4% of total elapsed
```

良いですね、トータルの使用メモリが 32MB になり、全体の時間は 10分の1 ぐらい減りました。しかも、GC にかかっている時間はたったの 22% です。我々の大勝利です。

ヒープのプロファイル結果を見てみましょう:

![ヒーププロファイルの結果](http://www.parsonsmatt.org/treap-foldl.png)

## 何があっても foldl を使わず、常に foldl' を使うべし

最初の状態で foldl を foldl' に変更してみたらどうでしょう?

`git checkout base` を走らせて元に戻し、`foldl` を `foldl'` に変更してみました。`-s` の結果です:

```plain
1,581,972,168 bytes allocated in the heap
   1,140,799,032 bytes copied during GC
      40,964,944 bytes maximum residency (43 sample(s))
         495,784 bytes maximum slop
             114 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      3044 colls,     0 par    0.664s   0.636s     0.0002s    0.0004s
  Gen  1        43 colls,     0 par    0.796s   0.812s     0.0189s    0.0611s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.964s  (  1.226s elapsed)
  GC      time    1.336s  (  1.320s elapsed)
  RP      time    0.000s  (  0.000s elapsed)
  PROF    time    0.124s  (  0.128s elapsed)
  EXIT    time    0.000s  (  0.002s elapsed)
  Total   time    2.488s  (  2.548s elapsed)

  %GC     time      53.7%  (51.8% elapsed)

  Alloc rate    1,641,049,966 bytes per MUT second

  Productivity  41.3% of total user, 43.2% of total elapsed
```

よろしくはないですね。というか、初めよりもちょっと悪くなってます! ヒープのプロファイル結果はどうでしょう?

![ヒーププロファイルの結果](http://www.parsonsmatt.org/treap-just-foldl.png)

これもほとんど同じですね! 割り当てはちょっとなめらかになっていますが、顕著な違いでもありません。というわけで、データ構造を正格にせずに、ただ `foldl'` に変更しても意味がありませんでした。

## 最終結果
プロファイリングを無効にして、もう一回コードを実行してみます:

```plain
     650,786,800 bytes allocated in the heap
     132,515,880 bytes copied during GC
       7,278,528 bytes maximum residency (17 sample(s))
         353,296 bytes maximum slop
              21 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      1233 colls,     0 par    0.112s   0.100s     0.0001s    0.0004s
  Gen  1        17 colls,     0 par    0.056s   0.056s     0.0033s    0.0122s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.212s  (  0.341s elapsed)
  GC      time    0.168s  (  0.156s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.436s  (  0.497s elapsed)

  %GC     time      38.5%  (31.3% elapsed)

  Alloc rate    3,069,749,056 bytes per MUT second

  Productivity  61.5% of total user, 68.7% of total elapsed
```

メモリ消費は 21MB, 実行時間は 0.43秒でした。

## 結論?
### spine は正格に、リーブは lazy に
データ構造的には、spine は正格に、リーブは lazy にすべきです。データコンストラクタを、ストリームの形で組み立てたり消費したいという強い意思があるなら話は別ですが。

### foldl は絶対に使うな
まじめに使っちゃダメです。`hlint` のルールにそうしないように書いてください。コードから排除しましょう。廃止して、`Prelude` を置き換えるように GHC に提案しましょう。