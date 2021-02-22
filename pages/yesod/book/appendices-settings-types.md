---
title: Settings Types
published: 2021/02/22
---

ウェブサーバを書いているとしよう. サーバにlistenするポートと実行するアプリケーションを取りたいとする. そこで次のような関数を作る:

``` haskell
run :: Int -> Application -> IO ()
```

しかし, 突然誰かがタイムアウト時間をカスタマイズしたいことに気づく. するとAPIを次のように変更する:

``` haskell
run :: Int -> Int -> Application -> IO ()
```

このときどの`Int`がタイムアウトで, どれがポートなのであろうか? ええ, 型エイリアスを作るか, コードにコメントする必要がある. しかし, このコードに忍び込む他の問題が存在する: この`run`関数は管理不能になる. どのように例外が処理されるかを指示する他のパラメータや, どのホストにバインドするかについて制御するパラメータなどのまた別のパラメータが必要になる.

より拡張可能な解決策としてsetthingsデータ型を用意することである:

``` haskell
data Settings = Settings
    { settingsPort :: Int
    , settingsHost :: String
    , settingsTimeout :: Int
    }
```

これにより呼び出しコードをほとんど自己文書化できる:

``` haskell
run Settings
    { settingsPort = 8080
    , settingsHost = "127.0.0.1"
    , settingsTimeout = 30
    } myApp
```

素晴らしい, 明快で, 正しく見えませんか? ええ, しかしウェブサーバに対し50のセッティングがあったらどうであろうか? 全てを毎回指定しなければならないのであろうか? もちろん, そうでない. その代わりに, ウェブサーバはデフォルトのセットを与えるべきである:

``` haskell 
defaultSettings = Settings 3000 "127.0.0.1" 30
```

今や上のような長いコードを書かねばならぬ代わりに, 以下のように済ますことができる:

``` haskell
run defaultSettings { settingsPort = 8080 } myApp -- (1)
```

これは1つの小さな障害を除き素晴らしい. 今`Settings`に対しもう一つのレコードを付け足したすことを決めたとしよう. 外にある次のようなコード:

``` haskell
run (Settings 8080 "127.0.0.1" 30) myApp -- (2)
```

は壊れてしまう. なぜならば, コンストラクタが今や4つの引数を取るためである. 適切なことはメジャーバージョン数を上げ, 依存パッケージがこわれないようにすることである. しかし追加する小さなセッティングのためにメジャーバーションを変更することは不愉快である. 解決策は? `Setting`コンストラクタをエクスポートしないことである:

``` haskell
module MyServer
    ( Settings
    , settingsPort
    , settingsHost
    , settingsTimeout
    , run
    , defaultSettings
    ) where
```

この方法により, 誰も(2)のようなコードは書けない.

この方法の1つの欠点は, 実際にレコード構文を用いてセッティングを変更できることが, Haddockから即座には明確ではないことである. それはこの章における要点である: この技術を使ったライブラリにおいて何が生じているかを明らかにすること.

個人的には, いくつかの場所でこの技術を用いている. 自由にHaddockを見て意味するところを理解してください.

- Warp: Settings

- http-conduit: Request and ManagerSettings

- xml-conduit

- Parsing: ParseSettings

- Rendering: RenderSettings

脱線すると, `http-conduit`と`xml-conduit`は実際に全く新しい識別子を宣言する代わりに, `Default`型クラスのインスタンスを作っている. これは`defaultParserSettings`の代わりに`def`とだけ打てばよいことを意味する. 