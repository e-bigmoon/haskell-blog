---
title: コマンドリファレンス
---

##### stack build

###### 並列ビルドについて

`stack` では自動的にビルドが並列化される (現在のCPUのコア数を自動的に設定する) ため、明示的に `-j` オプションを渡す必要はありません。(むしろ、現状は間違ったコア数を渡してしまうとパフォーマスに悪影響を及ぼすバグがあるため、非推奨です。)

並列化オプションとして `-jN` が提供されていますが、以下のようにコア数を抑制したい場合にのみ、利用した方が良いでしょう。(`-j` はエラーになります)

```bash
#### 自動的に並列化してビルド
$ stack build

#### コア数1でビルド
$ stack build -j1
```

`stack.yaml` の書式。

```yaml
jobs: 1
```

##### stack hoogle

```bash
# グローバルデータベースの生成
$ stack exec -- hoogle generate
# プロジェクトデータベースの生成
$ stack hoogle
# 検索
$ stack hoogle "<search_text>"
```

##### stack clean

ビルド結果はキャッシュされてしまうので、警告とか見たい時の強制リビルトとかで良く使う。

通常は `.stack-work/dist` 以下のパッケージディレクトリを削除します。パッケージを指定して個別に削除することも可能です。また、`--full` オプションをつけると `.stack-work` ディレクトリが丸ごと削除されます。

```bash
$ stack clean
$ stack clean <package>
$ stack clean --full
```

##### stack exec

`stack exec` は通常、ビルドしたバイナリを実行するために利用します。

```bash
$ stack build
$ stack exec -- <binary>
```

また、あまり知られていませんが `stack exec` コマンドは `shell` で利用できるコマンドがそのまま使えます。

```bash
$ stack exec -- ls
$ stack exec -- env
```

これを少し応用すると、ビルドしたバイナリのパスを簡単に取得することができます。

```bash
$ stack exec -- which <binary>
```

デプロイスクリプトなどで使うと便利です。

##### stack path

基本的にはあまり使いません。
`stack` で問題が発生した際、この内容も教えてあげると解決しやすくなります。

```bash
$ stack path
....
```

##### stack unpack

`Hackage` に登録されているパッケージをローカルに保存します。

```bash
$ stack unpack <package>
```

`git` にソースコードがあれば `git clone` で良いのですが、古いパッケージだと `Hackage` に登録されているソースコードしかない場合もあるため、そのような場合に使います。

自分で `Hackage` からパッケージをダウンロードして `tar.gz` を展開することと同じですが、こっちの方が楽です。

##### stack test

##### stack test --test-arguments 

テスト時に引数を渡したい場合に使います。
プロファイルのオプションを指定する際や、 `tasty-html` などで使う場合があります。

```bash
# 書式
$ stack test --test-arguments="<option>"

# 実際の使い方
$ stack test --profile --test-arguments "+RTS -hm"
$ stack test --test-arguments="--html results.html"
```

##### stack templates

利用可能なプロジェクトテンプレートの一覧をリストアップするコマンド。

```bash
$ stack templates
Template                    Description
chrisdone
foundation                - Project based on an alternative prelude with batteries and no dependencies.
franklinchen
ghcjs                     - Haskell to JavaScript compiler, based on GHC
ghcjs-old-base
hakyll-template           - a static website compiler library
haskeleton                - a project skeleton for Haskell packages
hspec                     - a testing framework for Haskell inspired by the Ruby library RSpec
new-template
protolude                 - Project using a custom Prelude based on the Protolude library
quickcheck-test-framework - a library for random testing of program properties
readme-lhs                - small scale, quick start, literate haskell projects
rubik
scotty-hello-world
scotty-hspec-wai
servant                   - a set of packages for declaring web APIs at the type-level
servant-docker
simple
simple-hpack
simple-library
spock                     - a lightweight web framework
tasty-discover            - a project with tasty-discover with setup
tasty-travis
unicode-syntax-exe
unicode-syntax-lib
yesod-minimal
yesod-mongo
yesod-mysql
yesod-postgres
yesod-postgres-fay
yesod-simple
yesod-sqlite
```

##### stack build

###### 並列ビルドについて

`stack` では自動的にビルドが並列化される (現在のCPUのコア数を自動的に設定する) ため、明示的に `-j` オプションを渡す必要はありません。(むしろ、現状は間違ったコア数を渡してしまうとパフォーマスに悪影響を及ぼすバグがあるため、非推奨です。)

並列化オプションとして `-jN` が提供されていますが、以下のようにコア数を抑制したい場合にのみ、利用した方が良いでしょう。(`-j` はエラーになります)

```bash
#### 自動的に並列化してビルド
$ stack build

#### コア数1でビルド
$ stack build -j1
```

`stack.yaml` の書式。

```yaml
jobs: 1
```

##### stack hoogle

```bash
# グローバルデータベースの生成
$ stack exec -- hoogle generate
# プロジェクトデータベースの生成
$ stack hoogle
# 検索
$ stack hoogle "<search_text>"
```

##### stack clean

ビルド結果はキャッシュされてしまうので、警告とか見たい時の強制リビルトとかで良く使う。

通常は `.stack-work/dist` 以下のパッケージディレクトリを削除します。パッケージを指定して個別に削除することも可能です。また、`--full` オプションをつけると `.stack-work` ディレクトリが丸ごと削除されます。

```bash
$ stack clean
$ stack clean <package>
$ stack clean --full
```

##### stack exec

`stack exec` は通常、ビルドしたバイナリを実行するために利用します。

```bash
$ stack build
$ stack exec -- <binary>
```

また、あまり知られていませんが `stack exec` コマンドは `shell` で利用できるコマンドがそのまま使えます。

```bash
$ stack exec -- ls
$ stack exec -- env
```

これを少し応用すると、ビルドしたバイナリのパスを簡単に取得することができます。

```bash
$ stack exec -- which <binary>
```

デプロイスクリプトなどで使うと便利です。

##### stack path

基本的にはあまり使いません。
`stack` で問題が発生した際、この内容も教えてあげると解決しやすくなります。

```bash
$ stack path
....
```

##### stack unpack

`Hackage` に登録されているパッケージをローカルに保存します。

```bash
$ stack unpack <package>
```

`git` にソースコードがあれば `git clone` で良いのですが、古いパッケージだと `Hackage` に登録されているソースコードしかない場合もあるため、そのような場合に使います。

自分で `Hackage` からパッケージをダウンロードして `tar.gz` を展開することと同じですが、こっちの方が楽です。

##### stack test

##### stack test --test-arguments 

テスト時に引数を渡したい場合に使います。
プロファイルのオプションを指定する際や、 `tasty-html` などで使う場合があります。

```bash
# 書式
$ stack test --test-arguments="<option>"

# 実際の使い方
$ stack test --profile --test-arguments "+RTS -hm"
$ stack test --test-arguments="--html results.html"
```

##### stack templates

利用可能なプロジェクトテンプレートの一覧をリストアップするコマンド。

```bash
$ stack templates
Template                    Description
chrisdone
foundation                - Project based on an alternative prelude with batteries and no dependencies.
franklinchen
ghcjs                     - Haskell to JavaScript compiler, based on GHC
ghcjs-old-base
hakyll-template           - a static website compiler library
haskeleton                - a project skeleton for Haskell packages
hspec                     - a testing framework for Haskell inspired by the Ruby library RSpec
new-template
protolude                 - Project using a custom Prelude based on the Protolude library
quickcheck-test-framework - a library for random testing of program properties
readme-lhs                - small scale, quick start, literate haskell projects
rubik
scotty-hello-world
scotty-hspec-wai
servant                   - a set of packages for declaring web APIs at the type-level
servant-docker
simple
simple-hpack
simple-library
spock                     - a lightweight web framework
tasty-discover            - a project with tasty-discover with setup
tasty-travis
unicode-syntax-exe
unicode-syntax-lib
yesod-minimal
yesod-mongo
yesod-mysql
yesod-postgres
yesod-postgres-fay
yesod-simple
yesod-sqlite
```
