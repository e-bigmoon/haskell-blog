---
title: Haskell Development
date: 2019/09/14
---

<div class="row">
  <div class="col s12 m6">

## Tips

- [完全なリビルド](tips/full-rebuild.html)
- [おすすめの開発方法](tips/recommend-dev.html)
- [script interpreter + stack script でスクリプティング！](tips/script-interpreter.html)
- [config.yaml のよくある設定](tips/config-yaml.html)
- [ghc-options の推奨設定](tips/recommended-ghc-options.html)
- [HDD の容量が少なくなってきた時](tips/hdd-space.html)
- [最小のプロジェクト](tips/minimal-stack-proj.html)
- [ファイル単位で ghc-options を指定する方法](tips/enable-ghc-options-by-file.html)
- [カスタムスナップショットの紹介](/posts/2017/12-23-stack161.html)
- [namespaced templates](/posts/2018/06-27-namespaced-templates.html)
- [プロファイルの取得方法](etc/profiling.html)
- [extra-deps に github の短縮形が指定できるようになります](/posts/2018/03-13-stack-extra-deps-shorthand.html)
- [stack-1.11 から location に extra-dep を指定できなくなります](/posts/2018/08-31-stack-extradep-legacy-syntax.html)
- [アプリケーションのバックトレースを取得する](/posts/2018/09-01-stack-profile-build.html)
- [cabal コマンドとの対応表](tips/cabal.html)

### stack サブコマンド

- [stack build](command/build.html)
- [stack test](command/test.html)
- [stack run](/posts/2018/06-25-stack-run.html)
- [stack exec](command/exec.html)
- [stack clean](command/clean.html)
- [stack ls](/posts/2017/12-20-stack-ls-command.html)
- [stack hoogle](command/hoogle.html)
- [stack path](command/path.html)
- [stack unpack](command/unpack.html)
- [stack templates](command/templates.html)
- [stack image container](command/image-container.html)

### stack リリース解説

- [stack v1.9.3](/posts/2018/12-10-stack-193.html)
- [stack v1.9.1](/posts/2018/10-22-stack-191.html)
- [stack v1.7.1](/posts/2018/05-04-stack171.html)
- [stack v1.6.5](/posts/2018/02-21-stack165.html)
- [stack v1.6.3](/posts/2017/12-24-stack163.html)

## HLint

- [はじめに](hlint/)
- [HLint の導入と実行](hlint/hlint-intro.html)
- [HLint のヒント](hlint/hlint-hint.html)
- [HLint のカスタムヒント](hlint/hlint-customhint.html)
- [プロジェクトで禁止している関数の検出](hlint/forbidden-functions.html)
- [HLint のヒントを無視する方法](hlint/hlint-ignore.html)
- [CI を回す!](hlint/hlint-ci.html)
- [チートシート](hlint/cheatsheet.html)

## Tools

- [stylish-haskell](etc/stylish-haskell.html)
- [Travis CI](etc/travis-ci.html)
- [Circle CI 2.0](/posts/2018/07-21-circleci-2.html)

## その他

- [hoogle](etc/hoogle.html)

## wip

_ | stack | cabal
-----|------|--------
更新 | [stack upgrade](tips/stack-upgrade.html) | [ghcup, cabal, stack](tips/cabal-upgrade.html)
バージョン確認 | [stack --version](tips/stack-version.html) | [cabal -V](tips/cabal-version.html)
削除 | [stack clean --full](tips/stack-uninstall.html)

  </div>
  <div class="col s12 m6">

## チュートリアル

- [イントロダクション](intro/)
  - [なぜ stack を使うのか？](intro/why-stack.html)
  - [Stackage とは何か？](intro/stackage.html)
  - [stack のインストールと設定](intro/stack-install.html)
  - [hpack について](intro/hpack.html)
  - [stack 以外の選択肢について](intro/alt-stack.html)
- [プロジェクトの作成](intro/create-prj.html)
- [ライブラリの作成](intro/create-lib.html)
  - [repl 環境の使い方](intro/repl.html)
  - [GHC について](intro/ghc.html)
  - [パッケージと依存関係](intro/package-and-deps.html)
  - [extra-deps の指定方法](intro/extra-deps.html)
- [アプリケーションの作成](intro/create-app.html)
- [ドキュメントの作成](doc/)
  - [haddockの基礎知識](doc/haddock-intro.html)
  - [haddockコメント形式](doc/haddock-comment.html)
  - [haddockのための設定](doc/haddock-settings.html)
- [テストの作成](test/)
  - [HSpec](test/hspec.html)
  - [QuickCheck](test/quickcheck.html)
  - [doctest](test/doctest.html)

## プラクティス

- [Haskell で暗号学的ハッシュを扱う (翻訳)](/posts/2017/09-18-cryptographic-hashing-haskell.html)
- [将来も使えるテストスイート (翻訳)](/posts/2017/12-22-future-proofing-test-suites.html)
- [Haskell のパフォーマンスをデバッグする (翻訳)](/posts/2017/12-27-Haskell-Performance-Debugging.html)
- [travis-ci の初回ビルドで OUT OF MEMORY が出た時の対処法](/posts/2017/12-31-travis-out-of-memory.html)
- [Pattern Synonyms で DEPRECATED](/posts/2018/02-12-pattern-synonyms.html)
- [Prelude を カスタムPrelude で置き換える](/posts/2018/05-23-extended-prelude.html)
- [wizard モノイド (翻訳)](/posts/2018/03-07-The-wizard-monoid.html)
- [アプリケーションのバージョンに Git の情報を出してみよう！](/posts/2018/03-20-gitrev.html)
- [正格性のすべて (翻訳)](/posts/2018/06-25-All-About-Strictness.html)
- [ContT を使ってコードを綺麗にしよう！](/posts/2018/06-26-cont-param.html)
- [RecordWildCards と Reader モナド](/posts/2018/08-26-recordwildcards.html)
- [Haskell 情報収集術 - Qiita](https://qiita.com/waddlaw/items/b7ed253db36c6f8a04fc)
- [GHC Source Plugin 作ってみた - Qiita](https://qiita.com/waddlaw/items/65b57517f105fcbbe724)
- [Monoid と DerivingVia - Qiita](https://qiita.com/waddlaw/items/f349bd363963d59e9ef5)
- [RankNTypes と型レベルリストと extensible - Qiita](https://qiita.com/waddlaw/items/b49e9daa02b2d254fba3)

## エディタ

- [VS Code で Ghcid を使う](/posts/2017/12-24-Ghcid-with-VS-Code.html)
- [Emacs で Haskell IDE Engine を使う](/hie/emacs.html)
- [VS Code と haskell-ide-engine で Haskell 開発環境を構築する - Qiita](https://qiita.com/waddlaw/items/b83cd10311200095fe87)

## リンク

- Haskell
  - [haskell.org](https://www.haskell.org/)
  - [haskell wiki](https://wiki.haskell.org/Haskell)
  - [Try Haskell](http://tryhaskell.org/)
- GHC (Glasgow Haskell Compiler)
  - [Glasgow Haskell Compiler User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)
  - [GHC Developer Wiki](https://ghc.haskell.org/trac/ghc/)
  - [ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals)
  - [Glasgow Haskell Compiler - GitLab](https://gitlab.haskell.org/ghc/ghc)
  - [Glasgow Haskell Compiler - GitHub](https://github.com/ghc/ghc)
- パッケージ
  - [Hackage](https://hackage.haskell.org/)
  - [Stackage](https://www.stackage.org/)
  - [Hackage Dependency Monitor](http://packdeps.haskellers.com/)
  - [Hackage Matrix Builder 3rd](https://matrix.hackage.haskell.org/)
  - [hdiff](http://hdiff.luite.com/)
- 型検索
  - [Hoogle](https://hoogle.haskell.org/)
- スタイルガイド
  - [tweag/guides](https://github.com/tweag/guides)
  - [input-output-hk/cardano-sl](https://github.com/input-output-hk/cardano-sl/blob/develop/docs/style-guide.md)
  - [serokell/serokell-util](https://github.com/serokell/serokell-util/blob/master/serokell-style.md)
  - [kowainik](https://kowainik.github.io/posts/2019-02-06-style-guide)
- Haskell 情報収集
  - [Haskell News](http://haskellnews.org/)
  - [Haskell Weekly](https://haskellweekly.news/)
  - [planet haskell](https://planet.haskell.org/)
  - [Haskell mailing list](http://haskell.1045720.n5.nabble.com/)
  - [r/haskell/ - reddit](https://www.reddit.com/r/haskell/)
- コミュニティ
  - [Haskell-jp](https://haskell.jp/)
- 学習
  - [wikibooks](https://en.wikibooks.org/wiki/Haskell)
  - [FP Complete's Haskell Homepage](https://haskell.fpcomplete.com/)
- [その他](etc/links.html)

  </div>
</div>
