---
title: 最近遭遇した stack の ld (pthread, -fPIC) エラー
author: Shinya Yamaguchi
tags: bigmoon, stack
---

## はじめに

ここ最近、プロジェクトのビルド時に謎のエラーが発生するようになりました。

以下の2種類のエラーに遭遇したので、今後のために記録しておきます。

- `pthread link error`
- `-fPIC error`

```shell
$ stack --version
Version 1.7.0, Git revision 19e3460496f8fd2c462fb35a5825301e5c3c4eb0 (5527 commits) x86_64 hpack-0.20.0
```

<!--more-->

## pthread link error

このエラーは `Hakyll` を利用しているサイトをビルドしている時に発生したものです。

関連する `issue` は以下の通りです。

- [pthread link errors on linux with GHC 8.2 #4130](https://github.com/jgm/pandoc/issues/4130)
- [Error building on Ubuntu 16 #311](https://github.com/jgm/pandoc-citeproc/issues/311)

`issue` に掲載されているエラーメッセージですが、以下のように `pthread` 系のエラーが表示され、結果として `gcc failed in phase Linker. (Exit code: 1)` となります。

```shell
    Linking .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/pandoc-citeproc/pandoc-citeproc ...

    /tmp/stack11451/pandoc-citeproc-0.12.1.1/rts/posix/OSThreads.c:137:0: error:
         error: undefined reference to 'pthread_create'

    /tmp/stack11451/pandoc-citeproc-0.12.1.1/rts/posix/OSThreads.c:139:0: error:
         error: undefined reference to 'pthread_detach'

    /tmp/stack11451/pandoc-citeproc-0.12.1.1/rts/posix/OSThreads.c:141:0: error:
         error: undefined reference to 'pthread_setname_np'

    /tmp/stack11451/pandoc-citeproc-0.12.1.1/rts/posix/OSThreads.c:184:0: error:
         error: undefined reference to 'pthread_key_create'

    /tmp/stack11451/pandoc-citeproc-0.12.1.1/rts/posix/OSThreads.c:192:0: error:
         error: undefined reference to 'pthread_getspecific'

    /tmp/stack11451/pandoc-citeproc-0.12.1.1/rts/posix/OSThreads.c:203:0: error:
         error: undefined reference to 'pthread_setspecific'

    /tmp/stack11451/pandoc-citeproc-0.12.1.1/rts/posix/OSThreads.c:212:0: error:
         error: undefined reference to 'pthread_key_delete'

    /tmp/stack11451/pandoc-citeproc-0.12.1.1/rts/posix/OSThreads.c:371:0: error:
         error: undefined reference to 'pthread_kill'

    /tmp/stack11451/pandoc-citeproc-0.12.1.1/includes/rts/OSThreads.h:59:0: error:
         error: undefined reference to 'pthread_mutex_trylock'
    collect2: error: ld returned 1 exit status
    `gcc' failed in phase `Linker'. (Exit code: 1)
```

修正方法は簡単で `package.yaml` や `cabal` ファイルの `ghc-options` に `-threaded` を追記するだけです。

具体的にはこんな感じで修正しました。

- [Added -threaded to ghc-options. pthread link errors on linux with GHC 8.2 #601](https://github.com/jaspervdj/hakyll/pull/601/files)

なぜこのエラーが発生したのかはわからないのですが `lts-10` 系にしたタイミングで遭遇しました。

## -fPIC error

`Mac` では確認できなかったのですが `Ubuntu 17.10` で以下のエラーに遭遇しました。

```shell
    /usr/bin/ld: /home/bm12/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/rts/libHSrts_thr.a(Arena.thr_o): relocation R_X86_64_32 against .rodata.str1.1 can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
    /usr/bin/ld: /home/bm12/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/rts/libCffi.a(closures.o): relocation R_X86_64_32 against .rodata can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
    /usr/bin/ld: /home/bm12/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/rts/libCffi.a(ffi64.o): relocation R_X86_64_32S against .rodata can not be used when making a shared object。 -fPIC を付けて再コンパイルしてください。
    /usr/bin/ld: 最終リンクに失敗しました: 出力に対応するセクションがありません
    collect2: error: ld returned 1 exit status
    gcc failed in phase Linker. (Exit code: 1)
```

`stack` の `issue` でも話題になってました。

- [Stack 1.6 linking issues on Arch Linux #3518](https://github.com/commercialhaskell/stack/issues/3518)
- [Linker error makes it impossible to use a stack-provided ghc #2712](https://github.com/commercialhaskell/stack/issues/2712)
- [Benchmarks can no longer be built with Stack 1.6.1 #3630](https://github.com/commercialhaskell/stack/issues/3630)
- [ghc-tinfo6-nopie-8.2.2 requires changes to its settings file to work on Arch Linux #3648](https://github.com/commercialhaskell/stack/issues/3648)

解決策がまとまり `faq` に追加されたので、同じエラーで悩んでいる人は一度ご確認ください。

- [faq update: ld errors about recompiling with -fPIC #3725](https://github.com/commercialhaskell/stack/pull/3725)

解決方法がとても簡単で、以下のコマンドを実行して `ghc` を再インストールするだけです。

```shell
$ stack setup --reinstall
```

## まとめ

何もしていなくても突然ビルドできなくなることもあるんですね・・・。
