---
title: stack clean
published: 2018/07/30
# updated: 2019/09/15
---

## よく使うコマンド

ビルド結果はキャッシュされてしまうので、警告とか見たい時の強制リビルトとかで良く利用します。

`.stack-work/dist` ディレクトリの削除。

```shell
$ stack clean
```

`.stack-work/dist` に保存されているパッケージを指定して削除。

```shell
$ stack clean <package>
```

プロジェクトの `.stack-work` ディレクトリを削除。

```shell
$ stack clean --full
```

## 削除についての関連する情報

- [完全なリビルド](tips/full-rebuild.html)
- [Stack の削除](tips/stack-uninstall.html)