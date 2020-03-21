---
title: stack hoogle
published: 2018/07/30
# updated: 2019/09/15
---

## よく使うコマンド

グローバルデータベースの生成

```shell
$ stack exec -- hoogle generate
```

プロジェクトデータベースの生成

```shell
$ stack hoogle
```

生成した hoogle データベースを使って検索

```shel
$ stack hoogle "<search_text>"
```