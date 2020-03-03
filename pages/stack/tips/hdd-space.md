---
title: HDD の容量が少なくなってきた時
published: 2018/05/16
# updated: 2020/02/22
---

## 良くある事例

**stack** を長く使っていると、いつの間にか `~/.stack` が肥大化しており **100GB** 以上の容量になっていることは珍しくありません。

原因としては、GHC のバイナリやスナップショットごとに依存関係のためダウンロードしたパッケージなどが徐々に蓄積されるために起こります。

## 確認方法

自分の環境でどのぐらいの容量を消費しているかというのは、例えば次のようなコマンドで確認できます。

```shell
$ du -sh $(stack path --stack-root)/* --total
671M    /home/bm12/.stack/build-plan
49M     /home/bm12/.stack/build-plan-cache
4.0K    /home/bm12/.stack/config.yaml
12K     /home/bm12/.stack/custom-plan
20K     /home/bm12/.stack/docker.db
0       /home/bm12/.stack/docker.db.lock
270M    /home/bm12/.stack/global-project
1.3G    /home/bm12/.stack/indices
507M    /home/bm12/.stack/loaded-snapshot-cache
8.1M    /home/bm12/.stack/precompiled
7.9G    /home/bm12/.stack/programs
9.4M    /home/bm12/.stack/script
67M     /home/bm12/.stack/setup-exe-cache
60K     /home/bm12/.stack/setup-exe-src
9.7G    /home/bm12/.stack/snapshots
916K    /home/bm12/.stack/templates
21G     total
```

合計で **21GB** もあるんですね・・・。

また、この結果を見ると以下の3つは特に容量を食っています。

1. snapshots (9.7 GB)
1. programs (7.9 GB)
1. indices (1.3 GB)

## 解決策

簡単なのは、ディレクトリごと削除してしまうことです。

デメリットとしては、もう一度 **GHC** のダウンロードや snapshot で指定したパッケージたちをビルドする必要があるので時間がかかりますが、それだけです。

一番良いのは、プロジェクトで利用している **GHC** とスナップショットのディレクトリのみを残して、それ以外を削除することだと思います。