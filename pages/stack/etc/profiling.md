---
title: プロファイリング
---

```shell-session
$ stack build --profile
$ stack exec -- <bin_name> +RTS -p -hc

# テストのプロファイリング
$ stack test --profile --test-arguments "+RTS -hm"

$ stack exec -- hp2ps -e8in -c <proj_name>.hp
```