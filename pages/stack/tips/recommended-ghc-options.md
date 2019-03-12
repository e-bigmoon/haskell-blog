---
title: ghc-options の推奨設定
date: 2019/03/12
---

## ghc-options の設定例

プロジェクトごとの **ghc-options** は **package.yaml** に以下の内容を記述します。

```yaml
ghc-options:
- -Wall
- -Wcompat
- -Widentities
- -Wincomplete-uni-patterns
- -Wincomplete-record-updates
- -Wnoncanonical-monad-instances
- -Wpartial-fields
- -Wredundant-constraints
- -Wtabs
```
