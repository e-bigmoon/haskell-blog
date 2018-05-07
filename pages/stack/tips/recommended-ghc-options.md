---
title: ghc-options の推奨設定
date: 2018/05/05
---

## ghc-options の設定例

プロジェクトごとの **ghc-options** は **package.yaml** に以下の内容を記述します。

```yaml
ghc-options:
- -Wall
- -Wcompat
- -Wincomplete-uni-patterns
- -Wincomplete-record-updates
- -Wnoncanonical-monad-instances
- -Wredundant-constraints
- -Wtabs
```