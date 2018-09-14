---
title: チートシート
date: 2018/09/14
---

## functions

### head, tail, undefined を禁止する例 (全モジュール)

```yaml
- functions:
  - { name: [ head, tail, undefined ]
    , within: []
    }
```

### (++) を禁止する例 (Bigmoon.Unsafe モジュールは対象外)

```yaml
- functions:
  - { name: ++
    , within: [ Bigmoon.Unsafe ]
    }
```

## modules

### RIO.Text.Partial と RIO.ByteString.Partial を禁止する例 (全モジュール)

```yaml
- modules:
  - { name: [ RIO.Text.Partial, RIO.ByteString.Partial ]
    , within: []
    }
```

### RIO.Partial を禁止する例 (Bigmoon.Unsafe モジュールは対象外)

```yaml
- modules:
  - { name: [ RIO.Partial ]
    , within: [ Bigmoon.Unsafe ]
    }
```

## error

### foldl の代わりに foldl' の利用を促す例

```yaml
- error: { lhs: foldl, rhs: foldl' }
```

### RIO の pack を使っているコードで、 pack (show x) の代わりに tshow の利用を促す例

```yaml
- error: { lhs: RIO.Text.pack (show x), rhs: tshow x }
```

## ignore

### Use . を無視する例

```yaml
- ignore: { name: Use . }
```