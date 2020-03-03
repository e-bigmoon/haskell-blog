---
title: 最小の stack プロジェクト
published: 2018/05/16
# updated: 2020/02/22
---

## stack.yaml

```yaml
resolver: lts-11.9
```

必須項目

- [resolver](https://docs.haskellstack.org/en/stable/yaml_configuration/#resolver)

## package.yaml

```yaml
name: test-proj
dependencies:
  - base
library: {}
```

必須項目

- name
- dependencies
- library