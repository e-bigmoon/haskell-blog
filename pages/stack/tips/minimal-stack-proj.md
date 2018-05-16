---
title: 最小の stack プロジェクト
date: 2018/05/16
---

## stack.yaml

### 必須項目

- [resolver](https://docs.haskellstack.org/en/stable/yaml_configuration/#resolver)

### サンプル

```yaml
resolver: lts-11.9
```

## package.yaml

### 必須項目

- name
- dependencies
- library

### サンプル

```yaml
name: test-proj
dependencies:
  - base
library: {}
```