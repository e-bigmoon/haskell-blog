---
title: config.yaml のよくある設定
date: 2018/05/05
---

## config.yaml について

**config.yaml** はプロジェクトに共通する設定を記述するためにあります。

以下のコマンドで **config.yaml** の保存場所を確認できます。

```shell
$ ls $(stack path --stack-root)/config.yaml
...
```

詳しい設定方法は [Non-project-specific config](https://github.com/commercialhaskell/stack/blob/master/doc/yaml_configuration.md#non-project-specific-config) を参照してください。

## よくある一般的な設定

```yaml
default-template: new-template
templates:
  scm-init: git
  params:
    author-name: Your Name
    author-email: youremail@example.com
    github-username: yourusername
```

**copyright** は省略しておけば自動的に適切な **年** が挿入されるため、カスタマイズする必要が無ければ省略しておくと良いでしょう。

具体例: 以下のように年と **author-name** で設定した値が挿入されます。

> 2018 Your Name

## デフォルトのテンプレートを変更する場合

通常 **stack new** で作られるプロジェクトテンプレートは [new-template](https://github.com/commercialhaskell/stack-templates/blob/master/new-template.hsfiles) です。

これ以外にも [commercialhaskell/stack-templates](https://github.com/commercialhaskell/stack-templates) には数多くのプロジェクトテンプレートがあります。

デフォルトで利用するテンプレートを変更したい場合は **default-template** を指定します。

具体的に [rio](https://github.com/commercialhaskell/stack-templates/blob/master/rio.hsfiles) テンプレートにする場合は **config.yaml** に次の1行を追加します。

```yaml
default-template: rio
```

### プロジェクトテンプレートの確認方法

利用可能なテンプレートに関する情報は[ユーザガイド](https://docs.haskellstack.org/en/stable/GUIDE/#templates)や [stack-templates Wiki](https://github.com/commercialhaskell/stack-templates/wiki) を参照してください。
