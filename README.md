[![Build Status](https://travis-ci.org/e-bigmoon/haskell-blog.svg?branch=master)](https://travis-ci.org/e-bigmoon/haskell-blog)
[![CircleCI](https://circleci.com/gh/e-bigmoon/haskell-blog.svg?style=svg)](https://circleci.com/gh/e-bigmoon/haskell-blog)

## Usage
### Pages
Add a new page in the `pages` folder.


### Posts

Each blog post **must** have a title and a teaser - read more about teasers in
[this tutorial](https://jaspervdj.be/hakyll/tutorials/using-teasers-in-hakyll.html) -
and can have zero or more tags and an author:

```
---
title: Haskell で暗号学的ハッシュを扱う
author: Michael Snoyman
translator: pythonissam
tags: fpcomplete
---

Great original post: [CRYPTOGRAPHIC HASHING IN HASKELL.](https://www.fpcomplete.com/blog/2017/09/cryptographic-hashing-haskell).

Fusce tortor quam, egestas in posuere quis, porttitor vel turpis...

<!--more-->

Proin vulputate sapien facilisis leo ornare pulvinar...
```

### Build on Windows

can't build `hakyll-sass`.
So, build css on local using `sass`.

```
$ sass -I sass --scss ./css/main.scss ./css/main.css
```
