# NewBase60, Haskell Edition

[![Tests](https://github.com/astralbijection/newbase60-hs/actions/workflows/tests.yml/badge.svg)](https://github.com/astralbijection/newbase60-hs/actions/workflows/tests.yml)
[![Hackage](https://img.shields.io/hackage/v/newbase60.svg)](https://hackage.haskell.org/package/newbase60)

This package includes an implementation of [Tantek Çelik's New Base 60](http://tantek.pbworks.com/w/page/19402946/NewBase60) number system.

## Examples

```haskell
import Data.String.NewBase60 ( numToSxg, sxgToNum )

-- It converts numbers into base 60
numToSxg 60 -- "10"
numToSxg 1337 -- "NH"

-- Converting to num filters out invalid characters
sxgToNum "1#O" -- 60
sxgToNum "NH*" -- 1337
```
