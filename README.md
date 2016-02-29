# Order Statistic Tree

This repository contains an implementation of order statistic tree in Haskell programming language.

I could not find an order statistic tree at Hackage, so I have to develop one.

This implementation uses weight-balanced trees as desribed in
- Hirai, Yoichi, and Kazuhiko Yamamoto. "Balancing weight-balanced trees." Journal of Functional Programming 21.03 (2011): 287-307.

Also some of its code is based on code from containers package.

Implementation of order statistic tree is described in
- Cormen, T.H., Leiserson, Rivest, Stein. Introduction to algorithms. The MIT Press. 3rd ed.

# Installation

This package will be deployed to hackage, so you can install it using cabal:

```
cabal install order-statistic-tree
```

# Building

```
cabal configure
cabal build
```

# Testing

```
cabal configure --enable-tests --enable-benchmarks
cabal test
```

# Benchmarks

I tried to make this tree as fast as possible. I'm not bos, but results on my i7-4790 with 16Gb RAM are following:
- OSTree was created from 1.000.000 random numbers in 2.087 ± 0.021 s (e.g. for Data.Map.Strict - 1.977 ± 0.016 s);
- deletion from OSTree with 1.000.000 random numbers was made in 13.94 ± 0.93 ms;
- lookup from OSTree with 1.000.000 random numbers was made in 208.2 ± 3.48 ns;
- selection from OSTree with 1.000.000 random numbers was made in 92.72 ± 1.91 ns;
- full testing protocol can be found in result-bench.txt.

```
cabal configure --enable-tests --enable-benchmarks
cabal bench
```

If someone knows how to improve these results or benchmarking itself, please don't hesitate to contact me

