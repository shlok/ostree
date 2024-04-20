{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Data.OSTree
-- Description : Order Statistic Tree
-- Copyright   : (c) Lambda Kazan, 2016
-- License     : BSD3
-- Maintainer  : mz@lambdasoft.ru
-- Stability   : experimental
-- Portability : POSIX
--
-- = Order Statistic Tree
--
-- This implementation uses weight-balanced trees which are desribed in
--
-- - Hirai, Yoichi, and Kazuhiko Yamamoto. "Balancing weight-balanced trees." Journal of Functional Programming 21.03 (2011): 287-307.
--
-- Also some of its code is based on containers package.
--
-- Implementation of order statistic tree is described in
--
-- - Cormen, T.H., Leiserson, Rivest, Stein. Introduction to algorithms. The MIT Press. 3rd ed.
--
-- = Benchmarks
--
-- I tried to make this tree as fast as possible. I'm not bos, but results on my i7-4790 with 16Gb RAM are following:
--
--  * OSTree was created from 1.000.000 random numbers in 2.087 ± 0.021 s (e.g. for Data.Map.Strict - 1.977 ± 0.016 s);
--  * deletion from OSTree with 1.000.000 random numbers was made in 13.94 ± 0.93 ms;
--  * lookup from OSTree with 1.000.000 random numbers was made in 208.2 ± 3.48 ns;
--  * selection from OSTree with 1.000.000 random numbers was made in 92.72 ± 1.91 ns;
--  * full testing protocol can be found in result-bench.txt.
--
-- @
-- cabal configure --enable-tests --enable-benchmarks
-- cabal bench
-- @
--
-- If someone knows how to improve these results or benchmarking itself, please don't hesitate to contact me
module Data.OSTree
  ( OSTree,

    -- * Creating OSTree
    empty,
    singleton,

    -- * Search Tree operations
    size,
    insert,
    lookup,
    delete,
    deleteFindMin,
    deleteFindMax,

    -- * Conversions
    toList,
    fromList,

    -- * Statistics
    select,

    -- * Rank
    rank,
    rank',
  )
where

import Data.List (foldl')
import Data.OSTree.Internal
import Data.OSTree.Types
import Prelude hiding (lookup)

-- https://yoichihirai.com/bst.pdf

-- | /O(1)/. Returns an empty tree
empty :: OSTree a
empty = Tip

-- | /O(1)/. Returns a tree with single element
singleton :: a -> OSTree a
singleton k = Bin 1 k Tip Tip

-- | /O(log n)/. Insert the element into the tree
insert :: (Ord a) => a -> OSTree a -> OSTree a
insert kx Tip = singleton kx
insert kx t@(Bin sz ky l r) = case compare kx ky of
  LT -> balanceR ky (insert kx l) r
  GT -> balanceL ky l (insert kx r)
  EQ -> balanceR ky l (insert kx r)

-- | /O(log n)/. Lookup the element in the tree
lookup :: (Ord a) => a -> OSTree a -> Maybe a
lookup kx Tip = Nothing
lookup kx (Bin _ ky l r) = case compare kx ky of
  LT -> lookup kx l
  GT -> lookup kx r
  EQ -> Just ky

-- | /O(log n)/. Delete first occurence of the element from the tree
delete :: (Ord a) => a -> OSTree a -> OSTree a
delete k t =
  case t of
    Tip -> Tip
    Bin _ kx l r ->
      case compare k kx of
        LT -> balanceL kx (delete k l) r
        GT -> balanceR kx l (delete k r)
        EQ -> glue l r

-- | /O(n)/. Return list of elements of the tree
toList :: OSTree a -> [a]
toList k = toListL k []

toListL :: OSTree a -> [a] -> [a]
toListL Tip = id
toListL (Bin _ k l r) = toListL l . (k :) . toListL r

-- | /O(n log n)/. Convert list of elements to the tree
fromList :: (Ord a) => [a] -> OSTree a
fromList = foldl' (flip insert) empty

-- | /O(log n)/. Returns i-th least element of the tree
select ::
  -- | tree
  OSTree a ->
  -- | index 'i', starting from 1
  Int ->
  -- | if there are at least 'i' elements, returns i-th least element
  Maybe a
select Tip i = Nothing
select (Bin _ k l r) i =
  let n = size l + 1
   in case compare i n of
        EQ -> Just k
        LT -> select l i
        GT -> select r $ i - n

-- | /O(log n)/. If the given element is in the tree, returns its rank; returns 'Nothing' otherwise.
rank ::
  (Ord a) =>
  OSTree a ->
  -- | A given element.
  a ->
  Maybe Int
rank t a =
  go 0 False t
  where
    -- + sum': Number of elements already (in earlier iterations) identified to be less than the
    --   given element a.
    -- + found': Whether the given element a was already found (in earlier iterations).
    go !sum' !found' = \case
      Tip ->
        if found'
          then Just sum'
          else Nothing
      Bin _ k l r ->
        case compare a k of
          EQ ->
            -- The given element a is equal to k (and potentially equal to other elements in l).
            go sum' True l
          LT ->
            -- The given element a is less than k (and all elements in r).
            go sum' found' l
          GT ->
            -- The given element a is greater than k (and all elements in l).
            go (sum' + size l + 1) found' r

-- | /O(log n)/. An alternative implementation of 'rank'. Less performant (more thunking), but
-- possibly simpler to understand for programmers.
rank' ::
  (Ord a) =>
  OSTree a ->
  -- | A given element.
  a ->
  Maybe Int
rank' Tip _ = Nothing
rank' (Bin _ k l r) a =
  case compare a k of
    EQ ->
      -- The given element a is equal to k (and potentially equal to other elements in l).
      case rank' l a of
        Nothing -> Just $ size l
        Just x -> Just x
    LT ->
      -- The given element a is less than k (and all elements in r).
      rank' l a
    GT ->
      -- The given element a is greater than k (and all elements in l).
      (size l + 1 +) <$> rank' r a
