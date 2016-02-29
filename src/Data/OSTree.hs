{-|
Module      : Data.OSTree
Description : Order statistic tree
Copyright   : (c) Lambda Kazan, 2016
License     : BSD3
Maintainer  : mz@lambdasoft.ru
Stability   : experimental
Portability : POSIX


-}
module Data.OSTree
       ( OSTree
       , empty
       , singleton
       , size
       , insert
       , lookup
       , delete
       , toList
       , fromList
       , select
       )
       where

import Prelude hiding (lookup)
import Data.List (foldl')

import Data.OSTree.Types
import Data.OSTree.Internal

-- https://yoichihirai.com/bst.pdf

-- | Returns an empty tree
empty :: OSTree a
empty = Tip

-- | Returns a tree with single element
singleton :: a -> OSTree a
singleton k = Bin 1 k Tip Tip

-- | Insert the element into the tree
insert :: (Ord a) => a -> OSTree a -> OSTree a
insert kx Tip = singleton kx
insert kx t@(Bin sz ky l r) = case compare kx ky of
  LT -> balanceR ky (insert kx l) r
  GT -> balanceL ky l (insert kx r)
  EQ -> balanceR ky l (insert kx r)

-- | Lookup the element in the tree
lookup :: (Ord a) => a -> OSTree a -> Maybe a
lookup kx Tip = Nothing
lookup kx (Bin _ ky l r) = case compare kx ky of
  LT -> lookup kx l
  GT -> lookup kx r
  EQ -> Just ky

-- | Delete first occurence of the element from the tree
delete :: (Ord a) => a -> OSTree a -> OSTree a
delete k t
  = case t of
      Tip -> Tip
      Bin _ kx l r
          -> case compare k kx of
               LT -> balanceL kx (delete k l) r
               GT -> balanceR kx l (delete k r)
               EQ -> glue l r

-- | Return list of elements of the tree
toList :: OSTree a -> [a]
toList k = toListL k []

toListL :: OSTree a -> [a] -> [a]
toListL Tip = id
toListL (Bin _ k l r) = toListL l . (k:) . toListL r

-- | Convert list of elements to the tree
fromList :: (Ord a) => [a] -> OSTree a
fromList = foldl' (flip insert) empty

-- | Returns i-th least element of the tree
select :: OSTree a              -- ^ tree
       -> Int                   -- ^ 'i', starting from 1
       -> Maybe a               -- ^ if there are at least 'i' elements, return
select Tip i = Nothing
select (Bin _ k l r) i = let
  n = size l + 1
  in case compare i n of
    EQ -> Just k
    LT -> select l i
    GT -> select r $ i-n


