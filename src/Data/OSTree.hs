module Data.OSTree
       ( OSTree (..))
       where

-- https://yoichihirai.com/bst.pdf

type Size = Int
data OSTree a = Tip | Bin Size a (OSTree a) (OSTree a)



