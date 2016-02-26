module Data.OSTree
       where

import Prelude hiding (lookup)

-- https://yoichihirai.com/bst.pdf

type Size = Int
data OSTree a = Tip
              | Bin Size a (OSTree a) (OSTree a)
              deriving (Eq,Show)

empty :: OSTree a
empty = Tip

size :: OSTree a -> Size
size Tip = 0
size (Bin sz _ _ _) = sz

singleton :: a -> OSTree a
singleton k = Bin 1 k Tip Tip

bin :: a -> OSTree a -> OSTree a -> OSTree a
bin k l r = Bin (size l + size r + 1) k l r

balanced :: OSTree a -> Bool
balanced Tip = True
balanced (Bin _ _ l r) = isBalanced l r && isBalanced r l
                      && balanced l     && balanced r

count :: OSTree a -> Size
count Tip = 0
count (Bin _ _ l r) = 1 + count l + count r

-- INSERT

insert :: (Ord a) => a -> OSTree a -> OSTree a
insert kx Tip = singleton kx
insert kx t@(Bin sz ky l r) = case compare kx ky of
  LT -> balanceR ky (insert kx l) r
  GT -> balanceL ky l (insert kx r)
  EQ -> t

-- balances

balanceL :: a -> OSTree a -> OSTree a -> OSTree a
balanceL k l r
  | isBalanced l r = bin k l r
  | otherwise      = rotateL k l r

balanceR :: a -> OSTree a -> OSTree a -> OSTree a
balanceR k l r
  | isBalanced r l = bin k l r
  | otherwise      = rotateR k l r

-- rotation L

rotateL :: a -> OSTree a -> OSTree a -> OSTree a
rotateL k l r@(Bin _ _ rl rr)
  | isSingle rl rr = singleL k l r
  | otherwise      = doubleL k l r
rotateL _ _ _      = error "rotateL"

singleL :: a -> OSTree a -> OSTree a -> OSTree a
singleL k1 t1 (Bin _ k2 t2 t3) = bin k2 (bin k1 t1 t2) t3
singleL _ _ _                  = error "singleL"

doubleL :: a -> OSTree a -> OSTree a -> OSTree a
doubleL k1 t1 (Bin _ k2 (Bin _ k3 t2 t3) t4) =
  bin k3 (bin k1 t1 t2) (bin k2 t3 t4)

-- rotation R

rotateR :: a -> OSTree a -> OSTree a -> OSTree a
rotateR k l@(Bin _ _ ll lr) r
  | isSingle lr ll = singleR k l r
  | otherwise      = doubleR k l r
rotateR _ _ _      = error "rotateR"

singleR :: a -> OSTree a -> OSTree a -> OSTree a
singleR k1 (Bin _ k2 t1 t2) t3 = bin k2 t1 (bin k1 t2 t3)
singleR _ _ _                  = error "singleR"

doubleR :: a -> OSTree a -> OSTree a -> OSTree a
doubleR k1 (Bin _ k2 t1 (Bin _ k3 t2 t3)) t4 =
  bin k3 (bin k2 t1 t2) (bin k1 t3 t4)

-- config

isBalanced :: OSTree a -> OSTree a -> Bool
isBalanced a b = 3 * (size a + 1) >= size b + 1

isSingle :: OSTree a -> OSTree a -> Bool
isSingle a b = size a + 1 < 2 * (size b + 1)

-- LOOKUP

lookup :: (Ord a) => a -> OSTree a -> Maybe a
lookup kx Tip = Nothing
lookup kx (Bin _ ky l r) = case compare kx ky of
  LT -> lookup kx l
  GT -> lookup kx r
  EQ -> Just ky

delete :: (Ord a) => a -> OSTree a -> OSTree a
delete k t
  = case t of
      Tip -> Tip
      Bin _ kx l r
          -> case compare k kx of
               LT -> balanceL kx (delete k l) r
               GT -> balanceR kx l (delete k r)
               EQ -> glue l r

glue :: OSTree a -> OSTree a -> OSTree a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let (km,l') = deleteFindMax l in balanceL km l' r
  | otherwise       = let (km,r') = deleteFindMin r in balanceR km l r'

-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map

deleteFindMin :: OSTree a -> (a,OSTree a)
deleteFindMin t
  = case t of
      Bin _ k Tip r -> (k,r)
      Bin _ k l r   -> let (km,l') = deleteFindMin l in (km,balance k l' r)
      Tip           -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
-- > deleteFindMax empty                                      Error: can not return the maximal element of an empty map

deleteFindMax :: OSTree a -> (a,OSTree a)
deleteFindMax t
  = case t of
      Bin _ k l Tip -> (k,l)
      Bin _ k l r   -> let (km,r') = deleteFindMax r in (km,balance k l r')
      Tip           -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)

balance :: a -> OSTree a -> OSTree a -> OSTree a
balance k l r
  | isBalanced l r && isBalanced r l = bin k l r
  | size l > size r                  = rotateR k l r
  | otherwise                        = rotateL k l r

select :: OSTree a -> Int -> a
select = undefined

rank :: a -> OSTree a -> Maybe Int
rank = undefined

