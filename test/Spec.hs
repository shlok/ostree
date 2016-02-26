
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.List
import Data.Maybe
import qualified Data.OSTree as M
import Data.OSTree (OSTree)

main = defaultMain tests

-- instance Arbitrary OSTree where
--   arbitrary = sized $ \n -> do
--     k <- 
  
--   shrink Tip = []
--   shring (Bin _ k l r) =
--     [Tip] ++
--     [l, r] ++
--     [bin k' l' r' | (k',l',r') <- shrink (k,l,r)]

prepareInsert :: [Int] -> [Int] -> (OSTree Int, [Int])
prepareInsert inserted notInserted = let
  toInsert = nub inserted \\ nub notInserted :: [Int]
  m = foldr M.insert M.empty toInsert
  in (m, toInsert)

prepareDelete :: [Int] -> [Int] -> OSTree Int
prepareDelete inserted deleted = let
  toInsert = nub $ inserted ++ deleted :: [Int]
  m' = foldr M.insert M.empty toInsert
  in foldr M.delete m' deleted

tests :: TestTree
tests = testGroup "Tests"
        [ testGroup "Insertion"
          [ testProperty "Insert inserts all elements" $ \inserted notInserted -> let (m, toInsert) = prepareInsert inserted notInserted
                                                                                  in all (isJust . flip M.lookup m) toInsert &&
                                                                                     all (isNothing . flip M.lookup m) notInserted
          , testProperty "Insert leave tree balanced" $ \inserted notInserted -> let (m, toInsert) = prepareInsert inserted notInserted
                                                                                 in M.balanced m
          , testProperty "Insert leave tree correctly sized" $ \inserted notInserted -> let (m, toInsert) = prepareInsert inserted notInserted
                                                                                        in M.count m === M.size m
          , testProperty "Delete deletes" $ \inserted deleted -> let m = prepareDelete inserted deleted
                                                                 in all (isNothing . flip M.lookup m) deleted
          , testProperty "Delete leave tree correctly sized" $ \inserted deleted -> let m = prepareDelete inserted deleted
                                                                                    in M.count m === M.size m
          ]
        ]

