
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.List
import Data.Maybe
import qualified Data.OSTree as M
import Data.OSTree (OSTree(..))

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

(!!?) :: [a] -> Int -> Maybe a
xs !!? k
  | 0 <= k && k < length xs = Just $ xs !! k
  | otherwise               = Nothing

tests :: TestTree
tests = testGroup "Tests"
        [ testGroup "Insertion"
          [ testProperty "insert inserts all elements" $ \inserted notInserted -> let (m, toInsert) = prepareInsert inserted notInserted
                                                                                  in all (isJust . flip M.lookup m) toInsert &&
                                                                                     all (isNothing . flip M.lookup m) notInserted
          , testProperty "insert leave tree balanced" $ \inserted notInserted -> let (m, toInsert) = prepareInsert inserted notInserted
                                                                                 in M.balanced m
          , testProperty "insert leave tree correctly sized" $ \inserted notInserted -> let (m, toInsert) = prepareInsert inserted notInserted
                                                                                        in M.count m === M.size m
          ]
        , testGroup "Deletion"
          [ testProperty "delete deletes" $ \inserted deleted -> let m = prepareDelete inserted deleted
                                                                 in all (isNothing . flip M.lookup m) deleted
          , testProperty "delete leave tree correctly sized" $ \inserted deleted -> let m = prepareDelete inserted deleted
                                                                                    in M.count m === M.size m 
          ]
        , testGroup "Conversion to List"
          [ testProperty "toList correctly converts" $ \inserted -> let m = prepareDelete inserted []
                                                                    in M.toList m === sort (nub inserted)
          , testCase "toList (Bin 1 Tip Tip)" $ M.toList (Bin 1 1 Tip Tip) @?= [1]
          ]
        , testGroup "Conversion from List"
          [ testProperty "toList . fromList == id" $ \list -> M.toList (M.fromList list) === sort (nub (list :: [Int]))
          ]
        , testGroup "Selection"
          [ testProperty "select selects correct item" $ \list k -> M.select (M.fromList (list :: [Int])) (k::Int) === sort (nub list) !!? (k-1)
          ]
        , testGroup "Ranking"
          [ testProperty "rank ranks correctly when item is presented in tree" $ \list k -> (0<=k && k<length list) ==> let
               sorted = sort $ nub list :: [Int]
               item = sorted !! k
               in M.rank item (M.fromList list) === Just k
          , testProperty "rank ranks correctly when item is not presented in tree" $ \list item -> item`notElem`list ==> let
               sorted = sort $ nub list :: [Int]
               in M.rank item (M.fromList list) === Nothing
          ]
        ]

