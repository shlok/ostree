{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Criterion.Main
import Control.Arrow
import Control.DeepSeq (NFData)
import Control.Monad
import System.Random

import qualified Data.Set as Set
import qualified Data.OSTree as M
import Data.OSTree (OSTree)

instance NFData a => NFData (OSTree a)

type Env = [Int]

sortedItems :: Int -> IO Env
sortedItems n = do
  return [1000..1000+n]

revSortedItems :: Int -> IO Env
revSortedItems = (reverse <$>) . sortedItems

randomItems :: Int -> IO Env
randomItems n = replicateM n randomIO

tree :: IO Env -> IO (OSTree Int, Int)
tree items = do
  tree <- M.fromList <$> items
  item <- (`mod` M.size tree) <$> randomIO
  return (tree,item)

allKs = [10000,20000,50000,100000,200000,1000000]

envs :: [(String, Int -> IO Env)]
envs = [ ("Sorted", sortedItems), ("Reverse", revSortedItems), ("Random", randomItems) ]

envs' :: [(String, Int -> IO (OSTree Int, Int))]
envs' = map (second (tree.)) envs

onAllEnv :: (NFData a) =>
            [(String, Int -> IO a)] ->
            [Int] ->
            (String -> a -> Benchmark) ->
            [Benchmark]
onAllEnv envs ks test = [ bgroup (show k)
                          [ env (getEnv k) $ \ ~environ ->
                             test name environ
                          | (name, getEnv) <- envs]
                        | k <- ks ]

main = defaultMain
       [ bgroup "Insertions" $ onAllEnv envs  allKs insertions
       , bgroup "Deletions"  $ onAllEnv envs' allKs deletions
       , bgroup "Lookups"    $ onAllEnv envs' allKs lookups
       , bgroup "Selections" $ onAllEnv envs' allKs selections
       , bgroup "Data.Map.Strict.insert" $ onAllEnv envs [last allKs] strictMapInsert
       ]


insertions :: String -> Env -> Benchmark
insertions name ~environ = bench name $ nf M.fromList environ

deletions :: String -> (OSTree Int, Int) -> Benchmark
deletions name ~(tree,item) = bench name $ nf (M.delete item) tree

lookups :: String -> (OSTree Int, Int) -> Benchmark
lookups name ~(tree,item) = bench name $ nf (M.lookup item) tree

selections :: String -> (OSTree Int, Int) -> Benchmark
selections name ~(tree,item) = bench name $ nf (flip M.select item) tree

strictMapInsert :: String -> Env -> Benchmark
strictMapInsert name ~environ = bench name $ nf Set.fromList environ

