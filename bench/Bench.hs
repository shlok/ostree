
import Criterion.Main

import Data.OSTree

setupEnv = do
  return ()

main = defaultMain [
  env setupEnv $ \ ~() ->
   bgroup "Group 1"
   [
   ]
  ]
