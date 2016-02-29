{-# LANGUAGE DeriveGeneric #-}

module Data.OSTree.Types where

import GHC.Generics (Generic)

type Size = Int

-- | Order statistic tree with elements of type 'a'
data OSTree a = Tip
              | Bin {-# UNPACK #-} !Size !a !(OSTree a) !(OSTree a)
              deriving (Eq,Show,Generic)

