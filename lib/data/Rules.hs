module Data.Rules
  ( Species (..),
    Rules (..),
  )
where

import qualified Data.Misc as Misc

data Species = Species
  { id :: Misc.UniqueId,
    name :: String,
    description :: String,
    exemptions :: String
  }

data Rules = Rules
  { species :: [Species]
  }