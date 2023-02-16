module Data.Rules (
  Species(..),
  Rules(..)
) where

import Data.Misc ( UniqueId )

data Species = Species {
  id :: UniqueId,
  name :: String,
  description :: String,
  exemptions :: String
}

data Rules = Rules {
  species :: [Species]
}