{-# LANGUAGE DuplicateRecordFields #-}
module Rules (
  Species(..),
  Rules(..)
) where

import Misc ( UniqueId(..) )

data Species = {
  id :: UniqueId,
  name :: String,
  description :: String,
  exemptions :: String
}

data Rules = Rules {
  species :: [Species]
}