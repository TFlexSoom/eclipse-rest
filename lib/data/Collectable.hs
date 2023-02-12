{-# LANGUAGE DuplicateRecordFields #-}

module Data.Collectable (
  Collectable(..),
  Discovery(..),
  Development(..)
)
where

import Data.Misc ( UniqueId, Cost )

class Collectable a where
  uniqueId :: a -> UniqueId
  description :: a -> String
  cost :: a -> Cost


data Discovery = Discovery {
  id :: UniqueId,
  description :: String
}

-- private
zerocost :: Cost
zerocost = {
  money = 0,
  science = 0,
  material = 0
}

instance Collectable Discovery
  uniqueId disc = (id :: Discovery -> UniqueId) disc
  description disc = (description :: Discovery -> String) disc
  cost disc = zerocost

data Development = Development {
  id :: UniqueId,
  description :: String
  cost :: Cost
}

instance Collectable Development
  uniqueId dev = (id :: Development -> UniqueId) dev
  description dev = (description :: Development -> String) dev
  cost dev = (cost :: Development -> Cost) dev