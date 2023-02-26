module Data.Misc
  ( ResourceType (..),
    PlanetResourceType (..),
    ShipType (..),
    Phase (..),
    Building (..),
    TileDegree (..),
    UniqueId,
    Cost,
  )
where

import qualified Data.Map as Map

data ResourceType = MONEY | SCIENCE | MATERIAL
  deriving (Read, Show, Enum, Eq, Ord)

data PlanetResourceType = PURE ResourceType | MONEY_SCIENCE | WILD
  deriving (Read, Show, Eq, Ord)

data ShipType = INTERCEPTOR | CRUISER | DREADNOUGHT | STARBASE
  deriving (Read, Show, Enum, Eq, Ord)

data Phase = ACTION_PHASE | COMBAT_PHASE | UPKEEP_PHASE | CLEANUP_PHASE
  deriving (Read, Show, Enum, Eq, Ord)

-- TODO | UNIQUE UniqueId
data Building = MONOLITH | ORBITAL | WARP_PAD
  deriving (Read, Show, Eq, Ord)

data TileDegree = I | II | III
  deriving (Read, Show, Enum, Eq, Ord)

type UniqueId = Word64

type Cost = Map.Map ResourceType Int8