module Misc (
    ResourceType(..),
    PlanetResourceType(..),
    ShipType(..),
    Phase(..),
    Building(..),
    UniqueId,
    Cost
) where

import qualified Data.Map as Map

data ResourceType = MONEY | SCIENCE | MATERIAL
    deriving( Read, Show, Enum, Eq, Ord )

data PlanetResourceType = PURE ResourceType | MONEY_SCIENCE | WILD
    deriving( Read, Show, Enum, Eq, Ord )
    
data ShipType = INTERCEPTOR | CRUISER | DREADNOUGHT | STARBASE
    deriving( Read, Show, Enum, Eq, Ord )

data Phase = ACTION_PHASE | COMBAT_PHASE | UPKEEP_PHASE | CLEANUP_PHASE
    deriving( Read, Show, Enum, Eq, Ord )

-- With Shrines we made need an astricks type system so these can
-- be looked up lazily
data Building = MONOLITH | ORBITAL | WARP_PAD
    deriving( Read, Show, Enum, Eq, Ord )
    
type UniqueId = Int

type Cost = Map ResourceType Int