module Data.Collectable
  ( Research,
    ResearchStore,
    Discovery,
    Development,
  )
where

import qualified Data.Map as Map
import qualified Data.Misc as Misc

type ResearchType = Int -- 0 -> None | 1 -> Military | 2 -> Efficiency | 3 -> Grid

type DiscoveryId = Misc.UniqueId

type DevelopmentId = Misc.DevelopmentId

type ResearchId = Misc.UniqueId

-- These are not collectables since they can be discounted by the research track
-- and players cannot have an infinite amount
data Research = Research
  { uniqueId :: ResearchId,
    description :: String,
    researchType :: ResearchType,
    maxCost :: Misc.Cost,
    minCost :: Misc.Cost
  }

type ResearchStore = [ResearchId]

data Discovery = Discovery
  { uniqueId :: DiscoveryId,
    description :: String
  }

data Development = Development
  { uniqueId :: DevelopmentId,
    description :: String,
    cost :: Misc.Cost
  }
