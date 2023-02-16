module Data.ResearchStore (
  ResearchType,
  Research(..),
  ResearchStore(..)
) where

import Data.Misc ( UniqueId, Cost(..) )

type ResearchType = Int -- 1 -> Military | 2 -> Grid | 3 -> Economy

-- These are not collectables since they can be discounted by the research track
-- and players cannot have an infinite amount
data Research = Research {
  idImpl :: UniqueId,
  description :: String,
  researchType :: ResearchType,
  maxCost :: Cost,
  minCost :: Cost
}

type ResearchStore = [Research]
