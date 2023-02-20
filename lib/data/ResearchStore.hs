module Data.ResearchStore
  ( ResearchType,
    Research (..),
    ResearchStore (..),
  )
where

import qualified Data.Misc as Misc

type ResearchType = Int -- 0 -> None | 1 -> Military | 2 -> Efficiency | 3 -> Grid

-- These are not collectables since they can be discounted by the research track
-- and players cannot have an infinite amount
data Research = Research
  { uniqueId :: Misc.UniqueId,
    description :: String,
    researchType :: ResearchType,
    maxCost :: Misc.Cost,
    minCost :: Misc.Cost
  }

type ResearchStore = [Research]
