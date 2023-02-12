{-# LANGUAGE DuplicateRecordFields #-}
module Data.ResearchStore (
  ResearchType,
  Research(..),
  ResearchStore(..)
) where

import Data.Misc ( UniqueId )

type ResearchType = Int -- 1 -> Military | 2 -> Grid | 3 -> Economy

-- These are not collectables since they can be discounted by the research track
-- and players cannot have an infinite amount
data Research = Research {
  id :: UniqueId,
  description :: String,
  Type :: ResearchType,
  MaxCost :: Cost,
  MinCost :: Cost
}

type ResearchStore = [Research]
