module Data.Rules
  ( Species (..),
    Rules (..),
    getTileMapFromRules,
  )
where

import qualified Data.Collectable as Collectable
import qualified Data.Map as Map
import qualified Data.Misc as Misc
import qualified Data.Player as Player
import qualified Data.ResearchStore as ResearchStore
import qualified Data.Tile as Tile

data Species = Species
  { uniqueId :: Misc.UniqueId,
    name :: String,
    description :: String,
    rulesDescription :: String,
    homeSystem :: Tile.Tile,
    bonuses :: [Player.Player -> Player.Player]
  }

data Rules = Rules
  { numPlayers :: Int,
    species :: [Species],
    defaultPlayer :: Player.Player,
    discoveries :: [Collectable.Collectable],
    developments :: [Collectable.Collectable],
    researchCounts :: [(ResearchStore.Research, Int)],
    center :: Tile.Tile,
    tileTierStacks :: Map.Map Misc.TileDegree [Tile.Tile]
  }

getTileMapFromRules :: Rules -> Tile.TileMapImpl
getTileMapFromRules rules = error "Not Implemented"