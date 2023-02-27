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
import qualified Data.Tile as Tile

type SpeciesId = Misc.UniqueId

data Species = Species
  { name :: String,
    description :: String,
    rulesDescription :: String,
    homeSystem :: Tile.Tile,
    bonuses :: [Player.Player -> Player.Player]
  }

data Rules = Rules
  { numPlayers :: Int,
    species :: Map.Map SpeciesId Species,
    defaultPlayer :: Player.Player,
    ancientPlayer :: Player.Player,
    discovery :: [(Collectable.Discovery, Int)],
    development :: [(Collectable.Development, Int)],
    research :: [(Collectable.Research, Int)],
    startingDevelopment :: Int,
    startingResearch :: Int,
    researchPerTurn :: Int,
    maxRounds :: Int,
    center :: Tile.Tile,
    tile :: [(Misc.TileDegree, Tile.Tile)]
  }
