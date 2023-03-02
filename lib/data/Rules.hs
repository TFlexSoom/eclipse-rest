module Data.Rules
  ( SpeciesId,
    Species (..),
    Rules (..),
  )
where

import qualified Data.Collectable as Collectable
import qualified Data.Int as Int
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
    startingDevelopment :: Int.Int8,
    startingResearch :: Int.Int8,
    researchPerTurn :: Int.Int8,
    maxRounds :: Int.Int8,
    center :: Tile.Tile,
    tile :: [(Misc.TileDegree, Tile.Tile)]
  }
