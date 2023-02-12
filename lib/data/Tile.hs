{-# LANGUAGE DuplicateRecordFields #-}
module Tile (
  Rotation,
  TileDegree(..),
  PlanetSlot(..),
  Tile(..)
  TileMap(..),
  newTileMap
) where

import Data.Misc ( UniqueId(..), PlanetResourceType, Building )
import qualified Data.Map as Map

type Rotation = Int -- Integer from [0, 5] representing 60 degree rotations. 0 == 0deg and 5 = 300deg

data TileDegree = I | II | III
  deriving (Read, Show, Enum, Eq, Ord)

data PlanetSlot = PlanetSlot {
  type :: PlanetResourceType,
  isOccupied :: Boolean,
  isOrbital :: Boolean
}

data Tile {
  id :: UniqueId,
  disk :: Maybe PlayerId,
  coordinate :: (Int, Int),
  slots :: [PlanetSlot],
  ships :: [Ship],
  buildings :: [Building],
  gates :: [Boolean]
}

class TileMap a where
  getStackCount :: a -> TileDegree -> Int
  getCenter :: a -> Tile
  getHomeSystem :: a -> PlayerId -> Tile
  getTile :: a -> UniqueId -> Tile
  getTileAt :: a -> (Int, Int) -> Maybe Tile
  getAllTiles :: a -> [Tile]

  drawTile :: a -> TileDegree -> (a, Tile)
  placeTile :: a -> (Tile, Rotation) -> (Int, Int) -> a

data TileMapImpl = TileMapImpl {
  stacks :: Map TileDegree [Tile]
  placements :: Map (Int, Int) UniqueId
  graph :: Map UniqueId Tile
  center :: Tile,
  homeSystems :: Map PlayerId Tile
}

instance TileMap TileMapImpl where
  getStackCount TileMapImpl { stacks = stacks } degree = length ((!) stacks degree)
  getCenter TileMapImpl { center = center } = center
  getHomeSystem TileMapImpl { homeSystems = homeSystems } pid = (!) homeSystems pid
  getTile TileMapImpl { graph = graph } uid = (!) graph uid
  getTileAt TileMapImple { placements = placements } coord = lookup coord placements
  getAllTiles TileMapImpl { graph = graph } = elems graph

  drawTile tileMap@TileMapImpl {stacks = stacks} deg = (altered, head degreeStack)
    where 
      degreeStack = (!) stacks deg
      altered = tileMap{stacks= insert deg (tail degreeStack) stacks}
  
  -- TODO do complex logic to unstub this
  placeTile tileMap (t, offset) (distance, angle) = error "This Function is not Implemented!"

newTileMap :: TileMap a => a
newTileMap = TileMapImpl {
  stacks = empty, -- TODO Gen from Rules
  placements = empty, -- TODO Needs HomeSystems + Placements
  graph = empty, -- TODO Needs HomeSystems + Placements
  center = newTile -- TODO Gen from Rules
  homeSystems = empty -- TODO Gen from Rules?
}

newTile :: Tile
newTile = Tile {
  id = 0, -- TODO Gen from Rules
  disk = Nothing,
  coordinate = (0, 0),
  slots = [], -- TODO Gen From Rules
  ships = [],
  buildings = [],
  gates = [] -- TODO Gen From Rules
}