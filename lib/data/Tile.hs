{-# LANGUAGE DuplicateRecordFields #-}
module Tile (
  TileMap(..),
  TileMapImpl,
  newTileMap,

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

data Neighbor = NO_GATE | EMPTY | CONNECTED UniqueId

data Tile {
  id :: UniqueId,
  disk :: Maybe PlayerId,
  slots :: [PlanetSlot],
  ships :: [Ship],
  buildings :: [Building],
  neighbors :: [Neighbor]
}

class TileMap a where
  getStackCount :: a -> TileDegree -> Int
  getCenter :: a -> Tile
  getHomeSystem :: a -> PlayerId -> Tile
  getTile :: a -> UniqueId -> Tile
  getNeighbors :: a -> Tile -> [Maybe Tile]
  getNeighborsFromId :: a -> UniqueId -> [Maybe Tile]
  getAllTiles :: a -> [Tile]

  drawTile :: a -> TileDegree -> (a, Tile)
  placeTile :: a -> (Tile, Rotation) -> (Int, Rotation) -> a

data TileMapImpl = TileMapImpl {
  stacks :: Map TileDegree [Tile]
  graph :: Map UniqueId Tile
  center :: Tile,
  homeSystems :: Map PlayerId Tile
}

instance TileMap TileMapImpl where
  getStackCount TileMapImpl { stacks = stacks } degree = length ((!) stacks degree)
  getCenter TileMapImpl { center = center } = center
  getHomeSystem TileMapImpl { homeSystems = homeSystems } pid = (!) homeSystems pid
  getTile TileMapImpl {graph = graph} uid = fst((!) graph uid )
  getNeighbors self tile = getNeighborsFromId self ((id :: Tile -> UniqueId) tile)
  getNeighborsFromId TileMapImpl {graph = graph} uid = map getTile (snd((!) graph uid))
  getAllTiles TileMapImpl {graph = graph} = map fst (elems graph)

  drawTile tileMap@TileMapImpl {stacks = stacks} deg = (altered, head degreeStack)
    where 
      degreeStack = (!) stacks deg
      altered = tileMap{stacks= insert deg (tail degreeStack) stacks}
  
  -- TODO do complex logic to unstub this
  placeTile tileMap (t, offset) (distance, angle) = tileMap

newTileMap :: TileMapImpl
newTileMap = TileMapImpl {}

newTile :: Tile
newTile = Tile {}