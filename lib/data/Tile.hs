module Data.Tile (
  Rotation,
  TileDegree(..),
  PlanetSlot(..),
  Tile(..),
  TileMap(..),
  newTileMap
) where

import Data.Misc ( UniqueId(..), PlanetResourceType, Building )
import Data.Player ( PlayerId, Ship(..) )
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Rotation = Int -- Integer from [0, 5] representing 60 degree rotations. 0 == 0deg and 5 = 300deg

data TileDegree = I | II | III
  deriving (Read, Show, Enum, Eq, Ord)

data PlanetSlot = PlanetSlot {
  resourceType :: PlanetResourceType,
  isOccupied :: Bool,
  isOrbital :: Bool
}

data Tile = Tile {
  id :: UniqueId,
  disk :: Maybe.Maybe PlayerId,
  coordinate :: (Int, Int),
  slots :: [PlanetSlot],
  ships :: [Ship],
  buildings :: [Building],
  gates :: [Bool]
}

class TileMap a where
  getStackCount :: a -> TileDegree -> Int
  getCenter :: a -> Tile
  getHomeSystem :: a -> PlayerId -> Tile
  getTile :: a -> UniqueId -> Tile
  getTileAt :: a -> (Int, Int) -> Maybe.Maybe Tile
  getAllTiles :: a -> [Tile]

  drawTile :: a -> TileDegree -> (a, Tile)
  placeTile :: a -> (Tile, Rotation) -> (Int, Int) -> a

data TileMapImpl = TileMapImpl {
  stacks :: Map.Map TileDegree [Tile],
  placements :: Map.Map (Int, Int) UniqueId,
  graph :: Map.Map UniqueId Tile,
  center :: Tile,
  homeSystems :: Map.Map PlayerId Tile
}

instance TileMap TileMapImpl where
  getStackCount TileMapImpl { stacks = stacks } degree = length ((Map.!) stacks degree)
  getCenter TileMapImpl { center = center } = center
  getHomeSystem TileMapImpl { homeSystems = homeSystems } pid = (Map.!) homeSystems pid
  getTile TileMapImpl { graph = graph } uid = (Map.!) graph uid
  getTileAt tileMap@TileMapImpl { placements = placements } coord = Maybe.maybe Nothing (Just . getTile tileMap) (Map.lookup coord placements)
  getAllTiles TileMapImpl { graph = graph } = Map.elems graph

  drawTile tileMap@TileMapImpl {stacks = stacks} deg = (altered, head degreeStack)
    where 
      degreeStack = (Map.!) stacks deg
      altered = tileMap{stacks= Map.insert deg (tail degreeStack) stacks}
  
  -- TODO do complex logic to unstub this
  placeTile tileMap (t, offset) (distance, angle) = error "This Function is not Implemented!"

newTileMap :: TileMapImpl
newTileMap = TileMapImpl {
  stacks = Map.empty, -- TODO Gen from Rules
  placements = Map.empty, -- TODO Needs HomeSystems + Placements
  graph = Map.empty, -- TODO Needs HomeSystems + Placements
  center = newTile, -- TODO Gen from Rules
  homeSystems = Map.empty -- TODO Gen from Rules?
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