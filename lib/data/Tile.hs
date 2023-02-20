module Data.Tile
  ( Rotation,
    PlanetSlot (..),
    Tile (..),
    TileMap (..),
    TileMapImpl,
    newTileMap,
  )
where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Misc as Misc
import qualified Data.Player as Player

type Rotation = Int -- Integer from [0, 5] representing 60 degree rotations. 0 == 0deg and 5 = 300deg

data PlanetSlot = PlanetSlot
  { resourceType :: Misc.PlanetResourceType,
    isAdvanced :: Bool,
    isOccupied :: Bool,
    isOrbital :: Bool
  }

data Tile = Tile
  { uniqueId :: Misc.UniqueId,
    disk :: Maybe.Maybe Player.PlayerId,
    coordinate :: (Int, Int),
    slots :: [PlanetSlot],
    ships :: [Player.Ship],
    buildings :: [Misc.Building],
    gates :: [Bool]
  }

class TileMap a where
  getStackCount :: a -> Misc.TileDegree -> Int
  getCenter :: a -> Tile
  getHomeSystem :: a -> Player.PlayerId -> Tile
  getTile :: a -> Misc.UniqueId -> Tile
  getTileAt :: a -> (Int, Int) -> Maybe.Maybe Tile
  getAllTiles :: a -> [Tile]

  drawTile :: a -> Misc.TileDegree -> (a, Tile)
  placeTile :: a -> (Tile, Rotation) -> (Int, Int) -> a

data TileMapImpl = TileMapImpl
  { stacks :: Map.Map Misc.TileDegree [Tile],
    placements :: Map.Map (Int, Int) Misc.UniqueId,
    graph :: Map.Map Misc.UniqueId Tile,
    center :: Tile,
    homeSystems :: Map.Map Player.PlayerId Tile
  }

drawTileImpl :: TileMapImpl -> Misc.TileDegree -> (TileMapImpl, Tile)
drawTileImpl tileMap@TileMapImpl {stacks = stacks} deg = (altered, head degreeStack)
  where
    degreeStack = (Map.!) stacks deg
    altered = tileMap {stacks = Map.insert deg (tail degreeStack) stacks}

-- TODO do complex logic to unstub this
placeTileImpl :: TileMapImpl -> (Tile, Rotation) -> (Int, Int) -> TileMapImpl
placeTileImpl tileMap (t, offset) (distance, angle) = error "This Function is not Implemented!"

instance TileMap TileMapImpl where
  getStackCount TileMapImpl {stacks = stacks} degree = length ((Map.!) stacks degree)
  getCenter TileMapImpl {center = center} = center
  getHomeSystem TileMapImpl {homeSystems = homeSystems} pid = (Map.!) homeSystems pid
  getTile TileMapImpl {graph = graph} uid = (Map.!) graph uid
  getTileAt tileMap@TileMapImpl {placements = placements} coord = Maybe.maybe Nothing (Just . getTile tileMap) (Map.lookup coord placements)
  getAllTiles TileMapImpl {graph = graph} = Map.elems graph
  drawTile = drawTileImpl
  placeTile = placeTileImpl

newTileMap :: Map.Map Misc.TileDegree [Tile] -> Tile -> Map.Map Player.PlayerId Tile -> TileMapImpl
newTileMap stacks center homeSystems =
  TileMapImpl
    { stacks = stacks,
      placements = Map.empty, -- TODO Needs HomeSystems + Placements
      graph = Map.empty, -- TODO Needs HomeSystems + Placements
      center = center,
      homeSystems = homeSystems
    }
  where
    playerCount = Map.size homeSystems
