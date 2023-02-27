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

type Rotation = Int8 -- Integer from [0, 5] representing 60 degree rotations. 0 == 0deg and 5 = 300deg

-- TODO make a Word type instead of component
data PlanetSlot = PlanetSlot
  { resourceType :: Misc.PlanetResourceType,
    isAdvanced :: Bool,
    isOccupied :: Bool,
    isOrbital :: Bool
  }

type TileId = Misc.UniqueId

data Tile = Tile
  { uniqueId :: TileId,
    disk :: Maybe.Maybe Player.PlayerId,
    coordinate :: (Int8, Int8),
    slots :: [PlanetSlot],
    ships :: [Player.Ship],
    buildings :: [Misc.Building],
    gates :: [Bool]
  }

applyRot :: Rotation -> Tile -> Tile
applyRot 0 tile = tile
applyRot rot tile@Tile{gates=a:b:c:d:e:f:[]} | rot > 0 = applyRot (rot - 1) tile{gates=[f,a,b,c,d,e]} 

class TileMap a where
  getStackCount :: a -> Misc.TileDegree -> Int
  getCenter :: a -> Tile
  getHomeSystem :: a -> Player.PlayerId -> Tile
  getTile :: a -> TileId -> Tile
  getTileAt :: a -> (Int, Int) -> Maybe.Maybe Tile
  getAllTiles :: a -> [Tile]

  drawTile :: a -> Misc.TileDegree -> (a, Tile)
  placeTile :: a -> (Tile, Rotation) -> (Int, Int) -> a

data TileMapImpl = TileMapImpl
  { stacks :: Map.Map Misc.TileDegree [Tile],
    placements :: Map.Map (Int, Int) TileId,
    graph :: Map.Map TileId Tile,
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

getStartCoords :: [Tile] -> [((Int8, Int8), Int8, Tile)]
getStartCoords [] = []
getStartCoords z:[] = ((0, 0), 0, z)
getStartCoords y:z:[] = ((0, 2), 0, y):(getStartCoords [z])
getStartCoords x:y:z:[] = ((0, (-2)), 0, x):(getStartCoords [y,z])
getStartCoords w:x:y:z:[] = (((-2), (-1)), 1, w):((2, (-1)), 2, x):(getStartCoords [y,z])
getStartCoords v:w:x:y:z:[] = (((-2), (-1)), 1, v):((0, (-2)), 0, w):((2, (1)), 1, x):(getStartCoords [y,z])
getStartCoords u:v:w:x:y:z:[] = (((-2), 1), 2, u):(getStartCoords [v,w,x,y,z])
getStartCoords t:u:v:w:x:y:z:[] = ((2, (-1)), 2, t):(getStartCoords [u,v,w,x,y,z])
getStartCoords s:t:u:v:w:x:y:z:[] = 
  (((-2), 2), 2, s):
  (((-3), (-1)), 1, t):
  (((-2), (-2)), 0, u):
  ((0, (-3)), 0, v):
  ((2, (-2)), 2, w):
  ((3, 0), 1, x):
  ((2, 2), 0, y):
  (getStartCoords [z])
getStartCoords r:s:t:u:v:w:x:y:z:[] = 
  (((-2), 2), 2, r):
  (((-3), (-1)), 1, s):
  (((-2), (-2)), 0, t):
  ((0, (-3)), 0, u):
  ((2, (-2)), 2, v):
  ((3, 0), 1, w):
  ((2, 2), 0, x):
  ((0, 3), 0, y):
  (getStartCoords [z])
getStartCoords q:r:s:t:u:v:w:x:y:z:[] = 
  (((-1), 2), 2, q):
  (((-3), 1), 2, r):
  (((-2), (-2)), 1, s):
  (((-3), (-1)), 1, t):
  ((0, (-3)), 0, u):
  ((2, (-2)), 2, v):
  ((3, (-1)), 1, w):
  ((3, 1), 1, x):
  ((1, 2), 1, y):
  (getStartCoords [z])
getStartCoords _ = error "Unknown Configuration for 10+ Player Game"

newTileMap :: Map.Map Misc.TileDegree [Tile] -> Tile -> Map.Map Player.PlayerId Tile -> TileMapImpl
newTileMap stacks center homeSystems =
  TileMapImpl
    { stacks = stacks,
      placements = Map.fromList coordsTupl,
      graph = Map.fromList $ zip tileList (take (len tileList) idGen),
      center = center,
      homeSystems = homeSystems
    }
  where
    idGen = iterate (\x -> x + 1) 0
    tileList = center : (Map.elems homeSystems)
    playerCount = Map.size homeSystems
    coordsAndRotation = getStartCoords tileList
    coordsTupl = map (\(coord, rot, tile) -> (coord, applyRot rot tile)) coordsAndRotation