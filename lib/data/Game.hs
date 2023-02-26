module Data.Game
  ( Roll (..),
    PlayerAction (..),
    Game (..),
    newGame,
  )
where

import qualified Data.Collectable as Collectable
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Misc as Misc
import qualified Data.Player as Player
import qualified Data.ResearchStore as ResearchStore
import qualified Data.Rules as Rules
import qualified Data.Tile as Tile

type GameId = Misc.UniqueId

data Roll = RED Int | ORANGE Int | YELLOW Int

-- TODO
data PlayerAction = NOTHING

data TokenType = DISCOVERY | DEVELOPMENT | RESEARCH
  deriving (Read, Show, Enum, Eq, Ord)

data Token = Token
  { uniqueId :: Misc.UniqueId,
    description :: String,
    cost :: Misc.Cost
  }

data TokenLibrary = TokenLibrary
  { discovery :: Map.Map Collectable.DiscoveryId Collectable.Discovery,
    development :: Map.Map Collectable.DevelopmentId Collectable.Development,
    research :: Map.Map ResearchStore.ResearchId ResearchStore.Research
  }

class Game a where
  getGameId :: a -> GameId
  getPlayerAll :: a -> [Player.Player]
  getPlayer :: a -> Player.PlayerId -> Player.Player
  getPublicToken :: a -> TokenType -> Misc.UniqueId -> Maybe Token
  getPrivateToken :: a -> TokenType -> Misc.UniqueId -> Player.PlayerId -> Maybe Token
  getCurrentTurn :: a -> Player.PlayerId
  getCurrentTurnIndex :: a -> Int
  getPhase :: a -> Misc.Phase
  getResearchStore :: a -> ResearchStore.ResearchStore
  getDiceRoll :: a -> [Roll]
  getTiles :: a -> Tile.TileMapImpl

  update :: a -> PlayerAction -> Maybe a

data GameImpl = GameImpl
  { gameId :: GameId,
    tokenLibrary :: TokenLibrary,
    ownership :: Map.Map Misc.UniqueId Player.PlayerId,
    players :: Map.Map Player.PlayerId Player.Player,
    turnOrder :: [Player.PlayerId],
    currentTurn :: Int,
    phase :: Misc.Phase,
    researchStore :: ResearchStore.ResearchStore,
    researchPile :: ResearchStore.ResearchStore,
    discoveryPile :: [Collectable.DiscoveryId],
    developmentPile :: [Collectable.DevelopmentId],
    recentRoll :: [Roll],
    tileMap :: Tile.TileMapImpl
  }

getTokenFromLibrary :: TokenType -> Misc.UniqueId -> TokenLibrary -> Maybe Token
getTokenFromLibrary DISCOVERY uid TokenLibrary{discovery=discovery} = Map.lookup uid discovery
getTokenFromLibrary DEVELOPMENT uid TokenLibrary{development=development} = Map.lookup uid development
getTokenFromLibrary RESEARCH uid TokenLibrary{research=research} = Map.lookup uid research
getTokenFromLibrary _ = error "Method not implemented!"

getPublicTokenImpl :: GameImpl -> TokenType -> Misc.UniqueId -> Maybe Token
getPublicTokenImpl type uid GameImpl {tokenLibrary=library, ownership=owners}
  | Map.notMember uid owners = getTokenFromLibrary type uid library
  | otherwise = Nothing

getPrivateTokenImpl :: GameImpl -> TokenType -> Misc.UniqueId -> Maybe Token
getPrivateTokenImpl type uid pid GameImpl {tokenLibrary=library, ownership=owners}
  | Map.lookup uid owners == (Just pid) = getTokenFromLibrary type uid library
  | otherwise = Nothing

updateGameImpl :: GameImpl -> PlayerAction -> Maybe GameImpl
updateGameImpl game@GameImpl {} NOTHING = Just game
updateGameImpl _ _ = Nothing

instance Game GameImpl where
  getGameId GameImpl {gameId = gameId} = gameId
  getPlayerAll GameImpl {players = players} = Map.elems players
  getPlayer GameImpl {players = players} = (Map.!) players
  getPublicToken = getPublicTokenImpl
  getPrivateToken = getPrivateTokenImpl
  getCurrentTurn GameImpl {turnOrder = turnOrder, currentTurn = currentTurn} = (!!) turnOrder currentTurn
  getCurrentTurnIndex GameImpl {currentTurn = currentTurn} = currentTurn
  getPhase GameImpl {phase = phase} = phase
  getResearchStore GameImpl {researchStore = researchStore} = researchStore
  getDiceRoll GameImpl {recentRoll = recentRoll} = recentRoll
  getTiles GameImpl {tileMap = tileMap} = tileMap
  update = updateGameImpl

inflate :: [(a, Int)] -> [a]
inflate = foldl (\(elem,count) lst -> (replicate count elem) ++ lst) [] 

-- TODO Impl
shuffle :: [a] -> [a]
shuffle = id

indexMap :: [a] -> Map Misc.UniqueId a
indexMap = (.) (snd) (foldl (\elem (cnt, m) -> (cnt + 1, Map.insert cnt elem m)) (1, Map.empty))

newGame :: Rules.Rules -> GameId -> GameImpl
newGame Rules 
  { numPlayers = numPlayers, -- :: Int,
    species = species, -- :: [Species],
    defaultPlayer = defaultPlayer, -- :: Player.Player,
    ancientPlayer = ancientPlayer, -- :: Player.Player,
    discovery = discovery, -- :: [(Collectable.Discovery, Int)],
    development =  development, -- :: [(Collectable.Development, Int)],
    research = research, -- :: [(ResearchStore.Research, Int)],
    center = center, -- :: Tile.Tile,
    tiles = tiles -- :: [Tile.Tile]
  } 
  gid =
    GameImpl
      { gameId = gid,
        tokenLibrary = TokenLibrary 
        { discovery = discoveryMap,
          development = developmentMap,
          research = researchMap 
        },
        ownership = Map.empty,
        players = indexMap ancientPlayer:(replicate numPlayers defaultPlayer),
        turnOrder = take numPlayers (iterate (\x -> x + 1) 0),
        currentTurn = 0,
        phase = ACTION_PHASE,
        researchStore = take 10 researchShuffled, -- TODO 10 is a magic number
        researchPile = drop 10 researchShuffled, -- TODO 10 is a magic number
        discoveryPile = shuffle (Map.keys discovery),
        developmentPile = shuffle (Map.keys development),
        recentRoll = [],
        tileMap = tileMapSetup
      }
    where
      discoveryMap = indexMap (inflate discovery)
      developmentMap = indexMap (inflate development)
      research = indexMap (inflate research)
      researchShuffled = shuffle (Map.keys research)
      tileMapSetup = Tile.newTileMap
