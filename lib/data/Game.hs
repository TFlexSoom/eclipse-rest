module Data.Game
  ( GameId(..),
    PreGameSelection,
    Roll (..),
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
import qualified Data.Rules as Rules
import qualified Data.Set as Set
import qualified Data.Tile as Tile

type GameId = Misc.UniqueId

data PreGameSelection = PreGameSelection 
  { gameId :: GameId,
    speciesSelection :: Map.Map Player.PlayerId Rules.SpeciesId,
    playersLeftToSelect :: Set.Set Player.PlayerId
  }

newPreGameSelection :: GameId -> Rules.Rules -> PreGameSelection
newPreGameSelection gid Rules{ numPlayers = numPlayers } = PreGameSelection 
  { gameId = gid,
    speciesSelection = Map.empty,
    playersLeftToSelect = Set.fromList (take numPlayers countFromOne)
  }
  where countFromOne = iterate (\x -> x + 1) firstPlayerId

selectSpecies :: Rules.SpeciesId -> Player.PlayerId -> Rules.Rules -> PreGameSelection -> Maybe PreGameSelection
selectSpecies _ _ _ _ = Nothing

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
    research :: Map.Map Collectable.ResearchId Collectable.Research
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
  getResearchStore :: a -> Collectable.ResearchStore
  getDiceRoll :: a -> [Roll]
  getTiles :: a -> Tile.TileMapImpl
  getCurrentRound :: a -> Int
  getMaxRounds :: a -> Int

  update :: a -> PlayerAction -> Maybe a

data GameImpl = GameImpl
  { gameId :: GameId,
    tokenLibrary :: TokenLibrary,
    ownership :: Map.Map Misc.UniqueId Player.PlayerId,
    players :: Map.Map Player.PlayerId Player.Player,
    turnOrder :: [Player.PlayerId],
    currentTurn :: Int,
    currentRound :: Int,
    maxRounds :: Int,
    phase :: Misc.Phase,
    researchStore :: Collectable.ResearchStore,
    researchPile :: [Collectable.ResearchId],
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
  getCurrentRound GameImpl {currentRound = currentRound} = currentRound
  getMaxRounds GameImpl {maxRounds = maxRounds} = maxRounds
  update = updateGameImpl

inflate :: [(a, Int)] -> [a]
inflate = foldl (\(elem,count) lst -> (replicate count elem) ++ lst) [] 

-- TODO Impl
shuffle :: [a] -> [a]
shuffle = id

indexMap :: [a] -> Map Misc.UniqueId a
indexMap = (.) (snd) (foldl (\elem (cnt, m) -> (cnt + 1, Map.insert cnt elem m)) (1, Map.empty))

newGame :: Rules.Rules -> PreGameSelection -> GameImpl
newGame  
  Rules 
  { numPlayers = numPlayers, -- :: Int,
    species = species, -- :: [Species],
    defaultPlayer = defaultPlayer, -- :: Player.Player,
    ancientPlayer = ancientPlayer, -- :: Player.Player,
    discovery = discovery, -- :: [(Collectable.Discovery, Int)],
    development =  development, -- :: [(Collectable.Development, Int)],
    research = research, -- :: [(Collectable.Research, Int)],
    startingDevelopment = startingDevelopment, -- :: Int,
    startingResearch = startingResearch, -- :: Int,
    researchPerTurn = researchPerTurn, -- :: Int,
    maxRounds = maxRounds, -- :: Int,
    center = center, -- :: Tile.Tile,
    tiles = tiles -- :: [(Misc.TileDegree, Tile.Tile)]
  } 
  PreGameSelection
  { gameId = gameId,
    speciesSelection = speciesSelection
  }
   =
    GameImpl
      { gameId = gid,
        tokenLibrary = TokenLibrary 
        { discovery = discoveryMap,
          development = developmentMap,
          research = researchMap 
        },
        ownership = Map.empty,
        players = indexMap ancientPlayer:(replicate numPlayers defaultPlayer),
        turnOrder = take numPlayers (iterate (\x -> x + 1) startingTurn),
        currentTurn = startingTurn,
        currentRound = 1,
        maxRounds = maxRounds,
        phase = ACTION_PHASE,
        researchStore = take startingResearch researchShuffled, 
        researchPile = drop startingResearch researchShuffled,
        discoveryPile = shuffle (Map.keys discovery),
        developmentPile = shuffle (Map.keys development),
        recentRoll = [],
        tileMap = Nothing
      }
    where
      startingTurn = 1 -- Player 0 is the Ancient Player
      discoveryMap = indexMap (inflate discovery)
      developmentMap = indexMap (take startingDevelopment (inflate development))
      research = indexMap (inflate research)
      researchShuffled = shuffle (Map.keys research)
