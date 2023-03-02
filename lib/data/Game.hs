module Data.Game
  ( GameId (..),
    PreGameSelection,
    Roll (..),
    PlayerAction (..),
    Game (..),
    newGame,
  )
where

import qualified Data.Collectable as Collectable
import qualified Data.Int as Int
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

firstPlayerId :: Player.PlayerId
firstPlayerId = 1

newPreGameSelection :: GameId -> Rules.Rules -> PreGameSelection
newPreGameSelection gid Rules.Rules {numPlayers = numPlayers} =
  PreGameSelection
    { gameId = gid,
      speciesSelection = Map.empty,
      playersLeftToSelect = Set.fromList (take numPlayers countFromOne)
    }
  where
    countFromOne = iterate (\x -> x + 1) firstPlayerId

selectSpecies :: Rules.SpeciesId -> Player.PlayerId -> Rules.Rules -> PreGameSelection -> Maybe PreGameSelection
selectSpecies
  sid
  pid
  Rules.Rules {species = species}
  preGame@PreGameSelection {speciesSelection = speciesSelection, playersLeftToSelect = playersLeftToSelect}
    | Map.notMember sid species = Nothing
    | Set.notMember pid playersLeftToSelect = Nothing
    | otherwise =
        Just preGame {speciesSelection = Map.insert pid sid speciesSelection, playersLeftToSelect = Set.delete pid playersLeftToSelect}

data Roll = RED Int.Int8 | ORANGE Int.Int8 | YELLOW Int.Int8

-- TODO
data PlayerAction = NOTHING

data TokenType = DISCOVERY | DEVELOPMENT | RESEARCH
  deriving (Read, Show, Enum, Eq, Ord)

data Token = Token
  { uniqueId :: Misc.UniqueId,
    description :: String
  }

class Tokenizeable a where
  tokenize :: a -> Token

data TokenLibrary = TokenLibrary
  { discovery :: Map.Map Collectable.DiscoveryId Collectable.Discovery,
    development :: Map.Map Collectable.DevelopmentId Collectable.Development,
    research :: Map.Map Collectable.ResearchId Collectable.Research
  }

instance Tokenizeable Collectable.Discovery where
  tokenize
    Collectable.Discovery
      { uniqueId = uniqueId,
        description = description
      } =
      Token {uniqueId = uniqueId, description = description}

instance Tokenizeable Collectable.Development where
  tokenize
    Collectable.Development
      { uniqueId = uniqueId,
        description = description
      } =
      Token {uniqueId = uniqueId, description = description}

instance Tokenizeable Collectable.Research where
  tokenize
    Collectable.Research
      { uniqueId = uniqueId,
        description = description
      } =
      Token {uniqueId = uniqueId, description = description}

class Game a where
  getGameId :: a -> GameId
  getPlayerAll :: a -> [Player.Player]
  getPlayer :: a -> Player.PlayerId -> Player.Player
  getPublicToken :: a -> TokenType -> Misc.UniqueId -> Maybe Token
  getPrivateToken :: a -> TokenType -> Misc.UniqueId -> Player.PlayerId -> Maybe Token
  getCurrentTurn :: a -> Player.PlayerId
  getCurrentTurnIndex :: a -> Int.Int8
  getPhase :: a -> Misc.Phase
  getResearchStore :: a -> Collectable.ResearchStore
  getDiceRoll :: a -> [Roll]
  getTiles :: a -> Tile.TileMapImpl
  getCurrentRound :: a -> Int.Int8
  getMaxRounds :: a -> Int.Int8

  update :: a -> PlayerAction -> Maybe a

data GameImpl = GameImpl
  { gameId :: GameId,
    tokenLibrary :: TokenLibrary,
    ownership :: Map.Map Misc.UniqueId Player.PlayerId,
    players :: Map.Map Player.PlayerId Player.Player,
    turnOrder :: [Player.PlayerId],
    currentTurn :: Int.Int8,
    currentRound :: Int.Int8,
    maxRounds :: Int.Int8,
    researchPerTurn :: Int.Int8,
    phase :: Misc.Phase,
    researchStore :: Collectable.ResearchStore,
    researchPile :: [Collectable.ResearchId],
    discoveryPile :: [Collectable.DiscoveryId],
    developmentPile :: [Collectable.DevelopmentId],
    recentRoll :: [Roll],
    tileMap :: Tile.TileMapImpl
  }

getTokenFromLibrary :: TokenType -> Misc.UniqueId -> TokenLibrary -> Maybe Token
getTokenFromLibrary DISCOVERY uid TokenLibrary {discovery = discovery} = Maybe.maybe Nothing (Just . tokenize) (Map.lookup uid discovery)
getTokenFromLibrary DEVELOPMENT uid TokenLibrary {development = development} = Maybe.maybe Nothing (Just . tokenize) (Map.lookup uid development)
getTokenFromLibrary RESEARCH uid TokenLibrary {research = research} = Maybe.maybe Nothing (Just . tokenize) (Map.lookup uid research)
getTokenFromLibrary _ _ _ = error "Method not implemented!"

getPublicTokenImpl :: GameImpl -> TokenType -> Misc.UniqueId -> Maybe Token
getPublicTokenImpl GameImpl {tokenLibrary = library, ownership = owners} tokenType uid
  | Map.notMember uid owners = getTokenFromLibrary tokenType uid library
  | otherwise = Nothing

getPrivateTokenImpl :: GameImpl -> TokenType -> Misc.UniqueId -> Player.PlayerId -> Maybe Token
getPrivateTokenImpl GameImpl {tokenLibrary = library, ownership = owners} tokenType uid pid
  | Map.lookup uid owners == (Just pid) = getTokenFromLibrary tokenType uid library
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
  getCurrentTurn GameImpl {turnOrder = turnOrder, currentTurn = currentTurn} = (!!) turnOrder (fromIntegral currentTurn)
  getCurrentTurnIndex GameImpl {currentTurn = currentTurn} = currentTurn
  getPhase GameImpl {phase = phase} = phase
  getResearchStore GameImpl {researchStore = researchStore} = researchStore
  getDiceRoll GameImpl {recentRoll = recentRoll} = recentRoll
  getTiles GameImpl {tileMap = tileMap} = tileMap
  getCurrentRound GameImpl {currentRound = currentRound} = currentRound
  getMaxRounds GameImpl {maxRounds = maxRounds} = maxRounds
  update = updateGameImpl

inflate :: [(a, Int)] -> [a]
inflate = foldr (\(elem, count) lst -> (replicate count elem) ++ lst) []

-- TODO Impl
shuffle :: [a] -> [a]
shuffle = id

indexMap :: [a] -> Map.Map Misc.UniqueId a
indexMap = (.) (snd) (foldr (\elem (cnt, m) -> (cnt + 1, Map.insert cnt elem m)) (1, Map.empty))

newGame :: Rules.Rules -> PreGameSelection -> GameImpl
newGame
  Rules.Rules
    { numPlayers = numPlayers, -- :: Int,
      species = species, -- :: Map.Map Rules.SpeciesId Rules.Species,
      defaultPlayer = defaultPlayer, -- :: Player.Player,
      ancientPlayer = ancientPlayer, -- :: Player.Player,
      discovery = discovery, -- :: [(Collectable.Discovery, Int)],
      development = development, -- :: [(Collectable.Development, Int)],
      research = research, -- :: [(Collectable.Research, Int)],
      startingDevelopment = startingDevelopment, -- :: Int,
      startingResearch = startingResearch, -- :: Int,
      researchPerTurn = researchPerTurn, -- :: Int,
      maxRounds = maxRounds, -- :: Int,
      center = center, -- :: Tile.Tile,
      tile = tiles -- :: [(Misc.TileDegree, Tile.Tile)]
    }
  PreGameSelection
    { gameId = gameId,
      speciesSelection = speciesSelection
    } =
    GameImpl
      { gameId = gameId,
        tokenLibrary =
          TokenLibrary
            { discovery = discoveryMap,
              development = developmentMap,
              research = researchMap
            },
        ownership = Map.empty,
        players = playerApplied,
        turnOrder = take numPlayers (iterate (\x -> x + 1) startingTurn),
        currentTurn = fromIntegral startingTurn,
        currentRound = 1,
        maxRounds = maxRounds,
        researchPerTurn = researchPerTurn,
        phase = Misc.ACTION_PHASE,
        researchStore = take (fromIntegral startingResearch) researchShuffled,
        researchPile = drop (fromIntegral startingResearch) researchShuffled,
        discoveryPile = shuffle $ Map.keys discoveryMap,
        developmentPile = shuffle $ Map.keys developmentMap,
        recentRoll = [],
        tileMap = Tile.newTileMap tilePilesShuffled center selectedHomeworlds
      }
    where
      startingTurn = firstPlayerId -- Player 0 is the Ancient Player
      discoveryMap = indexMap $ inflate discovery
      developmentMap = indexMap $ take (fromIntegral startingDevelopment) $ inflate development
      researchMap = indexMap $ inflate research
      researchShuffled = shuffle $ Map.keys researchMap
      selectedHomeworlds = Map.map ((\Rules.Species {homeSystem = homeSystem} -> homeSystem) . (Map.!) species) speciesSelection
      tilePiles = foldr (\(deg, tile) dict -> Map.update (Just . (:) tile) deg dict) (Map.fromList [(Misc.I, []), (Misc.II, []), (Misc.III, [])]) tiles
      tilePilesShuffled = Map.map shuffle tilePiles
      playerDefaultMap = indexMap $ ancientPlayer : (replicate numPlayers defaultPlayer)
      playerChanges pid = (\Rules.Species {bonuses = bonuses} -> bonuses) $ (Map.!) species ((Map.!) speciesSelection pid)
      playerApplied = Map.mapWithKey (\pid player -> player) playerDefaultMap
