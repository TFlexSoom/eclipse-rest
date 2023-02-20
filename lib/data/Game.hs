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

type GameException = GameImpl -> Bool

-- TODO
data PlayerAction = NOTHING

class Game a where
  getGameId :: a -> GameId
  getPlayerAll :: a -> [Player.Player]
  getPlayer :: a -> Player.PlayerId -> Player.Player

  applyOnPublicCollectable :: a -> Misc.UniqueId -> Maybe.Maybe Collectable.Collectable
  applyOnOwnedCollectable :: a -> Misc.UniqueId -> Player.PlayerId -> Maybe.Maybe Collectable.Collectable

  getCurrentTurn :: a -> Player.PlayerId
  getCurrentTurnIndex :: a -> Int
  getPhase :: a -> Misc.Phase

  -- Rules seem to heavy to provide with a game
  -- getRules :: a -> Rules.Rules
  getResearchStore :: a -> ResearchStore.ResearchStore
  getDiceRoll :: a -> [Roll]
  getTiles :: a -> Tile.TileMapImpl

  update :: a -> PlayerAction -> Maybe a

data GameImpl = GameImpl
  { gameId :: GameId,
    players :: Map.Map Player.PlayerId Player.Player,
    publicCollectable :: Map.Map Misc.UniqueId Collectable.Collectable,
    ownedCollectable :: Map.Map (Player.PlayerId, Misc.UniqueId) Collectable.Collectable,
    turnOrder :: [Player.PlayerId],
    currentTurn :: Int,
    phase :: Misc.Phase,
    researchStore :: ResearchStore.ResearchStore,
    researchPile :: ResearchStore.ResearchStore,
    recentRoll :: [Roll],
    tileMap :: Tile.TileMapImpl,
    discoveries :: [Collectable.Collectable],
    developments :: [Collectable.Collectable]
  }

updateGameImpl :: GameImpl -> PlayerAction -> Maybe GameImpl
updateGameImpl game@GameImpl {} NOTHING = Just game
updateGameImpl _ _ = Nothing

instance Game GameImpl where
  getGameId GameImpl {gameId = gameId} = gameId
  getPlayerAll GameImpl {players = players} = Map.elems players
  getPlayer GameImpl {players = players} = (Map.!) players

  applyOnPublicCollectable GameImpl {publicCollectable = publicCollectable} uid = Map.lookup uid publicCollectable
  applyOnOwnedCollectable GameImpl {ownedCollectable = ownedCollectable} uid pid = Map.lookup (pid, uid) ownedCollectable

  getCurrentTurn GameImpl {turnOrder = turnOrder, currentTurn = currentTurn} = (!!) turnOrder currentTurn
  getCurrentTurnIndex GameImpl {currentTurn = currentTurn} = currentTurn
  getPhase GameImpl {phase = phase} = phase

  -- getRules GameImpl {rules = rules} = rules
  getResearchStore GameImpl {researchStore = researchStore} = researchStore
  getDiceRoll GameImpl {recentRoll = recentRoll} = recentRoll
  getTiles GameImpl {tileMap = tileMap} = tileMap

  update = updateGameImpl

newGame :: Rules.Rules -> GameImpl
newGame rulesParam =
  GameImpl
    { players = Map.empty,
      publicCollectable = Map.empty,
      ownedCollectable = Map.empty,
      turnOrder = [],
      currentTurn = 0,
      phase = Misc.ACTION_PHASE,
      researchStore = [],
      recentRoll = [],
      tileMap = Rules.getTileMapFromRules rulesParam
    }
