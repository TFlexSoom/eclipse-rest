module Data.Game
  ( Roll (..),
    PlayerAction (..),
    GameCollectable (..),
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

data Roll = RED Int | ORANGE Int | YELLOW Int

-- TODO
data PlayerAction = NOTHING

data GameCollectable = GameCollectable
  { uniqueIdImpl :: Misc.UniqueId,
    descriptionImpl :: String,
    costImpl :: Misc.Cost
  }

copyToGameCollectable :: Collectable.Collectable a => a -> GameCollectable
copyToGameCollectable collectable =
  GameCollectable
    { uniqueIdImpl = Collectable.uniqueId collectable,
      descriptionImpl = Collectable.description collectable,
      costImpl = Collectable.cost collectable
    }

class Game a where
  getPlayerAll :: a -> [Player.Player]
  getPlayer :: a -> Player.PlayerId -> Player.Player

  -- TODO Would be cool to output any Collectable here rather than strict GameCollectable
  applyOnPublicCollectable :: a -> Misc.UniqueId -> Maybe.Maybe GameCollectable
  applyOnOwnedCollectable :: a -> Misc.UniqueId -> Player.PlayerId -> Maybe.Maybe GameCollectable

  getCurrentTurn :: a -> Player.PlayerId
  getCurrentTurnIndex :: a -> Int
  getPhase :: a -> Misc.Phase

  getRules :: a -> Rules.Rules
  getResearchStore :: a -> ResearchStore.ResearchStore
  getDiceRoll :: a -> [Roll]
  getTiles :: a -> Tile.TileMapImpl

  update :: a -> PlayerAction -> Maybe a

data GameImpl = GameImpl
  { players :: Map.Map Player.PlayerId Player.Player,
    publicCollectable :: Map.Map Misc.UniqueId GameCollectable,
    ownedCollectable :: Map.Map (Player.PlayerId, Misc.UniqueId) GameCollectable,
    turnOrder :: [Player.PlayerId],
    currentTurn :: Int,
    phase :: Misc.Phase,
    rules :: Rules.Rules,
    researchStore :: ResearchStore.ResearchStore,
    recentRoll :: [Roll],
    tileMap :: Tile.TileMapImpl
    -- TODO Random Gen Stacks
  }

updateGameImpl :: GameImpl -> PlayerAction -> Maybe GameImpl
updateGameImpl game@GameImpl {} NOTHING = Just game
updateGameImpl _ _ = Nothing

instance Game GameImpl where
  getPlayerAll GameImpl {players = players} = Map.elems players
  getPlayer GameImpl {players = players} = (Map.!) players

  applyOnPublicCollectable GameImpl {publicCollectable = publicCollectable} uid = Map.lookup uid publicCollectable
  applyOnOwnedCollectable GameImpl {ownedCollectable = ownedCollectable} uid pid = Map.lookup (pid, uid) ownedCollectable

  getCurrentTurn GameImpl {turnOrder = turnOrder, currentTurn = currentTurn} = (!!) turnOrder currentTurn
  getCurrentTurnIndex GameImpl {currentTurn = currentTurn} = currentTurn
  getPhase GameImpl {phase = phase} = phase
  getRules GameImpl {rules = rules} = rules
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
      rules = rulesParam,
      researchStore = [],
      recentRoll = [],
      tileMap = Tile.newTileMap rulesParam
    }