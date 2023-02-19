module Data.Game (
  Roll(..),
  PlayerAction(..),
  GameCollectable(..),
  Game(..),
  newGame
) where

import Data.Misc ( UniqueId, Phase(..), Cost )
import Data.Collectable ( Collectable(..) )
import Data.Rules ( Rules (..) )
import Data.ResearchStore( ResearchStore(..) )
import Data.Player ( PlayerId, Player(..) )
import Data.Tile ( TileMap(..), TileMapImpl, newTileMap )

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Roll = RED Int | ORANGE Int | YELLOW Int

-- TODO
data PlayerAction = NOTHING

data GameCollectable = GameCollectable {
  uniqueIdImpl :: UniqueId,
  descriptionImpl :: String,
  costImpl :: Cost
}

copyToGameCollectable :: Collectable a => a -> GameCollectable
copyToGameCollectable collectable = GameCollectable {
  uniqueIdImpl = uniqueId collectable,
  descriptionImpl = description collectable,
  costImpl = cost collectable
}

class Game a where
  getPlayerAll :: a -> [Player]
  getPlayer :: a -> PlayerId -> Player

  -- TODO Would be cool to output any Collectable here rather than strict GameCollectable
  applyOnPublicCollectable :: a -> UniqueId -> Maybe.Maybe GameCollectable
  applyOnOwnedCollectable :: a -> UniqueId -> PlayerId -> Maybe.Maybe GameCollectable

  getCurrentTurn :: a -> PlayerId
  getCurrentTurnIndex :: a -> Int
  getPhase :: a -> Phase

  getRules :: a -> Rules
  getResearchStore :: a -> ResearchStore
  getDiceRoll :: a -> [Roll]
  getTiles :: a -> TileMapImpl

  update :: a -> PlayerAction -> Maybe a

data GameImpl = GameImpl {
  players :: Map.Map PlayerId Player,
  publicCollectable :: Map.Map UniqueId GameCollectable,
  ownedCollectable :: Map.Map (PlayerId, UniqueId) GameCollectable,
  turnOrder :: [PlayerId],
  currentTurn :: Int,
  phase :: Phase,
  rules :: Rules,
  researchStore :: ResearchStore,
  recentRoll :: [Roll],
  tileMap :: TileMapImpl
  -- TODO Random Gen Stacks
}

updateGameImpl :: GameImpl -> PlayerAction -> Maybe GameImpl
updateGameImpl game@GameImpl{} NOTHING = Just game
updateGameImpl _ _ = Nothing

instance Game GameImpl where
  getPlayerAll GameImpl{players=players} = Map.elems players
  getPlayer GameImpl{players=players} = (Map.!) players

  applyOnPublicCollectable GameImpl{publicCollectable=publicCollectable} uid = Map.lookup uid publicCollectable
  applyOnOwnedCollectable GameImpl{ownedCollectable=ownedCollectable} uid pid = Map.lookup (pid, uid) ownedCollectable

  getCurrentTurn GameImpl{turnOrder=turnOrder, currentTurn=currentTurn} = (!!) turnOrder currentTurn
  getCurrentTurnIndex GameImpl{currentTurn=currentTurn} = currentTurn
  getPhase GameImpl{phase=phase} = phase
  getRules GameImpl{rules=rules} = rules
  getResearchStore GameImpl{researchStore=researchStore} = researchStore
  getDiceRoll GameImpl{recentRoll=recentRoll} = recentRoll
  getTiles GameImpl{tileMap=tileMap} = tileMap

  update = updateGameImpl

newGame :: Rules -> GameImpl
newGame rulesParam = GameImpl {
  players = Map.empty,
  publicCollectable = Map.empty,
  ownedCollectable = Map.empty,
  turnOrder = [],
  currentTurn = 0,
  phase = ACTION_PHASE,
  rules = rulesParam,
  researchStore = [],
  recentRoll = [],
  tileMap = newTileMap rulesParam
}