module Data.Game (
  Game(..)
) where

import Data.Misc ( UniqueId, Phase(..) )
import Data.Collectable ( Collectable(..) )
import Data.Rules ( Rules (..) )
import Data.ResearchStore( ResearchStore(..) )
import Data.Player ( PlayerId, Player(..) )
import Data.Tile ( TileMap(..) )

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Roll = Int

-- TODO
data PlayerAction = NOTHING

class Game a where
  getPlayerAll :: a -> [Player]
  getPlayer :: a -> PlayerId -> Player

  -- getRandom :: a -> RandomType -> RandomData
  -- getCollectable :: Collectable c => a -> UniqueId -> c
  getPublicCollectable :: Collectable c => a -> UniqueId -> Maybe.Maybe c
  getOwnedCollectable :: Collectable c => a -> UniqueId -> Maybe.Maybe PlayerId -> Maybe.Maybe c

  getCurrentTurn :: a -> PlayerId
  getCurrentTurnIndex :: a -> Int
  getPhase :: a -> Phase

  getRules :: a -> Rules
  getResearchStore :: a -> ResearchStore
  getDiceRoll :: a -> [Roll]
  getTiles :: TileMap b => a -> b

  update :: a -> PlayerAction -> a

data GameCollectable = GameCollectable {
  uniqueIdImpl :: UniqueId,
  descriptionImpl :: String,
  costImpl :: Cost
}

instance Collectable GameCollectable where
  uniqueId GameCollectable{uniqueIdImpl=uniqueIdImpl} = uniqueIdImpl
  descriptionImpl GameCollectable{descriptionImpl=descriptionImpl} = descriptionImpl
  cost GameCollectable{costImpl=costImpl} = costImpl

copyToGameCollectable :: Collectable a => a -> GameCollectable
copyToGameCollectable collectable = GameCollectable {
  uniqueIdImpl = uniqueId collectable,
  descriptionImpl = description collectable,
  costImpl = costImpl collectable
}

data GameImpl = GameImpl {
  players :: Map.Map PlayerId Player,
  publicCollectable :: Map.Map UniqueId GameCollectable
  ownedCollectable :: Map.Map (PlayerId, UniqueId) GameCollectable
  
}