module Data.Game (
  Game(..)
) where

import Data.Misc ( UniqueId, Phase(..) )
import Data.Collectable ( Collectable(..) )
import Data.Rules ( Rules (..) )
import Data.ResearchStore( ResearchStore(..) )
import Data.Player ( PlayerId, Player(..) )
import Data.Tile ( TileMap(..) )

import qualified Data.Maybe as Maybe

type Roll = Int

-- TODO
type PlayerAction = Int

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

-- TODO Implement Game
