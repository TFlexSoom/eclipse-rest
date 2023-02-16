module Data.Game (
  Game(..)
) where

import Data.Misc ( UniqueId, Phase(..) )
import Data.Collectable ( Collectable(..) )
import Data.Rules ( Rules (..) )
import Data.ResearchStore( ResearchStore(..) )
import Data.Player ( PlayerId, PlayerStatic(..), PlayerDynamic(..))
import Data.Tile ( TileMap(..) )

import qualified Data.Maybe as Maybe

type Roll = Int

-- TODO
type PlayerAction = Int

class Game a where
  getPlayerStaticAll :: a -> [PlayerStatic]
  getPlayerStatic :: a -> PlayerId -> PlayerStatic

  getPlayerDynamicAll :: a -> [PlayerDynamic]
  getPlayerDynamic :: a -> PlayerId -> PlayerDynamic

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
