{-# LANGUAGE DuplicateRecordFields #-}
module Data.Game (
  Game(..)
) where

import Data.Misc ( Phase(..) )
import Data.Collectable ( Collectable(..) )
import Data.Rules ( Rules (..) )

type Roll = Int

class Game a where
  getPlayerStaticAll :: a -> [PlayerStatic]
  getPlayerStatic :: a -> PlayerId -> PlayerStatic

  getPlayerDynamicAll :: a -> [PlayerDynamic]
  getPlayerDynamic :: a -> PlayerId -> PlayerDynamic

  -- getRandom :: a -> RandomType -> RandomData
  -- getCollectable :: Collectable c => a -> UniqueId -> c
  getPublicCollectable :: Collectable c => a -> UniqueId -> Maybe c
  getOwnedCollectable :: Collectable c => a -> UniqueId -> Maybe PlayerId -> Maybe c

  getCurrentTurn :: a -> PlayerId
  getCurrentTurnIndex :: a -> Int
  getPhase :: a -> Phase

  getRules :: a -> Rules
  getResearchStore :: a -> ResearchStore
  getDiceRoll :: a -> [Roll]
  getTiles :: a -> Tiles

  update :: a -> PlayerAction -> a

-- TODO Implement Game
