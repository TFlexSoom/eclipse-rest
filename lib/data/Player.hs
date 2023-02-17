module Data.Player (
  PlayerId,
  SpeciesId,
  DiplomacySlot(..),
  DiplomacyToken(..),
  Influence(..),
  Tech(..),
  Player(..),
  Species(..),
  Ship(..)
) where

import Data.Misc ( UniqueId, ShipType, Cost(..) )
import Data.ResearchStore ( ResearchType, Research(..) )

import qualified Data.Map as Map

type PlayerId = UniqueId
type SpeciesId = UniqueId

data DiplomacySlot = FILLED DiplomacyToken | OPEN | OPEN_REPUTATION | OPEN_AMBASSADOR
data DiplomacyToken = REPUTATION Int | AMBASSADOR PlayerId

data Influence = TAX Int | INFLUENCED Int

data Tech = EMPTY Int | TECHED Research

data Blueprint = Blueprint () -- TODO

data Player = Player {
  species :: SpeciesId,
  diplomacy :: [Maybe DiplomacySlot],
  ambassadors :: Int,
  colonyShips :: [Bool],
  resources :: Cost,
  income :: Cost,
  neutrino :: Cost,
  influence :: [Influence], -- Make an object instead of manual work
  blueprint :: Blueprint,
  tech :: Map.Map ResearchType Tech
}

data Species = Species {
  species :: SpeciesId,
  diplomacyLimit :: Int,
  ambassadorLimit :: Int,
  reputationLimit :: Int,
  description :: String,
  bonuses :: [Player -> Player]
}

data Ship = Ship {
  shipOwner :: PlayerId,
  shipType :: ShipType
}