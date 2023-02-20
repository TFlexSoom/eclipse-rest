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

import qualified Data.Misc as Misc
import qualified Data.ResearchStore as ResearchStore
import qualified Data.Map as Map

type PlayerId = Misc.UniqueId
type SpeciesId = Misc.UniqueId

data DiplomacyToken = REPUTATION Int | AMBASSADOR PlayerId
data DiplomacySlot = FILLED DiplomacyToken | OPEN | OPEN_REPUTATION | OPEN_AMBASSADOR

data Influence = TAX {-signed-} Int | INFLUENCED {-signed-} Int

data Tech = EMPTY {-signed-} Int | TECHED ResearchStore.Research

data Blueprint = Blueprint () -- TODO

data Player = Player {
  species :: SpeciesId,
  diplomacy :: [Maybe DiplomacySlot],
  ambassadors :: Int,
  colonyShips :: [Bool],
  resources :: Misc.Cost,
  income :: Misc.Cost,
  neutrino :: Misc.Cost,
  influence :: [Influence], -- Make an object instead of manual work
  blueprint :: Blueprint,
  tech :: Map.Map ResearchStore.ResearchType Tech
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
  shipType :: Misc.ShipType
}