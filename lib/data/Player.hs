module Data.Player
  ( PlayerId,
    DiplomacySlot (..),
    DiplomacyToken (..),
    Influence (..),
    Tech (..),
    ShipBonus (..),
    ShipPart (..),
    ShipBlueprint (..),
    ShipBlueprints (..),
    Player (..),
    Ship (..),
  )
where

import qualified Data.Map as Map
import qualified Data.Misc as Misc
import qualified Data.ResearchStore as ResearchStore

type PlayerId = Misc.UniqueId,
type PartId = Misc.UniqueId

data DiplomacyToken = REPUTATION Int | AMBASSADOR PlayerId

data DiplomacySlot = FILLED DiplomacyToken | OPEN | OPEN_REPUTATION | OPEN_AMBASSADOR

data Influence = TAX {-signed-} Int | INFLUENCED {-signed-} Int

data Tech = EMPTY {-signed-} Int | TECHED ResearchStore.Research

data ShipBonus = INITIATIVE Int | POWER Int | COMPUTER Int | SHIELD Int

data ShipPart = ShipPart
  { partId :: PartId,
    description :: String
  }

data ShipBlueprint = ShipBlueprint {
  blueprint :: [Maybe ShipPart],
  bonus :: [ShipBonus]
}

data ShipBlueprints = ShipBlueprints
  { interceptor :: ShipBlueprint,
    cruiser :: ShipBlueprint,
    dreadnought :: ShipBlueprint,
    starbase :: ShipBlueprint,
    orbital :: ShipBlueprint
  }

data Player = Player
  { description :: String,
    diplomacy :: [DiplomacySlot],
    ambassadors :: Int,
    colonyShips :: [Bool],
    resources :: Misc.Cost,
    income :: Misc.Cost,
    neutrino :: Misc.Cost,
    influence :: [Influence],
    blueprints :: ShipBlueprints,
    tech :: Map.Map ResearchStore.ResearchType [Tech]
  }

data Ship = Ship
  { shipOwner :: PlayerId,
    shipType :: Misc.ShipType
  }
