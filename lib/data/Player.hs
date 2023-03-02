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

import qualified Data.Collectable as Collectable
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Misc as Misc

type PlayerId = Misc.UniqueId

type PartId = Misc.UniqueId

data DiplomacyToken = REPUTATION Int.Int8 | AMBASSADOR PlayerId

data DiplomacySlot = FILLED DiplomacyToken | OPEN | OPEN_REPUTATION | OPEN_AMBASSADOR

data Influence = TAX {-signed-} Int.Int8 | INFLUENCED {-signed-} Int.Int8

data Tech = EMPTY {-signed-} Int.Int8 | TECHED Collectable.ResearchId

data ShipBonus = INITIATIVE Int.Int8 | POWER Int.Int8 | COMPUTER Int.Int8 | SHIELD Int.Int8

data ShipPart = ShipPart
  { partId :: PartId,
    description :: String
  }

data ShipBlueprint = ShipBlueprint
  { blueprint :: [Maybe ShipPart],
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
  { playerId :: PlayerId,
    description :: String,
    diplomacy :: [DiplomacySlot],
    ambassadors :: Int,
    colonyShips :: [Bool],
    resources :: Misc.Cost,
    income :: Misc.Cost,
    neutrino :: Misc.Cost,
    influence :: [Influence],
    blueprints :: ShipBlueprints,
    tech :: Map.Map Collectable.ResearchType [Tech]
  }

data Ship = Ship
  { shipOwner :: PlayerId,
    shipType :: Misc.ShipType
  }
