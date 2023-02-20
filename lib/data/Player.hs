module Data.Player
  ( PlayerId,
    DiplomacySlot (..),
    DiplomacyToken (..),
    Influence (..),
    Tech (..),
    Player (..),
    Ship (..),
  )
where

import qualified Data.Map as Map
import qualified Data.Misc as Misc
import qualified Data.ResearchStore as ResearchStore

type PlayerId = Misc.UniqueId

data DiplomacyToken = REPUTATION Int | AMBASSADOR PlayerId

data DiplomacySlot = FILLED DiplomacyToken | OPEN | OPEN_REPUTATION | OPEN_AMBASSADOR

data Influence = TAX {-signed-} Int | INFLUENCED {-signed-} Int

data Tech = EMPTY {-signed-} Int | TECHED ResearchStore.Research

data ShipPart = ShipPart
  { partId :: Misc.UniqueId,
    description :: String
  }

data Blueprint = Blueprint
  { interceptor :: [ShipPart],
    cruiser :: [ShipPart],
    dreadnought :: [ShipPart],
    starbase :: [ShipPart],
    orbital :: [ShipPart]
  }

data Player = Player
  { description :: String,
    diplomacy :: [Maybe DiplomacySlot],
    ambassadors :: Int,
    colonyShips :: [Bool],
    resources :: Misc.Cost,
    income :: Misc.Cost,
    neutrino :: Misc.Cost,
    influence :: [Influence],
    blueprint :: Blueprint,
    tech :: Map.Map ResearchStore.ResearchType Tech
  }

data Ship = Ship
  { shipOwner :: PlayerId,
    shipType :: Misc.ShipType
  }
