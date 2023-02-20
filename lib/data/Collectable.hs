module Data.Collectable
  ( Collectable (..),
    discovery,
    development,
  )
where

import qualified Data.Map as Map
import qualified Data.Misc as Misc

data Collectable = Collectable
  { uniqueId :: Misc.UniqueId,
    description :: String,
    cost :: Misc.Cost
  }

-- private
zerocost :: Misc.Cost
zerocost = Map.fromList [(Misc.MONEY, 0), (Misc.SCIENCE, 0), (Misc.MATERIAL, 0)]

discovery :: Misc.UniqueId -> String -> Collectable
discovery uid desc =
  Collectable
    { uniqueId = uid,
      description = desc,
      cost = zerocost
    }

development :: Misc.UniqueId -> String -> Misc.Cost -> Collectable
development uid desc cost =
  Collectable
    { uniqueId = uid,
      description = desc,
      cost = cost
    }
