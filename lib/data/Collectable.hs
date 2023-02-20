module Data.Collectable (
  Collectable(..),
  Discovery(..),
  Development(..)
)
where


import qualified Data.Misc as Misc
import qualified Data.Map as Map

class Collectable a where
  uniqueId :: a -> Misc.UniqueId
  description :: a -> String
  cost :: a -> Misc.Cost


data Discovery = Discovery {
  idImpl :: Misc.UniqueId,
  descriptionImpl :: String
}

-- private
zerocost :: Misc.Cost
zerocost = Map.fromList [(Misc.MONEY, 0), (Misc.SCIENCE, 0), (Misc.MATERIAL, 0)]

instance Collectable Discovery where
  uniqueId Discovery{idImpl=idImpl} = idImpl
  description Discovery{descriptionImpl=descriptionImpl} = descriptionImpl
  cost _ = zerocost

data Development = Development {
  idImpl :: Misc.UniqueId,
  descriptionImpl :: String,
  costImpl :: Misc.Cost
}

instance Collectable Development where
  uniqueId Development{idImpl=idImpl} = idImpl
  description Development{descriptionImpl=descriptionImpl} = descriptionImpl
  cost Development{costImpl=costImpl} = costImpl