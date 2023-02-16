module Data.Collectable (
  Collectable(..),
  Discovery(..),
  Development(..)
)
where

import Data.Misc ( UniqueId, Cost )

import qualified Data.Map as Map

class Collectable a where
  uniqueId :: a -> UniqueId
  description :: a -> String
  cost :: a -> Cost


data Discovery = Discovery {
  idImpl :: UniqueId,
  descriptionImpl :: String
}

-- private
zerocost :: Cost
zerocost = Map.empty
-- TODO

instance Collectable Discovery where
  uniqueId Discovery{idImpl=idImpl} = idImpl
  description Discovery{descriptionImpl=descriptionImpl} = descriptionImpl
  cost _ = zerocost

data Development = Development {
  idImpl :: UniqueId,
  descriptionImpl :: String,
  costImpl :: Cost
}

instance Collectable Development where
  uniqueId Development{idImpl=idImpl} = idImpl
  description Development{descriptionImpl=descriptionImpl} = descriptionImpl
  cost Development{costImpl=costImpl} = costImpl