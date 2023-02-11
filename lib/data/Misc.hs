module Misc (
    Cost(..),
    Phase(..),

) where

data Cost = Cost {
    money :: Int ,
    science :: Int ,
    material :: Int
}

data Phase = ACTION_PHASE | COMBAT_PHASE | UPKEEP_PHASE | CLEANUP_PHASE
    deriving( Read, Show, Enum, Eq, Ord )

type UniqueId = Int