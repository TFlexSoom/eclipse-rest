{-
Per Game
  Player ID
  Species ID
  Ambassador Limit
  Ship Bonuses
  Disk Tax Map/Int->Int
  Color

Per Round
  Player ID
  Ambassador List:
    - Reputation
    - Ambassador
    
  Blueprint Matrix
  Tech Map List
  Disk Uses Map

  Counters:
    Money
    Science
    Material
    Money Income
    Science Income
    Material Income
    Money Neutrino
    Science Neutrino
    Material Neutrino
    Influence Disks
    Colony Ships
    Ambassadors

  Species Bonus Counters?:
    Species Counter 1
    Species Counter 2
    Species Counter 3
  
  Ships:
    Interceptors
    Cruisers
    Dreadnoughts
    Starbases
  
  Collectables 
    Map of id -> Development Tile | Discovery Tile | Species Bonus
-}
module Data.Player (
  PlayerId,
  Ship(..),
  PlayerStatic(..),
  PlayerDynamic(..)
) where

import Data.Misc ( UniqueId, ShipType )

type PlayerId = UniqueId

-- TODO
data PlayerStatic = PlayerStatic ()
data PlayerDynamic = PlayerDynamic ()

data Ship = Ship {
  shipOwner :: PlayerId,
  shipType :: ShipType
}