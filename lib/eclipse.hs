{-

What is the state

Players ----

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

Rules ----
  Configuration
  Numbers
  Species IDs
  Dumb Stuff

Game ----

  Player <> Per Game
  Random Tables
  Collectables Reference Map

  Dynamics
    Player <> Per Round
    Research Store
    Collectables Reference Stacks
    Turn Counter :: int
    Dice :: List of Ints
    Tiles

Research Store ----
  List of {
      numeric id,
      description,
      Type,
      Max Cost,
      Min Cost
  }

Collectables Reference Stacks ----
  Development Tiles : List of {
     numeric id,
     description,
     Cost
  }

  Discovery Tile : List of {
    numeric id
  }

Tiles ----
Graph of "Tile"
Tile {
    -- Graph Determines Warp Gates

    Disk: Maybe Player ID
    Planets Slots: List of Planet Slot {resource type, used, is_orbital}
    Ships: List of Ships { Player ID, size type }
    Buildings: List of Monolith | Warp Pad | ETC?
}

-}