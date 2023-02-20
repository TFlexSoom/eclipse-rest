module Data.Rules
  ( Species (..),
    Rules (..),
    getTileMapFromRules,
  )
where

import qualified Data.Collectable as Collectable
import qualified Data.Map as Map
import qualified Data.Misc as Misc
import qualified Data.Player as Player
import qualified Data.ResearchStore as ResearchStore
import qualified Data.Tile as Tile

type SpeciesId = Misc.UniqueId

data Species = Species
  { uniqueId :: SpeciesId,
    name :: String,
    description :: String,
    rulesDescription :: String,
    homeSystem :: Tile.Tile,
    bonuses :: [Player.Player -> Player.Player]
  }

data Rules = Rules
  { numPlayers :: Int,
    species :: [Species],
    defaultPlayer :: Player.Player,
    discoveries :: [Collectable.Collectable],
    developments :: [Collectable.Collectable],
    researchCounts :: [(ResearchStore.Research, Int)],
    center :: Tile.Tile,
    tileTierStacks :: Map.Map Misc.TileDegree [Tile.Tile]
  }

getTileMapFromRules :: Rules -> Tile.TileMapImpl
getTileMapFromRules rules = error "Not Implemented"

defaultRules :: Rules
defaultRules = Rules {
  numPlayers = 2,
  species = [
    Species {
      uniqueId = 1,
      name = "Eridani Empire",
      description = 
        "Money Thirsty Capitalists.\n" ++
        "They were the OGs and now they want to claim the system.\n" ++
        "Starts with extra tax but also extra money in storage.\n",
      rulesDescription = 
        "- Draws Two Random Reputation Tiles at the Beginning of Game\n" ++
        "- Starts with 2 less influence Disks\n" ++
        "- Has Different Ship Blueprints\n" ++
        "- Starts with Extra Tech\n" ++
        "- Starts with 26 Money\n",
      homeSystem = Tile.Tile 
        { uniqueId = 0, 
          disk = Nothing,
          coordinate = (0, 0),
          slots = [
            Tile.PlanetSlot 
            { resourceType = Misc.PURE Misc.MONEY,
              isAdvanced = False,
              isOccupied = False,
              isOrbital = False
            },
            Tile.PlanetSlot 
            { resourceType = Misc.PURE Misc.MONEY,
              isAdvanced = True,
              isOccupied = False,
              isOrbital = False
            },
            Tile.PlanetSlot 
            { resourceType = Misc.PURE Misc.SCIENCE,
              isAdvanced = False,
              isOccupied = False,
              isOrbital = False
            },
            Tile.PlanetSlot 
            { resourceType = Misc.PURE Misc.SCIENCE,
              isAdvanced = True,
              isOccupied = False,
              isOrbital = False
            }
          ], 
          ships = [],
          buildings = [],
          gates = [True, True, False, True, True, False]
      },
      bonuses = []
    },
    Species {
      uniqueId = 2,
      name = "Hydran Progress",
      description = 
        "Scientific Computers\n" ++
        "Never satisfied and always curious, continuing strive.\n" ++
        "May research 2 per research action and has advanced labs.\n",
      rulesDescription = 
        "- With the research action may buy 2 technologies.\n" ++
        "- Has 1 less reputation opportunity.\n" ++
        "- Starts with Advanced Labs.\n" ++
        "- May start with cubes in advanced Labs\n",
      homeSystem = Tile.Tile 
        { uniqueId = 0, 
          disk = Nothing,
          coordinate = (0, 0),
          slots = [
            Tile.PlanetSlot 
            { resourceType = Misc.PURE Misc.MONEY,
              isAdvanced = False,
              isOccupied = False,
              isOrbital = False
            },
            Tile.PlanetSlot 
            { resourceType = Misc.PURE Misc.SCIENCE,
              isAdvanced = True,
              isOccupied = False,
              isOrbital = False
            },
            Tile.PlanetSlot 
            { resourceType = Misc.PURE Misc.SCIENCE,
              isAdvanced = False,
              isOccupied = False,
              isOrbital = False
            },
            Tile.PlanetSlot 
            { resourceType = Misc.PURE Misc.MATERIAL,
              isAdvanced = True,
              isOccupied = False,
              isOrbital = False
            }
          ], 
          ships = [],
          buildings = [],
          gates = [True, True, False, True, True, False]
      },
      bonuses = []
    }
  ],
  defaultPlayer = Player.Player {
    description = "",
    diplomacy = [Player.OPEN, Player.OPEN, Player.OPEN, Player.OPEN],
    ambassadors = 3,
    colonyShips = [True, True, True],
    resources = Map.fromList [
      (Misc.MONEY, 2),
      (Misc.SCIENCE, 3),
      (Misc.MATERIAL, 3)
    ],
    income = Map.fromList [
      (Misc.MONEY, 2),
      (Misc.SCIENCE, 2),
      (Misc.MATERIAL, 2)
    ],
    neutrino = Map.fromList [
      (Misc.MONEY, 0),
      (Misc.SCIENCE, 0),
      (Misc.MATERIAL, 0)
    ],
    influence = [
      Player.INFLUENCED (-30),
      Player.INFLUENCED (-25),
      Player.INFLUENCED (-21),
      Player.INFLUENCED (-17),
      Player.INFLUENCED (-13),
      Player.INFLUENCED (-10),
      Player.INFLUENCED (-7),
      Player.INFLUENCED (-5),
      Player.INFLUENCED (-3),
      Player.INFLUENCED (-2),
      Player.INFLUENCED (-1),
      Player.INFLUENCED 0,
      Player.INFLUENCED 0
    ],
    blueprints = Player.ShipBlueprints {
      interceptor = Player.ShipBlueprint {
        blueprint = [
          Nothing,
          Just Player.ShipPart {
            partId = 1,
            description = "Ion Cannon"
          },
          Just Player.ShipPart {
            partId = 2,
            description = "Nuclear Drive"
          },
          Just Player.ShipPart {
            partId = 3,
            description = "Nuclear Source"
          }
        ],
        bonus = [
          Player.INITIATIVE 2
        ]
      },
      cruiser = Player.ShipBlueprint {
        blueprint = [
          Nothing,
          Just Player.ShipPart {
            partId = 1,
            description = "Ion Cannon"
          },
          Just Player.ShipPart {
            partId = 2,
            description = "Nuclear Drive"
          },
          Just Player.ShipPart {
            partId = 3,
            description = "Nuclear Source"
          },
          Just Player.ShipPart {
            partId = 4,
            description = "Electron Computer"
          },
          Just Player.ShipPart {
            partId = 5,
            description = "Hull"
          }
        ],
        bonus = [
          Player.INITIATIVE 1
        ]
      },
      dreadnought = Player.ShipBlueprint {
        blueprint = [
          Nothing,
          Just Player.ShipPart {
            partId = 1,
            description = "Ion Cannon"
          },
          Just Player.ShipPart {
            partId = 1,
            description = "Ion Cannon"
          },
          Just Player.ShipPart {
            partId = 2,
            description = "Nuclear Drive"
          },
          Just Player.ShipPart {
            partId = 3,
            description = "Nuclear Source"
          },
          Just Player.ShipPart {
            partId = 4,
            description = "Electron Computer"
          },
          Just Player.ShipPart {
            partId = 5,
            description = "Hull"
          },
          Just Player.ShipPart {
            partId = 5,
            description = "Hull"
          }
        ],
        bonus = []
      },
      starbase = Player.ShipBlueprint {
        blueprint = [
          Nothing,
          Just Player.ShipPart {
            partId = 1,
            description = "Ion Cannon"
          },
          Just Player.ShipPart {
            partId = 4,
            description = "Electron Computer"
          },
          Just Player.ShipPart {
            partId = 5,
            description = "Hull"
          },
          Just Player.ShipPart {
            partId = 5,
            description = "Hull"
          }
        ],
        bonus = [
          Player.POWER 3
        ]
      },
      orbital = Player.ShipBlueprint {
        blueprint = [],
        bonus = []
      }
    },
    tech = Map.fromList [
      (1, [ 
        Player.TECHED ResearchStore.Research {
          uniqueId = 1,
          description = "Starbase",
          researchType = 1,
          maxCost = Map.fromList [
            (Misc.MONEY, 0),
            (Misc.SCIENCE, 4),
            (Misc.MATERIAL, 0)
          ],
          minCost = Map.fromList [
            (Misc.MONEY, 0),
            (Misc.SCIENCE, 3),
            (Misc.MATERIAL, 0)
          ]
        },
        Player.EMPTY (-1),
        Player.EMPTY (-2),
        Player.EMPTY (-3),
        Player.EMPTY (-4),
        Player.EMPTY (-6),
        Player.EMPTY (-8)
      ]),
      (2, [
        Player.EMPTY (0),
        Player.EMPTY (-1),
        Player.EMPTY (-2),
        Player.EMPTY (-3),
        Player.EMPTY (-4),
        Player.EMPTY (-6),
        Player.EMPTY (-8)
      ]),
      (3, [
        Player.EMPTY (0),
        Player.EMPTY (-1),
        Player.EMPTY (-2),
        Player.EMPTY (-3),
        Player.EMPTY (-4),
        Player.EMPTY (-6),
        Player.EMPTY (-8)
      ])
    ]
  },
  discoveries = [],
  developments = [],
  researchCounts = [],
  center = Tile.Tile 
  { uniqueId = 0, 
    disk = Nothing,
    coordinate = (0, 0),
    slots = [
      Tile.PlanetSlot 
      { resourceType = Misc.PURE Misc.MONEY,
        isAdvanced = False,
        isOccupied = False,
        isOrbital = False
      },
      Tile.PlanetSlot 
      { resourceType = Misc.PURE Misc.SCIENCE,
        isAdvanced = True,
        isOccupied = False,
        isOrbital = False
      },
      Tile.PlanetSlot 
      { resourceType = Misc.PURE Misc.SCIENCE,
        isAdvanced = False,
        isOccupied = False,
        isOrbital = False
      },
      Tile.PlanetSlot 
      { resourceType = Misc.PURE Misc.MATERIAL,
        isAdvanced = True,
        isOccupied = False,
        isOrbital = False
      }
    ], 
    ships = [],
    buildings = [],
    gates = [True, True, False, True, True, False]
  },
  tileTierStacks = Map.empty
}