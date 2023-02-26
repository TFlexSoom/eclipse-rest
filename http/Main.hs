module Main where

import qualified Eclipse as Eclipse

main :: IO ()
main = do
  putStrLn "Starting Game Test"
  rules <-
    return
      Eclipse.Rules
        { species = []
        }
  game <- return (Eclipse.newGame rules)
  game <- return (Eclipse.update game Eclipse.NOTHING)
  putStrLn "Finished Game Test!"
