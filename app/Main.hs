module Main where

import Eclipse ( 
    Rules(..),
    PlayerAction(..),
    Game(..),
    newGame)

main :: IO ()
main = do {
    putStrLn "Starting Game Test";
    rules <- return Rules {
            species = []
        };
    game <- return (newGame rules);
    game <- return (update game NOTHING);
    putStrLn "Finished Game Test!"
}
