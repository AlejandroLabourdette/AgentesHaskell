module Main where

import Board
import ListUtils
import Elements
import Print
import Operations
import Generate
import Objectives

main :: IO ()
main = do
    -- putStrLn (printBoard board01)
    -- putStrLn (printBoard board02)
    -- putStrLn (printBoard board03)
    -- putStrLn (printBoard board04)
    putStrLn (printBoard board05)
    -- putStrLn (printBoard board06)
    -- putStrLn (printBoard board07)
    -- putStrLn (printBoard board08)
    -- putStrLn (printBoard board09)
    -- putStrLn (printBoard board10)
    -- putStrLn (printBoard board11)
    -- putStrLn (printBoard board12)
    print (computeObjectives board06)

-- board01 = initializeBoard 6 6 876318
board01 = initializeBoard 6 6 876314
board02 = generateCribs board01 0
board03 = generateKids board02 3
board04 = generateRobots board03 3 
board05 = generateDirts board04 3 

-- board06 = Operations.moveRght board05 (Kid 1 2) 
-- board07 = Operations.moveRght board06 (Kid 1 3) 
-- board08 = Operations.moveRght board07 (Kid 1 4)
-- board09 = Board.add board08 (Robot 2 5 False)
-- board10 = Operations.moveUp board09 (Robot 2 5 False)
-- board11 = Operations.moveDwn board10 (Robot 1 5 True)
-- board12 = Operations.dropKid board11 (Robot 2 5 True)
